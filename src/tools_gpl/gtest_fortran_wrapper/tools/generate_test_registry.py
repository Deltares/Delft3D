"""Generate GoogleTest registrations from explicitly marked, no-argument Fortran tests."""

from __future__ import annotations

import argparse
from dataclasses import dataclass
import json
from pathlib import Path
import re
import sys
import tempfile


IDENTIFIER = r"[A-Za-z][A-Za-z0-9_]*"
CXX_RESERVED = frozenset(
    """alignas alignof and and_eq asm auto bitand bitor bool break case catch char
    char8_t char16_t char32_t class compl concept const consteval constexpr constinit
    const_cast continue co_await co_return co_yield decltype default delete do double
    dynamic_cast else enum explicit export extern false float for friend goto if inline
    int long mutable namespace new noexcept not not_eq nullptr operator or or_eq private
    protected public register reinterpret_cast requires return short signed sizeof static
    static_assert static_cast struct switch template this thread_local throw true try
    typedef typeid typename union unsigned using virtual void volatile wchar_t while
    xor xor_eq main""".split()
)
MARKER = re.compile(r"^\s*!\$gtest\b(.*)$", re.IGNORECASE)
MODULE = re.compile(rf"module\s+({IDENTIFIER})\s*$", re.IGNORECASE)
SUBROUTINE = re.compile(
    rf"(?:(?:recursive|non_recursive|pure|impure|elemental|module)\s+)*"
    rf"subroutine\s+({IDENTIFIER})\s*\(\s*\)\s*"
    r"bind\s*\(\s*c\s*(?:,\s*name\s*=\s*(['\"])(.*?)\2\s*)?\)\s*$",
    re.IGNORECASE,
)


class RegistryError(ValueError):
    """Invalid directive or unsupported marked declaration."""


@dataclass(frozen=True)
class Test:
    source: Path
    line: int
    module: str | None
    suite: str
    name: str
    procedure: str
    binding: str


def fail(source: Path, line: int, message: str) -> None:
    raise RegistryError(f"{source}:{line}: {message}")


def uncomment(line: str) -> str:
    """Remove comments without treating exclamation marks in literals as comments."""
    quote = None
    i = 0
    while i < len(line):
        char = line[i]
        if quote:
            if char == quote:
                if i + 1 < len(line) and line[i + 1] == quote:
                    i += 1
                else:
                    quote = None
        elif char in "'\"":
            quote = char
        elif char == "!":
            return line[:i]
        i += 1
    return line


def statements(source: Path):
    """Yield markers and free-form statements, joining ampersand continuations."""
    pending = ""
    start = 0
    for number, line in enumerate(source.read_text(encoding="utf-8-sig").splitlines(), 1):
        marker = MARKER.fullmatch(line)
        if marker:
            if pending:
                fail(source, number, "directive inside a continued declaration")
            yield number, True, uncomment(marker[1]).strip()
            continue
        code = uncomment(line).strip()
        if not code:
            continue
        if not pending:
            start = number
        else:
            code = code[1:] if code.startswith("&") else " " + code
        continued = code.endswith("&")
        # Do not insert whitespace: continuation can split a Fortran identifier.
        pending += code[:-1] if continued else code
        if not continued:
            yield start, False, pending.strip()
            pending = ""
    if pending:
        fail(source, start, "unterminated continued declaration")


def fields(source: Path, line: int, text: str) -> tuple[str, dict[str, str]]:
    parts = text.split()
    if not parts:
        fail(source, line, "empty gtest directive")
    kind = parts[0].lower()
    if kind not in {"test", "suite"}:
        fail(source, line, f"unknown directive: {parts[0]}")
    options = {}
    # A suite override is written !$gtest suite=NAME, not !$gtest suite suite=NAME.
    for part in parts[1:]:
        match = re.fullmatch(rf"(suite|name)=({IDENTIFIER})", part, re.IGNORECASE)
        if not match or match[1].lower() in options:
            fail(source, line, f"invalid or duplicate directive field: {part}")
        options[match[1].lower()] = match[2]
    return kind, options


def parse(source: Path) -> list[Test]:
    tests = []
    module = None
    suite = None
    in_specification = False
    pending = None
    for line, marker, text in statements(source):
        if marker:
            if pending:
                fail(source, pending[0], "orphaned test marker")
            suite_override = re.fullmatch(rf"suite=({IDENTIFIER})", text, re.IGNORECASE)
            if suite_override or text.lower() == "suite":
                if module is None or not in_specification:
                    fail(source, line, "suite marker must be in a module specification")
                suite = suite_override[1] if suite_override else module
                continue
            kind, options = fields(source, line, text)
            if kind != "test":
                fail(source, line, "malformed suite marker")
            pending = (line, options)
            continue

        if pending:
            # Intel attributes and access declarations may precede the subroutine.
            if re.match(r"(?:attributes\s*\(|(?:public|private)\s*::)", text, re.IGNORECASE):
                continue
            declaration = SUBROUTINE.fullmatch(text)
            if not declaration:
                fail(source, line, "test marker requires a no-argument subroutine with bind(C)")
            procedure, _, binding = declaration.groups()
            marker_line, options = pending
            test_suite = options.get("suite", suite or module)
            if test_suite is None:
                fail(source, marker_line, "test outside a module requires suite=NAME")
            binding = binding if binding is not None else procedure.lower()
            if not re.fullmatch(IDENTIFIER, binding) or binding in CXX_RESERVED:
                fail(source, line, "C binding name must be an identifier, not a C++ keyword or main")
            tests.append(
                Test(source, marker_line, module, test_suite, options.get("name", procedure), procedure, binding)
            )
            pending = None
            continue

        declaration = MODULE.fullmatch(text)
        if declaration:
            if module:
                fail(source, line, "nested modules are not supported")
            module = declaration[1]
            suite = module
            in_specification = True
        elif re.fullmatch(r"end\s*module(?:\s+\w+)?", text, re.IGNORECASE):
            module = suite = None
            in_specification = False
        elif text.lower() == "contains":
            in_specification = False
    if pending:
        fail(source, pending[0], "orphaned test marker")
    return tests


def collect(sources: list[Path]) -> list[Test]:
    tests = [test for source in sources for test in parse(source)]
    names = {}
    bindings = {}
    for test in tests:
        for key, seen, label in [
            ((test.suite, test.name), names, "GoogleTest name"),
            (test.binding, bindings, "C binding"),
        ]:
            if key in seen:
                previous = seen[key]
                fail(test.source, test.line, f"duplicate {label}: {key} (first at {previous.source}:{previous.line})")
            seen[key] = test
    return tests


def render(tests: list[Test], teardown: str | None = None) -> str:
    lines = ['// Generated from !$gtest markers. Do not edit.', '#include <gtest/gtest.h>', ""]
    for test in tests:
        lines.append(f'extern "C" void {test.binding}();')
    if teardown:
        if not re.fullmatch(IDENTIFIER, teardown) or teardown in CXX_RESERVED:
            raise RegistryError("teardown must be a C identifier, not a C++ keyword or main")
        lines.extend(
            [
                f'extern "C" void {teardown}();',
                "namespace {",
                "class FortranTestCleanup : public testing::EmptyTestEventListener {",
                "    void OnTestEnd(const testing::TestInfo&) override {",
                f"        ::{teardown}();",
                "    }",
                "};",
                "}",
            ]
        )
    for test in tests:
        lines.extend(
            [
                f"#line {test.line} {json.dumps(str(test.source))}",
                f"TEST({test.suite}, {test.name}) {{ ::{test.binding}(); }}",
            ]
        )
    lines.extend(["", "int main(int argc, char** argv) {", "    testing::InitGoogleTest(&argc, argv);"])
    if teardown:
        lines.append("    testing::UnitTest::GetInstance()->listeners().Append(new FortranTestCleanup);")
    lines.extend(["    return RUN_ALL_TESTS();", "}", ""])
    return "\n".join(lines)


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--teardown")
    parser.add_argument("sources", nargs="+", type=Path)
    args = parser.parse_args()
    try:
        result = render(collect(args.sources), args.teardown)
        args.output.parent.mkdir(parents=True, exist_ok=True)
        # Each invocation owns one output; replace atomically, including on Windows.
        with tempfile.NamedTemporaryFile(
            mode="w", encoding="utf-8", newline="\n", dir=args.output.parent, delete=False
        ) as temporary:
            temporary.write(result)
        temporary_path = Path(temporary.name)
        try:
            temporary_path.replace(args.output)
        finally:
            temporary_path.unlink(missing_ok=True)
    except (RegistryError, OSError, UnicodeError) as error:
        print(error, file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
