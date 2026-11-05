"""Generate GoogleTest registrations from explicitly marked, no-argument Fortran tests."""

from __future__ import annotations

import argparse
import json
import re
import sys
import tempfile
from collections.abc import Iterator
from dataclasses import dataclass
from pathlib import Path
from typing import NoReturn

IDENTIFIER = r"[A-Za-z][A-Za-z0-9_]*"
MARKER = re.compile(r"^\s*!\$gtest\b(?P<directive>.*)$", re.IGNORECASE)
MODULE = re.compile(rf"module\s+(?P<module>{IDENTIFIER})\s*$", re.IGNORECASE)
SUBROUTINE = re.compile(
    rf"subroutine\s+(?P<procedure>{IDENTIFIER})\s*\(\s*\)\s*bind\s*\(\s*c\s*\)\s*$",
    re.IGNORECASE,
)


class RegistryError(ValueError):
    """Invalid directive or unsupported marked declaration."""


@dataclass(frozen=True)
class Test:
    source: Path
    line: int
    module: str
    procedure: str


def fail(source: Path, line: int, message: str) -> NoReturn:
    raise RegistryError(f"{source}:{line}: {message}")


def uncomment(line: str) -> str:
    """Remove trailing comments from the restricted registration syntax."""
    return line.partition("!")[0]


def source_lines(source: Path) -> Iterator[tuple[int, bool, str]]:
    """Yield markers and nonempty code lines without joining continuations."""
    for number, line in enumerate(source.read_text(encoding="utf-8-sig").splitlines(), 1):
        marker = MARKER.fullmatch(line)
        if marker:
            yield number, True, uncomment(marker["directive"]).strip()
            continue
        code = uncomment(line).strip()
        if code:
            yield number, False, code


def parse(source: Path) -> list[Test]:
    tests = []
    module = None
    lines = source_lines(source)
    for line, marker, text in lines:
        if marker:
            if text:
                fail(source, line, f"invalid gtest directive: {text}")

            following = next(lines, None)
            if following is None:
                fail(source, line, "orphaned test marker")
            declaration_line, declaration_marker, declaration_text = following
            if declaration_marker:
                fail(source, line, "orphaned test marker")
            declaration = SUBROUTINE.fullmatch(declaration_text)
            if not declaration:
                fail(source, declaration_line,
                     "test marker requires a plain, single-line, no-argument subroutine with bind(C)")
            if module is None:
                fail(source, line, "test marker must be inside a module")
            tests.append(Test(source, line, module, declaration["procedure"]))
            continue

        declaration = MODULE.fullmatch(text)
        if declaration:
            if module:
                fail(source, line, "nested modules are not supported")
            module = declaration["module"]
        elif re.fullmatch(r"end\s*module(?:\s+\w+)?", text, re.IGNORECASE):
            module = None
    return tests


def collect(sources: list[Path]) -> list[Test]:
    tests = [test for source in sources for test in parse(source)]
    bindings = {}
    for test in tests:
        binding = test.procedure.lower()
        if binding in bindings:
            previous = bindings[binding]
            fail(test.source, test.line,
                 f"duplicate C binding: {binding} (first at {previous.source}:{previous.line})")
        bindings[binding] = test
    return tests


def render(tests: list[Test], teardown: str | None = None) -> str:
    lines = ['// Generated from !$gtest markers. Do not edit.', '#include <gtest/gtest.h>', ""]
    for test in tests:
        lines.append(f'extern "C" void {test.procedure.lower()}();')
    if teardown:
        if not re.fullmatch(IDENTIFIER, teardown):
            raise RegistryError("teardown must be a C identifier")
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
                f"TEST({test.module}, {test.procedure}) {{ ::{test.procedure.lower()}(); }}",
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
