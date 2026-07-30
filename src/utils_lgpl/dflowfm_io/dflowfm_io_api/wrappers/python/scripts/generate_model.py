"""Generate the typed MduModel from the dflowfm_io_api C header.

Reads `dflowfm_io_api.h` and writes `src/dflowfm_io/mdu/model.py`: the typed :class:`MduModel`, one
get/set method per `mdu_get_*`/`mdu_set_*` function, with the per-type value marshalling.

Run:
    python scripts/generate_model.py

The header is small and regular (flat `extern "C"` declarations), so this parses it directly rather
than depending on a C preprocessor. The ctypes bindings (`base/bindings.py`) are NOT generated — they
are a small, rarely-changing file kept in sync by hand. The lifecycle (`MduDocument`), the report
(`MduReport`), and the `Issue`/`Severity` value types also stay hand-written: they encode Python
design decisions with no 1:1 C-function counterpart.
"""

import re
from dataclasses import dataclass
from pathlib import Path

from generator_base import GeneratedModule, Generator, SourceBuilder

HERE = Path(__file__).resolve().parent  # .../dflowfm_io_api/wrappers/python/scripts
HEADER = HERE.parents[2] / "include" / "dflowfm_io_api" / "dflowfm_io_api.h"
OUTPUT = HERE.parent / "src" / "dflowfm_io" / "mdu" / "model.py"


@dataclass(frozen=True)
class CFunction:
    """An exported C function: its return type, name, and raw comma-separated parameter string."""

    restype: str
    name: str
    params: str


class HeaderParser:
    """Extracts the exported functions from the flat `extern "C"` dflowfm_io_api.h."""

    def __init__(self, text: str) -> None:
        self._text = text

    def functions(self) -> list[CFunction]:
        """Return every DFLOWFM_IO_API_EXPORT function declaration."""
        matches = re.findall(r"DFLOWFM_IO_API_EXPORT\s+(.+?)\s+(\w+)\s*\((.*?)\)\s*;", self._text, re.S)
        return [CFunction(restype, name, params) for restype, name, params in matches]


class ModelRenderer:
    """Renders the typed MduModel (mdu/model.py), one get/set method per accessor function.

    The four tables below map the *type suffix* of the C function name to the marshalling for that
    method. The suffix carries the semantic type (mdu_get_path vs mdu_get_string) that the raw
    ctypes signature does not.
    """

    # scalar getter: suffix -> (ctypes box type, Python return type, expression converting `value`)
    GET_SCALAR = {
        "int": ("ctypes.c_int32", "int", "value.value"),
        "bool": ("ctypes.c_int32", "bool", "value.value != 0"),
        "double": ("ctypes.c_double", "float", "value.value"),
        "int_enum": ("ctypes.c_int32", "int", "value.value"),
        "string_enum": ("ctypes.c_char_p", "str", 'value.value.decode("utf-8")'),
        "string": ("ctypes.c_char_p", "str", 'value.value.decode("utf-8")'),
        "path": ("ctypes.c_char_p", "Path", 'Path(value.value.decode("utf-8"))'),
        # Build from the epoch rather than datetime.fromtimestamp: the latter raises OSError on
        # Windows for negative (pre-1970) epochs, realistic MDU reference dates (e.g. 19000101).
        "datetime": (
            "ctypes.c_int64",
            "datetime",
            "datetime(1970, 1, 1, tzinfo=timezone.utc) + timedelta(seconds=value.value)",
        ),
    }

    # list getter: suffix -> (element ctypes type, Python return type, expression converting `array_out[i]`)
    GET_LIST = {
        "string": ("ctypes.c_char_p", "list[str]", 'array_out[i].decode("utf-8")'),
        "path": ("ctypes.c_char_p", "list[Path]", 'Path(array_out[i].decode("utf-8"))'),
        "double": ("ctypes.c_double", "list[float]", "array_out[i]"),
    }

    # scalar setter: suffix -> (Python argument type, expression producing the C argument from `value`)
    SET_SCALAR = {
        "int": ("int", "ctypes.c_int32(value)"),
        "bool": ("bool", "ctypes.c_int32(1 if value else 0)"),
        "double": ("float", "ctypes.c_double(value)"),
        "int_enum": ("int", "ctypes.c_int32(value)"),
        "string_enum": ("str", 'value.encode("utf-8")'),
        "string": ("str", 'value.encode("utf-8")'),
        "path": ("Path | str", 'str(value).encode("utf-8")'),
        # get_datetime returns UTC-aware; treat a naive input as UTC too, so round-trips are
        # symmetric (a naive datetime's .timestamp() would otherwise be interpreted in local time).
        "datetime": (
            "datetime",
            "ctypes.c_int64(int((value if value.tzinfo else value.replace(tzinfo=timezone.utc)).timestamp()))",
        ),
    }

    # list setter: suffix -> (Python argument type, element ctypes type, expression encoding `v` — None = identity)
    SET_LIST = {
        "string": ("list[str]", "ctypes.c_char_p", 'v.encode("utf-8")'),
        "path": ("list[Path | str]", "ctypes.c_char_p", 'str(v).encode("utf-8")'),
        "double": ("list[float]", "ctypes.c_double", None),
    }

    # C functions that are not typed model accessors: the report list and the document lifecycle.
    _NON_ACCESSORS = frozenset({"mdu_get_issue_list"})

    def __init__(self, header_name: str) -> None:
        self._header_name = header_name

    def render(self, functions: list[CFunction]) -> tuple[str, int]:
        """Render the MduModel module source; return (source, number of accessor methods)."""
        builder = SourceBuilder()
        builder.line(f'"""GENERATED from {self._header_name} by scripts/generate_model.py - do not edit.')
        builder.blank()
        builder.line("The typed MduModel: one get/set method per mdu_get_*/mdu_set_* C function, with the per-type")
        builder.line("value marshalling. Regenerated from the C header, so the accessors cannot drift from the ABI.")
        builder.line('"""')
        builder.blank()
        builder.line("import ctypes")
        builder.line("from datetime import datetime, timedelta, timezone")
        builder.line("from pathlib import Path")
        builder.blank()
        builder.line("from dflowfm_io.base import HandleRef, check_result, lib")
        builder.blank(2)
        builder.line("class MduModel:")
        builder.line('    """Typed get/set access to MDU properties, keyed by dotted `section.property` names."""')
        builder.blank()
        builder.line("    def __init__(self, ref: HandleRef):")
        builder.line("        self._ref = ref")

        count = 0
        for function in functions:
            accessor = self._accessor(function.name)
            if accessor is None:
                continue
            builder.blank()
            builder.extend(accessor)
            count += 1
        return builder.render(), count

    def _accessor(self, cname: str) -> list[str] | None:
        """Render the get/set method for a C accessor, or None if the function is not an accessor."""
        if cname in self._NON_ACCESSORS:
            return None
        if cname.startswith("mdu_get_"):
            kind, rest = "get", cname[len("mdu_get_") :]
        elif cname.startswith("mdu_set_"):
            kind, rest = "set", cname[len("mdu_set_") :]
        else:  # lifecycle: create/destroy/load/save — hand-written in MduDocument
            return None

        is_list = rest.endswith("_list")
        suffix = rest[: -len("_list")] if is_list else rest
        method = f"{kind}_{rest}"
        handle = 'self._ref.handle, key.encode("utf-8")'

        if kind == "get" and not is_list:
            box, pyret, convert = self._marshalling(self.GET_SCALAR, suffix, cname)
            return [
                f"    def {method}(self, key: str) -> {pyret}:",
                f"        value = {box}()",
                f"        check_result(lib.{cname}({handle}, ctypes.byref(value)))",
                f"        return {convert}",
            ]
        if kind == "get" and is_list:
            elem, pyret, convert = self._marshalling(self.GET_LIST, suffix, cname)
            return [
                f"    def {method}(self, key: str) -> {pyret}:",
                f"        array_out = ctypes.POINTER({elem})()",
                "        size_out = ctypes.c_uint64()",
                f"        check_result(lib.{cname}({handle}, ctypes.byref(array_out), ctypes.byref(size_out)))",
                f"        return [{convert} for i in range(size_out.value)]",
            ]
        if kind == "set" and not is_list:
            pyarg, c_arg = self._marshalling(self.SET_SCALAR, suffix, cname)
            return [
                f"    def {method}(self, key: str, value: {pyarg}) -> None:",
                f"        check_result(lib.{cname}({handle}, {c_arg}))",
            ]
        pyarg, elem, encode = self._marshalling(self.SET_LIST, suffix, cname)
        encoded = "list(values)" if encode is None else f"[{encode} for v in values]"
        return [
            f"    def {method}(self, key: str, values: {pyarg}) -> None:",
            f"        encoded = {encoded}",
            f"        arr = ({elem} * len(encoded))(*encoded)",
            f"        check_result(lib.{cname}({handle}, arr, ctypes.c_uint64(len(encoded))))",
        ]

    @staticmethod
    def _marshalling(table: dict, suffix: str, cname: str):
        """Look up the marshalling for an accessor suffix, failing closed with a clear message."""
        try:
            return table[suffix]
        except KeyError:
            raise ValueError(
                f"{cname}: unmapped accessor suffix {suffix!r}. Add it to the ModelRenderer "
                f"marshalling tables, or add {cname!r} to ModelRenderer._NON_ACCESSORS if it is "
                f"not a typed value accessor."
            ) from None


class ModelGenerator(Generator):
    """Generates mdu/model.py from the C header."""

    def __init__(self, header_path: Path = HEADER, output: Path = OUTPUT) -> None:
        self._header_path = header_path
        self._output = output

    def build(self) -> list[GeneratedModule]:
        functions = HeaderParser(self._header_path.read_text(encoding="utf-8")).functions()
        source, count = ModelRenderer(self._header_path.name).render(functions)
        return [GeneratedModule(self._output, source, f"{count} MduModel accessors")]


if __name__ == "__main__":
    ModelGenerator().run()
