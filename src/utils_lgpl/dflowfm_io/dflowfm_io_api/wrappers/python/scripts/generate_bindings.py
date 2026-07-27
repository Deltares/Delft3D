"""Generate the Python wrapper's ABI-coupled code from the dflowfm_io_api C header.

Reads `dflowfm_io_api.h` and writes two generated modules, so the Python side can never drift
from the DLL:

* `src/dflowfm_io/base/bindings.py` — the ctypes ABI mirror: enum constants, the `mdu_issue_t`
  struct, and `argtypes`/`restype` for every exported function, applied to the loaded `lib`.
* `src/dflowfm_io/mdu/model.py` — the typed :class:`MduModel`, one get/set method per
  `mdu_get_*`/`mdu_set_*` function, with the per-type value marshalling.

Run:
    python scripts/generate_bindings.py

The header is small and regular (flat `extern "C"` declarations), so this parses it directly
rather than depending on a C preprocessor. The lifecycle (`MduDocument`), the report
(`MduReport`), and the `Issue`/`Severity` value types stay hand-written: they encode Python
design decisions (ownership, `__del__`, convenience methods) with no 1:1 C-function counterpart.
"""

import re
from dataclasses import dataclass
from pathlib import Path

from codegen_support import GeneratedModule, Generator, SourceBuilder

HERE = Path(__file__).resolve().parent  # .../dflowfm_io_api/wrappers/python/scripts
HEADER = HERE.parents[2] / "include" / "dflowfm_io_api" / "dflowfm_io_api.h"
SRC = HERE.parent / "src" / "dflowfm_io"
OUTPUT_BINDINGS = SRC / "base" / "bindings.py"
OUTPUT_MODEL = SRC / "mdu" / "model.py"


def trailing_identifier(declaration: str, context: str) -> str:
    """Return the trailing identifier of a C declaration (the parameter/field name), or raise."""
    match = re.search(r"(\w+)\s*$", declaration)
    if match is None:
        raise ValueError(f"No name found in {context}: {declaration!r}")
    return match.group(1)


@dataclass(frozen=True)
class CFunction:
    """An exported C function: its return type, name, and raw comma-separated parameter string."""

    restype: str
    name: str
    params: str


class CTypeMapper:
    """Maps C type declarations to their ctypes expressions."""

    # C base type -> ctypes expression. `char` is special-cased (char* is c_char_p, not POINTER(c_char)).
    BASE_TYPES = {
        "void": "None",
        "char": "ctypes.c_char",
        "double": "ctypes.c_double",
        "int32_t": "ctypes.c_int32",
        "int64_t": "ctypes.c_int64",
        "uint64_t": "ctypes.c_uint64",
        "dflowfm_io_result_t": "ctypes.c_int32",
        "dflowfm_io_bool_t": "ctypes.c_int32",
        "mdu_severity_t": "ctypes.c_int32",
        "mdu_handle_t": "ctypes.c_void_p",
        "mdu_issue_t": "mdu_issue_t",
    }

    def to_ctypes(self, decl: str) -> str:
        """Map a C type declaration (possibly with `const` and `*`) to a ctypes expression."""
        stars = decl.count("*")
        base = decl.replace("const", "").replace("*", "").strip()
        if base == "char" and stars >= 1:  # char* -> c_char_p; each extra * wraps once more
            result = "ctypes.c_char_p"
            wraps = stars - 1
        else:  # bare char -> c_char; everything else via the base-type table
            result = self.BASE_TYPES[base]
            wraps = stars
        for _ in range(wraps):
            result = f"ctypes.POINTER({result})"
        return result

    def argtypes(self, param_str: str) -> list[str]:
        """Return the ctypes argtypes for a comma-separated C parameter list."""
        param_str = param_str.strip()
        if param_str in ("", "void"):
            return []
        argtypes = []
        for param in param_str.split(","):
            name = trailing_identifier(param, "parameter")
            type_decl = param[: param.rfind(name)]
            argtypes.append(self.to_ctypes(type_decl))
        return argtypes


class HeaderParser:
    """Parses the flat `extern "C"` dflowfm_io_api.h into enum members, struct fields, and functions."""

    def __init__(self, text: str, types: CTypeMapper) -> None:
        self._text = text
        self._types = types

    @staticmethod
    def parse_enum(enum_body: str) -> list[tuple[str, int]]:
        """Parse enum members, honouring implicit ordinals and hex; raise on an unrecognised form.

        Fail-closed on purpose: a member shape we cannot parse (a value referencing another
        constant, an unexpected token) breaks the build rather than silently dropping the constant.
        """
        members: list[tuple[str, int]] = []
        next_value = 0
        for raw in enum_body.split(","):
            member = raw.strip()
            if not member:  # trailing comma or blank line
                continue
            match = re.fullmatch(r"(\w+)\s*(?:=\s*(0[xX][0-9a-fA-F]+|\d+))?", member)
            if not match:
                raise ValueError(f"Cannot parse enum member: {member!r}")
            value = int(match.group(2), 0) if match.group(2) is not None else next_value
            members.append((match.group(1), value))
            next_value = value + 1
        return members

    def enum_members(self) -> list[tuple[str, int]]:
        """Return the (name, value) pairs of the header's single typedef enum."""
        body = re.search(r"typedef\s+enum\s+\w+\s*\{(.*?)\}", self._text, re.S).group(1)
        return self.parse_enum(body)

    def struct_fields(self) -> list[tuple[str, str]]:
        """Return the (name, ctypes-type) pairs of the mdu_issue_t struct."""
        body = re.search(r"typedef\s+struct\s+mdu_issue_t\s*\{(.*?)\}", self._text, re.S).group(1)
        fields = []
        for field in body.split(";"):
            field = field.strip()
            if not field:
                continue
            name = trailing_identifier(field, "struct field")
            fields.append((name, self._types.to_ctypes(field[: field.rfind(name)])))
        return fields

    def functions(self) -> list[CFunction]:
        """Return every DFLOWFM_IO_API_EXPORT function declaration."""
        matches = re.findall(r"DFLOWFM_IO_API_EXPORT\s+(.+?)\s+(\w+)\s*\((.*?)\)\s*;", self._text, re.S)
        return [CFunction(restype, name, params) for restype, name, params in matches]


class BindingsRenderer:
    """Renders the ctypes ABI mirror (base/bindings.py)."""

    def __init__(self, header_name: str, types: CTypeMapper) -> None:
        self._header_name = header_name
        self._types = types

    def render(self, enum_members, struct_fields, functions: list[CFunction]) -> str:
        """Render the full bindings module source."""
        builder = SourceBuilder()
        builder.line(f'"""GENERATED from {self._header_name} by scripts/generate_bindings.py - do not edit.')
        builder.blank()
        builder.line("The ctypes ABI mirror of the dflowfm_io_api C header: enum constants, the mdu_issue_t struct,")
        builder.line("and argtypes/restype for every exported function, applied to the loaded library.")
        builder.line('"""')
        builder.blank()
        builder.line("import ctypes")
        builder.blank()
        builder.line("from dflowfm_io.base.library import lib")
        builder.blank()
        builder.line("# --- enum mdu_severity_t ---")
        builder.extend([f"{name} = {value}" for name, value in enum_members])
        builder.blank(2)
        builder.line("# --- struct mdu_issue_t ---")
        builder.line("class mdu_issue_t(ctypes.Structure):")
        builder.line("    _fields_ = [")
        builder.extend([f'        ("{name}", {ctype}),' for name, ctype in struct_fields])
        builder.line("    ]")
        builder.blank(2)
        builder.line("# --- function signatures ---")
        for function in functions:
            argtypes = self._types.argtypes(function.params)
            builder.line(f"lib.{function.name}.argtypes = [{', '.join(argtypes)}]")
            builder.line(f"lib.{function.name}.restype = {self._types.to_ctypes(function.restype)}")
        return builder.render()


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
        "enum": ("ctypes.c_int32", "int", "value.value"),
        "enum_name": ("ctypes.c_char_p", "str", 'value.value.decode("utf-8")'),
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
        "enum": ("int", "ctypes.c_int32(value)"),
        "enum_name": ("str", 'value.encode("utf-8")'),
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
        builder.line(f'"""GENERATED from {self._header_name} by scripts/generate_bindings.py - do not edit.')
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
        builder.line('    """Typed get/set access to MDU properties, keyed by dotted ``section.property`` names."""')
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


class BindingsGenerator(Generator):
    """Generates base/bindings.py and mdu/model.py from the C header."""

    def __init__(
        self,
        header_path: Path = HEADER,
        bindings_out: Path = OUTPUT_BINDINGS,
        model_out: Path = OUTPUT_MODEL,
    ) -> None:
        self._header_path = header_path
        self._bindings_out = bindings_out
        self._model_out = model_out
        self._types = CTypeMapper()

    def build(self) -> list[GeneratedModule]:
        parser = HeaderParser(self._header_path.read_text(encoding="utf-8"), self._types)
        functions = parser.functions()

        bindings = BindingsRenderer(self._header_path.name, self._types).render(
            parser.enum_members(), parser.struct_fields(), functions
        )
        model, accessor_count = ModelRenderer(self._header_path.name).render(functions)
        return [
            GeneratedModule(self._bindings_out, bindings, f"{len(functions)} function signatures"),
            GeneratedModule(self._model_out, model, f"{accessor_count} MduModel accessors"),
        ]


if __name__ == "__main__":
    BindingsGenerator().run()
