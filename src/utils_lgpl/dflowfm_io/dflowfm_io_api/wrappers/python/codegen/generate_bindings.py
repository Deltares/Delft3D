"""Generate the Python wrapper's ABI-coupled code from the dflowfm_io_api C header.

Reads ``dflowfm_io_api.h`` and writes two generated modules, so the Python side can never drift
from the DLL:

* ``src/dflowfm_io/base/bindings.py`` — the ctypes ABI mirror: enum constants, the ``mdu_issue_t``
  struct, and ``argtypes``/``restype`` for every exported function, applied to the loaded ``lib``.
* ``src/dflowfm_io/mdu/model.py`` — the typed :class:`MduModel`, one get/set method per
  ``mdu_get_*``/``mdu_set_*`` function, with the per-type value marshalling.

Run:
    python codegen/generate_bindings.py

The header is small and regular (flat ``extern "C"`` declarations), so this parses it directly
rather than depending on a C preprocessor. The lifecycle (``MduDocument``), the report
(``MduReport``), and the ``Issue``/``Severity`` value types stay hand-written: they encode Python
design decisions (ownership, ``__del__``, convenience methods) with no 1:1 C-function counterpart.
"""

import re
from pathlib import Path

HERE = Path(__file__).resolve().parent  # .../dflowfm_io_api/wrappers/python/codegen
HEADER = HERE.parents[2] / "include" / "dflowfm_io_api" / "dflowfm_io_api.h"
SRC = HERE.parent / "src" / "dflowfm_io"
OUTPUT_BINDINGS = SRC / "base" / "bindings.py"
OUTPUT_MODEL = SRC / "mdu" / "model.py"

# C base type -> ctypes expression. `char` is handled specially (char* is c_char_p, not POINTER(c_char)).
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

# --- MduModel marshalling tables, keyed on the type suffix of the C function name ---
# The function name encodes the semantic type (mdu_get_path vs mdu_get_string), which the raw
# ctypes signature does not, so these tables drive the get/set method bodies.

# scalar getter: suffix -> (ctypes box type, Python return type, expression converting `value`)
GET_SCALAR = {
    "int": ("ctypes.c_int32", "int", "value.value"),
    "bool": ("ctypes.c_int32", "bool", "value.value != 0"),
    "double": ("ctypes.c_double", "float", "value.value"),
    "enum": ("ctypes.c_int32", "int", "value.value"),
    "enum_name": ("ctypes.c_char_p", "str", 'value.value.decode("utf-8")'),
    "string": ("ctypes.c_char_p", "str", 'value.value.decode("utf-8")'),
    "path": ("ctypes.c_char_p", "Path", 'Path(value.value.decode("utf-8"))'),
    "datetime": ("ctypes.c_int64", "datetime", "datetime.fromtimestamp(value.value, tz=timezone.utc)"),
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
    "datetime": ("datetime", "ctypes.c_int64(int(value.timestamp()))"),
}

# list setter: suffix -> (Python argument type, element ctypes type, expression encoding each `v` — None = identity)
SET_LIST = {
    "string": ("list[str]", "ctypes.c_char_p", 'v.encode("utf-8")'),
    "path": ("list[Path | str]", "ctypes.c_char_p", 'str(v).encode("utf-8")'),
    "double": ("list[float]", "ctypes.c_double", None),
}


def c_to_ctypes(decl: str) -> str:
    """Map a C type declaration (possibly with `const` and `*`) to a ctypes expression."""
    stars = decl.count("*")
    base = decl.replace("const", "").replace("*", "").strip()
    if base == "char":  # char* -> c_char_p; each extra * wraps once more
        result = "ctypes.c_char_p"
        wraps = stars - 1
    else:
        result = BASE_TYPES[base]
        wraps = stars
    for _ in range(wraps):
        result = f"ctypes.POINTER({result})"
    return result


def parse_params(param_str: str) -> list[str]:
    """Return the ctypes argtypes for a comma-separated C parameter list."""
    param_str = param_str.strip()
    if param_str in ("", "void"):
        return []
    argtypes = []
    for param in param_str.split(","):
        name = re.search(r"(\w+)\s*$", param).group(1)
        type_decl = param[: param.rfind(name)]
        argtypes.append(c_to_ctypes(type_decl))
    return argtypes


def parse_header(text: str):
    """Return (enum_members, struct_fields, functions) parsed from the header text."""
    enum_body = re.search(r"typedef\s+enum\s+\w+\s*\{(.*?)\}", text, re.S).group(1)
    enum_members = re.findall(r"(\w+)\s*=\s*(\d+)", enum_body)

    struct_body = re.search(r"typedef\s+struct\s+mdu_issue_t\s*\{(.*?)\}", text, re.S).group(1)
    struct_fields = []
    for field in struct_body.split(";"):
        field = field.strip()
        if not field:
            continue
        name = re.search(r"(\w+)\s*$", field).group(1)
        struct_fields.append((name, c_to_ctypes(field[: field.rfind(name)])))

    functions = re.findall(r"DFLOWFM_IO_API_EXPORT\s+(.+?)\s+(\w+)\s*\((.*?)\)\s*;", text, re.S)
    return enum_members, struct_fields, functions


def render_bindings(enum_members, struct_fields, functions) -> str:
    """Render the ctypes ABI mirror (bindings.py)."""
    lines = [
        f'"""GENERATED from {HEADER.name} by codegen/generate_bindings.py - do not edit.',
        "",
        "The ctypes ABI mirror of the dflowfm_io_api C header: enum constants, the mdu_issue_t struct,",
        "and argtypes/restype for every exported function, applied to the loaded library.",
        '"""',
        "",
        "import ctypes",
        "",
        "from dflowfm_io.base.library import lib",
        "",
        "# --- enum mdu_severity_t ---",
    ]
    lines += [f"{name} = {value}" for name, value in enum_members]
    lines += [
        "",
        "",
        "# --- struct mdu_issue_t ---",
        "class mdu_issue_t(ctypes.Structure):",
        "    _fields_ = [",
    ]
    lines += [f'        ("{name}", {ctype}),' for name, ctype in struct_fields]
    lines += ["    ]", "", "", "# --- function signatures ---"]
    for restype, name, params in functions:
        argtypes = parse_params(params)
        lines.append(f"lib.{name}.argtypes = [{', '.join(argtypes)}]")
        lines.append(f"lib.{name}.restype = {c_to_ctypes(restype)}")
    return "\n".join(lines) + "\n"


def _accessor_method(kind: str, suffix: str, is_list: bool, cname: str) -> list[str]:
    """Render one MduModel get/set method (4-space indented) for a C accessor function."""
    method = f"{kind}_{suffix}{'_list' if is_list else ''}"
    call_handle = 'self._ref.handle, key.encode("utf-8")'

    if kind == "get" and not is_list:
        box, pyret, convert = GET_SCALAR[suffix]
        return [
            f"    def {method}(self, key: str) -> {pyret}:",
            f"        value = {box}()",
            f"        check_result(lib.{cname}({call_handle}, ctypes.byref(value)))",
            f"        return {convert}",
        ]
    if kind == "get" and is_list:
        elem, pyret, convert = GET_LIST[suffix]
        return [
            f"    def {method}(self, key: str) -> {pyret}:",
            f"        array_out = ctypes.POINTER({elem})()",
            "        size_out = ctypes.c_uint64()",
            f"        check_result(lib.{cname}({call_handle}, ctypes.byref(array_out), ctypes.byref(size_out)))",
            f"        return [{convert} for i in range(size_out.value)]",
        ]
    if kind == "set" and not is_list:
        pyarg, c_arg = SET_SCALAR[suffix]
        return [
            f"    def {method}(self, key: str, value: {pyarg}) -> None:",
            f"        check_result(lib.{cname}({call_handle}, {c_arg}))",
        ]
    # set list
    pyarg, elem, encode = SET_LIST[suffix]
    encoded = "list(values)" if encode is None else f"[{encode} for v in values]"
    return [
        f"    def {method}(self, key: str, values: {pyarg}) -> None:",
        f"        encoded = {encoded}",
        f"        arr = ({elem} * len(encoded))(*encoded)",
        f"        check_result(lib.{cname}({call_handle}, arr, ctypes.c_uint64(len(encoded))))",
    ]


def render_model(functions) -> tuple[str, int]:
    """Render the typed MduModel (mdu/model.py). Returns (source, method_count)."""
    lines = [
        f'"""GENERATED from {HEADER.name} by codegen/generate_bindings.py - do not edit.',
        "",
        "The typed MduModel: one get/set method per mdu_get_*/mdu_set_* C function, with the per-type",
        "value marshalling. Regenerated from the C header, so the accessors cannot drift from the ABI.",
        '"""',
        "",
        "import ctypes",
        "from datetime import datetime, timezone",
        "from pathlib import Path",
        "",
        "from dflowfm_io.base import HandleRef, check_result, lib",
        "",
        "",
        "class MduModel:",
        '    """Typed get/set access to MDU properties, keyed by dotted ``section.property`` names."""',
        "",
        "    def __init__(self, ref: HandleRef):",
        "        self._ref = ref",
    ]
    count = 0
    for _restype, cname, _params in functions:
        if cname == "mdu_get_issue_list":  # belongs to MduReport, not the typed model
            continue
        if cname.startswith("mdu_get_"):
            kind, rest = "get", cname[len("mdu_get_") :]
        elif cname.startswith("mdu_set_"):
            kind, rest = "set", cname[len("mdu_set_") :]
        else:  # lifecycle: create/destroy/load/save — hand-written in MduDocument
            continue
        is_list = rest.endswith("_list")
        suffix = rest[: -len("_list")] if is_list else rest
        lines.append("")
        lines += _accessor_method(kind, suffix, is_list, cname)
        count += 1
    return "\n".join(lines) + "\n", count


def main() -> None:
    text = HEADER.read_text(encoding="utf-8")
    enum_members, struct_fields, functions = parse_header(text)

    OUTPUT_BINDINGS.write_text(render_bindings(enum_members, struct_fields, functions), encoding="utf-8")
    print(f"Wrote {len(functions)} function signatures to {OUTPUT_BINDINGS}")

    model_source, method_count = render_model(functions)
    OUTPUT_MODEL.write_text(model_source, encoding="utf-8")
    print(f"Wrote {method_count} MduModel accessors to {OUTPUT_MODEL}")


if __name__ == "__main__":
    main()
