"""Generate the ctypes binding layer from the dflowfm_io_api C header.

Reads `dflowfm_io_api.h` and writes `src/dflowfm_io/base/bindings.py`: the enum constants, the
`mdu_issue_t` struct, and the `argtypes`/`restype` for every exported function, applied to the
`lib` loaded by `dflowfm_io.base.library`. This is the ABI-coupled layer — regenerate it whenever
the C API changes so the Python side can never drift from the DLL.

Run:
    python codegen/generate_bindings.py

The header here is small and regular (flat `extern "C"` declarations), so this parses it directly
rather than depending on a C preprocessor.
"""

import re
from pathlib import Path

HERE = Path(__file__).resolve().parent  # .../dflowfm_io_api/wrappers/python/codegen
HEADER = HERE.parents[2] / "include" / "dflowfm_io_api" / "dflowfm_io_api.h"
OUTPUT = HERE.parent / "src" / "dflowfm_io" / "base" / "bindings.py"

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


def main() -> None:
    text = HEADER.read_text(encoding="utf-8")

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

    functions = re.findall(
        r"DFLOWFM_IO_API_EXPORT\s+(.+?)\s+(\w+)\s*\((.*?)\)\s*;", text, re.S
    )

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

    OUTPUT.write_text("\n".join(lines) + "\n", encoding="utf-8")
    print(f"Wrote {len(functions)} function signatures to {OUTPUT}")


if __name__ == "__main__":
    main()
