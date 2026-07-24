"""Layer-2 generator: typed per-keyword MDU access from mdu.json.

Reads ``json/mdu.json`` and writes ``src/dflowfm_io/mdu/schema.py``: one class per INI section, one
typed property per keyword, each delegating to the generated :class:`MduModel` (Layer 1). This is
the typed, discoverable surface over the stringly-typed get/set of Layer 1.

Run:
    python codegen/generate_schema.py

Enum handling: enum/intenum properties use the C ABI's mdu_get_enum_name/mdu_set_enum_name (surfaced
as MduModel.get_enum_name/set_enum_name), so they read and write the enum *name* (e.g. "S") as a
str. The int<->name mapping stays in the C++ core; it is not duplicated here.

Naming: property names are the MDU keys. Keys that are not valid Python identifiers are sanitised
(digit-leading keys get a leading underscore; keys colliding with a Python keyword get a trailing
one). The dotted key sent to Layer 1 always uses the real (lower-cased) MDU key.
"""

import json
import keyword
import re
from pathlib import Path

HERE = Path(__file__).resolve().parent
MDU_JSON = HERE.parents[3] / "json" / "mdu.json"
OUTPUT = HERE.parent / "src" / "dflowfm_io" / "mdu" / "schema.py"

# mdu.json value_type -> (get accessor, set accessor, Python get type, Python set-arg type)
ACCESSORS = {
    "string": ("get_string", "set_string", "str", "str"),
    "int": ("get_int", "set_int", "int", "int"),
    "float": ("get_double", "set_double", "float", "float"),
    "intbool": ("get_bool", "set_bool", "bool", "bool"),
    "path": ("get_path", "set_path", "Path", "Path | str"),
    "datetime": ("get_datetime", "set_datetime", "datetime", "datetime"),
    "list[path]": ("get_path_list", "set_path_list", "list[Path]", "list[Path | str]"),
    "list[float]": ("get_double_list", "set_double_list", "list[float]", "list[float]"),
    "enum": ("get_enum_name", "set_enum_name", "str", "str"),
    "intenum": ("get_enum_name", "set_enum_name", "str", "str"),
}


def class_name(section: str) -> str:
    """Turn a section name (possibly multi-word) into a CamelCase '<Name>Section' class name."""
    words = re.split(r"[^0-9a-zA-Z]+", section)
    return "".join(word[:1].upper() + word[1:] for word in words if word) + "Section"


def attr_name(key: str) -> str:
    """Map an MDU key to a valid, non-colliding Python attribute name."""
    if keyword.iskeyword(key):
        return key + "_"
    if not key.isidentifier():  # e.g. digit-leading MDU keys like 1D2DLinkFile
        return "_" + key
    return key


def docstring(text: str) -> str:
    """Collapse a description to one safe line for a triple-quoted property docstring."""
    line = " ".join((text or "").split()).replace("\\", "\\\\").replace('"""', "'''")
    # A trailing double-quote would merge with the closing delimiter (""""); separate it.
    if line.endswith('"'):
        line += " "
    return line


def render_property(section: str, prop: dict) -> list[str]:
    """Render one typed get/set property (4-space indented) for a keyword.

    Raises KeyError (via the ACCESSORS lookup) if the value_type is unknown; callers rely on this
    being fail-closed so a new mdu.json type cannot silently vanish from the typed API.
    """
    value_type = prop["value_type"]
    get_fn, set_fn, get_type, set_type = ACCESSORS[value_type]
    key = prop["key"]
    attr = attr_name(key)
    dotted = f"{section}.{key}".lower()
    doc = docstring(prop.get("description", ""))
    return [
        "    @property",
        f"    def {attr}(self) -> {get_type}:",
        f'        """{doc}"""',
        f'        return self._model.{get_fn}("{dotted}")',
        "",
        f"    @{attr}.setter",
        f"    def {attr}(self, value: {set_type}) -> None:",
        f'        self._model.{set_fn}("{dotted}", value)',
    ]


def main() -> None:
    spec = json.loads(MDU_JSON.read_text(encoding="utf-8"))
    sections = spec["ini_sections"]

    # Fail closed: a value_type we do not map would otherwise vanish from the typed API silently.
    unsupported = [
        (s["name"], p["key"], p["value_type"])
        for s in sections
        for p in s["ini_properties"]
        if p["value_type"] not in ACCESSORS
    ]
    if unsupported:
        raise ValueError(f"mdu.json has value_type(s) not mapped in ACCESSORS: {unsupported}")

    lines = [
        f'"""GENERATED from {MDU_JSON.name} by codegen/generate_schema.py - do not edit.',
        "",
        "Layer 2: typed per-keyword access to MDU properties. Each section is a class of typed",
        "properties over the generated MduModel (Layer 1); MduSchema exposes them keyed by section.",
        '"""',
        "",
        "from datetime import datetime",
        "from pathlib import Path",
        "",
        "from dflowfm_io.mdu.model import MduModel",
    ]

    section_attrs = []
    seen_classes: set[str] = set()
    seen_section_attrs: set[str] = set()
    for section in sections:
        name = section["name"]
        cls = class_name(name)
        attr = re.sub(r"[^0-9a-zA-Z]+", "_", name).strip("_").lower()
        # A collision would make one class/section silently shadow another; fail closed instead.
        if cls in seen_classes or attr in seen_section_attrs:
            raise ValueError(f"Section name collision for '{name}' (class {cls!r}, attr {attr!r})")
        seen_classes.add(cls)
        seen_section_attrs.add(attr)
        section_attrs.append((attr, cls))
        lines += [
            "",
            "",
            f"class {cls}:",
            f'    """Typed access to the [{name}] MDU section."""',
            "",
            "    def __init__(self, model: MduModel):",
            "        self._model = model",
        ]
        # Seed with the section class's own backing field so no property can shadow it.
        seen_props: set[str] = {"_model"}
        for prop in section["ini_properties"]:
            member = attr_name(prop["key"])
            if member in seen_props:
                raise ValueError(f"Property name collision '{member}' in section '{name}'")
            seen_props.add(member)
            lines.append("")
            lines += render_property(name, prop)

    lines += [
        "",
        "",
        "class MduSchema:",
        '    """Typed section access over an MduModel: the sections of an MDU file as typed objects."""',
        "",
        "    def __init__(self, model: MduModel):",
    ]
    for attr, cls in section_attrs:
        lines.append(f"        self.{attr} = {cls}(model)")

    OUTPUT.write_text("\n".join(lines) + "\n", encoding="utf-8")
    total = sum(len(s["ini_properties"]) for s in sections)
    print(f"Wrote {len(sections)} sections, {total} typed properties to {OUTPUT}")


if __name__ == "__main__":
    main()
