"""Layer-2 generator: typed per-keyword MDU access from mdu.json.

Reads `json/mdu.json` and writes `src/dflowfm_io/mdu/schema.py`: one class per INI section, one
typed property per keyword, each delegating to the generated :class:`MduModel` (Layer 1). This is
the typed, discoverable surface over the stringly-typed get/set of Layer 1.

Run:
    python scripts/generate_schema.py

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

from generator_base import GeneratedModule, Generator, SourceBuilder

HERE = Path(__file__).resolve().parent
MDU_JSON = HERE.parents[3] / "json" / "mdu.json"
OUTPUT = HERE.parent / "src" / "dflowfm_io" / "mdu" / "schema.py"


class NameSanitizer:
    """Maps MDU section and keyword names to valid, non-colliding Python identifiers."""

    @staticmethod
    def attribute(key: str) -> str:
        """Map an MDU keyword to a valid property/attribute name."""
        if keyword.iskeyword(key):
            return key + "_"
        if not key.isidentifier():  # e.g. digit-leading MDU keys like 1D2DLinkFile
            return "_" + key
        return key

    @staticmethod
    def section_class(name: str) -> str:
        """Turn a section name (possibly multi-word) into a CamelCase '<Name>Section' class name."""
        words = re.split(r"[^0-9a-zA-Z]+", name)
        return "".join(word[:1].upper() + word[1:] for word in words if word) + "Section"

    @staticmethod
    def section_attribute(name: str) -> str:
        """Turn a section name into the snake-ish attribute used on MduSchema."""
        return re.sub(r"[^0-9a-zA-Z]+", "_", name).strip("_").lower()


class SchemaRenderer:
    """Renders the typed MduSchema module (mdu/schema.py) from parsed sections."""

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
        # A string enum carries a label, surfaced by name; an intenum has no label in the schema
        # (EnumValueSchema.label is empty), so it is surfaced by its integer value.
        "enum": ("get_enum_name", "set_enum_name", "str", "str"),
        "intenum": ("get_enum", "set_enum", "int", "int"),
    }

    def __init__(self, json_name: str, names: NameSanitizer | None = None) -> None:
        self._json_name = json_name
        self._names = names or NameSanitizer()

    @staticmethod
    def docstring(text: str) -> str:
        """Collapse a description to one safe line for a triple-quoted property docstring."""
        line = " ".join((text or "").split()).replace("\\", "\\\\").replace('"""', "'''")
        # A trailing double-quote would merge with the closing delimiter (""""); separate it.
        if line.endswith('"'):
            line += " "
        return line

    def render_property(self, section: str, prop: dict) -> list[str]:
        """Render one typed get/set property (4-space indented) for a keyword.

        Raises KeyError (via the ACCESSORS lookup) if the value_type is unknown; callers rely on
        this being fail-closed so a new mdu.json type cannot silently vanish from the typed API.
        """
        get_fn, set_fn, get_type, set_type = self.ACCESSORS[prop["value_type"]]
        attr = self._names.attribute(prop["key"])
        dotted = f"{section}.{prop['key']}".lower()
        doc = self.docstring(prop.get("description", ""))
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

    def render(self, sections: list[dict]) -> tuple[str, int]:
        """Render the full schema module; return (source, number of typed properties)."""
        builder = SourceBuilder()
        builder.line(f'"""GENERATED from {self._json_name} by scripts/generate_schema.py - do not edit.')
        builder.blank()
        builder.line("Layer 2: typed per-keyword access to MDU properties. Each section is a class of typed")
        builder.line("properties over the generated MduModel (Layer 1); MduSchema exposes them keyed by section.")
        builder.line('"""')
        builder.blank()
        builder.line("from datetime import datetime")
        builder.line("from pathlib import Path")
        builder.blank()
        builder.line("from dflowfm_io.mdu.model import MduModel")

        section_attrs = self._render_sections(builder, sections)
        self._render_facade(builder, section_attrs)

        total = sum(len(section["ini_properties"]) for section in sections)
        return builder.render(), total

    def _render_sections(self, builder: SourceBuilder, sections: list[dict]) -> list[tuple[str, str]]:
        """Emit one class per section; return the (attribute, class) pairs for the facade.

        Fails closed on any name collision — two sections mapping to the same class/attribute, or two
        keys mapping to the same property (including the reserved `_model` backing field).
        """
        section_attrs: list[tuple[str, str]] = []
        seen_classes: set[str] = set()
        seen_section_attrs: set[str] = set()
        for section in sections:
            name = section["name"]
            cls = self._names.section_class(name)
            attr = self._names.section_attribute(name)
            if cls in seen_classes or attr in seen_section_attrs:
                raise ValueError(f"Section name collision for '{name}' (class {cls!r}, attr {attr!r})")
            seen_classes.add(cls)
            seen_section_attrs.add(attr)
            section_attrs.append((attr, cls))

            builder.blank(2)
            builder.line(f"class {cls}:")
            builder.line(f'    """Typed access to the [{name}] MDU section."""')
            builder.blank()
            builder.line("    def __init__(self, model: MduModel):")
            builder.line("        self._model = model")

            seen_props: set[str] = {"_model"}  # attribute names — the backing field must not be shadowed
            seen_keys: set[str] = set()  # lower-cased keys — the underlying Layer-1 key is lower-cased
            for prop in section["ini_properties"]:
                member = self._names.attribute(prop["key"])
                lower_key = prop["key"].lower()
                if member in seen_props:
                    raise ValueError(f"Property name collision '{member}' in section '{name}'")
                if lower_key in seen_keys:  # two keys differing only by case alias the same Layer-1 key
                    raise ValueError(f"Case-only key collision '{prop['key']}' in section '{name}'")
                seen_props.add(member)
                seen_keys.add(lower_key)
                builder.blank()
                builder.extend(self.render_property(name, prop))
        return section_attrs

    def _render_facade(self, builder: SourceBuilder, section_attrs: list[tuple[str, str]]) -> None:
        """Emit the MduSchema class exposing each section as a typed attribute."""
        builder.blank(2)
        builder.line("class MduSchema:")
        builder.line('    """Typed section access over an MduModel: the sections of an MDU file as typed objects."""')
        builder.blank()
        builder.line("    def __init__(self, model: MduModel):")
        for attr, cls in section_attrs:
            builder.line(f"        self.{attr} = {cls}(model)")


class SchemaGenerator(Generator):
    """Generates the typed mdu/schema.py from mdu.json."""

    def __init__(self, mdu_json: Path = MDU_JSON, output: Path = OUTPUT) -> None:
        self._mdu_json = mdu_json
        self._output = output

    def build(self) -> list[GeneratedModule]:
        sections = json.loads(self._mdu_json.read_text(encoding="utf-8"))["ini_sections"]
        self._require_all_types_supported(sections)

        source, total = SchemaRenderer(self._mdu_json.name).render(sections)
        summary = f"{len(sections)} sections, {total} typed properties"
        return [GeneratedModule(self._output, source, summary)]

    @staticmethod
    def _require_all_types_supported(sections: list[dict]) -> None:
        """Fail closed: a value_type we do not map would otherwise vanish from the typed API."""
        unsupported = [
            (section["name"], prop["key"], prop["value_type"])
            for section in sections
            for prop in section["ini_properties"]
            if prop["value_type"] not in SchemaRenderer.ACCESSORS
        ]
        if unsupported:
            raise ValueError(f"mdu.json has value_type(s) not mapped in ACCESSORS: {unsupported}")


if __name__ == "__main__":
    SchemaGenerator().run()
