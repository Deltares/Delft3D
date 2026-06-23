import json
import os
import sys

# Maps the JSON "value_type" strings to the C++ ValueType enum names.
VALUE_TYPE_MAP = {
    "string": "String",
    "int": "Int",
    "float": "Float",
    "intbool": "IntBool",
    "path": "Path",
    "enum": "Enum",
    "intenum": "IntEnum",
    "list[path]": "PathList",
    "list[string]": "StringList",
    "list[float]": "FloatList",
    "datetime": "DateTime",
}

# Template for the generated C++ source. Literal braces are doubled so the
# string can be filled in with str.format(description=..., body=...).
CPP_TEMPLATE = """\
#include <dflowfm_io/MduSchema.h>

// This file is generated from mdu.json. Manual edits will be lost.

namespace dflowfm_io
{{

    const MduSchema MDU_SCHEMA {{
        .description = "{description}",
        .sections = {{
{body}
        }}
    }};

}} // namespace dflowfm_io
"""


def default_value_str(value):
    """Produce the human-readable default value string stored in the schema."""
    if isinstance(value, list):
        return " ".join(str(v) for v in value)
    return str(value)


def enum_entries(prop):
    """Return an ordered list of (int_key, name) pairs for an enum property.

    For "enum" the JSON keys are symbolic names and the integer index is the
    position. For "intenum" the JSON keys are the integer values themselves.
    """
    value_type = prop["value_type"]
    enum_values = prop.get("enum_values", {})
    entries = []
    if value_type == "intenum":
        for key, name in enum_values.items():
            entries.append((int(key), name))
    else:  # enum
        for index, name in enumerate(enum_values.keys()):
            entries.append((index, name))
    return entries


def render_property(prop, indent):
    """Render a single PropertySchema block."""
    pad = " " * indent
    inner = " " * (indent + 4)
    lines = [pad + "PropertySchema {"]

    required = bool(prop.get("validation", {}).get("is_required", False))
    nullable = bool(prop.get("validation", {}).get("is_nullable", False))
    value_type = VALUE_TYPE_MAP[prop["value_type"]]

    # Field names are padded to the width of the longest one so the
    # "=" signs line up in the generated C++.
    width = len(".default_value")

    def field(name, value):
        return f"{inner}{name.ljust(width)} = {value}"

    lines.append(field(".key", f'"{prop["key"]}",'))
    lines.append(field(".required", f"{'true' if required else 'false'},"))
    if nullable:
        lines.append(field(".nullable", "true,"))
    lines.append(field(".value_type", f"ValueType::{value_type},"))

    if "default_value" in prop:
        dvs = default_value_str(prop["default_value"])
        lines.append(field(".default_value", f'"{dvs}",'))

    entries = enum_entries(prop)
    if entries:
        lines.append(field(".enum_values", "{"))
        for i, (key, name) in enumerate(entries):
            comma = "," if i < len(entries) - 1 else ""
            lines.append(f'{inner}    {{{key}, "{name}"}}{comma}')
        lines.append(f"{inner}}},")

    lines.append(field(".description", f'"{prop.get("description", "")}"'))
    lines.append(pad + "}")
    return "\n".join(lines)


def render_section(section, indent):
    """Render a single SectionSchema block."""
    pad = " " * indent
    inner = " " * (indent + 4)
    properties = section.get("ini_properties", [])

    # A section is required when it contains at least one required property.
    required = any(
        p.get("validation", {}).get("is_required", False) for p in properties
    )

    lines = [pad + "SectionSchema {"]
    lines.append(f'{inner}.name        = "{section["name"]}",')
    lines.append(f"{inner}.required    = {'true' if required else 'false'},")
    lines.append(f'{inner}.description = "{section.get("description", "")}",')
    lines.append(f"{inner}.properties  = {{")

    prop_blocks = [render_property(p, indent + 8) for p in properties]
    lines.append(",\n".join(prop_blocks))

    lines.append(f"{inner}}}")
    lines.append(pad + "}")
    return "\n".join(lines)


def generate_schema_file(spec):
    """Generate the full C++ source from the parsed JSON specification."""
    sections = spec.get("ini_sections", [])
    section_blocks = [render_section(s, 12) for s in sections]

    description = spec.get("description", "")
    body = ",\n".join(section_blocks)

    return CPP_TEMPLATE.format(description=description, body=body)


def main():
    script_dir = os.path.dirname(os.path.abspath(__file__))
    input_path = os.path.join(script_dir, "..", "json", "mdu.json")
    output_path = os.path.join(script_dir, "..", "dflowfm_io", "src", "MduSchema.cpp")

    with open(input_path, "r", encoding="utf-8") as f:
        spec = json.load(f)

    source = generate_schema_file(spec)

    with open(output_path, "w", encoding="utf-8", newline="\n") as f:
        f.write(source)

    print(f"Generated {os.path.normpath(output_path)} from {os.path.normpath(input_path)}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
