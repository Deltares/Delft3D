import argparse
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

# Maps the JSON "status" strings to the C++ StatusType enum names.
STATUS_TYPE_MAP = {
    "GA": "GA",
    "research": "Research",
    "deprecated": "Deprecated",
    "obsolete": "Obsolete",
}

# Template for the generated C++ source. Literal braces are doubled so the
# string can be filled in with str.format(description=..., body=...).
CPP_TEMPLATE = """\
#include <dflowfm_io/MduSchema.h>

// This file is generated from mdu.json. Manual edits will be lost.

namespace dflowfm_io
{{
    /// @brief Builds an @ref MduSchema from the MDU specification.
    inline MduSchema BuildMduSchema()
    {{
        return MduSchema {{
            "{description}",
            {{
{body}
            }}
        }};
    }}

}} // namespace dflowfm_io
"""


def default_value_str(value):
    """Produce the human-readable default value string stored in the schema."""
    if isinstance(value, list):
        return " ".join(str(v) for v in value)
    return str(value)


def enum_entries(prop):
    """Return an ordered list of (int_key, label, status) tuples for an enum property."""
    value_type = prop["value_type"]
    enum_values = prop.get("enum_values", {})
    entries = []
    for index, (key, entry) in enumerate(enum_values.items()):
        int_key = int(key) if value_type == "intenum" else index
        label = None if value_type == "intenum" else key
        status = entry.get("status", {}) if isinstance(entry, dict) else {}
        entries.append((int_key, label, status))
    return entries


def render_enum_value(value, label, status, indent):
    """Render a single EnumValueSchema block."""
    pad = " " * indent
    inner = " " * (indent + 4)
    sub_width = len(".status") if status else len(".value")
    lines = [pad + "EnumValueSchema {"]
    lines.append(f'{inner}{".value".ljust(sub_width)} = {value},')
    if label is not None:
        lines.append(f'{inner}{".label".ljust(sub_width)} = "{label}",')
    if status:
        status_type = STATUS_TYPE_MAP[status["value"]]
        comment = status.get("comment", "")
        sub_status_width = len(".comment") if comment else len(".type")
        lines.append(f'{inner}{".status".ljust(sub_width)} = {{')
        lines.append(f'{inner}    {".type".ljust(sub_status_width)} = StatusType::{status_type},')
        if comment:
            lines.append(f'{inner}    {".comment".ljust(sub_status_width)} = "{comment}",')
        lines.append(f'{inner}}},')
    lines.append(pad + "}")
    return "\n".join(lines)


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
    if required:
        lines.append(field(".required", "true,"))
    if nullable:
        lines.append(field(".nullable", "true,"))
    lines.append(field(".value_type", f"ValueType::{value_type},"))

    if "default_value" in prop:
        dvs = default_value_str(prop["default_value"])
        lines.append(field(".default_value", f'"{dvs}",'))

    entries = enum_entries(prop)
    if entries:
        lines.append(field(".enum_values", "{"))
        enum_blocks = [render_enum_value(value, label, status, indent + 8) for value, label, status in entries]
        lines.append(",\n".join(enum_blocks))
        lines.append(f"{inner}}},")

    if "format" in prop:
        lines.append(field(".format", f'"{prop["format"]}",'))

    lines.append(field(".description", f'"{prop.get("description", "")}",'))

    status = prop.get("status", {})
    if status:
        status_type = STATUS_TYPE_MAP[status["value"]]
        comment = status.get("comment", "")
        sub_width = len(".comment") if comment else len(".type")
        lines.append(field(".status", "{"))
        lines.append(f'{inner}    {".type".ljust(sub_width)} = StatusType::{status_type},')
        if comment:
            lines.append(f'{inner}    {".comment".ljust(sub_width)} = "{comment}",')
        lines.append(f"{inner}}},")

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
    if required:
        lines.append(f"{inner}.required    = true,")
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
    section_blocks = [render_section(s, 16) for s in sections]

    description = spec.get("description", "")
    body = ",\n".join(section_blocks)

    return CPP_TEMPLATE.format(description=description, body=body)


def main():
    parser = argparse.ArgumentParser(description="Generate the C++ MDU schema source from mdu.json.")
    parser.add_argument("input", help="Path to the mdu.json specification.")
    parser.add_argument("output", help="Path of the C++ source file to generate.")
    args = parser.parse_args()

    with open(args.input, "r", encoding="utf-8") as f:
        spec = json.load(f)

    source = generate_schema_file(spec)

    output_dir = os.path.dirname(os.path.abspath(args.output))
    os.makedirs(output_dir, exist_ok=True)

    with open(args.output, "w", encoding="utf-8", newline="\n") as f:
        f.write(source)

    print(f"Generated {os.path.normpath(args.output)} from {os.path.normpath(args.input)}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
