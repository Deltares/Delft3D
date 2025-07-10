"""
Markdown generation based on JSON files

This script reads JSON files and generates Markdown documentation for each.

The output Markdown files are saved alongside their corresponding JSON files
with the same filename but a `.md` extension.

Usage:
    Run this script directly to convert all JSON files under the "schemas"
    directory to Markdown documentation.
"""

import json
from pathlib import Path
import sys
from typing import Dict, List, Any

SCHEMA_PATH = Path(__file__).parent.parent / "schemas"


def json_to_markdown(json_path: Path) -> str:
    """Convert a schema JSON file to a Markdown representation."""

    with open(json_path, "r", encoding="utf-8") as f:
        data = json.load(f)

    file_format = data.get("file_format_type")
    if file_format != "ini":
        raise ValueError(f"File format '{file_format}' not supported yet.")

    header = generate_header(data)
    legend = generate_legend()
    toc = generate_table_of_contents(data)
    content = generate_content(data)

    return "\n\n".join([header, legend, toc, content])


def generate_header(data: Dict[str, Any]) -> str:
    """Generate Markdown file header metadata."""

    return format_lines(
        [
            f"**File description**: {data['description']}",
            "",
            f"**File version**: {data['file_version']}",
        ]
    )


def generate_table_of_contents(data: Dict[str, Any]) -> str:
    """Generate a clickable table of contents."""

    toc = ["## Table of Contents"]

    for section in data["ini_sections"]:
        name = section["name"]
        link = get_link(f"[{name}]", name)
        toc.append(f"- {link}")

    return format_lines(toc)


def generate_legend() -> str:
    """Generate a legend for the Markdown file."""

    return format_lines(
        [
            "**Legend**",
            "- 🔬: Indicates keywords that are still experimental.",
        ]
    )


def get_link(shown: str, ref: str):
    return f"[{shown}](#{ref.lower().replace(" ", "-")})"


def generate_content(data: Dict[str, Any]) -> str:
    """Generate content for each section as Markdown tables."""

    sections_md = []
    for section in data["ini_sections"]:
        section_md = []

        name = section["name"]
        description = section.get("description", "")
        properties = section["ini_properties"]

        section_md.append("")
        section_md.append(f"# [{name}]")
        section_md.append("")
        section_md.append(description)
        section_md.append("")
        section_md.append(
            "| Keyword | Description | Default | Supported values | Unit |"
        )
        section_md.append(
            "| ------- | ----------- | ------- | ---------------- | ---- |"
        )

        for prop in properties:
            section_md.append(get_md_row(prop))

        sections_md.append(format_lines(section_md))
    return "\n\n".join(sections_md)


def get_md_row(property: Dict[str, Any]) -> str:
    """Create a Markdown table row for an INI property."""

    key = get_key_formatted(property)
    description = get_description_formatted(property)
    default = get_default_value_formatted(property)
    supported_values = get_supported_values_formatted(property)
    unit = get_unit_formatted(property)

    return f"| {key} | {description} | {default} | {supported_values} | {unit} |"


def get_key_formatted(property) -> str:
    key = f"`{property["key"]}`"

    if property.get("status") == "research":
        return key + " 🔬"

    return key


def get_description_formatted(property: Dict[str, Any]) -> str:
    return property["description"].replace("*", "\\*").replace("|", "\\|")


def get_default_value_formatted(property: Dict[str, Any]) -> str:
    default_value = property.get("default_value")

    if default_value is None:
        return ""

    if isinstance(default_value, list):
        return f"`{' '.join(map(str, default_value))}`"

    return f"`{default_value}`"


def get_supported_values_formatted(property: Dict[str, Any]) -> str:

    validation = property.get("validation")

    if validation:
        range = validation.get("range")

        if range:
            return get_range_formatted(range)

    supported_values = ""

    enum_values = property.get("enum_values")

    if enum_values:
        for value, description in enum_values.items():
            supported_values += get_supported_value_line(value, description)

    elif property["value_type"] == "intbool":
        supported_values += get_supported_value_line("0", "no")
        supported_values += get_supported_value_line("1", "yes")

    elif property["value_type"] == "bool":
        supported_values += get_list_item("False")
        supported_values += get_list_item("True")

    if supported_values != "":
        return f"<ul>{supported_values}</ul>"

    return supported_values


def get_range_formatted(range: dict) -> str:
    """Format a value range as interval notation."""

    min_incl = range.get("min_inclusive")
    min_excl = range.get("min_exclusive")
    max_incl = range.get("max_inclusive")
    max_excl = range.get("max_exclusive")

    left_bracket = "[" if min_incl is not None else "(" if min_excl is not None else ""
    right_bracket = "]" if max_incl is not None else ")" if max_excl is not None else ""

    lower = min_incl if min_incl is not None else min_excl
    upper = max_incl if max_incl is not None else max_excl

    if lower is not None and upper is not None:
        return f"`{left_bracket}{lower}, {upper}{right_bracket}`"
    elif lower is not None:
        return f"`>{lower}`" if min_excl is not None else f"`≥{lower}`"
    elif upper is not None:
        return f"`<{upper}`" if max_excl is not None else f"`≤{upper}`"

    return ""


def get_supported_value_line(value: str, description: str) -> str:
    return get_list_item(f"**`{value}`**: {description}")


def get_list_item(item: str) -> str:
    return f"<li>{item}</li>"


def get_unit_formatted(property: Dict[str, Any]) -> str:
    unit = property.get("unit")
    return f"[{unit}]" if unit else ""


def format_lines(lines: List[str]) -> str:
    return "\n".join(lines)


def run():
    for json_file in SCHEMA_PATH.glob("**/*.json"):
        markdown = json_to_markdown(json_file)
        md_path = json_file.parent / f"{json_file.stem}.md"

        with open(md_path, "w", encoding="utf-8") as f:
            print(f"Writing Markdown file {md_path.name}")
            f.write(markdown)


if __name__ == "__main__":

    try:
        run()
    except Exception as e:
        print(e)
        sys.exit(1)
