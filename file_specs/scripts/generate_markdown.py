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
from dataclasses import dataclass
from datetime import datetime
from pathlib import Path
import sys
from typing import List, Optional

from ini_spec import (
    DateTimeFormatType,
    FileFormatType,
    IniFileSpec,
    IniPropertySpec,
    Range,
    Status,
    StatusValue,
    ValueType
)

KEYWORDS_SECTION_TITLE = "Supported keywords and options"
RESEARCH_SECTION_TITLE = "Research keywords and options"
DEPRECATED_SECTION_TITLE = "Deprecated keywords and options"
OBSOLETE_SECTION_TITLE = "Obsolete keywords and options"

SPECIFICATIONS_FOLDER = Path(__file__).parent.parent / "specifications"

STATUS_ICONS = {StatusValue.RESEARCH: "🔬", StatusValue.DEPRECATED: "⚠️"}

@dataclass
class FileItem:
    """A file item representing a section, keyword or keyword value in the file."""

    status: Status
    section: str
    key: Optional[str] = None
    value: Optional[str] = None
    description: Optional[str] = None

    @property
    def identifier(self) -> str:
        """Return a unique identifier for the item."""
        identifier = f"[{self.section}]"
        if self.key:
            identifier += f" {self.key}"
            if self.value:
                identifier += f"={self.value}"
        return identifier


def is_supported(status: Optional[Status]) -> bool:
    return status is None or status.value != StatusValue.OBSOLETE


def json_to_markdown(ini_file_spec: IniFileSpec) -> str:
    """Convert an INI specification to a Markdown representation."""

    if ini_file_spec.file_format_type != FileFormatType.INI:
        raise ValueError(f"File format '{ini_file_spec.file_format_type}' not supported yet.")

    research_items = collect_items_with_status(ini_file_spec, StatusValue.RESEARCH)
    deprecated_items = collect_items_with_status(ini_file_spec, StatusValue.DEPRECATED)
    obsolete_items = collect_items_with_status(ini_file_spec, StatusValue.OBSOLETE)

    header = generate_header(ini_file_spec)

    toc = generate_table_of_contents(
        ini_file_spec,
        len(research_items) > 0,
        len(deprecated_items) > 0,
        len(obsolete_items) > 0
    )
    content = generate_supported_content(ini_file_spec)

    parts = [header, toc, content]

    if len(research_items) > 0:
        parts.append(generate_research_content(research_items))

    if len(deprecated_items) > 0:
        parts.append(generate_deprecated_section(deprecated_items))

    if len(obsolete_items) > 0:
        parts.append(generate_obsolete_section(obsolete_items))

    return "\n\n".join(parts)


def generate_header(ini_file_spec: IniFileSpec) -> str:
    """Generate Markdown file header metadata."""

    return format_lines(
        [
            f"**File description**: {ini_file_spec.description}",
            "",
            f"**File version**: {ini_file_spec.file_version}",
        ]
    )


def generate_table_of_contents(
    ini_file_spec: IniFileSpec,
    has_research: bool = False,
    has_deprecated: bool = False,
    has_obsolete: bool = False,
) -> str:
    """Generate a clickable table of contents."""

    toc = ["## Table of Contents"]

    toc.append(f"- {get_anchor_link(KEYWORDS_SECTION_TITLE, KEYWORDS_SECTION_TITLE)}")
    for section in ini_file_spec.ini_sections:
        if not is_supported(section.status):
            continue
        if any(is_supported(prop.status) for prop in section.ini_properties):
            marker = get_status_marker(section.status)
            link = get_anchor_link(f"[{section.name}]{marker}", f"{KEYWORDS_SECTION_TITLE} {section.name}")
            toc.append(f"  - {link}")

    if has_research:
        toc.append(f"- {get_anchor_link(RESEARCH_SECTION_TITLE, RESEARCH_SECTION_TITLE)}")

    if has_deprecated:
        toc.append(f"- {get_anchor_link(DEPRECATED_SECTION_TITLE, DEPRECATED_SECTION_TITLE)}")

    if has_obsolete:
        toc.append(f"- {get_anchor_link(OBSOLETE_SECTION_TITLE, OBSOLETE_SECTION_TITLE)}")

    return format_lines(toc)


def get_anchor_link(shown: str, ref: str):
    return f"[{shown}](#{get_anchor_id(ref)})"


def format_anchor(value: str) -> str:
    anchor_id = get_anchor_id(value)
    return f"<a id=\"{anchor_id}\"></a>"

def get_anchor_id(value: str) -> str:
    return value.lower().replace(" ", "-")


def generate_supported_content(ini_file_spec: IniFileSpec) -> str:
    """Generate content for each section as Markdown tables."""


    sections_md = [
        f"{format_anchor(KEYWORDS_SECTION_TITLE)}",
        f"# {KEYWORDS_SECTION_TITLE}",
    ]

    legend_lines = [
            "**Legend**",
            "- 🔬: Indicates keywords or options that are still experimental.",
            "- ⚠️: Indicates keywords or options that are deprecated and will be removed in future release versions.",
        ]
    sections_md.extend(legend_lines)
    
    for section in ini_file_spec.ini_sections:
        
        if not is_supported(section.status):
            continue

        properties = [
            property
            for property in section.ini_properties
            if is_supported(property.status)
        ]
        
        if len(properties) == 0:
            continue

        section_md = []

        section_md.append("")
        section_md.append(f"{format_anchor(f'{KEYWORDS_SECTION_TITLE} {section.name}')}")
        section_md.append(f"## [{section.name}]{get_status_marker(section.status)}")
        section_md.append("")
        section_md.append(section.description)
        section_md.append("")
        section_md.append(
            "| Keyword | Description | Default | Supported values | Unit | Format |"
        )
        section_md.append(
            "| ------- | ----------- | ------- | ---------------- | ---- | ------ |"
        )

        for property in properties:
            section_md.append(get_md_row(property))

        sections_md.append(format_lines(section_md))
    return "\n\n".join(sections_md)


def generate_research_content(
    research_items: List[FileItem]
) -> str:
    """Generate one combined list of research items."""

    lines = [
        f"{format_anchor(RESEARCH_SECTION_TITLE)}",
        f"# {RESEARCH_SECTION_TITLE}",
        "",
        "The keyword and options listed below are available for research "
        "purposes.",
        "",
        "",
    ]

    table_rows = create_status_table_rows(research_items)
    lines.extend(table_rows)

    return format_lines(lines)

def create_status_table_rows(items: List[FileItem]) -> List[str]:
    lines = [
        "| Keyword/value | Notes |",
        "| ------------- | ----- |",
    ]

    alphabetically_sorted = sorted(items, key=lambda x: x.identifier.lower())

    for item in alphabetically_sorted:
        notes = escape_table_characters(item.status.comment) if item.status.comment else ""
        lines.append(f"| `{item.identifier}` | {notes} |")

    return lines

def get_md_row(ini_property: IniPropertySpec) -> str:
    """Create a Markdown table row for an INI property."""

    key = get_key_formatted(ini_property)
    description = get_description_formatted(ini_property)
    default = get_default_value_formatted(ini_property)
    supported_values = get_supported_values_formatted(ini_property)
    unit = get_unit_formatted(ini_property)
    format = get_format_formatted(ini_property)

    return f"| {key} | {description} | {default} | {supported_values} | {unit} | {format} |"


def get_key_formatted(ini_property: IniPropertySpec) -> str:
    marker = get_status_marker(ini_property.status)
    return f"`{ini_property.key}`{marker}"


def get_description_formatted(ini_property: IniPropertySpec) -> str:
    return escape_table_characters(ini_property.description or "")


def get_default_value_formatted(ini_property: IniPropertySpec) -> str:
    default_value = ini_property.default_value

    if default_value is None:
        return ""

    if isinstance(default_value, (str, list)) and not default_value:
        return '`<empty>`'

    if isinstance(default_value, list):
        return f"`{' '.join(map(str, default_value))}`"

    return f"`{default_value}`"


def get_supported_values_formatted(ini_property: IniPropertySpec) -> str:

    if ini_property.validation and ini_property.validation.range:
        return get_range_formatted(ini_property.validation.range)

    supported_values = ""

    if ini_property.enum_values:
        for name, enum_value in ini_property.enum_values.items():
            if not is_supported(enum_value.status):
                continue

            supported_values += get_supported_value_line(
                name,
                enum_value.description,
                enum_value.status,
            )

    elif ini_property.value_type == ValueType.INTBOOL:
        supported_values += get_supported_value_line("0", "no")
        supported_values += get_supported_value_line("1", "yes")

    if supported_values != "":
        return f"<ul>{supported_values}</ul>"

    return supported_values


def get_range_formatted(range: Range) -> str:
    """Format a value range as interval notation."""

    min_incl = range.min_inclusive
    min_excl = range.min_exclusive
    max_incl = range.max_inclusive
    max_excl = range.max_exclusive

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


def get_supported_value_line(
    value: str,
    description: str,
    status = None,
) -> str:
    marker = get_status_marker(status)
    return get_list_item(f"**`{value}`**{marker}: {description}")


def get_status_marker(status: Optional[Status]) -> str:
    if status is None or status.value not in STATUS_ICONS:
        return ""
    return f" {STATUS_ICONS[status.value]}"


def get_list_item(item: str) -> str:
    return f"<li>{item}</li>"


def get_unit_formatted(ini_property: IniPropertySpec) -> str:
    return f"[{ini_property.unit}]" if ini_property.unit else ""


def get_format_formatted(ini_property: IniPropertySpec) -> str:
    if isinstance(ini_property.format, DateTimeFormatType):
        if ini_property.format == DateTimeFormatType.DATE:
            return "`yyyymmdd`"
        if ini_property.format == DateTimeFormatType.DATETIME:
            return "`yyyymmddhhmmss`"
        else:
            raise ValueError(f"Unknown DateTimeFormatType: {ini_property.format}")

    return ""

def collect_items_with_status(ini_file_spec: IniFileSpec, status_value: StatusValue) -> List[FileItem]:
    """Collect sections, keywords and keyword values with a specific status across all sections.

    Returns a list of items with the specified status.
    """

    items: List[FileItem] = []

    for ini_section in ini_file_spec.ini_sections:
        if ini_section.status and ini_section.status.value == status_value:
            items.append(
                FileItem(
                    section=ini_section.name,
                    description=ini_section.description,
                    status=ini_section.status,
                )
            )

        for ini_property in ini_section.ini_properties:
            if ini_property.status and ini_property.status.value == status_value:
                items.append(
                    FileItem(
                        section=ini_section.name,
                        key=ini_property.key,
                        description=ini_property.description,
                        status=ini_property.status
                    )
                )
                continue

            if ini_property.enum_values is not None:
                for name, enum_value in ini_property.enum_values.items():
                    if enum_value.status and enum_value.status.value == status_value:
                        items.append(
                            FileItem(
                                section=ini_section.name,
                                key=ini_property.key,
                                value=name,
                                description=enum_value.description,
                                status=enum_value.status
                            )
                        )

    return items


def format_release(release: Optional[float]) -> str:
    """Format a release identifier (e.g. 2026.02) for display."""
    if isinstance(release, (int, float)):
        return f"{release:.2f}"
    return str(release)

def generate_deprecated_section(
    deprecated_items: List[FileItem]
) -> str:
    """Generate one combined list of deprecated items."""

    lines = [
        f"{format_anchor(DEPRECATED_SECTION_TITLE)}",
        f"# {DEPRECATED_SECTION_TITLE}",
        "",
        "The keywords and options listed below are deprecated and will be "
        "removed in a future release. If your model still uses one of them, "
        "please update it as soon as possible by following the notes or user manual.",
        "",
        "",
    ]

    table = create_status_table_rows(deprecated_items)

    lines.extend(table)

    return format_lines(lines)


def generate_obsolete_section(
    obsolete_items: List[FileItem]
) -> str:
    """Generate the overview of obsolete sections, keywords and keyword values per release."""

    releases = sorted(
        {item.status.since_release for item in obsolete_items},
        key=lambda r: (r is None, r),
        reverse=True,
    )

    lines = [
        f"{format_anchor(OBSOLETE_SECTION_TITLE)}",
        f"# {OBSOLETE_SECTION_TITLE}",
        "",
        "The keywords and options listed below are no longer supported and "
        "will cause an error if used in D-Flow FM. They are grouped by the release in which they "
        "became obsolete, most recent first. If your model still uses one of them, "
        "please update it by following the notes or user manual.",
    ]

    for release in releases:
        release_label = f"Release {format_release(release)}"
        lines.append("")
        lines.append(f"{format_anchor(release_label)}")
        lines.append(f"## {release_label}")

        obsolete_for_release = [item for item in obsolete_items if item.status.since_release == release]
    
        if obsolete_for_release:
            lines.append("")
            lines.append("")

            table_lines = create_status_table_rows(obsolete_for_release)
            lines.extend(table_lines)

    return format_lines(lines)


def format_lines(lines: List[str]) -> str:
    return "\n".join(lines)


def escape_table_characters(text: str) -> str:
    """Escape characters that have special meaning in Markdown tables."""
    return text.replace("*", "\\*").replace("|", "\\|")


def run():
    if not SPECIFICATIONS_FOLDER.exists():
        print(f"Folder {SPECIFICATIONS_FOLDER} does not exist.")
        sys.exit(1)

    for json_file in SPECIFICATIONS_FOLDER.glob("**/*.json"):
        with open(json_file, "r", encoding="utf-8") as f:
            data = json.load(f)

        spec = IniFileSpec.from_dict(data)
        markdown = json_to_markdown(spec)
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
