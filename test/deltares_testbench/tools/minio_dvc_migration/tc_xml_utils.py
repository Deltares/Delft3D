"""Utility functions for the XML analysis tool."""

from pathlib import Path
from typing import List


def load_teamcity_xml_files(csv_path: str) -> List[Path]:
    """Load the list of XML files from the teamcity CSV and return as a set of normalized paths."""
    xml_files_in_csv: List[Path] = []

    with open(csv_path, "r", encoding="utf-8") as file:
        lines = file.readlines()

    if not lines:
        raise ValueError(f"File is empty: {csv_path}")

    # Parse the header to find the column index
    header = lines[0].strip().split(",")
    config_column_index = None
    for i, column_name in enumerate(header):
        if column_name.strip() == "#config":
            config_column_index = i
            break

    if config_column_index is None:
        raise ValueError(f"Missing '#config' column in CSV: {csv_path}")

    # Read all the data rows
    for line in lines[1:]:
        row = line.strip().split(",")
        if len(row) > config_column_index:
            config_path = row[config_column_index].strip()
            if config_path:
                config_path = Path("configs") / Path(config_path)
                xml_files_in_csv.append(config_path)

    return xml_files_in_csv
