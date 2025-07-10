"""
JSON validation script

This script checks whether all JSON files within the "schemas" directory
comply with their corresponding JSON Schema file.

Each JSON file is expected to contain a "file_format_type" property, which determines the schema it
should be validated against. Schema files must have the ".json.schema" extension and their
filenames should start with the corresponding "file_format_type" string.

Usage:
    Run this script directly to validate all JSON files under the "schemas" directory.
"""

import json
from pathlib import Path
import sys
from jsonschema import Draft7Validator

SCHEMA_PATH = Path(__file__).parent.parent / "schemas"


def load_json(file: Path) -> dict:
    with open(file, "r", encoding="utf-8") as f:
        return json.load(f)


def find_json_schema(json_data) -> Path:

    file_format = json_data.get("file_format_type")

    for json_schema_file in SCHEMA_PATH.glob("**/*.json.schema"):

        if json_schema_file.stem.startswith(file_format):
            return json_schema_file

    raise FileNotFoundError(f"No JSON schema file found for file format: {file_format}")


def is_json_valid(data, schema):
    validator = Draft7Validator(schema)
    errors = sorted(validator.iter_errors(data), key=lambda e: e.path)

    if errors:
        print(f"❌ Validation failed with {len(errors)} error(s):")
        for error in errors:
            print(f"- {list(error.path)}: {error.message}")

        return False

    return True


def run():
    validation_failed = False

    for json_file in SCHEMA_PATH.glob("**/*.json"):
        data = load_json(json_file)

        json_schema_file = find_json_schema(data)

        schema = load_json(json_schema_file)

        print(f"Validating {json_file.name} against schema {json_schema_file.name}")

        if not is_json_valid(data, schema):
            validation_failed = True

    if validation_failed:
        sys.exit(1)


if __name__ == "__main__":

    try:
        run()
    except Exception as e:
        print(e)
        sys.exit(1)
