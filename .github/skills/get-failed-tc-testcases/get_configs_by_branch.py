import argparse
from pathlib import Path
import json
import sys
from typing import TextIO

DEFAULT_TESTBENCH_TABLE_CSV: Path = Path("ci/teamcity/Delft3D/vars/dimr_testbench_table.csv")


def make_sub_build_names_to_xml_paths(lines: TextIO, column_name: str) -> dict[str, Path]:
    first_line = next(lines)
    column_index = next((i for i, col in enumerate(first_line.split(",")) if col.strip() == column_name), None)
    if column_index is None:
        raise ValueError(f"'{column_name}' matches no columns in the TestBench table.")
    if column_index < 2:
        raise ValueError("The first two columns are reserved for config name and config path.")

    splits = (line.split(",", maxsplit=column_index+1) for line in lines if line.strip())
    return {
        f"[{split[0].strip()}]": configs_dir / split[1].strip() for split in splits 
        if platform in split[1] and split[column_index].strip().lower() == "true"
    }


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--platform", required=True, choices=["win64", "lnx64"])
    parser.add_argument("--branch-name", default="main")
    parser.add_argument("--configs-dir", type=Path, default=Path("configs"))
    parser.add_argument("--testbench-table-csv", type=Path, default=DEFAULT_TESTBENCH_TABLE_CSV)
    args = parser.parse_args()

    branch_name: str = args.branch_name
    platform: str = args.platform
    configs_dir: Path = args.configs_dir
    testbench_table_csv: Path = args.testbench_table_csv

    idx = next((i for i, c in enumerate(branch_name) if c == '/'), None)
    column_name = ("all" if idx is None else branch_name[:idx]) + "-testbench"

    with testbench_table_csv.open("r") as lines:
        sub_builds_to_xml_paths = make_sub_build_names_to_xml_paths(lines, column_name)

    json.dump(sub_builds_to_xml_paths, fp=sys.stdout, default=str, indent=4)
    print()
