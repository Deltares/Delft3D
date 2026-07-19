import argparse
import re
from pathlib import Path
import json
import sys

DEFAULT_TESTBENCH_TABLE_CSV: Path = Path("ci/teamcity/Delft3D/vars/dimr_testbench_table.csv")

def validate_branch_name(value: str) -> str:
    branch_name = value.strip()
    mo = re.match(r"^(?P<engine>[a-z]+)/[a-z]+/[A-Z]+-[0-9]+[-\w]*$", branch_name)
    if mo is None:
        raise ValueError("Not a valid branch name")
    return branch_name

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--branch-name", required=True, type=validate_branch_name)
    parser.add_argument("--platform", required=True, choices=["win64", "lnx64"])
    parser.add_argument("--configs-dir", type=Path, default=Path("configs"))
    parser.add_argument("--testbench-table-csv", type=Path, default=DEFAULT_TESTBENCH_TABLE_CSV)
    args = parser.parse_args()

    branch_name: str = args.branch_name
    platform: str = args.platform
    configs_dir: Path = args.configs_dir
    testbench_table_csv: Path = args.testbench_table_csv

    build_config = branch_name.split("/", maxsplit=1)[0] + "-testbench"
    with testbench_table_csv.open("r") as lines:
        first_line = next(lines)
        column_index = next((i for i, col in enumerate(first_line.split(",")) if col.strip() == build_config), None)
        if column_index is None:
            raise ValueError(f"Build config '{build_config}' matches no testbench configs.")
        if column_index < 2:
            raise ValueError("The first two columns are reserved for config name and config path.")

        splits = (line.split(",", maxsplit=column_index+1) for line in lines if line.strip())
        name_to_config: dict[str, Path] = {
            split[0].strip(): configs_dir / split[1].strip() for split in splits 
            if platform in split[1] and split[column_index].strip().lower() == "true"
        }

    json.dump(name_to_config, fp=sys.stdout, default=str, indent=4)
    print()
