import argparse
import csv
from typing import Dict, List


def csv_to_dict(csv_table_path: str) -> Dict[str, List[str]]:
    """Open csv file and make a dict from the results."""
    with open(csv_table_path, mode="r", newline="") as file:
        reader = csv.reader(file)
        headers = next(reader)
        data_dict: Dict[str, List[str]] = {header: [] for header in headers}

        for row in reader:
            for header, value in zip(headers, row, strict=False):
                data_dict[header].append(value)

        return data_dict


# Function to filter values based on the 'this' list
def filter_config(csv_table_path: str, csv_data: Dict[str, List], product: str) -> List[str]:
    """Filter the dict with the branch name and return a list of matches."""
    if product not in csv_data:
        raise ValueError(f"Branch name '{product}' does not exist in file {csv_table_path}.")

    config_names = csv_data["#name"]
    config_values = csv_data["#config"]
    this_values = csv_data[product]

    # Filtered list based on 'this' values being "TRUE"
    filtered_values = [
        f"{config}"
        for name, config, this in zip(config_names, config_values, this_values, strict=False)
        if this == "TRUE"
    ]
    # Have to research how to pass {name}=>{config} through TeamCity REST API

    return filtered_values


if __name__ == "__main__":
    """Select which testbench configs to run based on the 'component' argument."""
    parser = argparse.ArgumentParser(description="Filter csv config values using arguments")

    # Add arguments
    parser.add_argument("-n", type=str, dest="component", required=True, help="Components that were hit by the change")
    parser.add_argument("-f", type=str, dest="csv_file_path", required=True, help="Path to the config value file.")
    parser.add_argument(
        "-v",
        type=str,
        dest="value_filter",
        default="",
        help="Filter the configurations by wether it contains the filter.",
    )

    args = parser.parse_args()
    product = args.component
    csv_table_path = args.csv_file_path
    value_filter = args.value_filter

    branch_config_dict = csv_to_dict(csv_table_path)

    filtered_configs = filter_config(csv_table_path, branch_config_dict, product)
    matrix_list = ",".join([config for config in filtered_configs if value_filter in config])
    print(f"##teamcity[setParameter name='matrix_list_{value_filter}' value='{matrix_list}']")
