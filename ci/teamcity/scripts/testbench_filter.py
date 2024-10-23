import csv
import argparse


def csv_to_dict(csv_table_path):
    with open(csv_table_path, mode="r", newline="") as file:
        reader = csv.reader(file)
        headers = next(reader)  # Get the first line as headers
        data_dict = {header: [] for header in headers}  # Initialize dictionary with headers as keys

        for row in reader:
            for header, value in zip(headers, row):
                data_dict[header].append(value)  # Append values to the corresponding key

        return data_dict


# Function to filter values based on the 'this' list
def filter_config(csv_table_path: str, csv_data, branch_name: str):
    # Check if the branch name exists in the data
    if branch_name not in csv_data:
        raise ValueError(f"Branch name '{branch_name}' does not exist in file {csv_table_path}.")

    config_names = csv_data["#name"]
    config_values = csv_data["#config"]
    this_values = csv_data[branch_name]

    # Filtered list based on 'this' values being "TRUE"

    filtered_values = [f"{config}" for name, config, this in zip(config_names, config_values, this_values) if this == "TRUE"]

    return filtered_values


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Filter csv config values using arguments")

    # Add arguments
    parser.add_argument("-n", type=str, dest="component", required=True, help="Components that were hit by the change")
    parser.add_argument("-f", type=str, dest="csv_file_path", required=True, help="Path to the config value file.")
    parser.add_argument("-v", type=str, dest="value_filter", default="", help="Filter the configurations by wether it contains the filter.")

    args = parser.parse_args()
    branch_name = args.component
    csv_table_path = args.csv_file_path
    value_filter = args.value_filter

    if branch_name == "main":
        branch_name = "all"

    with open(csv_table_path, mode="r", newline="") as csv_data:
        branch_config_dict = csv_to_dict(csv_table_path)

    filtered_configs = filter_config(csv_table_path, branch_config_dict, branch_name)
    matrix_list = ",".join([config for config in filtered_configs if value_filter in config])
    print(f"##teamcity[setParameter name='matrix_list_{value_filter}' value='{matrix_list}']")
