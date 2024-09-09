import sys
import csv

def csv_to_dict(csv_table_path):
    reader = csv.reader(csv_table_path)
    headers = next(reader)  # Get the first line as headers
    data_dict = {header: [] for header in headers}  # Initialize dictionary with headers as keys

    for row in reader:
        for header, value in zip(headers, row):
            data_dict[header].append(value)  # Append values to the corresponding key

    return data_dict

# Function to filter values based on the 'this' list
def filter_config(csv_table_path: str, csv_data, branch_name: str):
    if branch_name == "main":
        branch_name = "all"
    # Check if the branch name exists in the data
    if branch_name not in csv_data:
        raise ValueError(f"Branch name '{branch_name}' does not exist in file {csv_table_path}.")
    
    config_names  = csv_data["#name"]
    config_values = csv_data["#config"]
    this_values   = csv_data[branch_name]
    
    # Filtered list based on 'this' values being "TRUE"
    filtered_values = [f"{name}=>{config}" for name, config, this in zip(config_names, config_values, this_values) if this == "TRUE"]
    
    return filtered_values



if __name__ == "__main__":
    # Check if the container_id argument is provided
    if len(sys.argv) != 4:
        print("Usage: python script.py <branch_name> <csv_table_file_path> <parameter_name>")
        sys.exit(1)
        
    branch_name = sys.argv[1]
    csv_table_path = sys.argv[2]
    parameter_name = sys.argv[3]

    with open(csv_table_path, mode='r', newline='') as csv_data:
        branch_config_dict = csv_to_dict(csv_table_path)

    for key, value in branch_config_dict.items():
        if key == branch_name:
            matrix_list = ",".join(filter_config(csv_table_path, branch_config_dict, branch_name))
            print(f"##teamcity[setParameter name='matrix_list' value='{matrix_list}']")
