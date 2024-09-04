import sys
import csv

def csv_to_dict(file_path):
    with open(file_path, mode='r', newline='') as csvfile:
        reader = csv.reader(csvfile)
        headers = next(reader)  # Get the first line as headers
        data_dict = {header: [] for header in headers}  # Initialize dictionary with headers as keys

        for row in reader:
            for header, value in zip(headers, row):
                data_dict[header].append(value)  # Append values to the corresponding key

    return data_dict

# Function to filter values based on the 'this' list
def filter_config(data, branch_name):
    config_values = data["config"]
    this_values = data[branch_name]
    
    # Filtered list based on 'this' values being "TRUE"
    filtered_values = [config for config, this in zip(config_values, this_values) if this == "TRUE"]
    
    return filtered_values

branch_name = sys.argv[1]
csv_table_path = sys.argv[2]

if __name__ == "__main__":
    # Check if the container_id argument is provided
    if len(sys.argv) != 3:
        print("Usage: python script.py <branch_name> <csv_table_file_path>")
        sys.exit(1)
    
    branch_config_dict = csv_to_dict(csv_table_path)
    done = False
    for key, value in branch_config_dict:
        if key == branch_name:
            matrix_list = ",".join(filter_config(branch_config_dict, branch_name))
            print(f"##teamcity[setParameter name='matrix_list' value='{matrix_list}']")
            done = True

    if not done:
        matrix_list = ",".join(branch_config_dict["config"])
        print(f"##teamcity[setParameter name='matrix_list' value='{matrix_list}']")
