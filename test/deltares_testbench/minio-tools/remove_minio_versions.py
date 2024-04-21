import json
import subprocess
from concurrent.futures import ThreadPoolExecutor
from datetime import datetime

import pytz

# S3 Object Version Management Script
# This Python script helps manage object versions in an Amazon S3 bucket. It
# uses the AWS CLI to list object versions, convert the output to JSON, and
# selectively delete versions based on their last modified timestamps.

# Prerequisites
# AWS CLI: Ensure that you have the AWS Command Line Interface (CLI) installed
# and configured with appropriate credentials to access your S3 bucket.

# Improvements
# This script uses the AWS CLI as prerequisite. It was done this way because
# this was a one of run. It would be better if it utelizes the minio python
# library

# Configuration:
# Set the following parameters at the beginning of the script:
bucket_name = 'dsc-testbench'               # bucket_name: The name of your S3 bucket.
endpoint_url = 'https://s3.deltares.nl/'    # endpoint_url: The endpoint URL for your S3 bucket (e.g., https://s3.deltares.nl/).
threshold_date_str = '2024-02-15'           # threshold_date_str: The threshold date (in the format ‘YYYY-MM-DD’) for version deletion.
parallel_processes = 5                      # parallel_processes: Number of parallel processes for deletion (adjust as needed).


threshold_date_obj = datetime.strptime(threshold_date_str, '%Y-%m-%d').replace(tzinfo=pytz.UTC)

print("List all objects in the bucket")
command = f"aws s3api list-object-versions --bucket {bucket_name} --endpoint-url {endpoint_url}"
output = subprocess.check_output(command, shell=True).decode('utf-8', errors='ignore')

print("Convert to json")
output_json = json.loads(output)


def delete_object(version):
    last_modified_str = version['LastModified']
    last_modified_obj = datetime.fromisoformat(last_modified_str)

    if last_modified_obj > threshold_date_obj:
        delete_command = f'aws s3api delete-object --bucket {bucket_name} --key "{version['Key']}" --version-id {version['VersionId']} --endpoint-url {endpoint_url}'
        result = subprocess.run(delete_command, shell=True, capture_output=True)
        if result.returncode == 0:
            print(f"Deleted file: {version['Key']} with timestamp: {last_modified_obj}")
        else:
            print(f"Failed to delete file: {version['Key']} with timestamp: {last_modified_obj}. Error: {result.stderr.decode()}")


with ThreadPoolExecutor(max_workers=parallel_processes) as executor:
    if output_json['Versions']:
        futures = [executor.submit(delete_object, version) for version in output_json['Versions']]
    else:
        print("No versions found in the output.")
