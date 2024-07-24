import argparse
import json
import subprocess
from concurrent.futures import ThreadPoolExecutor
from datetime import datetime, timezone

from tools.minio.prompt import InteractivePrompt

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

# Usage: from `test/deltares_testbench`:
# $ python tools/minio/remove_versions.py --bucket-name=<bucket_name> --threshold-date=2024-04-01

HELP_BUCKET_NAME = "The name of your S3 bucket."
HELP_THRESHOLD_TIMESTAMP = "The timestamp after which to delete all object versions (format YYYY-MM-DD)."
HELP_ENDPOINT_URL = "The endpoint URL for your S3 bucket."
HELP_PARALLEL = "Number of parallel processes to use."

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--bucket-name", required=True, help=HELP_BUCKET_NAME)
    parser.add_argument("--threshold-date", required=True, help=HELP_THRESHOLD_TIMESTAMP)
    parser.add_argument("--endpoint-url", required=False, help=HELP_ENDPOINT_URL)
    parser.add_argument("-p", "--parallel", type=int, required=False, help=HELP_PARALLEL)
    parser.set_defaults(endpoint_url="https://s3.deltares.nl/", parallel=5)
    args = parser.parse_args()

    bucket_name = args.bucket_name  # bucket_name: The name of your S3 bucket.
    endpoint_url = args.endpoint_url
    threshold_date_obj = datetime.strptime(args.threshold_date, "%Y-%m-%d").replace(tzinfo=timezone.utc)
    parallel_processes = args.parallel

    prompt = InteractivePrompt()
    message = f"Are you sure you want to delete all object versions published after {threshold_date_obj}?"
    if not prompt.yes_no(message, default_yes=False):
        print("Aborted.")
        exit(0)

    print("List all objects in the bucket")
    command = f"aws s3api list-object-versions --bucket {bucket_name} --endpoint-url {endpoint_url}"
    output = subprocess.check_output(command, shell=True).decode("utf-8", errors="ignore")

    print("Convert to json")
    output_json = json.loads(output)

    def delete_object(version) -> None:
        last_modified_str = version["LastModified"]
        last_modified_obj = datetime.fromisoformat(last_modified_str)

        if last_modified_obj > threshold_date_obj:
            delete_command = f'aws s3api delete-object --bucket {bucket_name} --key "{version["Key"]}" --version-id {version["VersionId"]} --endpoint-url {endpoint_url}'
            result = subprocess.run(delete_command, shell=True, capture_output=True)
            if result.returncode == 0:
                print(f"Deleted file: {version['Key']} with timestamp: {last_modified_obj}")
            else:
                print(
                    f"Failed to delete file: {version['Key']} with timestamp: {last_modified_obj}. Error: {result.stderr.decode()}"
                )

    with ThreadPoolExecutor(max_workers=parallel_processes) as executor:
        if output_json["Versions"]:
            futures = [executor.submit(delete_object, version) for version in output_json["Versions"]]
        else:
            print("No versions found in the output.")
