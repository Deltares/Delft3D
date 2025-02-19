import json
import os
import subprocess
import sys
from concurrent.futures import ThreadPoolExecutor

signtool = "signtool.exe"


def has_signtool(developer_promt) -> bool:
    try:
        result = subprocess.run(
            [
                developer_promt,
                "&&",
                signtool,
                "verify",
                "/?",
            ],
            capture_output=True,
            text=True,
            shell=True,
        )
        if result.returncode == 0:
            return True
        else:
            print("signtool is not available or not found in the PATH.")
            return False
    except Exception as e:
        print(f"Error checking signtool: {e}")
        return False


def get_signing_authority(filepath, developer_promt) -> str:
    try:
        result = subprocess.run(
            [
                developer_promt,
                "&&",
                signtool,
                "verify",
                "/pa",
                "/v",
                filepath,
            ],
            capture_output=True,
            text=True,
            shell=True,
        )
        if "Successfully verified" in result.stdout:
            issuer = ""
            cut_off = result.stdout.split("The signature is timestamped")[0]
            for line in cut_off.splitlines():
                if "Issued to:" in line:
                    issuer = line.split("Issued to:")[1].strip()
            return f"Verified (Issued to: {issuer})"
        else:
            return "Not Verified"
    except Exception as e:
        return f"Error: {e}"


def get_actual_files(directory) -> list:
    actual_files = []
    for root, dirs, files in os.walk(directory):
        for file in files:
            if file.lower().endswith((".dll", ".exe")):
                filepath = os.path.join(root, file)
                relative_filepath = os.path.relpath(filepath, directory)
                actual_files.append(relative_filepath)
    return actual_files


def check_signing_status(
    file,
    directory,
    files_that_should_be_signed_with_issued_to,
    files_that_should_not_be_signed,
    developer_promt,
) -> tuple:
    filepath = os.path.join(directory, file)
    signing_status = get_signing_authority(filepath, developer_promt)
    if file in [item["file"] for item in files_that_should_be_signed_with_issued_to]:
        if "Verified" in signing_status:
            return f"File is correctly signed: {file} by {signing_status}", True
        else:
            return f"File should be signed but is not: {file}", False
    elif file in files_that_should_not_be_signed:
        if "Not Verified" in signing_status:
            return f"File is correctly not signed: {file}", True
        else:
            return (
                f"File should not be signed but is: {file} by {signing_status}",
                False,
            )
    return "", True


def is_signing_correct(
    actual_files,
    files_that_should_be_signed_with_issued_to,
    files_that_should_not_be_signed,
    developer_promt,
) -> bool:
    files_signed_correctly = True

    with ThreadPoolExecutor() as executor:
        signing_statuses = [
            executor.submit(
                check_signing_status,
                file,
                directory,
                files_that_should_be_signed_with_issued_to,
                files_that_should_not_be_signed,
                developer_promt,
            )
            for file in actual_files
        ]
        for signing_status in signing_statuses:
            message, status = signing_status.result()
            if message:
                print(message)
            if not status:
                files_signed_correctly = False

    return files_signed_correctly


def is_directory_correct(actual_files, expected_files) -> bool:
    files_complete_and_valid = True
    missing_files = []
    extra_files = []

    for expected_file in expected_files:
        found = False
        for actual_file in actual_files:
            if expected_file in actual_file:
                found = True
                break
        if not found:
            missing_files.append(expected_file)
            files_complete_and_valid = False

    for actual_file in actual_files:
        found = False
        for expected_file in expected_files:
            if expected_file in actual_file:
                found = True
                break
        if not found:
            extra_files.append(actual_file)
            files_complete_and_valid = False

    if missing_files:
        print("Missing files:")
        for file in missing_files:
            print(file)

    if extra_files:
        print("Extra files:")
        for file in extra_files:
            print(file)

    return files_complete_and_valid


if __name__ == "__main__":
    if len(sys.argv) != 3:
        print("Usage: python script.py <developer_promt> <directory>")
        sys.exit(1)
    else:
        developer_promt = sys.argv[1]
        directory = sys.argv[2]
        try:
            with open("ci/DIMRset_delivery/src/DIMRset-binaries.json", "r") as f:
                files_to_check = json.load(f)
        except Exception as e:
            print(f"Error loading JSON file: {e}")
            sys.exit(1)

        files_that_should_be_signed = [
            item["file"] for item in files_to_check["Signed"]
        ]
        files_that_should_not_be_signed = files_to_check["Not signed"]
        expected_files = files_that_should_be_signed + files_that_should_not_be_signed
        actual_files = get_actual_files(directory)

        if not is_directory_correct(actual_files, expected_files):
            print("Directory check failed: Missing or extra files detected.")
            sys.exit(1)

        if has_signtool(developer_promt):
            files_that_should_be_signed_with_issued_to = files_to_check["Signed"]
            if not is_signing_correct(
                actual_files,
                files_that_should_be_signed_with_issued_to,
                files_that_should_not_be_signed,
                developer_promt,
            ):
                print("Some files are not correctly signed")
                sys.exit(1)
        else:
            print(
                "signtool is required to run this script. Please ensure it is installed and available in the PATH."
            )
            sys.exit(1)

        print(
            "All files are correctly signed and the directory contains all expected files."
        )
        sys.exit(0)
