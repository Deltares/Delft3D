import json
import os
import subprocess
import sys
from concurrent.futures import ThreadPoolExecutor

signtool = "signtool.exe"
file_structure_json = "ci/DIMRset_delivery/src/DIMRset-binaries.json"


def is_signtool_available(developer_promt: str) -> bool:
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


def verify_signing_authority(filepath: str, developer_promt: str) -> tuple:
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
            return "Verified", f"{issuer}"
        else:
            return "Not Verified", ""
    except Exception as e:
        return f"Error: {e}"


def get_actual_files(directory: str) -> list:
    actual_files = []
    for root, dirs, files in os.walk(directory):
        for file in files:
            if file.lower().endswith((".dll", ".exe")):
                filepath = os.path.join(root, file)
                relative_filepath = os.path.relpath(filepath, directory)
                actual_files.append(relative_filepath)
    return actual_files


def validate_signing_status(
    file: str,
    directory: str,
    files_that_should_be_signed_with_issued_to: str,
    files_that_should_not_be_signed: str,
    developer_promt: str,
) -> tuple:
    filepath = os.path.join(directory, file)
    status, issued_to = verify_signing_authority(filepath, developer_promt)
    if file in [item["file"] for item in files_that_should_be_signed_with_issued_to]:
        if status == "Verified":
            for item in files_that_should_be_signed_with_issued_to:
                if item["file"] == file:
                    break
            expected_issued_to = item["Issued to"]
            if expected_issued_to == issued_to:
                return f"File is correctly signed: {file} by {issued_to}", True
            else:
                return (
                    f"File is not correctly signed: {file} by {expected_issued_to} but by {issued_to}",
                    False,
                )
        else:
            return f"File should be signed but is not: {file}", False
    elif file in files_that_should_not_be_signed:
        if status == "Not Verified":
            return f"File is correctly not signed: {file}", True
        else:
            return (
                f"File should not be signed but is: {file} by {issued_to}",
                False,
            )
    return "", True


def is_signing_correct(
    actual_files: list,
    files_that_should_be_signed_with_issued_to: list,
    files_that_should_not_be_signed: list,
    developer_promt: str,
) -> bool:
    files_signed_correctly = True

    with ThreadPoolExecutor() as executor:
        signing_statuses = [
            executor.submit(
                validate_signing_status,
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


def validate_directory_contents(actual_files: list, expected_files: list) -> bool:
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


def print_example_json_file_structure() -> None:
    print("Example JSON file structure:{")
    print('    "Signed": [')
    print("        {")
    print('            "file": "file_1.exe",')
    print('            "Issued to": "party A"')
    print("        },")
    print("        {")
    print('            "file": "file_2.dll",')
    print('            "Issued to": "Party B"')
    print("        }")
    print("    ],")
    print('    "Not signed": [')
    print('        "file_3.dll",')
    print('        "lib\\file_4.dll"')
    print("    ]")
    print("}")


if __name__ == "__main__":
    if len(sys.argv) != 3:
        print("Usage: python script.py <developer_promt> <directory>")
        sys.exit(1)
    else:
        developer_promt = sys.argv[1]
        directory = sys.argv[2]
        try:
            with open(file_structure_json, "r") as f:
                files_to_check = json.load(f)
        except Exception as e:
            print(f"Error loading JSON file: {e}")
            sys.exit(1)
        try:
            files_that_should_be_signed = [
                item["file"] for item in files_to_check["Signed"]
            ]
            files_that_should_not_be_signed = files_to_check["Not signed"]
        except Exception as e:
            print(f"Error parsing JSON file: {file_structure_json}")
            print(f"Error: {e}")
            print_example_json_file_structure()
            sys.exit(1)
        expected_files = files_that_should_be_signed + files_that_should_not_be_signed
        actual_files = get_actual_files(directory)

        if not validate_directory_contents(actual_files, expected_files):
            print("Directory check failed: Missing or extra files detected.")
            sys.exit(1)

        if is_signtool_available(developer_promt):
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
