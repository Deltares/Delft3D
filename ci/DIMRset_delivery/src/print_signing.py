import json
import os
import subprocess

signtool = (
    "C:\\Program Files (x86)\\Windows Kits\\10\\bin\\10.0.22621.0\\x64\\signtool.exe"
)


def has_signtool():
    try:
        result = subprocess.run(
            [signtool, "verify", "/?"], capture_output=True, text=True, shell=True
        )
        if result.returncode == 0:
            return True
        else:
            print("signtool is not available or not found in the PATH.")
            return False
    except Exception as e:
        print(f"Error checking signtool: {e}")
        return False


def get_signing_authority(filepath):
    try:
        result = subprocess.run(
            [signtool, "verify", "/pa", filepath],
            capture_output=True,
            text=True,
            shell=True,
        )
        if "Successfully verified" in result.stdout:
            return "Verified"
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


def is_signing_correct(actual_files, files_that_should_be_signed, files_that_should_not_be_signed):
    files_signed_correctly = True

    for file in actual_files:
        filepath = os.path.join(directory, file)
        signing_status = get_signing_authority(filepath)

        if file in files_that_should_be_signed:
            if signing_status != "Verified":
                print(f"File should be signed but is not: {file}")
                files_signed_correctly = False
        elif file in files_that_should_not_be_signed:
            if signing_status == "Verified":
                print(f"File should not be signed but is: {file}")
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
    import sys

    if len(sys.argv) != 2:
        print("Usage: python script.py <directory>")
    else:
        directory = sys.argv[1]

        # Load the JSON file
        with open("ci/DIMRset_delivery/src/DIMRset-binaries.json", "r") as f:
            files_to_check = json.load(f)

        files_that_should_be_signed = files_to_check["Signed"]
        files_that_should_not_be_signed = files_to_check["Not signed"]
        expected_files = files_that_should_be_signed + files_that_should_not_be_signed
        actual_files = get_actual_files(directory)
        if not is_directory_correct(actual_files, expected_files):
            print(
                "All expected files are present in the directory and the directory does not contain any extra files."
            )
        if has_signtool():
            if not is_signing_correct(
                actual_files,
                files_that_should_be_signed,
                files_that_should_not_be_signed,
            ):
                print("Some files are not correctly signed")
        else:
            print(
                "signtool is required to run this script. Please ensure it is installed and available in the PATH."
            )
        print(
            "All files are correctly signed and the directory contains all expected files."
        )
