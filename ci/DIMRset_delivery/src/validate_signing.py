import argparse
import json
import os
import subprocess
import sys
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path

SIGNTOOL = "signtool.exe"


def is_signtool_available(developer_prompt: str) -> bool:
    """
    Checks if the 'signtool' is available in the given developer prompt.

    Args:
        developer_prompt (str): The command to open the developer prompt.

    Returns:
        bool: True if 'signtool' is available, False otherwise.
    """
    try:
        result = subprocess.run(
            [
                developer_prompt,
                "&&",
                SIGNTOOL,
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


def verify_signing_authority(filepath: str, developer_prompt: str) -> tuple:
    """
    Verifies the signing authority of a given file using the specified developer prompt.

    Args:
        filepath (str): The path to the file to be verified.
        developer_prompt (str): The developer prompt command to be used for verification.

    Returns:
        tuple: A tuple containing the verification status ("Verified" or "Not Verified")
               and the issuer name if verified, or an error message if an exception occurs.
    """
    try:
        result = subprocess.run(
            [
                developer_prompt,
                "&&",
                SIGNTOOL,
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


def get_actual_files(directory: str) -> list[Path]:
    """
    Recursively retrieves a list of relative file paths for all .dll and .exe files in the given directory.

    Args:
        directory (str): The root directory to search for files.

    Returns:
        list: A list of relative file paths for .dll and .exe files found in the directory.
    """
    directory = Path(directory)
    return [
        path.relative_to(directory)
        for path in directory.glob("**/*")
        if path.suffix.lower() in (".dll", ".exe")
    ]


def validate_signing_status(
    file: str,
    directory: str,
    files_that_should_be_signed_with_issued_to: list,
    files_that_should_not_be_signed: list[Path],
    developer_prompt: str,
) -> tuple:
    """
    Validate the signing status of a file.

    Args:
        file (str): The name of the file to validate.
        directory (str): The directory where the file is located.
        files_that_should_be_signed_with_issued_to (str): List of files that should be signed with a specific issuer.
        files_that_should_not_be_signed (str): List of files that should not be signed.
        developer_prompt (str): Prompt for the developer.

    Returns:
        tuple: A message indicating the validation result and a boolean indicating if the validation was successful.
    """
    filepath = os.path.join(directory, file)
    status, issued_to = verify_signing_authority(filepath, developer_prompt)
    if file in [item["file"] for item in files_that_should_be_signed_with_issued_to]:
        if status == "Verified":
            for item in files_that_should_be_signed_with_issued_to:
                if item["file"] == file:
                    break
            expected_issued_to = item["issuedTo"]
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


def signing_is_valid(
    filepath: str, developer_prompt: str, expected_issued_to: str = ""
) -> bool:
    status, issued_to = verify_signing_authority(filepath, developer_prompt)
    if expected_issued_to and issued_to != expected_issued_to:
        print(f"file not correctly signed: {filepath}, signed to '{issued_to}' but expexted '{expected_issued_to}'")
    elif not expected_issued_to and status == "Verified":
        print(f"file should not be signed: {filepath}")
    else:
        print(f"file is correctly (un)signed: {filepath}")

    return status == "Verified" and expected_issued_to == issued_to


def is_signing_correct(
    directory: str,
    files_that_should_be_signed_with_issued_to: list,
    files_that_should_not_be_signed: list[Path],
    developer_prompt: str,
) -> bool:
    """
    Checks if the signing status of files is correct.
    Args:
        files_that_should_be_signed_with_issued_to (list): List of files that should be signed with "issuedTo".
        files_that_should_not_be_signed (list): List of files that should not be signed.
        developer_prompt (str): Developer prompt for signing validation.
    Returns:
        bool: True if all files are signed correctly, False otherwise.
    """
    files_signed_correctly = True
    files_unsigned_correctly = True
    with ThreadPoolExecutor() as executor:
        signed_results = executor.map(
            lambda item: signing_is_valid(
                os.path.join(directory, item["file"]),
                developer_prompt,
                item["issuedTo"],
            ),
            files_that_should_be_signed_with_issued_to,
        )
        files_signed_correctly = all(signed_results)

        unsigned_results = executor.map(
            lambda item: signing_is_valid(
                os.path.join(directory, item), developer_prompt
            ),
            files_that_should_not_be_signed,
        )
        files_unsigned_correctly = not any(unsigned_results)

    return files_signed_correctly and files_unsigned_correctly


def validate_directory_contents(
    actual_files: list[Path], expected_files: list[Path]
) -> bool:
    """
    Validates the contents of a directory by comparing the actual files against the expected files.
    Args:
        actual_files (list): A list of filenames that are actually present in the directory.
        expected_files (list): A list of filenames that are expected to be present in the directory.
    Returns:
        bool: True if all expected files are present and there are no extra files, False otherwise.
    """
    missing_files = set(expected_files) - set(actual_files)
    extra_files = set(actual_files) - set(expected_files)

    if missing_files:
        print("\nMissing files:")
        for file in missing_files:
            print(file)

    if extra_files:
        print("\nExtra files:")
        for file in extra_files:
            print(file)

    return not missing_files and not extra_files


def print_example_json_file_structure() -> None:
    print("Example JSON file structure:{")
    print('    "signed": [')
    print("        {")
    print('            "file": "file_1.exe",')
    print('            "issuedTo": "party A"')
    print("        },")
    print("        {")
    print('            "file": "file_2.dll",')
    print('            "issuedTo": "Party B"')
    print("        }")
    print("    ],")
    print('    "notSigned": [')
    print('        "file_3.dll",')
    print('        "lib\\file_4.dll"')
    print("    ]")
    print("}")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Validate file structure and signing status of files in a directory."
    )
    parser.add_argument(
        "expected_structure_json",
        help="Json file with expected file structure.",
        type=str,
    )
    parser.add_argument(
        "developer_prompt", help="Path to the vs studio developer promt", type=str
    )
    parser.add_argument("directory", help="Directiry to validate.", type=str)
    if len(sys.argv) != 4:
        print(
            "Usage: python script.py <expected_structure_json> <developer_prompt> <directory>"
        )
        sys.exit(1)

    args = parser.parse_args()
    file_structure_json = args.expected_structure_json
    developer_prompt = args.developer_prompt
    directory = args.directory

    try:
        with open(file_structure_json, "r") as f:
            files_to_check = json.load(f)
    except Exception as e:
        print(f"Error loading JSON file: {e}")
        sys.exit(1)

    try:
        files_that_should_be_signed = [
            Path(item["file"]) for item in files_to_check["signed"]
        ]
        files_that_should_not_be_signed = [
            Path(file) for file in files_to_check["notSigned"]
        ]
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

    print(
        "Directory check passed: All expected files are present and in the right structure."
    )

    if not is_signtool_available(developer_prompt):
        print(
            "Signtool is required to run this script. Please ensure it is installed and available in the PATH."
        )
        sys.exit(1)

    files_that_should_be_signed_with_issued_to = files_to_check["signed"]
    if not is_signing_correct(
        directory,
        files_that_should_be_signed_with_issued_to,
        files_that_should_not_be_signed,
        developer_prompt,
    ):
        print("Some files are not correctly signed")
        sys.exit(1)

    print(
        "All files are correctly signed and the directory contains all expected files."
    )
    sys.exit(0)
