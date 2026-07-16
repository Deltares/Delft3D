#!/usr/bin/env python
import os
import re
import shutil
import sys
from typing import TextIO

from src.utils.comparers.end_result import EndResult


def get_failed_tests(filename: str) -> list:
    """Get a list of failed test cases from the given log file.

    Parameters
    ----------
    filename : str
        The path to the log file.

    Returns
    -------
    list
        A list of failed test case names.

    """
    pattern = rf"\|(.*?)\|.*?\|({EndResult.ERROR.value}|{EndResult.NOK.value})"
    failed_tests = []

    with open(filename, "r") as file:
        for line in file:
            match = re.search(pattern, line)
            if match:
                failed_tests.append(match.group(1).strip())

    return failed_tests


def prepare_failed_tests_directory() -> None:
    """Remove any existing 'failed' directory and create a new one.

    Prepares the directory for failed tests by removing any existing 'failed' directory
    and creating a new one.
    """
    if os.path.exists("failed"):
        shutil.rmtree("failed", ignore_errors=True)
    os.makedirs("failed")


def copy_directory(src: str, dst: str, copy_log: TextIO) -> None:
    """Copy a directory from the source to the destination.

    Parameters
    ----------
    src, dst : str
        The source and destination directory path.
    copy_log : TextIO
        The log file object to write the copy status.

    """
    copy_log.write(f"{src} => {dst}")
    try:
        shutil.copytree(src, dst)
        copy_log.write("  COPY OK\n")
    except:
        copy_log.write("  COPY FAILED\n")


def copy_failed_tests(failed_tests, platform: str) -> None:
    """Copy the directories of failed tests to a 'failed' directory.

    Parameters
    ----------
    failed_tests : list
        A list of failed test case names.
    platform : str
        The platform name used to locate the reference directories.

    """
    prepare_failed_tests_directory()

    with open(os.path.join("failed", "list.txt"), "w") as copy_log:
        for test in failed_tests:
            # copy case directory
            src_case = os.path.join("data", "cases", test)
            dst_case = os.path.join("failed", test)
            copy_directory(src_case, dst_case, copy_log)

            # copy reference directory
            src_ref = src_case.replace("cases", os.path.join("references", platform))
            dst_ref = os.path.join("failed", src_ref)
            copy_directory(src_ref, dst_ref, copy_log)

            # copy reference resluts directory
            src_ref_result = src_ref.replace("references", "references_results")
            dst_ref_result = dst_ref.replace("references", "references_results")
            copy_directory(src_ref_result, dst_ref_result, copy_log)


def main() -> None:
    """Execute the script.

    Main function to execute the script. It reads the platform argument from the command line,
    retrieves the list of failed tests from the log file, and copies the failed test directories.
    """
    platform = sys.argv[1]
    failed_tests = get_failed_tests("logs/testbench.log")
    copy_failed_tests(failed_tests, platform)


if __name__ == "__main__":
    main()
