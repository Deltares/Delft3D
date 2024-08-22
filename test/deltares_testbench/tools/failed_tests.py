#!/usr/bin/env python
import os
import re
import shutil
import sys


def get_failed_tests(filename):
    # Regular expression pattern to match the test case lines
    pattern = r"\|(.*?)\|.*?\|(ERROR|NOK)"
    failed_tests = []

    with open(filename, "r") as file:
        for line in file:
            match = re.search(pattern, line)
            if match:
                failed_tests.append(match.group(1).strip())

    return failed_tests


def prepare_failed_tests_directory() -> None:
    if os.path.exists("failed"):
        shutil.rmtree("failed", ignore_errors=True)
    os.makedirs("failed")


def copy_directory(src, dst, copy_log) -> None:
    copy_log.write(f"{src} => {dst}")
    try:
        shutil.copytree(src, dst)
        copy_log.write("  COPY OK\n")
    except:
        copy_log.write("  COPY FAILED\n")


def copy_failed_tests(failed_tests, platform) -> None:
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
    platform = sys.argv[1]
    failed_tests = get_failed_tests("logs/testbench.log")
    copy_failed_tests(failed_tests, platform)


if __name__ == "__main__":
    main()
