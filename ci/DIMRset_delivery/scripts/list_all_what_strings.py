import argparse
import os
import string
import sys
from datetime import datetime

"""
This script lists all what strings, starting with @(#)Deltares or containing HeadURL,
in all subdirectories of a given root directory.
The root directory is specified by the argument --srcdir.
"""


def clean(text: str) -> str:
    # Remove all non-printable characters
    sanitized_text = "".join(
        c
        for c in text
        if c in string.printable
        and c
        not in "\x0b\x0c\x0e\x0f\x10\x11\x12\x13\x14\x15\x16\x17\x18\x19\x1a\x1b\x1c\x1d\x1e\x1f"
    )
    return sanitized_text


def extract_printable_strings(filename, min_length=4):
    try:
        with open(filename, "r", errors="ignore") as f:
            printable_set = set(string.printable)
            result = ""
            while True:
                chunk = f.read(4096)
                if not chunk:
                    break
                for c in chunk:
                    if c in printable_set:
                        result += c
                        continue
                    if len(result) >= min_length:
                        yield result
                    result = ""
            if len(result) >= min_length:
                yield result
    except Exception as e:
        # Could not read file, skip it
        yield from ()


def list_what_strings(file_path: str, log_file) -> None:
    # Determine the minimum length needed for the search strings
    min_len = 7  # Minimum length of the search strings "@(#)Deltares" and "HeadURL"
    try:
        string_list = list(extract_printable_strings(file_path, min_length=min_len))
    except Exception as e:
        log_file.write(
            clean(f"\t[Unreadable file] {file_path}: {type(e).__name__} - {e}\n")
        )
        return

    what_strings = []
    for str in string_list:
        if "@(#)Deltares" in str:
            what_strings.append(str[str.find("@(#)Deltares") :])
        if "HeadURL" in str:
            what_strings.append(str[str.find("HeadURL") :])

    if what_strings:
        log_file.write(clean(f"\t{file_path}\n"))
        for what_string in what_strings:
            if what_string.startswith("@(#)"):
                log_file.write(clean(f"\t\t{what_string[4:]}\n"))
            elif what_string.startswith("HeadURL"):
                log_file.write(clean(f"\t\t{what_string[9:]}\n"))


def walk_and_list_what_strings(root_folder, log_file):
    for current_dir, subdirs, files in os.walk(root_folder):
        if current_dir != root_folder:
            print(f"\t{current_dir}")
            for file_name in files:
                file_path = os.path.join(current_dir, file_name)
                list_what_strings(file_path, log_file)


def get_command_line_args():
    parser = argparse.ArgumentParser(
        description="Batch process to list all what-strings"
    )
    parser.add_argument(
        "-s",
        "--srcdir",
        help="Root directory from which the what-strings are listed",
        dest="src_dir",
    )
    parser.add_argument("-o", "--output", help="Output filename.", dest="out_put")
    args = parser.parse_args()
    return args


if __name__ == "__main__":
    start_time = datetime.now()

    args = get_command_line_args()

    src_dir = "."
    out_put = "dimr_version.txt"
    if args.src_dir:
        src_dir = args.src_dir
    start_dir = os.getcwd()
    src_dir = os.path.normpath(os.path.join(start_dir, src_dir))
    if not os.path.exists(src_dir):
        print(f"Given directory does not exist: {src_dir}")
        sys.exit(1)

    if args.out_put:
        out_put = args.out_put
    if os.path.exists(out_put):
        os.remove(out_put)

    print(f"Start: {start_time}\n")
    print(f"{sys.version}\n")
    print(f"Listing is written to: {out_put}")
    print(f"Root Directory: {src_dir}")

    with open(out_put, "a") as log_file:
        log_file.write(clean(f"Start: {start_time}\n"))
        log_file.write(clean(f"Root Directory: {src_dir}\n"))
        walk_and_list_what_strings(src_dir, log_file)
        log_file.write(clean("Processing done\n"))
        log_file.write(clean(f"\nStart: {start_time}\n"))
        log_file.write(clean(f"End  : {datetime.now()}\n"))
        log_file.write(clean("Klaar\n"))

    print("Processing done")
    print(f"\nStart: {start_time}")
    print(f"End  : {datetime.now()}")
    print("Klaar")
