import argparse
import os
import string
import sys
from datetime import datetime, timezone
from typing import Generator, TextIO

"""
This script lists all what strings, starting with @(#)Deltares or containing HeadURL,
in all subdirectories of a given root directory.
The root directory is specified by the argument --srcdir.
"""


def clean(text: str) -> str:
    """
    Remove all non-printable characters from text.

    Parameters
    ----------
    text : str
        The input text to clean.

    Returns
    -------
    str
        The cleaned text with only printable characters.
    """
    # Remove all non-printable characters
    sanitized_text = "".join(
        c
        for c in text
        if c in string.printable
        and c not in "\x0b\x0c\x0e\x0f\x10\x11\x12\x13\x14\x15\x16\x17\x18\x19\x1a\x1b\x1c\x1d\x1e\x1f"
    )
    return sanitized_text


def extract_printable_strings(filename: str, min_length: int = 4) -> Generator[str, None, None]:
    """
    Extract printable strings from a file.

    Parameters
    ----------
    filename : str
        Path to the file to process.
    min_length : int, optional
        Minimum length of strings to extract. Defaults to 4.

    Yields
    ------
    str
        Printable strings found in the file.
    """
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
    except FileNotFoundError:
        print(f"File not found: {filename}")
    except PermissionError:
        print(f"Permission denied: {filename}")
    except IsADirectoryError:
        print(f"Expected a file but got a directory: {filename}")
    except OSError as e:
        print(f"OS error while reading {filename}: {e}")
    except Exception as e:
        print(f"Unexpected error while reading {filename}: {e}")


def list_what_strings(file_path: str, log_file: TextIO) -> None:
    """
    List what strings found in a file.

    Parameters
    ----------
    file_path : str
        Path to the file to process.
    log_file : TextIO
        File object to write results to.
    """
    # Determine the minimum length needed for the search strings
    min_len = 7  # Minimum length of the search strings "@(#)Deltares" and "HeadURL"
    try:
        string_list = list(extract_printable_strings(file_path, min_length=min_len))
    except Exception as e:
        log_file.write(clean(f"\t[Unreadable file] {file_path}: {type(e).__name__} - {e}\n"))
        return

    what_strings = []
    for string_item in string_list:
        if "@(#)Deltares" in string_item:
            what_strings.append(string_item[string_item.find("@(#)Deltares") :])
        if "HeadURL" in string_item:
            what_strings.append(string_item[string_item.find("HeadURL") :])

    if what_strings:
        log_file.write(clean(f"\t{file_path}\n"))
        for what_string in what_strings:
            if what_string.startswith("@(#)"):
                log_file.write(clean(f"\t\t{what_string[4:]}\n"))
            elif what_string.startswith("HeadURL"):
                log_file.write(clean(f"\t\t{what_string[9:]}\n"))


def walk_and_list_what_strings(root_folder: str, log_file: TextIO) -> None:
    """
    Walk through directory tree and list what strings in all files.

    Parameters
    ----------
    root_folder : str
        Root directory to start searching from.
    log_file : TextIO
        File object to write results to.
    """
    for current_dir, _subdirs, files in os.walk(root_folder):
        if current_dir != root_folder:
            print(f"\t{current_dir}")
            for file_name in files:
                file_path = os.path.join(current_dir, file_name)
                list_what_strings(file_path, log_file)


def get_command_line_args() -> argparse.Namespace:
    """
    Parse command line arguments.

    Returns
    -------
    argparse.Namespace
        Parsed command line arguments.
    """
    parser = argparse.ArgumentParser(description="Batch process to list all what-strings")
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
    start_time = datetime.now(tz=timezone.utc)

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
        log_file.write(clean(f"End  : {datetime.now(tz=timezone.utc)}\n"))
        log_file.write(clean("Klaar\n"))

    print("Processing done")
    print(f"\nStart: {start_time}")
    print(f"End  : {datetime.now(tz=timezone.utc)}")
    print("Klaar")
