import re
import sys
import argparse
from dataclasses import dataclass
from typing import List, Optional
from pathlib import Path
from html_pre_tag_parser import extract_text_from_last_pre_tag

@dataclass
class WarningMessage:
    file_path: Path
    line_number: int
    message_type: str
    message_number: int
    message_string: str

def parse_log(log_text: str) -> List[WarningMessage]:
    # Regular expression to match the lines with any 'warning' or 'remark' number using verbose mode
    pattern = re.compile(
        r'''
        ^(?P<file_path>.*\.([fF]90|[fF]95|[fF]03|[fF]08|[fF]))  # File path with extensions
        \((?P<line_number>\d+)\):                               # Line number in parentheses
        \s(?P<message_type>warning|remark)                      # Message type (warning or remark)
        \s\#(?P<message_number>\d+):                            # Message number preceded by #
        (?P<message_string>.*)                                  # The actual message
        ''', re.VERBOSE
    )

    messages = []

    lines = log_text.splitlines()
    for line in lines:
        match = pattern.match(line)
        if match:
            file_path = Path(match.group('file_path'))
            line_number = int(match.group('line_number'))
            message_type = match.group('message_type')
            message_number = int(match.group('message_number'))
            message_string = match.group('message_string')
            messages.append(WarningMessage(file_path, line_number, message_type, message_number, message_string))

    return messages

def is_compiled_with_intel(log_text: str) -> bool:
    return "Compiling with Intel" in log_text

def main():
    parser = argparse.ArgumentParser(description='Parse a log file for Fortran warnings and remarks.')
    parser.add_argument('log_file_path', type=str, help='Path to the log file.')
    parser.add_argument('--print-messages', action='store_true', help='Print the messages found in the log file.')
    parser.add_argument('--filter', type=str, nargs='*', action='append', help='Filter warnings and remarks by file path. Only file paths that contain at least one FILTER string are included.')
    parser.add_argument('--project-name', type=str, nargs='?', help='Add the project name to potential error messages.')
    args = parser.parse_args()

    log_file_path = Path(args.log_file_path)
    if not log_file_path.is_file():
        print(f"Error: The file {log_file_path} does not exist.")
        sys.exit(-1)

    log_text: Optional[str] = None

    if log_file_path.suffix in {'.html', '.htm'}:
        log_text = extract_text_from_last_pre_tag(log_file_path)
        if log_text is None:
            print("Error: No <pre> tags found within <body> of html file.")
            sys.exit(-1)
    elif log_file_path.suffix == '.txt':
        with open(log_file_path, 'r', encoding='utf-8') as file:
            log_text = file.read()
    else:
        print("Error: Unsupported file extension. Only .txt and .htm(l) files are supported.")
        sys.exit(-1)

    if not is_compiled_with_intel(log_text):
        print("Error: The log does not indicate that the code was compiled with Intel.")
        sys.exit(-1)

    messages = parse_log(log_text)

    if args.filter:
        # Flatten the list of lists into a single list
        filters = [item for sublist in args.filter for item in sublist]
        messages = [entry for entry in messages if any(f in entry.file_path.as_posix() for f in filters)]

    if args.print_messages:
        for entry in messages:
            print(f"{f'{args.project_name}: ' if args.project_name else ''}The following {entry.message_type} was treated as an error:")
            print(f"{entry.file_path}({entry.line_number}): error #{entry.message_number}: {entry.message_string}")
        print(f"Number of warnings and remarks found: {len(messages)}")

    # Exit with a nonzero code if there are any messages
    sys.exit(len(messages))

if __name__ == '__main__':
    main()
