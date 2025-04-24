import argparse
import os
import sys
from datetime import datetime

import pytz

netherlands_tz = pytz.timezone("Europe/Amsterdam")


def auto_generate_functionality_latex_files(engine_folder: str) -> None:
    """
    Recursively walks through the given folder, generates documentation files, and organizes them into a LaTeX.

    Args:
        engine_folder (str): The path to the folder to walk through.
    """
    functionalities_dir = os.path.join(engine_folder, "doc", "functionalities", "chapters")
    if not os.path.exists(functionalities_dir):
        os.makedirs(functionalities_dir)

    functionality_overview_file = os.path.join(functionalities_dir, "testcases.tex")
    write_file_with_auto_generated_header(functionality_overview_file)

    f_names = os.listdir(engine_folder)

    for folder_name in f_names:
        if folder_name.find(".svn") == -1:
            if folder_name.find("fxx") != -1 or folder_name[0] != "f":
                print(f"Do not generate documentation for {folder_name} in {engine_folder}.")
                continue

            doc_dir = os.path.join(engine_folder, folder_name, "doc")
            if not os.path.exists(doc_dir):
                os.makedirs(doc_dir)

            print(f"{folder_name}")
            funtionality_case_file = os.path.join(doc_dir, "chapters", "testcases.tex")
            if os.path.exists(funtionality_case_file):
                write_file_with_auto_generated_header(funtionality_case_file)
                append_table_of_contents(functionality_overview_file, folder_name)

                case_names = os.listdir(folder_name)
                for case_name in case_names:
                    if case_name.find("cxx") != -1 or case_name[0] != "c":
                        print(f"Do not generate documentation for {case_name} in {folder_name} folder.")
                        continue  # do not generate documentation

                    if os.path.isdir(os.path.join(os.getcwd(), folder_name, case_name)):
                        print(f"\t{case_name}")
                        append_case_to_functionality_file(funtionality_case_file, case_name)
                        append_case_to_overview_file(functionality_overview_file, folder_name, case_name)
            else:
                print(f"Do not generate documentation for {folder_name} as {funtionality_case_file} does not exist.")


def append_table_of_contents(functionality_overview_file: str, folder_name: str) -> None:
    """Append table of contents to file."""
    file = open(functionality_overview_file, "a")
    part_name = folder_name.replace("_", " ")
    line = "\part{\\newline %s}\n" % (part_name)
    file.write(line)
    file.write("\\adjustptc\n\\parttoc\n\\newpage\n%\n")
    file.close()


def append_case_to_overview_file(functionality_overview_file: str, folder_name: str, case_name: str) -> None:
    """Append case to overview file."""
    file = open(functionality_overview_file, "a")
    case = case_name.replace("_", " ")
    line = "\chapter{%s}\n" % case
    file.write(line)
    line = "\graphicspath{{../../%s/%s/doc/}}\n" % (folder_name, case_name)
    file.write(line)
    line = "\import{../../%s/%s/doc/}{chapters/case_text.tex}\n" % (folder_name, case_name)
    file.write(line)
    file.write("%\n")
    file.close()


def append_case_to_functionality_file(funtionality_case_file: str, case_name: str) -> None:
    """Append case to functionality file."""
    file = open(funtionality_case_file, "a")
    case = case_name.replace("_", " ")
    line = "\chapter{%s}\n" % case
    file.write(line)
    line = "\graphicspath{{../%s/doc/}}\n" % case_name
    file.write(line)
    line = "\import{../%s/doc/}{chapters/case_text.tex}\n" % case_name
    file.write(line)
    file.write("%\n")
    file.close()


def write_file_with_auto_generated_header(file_path: str) -> None:
    """Write file with auto generated header."""
    file = open(file_path, "w+")
    file.write("%\n")
    file.write("% Automatic generated file\n")
    file.write("%\n")
    file.close()


if __name__ == "__main__":
    start_time = datetime.now(netherlands_tz)
    print("Start: %s\n" % start_time)

    filename = "update_functionality_report.log"
    if os.path.exists(filename):
        os.remove(filename)
    print("Listing is written to: %s" % filename)

    parser = argparse.ArgumentParser(description="Batch process to remove side toc in HTML-files")
    parser.add_argument("-r", "--reldir", help="Relative path to engine directory, ex. e01_d3dflow.", dest="rel_dir")
    args = parser.parse_args()

    start_dir = os.getcwd()
    src_dir = os.path.join(start_dir, args.rel_dir)

    if not os.path.exists(src_dir):
        print("Given directory does not exists: %s" % src_dir)
        sys.exit(1)

    os.chdir(src_dir)

    print("Start directory  : %s" % start_dir)
    print("Working directory: %s" % os.getcwd())
    auto_generate_functionality_latex_files(src_dir)
    print("Processing done")

    os.chdir(start_dir)

    sys.stdout = sys.__stdout__

    print("\nStart: %s" % start_time)
    print("End  : %s" % datetime.now(netherlands_tz))
    print("Klaar")
