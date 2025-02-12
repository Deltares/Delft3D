import argparse
import os
import sys
from datetime import datetime

import pytz

# Define the timezone for the Netherlands
netherlands_tz = pytz.timezone("Europe/Amsterdam")

def inplace_change(s: str, old_string: str, new_string: str) -> str:
    """
    Replace occurrences of a substring within a string with a new substring.

    Args:
        s (str): The original string.
        old_string (str): The substring to be replaced.
        new_string (str): The substring to replace with.

    Returns
    -------
        str: The modified string with the old substring replaced by the new substring.
            If the old substring is not found, the original string is returned.
    """
    if old_string in s:
        f = s.replace(old_string, new_string)
        return f
    else:
        return s


def subs_in_file(file_name: str, src: str, dst: str) -> None:
    """
    Replace occurrences of a substring in a .tex file.

    This function reads the content of a .tex file, replaces all occurrences
    of the source substring (src) with the destination substring (dst), and
    writes the modified content back to the file.

    Args:
        file_name (str): The path to the .tex file.
        src (str): The substring to be replaced.
        dst (str): The substring to replace with.

    Returns
    -------
        None
    """
    if file_name.endswith(".tex"):
        s = open(file_name).read()
        s = inplace_change(s, src, dst)
        f = open(file_name, "w")
        f.write(s)
        f.flush()
        f.close()


def recursive_walk(folder: str, script_dir: str) -> None:
    """
    Recursively walks through the given folder, generates documentation files, and organizes them into a LaTeX.

    Args:
        folder (str): The path to the folder to walk through.
        script_dir (str): The path of the script.

    This function performs the following steps:
    1. Creates a "functionalities/chapters" directory inside the given folder if it doesn't exist.
    2. Creates a "testcases.tex" file inside the "functionalities/chapters" directory.
    3. Iterates through the items in the given folder:
        - Skips items containing ".svn".
        - Skips items that do not start with "f" or contain "fxx".
        - Creates a "doc" directory inside each valid item if it doesn't exist.
        - Copies a template functionality report directory to the "doc" directory.
        - Creates a "testcases.tex" file inside the "doc/chapters" directory.
        - Adds a PART section to the main "testcases.tex" file for each valid item.
        - Iterates through the sub-items of each valid item:
            - Skips sub-items containing "cxx" or that do not start with "c".
            - If the sub-item is a directory, adds a CHAPTER section to both the item's "testcases.tex" file
              and the main "testcases.tex" file.
    """
    e_name = os.path.basename(os.path.normpath(folder))
    functionalities_dir = os.path.join(folder, "doc", "functionalities", "chapters")
    if not os.path.exists(functionalities_dir):
        os.makedirs(functionalities_dir)

    funcs_tex = os.path.join(functionalities_dir, "testcases.tex")
    f1 = open(funcs_tex, "w+")
    f1.write("%\n")
    f1.write("% Automatic generated file\n")
    f1.write("%\n")
    f1.close()

    f_names = os.listdir(folder)

    for f_name in f_names:
        if f_name.find(".svn") == -1:
            if f_name.find("fxx") != -1 or f_name[0] != "f":
                continue  # do not generate documentation
            doc_dir = os.path.join(folder, f_name, "doc")
            if not os.path.exists(doc_dir):
                os.makedirs(doc_dir)

            print("%s" % f_name)
            # copy template functionality report directory
            src = os.path.join(script_dir, "template_functionality_report")
            dst = doc_dir

            func_tex = os.path.join(doc_dir, "chapters", "testcases.tex")
            f1 = open(func_tex, "w")
            f1.write("%\n")
            f1.write("% Automatic generated file\n")
            f1.write("%\n")
            f1.close()

            # Separtion of functionalities by added a PART in the document
            f1 = open(funcs_tex, "a")
            part_name = f_name.replace("_", " ")
            dst = "\part{\\newline %s}\n" % (part_name)
            f1.write(dst)
            f1.write("\\adjustptc\n\\parttoc\n\\newpage\n%\n")
            f1.close()

            c_names = os.listdir(f_name)
            for c_name in c_names:
                if c_name.find("cxx") != -1 or c_name[0] != "c":
                    continue  # do not generate documentation
                if os.path.isdir(os.path.join(os.getcwd(), f_name, c_name)):
                    print("\t%s" % c_name)

                    # for each functionality
                    f1 = open(func_tex, "a")
                    chp = c_name.replace("_", " ")
                    dst = "\chapter{%s}\n" % chp
                    f1.write(dst)
                    dst = "\graphicspath{{../%s/doc/}}\n" % c_name
                    f1.write(dst)
                    dst = "\import{../%s/doc/}{chapters/case_text.tex}\n" % c_name
                    f1.write(dst)
                    f1.write("%\n")
                    f1.close()

                    # all functionalities in one document
                    f1 = open(funcs_tex, "a")
                    chp = c_name.replace("_", " ")
                    dst = "\chapter{%s}\n" % chp
                    f1.write(dst)
                    dst = "\graphicspath{{../../%s/%s/doc/}}\n" % (f_name, c_name)
                    f1.write(dst)
                    dst = "\import{../../%s/%s/doc/}{chapters/case_text.tex}\n" % (f_name, c_name)
                    f1.write(dst)
                    f1.write("%\n")
                    f1.close()


def main(argv: list[str]) -> None:
    """
    Batch process and remove side TOC in HTML files.

    The function processes command-line arguments to determine the source directory
    (either relative or absolute) and performs a recursive walk to process HTML files.
    It prints the script directory, start directory, and working directory for reference.
    If the given directory does not exist, it prints an error message and exits.

    Args:
        argv (list[str]): List of command-line arguments.

    Returns
    -------
        None
    """
    parser = argparse.ArgumentParser(description="Batch process to remove side toc in HTML-files")
    # run_mode_group = parser.add_mutually_exclusive_group(required=False)
    parser.add_argument("-r", "--reldir", help="Relative path to engine directory, ex. e01_d3dflow.", dest="rel_dir")
    parser.add_argument("-a", "--absdir", help="Absolute path to engine directory, ex. e01_d3dflow.", dest="abs_dir")
    args = parser.parse_args()

    src_dir = "janm"
    start_dir = os.getcwd()
    if args.rel_dir:
        rel_dir = args.rel_dir
        src_dir = os.path.join(start_dir, rel_dir)
    if args.abs_dir:
        src_dir = args.abs_dir

    if not os.path.exists(src_dir):
        print("Given directory does not exists: %s" % src_dir)
        return

    script_dir = os.path.join(start_dir, os.path.dirname(__file__))

    # relative path

    os.chdir(src_dir)

    print("Script directory : %s" % script_dir)
    print("Start directory  : %s" % start_dir)
    print("Working directory: %s" % os.getcwd())
    recursive_walk(src_dir, script_dir)
    print("Processing done")

    cwd = os.chdir(start_dir)
    return


if __name__ == "__main__":
    start_time = datetime.now(netherlands_tz)
    print("Start: %s\n" % start_time)

    filename = "update_functionality_report.log"
    if os.path.exists(filename):
        os.remove(filename)
    print("Listing is written to: %s" % filename)

    main(sys.argv[0:])

    sys.stdout = sys.__stdout__

    print("\nStart: %s" % start_time)
    print("End  : %s" % datetime.now(netherlands_tz))
    print("Klaar")
