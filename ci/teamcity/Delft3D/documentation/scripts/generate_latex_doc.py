import argparse
import os
import subprocess
import sys
from datetime import datetime

import pytz
from executables import Executables

netherlands_tz = pytz.timezone("Europe/Amsterdam")


def print_test_started(name: str, msg: str, capture_standard_output: str) -> None:
    """Print test started message."""
    print_stderr(
        f"##teamcity[testStarted name='{name}' message='{msg}' captureStandardOutput='{capture_standard_output}']"
    )
    return


def print_test_failed(name: str, msg: str, details: str) -> None:
    """Print test failed message."""
    print_stderr(f"##teamcity[testFailed name='{name}' message='{msg}' details='{details}']")
    return


def print_test_finished(name: str) -> None:
    """Print test finished message."""
    print_stderr(f"##teamcity[testFinished name='{name}']")
    return


def print_stderr(msg: str) -> None:
    """Print std error message."""
    sys.stderr.write(msg + "\n")
    return


def run_pdflatex(u_doc: str, pdflatex_exe: str) -> int:
    """Run the pdflatex command on the given document."""
    log_file = open(u_doc + ".log", "w")
    to_execute = '"%s" -synctex=1 -interaction=nonstopmode -shell-escape %s' % (pdflatex_exe, u_doc)
    print(to_execute)
    ret_value = subprocess.call(to_execute, stdout=log_file, stderr=subprocess.STDOUT)
    log_file.close()
    return ret_value


def run_bibtex(u_doc: str, bibtex_exe: str) -> int:
    """Execute the BibTeX command on the provided document."""
    log_file = open(os.devnull, "w")
    to_execute = '"%s" %s' % (bibtex_exe, u_doc)
    print(to_execute)
    ret_value = subprocess.call(to_execute, stdout=log_file, stderr=subprocess.STDOUT)
    log_file.close()
    return ret_value


def run_make_index(u_doc: str, makeindex_exe: str) -> int:
    """Execute the makeindex command on the provided document."""
    log_file = open(os.devnull, "w")
    to_execute = '"%s" %s' % (makeindex_exe, u_doc)
    print(to_execute)
    ret_value = subprocess.call(to_execute, stdout=log_file, stderr=subprocess.STDOUT)
    log_file.close()
    return ret_value


def generate_pdf(u_dir: str, u_doc: str, executables: Executables) -> int:
    """
    Generate a PDF document from LaTeX sources located in the specified directory.

    Args:
        u_dir (str): The directory containing the LaTeX sources.
        u_doc (str): The name of the LaTeX document to generate.

    Returns
    -------
        int: Returns 0 if the PDF generation is successful, otherwise returns 1.
    """
    if os.path.exists(u_dir):
        os.chdir(u_dir)
    else:
        print_test_started(f"Generating: {(u_doc)}", "Nothing to build", "true")
        print_test_failed(f"Generating: {u_doc}", "Directory does not exists", f"{u_dir}")
        print_test_finished(f"Generating: {(u_doc)}")
        return 1

    print("\nEntering: %s" % (os.getcwd()))

    print_test_started(f"Generating: {(u_doc)}", "Final", "true")

    error = run_pdflatex(u_doc, executables.pdflatex)
    if error == 1:
        print_test_failed(f"Generating: {u_doc}", f"PDFLaTeX Run 1 Failed: {os.getcwd()}", executables.pdflatex)
        print_test_finished(f"Generating: {(u_doc)}")
        return 1

    error = run_bibtex(u_doc, executables.bibtex)
    if error == 1:
        print_test_failed(f"Generating: {u_doc}", f"BIBtex Failed: {os.getcwd()}", executables.bibtex)
        return 1

    error = run_pdflatex(u_doc, executables.pdflatex)
    if error == 1:
        print_test_failed(f"Generating: {u_doc}", f"PDFLaTeX Run 2 Failed: {os.getcwd()}", executables.pdflatex)
        return 1

    if os.path.isfile(u_doc + ".idx"):
        error = run_make_index(u_doc + ".idx", executables.makeindex)
        if error == 1:
            print_test_failed(f"Generating: {u_doc}", f"MakeIndex Failed: {os.getcwd()}", {executables.pdflatex})
            return 1

    error = run_pdflatex(u_doc, executables.pdflatex)
    if error == 1:
        print_test_failed(f"Generating: {u_doc}", f"PDFLaTeX Run 3 Failed: {os.getcwd()}", executables.pdflatex)
        return 1

    error = run_pdflatex(u_doc, executables.pdflatex)
    if error == 1:
        print_test_failed(f"Generating: {u_doc}", "PDFLaTeX Run 4 Failed", executables.pdflatex)
        return 1

    print_test_finished(f"Generating: {(u_doc)}")
    return 0


if __name__ == "__main__":
    start_time = datetime.now(netherlands_tz)
    print("Start: %s\n" % start_time)

    parser = argparse.ArgumentParser(description="Batch process to generate user manuals")

    parser.add_argument(
        "-m",
        "--texfile",
        nargs=1,
        help="Build the specified document (i.e. basename of main the tex-file)",
        dest="texfile",
    )

    args = parser.parse_args()

    if args.texfile:
        _um_specified = args.texfile

    error = 0
    try:
        os.environ["PATH"]
    except KeyError:
        print("Please set the environment variable PATH")
        error = 1
    executables = Executables()
    executables.assign_installations()
    if error == 1 or executables.are_executables_invalid():
        print("Check installation")
        sys.exit(1)

    um_dir, um_doc = os.path.split(_um_specified[0])
    lst = list(os.path.splitext(um_doc))
    if lst[1] != ".tex":  # if extension is not '.tex' keep extension
        lst[0] = lst[0] + lst[1]
    error = generate_pdf(um_dir, lst[0], executables)

    print("\nStart: %s" % start_time)
    print("End  : %s" % datetime.now(netherlands_tz))

    if error != 0:
        sys.exit(int(error))

    sys.exit(0)
