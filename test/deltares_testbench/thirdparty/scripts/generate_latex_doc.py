import argparse
import os  # file exists
import subprocess  # needed to run a subprocess and catch the result
import sys  # system
from datetime import datetime, timedelta

import pytz
from executables import Executables

# Define the timezone for the Netherlands
netherlands_tz = pytz.timezone("Europe/Amsterdam")

_d1 = 0  # reference date (i.e. today)
_d2 = 0  # reference date minus delta (delta = one day)
_draft = 0
_force = 0
suites = []


_build = 0  # count successful builds
_build_skipped = 0  # count skipped builds, because it is not modified in the last several (7) days
_build_failure = 0  # count failed builds

_um_specified = ["not set"]


def print_stderr(msg: str) -> None:
    """Print std error message.

    Args:
        msg (str): The message to print.
    """
    sys.stderr.write(msg + "\n")
    return


def run_pdflatex(u_doc: str, pdflatex_exe: str) -> int:
    """
    Run the pdflatex command on the given document.

    This function constructs a command to run pdflatex with specific options and executes it.
    The output of the command is written to a log file named after the document.

    Args:
        u_doc (str): The path to the document to be processed by pdflatex (without file extension).
        pdflatex_exe (str): The path to the executable.

    Returns
    -------
        int: The return value of the subprocess call, indicating the success or failure of the pdflatex command.
    """
    log_file = open(u_doc + ".log", "w")
    to_execute = '"%s" --pool-size=5000000 -shell-escape -interaction=nonstopmode %s' % (pdflatex_exe, u_doc)
    print(to_execute)
    ret_value = subprocess.call(to_execute, stdout=log_file, stderr=subprocess.STDOUT)
    log_file.close()
    return ret_value


def run_bibtex(u_doc: str, bibtex_exe: str) -> int:
    """
    Execute the BibTeX command on the provided document.

    Args:
        u_doc (str): The name of the document to process with BibTeX.
        bibtex_exe (str): The path to the executable.

    Returns
    -------
        int: Always returns 0 to indicate a successful run of BibTeX.
    """
    log_file = open(os.devnull, "w")
    to_execute = '"%s" %s' % (bibtex_exe, u_doc)
    print(to_execute)
    ret_value = subprocess.call(to_execute, stdout=log_file, stderr=subprocess.STDOUT)
    log_file.close()
    return 0  # Force a succesfull run of bibtex


def run_make_index(u_doc: str, makeindex_exe: str) -> int:
    """
    Execute the makeindex command on the provided document.

    Args:
        u_doc (str): The path to the document on which to run the makeindex command.
        makeindex_exe (str): The path to the executable.

    Returns
    -------
        int: The return code from the subprocess call to makeindex.
    """
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
    global _start_dir
    # global _svnexe
    global _build, _build_skipped, _build_failure
    if os.path.exists(u_dir):
        os.chdir(u_dir)
    else:
        print_stderr(
            "##teamcity[testStarted  name='Generating: %s' message='Nothing to build' captureStandardOutput='true']"
            % (u_doc)
        )
        print_stderr(
            "##teamcity[testFailed name='Generating: %s' message='Directory does not exists' details='%s']"
            % (u_doc, u_dir)
        )
        print_stderr("##teamcity[testFinished name='Generating: %s']" % (u_doc))
        return 1

    print("\nEntering: %s" % (os.getcwd()))

    svn_build = "build_it"

    if svn_build == "":
        print_stderr(
            "##teamcity[testStarted  name='Generating: %s' message='Nothing to build' captureStandardOutput='true']"
            % (u_doc)
        )
        print_stderr("##teamcity[testFinished name='Generating: %s']" % (u_doc))
        _build_skipped += 1
    else:
        if _draft:
            substitute_draft(u_doc)
            print_stderr(
                "##teamcity[testStarted  name='Generating: %s' message='Draft' captureStandardOutput='true']" % (u_doc)
            )
        else:
            print_stderr(
                "##teamcity[testStarted  name='Generating: %s' message='Final' captureStandardOutput='true']" % (u_doc)
            )

        error = run_pdflatex(u_doc, executables.pdflatex)
        if error == 1:
            print_stderr(
                "##teamcity[testFailed name='Generating: %s' message='PDFLaTeX Run 1 Failed: %s' details='%s']"
                % (u_doc, os.getcwd(), executables.pdflatex)
            )
            print_stderr("##teamcity[testFinished name='Generating: %s']" % (u_doc))
            _build_failure += 1
            return 1
        error = run_bibtex(u_doc, executables.bibtex)
        if error == 1:
            print_stderr(
                "##teamcity[testFailed name='Generating: %s' message='BIBtex Failed: %s' details='%s']"
                % (u_doc, os.getcwd(), executables.bibtex)
            )
            _build_failure += 1
            return 1
        error = run_pdflatex(u_doc, executables.pdflatex)
        if error == 1:
            print_stderr(
                "##teamcity[testFailed name='Generating: %s' message='PDFLaTeX Run 2 Failed: %s' details='%s']"
                % (u_doc, os.getcwd(), executables.pdflatex)
            )
            _build_failure += 1
            return 1
        if os.path.isfile(u_doc + ".idx"):
            error = run_make_index(u_doc + ".idx", executables.makeindex)
            if error == 1:
                print_stderr(
                    "##teamcity[testFailed name='Generating: %s' message='MakeIndex Failed: %s' details='%s']"
                    % (u_doc, os.getcwd(), executables.pdflatex)
                )
                _build_failure += 1
                return 1
        error = run_pdflatex(u_doc, executables.pdflatex)
        if error == 1:
            print_stderr(
                "##teamcity[testFailed name='Generating: %s' message='PDFLaTeX Run 3 Failed: %s' details='%s']"
                % (u_doc, os.getcwd(), executables.pdflatex)
            )
            _build_failure += 1
            return 1
        error = run_pdflatex(u_doc, executables.pdflatex)
        if error == 1:
            print_stderr(
                "##teamcity[testFailed name='Generating: %s' message='PDFLaTeX Run 4 Failed' details='%s']"
                % (u_doc, executables.pdflatex)
            )
            _build_failure += 1
            return 1
        print_stderr("##teamcity[testFinished name='Generating: %s']" % (u_doc))
        _build += 1
    return 0


def main(argv: list[str]) -> int:
    """
    Process command-line arguments and generate the PDF document.

    Args:
        argv (list): List of command-line arguments.

    Returns
    -------
        int: Returns 0 if successful, otherwise returns 1.
    """
    global _d1, _d2
    global _start_dir
    global _draft, _force
    global _um_specified

    parser = argparse.ArgumentParser(description="Batch process to generate user manuals")
    # run_mode_group = parser.add_mutually_exclusive_group(required=False)

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

    _d1 = datetime.now(netherlands_tz) - timedelta(days=1)
    _d2 = datetime.now(netherlands_tz) + timedelta(days=1)
    _start_dir = os.getcwd()

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

    return error


# ------------------------------------------------------------------------------
if __name__ == "__main__":
    start_time = datetime.now(netherlands_tz)
    print("Start: %s\n" % start_time)

    error = main(sys.argv[0:])

    print("\nStart: %s" % start_time)
    print("End  : %s" % datetime.now(netherlands_tz))

    if error != 0:
        sys.exit(int(error))

    sys.exit(0)
