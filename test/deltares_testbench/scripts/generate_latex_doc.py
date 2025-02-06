import argparse
import os  # file exists
import subprocess  # needed to run a subprocess and catch the result
import sys  # system
from datetime import datetime, timedelta

import pytz

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

_bibtex = "not set"
_initexmf = "not set"
_makeindex = "not set"
_miktexpm = "not set"
_pdflatex = "not set"
_start_dir = "not set"
_svnexe = "not set"

_um_specified = ["not set"]


def is_exe(fpath: str) -> bool:
    """Check if the file at the given path is executable.

    Args:
        fpath (str): The file path.

    Returns
    -------
        bool: True if the file is executable, False otherwise.
    """
    return os.path.isfile(fpath) and os.access(fpath, os.X_OK)


def print_stderr(msg: str) -> None:
    """Print std error message.

    Args:
        msg (str): The message to print.
    """
    sys.stderr.write(msg + "\n")
    return


def which(program: str) -> str:
    """Locate a program file in the system's PATH.

    Args:
        program (str): The name of the program to locate.

    Returns
    -------
        str: The path to the program if found, None otherwise.
    """
    fpath, fname = os.path.split(program)
    if fpath:
        if is_exe(program):
            return program
    else:
        for path in os.environ["PATH"].split(os.pathsep):
            path = path.strip('"')
            exe_file = os.path.join(path, program)
            if is_exe(exe_file):
                return exe_file

    return None


def check_installation() -> int:
    """Check the installation of required executables.

    Returns
    -------
        int: 0 if all required executables are found, 1 otherwise.
    """
    global _bibtex
    global _initexmf
    global _makeindex
    global _miktexpm
    global _pdflatex
    global _start_dir
    global _svnexe

    try:
        os.environ["PATH"]
    except KeyError:
        print("Please set the environment variable PATH")
        return 1

    _bibtex = which("bibtex.exe")
    _initexmf = which("initexmf.exe")
    _makeindex = which("makeindex.exe")
    _miktexpm = which("mpm.exe")
    _pdflatex = which("pdflatex.exe")

    print("Using bibtex   : %s" % _bibtex)
    print("Using initexmf : %s" % _initexmf)
    print("Using makeindex: %s" % _makeindex)
    print("Using miktexpm : %s" % _miktexpm)
    print("Using pdflatex : %s" % _pdflatex)

    if _bibtex is None or _initexmf is None or _makeindex is None or _miktexpm is None or _pdflatex is None:
        return 1

    _svnexe = which("svn.exe")
    if _svnexe is None:
        return 1
    print("Using svn      : %s" % _svnexe)
    return 0


def run_pdflatex(u_doc: str) -> int:
    """
    Run the pdflatex command on the given document.

    This function constructs a command to run pdflatex with specific options and executes it.
    The output of the command is written to a log file named after the document.

    Args:
        u_doc (str): The path to the document to be processed by pdflatex (without file extension).

    Returns
    -------
        int: The return value of the subprocess call, indicating the success or failure of the pdflatex command.
    """
    log_file = open(u_doc + ".log", "w")
    to_execute = '"%s" --pool-size=5000000 -shell-escape -interaction=nonstopmode %s' % (_pdflatex, u_doc)
    print(to_execute)
    ret_value = subprocess.call(to_execute, stdout=log_file, stderr=subprocess.STDOUT)
    log_file.close()
    return ret_value


def run_bibtex(u_doc: str) -> int:
    """
    Execute the BibTeX command on the provided document.

    Args:
        u_doc (str): The name of the document to process with BibTeX.

    Returns
    -------
        int: Always returns 0 to indicate a successful run of BibTeX.
    """
    log_file = open(os.devnull, "w")
    to_execute = '"%s" %s' % (_bibtex, u_doc)
    print(to_execute)
    ret_value = subprocess.call(to_execute, stdout=log_file, stderr=subprocess.STDOUT)
    log_file.close()
    return 0  # Force a succesfull run of bibtex


def run_make_index(u_doc: str) -> int:
    """
    Execute the makeindex command on the provided document.

    Args:
        u_doc (str): The path to the document on which to run the makeindex command.

    Returns
    -------
        int: The return code from the subprocess call to makeindex.
    """
    log_file = open(os.devnull, "w")
    to_execute = '"%s" %s' % (_makeindex, u_doc)
    print(to_execute)
    ret_value = subprocess.call(to_execute, stdout=log_file, stderr=subprocess.STDOUT)
    log_file.close()
    return ret_value


def generate_pdf(u_dir: str, u_doc: str) -> int:
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
    global _svnexe
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

        to_execute = "%s update" % _svnexe
        svn_update = subprocess.Popen(to_execute, stdout=subprocess.PIPE, stderr=subprocess.STDOUT).wait()
        if svn_update == "":
            print_stderr(
                "##teamcity[testFailed name='Generating: %s' message='SVN Update Failed' details='%s']"
                % (u_doc, to_execute)
            )
            _build_failure += 1
            return 1

        error = run_pdflatex(u_doc)
        if error == 1:
            print_stderr(
                "##teamcity[testFailed name='Generating: %s' message='PDFLaTeX Run 1 Failed: %s' details='%s']"
                % (u_doc, os.getcwd(), _pdflatex)
            )
            print_stderr("##teamcity[testFinished name='Generating: %s']" % (u_doc))
            _build_failure += 1
            return 1
        error = run_bibtex(u_doc)
        if error == 1:
            print_stderr(
                "##teamcity[testFailed name='Generating: %s' message='BIBtex Failed: %s' details='%s']"
                % (u_doc, os.getcwd(), _bibtex)
            )
            _build_failure += 1
            return 1
        error = run_pdflatex(u_doc)
        if error == 1:
            print_stderr(
                "##teamcity[testFailed name='Generating: %s' message='PDFLaTeX Run 2 Failed: %s' details='%s']"
                % (u_doc, os.getcwd(), _pdflatex)
            )
            _build_failure += 1
            return 1
        if os.path.isfile(u_doc + ".idx"):
            error = run_make_index(u_doc + ".idx")
            if error == 1:
                print_stderr(
                    "##teamcity[testFailed name='Generating: %s' message='MakeIndex Failed: %s' details='%s']"
                    % (u_doc, os.getcwd(), _pdflatex)
                )
                _build_failure += 1
                return 1
        error = run_pdflatex(u_doc)
        if error == 1:
            print_stderr(
                "##teamcity[testFailed name='Generating: %s' message='PDFLaTeX Run 3 Failed: %s' details='%s']"
                % (u_doc, os.getcwd(), _pdflatex)
            )
            _build_failure += 1
            return 1
        error = run_pdflatex(u_doc)
        if error == 1:
            print_stderr(
                "##teamcity[testFailed name='Generating: %s' message='PDFLaTeX Run 4 Failed' details='%s']"
                % (u_doc, _pdflatex)
            )
            _build_failure += 1
            return 1
        print_stderr("##teamcity[testFinished name='Generating: %s']" % (u_doc))
        _build += 1
    return 0


def main(argv):
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

    error = check_installation()
    if error == 1:
        print("Check installation: FAILED")
        sys.exit(1)

    um_dir, um_doc = os.path.split(_um_specified[0])
    lst = list(os.path.splitext(um_doc))
    if lst[1] != ".tex":  # if extension is not '.tex' keep extension
        lst[0] = lst[0] + lst[1]
    error = generate_pdf(um_dir, lst[0])

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
