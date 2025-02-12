import argparse
import os  # file exists
import subprocess  # needed to run a subprocess and catch the result
import sys  # system
from datetime import datetime, timedelta

import generate_latex_doc as gdoc  # gdoc: Generate DOCument
import pytz
from executables import Executables

# Define the timezone for the Netherlands
netherlands_tz = pytz.timezone("Europe/Amsterdam")

_d1 = 0  # reference date (i.e. today)
_d2 = 0  # reference date minus delta (delta = one day)

_start_dir = "not set"



def run_make_index(u_doc: str) -> int:
    """Run the makeindex command on the given document.

    Args:
        u_doc (str): The document to process.

    Returns
    -------
        int: The return value of the subprocess call.
    """
    log_file = open(os.devnull, "w")
    to_execute = '"%s" %s' % (_makeindex, u_doc)
    print(to_execute)
    ret_value = subprocess.call(to_execute, stdout=log_file, stderr=subprocess.STDOUT)
    log_file.close()
    return ret_value


def main(argv: list) -> int:
    """Generate functionality documentation.

    Args:
        argv (list): List of command-line arguments.

    Returns
    -------
        int: The maximum error code encountered during the process.
    """
    global _d1, _d2
    global _start_dir

    _d1 = datetime.now(netherlands_tz) - timedelta(days=1)
    _d2 = datetime.now(netherlands_tz) + timedelta(days=1)

    parser = argparse.ArgumentParser(description="Batch process to generate functionality document")
    parser.add_argument(
        "--engine_dir_name", help="Name of the directory of the engine, ex. e106_dflow1d", dest="engine_dir_name"
    )
    args = parser.parse_args()

    funcs_path = "JanM"
    error_funcs_doc = 0

    _start_dir = os.getcwd()
    if args.engine_dir_name:
        engine_dir_name = args.engine_dir_name
        engine_number, engine_name = engine_dir_name.split("_")
        funcs_path = os.path.join(
            _start_dir, engine_dir_name, "doc", "functionalities", engine_name + "_functionalities_doc.tex"
        )
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

    path_list = funcs_path.split(os.sep)
    engine_dir = path_list[-4]
    engine_dir = os.path.join(_start_dir, engine_dir)

    # Generate the functionalities document
    if os.path.exists(funcs_path):
        um_dir, um_doc = os.path.split(funcs_path)
        error_funcs_doc = gdoc.generate_pdf(um_dir, um_doc, executables)

    error_funcdoc = 0

    # Generate the functionality documents
    f_names = os.listdir(engine_dir)
    for f_name in f_names:
        if f_name.find("fxx") == -1:
            if f_name[0] == "f":
                um_dir = os.path.join(engine_dir, f_name, "doc")
                error = gdoc.generate_pdf(um_dir, "functionality_report", executables)
                error_funcdoc = max(error_funcdoc, error)

    return max(error_funcdoc, error_funcs_doc)


if __name__ == "__main__":
    start_time = datetime.now(netherlands_tz)

    print("Start: %s\n" % start_time)

    main(sys.argv[0:])

    print("\nStart: %s" % start_time)
    print("End  : %s" % datetime.now(netherlands_tz))
    print("Klaar")
