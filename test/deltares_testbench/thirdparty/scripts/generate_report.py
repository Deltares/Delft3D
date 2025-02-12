import argparse
import os  # file exists
import sys  # system
from datetime import datetime, timedelta

import generate_latex_doc as gdoc  # gdoc: Generate DOCument
import pytz
from executables import Executables

# Define the timezone for the Netherlands
netherlands_tz = pytz.timezone("Europe/Amsterdam")


def main(argv: list) -> int:
    """Generate report.

    Args:
        argv (list): List of command-line arguments.

    Returns
    -------
        int: The maximum error code encountered during the process.
    """
    _start_dir = "not set"

    _d1 = datetime.now(netherlands_tz) - timedelta(days=1) # reference date (i.e. today)
    _d2 = datetime.now(netherlands_tz) + timedelta(days=1) # reference date minus delta (delta = one day)

    parser = argparse.ArgumentParser(description="Batch process to generate validation and functionality document")
    # run_mode_group = parser.add_mutually_exclusive_group(required=False)
    parser.add_argument("-t", "--texfile", help="Name of the tex-file to generate a document from", dest="val_doc")
    args = parser.parse_args()

    val_path = "JanM"

    _start_dir = os.getcwd()
    if args.val_doc:
        val_doc = args.val_doc
        val_path = os.path.abspath(val_doc)

    if not os.path.exists(val_path):
        print("Given tex-file not found at: %s" % val_path)
        return 1

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

    path_list = val_path.split(os.sep)
    engine_dir = path_list[-4]
    engine_dir = os.path.join(_start_dir, engine_dir)

    # Generate the validation document
    um_dir, um_doc = os.path.split(val_path)
    error_valdoc = gdoc.generate_pdf(um_dir, um_doc, executables)

    return error_valdoc


# ------------------------------------------------------------------------------
if __name__ == "__main__":
    start_time = datetime.now(netherlands_tz)

    print("Start: %s\n" % start_time)

    main(sys.argv[0:])

    print("\nStart: %s" % start_time)
    print("End  : %s" % datetime.now(netherlands_tz))
