import argparse
import os
import sys
from datetime import datetime

import generate_latex_doc as gdoc
import pytz
from executables import Executables

# Define the timezone for the Netherlands
netherlands_tz = pytz.timezone("Europe/Amsterdam")


if __name__ == "__main__":
    start_time = datetime.now(netherlands_tz)

    print("Start: %s\n" % start_time)

    parser = argparse.ArgumentParser(description="Batch process to generate validation and functionality document")
    parser.add_argument("-t", "--texfile", help="Name of the tex-file to generate a document from", dest="val_doc")
    args = parser.parse_args()

    start_work_directory = os.getcwd()

    val_path = ""
    if args.val_doc:
        val_path = os.path.abspath(args.val_doc)

    if not os.path.exists(val_path):
        print(f"Given tex-file not found at:{val_path}")
        sys.exit(1)

    try:
        os.environ["PATH"]
    except KeyError:
        print("Please set the environment variable PATH")
        sys.exit(1)

    executables = Executables()
    executables.assign_installations()
    if executables.are_executables_invalid():
        print("Check installation")
        sys.exit(1)

    path_list = val_path.split(os.sep)
    engine_dir = path_list[-4]
    engine_dir = os.path.join(start_work_directory, engine_dir)

    # Generate the validation document
    um_dir, um_doc = os.path.split(val_path)
    error_valdoc = gdoc.generate_pdf(um_dir, um_doc, executables)

    print("\nStart: %s" % start_time)
    print("End  : %s" % datetime.now(netherlands_tz))
    sys.exit(error_valdoc)
