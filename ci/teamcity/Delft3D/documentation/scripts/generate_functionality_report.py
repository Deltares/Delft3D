import argparse
import os
import sys
from datetime import datetime

import generate_latex_doc as gdoc
import pytz
from executables import Executables

netherlands_tz = pytz.timezone("Europe/Amsterdam")

_start_dir = "not set"


if __name__ == "__main__":
    start_time = datetime.now(netherlands_tz)

    print("Start: %s\n" % start_time)

    parser = argparse.ArgumentParser(description="Batch process to generate functionality document")
    parser.add_argument(
        "--engine_dir_name", help="Name of the directory of the engine, ex. e106_dflow1d", dest="engine_dir_name"
    )
    args = parser.parse_args()

    funcs_path = ""
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

    # Generate the overview functionalities document
    if os.path.exists(funcs_path):
        um_dir, um_doc = os.path.split(funcs_path)
        error_funcs_doc = gdoc.generate_pdf(um_dir, um_doc, executables)

    error_funcdoc = 0

    # Generate the folder functionality documents
    f_names = os.listdir(engine_dir)
    for f_name in f_names:
        if f_name.find("fxx") == -1:
            if f_name[0] == "f":
                um_dir = os.path.join(engine_dir, f_name, "doc")
                error = gdoc.generate_pdf(um_dir, "functionality_report", executables)
                error_funcdoc = max(error_funcdoc, error)

    max(error_funcdoc, error_funcs_doc)

    print("\nStart: %s" % start_time)
    print("End  : %s" % datetime.now(netherlands_tz))
    print("Klaar")
