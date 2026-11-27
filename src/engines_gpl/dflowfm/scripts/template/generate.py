#!/usr/bin/env python

"""Parse Fortran source files and generate BMI functions from templates."""

import argparse
import fileinput
import glob
import inspect
import json
import logging
import os
import re
from pathlib import Path

import mako.lookup
import mako.template


def main(fortranfiles, templatedir="templates"):
    """read the fortran files, parse them and apply them
    to the templates in templatedir"""

    variable_re = re.compile(
        r"""
    ^\s*                                                                 # start of the line
    ([!][$]BMIEXPORT\s*)?                                                # optionally allow commented variables, useful if they resolve to another (non-commented) variable via the JSON "internal:" attribute.
    (?P<fortrantype>(character|logical|double\s+precision|integer|real)) # type
    .*                                                                   # anything
    (::)                                                                 # double colon
    \s*                                                                  # possible spaces
    (?P<name>\w[\w\d]*)                                                  # the variable name
    (?P<dimension>([(][:,\w]+[)])?)                                      # dimension
    .*                                                                   # anything
    [!][<]?\s*                                                           # comment
    ([(](?P<altname>[\w/\d-]+)[)])?                                      # alternative name
    \s*                                                                  # space
    [[](?P<unit>[ \.\*\w/\d-]+)[]]                                       # unit
    \s*                                                                  # space
    (?P<description>.*?)                                                 # description
    \s*                                                                  # space
    ((?P<json>[{].*[}]))                                                 # JSON key-value pairs
    \s*                                                                  # space
    $                                                                    # end of line
    """,
        re.VERBOSE,
    )

    FORTRANTYPESMAP = {
        "logical": "bool",
        "character": "char",
        "double precision": "double",
        "real": "float",
        "integer": "int",
    }

    variables = []
    numv = 0
    for fortranfile in fortranfiles:
        logging.info("Scanning file %s...", fortranfile)
        with open(Path(fortranfile)) as f:
            for linenr, line in enumerate(f, start=1):
                match = variable_re.match(line)
                if match:
                    variable = match.groupdict()
                    if variable["dimension"].strip():
                        variable["rank"] = variable["dimension"].count(",") + 1
                    else:
                        variable["rank"] = 0
                    variable["type"] = FORTRANTYPESMAP[variable["fortrantype"]]

                    try:
                        variable.update(json.loads(variable["json"]))
                    except ValueError:
                        logging.exception(
                            "Variable's JSON string could not be parsed: %s. File: %s, line %d.",
                            variable,
                            fortranfile,
                            linenr,
                        )

                    # shape overwrites rank
                    if "shape" in variable:
                        variable["rank"] = len(variable["shape"])

                    variables.append(variable)
        logging.info("Extracted %d variables.", len(variables) - numv)
        numv = len(variables)

    logging.info("Total: extracted %d variables.", len(variables))

    # Create some extra variables that can be used in the template
    ISOTYPESMAP = {
        "bool": "logical(c_bool)",
        "char": "character(kind=c_char)",
        "double": "real(c_double)",
        "float": "real(c_float)",
        "int": "integer(c_int)",
    }

    def dimstr(shape):
        shapetxt = ",".join(shape)
        if shapetxt:
            return "(" + shapetxt + ")"
        else:
            return ""

    templates = [
        template
        for template in os.listdir(templatedir)
        if template.lower().endswith(".f90")
    ]

    lookup = mako.lookup.TemplateLookup(
        directories=[templatedir], module_directory="/tmp/mako_modules"
    )

    for template_name in templates:
        # template = lookup.get_template(template_name) # is not so nice on Windows: changes CRLF into CRCRLF.
        ftpl = open(os.path.join(templatedir, template_name), "r")
        text = ftpl.read()
        template = mako.template.Template(text=text)

        filename = template_name.replace(".f90", ".inc")
        with open(filename, "w") as f:
            # You can use all the local variables in the templates
            rendered = template.render(**locals())
            # print rendered
            f.write(rendered)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--fortranfiles",
        nargs="+",
        action="extend",
        required=True,
        metavar="FILE",
        help="Fortran source paths to scan.",
    )
    parser.add_argument(
        "--template-dir",
        default="templates",
        help="Directory containing the Fortran templates.",
    )
    parser.add_argument(
        "--verbose", action="store_true", help="Log files scanned and variable counts."
    )
    arguments = parser.parse_args()

    if arguments.verbose:
        logging.basicConfig(level=logging.INFO)

    main(arguments.fortranfiles, arguments.template_dir)
