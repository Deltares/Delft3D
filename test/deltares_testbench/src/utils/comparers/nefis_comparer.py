"""Nefis file comparer.

Copyright (C)  Stichting Deltares, 2024
"""

import copy
import os
import time
from typing import List, Tuple

from src.config.file_check import FileCheck
from src.config.parameter import Parameter
from src.config.program_config import ProgramConfig
from src.suite.program import Program
from src.utils.comparers.ascii_comparer import AsciiComparer
from src.utils.comparers.comparison_result import ComparisonResult
from src.utils.comparers.i_comparer import IComparer
from src.utils.logging.i_logger import ILogger


class NefisComparer(IComparer):
    """Compare nefis content equality."""

    def __init__(self, vs_program: Program) -> None:
        self.__vs_program = vs_program

    def compare(
        self,
        left_path: str,
        right_path: str,
        file_check: FileCheck,
        testcase_name: str,
        logger: ILogger,
    ) -> List[Tuple[str, FileCheck, Parameter, ComparisonResult]]:
        # need to generate instruction file for vs.exe
        str_time = str(time.time())
        tmp_filename = f"vs_{str_time}.tmp"
        vs_stdout = f"vs_{str_time}.out"
        dict_quantity_filename = self.__createVsInput__(left_path, file_check, tmp_filename, logger)
        self.__createVsInput__(right_path, file_check, tmp_filename, logger)

        # run vs for all filenames
        in_file = f"<{tmp_filename} >{vs_stdout} 2>&1"
        self.__runVs__(left_path, in_file, self.__vs_program, logger)
        self.__runVs__(right_path, in_file, self.__vs_program, logger)

        # for all parameters, run the ascii comparer.
        comparer = AsciiComparer()
        results = []
        for parameters in file_check.parameters.values():
            for parameter in parameters:
                logger.debug(f"Checking parameter:  {parameter.name}")
                file_check_ascii = FileCheck()
                file_check_ascii.name = dict_quantity_filename[parameter.name]
                # file_check_ascii.setParameters({'1':parameters})
                name = str(parameter.name)
                param = []
                param.append(parameter)
                params = {}
                params[name] = param
                file_check_ascii.parameters = params
                try:
                    result_lst = comparer.compare(
                        left_path, right_path, file_check_ascii, testcase_name, logger
                    )  # The result should be [ (testcase_name, file_check, parameter, ascii_result ) ]

                    # reset filename to NEFIS filename in stead of the filename which was used by the comparer
                    result_lst[0][1].name = file_check.name
                    results += result_lst
                except Exception as e:
                    logger.error(e)
                del file_check_ascii

        return results

    # create a vs config file
    # input: path, filecheck, filename
    # output: array of filenames that will be created by vs
    def __createVsInput__(self, path: str, filecheck: FileCheck, uf: str, logger: ILogger):
        defextension = ".def"
        if os.path.splitext(filecheck.name)[1] == ".hda":
            defextension = ".hdf"
        if os.path.splitext(filecheck.name)[1] == ".ada":
            defextension = ".adf"
        if os.path.splitext(filecheck.name)[1] == ".nda":  # SOBEK restart file
            defextension = ".ndf"
        if os.path.splitext(filecheck.name)[1] == ".wdo":  # WANDA
            defextension = ".wdo"
        logger.debug(f"Creating temporary file {os.path.join(path, uf)}")
        result = {}  # Maps quantityNames to filenames.
        with open(os.path.join(path, uf), "w") as tmpinfile:
            tmpinfile.write(f"use {filecheck.name} def {os.path.splitext(filecheck.name)[0]}{defextension}\n")
            for grp in filecheck.parameters.keys():
                for i in range(0, len(filecheck.parameters[grp])):
                    quantityName = filecheck.parameters[grp][i].name
                    # WARNING: v/fn are not allowed to be "too long" (16 is the limit)
                    v = f"{quantityName[:11]}_{i}"
                    tmpinfile.write(f"let {v} = {quantityName} from {grp}\n")
                    filename = f"{filecheck.name[:8]}-{v}.tkl"
                    result[quantityName] = filename
                    tmpinfile.write(f"write {v} to {filename}\n")
            tmpinfile.write("quit\n")
        tmpinfile.closed
        return result

    def __runVs__(self, path: str, infile: str, vs_program: Program, logger: ILogger):
        logger.debug("initializing vs")
        pcnf = ProgramConfig()
        pcnf.working_directory = path
        pcnf.arguments = [infile]
        logger.debug(f"vs workdir {path}, infile {infile}")
        prgm = copy.deepcopy(vs_program)
        prgm.overwriteConfiguration(pcnf)
        try:
            logger.debug("running vs")
            prgm.run(logger)
            logger.debug("finished vs")
        except SystemError:
            logger.warning("vs executable can give errors while functioning, continuing")
