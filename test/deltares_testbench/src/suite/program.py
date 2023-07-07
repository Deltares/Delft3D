"""
Description: Process runner for test suite
-----------------------------------------------------
Copyright (C)  Stichting Deltares, 2013
"""

import copy
import ctypes
import os
import platform
import re
import subprocess
import threading
import time
from datetime import datetime
from typing import Optional

from src.config.program_config import ProgramConfig
from src.suite.run_time_data import RunTimeData
from src.suite.test_bench_settings import TestBenchSettings
from src.utils.common import add_search_path, stripPassword
from src.utils.logging.file_logger import FileLogger
from src.utils.logging.i_logger import ILogger
from src.utils.logging.log_level import LogLevel
from src.utils.paths import Paths


class Program:
    """Process runner that runs a program (part of a test case)"""

    # global variables
    __process: Optional[subprocess.Popen] = None
    __error = None
    __stop = False

    # constructor
    def __init__(self, program_config: ProgramConfig, settings: TestBenchSettings):
        if not program_config:
            raise RuntimeError("Cannot instantiate a program without a configuration")
        self.__program_config = program_config
        self.__settings: TestBenchSettings = copy.deepcopy(settings)

    @property
    def name(self) -> str:
        """The name of the Program"""
        return self.__program_config.name

    def run(self, logger: ILogger):
        """start a thread to run the program"""

        # reset thread flags
        self.__stop = False
        thread = threading.Thread(target=self.__execute__, args=[logger])
        thread.start()
        # set thread join on time-out if one is given
        if self.__program_config.max_run_time == 0:
            thread.join()
        else:
            logger.debug(f"Waiting {str(self.__program_config.max_run_time)} for join")
            thread.join(self.__program_config.max_run_time)
            # if the thread is still alive, runners are still active
            if thread.is_alive():
                # if the thread is still running, terminate all runners
                logger.warning(
                    f"{self.__program_config.name} thread still alive after maximum run time, terminating processes"
                )
                self.terminate(logger)
                # end the thread
                thread.join()
                self.__error = RuntimeError(
                    "Execution of "
                    + self.__program_config.name
                    + " timed out (s) > "
                    + str(self.__program_config.max_run_time)
                )

    def overwriteConfiguration(self, program_config: ProgramConfig):
        """overwrite program configuration settings

        Args:
            program_config (ProgramConfig): configuration to get parameters from
        """
        if program_config:
            # overwrite name if one is given
            if program_config.name != "":
                self.__program_config.name = program_config.name
            # overwrite a shell if one is given
            if program_config.shell:
                if program_config.shell.working_directory:
                    program_config.shell.working_directory = Paths().rebuildToLocalPath(
                        program_config.shell.working_directory
                    )
                else:
                    program_config.shell.working_directory = (
                        self.__program_config.working_directory
                    )

                self.__program_config.shell = program_config.shell
            # overwrite arguments if they are given
            if len(program_config.arguments) > 0:
                self.__program_config.arguments = program_config.arguments
            # overwrite shell arguments if they are given
            if len(program_config.shell_arguments) > 0:
                self.__program_config.shell_arguments = program_config.shell_arguments
            # overwrite working directory if it is given
            if program_config.working_directory:
                self.__program_config.working_directory = (
                    program_config.working_directory
                )
            # add environment settings
            if len(program_config.environment_variables) > 0:
                for ev in program_config.environment_variables:
                    self.__program_config.environment_variables[
                        ev
                    ] = program_config.environment_variables[ev]
            if len(program_config.search_paths) > 0:
                for sp in program_config.search_paths:
                    self.__program_config.search_paths.append(sp)
            if len(program_config.modules) > 0:
                for mod in program_config.modules:
                    self.__program_config.modules.append(mod)
            # overwrite max run time if one is given
            if program_config.max_run_time > 0:
                self.__program_config.max_run_time = program_config.max_run_time
            # overwrite delay if one is given
            if program_config.delay > 0:
                self.__program_config.delay = program_config.delay
            # overwrite flags
            if program_config.ignore_standard_error:
                self.__program_config.ignore_standard_error = True
            if program_config.ignore_return_value:
                self.__program_config.ignore_return_value = True
            if program_config.program_remove_quotes:
                self.__program_config.program_remove_quotes = True
            if program_config.shell_remove_quotes:
                self.__program_config.shell_remove_quotes = True

    def getError(self):
        """return sub process errors if any"""
        return self.__error

    def terminate(self, logger: ILogger):
        """terminate the run"""
        if self.__stop:
            return

        self.__stop = True

        if not self.__process:
            return
        #
        # It is important that all process activities are stopped
        # Checking whether the process still exists/ is active does
        # not seems to be necessary on Windows. So try it the rough way:
        logger.debug("Regular process termination")
        try:
            self.__process.terminate()
        except:
            logger.debug("Termination generated error")

        logger.debug("Regular process killing")
        try:
            self.__process.kill()
        except:
            logger.debug("Process kill generated error")

    def __execute__(self, logger: ILogger):
        """run given configuration"""
        try:
            logger.debug(
                f"Starting {self.__program_config.absolute_bin_path} with"
                + " delay {str(self.__program_config.delay)}"
            )
            time.sleep(self.__program_config.delay)
            execmd = self.__buildExeCommand__(logger)
            # Don't display the Windows GPF dialog if the invoked program dies.
            if platform.system() == "Windows":
                # http://msdn.microsoft.com/en-us/library/windows/desktop/ms680621(v=vs.85).aspx
                # 1 : SEM_FAILCRITICALERRORS = 0x0001
                # 2 : SEM_NOGPFAULTERRORBOX = 0x0002
                # 3 : SEM_FAILCRITICALERRORS | SEM_NOGPFAULTERRORBOX = 0x0003
                # 4 : SEM_NOALIGNMENTFAULTEXCEPT = 0x0004
                # 8 : SEM_NOOPENFILEERRORBOX = 0x8000
                # !! NOTE : that we are not able to suppress debug build errors of the runtime, only release builds
                ctypes.windll.kernel32.SetErrorMode(0x8003)  # @UndefinedVariable
            if self.__stop:
                logger.debug("Received terminate, stopped before starting process.")
            else:
                logger.info(f"Executing :: {str(stripPassword(execmd))}")
                logger.debug(
                    f"Executing (in separate thread)::{str(stripPassword(execmd))}"
                    + "::in directory::{self.__program_config.working_directory}"
                )

                program_env = self.__program_config.environment
                tb_root = self.__settings.test_bench_root
                if tb_root is not None:
                    program_env["TestBenchRoot"] = tb_root
                self.__process = subprocess.Popen(
                    execmd,
                    stdout=subprocess.PIPE,
                    stderr=subprocess.PIPE,
                    cwd=self.__program_config.working_directory,
                    env=program_env,
                )

                logger.debug("Subprocess created")
                # clear previous errors if they exist and create output variable
                self.__error = None
                out = ""
                # write output while executing, optionally create a log file
                unique_name = "%s-%s-%s" % (
                    self.__program_config.name,
                    datetime.now().strftime("%y%m%d_%H%M%S"),
                    str(self.__process.pid),
                )
                log_file = os.path.abspath(
                    os.path.join(
                        str(self.__program_config.working_directory),
                        ("%s.log" % (unique_name)),
                    )
                )
                file_logger: Optional[FileLogger] = None
                if self.__program_config.log_output_to_file:
                    logger.debug(f"Program output will be written to: {log_file}")
                    file_logger = FileLogger(LogLevel.DEBUG, unique_name, log_file)

                # needed for showing output while running
                while not self.__stop:
                    line = self.__process.stdout.readline().decode("utf-8")
                    if line == "" and self.__process.poll() != None:
                        break
                    elif line != "":
                        out += line
                        if file_logger:
                            file_logger.debug(line)

                # catch errors
                (_, stderr) = self.__process.communicate()
                # store output
                if self.__program_config.store_output:
                    RunTimeData().addOutput(self, out)
                # check results
                if self.__process.returncode != 0:
                    prog_path = str(self.__program_config.path)
                    return_code = str(self.__process.returncode)
                    if self.__program_config.ignore_return_value:
                        logger.debug(
                            f"{prog_path} generated non-null "
                            + f"error code {return_code}, but ignoring it"
                        )
                    else:
                        logger.warning(
                            f"{prog_path} generated "
                            + f"non-null error code {return_code}"
                        )
                        self.__error = subprocess.CalledProcessError(
                            self.__process.returncode,
                            self.__program_config.path,
                            "process raised non null error code",
                        )
                if stderr:
                    prog_path = str(self.__program_config.path)
                    error_message = str(stderr.rstrip()).replace("'", "")
                    if self.__program_config.ignore_standard_error:
                        logger.debug(
                            f"{prog_path} contained error message -"
                            + f" {error_message}, but ignoring it"
                        )
                    else:
                        logger.warning(
                            f"{prog_path} contained error message - {error_message}"
                        )
                        self.__error = subprocess.CalledProcessError(
                            -1, self.__program_config.path, str(stderr).rstrip()
                        )
                self.__process.stdout.close()
                if platform.system() == "Windows":
                    # clean up lingering subprocess
                    self.terminate(logger)
        except Exception as e:
            self.__error = str(e)

    # create a command string for either windows or linux
    def __buildExeCommand__(self, logger: ILogger):
        logger.debug("Building command to be executed")
        if platform.system() != "Windows":
            if len(self.__program_config.modules) > 0 or (
                self.__program_config.shell
                and len(self.__program_config.shell.modules) > 0
            ):
                logger.info(
                    "found modules to load, and on a non-windows system, trying to load module shell"
                )
                if not "MODULEPATH" in os.environ:
                    f = open(os.environ["MODULESHOME"] + "/init/.modulespath", "r")
                    path = []
                    for line in f.readlines():
                        line = re.sub("#.*$", "", line)
                        if line != "":
                            path.append(line)
                    os.environ["MODULEPATH"] = ":".join(path)
                if not "LOADEDMODULES" in os.environ:
                    os.environ["LOADEDMODULES"] = ""
                # initialize the modules environment
                if self.__program_config.shell:
                    logger.info("found shell modules, loading them first")
                    for mod in self.__program_config.shell.modules:
                        logger.debug(f"loading {mod}")
                        (output, error) = subprocess.Popen(
                            ["/usr/bin/modulecmd", "python", "load", mod],
                            stdout=subprocess.PIPE,
                        ).communicate()
                        exec(output)
                        if error:
                            logger.error(error)
                logger.debug("loading program modules")
                for mod in self.__program_config.modules:
                    logger.debug(f"loading {mod}")
                    (output, error) = subprocess.Popen(
                        ["/usr/bin/modulecmd", "python", "load", mod],
                        stdout=subprocess.PIPE,
                    ).communicate()
                    exec(output)
                    if error:
                        logger.error(error)
                logger.debug("done loading modules")
        # create a new working environment
        self.__program_config.environment = os.environ.copy()
        if "LD_LIBRARY_PATH" in self.__program_config.environment.keys():
            sep = ":"
            if platform.system() == "Windows":
                sep = ";"
            self.__program_config.environment["LD_LIBRARY_PATH"] = "%s%s." % (
                self.__program_config.environment["LD_LIBRARY_PATH"],
                sep,
            )
        else:
            self.__program_config.environment["LD_LIBRARY_PATH"] = "."
        logger.debug(
            f"LD_LIBRARY_PATH set to {self.__program_config.environment['LD_LIBRARY_PATH']}",
        )
        # add environment variables
        for ev in self.__program_config.environment_variables:
            logger.debug(
                f"Adding environment variable {ev} : "
                + f"{self.__program_config.environment_variables[ev][1]}"
            )
            self.__program_config.environment[ev] = self.__insertOutputVariable__(
                self.__program_config.environment_variables[ev][1], logger
            )
        for sp in self.__program_config.search_paths:
            add_search_path(self.__program_config.environment, sp, logger)
        # start with the binary path
        cmdAndArgs = str(self.__program_config.absolute_bin_path)
        if platform.system() == "Windows":
            # Needed when the path contains spaces
            cmdAndArgs = '"' + cmdAndArgs + '"'
        # add the given arguments
        for arg in self.__program_config.arguments:
            a = str(arg)
            # Add quotes around the argument when it is a path,
            # unless a flag is used to switch this off
            if Paths().isPath(arg):
                a = Paths().rebuildToLocalPath(a)
                if (
                    not self.__program_config.shell
                    and not self.__program_config.program_remove_quotes
                ) or (
                    self.__program_config.shell
                    and self.__program_config.shell.shell_remove_quotes
                ):
                    a = '"' + a + '"'
            cmdAndArgs += " " + a
        # replace argument variables containing [output()] variable and others with actual value
        cmdAndArgs = self.__insertOutputVariable__(cmdAndArgs, logger)
        # if a shell has been specified we need to reformat the command string
        if self.__program_config.shell:
            # add shell environment variables to the working environment
            for ev in self.__program_config.shell.environment_variables:
                logger.debug(
                    f"adding shell environment variable {ev} :"
                    + f" {self.__program_config.shell.environment_variables[ev][1]}"
                )
                self.__program_config.environment[ev] = self.__insertOutputVariable__(
                    self.__program_config.shell.environment_variables[ev][1], logger
                )
            for sp in self.__program_config.shell.search_paths:
                add_search_path(self.__program_config.environment, sp, logger)
            # format shell command
            shlAndArgs = str(self.__program_config.shell.absolute_bin_path)
            LocalShellArgument = self.__program_config.shell_arguments
            if LocalShellArgument == "":
                for arg in self.__program_config.shell.arguments:
                    a = str(arg)
                    if (
                        Paths().isPath(arg)
                        and not self.__program_config.shell.shell_remove_quotes
                    ):
                        a = '"' + a + '"'
                    shlAndArgs += " " + a
            else:
                shlAndArgs += " " + LocalShellArgument
            shlAndArgs = self.__insertOutputVariable__(shlAndArgs, logger)
            if platform.system() == "Windows":
                if self.__program_config.shell.shell_remove_quotes:
                    # Example: 'cmd \c vs <infile'
                    return str(shlAndArgs + " " + cmdAndArgs)
                # Example: 'cmd \c "vs <infile"'
                return str(shlAndArgs + ' "' + cmdAndArgs + '"')
            else:
                execmd = shlAndArgs.strip().split()
                if self.__program_config.shell.shell_remove_quotes:
                    # Example: ['bash', '-c', 'vs', '<infile']
                    execmd.extend(cmdAndArgs.split(" "))
                else:
                    # Example: ['bash', '-c', 'vs <infile']
                    execmd.append(cmdAndArgs)
                return execmd
        else:
            if platform.system() == "Windows":
                # Example: 'svn checkout -r head url targetdir'
                return cmdAndArgs
            else:
                # Example: ['svn', 'checkout', '-r', 'head', 'url', 'targetdir']
                return cmdAndArgs.split(" ")

    # replace an output variable and other with actual value
    # input: string to search in
    # return value: modified string
    def __insertOutputVariable__(self, original: str, logger: ILogger):
        # replace [output(some_name)] result from previous run some_name in command string
        retval = original
        if "[output(" in retval:
            sr = re.search(
                r"(?<=(\[output\())(.*?)(?=\)\])", retval, flags=re.IGNORECASE
            )
            if sr and len(sr.groups()) > 0:
                k = retval.find("[output(")
                outstr = RunTimeData().getOutputByName(sr.group(0))
                logger.debug(f"substituted: {outstr}")
                # retval = re.sub(r"\[output\((.*?)\)\]", outstr.encode('string_escape'), retval)
                outstr = os.path.join(outstr, retval[:k])
                retval = retval[k:]
                for i in range(0, len(sr.groups())):
                    retval = retval[len(sr.groups()[i]) :]
                retval = retval[3:]  # ')]/'
                retval = os.path.join(outstr, retval)
            else:
                logger.warning(f"Could not match {original} as [output(var)]")
        if "[programpath(" in retval:
            sr = re.search(
                r"(?<=(\[programpath\())(.*?)(?=\)\])", retval, flags=re.IGNORECASE
            )
            if sr and len(sr.groups()) > 0:
                k = retval.find("[programpath(")

                absolute_path = next(
                    p.absolute_bin_path
                    for p in self.__settings.programs
                    if p.name == sr.group(0)
                )

                outstr = os.path.dirname(absolute_path)

                logger.debug(f"substituted: {outstr}")
                outstr = retval[:k] + outstr
                retval = retval[k:]
                for i in range(0, len(sr.groups())):
                    retval = retval[len(sr.groups()[i]) :]
                retval = retval[3:]  # ')]/'
                retval = os.path.join(outstr, retval)
            else:
                logger.warning(f"Could not match {original} as [programpath(var)]")

        # call this routine again if there is still something to replace
        if "[programpath(" in retval:
            retval = self.__insertOutputVariable__(retval, logger)
        return retval
