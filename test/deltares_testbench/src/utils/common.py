"""
Description: Common static functions
-----------------------------------------------------
Copyright (C)  Stichting Deltares, 2013
"""
import os
import platform
import string
import subprocess
import tempfile
from typing import Dict, List, Tuple

from src.config.credentials import Credentials
from src.utils.dict_table import DictTable
from src.utils.logging.i_logger import ILogger
from src.utils.logging.log_level import LogLevel
from src.utils.paths import Paths

if platform.system() == "Windows":
    try:
        from ctypes import windll
    except:
        print("Unable to import windll")


# add search path to environment
# input: environment to add search path to, path
def add_search_path(environment, sp, logger: ILogger):
    # Return immediately when ".svn" is in the path
    if str(sp).find(".svn") != -1:
        return
    search_path = Paths().rebuildToLocalPath(sp)
    if platform.system() == "Windows":
        logger.debug(f"Adding windows search path {search_path}")
        environment["PATH"] = search_path + ";" + environment["PATH"]
    else:
        logger.debug(f"Adding linux search path {search_path}")
        environment["LD_LIBRARY_PATH"] = (
            search_path + ":" + environment["LD_LIBRARY_PATH"]
        )
        environment["PATH"] = search_path + ":" + environment["PATH"]


# strip all characters from a string that need to be escaped for TeamCity
def stripEscapeCharacters(cstr):
    if not cstr or cstr == "":
        return ""
    vstr = (
        str(cstr)
        .replace("\r", "")
        .replace("\n", "")
        .replace("'", "")
        .replace('"', "")
        .replace("|", "||")
        .replace("[", "")
        .replace("]", "")
    )
    # The decode method messes up the string completely on Linux
    # vstr = vstr.decode('utf-8')
    return vstr


# Replace a word by "***" when it follows a word containing the string "password"
# Needed when logging a command containing a password
def stripPassword(cstr):
    if not cstr or cstr == "":
        return ""
    splitchar = " "
    words = str(cstr).split(splitchar)
    i = 0
    for aword in words:
        if aword.find("password") > -1:
            words[i + 1] = "******"
        i += 1
    vstr = splitchar.join(words)
    # The decode method messes up the string completely on Linux
    # vstr = vstr.decode('utf-8')
    return vstr


def get_default_logging_folder_path() -> str:
    current_folder = os.path.join(
        os.path.dirname(os.path.realpath(__file__)), "..", ".."
    )

    return os.path.join(current_folder, "logs")


def get_log_level(level):
    """parse log level string to enum"""
    if not level or str(level) == "":
        return LogLevel.DEBUG
    if "info" in str(level).lower():
        return LogLevel.INFO
    if "warn" in str(level).lower():
        return LogLevel.WARNING
    if "err" in str(level).lower():
        return LogLevel.ERROR
    return LogLevel.DEBUG


#
# input: server name, folder name, optional credentials
# output: mount point, boolean specifying if we created it or if it already exists
def mount_network_drive(
    server: str, folder: str, credentials: Credentials, logger: ILogger
) -> Tuple[str, bool]:
    """mount a network drive in linux or windows

    Args:
        server (str): server name
        folder (str): folder name
        credentials (Credentials): credentials to use
        logger (ILogger): logger to use

    Raises:
        OSError: if no drive letters available

    Returns:
        Tuple[str, bool]: mount point, mounted (true/false)
    """
    if platform.system() == "Windows":
        pmp = check_if_already_mounted(server, folder, logger)
        if pmp != "":
            return pmp, False
        dl = getAvailableWindowsDriveLetter()
        if not dl:
            raise OSError("no drive letters available")
        mount_point = dl + ":"
        cmd = "net use " + mount_point + " \\\\" + server + "\\" + folder
        if credentials:
            cmd = cmd + " /user:" + credentials.username + " " + credentials.password
    else:
        mount_point = tempfile.mkdtemp()
        cmd = "mount -t smbfs //" + server + "/" + folder + " " + mount_point
        if credentials:
            cmd = (
                cmd
                + " -o username="
                + credentials.username
                + ",password="
                + credentials.password
            )
    subprocess.check_call(cmd, shell=True)
    return mount_point, True


def unmount_network_drive(mount_point: str):
    """unmount a network drive in linux or windows

    Args:
        mount_point (str): mountpoint to unmount
    """
    if platform.system() == "Windows":
        cmd = "net use " + mount_point + " /DELETE /YES"
    else:
        cmd = "umount -l " + mount_point
    subprocess.check_call(cmd, shell=True)


# detect available windows drive letter for mounting
def getAvailableWindowsDriveLetter():
    drives = []
    bitmask = windll.kernel32.GetLogicalDrives()
    for letter in string.uppercase:
        if bitmask & 1:
            drives.append(letter.lower())
        bitmask >>= 1
    for i in range(1, 26):
        val = string.lowercase[:26][-i]
        if not val in drives:
            return val
    return None


# check if a drive with folder is already mounted
def check_if_already_mounted(server, folder, logger: ILogger):
    if platform.system() == "Windows":
        process = subprocess.Popen(["net", "use"], stdout=subprocess.PIPE)
        while True:
            line = process.stdout.readline()
            if line == "" and process.poll() != None:
                break
            if " \\\\" + server + "\\" + folder in line:
                return line[line.find(":") - 1 : line.find(":") + 1]
        (_, stderr) = process.communicate()
        if stderr:
            logger.error(stderr)
        process.stdout.close()
    return ""


def log_header(
    header: str,
    logger: ILogger,
    log_level: LogLevel = LogLevel.INFO,
    width: int = 150,
    char: str = "=",
):
    """Logs a header like:
    ==============
    header
    ==============

    Args:
        header (str): header to log
        logger (ILogger): logger to use
        log_level (LogLevel, optional): level to log for. Defaults to LogLevel.INFO.
        width (int, optional): width of the header. Defaults to 150.
        char (str, optional): char to use for begin/end of header. Defaults to "=".
    """
    log_separator(logger, log_level, width, char)
    logger.log(header, log_level)
    log_separator(logger, log_level, width, char)


def log_sub_header(
    header: str,
    logger: ILogger,
    log_level: LogLevel = LogLevel.INFO,
    width: int = 150,
    char: str = "-",
):
    """Logs a sub header like
    -- header ---------

    Args:
        header (str): header to log
        logger (ILogger): logger to use
        log_level (LogLevel, optional): level to log for. Defaults to LogLevel.INFO.
        width (int, optional): width of the header. Defaults to 150.
        char (str, optional): char to use for the header. Defaults to "=".
    """
    log_separator_with_name(header, logger, log_level, width, char)


def log_separator(
    logger: ILogger,
    log_level: LogLevel = LogLevel.INFO,
    width: int = 150,
    char: str = "=",
    with_new_line: bool = False,
):
    """Logs a separator like
    ===============

    Args:
        logger (ILogger): logger to use
        log_level (LogLevel, optional): level to log for. Defaults to LogLevel.INFO.
        width (int, optional): width of the separator. Defaults to 150.
        char (str, optional): char to use for separator. Defaults to "-".
    """
    logger.log((char * width), log_level)
    if with_new_line:
        logger.log("", log_level)


def log_separator_with_name(
    name: str,
    logger: ILogger,
    log_level: LogLevel = LogLevel.INFO,
    width: int = 150,
    char: str = "=",
):
    """Logs a separator with a name like
    == name ========

    Args:
        name (str): name to log
        logger (ILogger): logger to use
        log_level (LogLevel, optional): level to log for. Defaults to LogLevel.INFO.
        width (int, optional): width of the separator. Defaults to 150.
        char (str, optional): char to use for separator. Defaults to "=".
    """
    name_to_print = f" {name} "
    name_length = len(name_to_print)

    text = (char * 2) + name_to_print + (char * (width - name_length - 2))

    logger.log(text, log_level)


def log_table(
    table: Dict[str, List[str]],
    logger: ILogger,
    log_level: LogLevel = LogLevel.INFO,
    char: str = "-",
):
    """Logs a dictionary as a table like:
    -------------------------------
    |header 1|header 2   |header 3|
    -------------------------------
    |       2|test string|     3.9|
    |       5|test 2     |     2.6|
    -------------------------------
    Args:
        table (Dict[str, List[str]]): table (keys as headers)
        logger (ILogger): logger to use
        log_level (LogLevel, optional): level to log for. Defaults to LogLevel.INFO.
        char (str, optional): char to use for header. Defaults to "-".
    """
    dict_table = DictTable(table)

    max_lengths = [dict_table.max_column_width(k) for k in table.keys()]
    total_length = sum(max_lengths) + len(dict_table.headers) + 1
    number_of_rows = dict_table.number_of_rows()

    header_str = __create_table_row(list(table.keys()), max_lengths)
    log_header(header_str, logger, log_level, total_length, char)

    for row_index in range(0, number_of_rows):
        row = dict_table.row_values(row_index)
        row_str = __create_table_row(
            row,
            max_lengths,
        )
        logger.log(row_str, log_level)

    log_separator(logger, log_level, total_length, char)


def __create_table_row(row: List, max_lengths: List[int]) -> str:
    row_str = ""
    for column_index, row_value in enumerate(row):
        if isinstance(row_value, float):
            value_to_print = f"{row_value:.3e}"
        else:
            value_to_print = row_value
        row_str += f"|{value_to_print:{max_lengths[column_index]}}"

    return f"{row_str}|"


# meta class for singleton type
class Singleton(type):
    _instances = {}

    # singleton constructor instance
    def __call__(self, *args, **kwargs):
        if self not in self._instances:
            self._instances[self] = super(Singleton, self).__call__(*args, **kwargs)
        return self._instances[self]
