"""Common static functions.

Copyright (C)  Stichting Deltares, 2024
"""

import os
import platform
import re
import shutil
import string
import subprocess
import tempfile
from typing import Dict, Iterator, List, Tuple

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
def add_search_path(environment, sp, logger: ILogger) -> None:
    search_path = Paths().rebuildToLocalPath(sp)
    if platform.system() == "Windows":
        logger.debug(f"Adding windows search path {search_path}")
        environment["PATH"] = f'{search_path};{environment["PATH"]}'
    else:
        logger.debug(f"Adding linux search path {search_path}")
        environment["LD_LIBRARY_PATH"] = f'{search_path}:{environment["LD_LIBRARY_PATH"]}'
        environment["PATH"] = f'{search_path}:{environment["PATH"]}'


def escape_teamcity(message: str) -> str:
    r"""Replace regular escape sequences in string with TeamCity escape sequences.

    TeamCity uses the alternative escape character '|' (vertical bar) instead of
    '\\' (back-slash) to encode things like unicode characters and line feeds. In
    addition, square brackets ('[', ']') have special meaning in the teamcity logs.
    These need to be escaped as well.

    Parameters
    ----------
    message : str
        Input string containing escape sequences like '\n' or '\\'.

    Returns
    -------
    str
        String with regular escape sequences replaced with TeamCity escape sequences.

    Notes
    -----
    TeamCity documentation:
    https://www.jetbrains.com/help/teamcity/cloud/service-messages.html#Escaped+Values
    """

    def generate_segments(message: str) -> Iterator[str]:
        last_end = 0
        for mo in re.finditer(r"[\n\r\[\]'|\u0080-\uffff]", message, re.UNICODE):
            begin, end = mo.span()
            yield message[last_end:begin]
            last_end = end

            char = mo.group()
            code = ord(char)
            if char in "\n\r":
                yield "|" + chr(char.encode("unicode_escape")[-1])
            elif char in "[]'|":
                yield "|" + char
            elif 0x80 <= code <= 0xFFFF:
                yield f"|0x{code:04X}"  # Watch out: code must be exactly four characters.
            else:
                raise RuntimeError("Unhandled escape character.")

        yield message[last_end:]

    return "".join(generate_segments(message))


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
    current_folder = os.path.join(os.path.dirname(os.path.realpath(__file__)), "..", "..")

    return os.path.join(current_folder, "logs")


def get_log_level(level):
    """Parse log level string to enum."""
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
def mount_network_drive(server: str, folder: str, credentials: Credentials, logger: ILogger) -> Tuple[str, bool]:
    """Mount a network drive in linux or windows.

    Parameters
    ----------
    server : str
        Server name.
    folder : str
        Folder name.
    credentials : Credentials
        Credentials to use.
    logger : ILogger
        Logger to use.

    Raises
    ------
    OSError
        If no drive letters available.

    Returns
    -------
    Tuple[str, bool]
        Mount point, mounted (true/false).
    """
    if platform.system() == "Windows":
        pmp = check_if_already_mounted(server, folder, logger)
        if pmp != "":
            return pmp, False
        dl = getAvailableWindowsDriveLetter()
        if not dl:
            raise OSError("no drive letters available")
        mount_point = dl + ":"
        cmd = f"net use {mount_point} \\\\{server}\\{folder}"
        if credentials:
            cmd = f"{cmd} /user:{credentials.username} {credentials.password}"
    else:
        mount_point = tempfile.mkdtemp()
        cmd = f"mount -t smbfs //{server}/{folder} {mount_point}"
        if credentials:
            cmd = f"{cmd} -o username={credentials.username},password={credentials.password}"
    subprocess.check_call(cmd, shell=True)
    return mount_point, True


def unmount_network_drive(mount_point: str) -> None:
    """Unmount a network drive in linux or windows.

    Parameters
    ----------
    mount_point : str
        Mountpoint to unmount.
    """
    if platform.system() == "Windows":
        cmd = f"net use {mount_point} /DELETE /YES"
    else:
        cmd = f"umount -l {mount_point}"
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
        if val not in drives:
            return val
    return None


# check if a drive with folder is already mounted
def check_if_already_mounted(server, folder, logger: ILogger):
    if platform.system() == "Windows":
        process = subprocess.Popen(["net", "use"], stdout=subprocess.PIPE)
        while True:
            line = process.stdout.readline()
            if line == "" and process.poll() is not None:
                break
            if f" \\\\{server}\\{folder}" in line:
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
) -> None:
    """Log a header.

    Resulting header looks like:
    ```
    ==============
    header
    ==============
    ```

    Parameters
    ----------
    header : str
        Header to log.
    logger : ILogger
        Logger to use.
    log_level : LogLevel, optional
        Level to log for. Defaults to `LogLevel.INFO`.
    width : int, optional
        Width of the header. Defaults to 150.
    char : str, optional
        Char to use for begin/end of header. Defaults to "=".
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
) -> None:
    """Log a sub header.

    Sub header looks like:
    ```
    -- header ---------
    ```

    Parameters
    ----------
    header : str
        Header to log
    logger : ILogger
        Logger to use
    log_level : LogLevel, optional
        Level to log for. Defaults to LogLevel.INFO.
    width : int, optional
        Width of the header. Defaults to 150.
    char : str, optional
        Char to use for the header. Defaults to "=".
    """
    log_separator_with_name(header, logger, log_level, width, char)


def log_separator(
    logger: ILogger,
    log_level: LogLevel = LogLevel.INFO,
    width: int = 150,
    char: str = "=",
    with_new_line: bool = False,
) -> None:
    """Log a separator.

    Separator looks like:
    ```
    ===============
    ```

    Parameters
    ----------
    logger : ILogger
        Logger to use.
    log_level : LogLevel, optional
        Level to log for. Defaults to `LogLevel.INFO`.
    width : int, optional
        Width of the separator. Defaults to 150.
    char : str, optional
        Char to use for separator. Defaults to "-".
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
) -> None:
    """Log a separator with a name.

    Separator looks like:
    ```
    == name ========
    ```

    Parameters
    ----------
    name : str
        Name to log.
    logger : ILogger
        Logger to use.
    log_level : LogLevel, optional
        Level to log for. Defaults to `LogLevel.INFO`.
    width : int, optional
        Width of the separator. Defaults to 150.
    char : str, optional
        Char to use for separator. Defaults to "=".
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
) -> None:
    """Log a dictionary as a table.

    Table looks like:
    ```
    -------------------------------
    |header 1|header 2   |header 3|
    -------------------------------
    |       2|test string|     3.9|
    |       5|test 2     |     2.6|
    -------------------------------
    ```

    Parameters
    ----------
    table : Dict[str, List[str]]
        Table (keys as headers).
    logger : ILogger
        Logger to use.
    log_level : LogLevel, optional)
        Level to log for. Defaults to `LogLevel.INFO`.
    char : str, optional
        Char to use for header. Defaults to "-".
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
        value_to_print = DictTable.format_value(row_value)
        row_str += f"|{value_to_print:{max_lengths[column_index]}}"

    return f"{row_str}|"


def delete_directory(directory: str, logger: ILogger) -> None:
    """Delete a directory recursively.

    Parameters
    ----------
    directory : str
        Directory to remove.
    logger : ILogger
        Logger for logging errors.
    """
    for filename in os.listdir(directory):
        file_path = os.path.join(directory, filename)
        try:
            if os.path.isfile(file_path) or os.path.islink(file_path):
                os.unlink(file_path)
            elif os.path.isdir(file_path):
                shutil.rmtree(file_path)
        except Exception as exc:
            logger.warning(f"error removing {directory}: {exc}")

    if os.path.exists(directory):
        os.rmdir(directory)
