import contextlib
import enum
import logging
import re
from datetime import datetime, timezone
from typing import Callable, Iterator, Literal

from typing_extensions import override


class MessageName(enum.StrEnum):
    """Supported TeamCity message names."""

    MESSAGE = "message"
    TEST_STARTED = "testStarted"
    TEST_FINISHED = "testFinished"
    TEST_FAILED = "testFailed"


@contextlib.contextmanager
def enter_test_context(test_id: str, logger: logging.Logger) -> Iterator[None]:
    """Enter a test context.

    Adds a `test_id` attribute to log records when using `logger` in this
    context. Upon entering this context manager, log a `testStarted` message.
    Upon leaving this context manager, log a `testFinished` message. This
    context manager catches any exception, and logs a `testFailed` message
    when it does so.

    Parameters
    ----------
    test_id : str
        The name of the test. Should be unique Within the TeamCity build.
    logger : logging.Logger
        The logger to attache the `test_id` attribute to.
    """
    try:
        test_id_filter = _filter_factory(test_id)
        logger.addFilter(test_id_filter)
        logger.info("test started: %s", test_id, extra={"message_name": MessageName.TEST_STARTED})
        yield None
    except Exception:
        logger.exception("test failed: %s", test_id, extra={"message_name": MessageName.TEST_FAILED})
    finally:
        logger.info("test finished: %s", test_id, extra={"message_name": MessageName.TEST_FINISHED})
        logger.removeFilter(test_id_filter)


def _filter_factory(test_id: str) -> Callable[[logging.LogRecord], bool]:
    def add_test_id_filter(record: logging.LogRecord) -> bool:
        record.test_id = test_id
        return True

    return add_test_id_filter


class TeamCityFormatter(logging.Formatter):
    """A `logging.Formatter` that formats logs as TeamCity service messages.

    Notes
    -----
    See: https://www.jetbrains.com/help/teamcity/cloud/service-messages.html
    """

    def __init__(
        self,
        fmt: str | None = None,
        datefmt: str | None = None,
        style: Literal["%", "{", "$"] = "%",
    ) -> None:
        super().__init__(fmt=fmt, datefmt=datefmt, style=style)

    @override
    def format(self, record: logging.LogRecord) -> str:
        message_name = getattr(record, "message_name", MessageName.MESSAGE)
        match message_name:
            case MessageName.TEST_STARTED:
                return self._format_test_flow(message_name, record)
            case MessageName.TEST_FINISHED:
                return self._format_test_flow(message_name, record)
            case MessageName.TEST_FAILED:
                return self._format_test_failed(record)
            case _:
                return self._format_message(record)

    @override
    def formatTime(self, record: logging.LogRecord, datefmt: str | None = None) -> str:
        timestamp = datetime.fromtimestamp(record.created, timezone.utc)
        millis = int(round(timestamp.microsecond / 1000))
        return timestamp.strftime(f"%Y-%m-%dT%H:%M:%S.{millis}%z")

    def _format_message(self, record: logging.LogRecord) -> str:
        status = {
            logging.WARNING: "WARNING",
            logging.FATAL: "ERROR",
            logging.ERROR: "ERROR",
            logging.CRITICAL: "ERROR",
        }.get(record.levelno, "NORMAL")
        message = escape_service_message(super().format(record))
        test_id: str | None = getattr(record, "test_id", None)
        flow_id_attr = "" if test_id is None else f"flowId='{escape_service_message(test_id)}' "
        return (
            f"##teamcity[{MessageName.MESSAGE.value} "
            f"timestamp='{self.formatTime(record)}' "
            f"{flow_id_attr}"
            f"status='{status}' "
            f"text='{message}']"
        )

    def _format_test_flow(self, message_name: MessageName, record: logging.LogRecord) -> str:
        test_id: str = escape_service_message(getattr(record, "test_id", "undefined"))
        return (
            f"##teamcity[{message_name.value} "
            f"timestamp='{self.formatTime(record)}' "
            f"flowId='{test_id}' "
            f"name='{test_id}']"
        )

    def _format_test_failed(self, record: logging.LogRecord) -> str:
        test_id: str = escape_service_message(getattr(record, "test_id", "undefined"))
        message = escape_service_message(super().format(record))
        return (
            f"##teamcity[{MessageName.TEST_FAILED.value} "
            f"timestamp='{self.formatTime(record)}' "
            f"flowId='{test_id}' "
            f"name='{test_id}' "
            f"message='{message}']"
        )


def escape_service_message(message: str) -> str:
    r"""Escape TeamCity service message special characters in message.

    TeamCity uses the alternative escape character '|' (vertical bar) instead of
    '\' (back-slash) to encode things like unicode characters and line feeds. In
    addition, square brackets ('[', ']') have special meaning in the teamcity logs.
    These need to be escaped.

    Parameters
    ----------
    message : str
        Input string containing escape sequences like '\n' or '\\'.

    Returns
    -------
    str

    Notes
    -----
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
