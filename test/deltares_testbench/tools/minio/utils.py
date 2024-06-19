import enum
from datetime import datetime, timedelta, timezone
from typing import Iterator, List


class Color(enum.Enum):
    """Use to map color to ANSI terminal color code."""

    RED = 31
    GREEN = 32
    YELLOW = 33


def color(s: str, color: Color) -> str:
    """Use ANSI escape codes to color text."""
    return f"\x1b[{color.value}m{s}\x1b[0m"


def format_size(size: int) -> str:
    """Print size `size` in bytes in a human readable format.

    Examples
    --------
    >>> format_size(100)
    '100 B  '
    >>> format_size(4096)
    '4 KiB'
    >>> format_size(42 * 1024 ** 3 + 42)
    '42 GiB'
    """

    def gen_quotients(n: int, divisor: int) -> Iterator[int]:
        while True:
            yield n
            n = n // divisor

    suffixes = ("B  ", "KiB", "MiB", "GiB")
    return next(
        f"{quo} {suffixes[power]}"
        for power, quo in enumerate(gen_quotients(size, 1024))
        if quo < 1024 or power + 1 == len(suffixes)
    )


def ceil_dt(dt: datetime, delta: timedelta) -> datetime:
    """Return the next datetime which is a multiple of `delta`.

    Examples
    --------
    >>> ceil_dt(datetime(2024, 4, 1, 12, 1, tzinfo=timezone.utc), timedelta(hours=1))
    datetime.datetime(2024, 4, 1, 13, 0, tzinfo=datetime.timezone.utc)
    >>> ceil_dt(datetime(2024, 4, 1, 12, 30, 0, tzinfo=timezone.utc), timedelta(minutes=1))
    datetime.datetime(2024, 4, 1, 12, 30, tzinfo=datetime.timezone.utc)
    >>> ceil_dt(datetime(2023, 12, 31, 23, 59, 59, tzinfo=timezone.utc), timedelta(minutes=1))
    datetime.datetime(2024, 1, 1, 0, 0, tzinfo=datetime.timezone.utc)
    """
    zero = datetime.min.replace(tzinfo=timezone.utc)
    quo, rem = divmod(dt - zero, delta)
    return zero + (quo + 1) * delta if rem else dt


def to_unix_path(path: str) -> str:
    r"""Convert Windows path separators with Unix path separators.

    Both kind of path separators work on Windows, on Unix only the
    unix path separators are valid.

    Examples
    --------
    >>> to_unix_path(r'\foo\bar/qux\quux')
    '/foo/bar/qux/quux'
    """
    return path.replace("\\", "/")


def resolve_relative(path: str) -> str:
    """Resolve relative path fragments ('.' and '..') in path strings.

    Parameters
    ----------
    path : str
        A 'path' string. Unix path separators are assumed.

    Raises
    ------
    ValueError
        When resolving a 'parent' ('..') directory is used to move out
        of the 'root' directory in the path.

    Examples
    --------
    >>> resolve_relative("/foo/../bar.txt")
    '/bar.txt'
    >>> resolve_relative("foo/../bar.txt")
    'bar.txt'
    >>> resolve_relative("./foo.txt")
    'foo.txt'
    >>> resolve_relative("foo/././bar/./../baz.txt")
    'foo/baz.txt'
    >>> resolve_relative("foo/../../bar.txt")
    Traceback (most recent call last):
    ValueError: ...
    """
    result: List[str] = []
    for part in path.split("/"):
        if part == ".":
            continue
        elif part == "..":
            if not result:
                raise ValueError("Invalid relative path: Can't move out of root directory.")
            result.pop()
        else:
            result.append(part)
    return "/".join(result)
