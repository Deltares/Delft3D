import abc
import re
from dataclasses import dataclass
from pathlib import Path

from typing_extensions import override

from ci_tools.minio.list_item import ListItem


class Matcher(abc.ABC):
    """Interface for matching `ListItem`s on arbitrary criteria."""

    @abc.abstractmethod
    def match(self, item: ListItem) -> bool:
        """Check if the object matches.

        Parameters
        ----------
        item : ListItem

        Returns
        -------
        bool
            True if the object matches, False otherwise.
        """


@dataclass
class GlobMatcher(Matcher):
    """Matcher that uses glob patterns to match `ListItem`s."""

    pattern: str

    @override
    def match(self, item: ListItem) -> bool:
        return Path(item.relative_path).match(self.pattern)


class RegexMatcher(Matcher):
    """Matcher that uses regex patterns to match `ListItem`s.

    The `re.search` method is used to check if the pattern is found
    anywhere in the `relative_path`. So the pattern does not need to
    match the entire `relative_path`.
    """

    def __init__(self, pattern: str, ignore_case: bool = False) -> None:
        flags = re.IGNORECASE if ignore_case else re.NOFLAG
        self.pattern = re.compile(pattern, flags=flags)

    @override
    def match(self, item: ListItem) -> bool:
        """Check if the `ListItem`s `relative_path` matches the regex pattern.

        Uses the `re.search` method to check if the pattern is found anywhere in the `relative_path`.
        So the pattern does not need to match the entire `relative_path`.
        """
        return self.pattern.search(item.relative_path) is not None
