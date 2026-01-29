import abc
import os
from datetime import datetime, timezone
from typing import ClassVar

from typing_extensions import override


class Clock:
    """Interface for getting the current time."""

    @abc.abstractmethod
    def now(self) -> datetime:
        """Get the current time.

        Returns
        -------
        datetime
            The current time with UTC timezone.
        """


class SystemClock(Clock):
    """Clock that uses the system clock to get the current time."""

    @override
    def now(self) -> datetime:
        return datetime.now(timezone.utc)


class TerminalSize:
    """Interface for getting the terminal size."""

    @property
    @abc.abstractmethod
    def terminal_size(self) -> tuple[int, int]:
        """Get the terminal size.

        Returns
        -------
        tuple[int, int]
            The terminal size as a tuple of (rows, columns).
        """


class OsTerminalSize(TerminalSize):
    """Terminal size that uses the os module to get the terminal size."""

    @property
    @override
    def terminal_size(self) -> tuple[int, int]:
        term_size = os.get_terminal_size()
        return (term_size.lines, term_size.columns)


class ProgressBar:
    """A simple progress bar for tracking the progress of a task."""

    UNITS: ClassVar[tuple[str, str, str, str, str]] = ("B", "KiB", "MiB", "GiB", "TiB")

    def __init__(
        self,
        total_size: int,
        object_count: int,
        show_progress_bar: bool = True,
        terminal_size: TerminalSize | None = None,
        clock: Clock | None = None,
    ) -> None:
        self._total_size = total_size
        self._object_count = object_count
        self._show_progress_bar = show_progress_bar

        self._current_size = 0
        self._completed_objects = 0
        self._bar_is_displayed = False

        self._terminal_size = terminal_size or OsTerminalSize()
        self._clock = clock or SystemClock()

        self._start_time = self._clock.now()

    def add_size(self, size: int) -> None:
        """Add size to the current size.

        Parameters
        ----------
        size : int
        """
        self._current_size += size

    def add_object_count(self, object_count: int) -> None:
        """Add object count to the current object count.

        Parameters
        ----------
        object_count : int
        """
        self._completed_objects += object_count

    def is_complete(self) -> bool:
        """Check if the progress bar is complete."""
        return self._current_size >= self._total_size and self._completed_objects >= self._object_count

    def print(self, message: str) -> None:
        """Print a message to the terminal, then redisplay the progress bar.

        Parameters
        ----------
        message : str
        """
        if not self._bar_is_displayed:
            print(message)
            return

        _, columns = self._terminal_size.terminal_size
        print("\r" + message.ljust(columns))
        self.display()

    def display(self) -> None:
        """Display the progress bar."""
        if not self._show_progress_bar:
            return

        _, columns = self._terminal_size.terminal_size
        scale = self._scale(self._total_size)
        factor = 1024.0 ** (-scale)
        unit = self.UNITS[scale]
        elapsed_seconds = (self._clock.now() - self._start_time).total_seconds() or 1e-9

        head = "Progress: ["
        tail = " ".join(
            [
                "]",
                f"{self._current_size * 1024.0**-2.0 / elapsed_seconds:>9.3f} MiB/s",
                f"{self._current_size * factor:>9.3f}/{self._total_size * factor:.3f} {unit}",
                f"{self._completed_objects:>4d}/{self._object_count} files",
            ]
        )

        max_bar_columns = max(columns - len(head) - len(tail), 1)
        bar_columns = int((self._current_size / self._total_size) * max_bar_columns)
        progress_line = f"\r{head}{(bar_columns * '*').ljust(max_bar_columns)}{tail}"

        print(progress_line, end="", flush=True)
        self._bar_is_displayed = True

    def close(self) -> None:
        """Close the progress bar."""
        if self._bar_is_displayed:
            print()

    @staticmethod
    def _scale(size: int) -> int:
        fsize = float(size)
        max_power = 4
        for power in range(max_power):
            if fsize < 1024.0:
                return power
            fsize /= 1024
        return max_power
