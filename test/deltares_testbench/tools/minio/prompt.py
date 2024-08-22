import abc
import sys
from typing import Optional, Sequence, TextIO


class Prompt(abc.ABC):
    """Interface used for prompting a user, or to make automated choices."""

    @abc.abstractmethod
    def choose(self, message: str, options: Sequence[str], default_idx: int = 0) -> Optional[str]:
        """Prompt user to choose one of the available options.

        Parameters
        ----------
        message : str
            A message to show to the users before prompting them to make a choice.
        options : Sequence[str]
            The list of available options.
        default_idx : int, optional
            The index in the `options` sequence to use as the default option.

        Returns
        -------
        Optional[str]
            The chosen option, or `None` if no choice was made (in case of an EOF).
        """

    @abc.abstractmethod
    def yes_no(self, message: str, default_yes: bool = True) -> Optional[bool]:
        """Prompt user to enter 'yes' or 'no'.

        Parameters
        ----------
        message : str
            A message to show to the users before prompting them to make a choice.
        default_yes : bool, optional
            Whether or not 'yes' is the default option.

        Returns
        -------
        Optional[bool]
            The chosen option. `True` in case of 'yes' and `False` in case of 'no.
            If no choice was made, returns `None` (in case of an EOF).
        """


class DefaultPrompt(Prompt):
    """Used to prompt non-interactively. In batch mode."""

    def __init__(self, force_yes: bool = False) -> None:
        self._force_yes = force_yes

    def choose(self, message: str, options: Sequence[str], default_idx: int = 0) -> Optional[str]:
        return options[default_idx]

    def yes_no(self, message: str, default_yes: bool = True) -> Optional[bool]:
        return self._force_yes or default_yes


class InteractivePrompt(Prompt):
    """Used to prompt user interactively through an input and output stream."""

    def __init__(self, in_stream: Optional[TextIO] = None, out_stream: Optional[TextIO] = None) -> None:
        self._in = in_stream or sys.stdin
        self._out = out_stream or sys.stdout

    def choose(self, message: str, options: Sequence[str], default_idx: int = 0) -> Optional[str]:
        if not options:
            raise ValueError("Options must be non-empty")
        if default_idx not in range(-len(options), len(options)):
            raise ValueError("Default index out of range")

        while True:
            self._out.write(f"{message} {'/'.join(options)} (Default: {options[default_idx]})\n> ")
            self._out.flush()

            line = self._in.readline()
            if not line.endswith("\n"):  # EOF
                return None

            # User hit enter.
            option = line.strip()
            if not option:
                return options[default_idx]
            if option in options:
                return option
            self._out.write(f"Invalid option: {option}\n")

    def yes_no(self, message: str, default_yes: bool = True) -> Optional[bool]:
        option = self.choose(message, ("yes", "no"), default_idx=(0 if default_yes else 1))
        return None if option is None else option == "yes"
