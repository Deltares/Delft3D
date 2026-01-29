import abc
import readline
import sys
from enum import StrEnum
from typing import Callable, Generic, Iterator, Protocol, Sequence, TextIO, TypeVar

from typing_extensions import overload, override


class Answer(StrEnum):
    """Enum for answers to yes/no questions."""

    YES = "yes"
    NO = "no"
    CANCEL = "cancel"


class Stringable(Protocol):
    """Protocol for types that can be converted to a string."""

    def __str__(self) -> str:
        """Return the string representation of the object."""


T = TypeVar("T")
T_Stringable = TypeVar("T_Stringable", bound=Stringable)


class InputParser(Generic[T], metaclass=abc.ABCMeta):
    """Abstract parser for a line of input entered by a user."""

    @abc.abstractmethod
    def parse(self, input_: str) -> T:
        """Parse a line of input."""

    def complete(self, hint: str) -> Iterator[str]:
        """Iterate over completions with prefix `hint`."""
        return iter([])


class Prompt(abc.ABC):
    """Interface used for prompting a user, or to make automated choices."""

    @abc.abstractmethod
    def choose(
        self,
        message: str,
        options: Sequence[T_Stringable],
        default_idx: int = 0,
        formatter: Callable[[T_Stringable], str] = str,
    ) -> T_Stringable | None:
        """Prompt user to choose one of the available options.

        Parameters
        ----------
        message : str
            A message to show to the users before prompting them to make a choice.
        options : Sequence[T_Stringable]
            The list of available options. The options have to be shown on the
            output, so the options must implement `__str__`.
        default_idx : int, optional
            The index in the `options` sequence to use as the default option.
        formatter : Callable[[T_Stringable], str], optional
            Optional formatter function to display the options.

        Returns
        -------
        T_Stringable | None
            The chosen option, or `None` if no choice was made (in case of an EOF).
        """

    @abc.abstractmethod
    def yes_no(self, message: str, default: Answer = Answer.YES) -> Answer:
        """Prompt user to enter 'yes' or 'no'.

        Parameters
        ----------
        message : str
            A message to show to the users before prompting them to make a choice.
        default : Answer, optional
            The default `Answer`, returned when the user enters an empty line.

        Returns
        -------
        Answer
        """

    @overload
    def input(self, message: str) -> str | None: ...

    @overload
    def input(self, message: str, parser: InputParser[T]) -> T: ...

    @abc.abstractmethod
    def input(self, message: str, parser=None):
        """Prompt user to enter a value.

        Parameters
        ----------
        message : str
            A message to show to the users before prompting them to enter a value.
        parser : InputParser[T], optional
            A parser used to parse the input lines. If omitted, use the default
            parser, which doesn't do any parsing, but simply returns the input line
            but without any leading or trailing whitespace.

        Returns
        -------
        T
            The result of parsing the input line.
        """


class DefaultPrompt(Prompt):
    """Used to prompt non-interactively. In batch mode."""

    def __init__(self, force_yes: bool = False) -> None:
        self._force_yes = force_yes

    @override
    def choose(
        self,
        message: str,
        options: Sequence[T_Stringable],
        default_idx: int = 0,
        formatter: Callable[[T_Stringable], str] = str,
    ) -> T_Stringable | None:
        return options[default_idx]

    @override
    def yes_no(self, message: str, default: Answer = Answer.YES) -> Answer:
        return Answer.YES if self._force_yes else default

    @overload
    def input(self, message: str) -> str | None: ...

    @overload
    def input(self, message: str, parser: InputParser[T]) -> T: ...

    @override
    def input(self, message: str, parser=None):
        if parser is None:
            parser = DefaultParser()
        return parser.parse("\n")


class InteractivePrompt(Prompt):
    """Used to prompt user interactively through an input and output stream."""

    def __init__(self, in_stream: TextIO | None = None, out_stream: TextIO | None = None) -> None:
        self._in: TextIO = in_stream or sys.stdin
        self._out: TextIO = out_stream or sys.stdout

    @override
    def choose(
        self,
        message: str,
        options: Sequence[T_Stringable],
        default_idx: int = 0,
        formatter: Callable[[T_Stringable], str] = str,
    ) -> T_Stringable | None:
        if not options:
            raise ValueError("Options must be non-empty")
        if not (0 <= default_idx < len(options)):
            raise ValueError("Default index out of range", default_idx)

        while True:
            self._out.write(f"{message}:\n")
            for i, option in enumerate(map(formatter, options), 1):
                suffix = " (default)" if default_idx == (i - 1) else ""
                self._out.write(f"{i:2d}) {option}{suffix}\n")
            self._out.write("> ")
            self._out.flush()

            line = self._in.readline()
            if not line.endswith("\n"):  # EOF
                return None

            # User hit enter.
            stripped_line = line.strip()
            if not stripped_line:
                return options[default_idx]

            if not stripped_line.isdigit():
                self._out.write("Please enter a number.\n")
                continue
            option_idx = int(stripped_line) - 1
            if not (0 <= option_idx < len(options)):
                self._out.write(f"Please enter a number between 1 and {len(options)}.\n")
                continue

            return options[option_idx]

    @override
    def yes_no(self, message: str, default: Answer = Answer.YES) -> Answer:
        while True:
            self._out.write(f"{message} (yes/no, default {default.value})\n> ")
            self._out.flush()

            line = self._in.readline()
            if not line.endswith("\n"):  # EOF
                return Answer.CANCEL

            stripped_line = line.strip().lower()
            if not stripped_line:
                return default
            if stripped_line == "yes":
                return Answer.YES
            if stripped_line == "no":
                return Answer.NO

            self._out.write("Please enter 'yes' or 'no'.\n")

    @overload
    def input(self, message: str) -> str | None: ...

    @overload
    def input(self, message: str, parser: InputParser[T]) -> T: ...

    @override
    def input(self, message: str, parser=None):
        if parser is None:
            parser = DefaultParser()

        if any(parser.complete("")):
            self.__init_readline_tab_completion(parser)
        while True:
            self._out.write(f"{message}\n")

            line = self.__read_line()
            try:
                return parser.parse(line)
            except ValueError as exc:
                self._out.write(f"Invalid input: {line.strip()}.\nMessage: {exc.args[0]}.\nPlease try again.\n")
                continue

    def __read_line(self) -> str:
        """Read a line from the input stream.

        Unfortunately, autocomplete with `readline` module only works
        with the builtin `input` function, and not with the `readline` method
        on the `TextIO` interface. So in the special case where we're reading
        from `sys.stdin`, use `input`, and use `TextIO.readline` otherwise.
        """
        if self._in == sys.stdin:
            try:
                return input("> ") + "\n"
            except EOFError:
                return ""  # Catch the Ctrl-D/Z and return an empty string.
        else:
            self._out.write("> ")
            self._out.flush()
            return self._in.readline()

    @classmethod
    def __init_readline_tab_completion(cls, parser: InputParser[T]) -> None:
        """Enable tab completion.

        `readline` may be implemented with the GNU readline library, or
        with libedit depending on the platform and how python was compiled.
        Unfortunately, the `parse_and_bind` command to enable tab completion
        depends on which `readline` implementation is used.
        """
        parse_and_bind_cmd = "tab: complete"
        if "libedit" in (readline.__doc__ or "").lower():
            parse_and_bind_cmd = "bind ^I rl_complete"
        readline.parse_and_bind(parse_and_bind_cmd)
        readline.set_completer_delims(" \t\n")
        readline.set_completer(cls.__completer_factory(parser))

    @staticmethod
    def __completer_factory(parser: InputParser[T]) -> Callable[[str, int], str | None]:
        """Build a completer function compatible with `readline` from an `InputHelper`.

        The returned completer function can be fed to `readline.set_completer` to perform
        autocompletions.
        """
        _iter: Iterator[str] = iter([])

        def completer(hint: str, state: int) -> str | None:
            nonlocal _iter
            if state == 0:
                _iter = parser.complete(hint)
            return next(_iter, None)

        return completer


class DefaultParser(InputParser[str | None]):
    """Default input line parser.

    Simply strips the whitespace from the input line and returns the result.
    """

    @override
    def parse(self, input_: str) -> str | None:
        return input_.strip() if input_.endswith("\n") else None
