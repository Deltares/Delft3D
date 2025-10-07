import logging
import subprocess
from pathlib import Path

from ci_tools.teamcity.log import enter_test_context


class Launcher:
    """Launches commands in the shell using `subprocess.call`."""

    def shell(self, command_line: str, working_dir: Path, stdout_file: Path, stderr_file: Path) -> int:
        """Run a command line in the shell.

        Parameters
        ----------
        command_line : str
            The command with arguments to run in the.
        working_dir : Path
            Run the command in this directory.
        stdout_file, stderr_file : Path
            The files to redirect the standard output and error to.
        """
        with stdout_file.open("wb") as stdout, stderr_file.open("wb") as stderr:
            return subprocess.call(command_line, cwd=working_dir, stdout=stdout, stderr=stderr, shell=True)


class ShellError(Exception):
    """Raised by `DocumentationBuilder` when a command exits with a non-zero return code."""

    def __init__(self, message: str, command_line: str, return_code: int) -> None:
        super().__init__(message)
        self.message = message
        self.command_line = command_line
        self.return_code = return_code


class DocumentationBuilder:
    """Builds latex files by invoking `pdflatex`, `bibtex` and `makeindex`."""

    def __init__(self, launcher: Launcher | None = None, logger: logging.Logger | None = None) -> None:
        self._launcher = launcher or Launcher()
        self._logger = logger or logging.getLogger(__name__)

    def build(self, tex_file: Path) -> None:
        """
        Generate a pdf file from the given Latex source file.

        Invokes `pdflatex` (multiple times), `bibtex` and optionally `makeindex`. If any of these commands
        fail, log the error and abort. The directory containing `tex_file` is the working directory
        for all shell invocations, and the standard error and output streams are written to files
        inside this working directory.

        Parameters
        ----------
        tex_file : Path
            Path to the latex file.
        """
        test_id = tex_file.parents[1].as_posix().replace("/", ".")
        with enter_test_context(test_id=test_id, logger=self._logger):
            self._pdflatex(tex_file)
            self._bibtex(tex_file.with_suffix(".aux"))
            self._pdflatex(tex_file)

            index_file = tex_file.with_suffix(".idx")
            if index_file.is_file():
                self._makeindex(tex_file.with_suffix(".idx"))
                self._pdflatex(tex_file)

            self._pdflatex(tex_file)

    def _pdflatex(self, path: Path) -> None:
        command_line = f"pdflatex -synctex=1 -interaction=nonstopmode -shell-escape {path.name}"
        self._run_in_working_dir(command_line, working_dir=path.parent)

    def _bibtex(self, path: Path) -> None:
        self._run_in_working_dir(f"bibtex {path.name}", working_dir=path.parent)

    def _makeindex(self, path: Path) -> None:
        self._run_in_working_dir(f"makeindex {path.name}", working_dir=path.parent)

    def _run_in_working_dir(self, command_line: str, working_dir: Path) -> None:
        self._logger.info(command_line)
        command_name = command_line.split(maxsplit=1)[0]
        stdout_file = self._unique_path(working_dir / f"{command_name}-stdout.log")
        stderr_file = self._unique_path(working_dir / f"{command_name}-stderr.log")
        return_code = self._launcher.shell(
            command_line, working_dir=working_dir, stdout_file=stdout_file, stderr_file=stderr_file
        )
        if return_code != 0:
            raise ShellError(
                f"Shell command exited with return code {return_code}. Please check {stdout_file} and {stderr_file}.",
                command_line,
                return_code,
            )

    @staticmethod
    def _unique_path(path: Path) -> Path:
        result = path
        count = 1
        while result.exists():
            result = path.with_stem(f"{path.stem}-{count}")
            count += 1
        return result
