import logging
from pathlib import Path

from pyfakefs.fake_filesystem import FakeFilesystem
from pytest_mock import MockerFixture

from ci_tools.documentation.documentation_builder import DocumentationBuilder, Launcher


class TestDocumentationBuilder:
    def test_build__run_pdflatex_and_bibtex(self, mocker: MockerFixture, fs: FakeFilesystem) -> None:
        # Arrange
        launcher = mocker.Mock(spec=Launcher)
        launcher.shell.return_value = 0
        doc_builder = DocumentationBuilder(launcher=launcher)
        path = Path("path/to/documentation.tex")

        # Act
        doc_builder.build(path)

        # Assert
        calls = launcher.shell.call_args_list
        pdflatex_calls = [calls[0], calls[2], calls[3]]
        bibtex_call = calls[1]
        assert all(
            call.args[0] == "pdflatex -synctex=1 -interaction=nonstopmode -shell-escape documentation.tex"
            for call in pdflatex_calls
        )
        assert bibtex_call.args[0] == "bibtex documentation.aux"

    def test_build__run_pdflatex_bibtex_and_makeindex(self, mocker: MockerFixture, fs: FakeFilesystem) -> None:
        # Arrange
        launcher = mocker.Mock(spec=Launcher)
        launcher.shell.return_value = 0
        doc_builder = DocumentationBuilder(launcher=launcher)
        path = Path("path/to/documentation.tex")
        fs.create_file(path.with_suffix(".idx"))

        # Act
        doc_builder.build(path)

        # Assert
        calls = launcher.shell.call_args_list
        pdflatex_calls = [calls[0], calls[2], calls[4], calls[5]]
        bibtex_call = calls[1]
        makeindex_call = calls[3]

        assert all(
            call.args[0] == "pdflatex -synctex=1 -interaction=nonstopmode -shell-escape documentation.tex"
            for call in pdflatex_calls
        )
        assert bibtex_call.args[0] == "bibtex documentation.aux"
        assert makeindex_call.args[0] == "makeindex documentation.idx"

    def test_build__command_fails__catch_error(self, mocker: MockerFixture) -> None:
        # Arrange
        launcher = mocker.Mock(spec=Launcher)
        logger = mocker.Mock(spec=logging.Logger)
        launcher.shell.side_effect = [0, 1]  # First command succeeds, the next one fails.
        doc_builder = DocumentationBuilder(launcher=launcher, logger=logger)

        # Act
        doc_builder.build(Path("path/to/my_doc.tex"))

        # Assert
        assert len(launcher.shell.call_args_list) == 2
        logger.exception.assert_called_once()

    def test_build__log_file_exists__generate_unique_name(self, mocker: MockerFixture, fs: FakeFilesystem) -> None:
        # Arrange
        launcher = mocker.Mock(spec=Launcher)
        launcher.shell.return_value = 1
        doc_builder = DocumentationBuilder(launcher=launcher)
        path = Path("path/to/my_doc.tex")
        fs.create_file(path.parent / "pdflatex-stdout.log")
        fs.create_file(path.parent / "pdflatex-stdout-1.log")

        # Act
        doc_builder.build(path)

        # Assert
        assert launcher.shell.call_args.kwargs["stdout_file"] == path.parent / "pdflatex-stdout-2.log"
