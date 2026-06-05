import textwrap
from pathlib import Path
from typing import Mapping, Sequence, TextIO


class TableOfContentsWriter:
    """Writes table of contents files based on the functionality documentation directory structure."""

    def __init__(self, engine_dir: Path, index: Mapping[str, Sequence[str]]) -> None:
        self._engine_dir = engine_dir
        self._index = index

    def write_table_of_contents(self) -> None:
        """Write table of contents files.

        Write one 'overall' table of contents file, containing the test cases of all
        functionality groups. And write a table of contents file for each individual
        functionality group.
        """
        overall_toc = self._engine_dir / "doc/functionalities/chapters/testcases.tex"
        overall_toc.parent.mkdir(parents=True, exist_ok=True)
        with overall_toc.open("w") as out:
            self._write_overall_table_of_contents(out)

        for func_group in self._index.keys():
            functionality_toc = self._engine_dir / func_group / "doc/chapters/testcases.tex"
            functionality_toc.parent.mkdir(parents=True, exist_ok=True)
            with functionality_toc.open("w") as out:
                self._write_functionality_table_of_contents(out, self._index[func_group])

    def _write_overall_table_of_contents(self, out: TextIO) -> None:
        out.write("%\n% Automatic generated file\n%\n")
        for functionality in sorted(self._index.keys()):
            lines = textwrap.dedent(f"""
                \\part{{\\newline {functionality.replace("_", " ")}}}
                \\adjustptc
                \\parttoc
                \\newpage\n
                """).lstrip()
            out.write(lines)

            for test_case in self._index[functionality]:
                lines = textwrap.dedent(
                    f"""
                    \\chapter{{{test_case.replace("_", " ")}}}
                    \\graphicspath{{{{../../{functionality}/{test_case}/doc/}}}}
                    \\import{{../../{functionality}/{test_case}/doc/}}{{chapters/case_text.tex}}\n
                    """
                ).lstrip()
                out.write(lines)

    def _write_functionality_table_of_contents(self, out: TextIO, test_cases: Sequence[str]) -> None:
        out.write("%\n% Automatic generated file\n%\n")
        for test_case in test_cases:
            lines = textwrap.dedent(
                f"""
                \\chapter{{{test_case.replace("_", " ")}}}
                \\graphicspath{{{{../{test_case}/doc/}}}}
                \\import{{../{test_case}/doc/}}{{chapters/case_text.tex}}\n
                """
            ).lstrip()
            out.write(lines)

    @staticmethod
    def from_engine_directory(engine_dir: Path) -> "TableOfContentsWriter":
        """Index the functionality documentation in `engine_dir` and make a `TableOfContentsWriter`.

        Only subdirectories of `engine_dir` starting with `f<two digits>` that contain the file
        `doc/chapters/testcases.tex` will be indexed. The functionality documentation only contains
        test cases in directories starting with `c<two digits>` that contain the file
        `doc/chapters/case_text.tex`

        Parameters
        ----------
        engine_dir : Path
            The directory to search for functionality documentation.

        Returns
        -------
        TableOfContentsWriter
        """
        # Only consider functionality docs for directories that contain the "testcases.tex" file.
        func_dirs = (func_dir.parents[2] for func_dir in engine_dir.glob("f[0-9][0-9]*/doc/chapters/testcases.tex"))

        index: dict[str, Sequence[str]] = {}
        for func_dir in func_dirs:
            # Only consider test cases that have a "case_text.tex" file.
            case_texts = sorted(
                case_text.parents[2].name for case_text in func_dir.glob("c[0-9][0-9]*/doc/chapters/case_text.tex")
            )
            if case_texts:
                index[func_dir.name] = case_texts

        return TableOfContentsWriter(engine_dir=engine_dir, index=index)
