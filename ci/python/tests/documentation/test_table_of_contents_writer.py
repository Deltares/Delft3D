from pathlib import Path

from pyfakefs.fake_filesystem import FakeFilesystem

from ci_tools.documentation.table_of_contents_writer import TableOfContentsWriter


class TestTableOfContentsWriter:
    def test_from_engine_directory__skip_functionality_without_digits(self, fs: FakeFilesystem) -> None:
        # Arrange
        engine_dir = Path("e42_delft4d5")
        functionalities = ["f00_warpdrive", "f01_replicator", "f02_holodeck", "fxx_phaser"]
        for func in functionalities:
            fs.create_file(engine_dir / func / "doc/chapters/testcases.tex")
            fs.create_file(engine_dir / func / "c00_test_case/doc/chapters/case_text.tex")

        # Act
        toc_writer = TableOfContentsWriter.from_engine_directory(engine_dir)

        # Assert
        assert sorted(toc_writer._index.keys()) == functionalities[:-1]
        for func in functionalities[:-1]:
            assert len(toc_writer._index[func]) == 1
            assert toc_writer._index[func][0] == "c00_test_case"

    def test_from_engine_directory__skip_functionality_without_cases(self, fs: FakeFilesystem) -> None:
        # Arrange
        engine_dir = Path("e42_delft4d5")
        fs.create_file(engine_dir / "f00_warpdrive" / "doc/chapters/testcases.tex")
        fs.create_file(engine_dir / "f00_warpdrive" / "c00_test_case/doc/chapters/classified.tex")  # Invalid case
        fs.create_file(engine_dir / "f01_replicator" / "doc/chapters/testcases.tex")
        fs.create_file(engine_dir / "f01_replicator" / "c00_test_case/doc/chapters/case_text.tex")
        fs.create_file(engine_dir / "f02_holodeck" / "doc/chapters/testcases.tex")  # No test cases

        # Act
        toc_writer = TableOfContentsWriter.from_engine_directory(engine_dir)

        # Assert
        assert len(toc_writer._index) == 1
        assert toc_writer._index.get("f01_replicator") == ["c00_test_case"]

    def test_write_table_of_contents(self, fs: FakeFilesystem) -> None:
        # Arrange
        engine_dir = Path("e42_delft4d5")
        functionalities = ["f00_warpdrive", "f01_replicator", "f02_holodeck"]
        for func in functionalities:
            fs.create_file(engine_dir / func / "doc/chapters/testcases.tex")
            fs.create_file(engine_dir / func / "c00_test_case_1/doc/chapters/case_text.tex")
            fs.create_file(engine_dir / func / "c01_test_case_2/doc/chapters/case_text.tex")

        # Act
        toc_writer = TableOfContentsWriter.from_engine_directory(engine_dir)
        toc_writer.write_table_of_contents()

        # Assert
        overall_toc = engine_dir / "doc/functionalities/chapters/testcases.tex"
        warpdrive_toc = engine_dir / "f00_warpdrive/doc/chapters/testcases.tex"
        replicator_toc = engine_dir / "f01_replicator/doc/chapters/testcases.tex"
        holodeck_toc = engine_dir / "f02_holodeck/doc/chapters/testcases.tex"

        assert all(toc.exists() for toc in (overall_toc, warpdrive_toc, replicator_toc, holodeck_toc))
