import io
import os
import sys
import tempfile
import unittest
from contextlib import redirect_stdout
from pathlib import Path

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "..", "scripts"))

from codegen_support import GeneratedModule, Generator, SourceBuilder


class TestSourceBuilder(unittest.TestCase):
    """The line/blank/extend/render accumulator."""

    def test_empty_render_is_a_single_newline(self):
        self.assertEqual(SourceBuilder().render(), "\n")

    def test_line_and_render(self):
        self.assertEqual(SourceBuilder().line("a").line("b").render(), "a\nb\n")

    def test_extend_appends_all(self):
        self.assertEqual(SourceBuilder().extend(["a", "b"]).render(), "a\nb\n")

    def test_blank_inserts_blank_lines(self):
        self.assertEqual(SourceBuilder().line("a").blank(2).line("b").render(), "a\n\n\nb\n")

    def test_methods_are_chainable(self):
        builder = SourceBuilder()
        self.assertIs(builder.line("a"), builder)
        self.assertIs(builder.blank(), builder)
        self.assertIs(builder.extend([]), builder)


class TestGeneratedModule(unittest.TestCase):
    """The value object for a module to write."""

    def test_is_frozen(self):
        module = GeneratedModule(Path("x.py"), "src", "summary")
        with self.assertRaises(Exception):
            module.source = "other"


class TestGeneratorRun(unittest.TestCase):
    """The base Generator writes each built module and reports it."""

    def test_run_writes_files_and_prints_summary(self):
        with tempfile.TemporaryDirectory() as tmp:
            target = Path(tmp) / "out.py"

            class _Fake(Generator):
                def build(self):
                    return [GeneratedModule(target, "hello\n", "1 thing")]

            out = io.StringIO()
            with redirect_stdout(out):
                _Fake().run()

            self.assertEqual(target.read_text(encoding="utf-8"), "hello\n")
            self.assertIn("Wrote 1 thing", out.getvalue())

    def test_build_is_abstract(self):
        with self.assertRaises(TypeError):
            Generator()


if __name__ == "__main__":
    unittest.main()
