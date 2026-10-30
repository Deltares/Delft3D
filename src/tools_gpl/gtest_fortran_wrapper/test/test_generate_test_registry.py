"""Parser and generated-registration contract tests; run through CTest."""

from pathlib import Path
import subprocess
import sys
import tempfile
import unittest

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "tools"))
import generate_test_registry as registry


class RegistryTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory(prefix="Fortran tests with spaces ")
        self.addCleanup(self.temporary.cleanup)
        self.folder = Path(self.temporary.name)

    def source(self, text, name="tests.f90"):
        path = self.folder / name
        path.write_bytes(text.encode("utf-8"))
        return path

    def test_defaults_continuations_comments_and_binding_case(self):
        source = self.source(
            "MODULE Sample\r\ncontains\r\n!$GTEST TEST\r\n! documentation\r\n"
            "RECURSIVE SUBROUTINE Mixed&\r\n! continuation comment\r\n"
            "&Case( &\r\n &) &\r\n BIND(C, NAME='binding!with_quote')\r\n"
        )
        with self.assertRaisesRegex(registry.RegistryError, "binding name"):
            registry.collect([source])
        source.write_text(source.read_text().replace("binding!with_quote", "ExactBinding"), encoding="utf-8")
        test = registry.collect([source])[0]
        self.assertEqual((test.module, test.suite, test.name, test.binding),
                         ("Sample", "Sample", "MixedCase", "ExactBinding"))

    def test_overrides_and_module_scope(self):
        source = self.source(
            "module implementation\n!$gtest suite=public_suite\ncontains\n"
            "!$gtest test name=test_rotation_Van_Thiel_Van_Rijn\n"
            "subroutine test_trab19() bind(c)\nend subroutine\n"
            "!$gtest test suite=other name=public_name\n"
            'subroutine internal() bind(c, name="ExactSymbol")\nend subroutine\n'
            "end module\nmodule second\ncontains\n!$gtest test\n"
            "subroutine another() bind(c)\nend subroutine\nend module\n"
        )
        tests = registry.collect([source])
        self.assertEqual([(t.suite, t.name, t.binding) for t in tests],
                         [("public_suite", "test_rotation_Van_Thiel_Van_Rijn", "test_trab19"),
                          ("other", "public_name", "ExactSymbol"), ("second", "another", "another")])

    def test_explicit_markers_only(self):
        source = self.source("module m\ncontains\nsubroutine test_unmarked() bind(c)\nend subroutine\nend module")
        self.assertEqual(registry.collect([source]), [])

    def test_continuation_whitespace_preserves_token_boundaries(self):
        source = self.source(
            "module&\n& example\ncontains\n!$gtest test\nsubroutine&\n"
            " test_one()bind&\n& (c, name=&\n 'explicit_binding')\nend subroutine\nend module"
        )
        test = registry.collect([source])[0]
        self.assertEqual((test.module, test.procedure, test.binding),
                         ("example", "test_one", "explicit_binding"))

    def test_standalone_test_and_attribute(self):
        source = self.source(
            "!$gtest test suite=standalone\npublic :: test_one\n"
            "subroutine test_one() bind(c)\nend subroutine"
        )
        self.assertEqual(registry.collect([source])[0].suite, "standalone")

    def test_reject_invalid_declarations_and_markers(self):
        cases = [
            ("!$gtest test typo=value", "invalid"),
            ("!$gtest test name=a name=b", "duplicate"),
            ("!$gtest test name=not-valid", "invalid"),
            ("!$gtest unknown", "unknown"),
            ("!$gtest", "empty"),
            ("!$gtest test", "orphaned"),
            ("!$gtest test\n!$gtest test", "orphaned"),
            ("!$gtest test\nsubroutine a()", "bind"),
            ("!$gtest test\nsubroutine a(x) bind(c)", "no-argument"),
            ("!$gtest test\nsubroutine a() &", "unterminated"),
            ("module outer\nmodule inner", "nested"),
            ("!$gtest suite=outside", "specification"),
            ("module m\ncontains\n!$gtest suite=late", "specification"),
            ("!$gtest test\nsubroutine a() bind(c)", "requires suite"),
            ("!$gtest test suite=s\nsubroutine a() bind(c, name='class')", "C\\+\\+ keyword"),
            ("!$gtest test suite=s\nsubroutine main() bind(c)", "main"),
        ]
        for text, error in cases:
            with self.subTest(text=text):
                with self.assertRaisesRegex(registry.RegistryError, rf"tests.f90:\d+:.*{error}"):
                    registry.collect([self.source(text)])

    def test_duplicate_names_and_c_symbols_across_files(self):
        first = self.source("!$gtest test suite=s name=t\nsubroutine a() bind(c)", "first.f90")
        second = self.source("!$gtest test suite=s name=t\nsubroutine b() bind(c)", "second.f90")
        with self.assertRaisesRegex(registry.RegistryError, "duplicate GoogleTest name"):
            registry.collect([first, second])
        second.write_text("!$gtest test suite=other\nsubroutine A() bind(c)", encoding="utf-8")
        with self.assertRaisesRegex(registry.RegistryError, "duplicate C binding"):
            registry.collect([first, second])

    def test_calls_are_not_hidden_by_google_test_members(self):
        source = self.source("!$gtest test suite=s name=t\nsubroutine a() bind(c, name='SetUp')")
        text = registry.render(registry.collect([source]), teardown="OnTestEnd")
        self.assertIn("TEST(s, t) { ::SetUp(); }", text)
        self.assertIn("        ::OnTestEnd();", text)

    def test_output_is_deterministic_with_paths_containing_spaces(self):
        source = self.source("module m\ncontains\n!$gtest test\nsubroutine a() bind(c)\nend subroutine\nend module")
        output = self.folder / "generated registry.cpp"
        command = [sys.executable, registry.__file__, "--output", str(output), "--teardown", "cleanup", str(source)]
        subprocess.run(command, check=True, capture_output=True)
        first = output.read_bytes()
        subprocess.run(command, check=True, capture_output=True)
        self.assertEqual(first, output.read_bytes())
        text = first.decode("utf-8")
        self.assertEqual(text.count("TEST(m, a)"), 1)
        self.assertIn('extern "C" void a();', text)
        self.assertIn("void OnTestEnd(", text)
        self.assertIn("cleanup();", text)
        self.assertNotIn("gtest_main", text)


if __name__ == "__main__":
    unittest.main()
