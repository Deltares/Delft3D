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

    def test_comments_and_binding_case(self):
        source = self.source(
            "\ufeffMODULE Sample\r\ncontains\r\n!$GTEST ! marker comment\r\n\r\n! documentation\r\n"
            "SUBROUTINE MixedCase() BIND(C) ! declaration comment\r\n"
        )
        test = registry.collect([source])[0]
        self.assertEqual((test.module, test.procedure), ("Sample", "MixedCase"))
        self.assertEqual(test.line, 3)
        text = registry.render([test])
        self.assertIn('extern "C" void mixedcase();', text)
        self.assertIn("TEST(Sample, MixedCase) { ::mixedcase(); }", text)

    def test_module_suites(self):
        source = self.source(
            "module implementation\ncontains\n!$gtest\n"
            "subroutine internal() bind(c)\nend subroutine\n"
            "end module\nmodule second\ncontains\n!$gtest\n"
            "subroutine another() bind(c)\nend subroutine\nend module\n"
        )
        tests = registry.collect([source])
        self.assertEqual([(t.module, t.procedure) for t in tests],
                         [("implementation", "internal"), ("second", "another")])
        text = registry.render(tests)
        self.assertIn("TEST(implementation, internal) { ::internal(); }", text)
        self.assertIn("TEST(second, another) { ::another(); }", text)

    def test_explicit_markers_only(self):
        source = self.source(
            "module example\ncontains\nsubroutine test_unmarked() bind(c)\nend subroutine\n"
            "subroutine helper(&\n"
            " value)\ninteger :: value\nvalue = &\n 1\nend subroutine\n"
            "!$gtest\nsubroutine test_one() bind(c)\n"
            "end subroutine\nend module"
        )
        tests = registry.collect([source])
        self.assertEqual([(test.module, test.procedure) for test in tests], [("example", "test_one")])
        self.assertEqual(registry.collect([self.source(
            "module m\ncontains\nsubroutine test_unmarked() bind(c)\nend subroutine\nend module"
        )]), [])

    def test_reject_tests_outside_modules(self):
        for prefix in ("", "module m\ncontains\nend module\n"):
            with self.subTest(prefix=prefix):
                source = self.source(f"{prefix}!$gtest\nsubroutine test_one() bind(c)\nend subroutine")
                with self.assertRaisesRegex(registry.RegistryError, "inside a module"):
                    registry.collect([source])

    def test_reject_invalid_declarations_and_markers(self):
        cases = [
            ("!$gtest unknown", "invalid"),
            ("!$gtest", "orphaned"),
            ("!$gtest\nsubroutine a(x) bind(c)", "no-argument"),
            ("!$gtest\nsubroutine a() bind(c, name='a')", "bind"),
            ("module outer\nmodule inner", "nested"),
        ]
        for text, error in cases:
            with self.subTest(text=text):
                with self.assertRaisesRegex(registry.RegistryError, rf"tests.f90:\d+:.*{error}"):
                    registry.collect([self.source(text)])

    def test_error_locations_with_intervening_comments(self):
        cases = [
            ("\n!$gtest\n\n! documentation\n", 2, "orphaned"),
            ("\n!$gtest\n\n! documentation\n!$gtest", 2, "orphaned"),
            ("\n!$gtest\n\n! documentation\nsubroutine a()", 5, "bind"),
            ("\n!$gtest\n\n! documentation\nsubroutine a() bind(c)", 2, "inside a module"),
        ]
        for text, line, error in cases:
            with self.subTest(text=text):
                with self.assertRaisesRegex(registry.RegistryError, rf"tests.f90:{line}:.*{error}"):
                    registry.collect([self.source(text)])

    def test_reject_prefixed_test_declarations(self):
        for prefix in ("recursive", "non_recursive", "pure", "impure", "elemental", "module"):
            with self.subTest(prefix=prefix):
                source = self.source(f"!$gtest\n{prefix} subroutine a() bind(c)")
                with self.assertRaisesRegex(registry.RegistryError, r"tests.f90:2:.*plain"):
                    registry.collect([source])

    def test_reject_continued_test_declarations(self):
        declarations = [
            "subroutine&\n a() bind(c)",
            "subroutine a(&\n ) bind(c)",
            "subroutine a() bind(&\n c)",
            "subroutine a() bind(c) &\n",
        ]
        for declaration in declarations:
            with self.subTest(declaration=declaration):
                source = self.source(f"!$gtest\n{declaration}")
                with self.assertRaisesRegex(registry.RegistryError, r"tests.f90:2:.*single-line"):
                    registry.collect([source])

    def test_reject_intervening_declarations(self):
        for declaration in ("public :: a", "private :: a", "attributes(dllexport) :: a"):
            with self.subTest(declaration=declaration):
                source = self.source(f"!$gtest\n{declaration}\nsubroutine a() bind(c)")
                with self.assertRaisesRegex(registry.RegistryError, r"tests.f90:2:.*plain"):
                    registry.collect([source])

    def test_duplicate_c_symbols_across_files(self):
        first = self.source("module m\ncontains\n!$gtest\nsubroutine a() bind(c)", "first.f90")
        second = self.source("module other\ncontains\n!$gtest\nsubroutine A() bind(c)", "second.f90")
        with self.assertRaisesRegex(registry.RegistryError, "duplicate C binding"):
            registry.collect([first, second])

    def test_output_is_deterministic_with_paths_containing_spaces(self):
        source = self.source("module m\ncontains\n!$gtest\nsubroutine a() bind(c)\nend subroutine\nend module")
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
