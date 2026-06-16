import unittest
import sys
import os
import io
from contextlib import redirect_stdout

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

from dflowfm_io import MduModel, MduReport, Issue, Severity

# TODO add a proper reference MDU file. This is just one I had on my system.
MDU_PATH = os.path.join(os.path.dirname(__file__), "tide-2.mdu")

class TestMduReport(unittest.TestCase):
    def test_load_returns_report(self):
        model = MduModel()
        report = model.load_from_file(MDU_PATH)
        self.assertIsInstance(report, MduReport)

    def test_get_issues(self):
        model = MduModel()
        report = model.load_from_file(MDU_PATH)
        issues = report.get_issues()
        self.assertIsInstance(issues, list)
        for issue in issues:
            self.assertIsInstance(issue, Issue)

    def test_issue_attribute_types(self):
        model = MduModel()
        report = model.load_from_file(MDU_PATH)
        for issue in report.get_issues():
            self.assertIsInstance(issue.line_number, int)
            self.assertIsInstance(issue.severity, Severity)
            self.assertIsInstance(issue.message, str)

    def test_valid_file_has_no_errors(self):
        model = MduModel()
        report = model.load_from_file(MDU_PATH)
        self.assertFalse(report.has_errors())
        self.assertEqual([i for i in report.get_issues() if i.severity == Severity.ERROR], [])

    def test_severity_enum_values(self):
        self.assertEqual(Severity.INFO, 0)
        self.assertEqual(Severity.WARNING, 1)
        self.assertEqual(Severity.ERROR, 2)

    def test_get_issues_is_stable_across_calls(self):
        model = MduModel()
        report = model.load_from_file(MDU_PATH)
        first = report.get_issues()
        second = report.get_issues()
        self.assertEqual(len(first), len(second))
        for a, b in zip(first, second):
            self.assertEqual((a.line_number, a.severity, a.message),
                             (b.line_number, b.severity, b.message))

    def test_print_overview_format(self):
        model = MduModel()
        report = model.load_from_file(MDU_PATH)
        buffer = io.StringIO()
        with redirect_stdout(buffer):
            report.print_overview()
        output_lines = buffer.getvalue().splitlines()
        self.assertEqual(len(output_lines), len(report.get_issues()))
        for line in output_lines:
            self.assertRegex(line, r"^\[(INFO|WARNING|ERROR)\] \((line \d+|no line)\) ")

if __name__ == "__main__":
    unittest.main()
