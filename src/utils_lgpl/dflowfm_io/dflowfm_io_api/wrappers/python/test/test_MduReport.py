import unittest
import sys
import os
import io
from contextlib import redirect_stdout

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

from dflowfm_io import MduDocument, MduReport, Issue, Severity

MDU_PATH = os.path.join(os.path.dirname(__file__), "tide-2.mdu")

def _loaded_doc(path=MDU_PATH):
    doc = MduDocument()
    doc.load_from_file(path)
    return doc

class TestMduReport(unittest.TestCase):
    def test_get_issues(self):
        doc = _loaded_doc()
        issues = doc.report.get_issues()
        self.assertIsInstance(issues, list)
        for issue in issues:
            self.assertIsInstance(issue, Issue)

    def test_issue_attribute_types(self):
        doc = _loaded_doc()
        for issue in doc.report.get_issues():
            self.assertIsInstance(issue.line_number, int)
            self.assertIsInstance(issue.severity, Severity)
            self.assertIsInstance(issue.message, str)

    def test_valid_file_has_no_errors(self):
        doc = _loaded_doc()
        self.assertFalse(doc.report.has_errors())
        self.assertEqual([i for i in doc.report.get_issues() if i.severity == Severity.ERROR], [])

    def test_severity_enum_values(self):
        self.assertEqual(Severity.INFO, 0)
        self.assertEqual(Severity.WARNING, 1)
        self.assertEqual(Severity.ERROR, 2)

    def test_get_issues_is_stable_across_calls(self):
        doc = _loaded_doc()
        first = doc.report.get_issues()
        second = doc.report.get_issues()
        self.assertEqual(len(first), len(second))
        for a, b in zip(first, second):
            self.assertEqual((a.line_number, a.severity, a.message),
                             (b.line_number, b.severity, b.message))

    def test_print_overview_format(self):
        doc = _loaded_doc()
        buffer = io.StringIO()
        with redirect_stdout(buffer):
            doc.report.print_overview()
        output_lines = buffer.getvalue().splitlines()
        self.assertEqual(len(output_lines), len(doc.report.get_issues()))
        for line in output_lines:
            self.assertRegex(line, r"^\[(INFO|WARNING|ERROR)\] \((line \d+|no line)\) ")


if __name__ == "__main__":
    unittest.main()