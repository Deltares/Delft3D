import unittest
import sys
import os
import tempfile

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

from dflowfm_io import MduDocument, MduModel, MduReport

MDU_PATH = os.path.join(os.path.dirname(__file__), "tide-2.mdu")

class TestMduDocument(unittest.TestCase):
    def test_create_and_destroy(self):
        doc = MduDocument()
        self.assertIsNotNone(doc._ref)
        del doc

    def test_get_model(self):
        doc = MduDocument()
        self.assertIsInstance(doc.model, MduModel)

    def test_get_report(self):
        doc = MduDocument()
        self.assertIsInstance(doc.report, MduReport)

    def test_load_from_file(self):
        doc = MduDocument()
        doc.load_from_file(MDU_PATH)

    def test_load_from_lines(self):
        with open(MDU_PATH, "r") as f:
            lines = f.read().splitlines()
        doc = MduDocument()
        doc.load_from_lines(lines)
        self.assertEqual(doc.model.get_string("general.program"), "D-Flow FM")

    def test_load_from_file_nonexistent_raises(self):
        doc = MduDocument()
        with self.assertRaises(RuntimeError):
            doc.load_from_file("nonexistent_file.mdu")

    def test_save_to_lines(self):
        doc = MduDocument()
        doc.load_from_file(MDU_PATH)
        actual_lines = [line.strip() for line in doc.save_to_lines()]
        expected_present = [
            "[general]",
            "program                                    = D-Flow FM            # Program.",
            "[geometry]",
            "netFile                                    = FlowFM_net.nc        # Net file (*_net.nc) containing mesh information.",
            "cdBreakPoints                              = 0.00063 0.00723      # Wind drag breakpoints.",
            "[time]",
            "refDate                                    = 20010101             # Reference date. By default midnight is taken (00h00m00s).",
        ]
        for line in expected_present:
            self.assertIn(line, actual_lines)

    def test_save_to_file(self):
        doc = MduDocument()
        doc.load_from_file(MDU_PATH)
        doc.model.set_string("general.program", "Modified")
        with tempfile.NamedTemporaryFile(suffix=".mdu", delete=False) as f:
            tmp_path = f.name
        try:
            doc.save_to_file(tmp_path)
            doc2 = MduDocument()
            doc2.load_from_file(tmp_path)
            self.assertEqual(doc2.model.get_string("general.program"), "Modified")
        finally:
            os.unlink(tmp_path)

    def test_load_change_save_roundtrip(self):
        doc = MduDocument()
        doc.load_from_file(MDU_PATH)
        doc.model.set_int("geometry.kmx", 10)
        doc.model.set_double("numerics.cflmax", 0.5)
        doc.model.set_bool("geometry.usecaching", False)
        with tempfile.NamedTemporaryFile(suffix=".mdu", delete=False) as f:
            tmp_path = f.name
        try:
            doc.save_to_file(tmp_path)
            doc2 = MduDocument()
            doc2.load_from_file(tmp_path)
            self.assertEqual(doc2.model.get_int("geometry.kmx"), 10)
            self.assertAlmostEqual(doc2.model.get_double("numerics.cflmax"), 0.5)
            self.assertFalse(doc2.model.get_bool("geometry.usecaching"))
        finally:
            os.unlink(tmp_path)

    def test_multiple_instances(self):
        doc1 = MduDocument()
        doc1.load_from_file(MDU_PATH)
        doc2 = MduDocument()
        doc2.load_from_file(MDU_PATH)
        self.assertEqual(doc1.model.get_string("general.program"), "D-Flow FM")
        self.assertEqual(doc2.model.get_string("general.program"), "D-Flow FM")
        del doc1
        del doc2

    def test_no_errors_after_valid_load(self):
        doc = MduDocument()
        doc.load_from_file(MDU_PATH)
        self.assertFalse(doc.report.has_errors())


if __name__ == "__main__":
    unittest.main()