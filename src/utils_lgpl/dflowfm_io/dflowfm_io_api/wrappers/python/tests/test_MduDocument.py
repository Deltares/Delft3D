import unittest
import sys
import os
import tempfile

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

from dflowfm_io import MduDocument, MduModel, MduReport

MDU_PATH = os.path.join(os.path.dirname(__file__), "tide-2.mdu")


def _parse_ini_lines(lines: list[str]) -> dict[str, dict[str, str]]:
    """Parse stripped INI-style lines into {section: {key: value}}, ignoring comments."""
    sections: dict[str, dict[str, str]] = {}
    current = None
    for line in lines:
        if not line:
            continue
        if line.startswith("[") and line.endswith("]"):
            current = line[1:-1]
            sections[current] = {}
            continue
        if "=" in line and current is not None:
            key, _, rest = line.partition("=")
            value = rest.split("#", 1)[0]
            sections[current][key.strip()] = value.strip()
    return sections


class TestMduDocument(unittest.TestCase):

    # --- construction ---

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

    # --- load ---

    def test_load_from_file(self):
        doc = MduDocument()
        doc.load_from_file(MDU_PATH)

    def test_load_from_file_no_errors(self):
        doc = MduDocument()
        doc.load_from_file(MDU_PATH)
        self.assertFalse(doc.report.has_errors())

    def test_load_from_file_nonexistent_raises(self):
        doc = MduDocument()
        with self.assertRaises(RuntimeError):
            doc.load_from_file("nonexistent_file.mdu")

    def test_load_from_lines(self):
        with open(MDU_PATH, "r") as f:
            lines = f.read().splitlines()
        doc = MduDocument()
        doc.load_from_lines(lines)
        self.assertEqual(doc.model.get_string("general.program"), "D-Flow FM")

    def test_multiple_instances(self):
        doc1 = MduDocument()
        doc1.load_from_file(MDU_PATH)
        doc2 = MduDocument()
        doc2.load_from_file(MDU_PATH)
        self.assertEqual(doc1.model.get_string("general.program"), "D-Flow FM")
        self.assertEqual(doc2.model.get_string("general.program"), "D-Flow FM")
        del doc1
        del doc2

    # --- save ---

    def test_save_to_lines(self):
        doc = MduDocument()
        doc.load_from_file(MDU_PATH)
        actual_lines = [line.strip() for line in doc.save_to_lines()]
        sections = _parse_ini_lines(actual_lines)
        self.assertEqual(sections["general"]["program"], "D-Flow FM")
        self.assertEqual(sections["general"]["fileType"], "modelDef")
        self.assertEqual(sections["general"]["fileVersion"], "1.02")
        self.assertEqual(sections["geometry"]["netFile"], "FlowFM_net.nc")
        self.assertEqual(sections["geometry"]["dryPointsFile"], "dry.pol dry.xyz")
        self.assertEqual(sections["geometry"]["bedLevUni"], "-5.0")
        self.assertEqual(sections["geometry"]["bedLevType"], "3")
        self.assertEqual(sections["geometry"]["useCaching"], "1")
        self.assertEqual(sections["wind"]["cdBreakPoints"], "0.00063 0.00723")
        self.assertEqual(sections["time"]["refDate"], "20010101")

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

    # --- load, change, save round-trip ---

    def test_load_change_save_roundtrip(self):
        doc = MduDocument()
        doc.load_from_file(MDU_PATH)
        doc.model.set_int("geometry.kmx", 10)
        doc.model.set_double("numerics.cflmax", 0.5)
        doc.model.set_bool("geometry.usecaching", False)

        lines = doc.save_to_lines()

        doc2 = MduDocument()
        doc2.load_from_lines(lines)
        self.assertFalse(doc2.report.has_errors())
        self.assertEqual(doc2.model.get_int("geometry.kmx"), 10)
        self.assertAlmostEqual(doc2.model.get_double("numerics.cflmax"), 0.5)
        self.assertFalse(doc2.model.get_bool("geometry.usecaching"))

    # --- new document, save round-trip ---

    def test_new_document_save_roundtrip(self):
        doc = MduDocument()
        doc.model.set_string("general.program", "MyProgram")
        doc.model.set_int("geometry.kmx", 3)
        doc.model.set_double("numerics.cflmax", 0.9)

        lines = doc.save_to_lines()

        doc2 = MduDocument()
        doc2.load_from_lines(lines)
        self.assertEqual(doc2.model.get_string("general.program"), "MyProgram")
        self.assertEqual(doc2.model.get_int("geometry.kmx"), 3)
        self.assertEqual(doc2.model.get_double("numerics.cflmax"), 0.9)

    def test_new_document_save_to_lines_contains_defaults(self):
        doc = MduDocument()
        actual_lines = [line.strip() for line in doc.save_to_lines()]
        sections = _parse_ini_lines(actual_lines)
        self.assertEqual(sections["general"]["program"], "D-Flow FM")
        self.assertEqual(sections["general"]["fileType"], "modelDef")
        self.assertEqual(sections["general"]["fileVersion"], "1.09")
        self.assertEqual(sections["geometry"]["bedLevUni"], "-5.0")
        self.assertEqual(sections["geometry"]["bedLevType"], "3")
        self.assertEqual(sections["geometry"]["useCaching"], "1")
        self.assertEqual(sections["numerics"]["cflMax"], "0.7")
        self.assertEqual(sections["wind"]["cdBreakPoints"], "0.00063 0.00723")
        self.assertEqual(sections["time"]["refDate"], "20010101")


if __name__ == "__main__":
    unittest.main()