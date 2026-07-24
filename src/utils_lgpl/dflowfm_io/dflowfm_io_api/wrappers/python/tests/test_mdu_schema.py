import unittest
import sys
import os
from datetime import datetime
from pathlib import Path

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "src"))

from dflowfm_io import MduDocument, MduSchema

MDU_PATH = os.path.join(os.path.dirname(__file__), "tide-2.mdu")


def _loaded_doc(path=MDU_PATH):
    doc = MduDocument()
    doc.load_from_file(path)
    return doc


class TestMduModelEnumName(unittest.TestCase):
    """The enum-name accessors surfaced from the C ABI's mdu_get_enum_name/mdu_set_enum_name."""

    def test_get_enum_name_returns_the_name(self):
        doc = _loaded_doc()
        # time.tUnit is an enum; mdu_get_enum returns the index, mdu_get_enum_name the name.
        self.assertEqual(doc.model.get_enum_name("time.tunit"), "S")

    def test_set_enum_name_round_trips(self):
        doc = _loaded_doc()
        doc.model.set_enum_name("time.tunit", "M")
        self.assertEqual(doc.model.get_enum_name("time.tunit"), "M")

    def test_set_unknown_enum_name_raises(self):
        doc = _loaded_doc()
        with self.assertRaises(RuntimeError):
            doc.model.set_enum_name("time.tunit", "not-a-valid-name")


class TestMduSchema(unittest.TestCase):
    """The generated Layer-2 typed per-keyword access (MduSchema)."""

    def test_document_exposes_schema(self):
        doc = _loaded_doc()
        self.assertIsInstance(doc.schema, MduSchema)

    def test_typed_scalar_properties(self):
        doc = _loaded_doc()
        self.assertIsInstance(doc.schema.geometry.kmx, int)
        self.assertIsInstance(doc.schema.geometry.waterLevIni, float)
        self.assertIsInstance(doc.schema.geometry.netFile, Path)
        self.assertIsInstance(doc.schema.time.refDate, datetime)

    def test_enum_property_reads_name(self):
        doc = _loaded_doc()
        self.assertEqual(doc.schema.time.tUnit, "S")

    def test_property_round_trips_through_schema(self):
        doc = _loaded_doc()
        doc.schema.geometry.kmx = 7
        self.assertEqual(doc.schema.geometry.kmx, 7)
        doc.schema.time.tUnit = "M"
        self.assertEqual(doc.schema.time.tUnit, "M")

    def test_multi_word_section_is_accessible(self):
        doc = _loaded_doc()
        self.assertTrue(hasattr(doc.schema, "external_forcing"))


if __name__ == "__main__":
    unittest.main()
