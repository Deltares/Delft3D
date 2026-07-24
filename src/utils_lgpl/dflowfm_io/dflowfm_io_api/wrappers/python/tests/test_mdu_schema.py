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

    def test_multi_word_section_key_round_trips(self):
        # The [external forcing] section has a space; the dotted key must keep it.
        doc = _loaded_doc()
        doc.schema.external_forcing.extForceFileNew = ["a.ext", "b.ext"]
        self.assertEqual(doc.schema.external_forcing.extForceFileNew, [Path("a.ext"), Path("b.ext")])

    def test_list_property_reads_as_list_of_paths(self):
        doc = _loaded_doc()
        value = doc.schema.external_forcing.extForceFileNew
        self.assertIsInstance(value, list)
        self.assertTrue(all(isinstance(item, Path) for item in value))

    def test_intenum_property_reads_name_and_round_trips(self):
        # bedLevType is an intenum; Layer 2 surfaces it by name, like the string enum.
        doc = _loaded_doc()
        name = doc.schema.geometry.bedLevType
        self.assertIsInstance(name, str)
        doc.schema.geometry.bedLevType = name
        self.assertEqual(doc.schema.geometry.bedLevType, name)

    def test_digit_leading_key_is_sanitised_to_an_accessible_attribute(self):
        # 1D2DLinkFile is not a valid identifier; attr_name prefixes an underscore.
        doc = _loaded_doc()
        self.assertTrue(hasattr(type(doc.schema.geometry), "_1D2DLinkFile"))


if __name__ == "__main__":
    unittest.main()
