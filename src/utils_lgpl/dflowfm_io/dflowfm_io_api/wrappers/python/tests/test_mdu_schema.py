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

    def test_enum_property_reads_int(self):
        # tUnit is an enum; Layer 2 surfaces its integer value (the C ABI has no name accessor).
        doc = _loaded_doc()
        self.assertIsInstance(doc.schema.time.tUnit, int)

    def test_property_round_trips_through_schema(self):
        doc = _loaded_doc()
        doc.schema.geometry.kmx = 7
        self.assertEqual(doc.schema.geometry.kmx, 7)
        tunit = doc.schema.time.tUnit
        doc.schema.time.tUnit = tunit
        self.assertEqual(doc.schema.time.tUnit, tunit)

    def test_multi_word_section_is_accessible(self):
        doc = _loaded_doc()
        self.assertTrue(hasattr(doc.schema, "external_forcing"))

    def test_known_properties_registry(self):
        # The generated dotted-key set consumers use for schema-existence queries.
        from dflowfm_io.mdu.schema import KNOWN_PROPERTIES

        self.assertIsInstance(KNOWN_PROPERTIES, frozenset)
        self.assertIn("geometry.netfile", KNOWN_PROPERTIES)
        self.assertIn("external forcing.extforcefilenew", KNOWN_PROPERTIES)
        self.assertNotIn("bogus.key", KNOWN_PROPERTIES)

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

    def test_intenum_property_reads_int_and_round_trips(self):
        # bedLevType is an intenum; the schema has no label for it, so Layer 2 surfaces the integer.
        doc = _loaded_doc()
        value = doc.schema.geometry.bedLevType
        self.assertIsInstance(value, int)
        doc.schema.geometry.bedLevType = value
        self.assertEqual(doc.schema.geometry.bedLevType, value)

    def test_digit_leading_key_is_sanitised_to_an_accessible_attribute(self):
        # 1D2DLinkFile is not a valid identifier; attr_name prefixes an underscore.
        doc = _loaded_doc()
        self.assertTrue(hasattr(type(doc.schema.geometry), "_1D2DLinkFile"))


if __name__ == "__main__":
    unittest.main()
