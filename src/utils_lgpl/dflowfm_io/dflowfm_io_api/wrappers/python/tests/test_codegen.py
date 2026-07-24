import os
import sys
import unittest

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "codegen"))

import generate_bindings
import generate_schema


class TestParseEnum(unittest.TestCase):
    """The enum harvester in the bindings generator."""

    def test_explicit_decimal_values(self):
        self.assertEqual(generate_bindings.parse_enum("A = 0, B = 1, C = 2"), [("A", 0), ("B", 1), ("C", 2)])

    def test_implicit_ordinals(self):
        self.assertEqual(generate_bindings.parse_enum("A, B, C"), [("A", 0), ("B", 1), ("C", 2)])

    def test_hex_and_implicit_mix(self):
        self.assertEqual(generate_bindings.parse_enum("A = 0x1, B, C = 0x4"), [("A", 1), ("B", 2), ("C", 4)])

    def test_trailing_comma_ignored(self):
        self.assertEqual(generate_bindings.parse_enum("A = 0,"), [("A", 0)])

    def test_unparseable_member_raises(self):
        with self.assertRaises(ValueError):
            generate_bindings.parse_enum("A = SOMETHING_ELSE")


class TestSchemaHelpers(unittest.TestCase):
    """Naming, docstring, and fail-closed helpers in the schema generator."""

    def test_attr_name_passthrough(self):
        self.assertEqual(generate_schema.attr_name("netFile"), "netFile")

    def test_attr_name_digit_leading_gets_underscore(self):
        self.assertEqual(generate_schema.attr_name("1D2DLinkFile"), "_1D2DLinkFile")

    def test_attr_name_keyword_gets_trailing_underscore(self):
        self.assertEqual(generate_schema.attr_name("class"), "class_")

    def test_class_name_multi_word(self):
        self.assertEqual(generate_schema.class_name("external forcing"), "ExternalForcingSection")

    def test_docstring_separates_trailing_quote(self):
        self.assertTrue(generate_schema.docstring('ends in "').endswith('" '))

    def test_docstring_collapses_triple_quote(self):
        self.assertNotIn('"""', generate_schema.docstring('a """ b'))

    def test_render_property_unknown_value_type_raises(self):
        with self.assertRaises(KeyError):
            generate_schema.render_property("geometry", {"key": "foo", "value_type": "uint"})


if __name__ == "__main__":
    unittest.main()
