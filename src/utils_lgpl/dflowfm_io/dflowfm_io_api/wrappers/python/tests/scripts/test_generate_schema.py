import os
import sys
import unittest

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "..", "scripts"))

from generate_schema import NameSanitizer, SchemaGenerator, SchemaRenderer


class TestNameSanitizer(unittest.TestCase):
    """Naming rules for sections and keywords."""

    def test_attribute_passthrough(self):
        self.assertEqual(NameSanitizer.attribute("netFile"), "netFile")

    def test_attribute_digit_leading_gets_underscore(self):
        self.assertEqual(NameSanitizer.attribute("1D2DLinkFile"), "_1D2DLinkFile")

    def test_attribute_keyword_gets_trailing_underscore(self):
        self.assertEqual(NameSanitizer.attribute("class"), "class_")

    def test_section_class_multi_word(self):
        self.assertEqual(NameSanitizer.section_class("external forcing"), "ExternalForcingSection")

    def test_section_attribute_multi_word(self):
        self.assertEqual(NameSanitizer.section_attribute("external forcing"), "external_forcing")


class TestSchemaRenderer(unittest.TestCase):
    """Docstring escaping, property rendering, and fail-closed collision behaviour."""

    def setUp(self):
        self.renderer = SchemaRenderer("mdu.json")

    def test_docstring_separates_trailing_quote(self):
        self.assertTrue(SchemaRenderer.docstring('ends in "').endswith('" '))

    def test_docstring_collapses_triple_quote(self):
        self.assertNotIn('"""', SchemaRenderer.docstring('a """ b'))

    def test_docstring_escapes_backslash(self):
        self.assertEqual(SchemaRenderer.docstring("a\\b"), "a\\\\b")

    def test_docstring_collapses_whitespace(self):
        self.assertEqual(SchemaRenderer.docstring("a\n  b\t c"), "a b c")

    def test_render_scalar_property(self):
        lines = self.renderer.render_property("geometry", {"key": "netFile", "value_type": "path"})
        self.assertIn("    def netFile(self) -> Path:", lines)
        self.assertIn('        return self._model.get_path("geometry.netfile")', lines)
        self.assertIn('        self._model.set_path("geometry.netfile", value)', lines)

    def test_render_enum_property_uses_int_enum(self):
        lines = self.renderer.render_property("time", {"key": "tUnit", "value_type": "enum"})
        self.assertIn("    def tUnit(self) -> int:", lines)
        self.assertIn('        return self._model.get_enum("time.tunit")', lines)

    def test_render_property_unknown_value_type_raises(self):
        with self.assertRaises(KeyError):
            self.renderer.render_property("geometry", {"key": "foo", "value_type": "uint"})

    def test_case_only_key_collision_raises(self):
        section = {
            "name": "geometry",
            "ini_properties": [
                {"key": "NetFile", "value_type": "path"},
                {"key": "netFile", "value_type": "path"},
            ],
        }
        with self.assertRaises(ValueError):
            self.renderer.render([section])

    def test_section_name_collision_raises(self):
        sections = [
            {"name": "geometry", "ini_properties": []},
            {"name": "geometry", "ini_properties": []},
        ]
        with self.assertRaises(ValueError):
            self.renderer.render(sections)

    def test_property_name_collision_raises(self):
        # 'class' sanitises to 'class_', colliding with a literal 'class_' key.
        section = {
            "name": "geometry",
            "ini_properties": [
                {"key": "class", "value_type": "int"},
                {"key": "class_", "value_type": "int"},
            ],
        }
        with self.assertRaises(ValueError):
            self.renderer.render([section])

    def test_render_returns_source_and_property_count(self):
        section = {"name": "time", "ini_properties": [{"key": "tZone", "value_type": "float"}]}
        source, total = self.renderer.render([section])
        self.assertEqual(total, 1)
        self.assertIn("class TimeSection:", source)
        self.assertIn("class MduSchema:", source)
        self.assertIn("        self.time = TimeSection(model)", source)

    def test_render_emits_known_properties_registry(self):
        section = {"name": "geometry", "ini_properties": [{"key": "netFile", "value_type": "path"}]}
        source, _ = self.renderer.render([section])
        self.assertIn("KNOWN_PROPERTIES = frozenset({", source)
        self.assertIn('    "geometry.netfile",', source)


class TestSchemaGenerator(unittest.TestCase):
    """The fail-closed value_type gate."""

    def test_unknown_value_type_raises(self):
        sections = [{"name": "geometry", "ini_properties": [{"key": "k", "value_type": "uint"}]}]
        with self.assertRaises(ValueError):
            SchemaGenerator._require_all_types_supported(sections)

    def test_all_supported_types_pass(self):
        sections = [{"name": "geometry", "ini_properties": [{"key": "k", "value_type": "int"}]}]
        SchemaGenerator._require_all_types_supported(sections)  # must not raise


if __name__ == "__main__":
    unittest.main()
