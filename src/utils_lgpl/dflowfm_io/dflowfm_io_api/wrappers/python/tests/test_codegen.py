import os
import sys
import unittest

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "scripts"))

from generate_bindings import BindingsGenerator, CTypeMapper, HeaderParser, ModelRenderer
from generate_schema import NameSanitizer, SchemaGenerator, SchemaRenderer


class TestHeaderParserEnum(unittest.TestCase):
    """The enum harvester in the bindings generator."""

    def test_explicit_decimal_values(self):
        self.assertEqual(HeaderParser.parse_enum("A = 0, B = 1, C = 2"), [("A", 0), ("B", 1), ("C", 2)])

    def test_implicit_ordinals(self):
        self.assertEqual(HeaderParser.parse_enum("A, B, C"), [("A", 0), ("B", 1), ("C", 2)])

    def test_hex_and_implicit_mix(self):
        self.assertEqual(HeaderParser.parse_enum("A = 0x1, B, C = 0x4"), [("A", 1), ("B", 2), ("C", 4)])

    def test_trailing_comma_ignored(self):
        self.assertEqual(HeaderParser.parse_enum("A = 0,"), [("A", 0)])

    def test_unparseable_member_raises(self):
        with self.assertRaises(ValueError):
            HeaderParser.parse_enum("A = SOMETHING_ELSE")


class TestCTypeMapper(unittest.TestCase):
    """The C-declaration to ctypes mapping."""

    def setUp(self):
        self.types = CTypeMapper()

    def test_scalar(self):
        self.assertEqual(self.types.to_ctypes("int32_t"), "ctypes.c_int32")

    def test_char_pointer_is_c_char_p(self):
        self.assertEqual(self.types.to_ctypes("const char*"), "ctypes.c_char_p")

    def test_bare_char_is_c_char(self):
        self.assertEqual(self.types.to_ctypes("char"), "ctypes.c_char")

    def test_pointer_wraps(self):
        self.assertEqual(self.types.to_ctypes("int32_t*"), "ctypes.POINTER(ctypes.c_int32)")


class TestNameSanitizer(unittest.TestCase):
    """Naming rules in the schema generator."""

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
    """Docstring and property rendering, including fail-closed behaviour."""

    def setUp(self):
        self.renderer = SchemaRenderer("mdu.json")

    def test_docstring_separates_trailing_quote(self):
        self.assertTrue(SchemaRenderer.docstring('ends in "').endswith('" '))

    def test_docstring_collapses_triple_quote(self):
        self.assertNotIn('"""', SchemaRenderer.docstring('a """ b'))

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


class TestModelRenderer(unittest.TestCase):
    """The accessor dispatch in the bindings generator."""

    def test_unmapped_accessor_suffix_raises_valueerror(self):
        with self.assertRaises(ValueError):
            ModelRenderer("dflowfm_io_api.h")._accessor("mdu_get_bogus")

    def test_non_accessor_function_is_skipped(self):
        self.assertIsNone(ModelRenderer("dflowfm_io_api.h")._accessor("mdu_get_issue_list"))
        self.assertIsNone(ModelRenderer("dflowfm_io_api.h")._accessor("mdu_create"))


class TestGeneratedFilesInSync(unittest.TestCase):
    """The committed generated files must equal a fresh regeneration — guards against generator drift."""

    def _assert_in_sync(self, generator):
        for module in generator.build():
            self.assertEqual(
                module.path.read_text(encoding="utf-8"),
                module.source,
                f"{module.path.name} is stale vs its generator; run scripts/ to regenerate and commit it",
            )

    def test_bindings_and_model_in_sync(self):
        self._assert_in_sync(BindingsGenerator())

    def test_schema_in_sync(self):
        self._assert_in_sync(SchemaGenerator())


if __name__ == "__main__":
    unittest.main()
