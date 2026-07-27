import io
import os
import sys
import tempfile
import unittest
from contextlib import redirect_stdout
from pathlib import Path

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "..", "scripts"))

from codegen_support import GeneratedModule, Generator, SourceBuilder
from generate_bindings import (
    BindingsGenerator,
    BindingsRenderer,
    CFunction,
    CTypeMapper,
    HeaderParser,
    ModelRenderer,
    trailing_identifier,
)
from generate_schema import NameSanitizer, SchemaGenerator, SchemaRenderer

# A minimal but representative header fragment covering the enum, the struct, and get/set functions.
HEADER_FRAGMENT = """
typedef enum mdu_severity_t
{
    MDU_SEVERITY_INFO = 0,
    MDU_SEVERITY_WARNING = 1,
    MDU_SEVERITY_ERROR = 2
} mdu_severity_t;

typedef struct mdu_issue_t
{
    int32_t line_number;
    mdu_severity_t severity;
    const char* message;
} mdu_issue_t;

DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_get_int(mdu_handle_t handle, const char* key, int32_t* int_out);
DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_set_int(mdu_handle_t handle, const char* key, int32_t value);
"""


# --- codegen_support.py ---


class TestSourceBuilder(unittest.TestCase):
    """The line/blank/extend/render accumulator."""

    def test_empty_render_is_a_single_newline(self):
        self.assertEqual(SourceBuilder().render(), "\n")

    def test_line_and_render(self):
        self.assertEqual(SourceBuilder().line("a").line("b").render(), "a\nb\n")

    def test_extend_appends_all(self):
        self.assertEqual(SourceBuilder().extend(["a", "b"]).render(), "a\nb\n")

    def test_blank_inserts_blank_lines(self):
        self.assertEqual(SourceBuilder().line("a").blank(2).line("b").render(), "a\n\n\nb\n")

    def test_methods_are_chainable(self):
        builder = SourceBuilder()
        self.assertIs(builder.line("a"), builder)
        self.assertIs(builder.blank(), builder)
        self.assertIs(builder.extend([]), builder)


class TestGeneratedModule(unittest.TestCase):
    """The value object for a module to write."""

    def test_is_frozen(self):
        module = GeneratedModule(Path("x.py"), "src", "summary")
        with self.assertRaises(Exception):
            module.source = "other"


class TestGeneratorRun(unittest.TestCase):
    """The base Generator writes each built module and reports it."""

    def test_run_writes_files_and_prints_summary(self):
        with tempfile.TemporaryDirectory() as tmp:
            target = Path(tmp) / "out.py"

            class _Fake(Generator):
                def build(self):
                    return [GeneratedModule(target, "hello\n", "1 thing")]

            out = io.StringIO()
            with redirect_stdout(out):
                _Fake().run()

            self.assertEqual(target.read_text(encoding="utf-8"), "hello\n")
            self.assertIn("Wrote 1 thing", out.getvalue())

    def test_build_is_abstract(self):
        with self.assertRaises(TypeError):
            Generator()


# --- generate_bindings.py ---


class TestTrailingIdentifier(unittest.TestCase):
    """Extraction of the trailing identifier of a C declaration."""

    def test_returns_trailing_name(self):
        self.assertEqual(trailing_identifier("const char* key", "parameter"), "key")

    def test_raises_when_no_name(self):
        with self.assertRaises(ValueError):
            trailing_identifier("const char*", "parameter")


class TestHeaderParserEnum(unittest.TestCase):
    """The enum harvester."""

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


class TestHeaderParser(unittest.TestCase):
    """Enum/struct/function extraction from header text."""

    def setUp(self):
        self.parser = HeaderParser(HEADER_FRAGMENT, CTypeMapper())

    def test_enum_members(self):
        self.assertEqual(
            self.parser.enum_members(),
            [("MDU_SEVERITY_INFO", 0), ("MDU_SEVERITY_WARNING", 1), ("MDU_SEVERITY_ERROR", 2)],
        )

    def test_struct_fields(self):
        self.assertEqual(
            self.parser.struct_fields(),
            [
                ("line_number", "ctypes.c_int32"),
                ("severity", "ctypes.c_int32"),
                ("message", "ctypes.c_char_p"),
            ],
        )

    def test_functions(self):
        functions = self.parser.functions()
        self.assertEqual([f.name for f in functions], ["mdu_get_int", "mdu_set_int"])
        self.assertIsInstance(functions[0], CFunction)
        self.assertEqual(functions[0].restype, "dflowfm_io_result_t")


class TestCTypeMapper(unittest.TestCase):
    """The C-declaration to ctypes mapping."""

    def setUp(self):
        self.types = CTypeMapper()

    def test_scalar(self):
        self.assertEqual(self.types.to_ctypes("int32_t"), "ctypes.c_int32")

    def test_void(self):
        self.assertEqual(self.types.to_ctypes("void"), "None")

    def test_char_pointer_is_c_char_p(self):
        self.assertEqual(self.types.to_ctypes("const char*"), "ctypes.c_char_p")

    def test_bare_char_is_c_char(self):
        self.assertEqual(self.types.to_ctypes("char"), "ctypes.c_char")

    def test_pointer_wraps(self):
        self.assertEqual(self.types.to_ctypes("int32_t*"), "ctypes.POINTER(ctypes.c_int32)")

    def test_double_pointer_char(self):
        self.assertEqual(self.types.to_ctypes("const char**"), "ctypes.POINTER(ctypes.c_char_p)")

    def test_struct_type_passthrough(self):
        self.assertEqual(self.types.to_ctypes("mdu_issue_t"), "mdu_issue_t")

    def test_argtypes_empty_and_void(self):
        self.assertEqual(self.types.argtypes(""), [])
        self.assertEqual(self.types.argtypes("void"), [])

    def test_argtypes_multiple(self):
        self.assertEqual(
            self.types.argtypes("mdu_handle_t handle, const char* key, int32_t* out"),
            ["ctypes.c_void_p", "ctypes.c_char_p", "ctypes.POINTER(ctypes.c_int32)"],
        )


class TestBindingsRenderer(unittest.TestCase):
    """Rendering of the ctypes ABI mirror."""

    def setUp(self):
        parser = HeaderParser(HEADER_FRAGMENT, CTypeMapper())
        self.source = BindingsRenderer("dflowfm_io_api.h", CTypeMapper()).render(
            parser.enum_members(), parser.struct_fields(), parser.functions()
        )

    def test_has_generated_banner(self):
        self.assertTrue(self.source.startswith('"""GENERATED from dflowfm_io_api.h'))

    def test_emits_enum_constants(self):
        self.assertIn("MDU_SEVERITY_ERROR = 2", self.source)

    def test_emits_struct_class(self):
        self.assertIn("class mdu_issue_t(ctypes.Structure):", self.source)
        self.assertIn('("message", ctypes.c_char_p),', self.source)

    def test_emits_function_signatures(self):
        self.assertIn(
            "lib.mdu_get_int.argtypes = [ctypes.c_void_p, ctypes.c_char_p, ctypes.POINTER(ctypes.c_int32)]",
            self.source,
        )
        self.assertIn("lib.mdu_get_int.restype = ctypes.c_int32", self.source)


class TestModelRenderer(unittest.TestCase):
    """The accessor dispatch and rendered method bodies."""

    def setUp(self):
        self.renderer = ModelRenderer("dflowfm_io_api.h")

    def test_scalar_getter_body(self):
        lines = self.renderer._accessor("mdu_get_int")
        self.assertIn("    def get_int(self, key: str) -> int:", lines)
        self.assertIn("        return value.value", lines)

    def test_list_getter_body(self):
        lines = self.renderer._accessor("mdu_get_double_list")
        self.assertIn("    def get_double_list(self, key: str) -> list[float]:", lines)
        self.assertTrue(any("for i in range(size_out.value)" in line for line in lines))

    def test_scalar_setter_body(self):
        lines = self.renderer._accessor("mdu_set_string")
        self.assertIn("    def set_string(self, key: str, value: str) -> None:", lines)

    def test_list_setter_identity_encoding(self):
        lines = self.renderer._accessor("mdu_set_double_list")
        self.assertIn("        encoded = list(values)", lines)

    def test_enum_name_accessor(self):
        lines = self.renderer._accessor("mdu_get_enum_name")
        self.assertIn("    def get_enum_name(self, key: str) -> str:", lines)

    def test_unmapped_accessor_suffix_raises_valueerror(self):
        with self.assertRaises(ValueError):
            self.renderer._accessor("mdu_get_bogus")

    def test_non_accessor_function_is_skipped(self):
        self.assertIsNone(self.renderer._accessor("mdu_get_issue_list"))
        self.assertIsNone(self.renderer._accessor("mdu_create"))


# --- generate_schema.py ---


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

    def test_render_enum_property_uses_enum_name(self):
        lines = self.renderer.render_property("time", {"key": "tUnit", "value_type": "enum"})
        self.assertIn("    def tUnit(self) -> str:", lines)
        self.assertIn('        return self._model.get_enum_name("time.tunit")', lines)

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


class TestSchemaGenerator(unittest.TestCase):
    """The fail-closed value_type gate."""

    def test_unknown_value_type_raises(self):
        sections = [{"name": "geometry", "ini_properties": [{"key": "k", "value_type": "uint"}]}]
        with self.assertRaises(ValueError):
            SchemaGenerator._require_all_types_supported(sections)

    def test_all_supported_types_pass(self):
        sections = [{"name": "geometry", "ini_properties": [{"key": "k", "value_type": "int"}]}]
        SchemaGenerator._require_all_types_supported(sections)  # must not raise


# --- committed output invariant ---


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
