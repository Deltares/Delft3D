import os
import sys
import unittest

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "..", "scripts"))

from generate_bindings import (
    BindingsRenderer,
    CFunction,
    CTypeMapper,
    HeaderParser,
    ModelRenderer,
    trailing_identifier,
)

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


if __name__ == "__main__":
    unittest.main()
