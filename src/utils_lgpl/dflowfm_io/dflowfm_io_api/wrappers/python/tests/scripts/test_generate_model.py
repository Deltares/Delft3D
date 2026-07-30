import os
import sys
import unittest

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "..", "scripts"))

from generate_model import CFunction, HeaderParser, ModelRenderer

# A minimal but representative header fragment covering get/set functions.
HEADER_FRAGMENT = """
DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_get_int(mdu_handle_t handle, const char* key, int32_t* int_out);
DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_set_int(mdu_handle_t handle, const char* key, int32_t value);
"""


class TestHeaderParser(unittest.TestCase):
    """Function extraction from header text."""

    def setUp(self):
        self.parser = HeaderParser(HEADER_FRAGMENT)

    def test_functions(self):
        functions = self.parser.functions()
        self.assertEqual([f.name for f in functions], ["mdu_get_int", "mdu_set_int"])
        self.assertIsInstance(functions[0], CFunction)
        self.assertEqual(functions[0].restype, "dflowfm_io_result_t")


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

    def test_string_enum_accessor(self):
        lines = self.renderer._accessor("mdu_get_string_enum")
        self.assertIn("    def get_string_enum(self, key: str) -> str:", lines)

    def test_int_enum_accessor(self):
        lines = self.renderer._accessor("mdu_get_int_enum")
        self.assertIn("    def get_int_enum(self, key: str) -> int:", lines)

    def test_unmapped_accessor_suffix_raises_valueerror(self):
        with self.assertRaises(ValueError):
            self.renderer._accessor("mdu_get_bogus")

    def test_non_accessor_function_is_skipped(self):
        self.assertIsNone(self.renderer._accessor("mdu_get_issue_list"))
        self.assertIsNone(self.renderer._accessor("mdu_create"))


if __name__ == "__main__":
    unittest.main()
