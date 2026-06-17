import unittest
import sys
import os
from datetime import datetime, timezone
from pathlib import Path

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

from dflowfm_io import MduDocument

MDU_PATH = os.path.join(os.path.dirname(__file__), "tide-2.mdu")

MDU_MINIMAL_REQUIRED_ONLY = [
    "[general]",
    "fileType      = modelDef",
    "fileVersion   = 1.09",
    "",
    "[geometry]",
    "NetFile       = simplechannel_net.nc",
]

def _loaded_doc(path=MDU_PATH):
    doc = MduDocument()
    doc.load_from_file(path)
    return doc

def _minimal_doc():
    doc = MduDocument()
    doc.load_from_lines(MDU_MINIMAL_REQUIRED_ONLY)
    return doc

class TestMduModel(unittest.TestCase):

    # --- get ---

    def test_get_int_value(self):
        doc = _loaded_doc()
        result = doc.model.get_int("geometry.kmx")
        self.assertIsInstance(result, int)
        self.assertEqual(result, 0)

    def test_get_double_value(self):
        doc = _loaded_doc()
        result = doc.model.get_double("geometry.waterlevini")
        self.assertIsInstance(result, float)
        self.assertAlmostEqual(result, 0.0)

    def test_get_string(self):
        doc = _loaded_doc()
        result = doc.model.get_string("general.program")
        self.assertIsInstance(result, str)
        self.assertEqual(result, "D-Flow FM")
        # Result should stay alive after doc is deleted
        del doc
        self.assertEqual(result, "D-Flow FM")

    def test_get_bool(self):
        doc = _loaded_doc()
        result = doc.model.get_bool("geometry.usecaching")
        self.assertIsInstance(result, bool)
        self.assertTrue(result)

    def test_get_path(self):
        doc = _loaded_doc()
        result = doc.model.get_path("geometry.netfile")
        self.assertIsInstance(result, Path)
        self.assertEqual(result, Path("FlowFM_net.nc"))
        # Result should stay alive after doc is deleted
        del doc
        self.assertEqual(result, Path("FlowFM_net.nc"))

    def test_get_path_list(self):
        doc = _loaded_doc()
        result = doc.model.get_path_list("geometry.drypointsfile")
        self.assertIsInstance(result, list)
        self.assertEqual(len(result), 2)
        for item in result:
            self.assertIsInstance(item, Path)
        self.assertEqual(result, [Path('dry.pol'), Path('dry.xyz')])
        # Result should stay alive after doc is deleted
        del doc
        self.assertEqual(result, [Path('dry.pol'), Path('dry.xyz')])

    def test_get_double(self):
        doc = _loaded_doc()
        self.assertAlmostEqual(doc.model.get_double("numerics.cflmax"), 0.7)

    def test_get_intenum(self):
        doc = _loaded_doc()
        result = doc.model.get_enum("general.autostart")
        self.assertIsInstance(result, int)
        self.assertEqual(result, 0)

    def test_get_enum(self):
        doc = _loaded_doc()
        result = doc.model.get_enum("general.filetype")
        self.assertIsInstance(result, int)
        self.assertEqual(result, 0)

    def test_get_datetime(self):
        doc = _loaded_doc()
        result = doc.model.get_datetime("time.refdate")
        self.assertIsInstance(result, datetime)
        self.assertEqual(result, datetime(2001, 1, 1, tzinfo=timezone.utc))

    # --- get: unknown key raises ---

    def test_get_int_unknown_key_raises(self):
        doc = _loaded_doc()
        with self.assertRaises(RuntimeError):
            doc.model.get_int("unknown_key")

    def test_get_double_unknown_key_raises(self):
        doc = _loaded_doc()
        with self.assertRaises(RuntimeError):
            doc.model.get_double("unknown_key")

    def test_get_bool_unknown_key_raises(self):
        doc = _loaded_doc()
        with self.assertRaises(RuntimeError):
            doc.model.get_bool("unknown_key")

    def test_get_path_unknown_key_raises(self):
        doc = _loaded_doc()
        with self.assertRaises(RuntimeError):
            doc.model.get_path("unknown_key")

    def test_get_datetime_unknown_key_raises(self):
        doc = _loaded_doc()
        with self.assertRaises(RuntimeError):
            doc.model.get_datetime("nonexisting.key")

    # --- get: default values ---

    def test_get_int_default_value(self):
        doc = _minimal_doc()
        self.assertEqual(doc.model.get_int("geometry.kmx"), 0)

    def test_get_double_default_value(self):
        doc = _minimal_doc()
        self.assertAlmostEqual(doc.model.get_double("numerics.cflmax"), 0.7)

    def test_get_bool_default_value(self):
        doc = _minimal_doc()
        self.assertTrue(doc.model.get_bool("geometry.usecaching"))

    def test_get_string_default_value(self):
        doc = _minimal_doc()
        self.assertEqual(doc.model.get_string("general.program"), "D-Flow FM")

    def test_get_intenum_default_value(self):
        doc = _minimal_doc()
        self.assertEqual(doc.model.get_enum("general.autoStart"), 0)

    def test_get_enum_default_value(self):
        doc = _minimal_doc()
        self.assertEqual(doc.model.get_enum("numerics.flowSolver"), 0)

    def test_get_double_list_default_value(self):
        doc = _minimal_doc()
        result = doc.model.get_double_list("wind.cdbreakpoints")
        self.assertEqual(len(result), 2)
        self.assertAlmostEqual(result[0], 0.00063)
        self.assertAlmostEqual(result[1], 0.00723)

    def test_get_datetime_default_value(self):
        doc = _minimal_doc()
        result = doc.model.get_datetime("time.refdate")
        self.assertEqual(result, datetime(2001, 1, 1, tzinfo=timezone.utc))

    # --- set ---

    def test_set_int(self):
        doc = _loaded_doc()
        self.assertNotEqual(doc.model.get_int("geometry.kmx"), 5)
        doc.model.set_int("geometry.kmx", 5)
        self.assertEqual(doc.model.get_int("geometry.kmx"), 5)

    def test_set_bool(self):
        doc = _loaded_doc()
        self.assertTrue(doc.model.get_bool("geometry.usecaching"))
        doc.model.set_bool("geometry.usecaching", False)
        self.assertFalse(doc.model.get_bool("geometry.usecaching"))

    def test_set_double(self):
        doc = _loaded_doc()
        self.assertNotAlmostEqual(doc.model.get_double("numerics.cflmax"), 0.9)
        doc.model.set_double("numerics.cflmax", 0.9)
        self.assertAlmostEqual(doc.model.get_double("numerics.cflmax"), 0.9)

    def test_set_string(self):
        doc = _loaded_doc()
        doc.model.set_string("general.program", "My Program")
        self.assertEqual(doc.model.get_string("general.program"), "My Program")

    def test_set_intenum(self):
        doc = _loaded_doc()
        doc.model.set_enum("general.autostart", 1)
        self.assertEqual(doc.model.get_enum("general.autostart"), 1)

    def test_set_enum(self):
        doc = _loaded_doc()
        doc.model.set_enum("numerics.flowSolver", 1)
        self.assertEqual(doc.model.get_enum("numerics.flowSolver"), 1)

    def test_set_path(self):
        doc = _loaded_doc()
        doc.model.set_path("geometry.netfile", Path("new_net.nc"))
        self.assertEqual(doc.model.get_path("geometry.netfile"), Path("new_net.nc"))

    def test_set_path_list(self):
        doc = _loaded_doc()
        new_paths = [Path("a.pol"), Path("b.xyz"), Path("c.nc")]
        doc.model.set_path_list("geometry.drypointsfile", new_paths)
        self.assertEqual(doc.model.get_path_list("geometry.drypointsfile"), new_paths)

    def test_set_double_list(self):
        doc = _loaded_doc()
        new_values = [0.001, 0.005, 0.01]
        doc.model.set_double_list("wind.cdbreakpoints", new_values)
        result = doc.model.get_double_list("wind.cdbreakpoints")
        self.assertEqual(len(result), 3)
        for expected, actual in zip(new_values, result):
            self.assertAlmostEqual(actual, expected)

    def test_set_double_list_empty(self):
        doc = _loaded_doc()
        doc.model.set_double_list("wind.cdbreakpoints", [])
        self.assertEqual(doc.model.get_double_list("wind.cdbreakpoints"), [])

    def test_set_double_list_single_value(self):
        doc = _loaded_doc()
        doc.model.set_double_list("wind.cdbreakpoints", [3.14])
        result = doc.model.get_double_list("wind.cdbreakpoints")
        self.assertEqual(len(result), 1)
        self.assertAlmostEqual(result[0], 3.14)

    def test_set_datetime(self):
        doc = _loaded_doc()
        new_dt = datetime(2025, 6, 11, 8, 30, 22, tzinfo=timezone.utc)
        doc.model.set_datetime("time.refdate", new_dt)
        self.assertEqual(doc.model.get_datetime("time.refdate"), new_dt)

    # --- set: nonexisting key raises ---

    def test_set_nonexisting_key_int(self):
        doc = _loaded_doc()
        with self.assertRaises(RuntimeError):
            doc.model.set_int("nonexisting.key", 42)

    def test_set_nonexisting_key_bool(self):
        doc = _loaded_doc()
        with self.assertRaises(RuntimeError):
            doc.model.set_bool("nonexisting.key", True)

    def test_set_nonexisting_key_double(self):
        doc = _loaded_doc()
        with self.assertRaises(RuntimeError):
            doc.model.set_double("nonexisting.key", 3.14)

    def test_set_nonexisting_key_string(self):
        doc = _loaded_doc()
        with self.assertRaises(RuntimeError):
            doc.model.set_string("nonexisting.key", "hello")

    def test_set_nonexisting_key_path(self):
        doc = _loaded_doc()
        with self.assertRaises(RuntimeError):
            doc.model.set_path("nonexisting.key", Path("some/path.nc"))

    def test_set_nonexisting_key_string_list(self):
        doc = _loaded_doc()
        with self.assertRaises(RuntimeError):
            doc.model.set_string_list("nonexisting.key", ["a", "b"])

    def test_set_nonexisting_key_path_list(self):
        doc = _loaded_doc()
        with self.assertRaises(RuntimeError):
            doc.model.set_path_list("nonexisting.key", [Path("a.pol")])

    def test_set_nonexisting_key_double_list(self):
        doc = _loaded_doc()
        with self.assertRaises(RuntimeError):
            doc.model.set_double_list("nonexisting.key", [1.0, 2.0])

    def test_set_nonexisting_key_datetime(self):
        doc = _loaded_doc()
        with self.assertRaises(RuntimeError):
            doc.model.set_datetime("nonexisting.key", datetime(2020, 1, 1, tzinfo=timezone.utc))

    # --- set: value out of range ---

    def test_set_intenum_out_of_range(self):
        doc = _loaded_doc()
        with self.assertRaises(RuntimeError):
            doc.model.set_enum("general.autostart", 3)

    def test_set_enum_out_of_range(self):
        doc = _loaded_doc()
        with self.assertRaises(RuntimeError):
            doc.model.set_enum("numerics.flowSolver", 3)


if __name__ == "__main__":
    unittest.main()