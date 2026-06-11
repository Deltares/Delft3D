import unittest
import sys
import os
from pathlib import Path

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

from dflowfm_io import MduModel

# TODO add a proper reference MDU file. This is just one I had on my system.
MDU_PATH = os.path.join(os.path.dirname(__file__), "tide-2.mdu")

class TestMduModel(unittest.TestCase):
    def test_create_and_destroy(self):
        model = MduModel()
        model.load_from_file(MDU_PATH)
        self.assertIsNotNone(model._handle)
        del model

    def test_load_from_lines(self):
        mdu_data = [
            "[General]",
            "Program                                   = D-Flow FM",
            "Version                                   = 1.2.184",
            "fileType                                  = modelDef",
            "fileVersion                               = 1.09",
            "",
            "[geometry]",
            "NetFile                                   = simplechannel_net.nc# Unstructured grid file *_net.nc",
            "",
            "[numerics]"
        ]

        model = MduModel()
        model.load_from_lines(mdu_data)
        self.assertIsNotNone(model._handle)
        self.assertEqual(model.get_string("general.program"), "D-Flow FM")

    def test_save_to_lines(self):
        expected_lines = [
            "# The master definition file of D-Flow FM",
            "#",
            "# This section contains the program name and its version.",
            "[general]",
            "program               = D-Flow FM           # Program.",
            "fileVersion           = 1.02",
            "",
            "# In this section, the main entry comprises the specification of the grid (i.e. the netCDF network file). In addition, thin dams and thin dykes can be specified.",
            "[geometry]",
            "netFile               = FlowFM_net.nc       # Net file (*_net.nc) containing mesh information.",
            "useCaching            = True                # Use caching for geometrical/network-related items.",
            "kmx                   = 0                   # Number of vertical layers. NB. If keyword `zLayerGrowthFactor` is used, then number of layers is determined by D-Flow FM.",
            "waterLevIni           = 0.0000000e+00       # Initial water levels sample file (*.xyz).",
            "dryPointsFile         = dry.pol dry.xyz     # Dry points file (*.xyz), third column dummy z values, or polygon file (*.pol).",
            "",
            "# This section contains the settings of specific parts of the flow solver, such as limiters and the iterative solver type.",
            "[numerics]",
            "cflMax                = 7.0000000e-01       # Maximum Courant nr.",
            ""
        ]

        model = MduModel()
        model.load_from_file(MDU_PATH)
        actual_lines = [line.strip() for line in model.save_to_lines()]
        self.assertEqual(actual_lines, expected_lines)

    def test_save_to_file(self):
        import tempfile
        model = MduModel()
        model.load_from_file(MDU_PATH)
        model.set_string("general.program", "Modified")
        with tempfile.NamedTemporaryFile(suffix=".mdu", delete=False) as f:
            tmp_path = f.name
        try:
            model.save_to_file(tmp_path)
            # Reload and verify
            model2 = MduModel()
            model2.load_from_file(tmp_path)
            self.assertEqual(model2.get_string("general.program"), "Modified")
        finally:
            os.unlink(tmp_path)

    def test_load_change_save_roundtrip(self):
        import tempfile
        model = MduModel()
        model.load_from_file(MDU_PATH)
        model.set_int("geometry.kmx", 10)
        model.set_double("numerics.cflmax", 0.5)
        model.set_bool("geometry.usecaching", False)
        with tempfile.NamedTemporaryFile(suffix=".mdu", delete=False) as f:
            tmp_path = f.name
        try:
            model.save_to_file(tmp_path)
            model2 = MduModel()
            model2.load_from_file(tmp_path)
            self.assertEqual(model2.get_int("geometry.kmx"), 10)
            self.assertAlmostEqual(model2.get_double("numerics.cflmax"), 0.5)
            self.assertFalse(model2.get_bool("geometry.usecaching"))
        finally:
            os.unlink(tmp_path)

    def test_multiple_instances(self):
        model1 = MduModel()
        model1.load_from_file(MDU_PATH)
        model2 = MduModel()
        model2.load_from_file(MDU_PATH)
        self.assertEqual(model1.get_string("general.program"), "D-Flow FM")
        self.assertEqual(model2.get_string("general.program"), "D-Flow FM")
        del model1
        del model2

    def test_get_int_value(self):
        model = MduModel()
        model.load_from_file(MDU_PATH)
        result = model.get_int("geometry.kmx")
        self.assertIsInstance(result, int)
        self.assertEqual(result, 0)

    def test_get_double_value(self):
        model = MduModel()
        model.load_from_file(MDU_PATH)
        result = model.get_double("geometry.waterlevini")
        self.assertIsInstance(result, float)
        self.assertAlmostEqual(result, 0.0)

    def test_get_string(self):
        model = MduModel()
        model.load_from_file(MDU_PATH)
        result = model.get_string("general.program")
        self.assertIsInstance(result, str)
        self.assertEqual(result, "D-Flow FM")
        # The returned string should stay alive even after the model is deleted
        del model
        self.assertEqual(result, "D-Flow FM")


    # Currently the test MDU file doesn't have any string lists to use in these tests
    # def test_get_string_list(self):
    #     model = MduModel()
    #     model.load_from_file(MDU_PATH)
    #     result = model.get_string_list("general.program")
    #     self.assertIsInstance(result, list)
    #     for item in result:
    #         self.assertIsInstance(item, str)
    #     self.assertEqual(len(result), 2)
    #     self.assertEqual(result[0], "first_string")
    #     self.assertEqual(result[1], "second_string")

    # def test_string_list_lifetime(self):
    #     model = MduModel()
    #     model.load_from_file(MDU_PATH)
    #     result = model.get_string_list("any_key")
    #     # The returned list should remain valid even after the model is deleted
    #     del model
    #     self.assertEqual(len(result), 2)
    #     self.assertEqual(result[0], "first_string")

    # def test_set_string_list(self):
    #     model = MduModel()
    #     model.load_from_file(MDU_PATH)
    #     new_values = ["value1", "value2", "value3"]
    #     model.set_string_list("general.somelistkey", new_values)
    #     self.assertEqual(model.get_string_list("general.somelistkey"), new_values)

    def test_load_from_file_nonexistent_raises(self):
        model = MduModel()
        model.load_from_file(MDU_PATH)
        with self.assertRaises(RuntimeError):
            model.load_from_file("nonexistent_file.mdu")

    def test_get_int_unknown_key_raises(self):
        model = MduModel()
        model.load_from_file(MDU_PATH)
        with self.assertRaises(RuntimeError):
            model.get_int("unknown_key")

    def test_get_double_unknown_key_raises(self):
        model = MduModel()
        model.load_from_file(MDU_PATH)
        with self.assertRaises(RuntimeError):
            model.get_double("unknown_key")

    def test_get_bool(self):
        model = MduModel()
        model.load_from_file(MDU_PATH)
        result = model.get_bool("geometry.usecaching")
        self.assertIsInstance(result, bool)
        self.assertTrue(result)

    def test_get_path(self):
        model = MduModel()
        model.load_from_file(MDU_PATH)
        result = model.get_path("geometry.netfile")
        self.assertIsInstance(result, Path)
        self.assertEqual(result, Path("FlowFM_net.nc"))
        # The returned path should stay alive even after the model is deleted
        del model 
        self.assertEqual(result, Path("FlowFM_net.nc"))

    def test_get_path_list(self):
        model = MduModel()
        model.load_from_file(MDU_PATH)
        result = model.get_path_list("geometry.drypointsfile")
        self.assertIsInstance(result, list)
        self.assertEqual(len(result), 2)
        for item in result:
            self.assertIsInstance(item, Path)
        self.assertEqual(result, [Path('dry.pol'), Path('dry.xyz')])
        # The returned path list should stay alive even after the model is deleted
        del model 
        self.assertEqual(result, [Path('dry.pol'), Path('dry.xyz')])

    def test_get_double(self):
        model = MduModel()
        model.load_from_file(MDU_PATH)
        result = model.get_double("numerics.cflmax")
        self.assertAlmostEqual(result, 0.7)

    def test_get_bool_unknown_key_raises(self):
        model = MduModel()
        model.load_from_file(MDU_PATH)
        with self.assertRaises(RuntimeError):
            model.get_bool("unknown_key")

    def test_get_path_unknown_key_raises(self):
        model = MduModel()
        model.load_from_file(MDU_PATH)
        with self.assertRaises(RuntimeError):
            model.get_path("unknown_key")

    def test_set_int(self):
        model = MduModel()
        model.load_from_file(MDU_PATH)
        self.assertNotEqual(model.get_int("geometry.kmx"), 5)
        model.set_int("geometry.kmx", 5)
        self.assertEqual(model.get_int("geometry.kmx"), 5)

    def test_set_bool(self):
        model = MduModel()
        model.load_from_file(MDU_PATH)
        self.assertTrue(model.get_bool("geometry.usecaching"))
        model.set_bool("geometry.usecaching", False)
        self.assertFalse(model.get_bool("geometry.usecaching"))

    def test_set_double(self):
        model = MduModel()
        model.load_from_file(MDU_PATH)
        self.assertNotAlmostEqual(model.get_double("numerics.cflmax"), 0.9)
        model.set_double("numerics.cflmax", 0.9)
        self.assertAlmostEqual(model.get_double("numerics.cflmax"), 0.9)

    def test_set_string(self):
        model = MduModel()
        model.load_from_file(MDU_PATH)
        self.assertNotEqual(model.get_string("general.program"), "My Program")
        model.set_string("general.program", "My Program")
        self.assertEqual(model.get_string("general.program"), "My Program")

    def test_set_path(self):
        model = MduModel()
        model.load_from_file(MDU_PATH)
        self.assertNotEqual(model.get_path("geometry.netfile"), Path("new_net.nc"))
        model.set_path("geometry.netfile", Path("new_net.nc"))
        self.assertEqual(model.get_path("geometry.netfile"), Path("new_net.nc"))

    def test_set_path_list(self):
        model = MduModel()
        model.load_from_file(MDU_PATH)
        new_paths = [Path("a.pol"), Path("b.xyz"), Path("c.nc")]
        model.set_path_list("geometry.drypointsfile", new_paths)
        self.assertEqual(model.get_path_list("geometry.drypointsfile"), new_paths)

    def test_set_double_list(self):
        model = MduModel()
        model.load_from_file(MDU_PATH)
        new_values = [0.001, 0.005, 0.01]
        model.set_double_list("wind.cdbreakpoints", new_values)
        result = model.get_double_list("wind.cdbreakpoints")
        self.assertEqual(len(result), 3)
        for expected, actual in zip(new_values, result):
            self.assertAlmostEqual(actual, expected)

    def test_set_double_list_empty(self):
        model = MduModel()
        model.load_from_file(MDU_PATH)
        model.set_double_list("wind.cdbreakpoints", [])
        result = model.get_double_list("wind.cdbreakpoints")
        self.assertEqual(result, [])

    def test_set_double_list_single_value(self):
        model = MduModel()
        model.load_from_file(MDU_PATH)
        model.set_double_list("wind.cdbreakpoints", [3.14])
        result = model.get_double_list("wind.cdbreakpoints")
        self.assertEqual(len(result), 1)
        self.assertAlmostEqual(result[0], 3.14)


if __name__ == "__main__":
    unittest.main()
