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
        model.load_file(MDU_PATH)
        self.assertIsNotNone(model._handle)
        del model

    def test_get_dummy_value(self):
        model = MduModel()
        model.load_file(MDU_PATH)
        self.assertEqual(model.get_dummy_value(), 42)

    def test_multiple_instances(self):
        model1 = MduModel()
        model1.load_file(MDU_PATH)
        model2 = MduModel()
        model2.load_file(MDU_PATH)
        self.assertEqual(model1.get_dummy_value(), 42)
        self.assertEqual(model2.get_dummy_value(), 42)
        del model1
        del model2

    def test_get_int_value(self):
        model = MduModel()
        model.load_file(MDU_PATH)
        result = model.get_int("geometry.kmx")
        self.assertIsInstance(result, int)
        self.assertEqual(result, 0)

    def test_get_double_value(self):
        model = MduModel()
        model.load_file(MDU_PATH)
        result = model.get_double("geometry.waterlevini")
        self.assertIsInstance(result, float)
        self.assertAlmostEqual(result, 0.0)

    def test_get_string(self):
        model = MduModel()
        model.load_file(MDU_PATH)
        result = model.get_string("general.program")
        self.assertIsInstance(result, str)
        self.assertEqual(result, "D-Flow FM")
        # The returned string should stay alive even after the model is deleted
        del model
        self.assertEqual(result, "D-Flow FM")


    # Currently the test MDU file doesn't have any string lists to use in these tests
    # def test_get_string_list(self):
    #     model = MduModel()
    #     model.load_file(MDU_PATH)
    #     result = model.get_string_list("general.program")
    #     self.assertIsInstance(result, list)
    #     for item in result:
    #         self.assertIsInstance(item, str)
    #     self.assertEqual(len(result), 2)
    #     self.assertEqual(result[0], "first_string")
    #     self.assertEqual(result[1], "second_string")

    # def test_string_list_lifetime(self):
    #     model = MduModel()
    #     model.load_file(MDU_PATH)
    #     result = model.get_string_list("any_key")
    #     # The returned list should remain valid even after the model is deleted
    #     del model
    #     self.assertEqual(len(result), 2)
    #     self.assertEqual(result[0], "first_string")

    def test_load_file_nonexistent_raises(self):
        model = MduModel()
        model.load_file(MDU_PATH)
        with self.assertRaises(RuntimeError):
            model.load_file("nonexistent_file.mdu")

    def test_get_int_unknown_key_raises(self):
        model = MduModel()
        model.load_file(MDU_PATH)
        with self.assertRaises(RuntimeError):
            model.get_int("unknown_key")

    def test_get_double_unknown_key_raises(self):
        model = MduModel()
        model.load_file(MDU_PATH)
        with self.assertRaises(RuntimeError):
            model.get_double("unknown_key")

    def test_get_bool(self):
        model = MduModel()
        model.load_file(MDU_PATH)
        result = model.get_bool("geometry.usecaching")
        self.assertIsInstance(result, bool)
        self.assertTrue(result)

    def test_get_path(self):
        model = MduModel()
        model.load_file(MDU_PATH)
        result = model.get_path("geometry.netfile")
        self.assertIsInstance(result, Path)
        self.assertEqual(result, Path("FlowFM_net.nc"))
        # The returned path should stay alive even after the model is deleted
        del model 
        self.assertEqual(result, Path("FlowFM_net.nc"))

    def test_get_path_list(self):
        model = MduModel()
        model.load_file(MDU_PATH)
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
        model.load_file(MDU_PATH)
        result = model.get_double("numerics.cflmax")
        self.assertAlmostEqual(result, 0.7)

    def test_get_bool_unknown_key_raises(self):
        model = MduModel()
        model.load_file(MDU_PATH)
        with self.assertRaises(RuntimeError):
            model.get_bool("unknown_key")

    def test_get_path_unknown_key_raises(self):
        model = MduModel()
        model.load_file(MDU_PATH)
        with self.assertRaises(RuntimeError):
            model.get_path("unknown_key")

if __name__ == "__main__":
    unittest.main()
