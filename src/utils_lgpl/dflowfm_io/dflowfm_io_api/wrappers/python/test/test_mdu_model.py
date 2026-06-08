import unittest
import sys
import os

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

from dflowfm_io import MduModel


class TestMduModel(unittest.TestCase):
    def test_create_and_destroy(self):
        model = MduModel()
        self.assertIsNotNone(model._handle)
        del model

    def test_get_dummy_value(self):
        model = MduModel()
        self.assertEqual(model.get_dummy_value(), 42)

    def test_multiple_instances(self):
        model1 = MduModel()
        model2 = MduModel()
        self.assertEqual(model1.get_dummy_value(), 42)
        self.assertEqual(model2.get_dummy_value(), 42)
        del model1
        del model2


if __name__ == "__main__":
    unittest.main()
