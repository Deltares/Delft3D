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

    def test_get_string_value(self):
        model = MduModel()
        result = model.get_string_value("any_key")
        self.assertIsInstance(result, str)
        self.assertEqual(result, "some_string_value")

    def test_string_lifetime(self):
        model = MduModel()
        result = model.get_string_value("any_key")
        # The returned string should remain valid even after the model is deleted
        del model
        self.assertEqual(result, "some_string_value")

    def test_get_string_list(self):
        model = MduModel()
        result = model.get_string_list("any_key")
        self.assertIsInstance(result, list)
        for item in result:
            self.assertIsInstance(item, str)
        self.assertEqual(len(result), 2)
        self.assertEqual(result[0], "first_string")
        self.assertEqual(result[1], "second_string")

    def test_string_list_lifetime(self):
        model = MduModel()
        result = model.get_string_list("any_key")
        # The returned list should remain valid even after the model is deleted
        del model
        self.assertEqual(len(result), 2)
        self.assertEqual(result[0], "first_string")


if __name__ == "__main__":
    unittest.main()
