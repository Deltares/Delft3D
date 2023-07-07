import test_utils
import unittest

class Test_f04_bottomfriction_c016_2DConveyance_bend(unittest.TestCase):
    def setUp(self):
        self.fm = test_utils.create_model('e00_unstruc/f04_bottomfriction/c016_2DConveyance_bend/input')
        self.fm.initialize('bendprof.mdu')

    def tearDown(self):
        self.fm.finalize()

    # TODO: add some meaningful functional tests here

    def test_conveyence_formula(self):
        self.assertTrue(1)
        pass

    def test_performance(self):
        pass

if __name__ == '__main__':
    unittest.main()
