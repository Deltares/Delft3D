import os
import unittest

import bmi.wrapper

from test_utils import modelpath

class TestCase(unittest.TestCase):
    def setUp(self):
        path = modelpath('san_francisco/parallel')
        self.fm = bmi.wrapper.BMIWrapper(
            engine="dflowfm",
            configfile=os.path.join(path, "r07e_bay.mdu") #"2d.mdu")
        )
        self.fm.initialize()

    def tearDown(self):
        self.fm.finalize()

    def test_initialize_finalize_multiple_times(self):
        self.tearDown()
        self.setUp()

    def test_xk(self):
        xk = self.fm.get_var('xk')
    def test_sa1(self):
        sa1 = self.fm.get_var('sa1')
        self.assertTrue(sa1 is not None)

    def test_flowelemnode(self):
        flowelemnode = self.fm.get_var('flowelemnode')

if __name__ == '__main__':
    unittest.main()
