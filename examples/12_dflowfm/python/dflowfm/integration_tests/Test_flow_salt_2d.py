import test_utils
import unittest

class Test_flow_salt_2d(unittest.TestCase):
    def setUp(self):
        self.fm = test_utils.create_model('flow_salt_2d/2d.mdu')
        self.fm.initialize()

    def tearDown(self):
        self.fm.finalize()

    def test_initialize_finalize_multiple_times(self):
        self.tearDown()
        self.setUp()

    def test_refdat(self):
        refdat = self.fm.get_string_attribute('refdat') # TODO: make it work like: start_time = self.fm.attributes[vars.start_time]

        # asserts
        self.assertEqual(refdat, '20010101')

    def test_xk(self):
        xk = self.fm.get_var('xk')

    def test_flowelemnode(self):
        flowelemnode = self.fm.get_var('flowelemnode')

if __name__ == '__main__':
    test_utils.run_test(Test_flow_salt_2d('test_xk'))
#   unittest.main()


