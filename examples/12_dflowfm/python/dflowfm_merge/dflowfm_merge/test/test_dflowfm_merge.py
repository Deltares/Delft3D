import glob
import unittest
from dflowfm_merge import merger

class Test_merge_nc_files(unittest.TestCase):
    def test_dd_example(self):
        input_files = glob.glob(r"..\..\..\..\test_data\dd_example\output\dd_example_*_map.nc")
        output_files = r"..\..\..\..\test_data\dd_example\output\dd_example_map_merged.nc"

        m = merger.merger(input_files, output_files)
        m.run()

        pass

if __name__ == '__main__':
    unittest.main()


