#  Description: NetCDF file comparer
#  -----------------------------------------------------
#  Copyright (C)  Stichting Deltares, 2013


import datetime
import os
from test.utils.test_logger import TestLogger

import netCDF4 as nc
import pytest

import src.utils.comparers.netcdf_comparer as nccmp
from src.config.file_check import FileCheck
from src.config.parameter import Parameter
from src.config.types.file_type import FileType


class TestNetcdfComparer:
    def setup_method(self):
        self.testroot = os.path.abspath(os.path.dirname(__file__))
        self.testdata = os.path.join(self.testroot, "data")
        self.lp = os.path.join(self.testdata, "left")
        self.rp = os.path.join(self.testdata, "right")
        self.filename = "str_map.nc"

    ##################################################

    def test_compare(self):
        fc = FileCheck()
        pm = Parameter()
        pm.name = "mesh2d_s1"
        pm.tolerance_absolute = 0.0001
        pm.tolerance_relative = 0.01

        fc.name = "str_map.nc"
        fc.type = FileType.NETCDF
        fc.parameters = {"par1": [pm]}
        comparer = nccmp.NetcdfComparer()
        logger = TestLogger()
        path = os.path.join('test')
        results = comparer.compare(self.lp, self.rp, fc, path, logger)
        resultstruc = results[0][3]

        # perform a set of asserts on the result structure
        assert not resultstruc.passed
        assert not resultstruc.error
        assert resultstruc.result == "NOK"
        assert pytest.approx(resultstruc.maxAbsDiff) == 0.01983249918399
        assert resultstruc.maxAbsDiffCoordinates == (1, 0)
        assert pytest.approx(resultstruc.maxRelDiff) == 0.21672465466549

    def test_time_independent_compare(self):
        fc = FileCheck()
        pm = Parameter()
        pm.name = "mesh2d_node_x"
        pm.tolerance_absolute = 0.0001
        pm.tolerance_relative = 0.01
        fc.name = "str_map.nc"
        fc.type = FileType.NETCDF
        fc.parameters = {"par1": [pm]}
        comparer = nccmp.NetcdfComparer()
        logger = TestLogger()
        path = os.path.join('test')
        results = comparer.compare(self.lp, self.rp, fc, path, logger)
        resultstruc = results[0][3]
        print(resultstruc.result)

    def test_search_time_variable(self):
        nc_root = nc.Dataset(os.path.join(self.lp, "str_map.nc"))
        varid = nccmp.search_time_variable(nc_root, "mesh2d_s1")
        stname = varid.getncattr("standard_name")
        assert stname == "time"

    def test_search_times_series_id(self):
        nc_root = nc.Dataset(os.path.join(self.lp, "str_his.nc"))
        tssid = nccmp.search_times_series_id(nc_root)
        assert tssid == ["station_name"]

    def test_interpret_time_unit(self):
        time_description = "seconds since 2015-11-01 00:00:00"
        (datum, delta) = nccmp.interpret_time_unit(time_description)
        assert datum == datetime.datetime(2015, 11, 1, 0, 0)
