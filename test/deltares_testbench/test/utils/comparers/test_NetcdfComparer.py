#  Description: NetCDF file comparer
#  -----------------------------------------------------
#  Copyright (C)  Stichting Deltares, 2013


import datetime
import os
import tempfile
from typing import AnyStr, List, Tuple

import netCDF4 as nc
import numpy as np
import pytest
from pytest_mock import MockerFixture

import src.utils.comparers.netcdf_comparer as nccmp
from src.config.file_check import FileCheck
from src.config.parameter import Parameter
from src.config.types.file_type import FileType
from src.utils.comparers.comparison_result import ComparisonResult
from src.utils.logging.i_logger import ILogger


@pytest.fixture
def testdata() -> str:
    testroot = os.path.abspath(os.path.dirname(__file__))
    return os.path.join(testroot, "data")


@pytest.fixture
def right_path(testdata: str) -> str:
    return os.path.join(testdata, "right")


@pytest.fixture
def left_path(testdata: str) -> str:
    return os.path.join(testdata, "left")


@pytest.fixture
def test_path() -> str:
    return os.path.join("test")


@pytest.fixture
def logger(mocker: MockerFixture) -> ILogger:
    logger = mocker.Mock(spec=ILogger)
    return logger


@pytest.fixture
def comparer() -> nccmp.NetcdfComparer:
    return nccmp.NetcdfComparer()


class TestNetcdfComparer:
    ##################################################

    def test_compare(self, left_path: str, right_path: str, logger: ILogger) -> None:
        fc = FileCheck()
        pm = Parameter()
        pm.name = "mesh2d_s1"
        pm.tolerance_absolute = 0.0001
        pm.tolerance_relative = 0.01

        fc.name = "str_map.nc"
        fc.type = FileType.NETCDF
        fc.parameters = {"par1": [pm]}
        comparer = nccmp.NetcdfComparer(enable_plotting=False)
        path = os.path.join("test")
        results = comparer.compare(left_path, right_path, fc, path, logger)
        resultstruc = results[0][3]

        # perform a set of asserts on the result structure
        assert not resultstruc.passed
        assert not resultstruc.error
        assert resultstruc.result == "NOK"
        assert pytest.approx(resultstruc.max_abs_diff) == 0.01983249918399
        assert resultstruc.max_abs_diff_coordinates == (1, 0)
        assert pytest.approx(resultstruc.max_rel_diff) == 0.21672465466549

    def test_time_independent_compare(self, left_path: str, right_path: str, logger: ILogger) -> None:
        fc = FileCheck()
        pm = Parameter()
        pm.name = "mesh2d_node_x"
        pm.tolerance_absolute = 0.0001
        pm.tolerance_relative = 0.01
        fc.name = "str_map.nc"
        fc.type = FileType.NETCDF
        fc.parameters = {"par1": [pm]}
        comparer = nccmp.NetcdfComparer(enable_plotting=False)
        path = os.path.join("test")
        results = comparer.compare(left_path, right_path, fc, path, logger)
        resultstruc = results[0][3]
        print(resultstruc.result)

    def test_search_time_variable(self, left_path: str) -> None:
        nc_root = nc.Dataset(os.path.join(left_path, "str_map.nc"))
        varid = nccmp.search_time_variable(nc_root, "mesh2d_s1")
        stname = varid.getncattr("standard_name")
        assert stname == "time"

    def test_search_times_series_id(self, left_path: str) -> None:
        nc_root = nc.Dataset(os.path.join(left_path, "str_his.nc"))
        tssid = nccmp.search_times_series_id(nc_root)
        assert tssid == ["station_name"]

    def test_interpret_time_unit(self) -> None:
        time_description = "seconds since 2015-11-01 00:00:00"
        datum = nccmp.DateTimeDelta(time_description)
        assert datum.date_time == datetime.datetime(2015, 11, 1, 0, 0)

    def test_strings_are_equal(
        self,
        comparer: nccmp.NetcdfComparer,
        test_path: str,
        logger: ILogger,
        mocker: MockerFixture,
    ) -> None:
        same_strings = ["foo", "bar", "baz"]
        left = self.make_string_dataset("pump_name", same_strings, row_name="pump", col_name="name_len")
        right = self.make_string_dataset("pump_name", same_strings, row_name="pump", col_name="name_len")

        dataset_constructor = mocker.patch("src.utils.comparers.netcdf_comparer.nc.Dataset")
        dataset_constructor.side_effect = [left, right]

        fc = self.create_netcdf_file_check("pump_name")

        results = comparer.compare(left.filepath(), right.filepath(), fc, test_path, logger)

        self.assert_comparison_passed(results)

    def test_strings_are_not_equal(
        self,
        comparer: nccmp.NetcdfComparer,
        test_path: str,
        logger: ILogger,
        mocker: MockerFixture,
    ) -> None:
        left = self.make_string_dataset("pump_name", ["foo", "bar"], row_name="pump", col_name="name_len")
        right = self.make_string_dataset("pump_name", ["bar", "baz"], row_name="pump", col_name="name_len")

        dataset_constructor = mocker.patch("src.utils.comparers.netcdf_comparer.nc.Dataset")
        dataset_constructor.side_effect = [left, right]

        fc = self.create_netcdf_file_check("pump_name")

        results = comparer.compare(left.filepath(), right.filepath(), fc, test_path, logger)

        self.assert_comparison_failed(results)

    def create_netcdf_file_check(self, parameter_name: str) -> FileCheck:
        fc = FileCheck()
        pm = Parameter()
        pm.name = parameter_name

        fc.name = "irrelevant"
        fc.type = FileType.NETCDF
        fc.parameters = {parameter_name: [pm]}

        return fc

    def assert_comparison_passed(self, results: List[Tuple[str, FileCheck, Parameter, ComparisonResult]]) -> None:
        self.assert_comparison_result_equals(results, True, "OK")

    def assert_comparison_failed(self, results: List[Tuple[str, FileCheck, Parameter, ComparisonResult]]) -> None:
        self.assert_comparison_result_equals(results, False, "NOK")

    def assert_comparison_result_equals(
        self, results: List[Tuple[str, FileCheck, Parameter, ComparisonResult]], passed: bool, message: AnyStr
    ) -> None:
        result = results[0][3]
        assert result.passed == passed
        assert result.result == message

    @staticmethod
    def make_string_dataset(
        var_name: str,
        strings: list[str],
        row_name: str = "y",
        col_name: str = "x",
    ) -> nc.Dataset:
        max_len = max(len(s) for s in strings)

        # Create netcdf dataset - somehow this needs a writable (temporary) file location
        temp_file = tempfile.NamedTemporaryFile(suffix=".nc")
        ds = nc.Dataset(temp_file.name, "w", diskless=True, persist=False, format="NETCDF4_CLASSIC")
        ds.createDimension(row_name, len(strings))
        ds.createDimension(col_name, max_len)
        variable = ds.createVariable(var_name, "S1", (row_name, col_name))

        # Fill variable with string data.
        result = np.zeros((len(strings), max_len), dtype="S1")
        for i, s in enumerate(strings):
            result[i, 0 : len(s)] = list(s)
        variable[:] = result

        temp_file.close()

        return ds
