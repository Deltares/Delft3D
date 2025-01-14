"""NetCDF file comparer.

Copyright (C)  Stichting Deltares, 2024
"""

import os
import re
import sys
from datetime import datetime, timedelta
from typing import List, Optional, Tuple

import netCDF4 as nc
import numpy as np

import src.utils.plot_differences as plot
from src.config.file_check import FileCheck
from src.config.parameter import Parameter
from src.utils.comparers.comparison_result import ComparisonResult
from src.utils.comparers.i_comparer import IComparer
from src.utils.logging.i_logger import ILogger


class RowColumnIndex:
    """A set or row and column."""

    def __init__(self, row: int, column: int) -> None:
        self.row = row
        self.column = column


class PlotData:
    """Some strings that are passed on as plot data."""

    def __init__(
        self,
        testcase_name: str,
        variable_name: str,
        right_path: str,
    ) -> None:
        self.testcase_name = testcase_name
        self.variable_name = variable_name
        self.right_path = right_path


class PlotValues:
    """Compare and reference data to be plotted."""

    def __init__(
        self,
        reference: np.ndarray,
        compare: np.ndarray,
    ) -> None:
        self.reference = reference
        self.compare = compare


class ReferenceValues:
    """Data class to store reference and compare values."""

    def __init__(
        self,
        minimum: float,
        maximum: float,
    ) -> None:
        self.minimum = minimum
        self.maximum = maximum

    def difference(self) -> float:
        """Return the deferance between max and min value."""
        return self.maximum - self.minimum


class NetCdfVariable:
    """Data class to store the netCDF4 varialbe of the left and right file."""

    def __init__(
        self,
        left: nc.Variable,
        right: nc.Variable,
    ) -> None:
        self.left = left
        self.right = right

    def get_difference(self) -> np.ndarray:
        """Calculate the absolute difference between two NetCDF variables."""
        return np.abs(self.left[:] - self.right[:])


class DateTimeDelta:
    """Data class to parse date time strings and extract more suiteable date time types."""

    def __init__(self, time_description: str) -> None:
        self.date_time = self.convert_time_description_2_datetime(time_description)
        self.delta = self.convert_time_description_2_delta(time_description)

    def convert_time_description_2_datetime(self, time_description: str) -> datetime:
        """Return a `start_datetime`.

        For instance, `'seconds since 1998-08-01 00:00:00'` yields the
        following tuple: `(datetime(1998, 8, 1, 0, 0, 0)`.
        """
        try:
            words = time_description.lower().strip().split(" ")

            # Deduce start_datetime
            date_split = words[2].split("-")
            if len(date_split) != 3:
                raise ValueError(f"Cannot infer date from: {words[2]}")

            time_split = words[3].split(":")
            if len(time_split) != 3:
                raise ValueError(f"Cannot infer time from: {words[3]}")

            start_datetime = datetime(
                int(date_split[0]),
                int(date_split[1]),
                int(date_split[2]),
                int(time_split[0]),
                int(time_split[1]),
                int(time_split[2]),
            )

        except Exception as e:
            raise ValueError(
                "Can not interpret the following unit: "
                + str(time_description)
                + ". A correct example is: 'seconds since 1998-08-01 00:00:00'."
            ) from e

        return start_datetime

    def convert_time_description_2_delta(self, time_description: str) -> timedelta:
        """Return a `(start_datetime, timedelta)` tuple.

        For instance, `'seconds since 1998-08-01 00:00:00'` yields the
        following tuple: `(datetime(1998, 8, 1, 0, 0, 0), timedelta(seconds=1))`.
        """
        try:
            words = time_description.lower().strip().split(" ")
            delta = self.get_time_delta(words)
        except Exception as e:
            raise ValueError(
                "Can not interpret the following unit: "
                + str(time_description)
                + ". A correct example is: 'seconds since 1998-08-01 00:00:00'."
            ) from e

        return delta

    def get_time_delta(self, words: list[str]) -> timedelta:
        """Get the timedelta."""
        if "millisecond" in words[0]:
            delta = timedelta(milliseconds=1)
        elif "second" in words[0]:
            delta = timedelta(seconds=1)
        elif "minute" in words[0]:
            delta = timedelta(minutes=1)
        elif "hour" in words[0]:
            delta = timedelta(hours=1)
        elif "day" in words[0]:
            delta = timedelta(days=1)
        elif "week" in words[0]:
            delta = timedelta(weeks=1)
        else:
            raise ValueError(f"Can not infer timedelta from: {words[0]}")
        return delta


class NetcdfComparer(IComparer):
    """Compare two netCDF files, according to the configuration in file_check."""

    def __init__(self, enable_plotting: bool = True) -> None:
        self._enable_plotting = enable_plotting

    def compare(
        self,
        left_path: str,
        right_path: str,
        file_check: FileCheck,
        testcase_name: str,
        logger: ILogger,
    ) -> List[Tuple[str, FileCheck, Parameter, ComparisonResult]]:
        """
        Compare two netCDF files, according to the configuration in file_check.

        The left file is the compare and the right is the reference file. The method returns a list of comparison result
        for all parameters that are in the filecheck.
        """
        results = []
        left_nc_root = self._open_netcdf_file(left_path, file_check.name)
        right_nc_root = self._open_netcdf_file(right_path, file_check.name)
        # For each parameter for this file:
        for parameters in file_check.parameters.values():
            for parameter in parameters:
                found_parameter_in_file = False
                for variable_name in left_nc_root.variables.keys():
                    if re.match(f"^{parameter.name}$", variable_name) is None:
                        continue
                    found_parameter_in_file = True
                    result = self._compare_nc_variable(
                        right_path,
                        file_check.name,
                        testcase_name,
                        logger,
                        parameter,
                        left_nc_root,
                        right_nc_root,
                        variable_name,
                    )
                    results.append((testcase_name, file_check, parameter, result))

                if not found_parameter_in_file:
                    error_msg = f"No match for parameter name {parameter.name} in file \
                        {os.path.join(left_path, file_check.name)}"
                    raise AttributeError(error_msg)
        return results

    def _compare_nc_variable(
        self,
        right_path: str,
        file_check_name: str,
        testcase_name: str,
        logger: ILogger,
        parameter: Parameter,
        left_nc_root: nc.Dataset,
        right_nc_root: nc.Dataset,
        variable_name: str,
    ) -> ComparisonResult:
        """
        Compare variable for an n-dimensional array.

        Compare a specified variable between two NetCDF datasets and evaluates the differences based on absolute and
        relative tolerances. The function is intended for n-dimensional arrays.
        """
        result = ComparisonResult()
        plot_data = PlotData(testcase_name, variable_name, right_path)
        try:
            logger.debug(f"Checking parameter: {variable_name}")
            nc_var = NetCdfVariable(left_nc_root.variables[variable_name], right_nc_root.variables[variable_name])
            self._check_for_dimension_equality(nc_var, variable_name)

            # http://docs.scipy.org/doc/numpy/reference/generated/numpy.argmax.html
            result = self._compare_array(parameter, left_nc_root, variable_name, nc_var)

        except RuntimeError as e:
            logger.error(str(e))
            result.error = True

        except Exception as e:
            logger.error(f"Could not find parameter: {variable_name}, in file: {file_check_name}")
            logger.error(str(e))
            result.error = True

        if result.result == "NOK":
            if nc_var.left.ndim == 1:
                logger.info(f"Plotting of 1d-array not yet supported, variable name: {variable_name}")
            if nc_var.left.ndim == 2 and self._enable_plotting:
                result.error = self._plot_2d_array(
                    logger,
                    plot_data,
                    left_nc_root,
                    nc_var,
                    parameter,
                )

        return result

    def _compare_array(
        self,
        parameter: Parameter,
        left_nc_root: nc.Dataset,
        variable_name: str,
        nc_var: NetCdfVariable,
    ) -> ComparisonResult:
        """Compare Netcdf variable based on dimensions of the left file."""
        if nc_var.left.dtype == np.dtype("S1"):
            return self._compare_strings(parameter, left_nc_root, variable_name, nc_var)
        else:
            return self._compare_floats(parameter, left_nc_root, variable_name, nc_var)

    def _compare_strings(
        self,
        parameter: Parameter,
        left_nc_root: nc.Dataset,
        variable_name: str,
        nc_var: NetCdfVariable,
    ) -> ComparisonResult:
        left_strings = nc.chartostring(nc_var.left[:])
        right_strings = nc.chartostring(nc_var.right[:])
        result = ComparisonResult()
        result.passed = np.array_equal(left_strings, right_strings)
        result.result = "OK" if result.passed else "NOK"
        return result

    def _compare_floats(
        self,
        parameter: Parameter,
        left_nc_root: nc.Dataset,
        variable_name: str,
        nc_var: NetCdfVariable,
    ) -> ComparisonResult:
        reference_values = ReferenceValues(float(np.min(nc_var.left[:])), float(np.max(nc_var.left[:])))
        if nc_var.left.ndim == 1:
            result = self._compare_1d_arrays(nc_var)
        elif nc_var.left.ndim == 2:
            cf_role_time_series_vars = search_times_series_id(left_nc_root)
            diff_arr = nc_var.get_difference()

            array_index = self._get_array_index(
                parameter.location, cf_role_time_series_vars, left_nc_root, diff_arr, variable_name
            )
            reference_values.minimum = np.min(nc_var.left[:, array_index.column])
            reference_values.maximum = np.max(nc_var.left[:, array_index.column])
            result = self._compare_2d_arrays(nc_var, diff_arr, array_index)
        else:
            result = self._compare_nd_arrays(nc_var)

        # if min_ref_value has no value (it is a  _FillValue) then the test is OK (presumed)
        if np.ma.is_masked(reference_values.minimum):
            result.max_abs_diff = 0.0
            result.max_abs_diff_values = 0.0
            reference_values.maximum = 0.0
            reference_values.minimum = 0.0

        result.max_rel_diff = self._get_max_rel_diff(result.max_abs_diff, reference_values)

        result.is_tolerance_exceeded(
            parameter.tolerance_absolute,
            parameter.tolerance_relative,
        )
        return result

    def _compare_1d_arrays(self, nc_var: NetCdfVariable) -> ComparisonResult:
        """Compare two 1D arrays datasets and returns the maximum absolute difference."""
        result = ComparisonResult()
        diff_arr = nc_var.get_difference()
        i_max = np.argmax(diff_arr)
        result.max_abs_diff = float(diff_arr[i_max])
        result.max_abs_diff_coordinates = (i_max,)
        result.max_abs_diff_values = (nc_var.left[i_max], nc_var.right[i_max])
        return result

    def _compare_2d_arrays(
        self,
        nc_var: NetCdfVariable,
        diff_arr: np.ndarray,
        array_index: RowColumnIndex,
    ) -> ComparisonResult:
        """Compare two 2D arrays datasets and returns the comparison result."""
        result = ComparisonResult()

        result.max_abs_diff = diff_arr[array_index.row, array_index.column]
        result.max_abs_diff_coordinates = (array_index.row, array_index.column)
        result.max_abs_diff_values = (
            nc_var.left[array_index.row, array_index.column],
            nc_var.right[array_index.row, array_index.column],
        )
        return result

    def _compare_nd_arrays(self, nc_var: NetCdfVariable) -> ComparisonResult:
        """
        Compare two n-dimensional arrays and find the maximum absolute difference.

        This method computes the absolute differences between corresponding elements
        of two n-dimensional arrays, identifies the maximum absolute difference, and
        returns the value of this difference along with its coordinates and the values
        at these coordinates in the original arrays.
        """
        result = ComparisonResult()
        diff_arr = nc_var.get_difference()
        i_max = np.argmax(diff_arr)

        block_sizes = self._get_block_sizes(diff_arr)
        coordinates = self._get_coordinates_of_max_deviation(i_max, block_sizes)

        maxdiff = diff_arr
        left_at_maxdiff = nc_var.left
        right_at_maxdiff = nc_var.right
        try:
            for c in coordinates:
                maxdiff = maxdiff[c]
                left_at_maxdiff = left_at_maxdiff[c]
                right_at_maxdiff = right_at_maxdiff[c]
        except Exception as e:
            error_msg = (
                "Mismatch dimensions: len maxdiff and coordinates: " + str(len(maxdiff)) + " , " + str(len(coordinates))
            )
            raise RuntimeError(error_msg, e) from e

        result.max_abs_diff = float(maxdiff)
        result.max_abs_diff_coordinates = tuple(coordinates)
        result.max_abs_diff_values = (
            left_at_maxdiff,
            right_at_maxdiff,
        )
        return result

    def _plot_2d_array(
        self,
        logger: ILogger,
        plot_data: PlotData,
        left_nc_root: nc.Dataset,
        nc_var: NetCdfVariable,
        parameter: Parameter,
    ) -> bool:
        """Plot a 2D array or time series based on the provided parameters."""
        try:
            cf_role_time_series_vars = search_times_series_id(left_nc_root)
            diff_arr = nc_var.get_difference()

            array_index = self._get_array_index(
                parameter.location, cf_role_time_series_vars, left_nc_root, diff_arr, plot_data.variable_name
            )

            time_var = search_time_variable(left_nc_root, plot_data.variable_name)
            if cf_role_time_series_vars.__len__() == 0:
                plot_values = PlotValues(nc_var.left[array_index.row, :], nc_var.right[array_index.row, :])
                observation_type = parameter.name
                subtitle = self._get_plot_subtitle(time_var, array_index.row)

                self._plot_2d(
                    plot_data,
                    plot_values,
                    left_nc_root,
                    nc_var.left,
                    parameter.tolerance_absolute,
                    subtitle,
                )
            else:
                plot_values = PlotValues(nc_var.left[:, array_index.column], nc_var.right[:, array_index.column])
                observation_type = self._get_observation_type(nc_var.left, cf_role_time_series_vars)
                plot_location = self._determine_plot_location(left_nc_root, observation_type, array_index.column)
                self._plot_time_series(
                    plot_data,
                    plot_values,
                    time_var,
                    plot_location,
                )

        except Exception as e:
            logger.error(f"Plotting of parameter {plot_data.variable_name} failed")
            logger.error(str(e))
            return True
        return False

    def _plot_2d(
        self,
        plot_data: PlotData,
        plot_values: PlotValues,
        left_nc_root: nc.Dataset,
        left_nc_var: nc.Variable,
        tolerance_absolute: float,
        subtitle: str,
    ) -> None:
        """Plot a 2D graph."""
        coords = left_nc_var.coordinates.split()
        x_coords = left_nc_root.variables[coords[0]][:]
        y_coords = left_nc_root.variables[coords[1]][:]

        plot.PlotDifferencesMap(
            plot_data.right_path,
            x_coords,
            y_coords,
            plot_values.reference,
            plot_values.compare,
            tolerance_absolute,
            plot_data.testcase_name,
            plot_data.variable_name,
            subtitle,
            "netcdf",
        )

    def _plot_time_series(
        self,
        plot_data: PlotData,
        plot_values: PlotValues,
        time_var: nc.Dataset,
        plot_location: str,
    ) -> None:
        """Plot a time series graph."""
        unit_txt = "".join(time_var.units).strip()
        date_time_delta = DateTimeDelta(unit_txt)
        datetime_series = [date_time_delta.date_time + int(t_i) * date_time_delta.delta for t_i in time_var[:]]

        plot.PlotDifferencesTimeSeries(
            plot_data.right_path,
            datetime_series,
            plot_values.reference,
            plot_values.compare,
            plot_data.testcase_name,
            plot_data.variable_name,
            plot_location,
            "netcdf",
        )

    def _determine_plot_location(self, left_nc_root: nc.Dataset, observation_type: str, column_id: int) -> str:
        """Determine location name, needed when no location is specified otherwise it is equal to parameter_location."""
        plot_location = left_nc_root.variables[observation_type][column_id][:]
        plot_location = b"".join(filter(None, plot_location)).decode("utf-8").strip()
        if plot_location == "":
            plot_location = "model_wide"
        return str(plot_location)

    def _get_plot_subtitle(self, time_var: nc.Variable, row_id: int) -> str:
        """Compute datetime for which we are making a plot / scalar field."""
        unit_txt = "".join(time_var.units).strip()

        date_time_delta = DateTimeDelta(unit_txt)
        plot_datetime = date_time_delta.date_time + date_time_delta.delta * int(time_var[row_id])

        subtitle = datetime.strftime(plot_datetime, "%Y%m%d_%H%M%S")
        return subtitle

    def _get_observation_type(self, left_nc_var: nc.Variable, cf_role_time_series_vars: List[str]) -> str:
        """Determine the observation type based on the coordinates attribute of the NetCDF variable."""
        if hasattr(left_nc_var, "coordinates"):
            location_types = left_nc_var.coordinates.split(" ")
            for variable in cf_role_time_series_vars:
                for location_type in location_types:
                    if location_type == variable:
                        return str(location_type)
        return str(cf_role_time_series_vars[0])

    def _get_max_rel_diff(self, max_abs_diff: float, reference_values: ReferenceValues) -> float:
        """
        Calculate the maximum relative difference.

        This method converts an absolute difference into a relative difference by dividing it by the difference between
        the maximum and minimum reference values. It handles edge cases where the differences are very small.
        """
        # Make the absolute difference in maxDiff relative, by dividing by (max_ref_value-min_ref_value).
        if max_abs_diff < 2 * sys.float_info.epsilon:
            return 0.0
        elif reference_values.difference() < 2 * sys.float_info.epsilon:
            return 1.0
        else:
            return min(1.0, max_abs_diff / (reference_values.difference()))

    def _get_array_index(
        self,
        parameter_location: Optional[str],
        cf_role_time_series_vars: List[str],
        left_nc_root: nc.Dataset,
        diff_arr: np.ndarray,
        variable_name: str,
    ) -> RowColumnIndex:
        """Find the column and row ID based on the given parameter location and difference array."""
        parameter_location_found = False
        column_id = 0
        row_id = 0

        if parameter_location is not None:
            for variable in cf_role_time_series_vars:
                if variable is not None:
                    location_var = left_nc_root.variables[variable]
                    location_var_values = [b"".join(x).strip().decode("utf-8").strip("\x00") for x in location_var[:]]
                    if parameter_location in location_var_values:
                        parameter_location_found = True
                        column_id = location_var_values.index(parameter_location)
                else:
                    parameter_location_found = True
                    column_id = 0

            if not parameter_location_found:
                raise KeyError(f"Cannot find parameter location {parameter_location} for variable {variable_name}")
            row_id = int(np.argmax(diff_arr[:, column_id]))
        else:
            i_max = np.argmax(diff_arr)
            column_id = int(i_max % diff_arr.shape[1])
            row_id = int(i_max / diff_arr.shape[1])

        return RowColumnIndex(row_id, column_id)

    def _get_coordinates_of_max_deviation(self, i_max: np.int64, block_sizes: list) -> list:
        """Calculate the coordinates of the maximum deviation in a multi-dimensional array."""
        coordinates = []
        remainder = i_max
        for size in block_sizes:
            coordinates.append(remainder // size)
            remainder %= size
        return coordinates

    def _get_block_sizes(self, diff_arr: np.ndarray) -> list:
        """Calculate block sizes based on the shape of the input array."""
        block_sizes = [1]
        for n in reversed(diff_arr.shape):
            block_sizes.append(block_sizes[-1] * n)
        block_sizes.pop()  # Last block size is irrelevant.
        block_sizes.reverse()
        return block_sizes

    def _open_netcdf_file(self, path: str, filename: str) -> nc.Dataset:
        """Open NetCDF file and return netCDF dataset."""
        nc_root = nc.Dataset(os.path.join(path, filename), "r", format="NETCDF4_CLASSIC")
        return nc_root

    def _check_for_dimension_equality(self, nc_var: NetCdfVariable, variable_name: str) -> None:
        """Check dimension equility and raises exception if not correct."""
        if nc_var.left.shape != nc_var.right.shape:
            raise ValueError(
                f"Shapes of parameter {variable_name} not compatible. Shape of reference: "
                + f"{nc_var.left.shape}. Shape of run data: {nc_var.right.shape}"
            )


def search_time_variable(nc_root: nc.Dataset, var_name: str) -> nc.Variable:
    """Return time dimension or `None`."""
    keywords = ("seconds", "minute", "hour", "days")
    for dim in nc_root.variables[var_name].dimensions:
        if dim in nc_root.variables:
            if any(keyword in nc_root.variables[dim].units for keyword in keywords):
                return nc_root.variables[dim]

    raise ValueError(
        "Can not find the time variable. Plotting of non-time dependent parameters is not supported."
        + f"Parameter name: '{var_name}'."
    )


def search_times_series_id(nc_root: nc.Dataset) -> List[str]:
    """Return variable key if `cf_role == timeseries_id`, otherwise `None`."""
    keys = []
    for key, value in nc_root.variables.items():
        try:
            if value.cf_role == "timeseries_id":
                keys.append(key)
        except Exception:
            pass
    return keys
