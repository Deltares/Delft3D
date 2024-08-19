"""NetCDF file comparer.

Copyright (C)  Stichting Deltares, 2024
"""

import copy
import os
import re
import sys
from datetime import datetime, timedelta
from typing import List, Tuple

import netCDF4 as nc
import numpy as np

from typing import Any
import src.utils.plot_differences as plot
from src.config.file_check import FileCheck
from src.config.parameter import Parameter
from src.utils.comparers.comparison_2d_array_result import Comparison2DArrayResult
from src.utils.comparers.comparison_result import ComparisonResult
from src.utils.comparers.i_comparer import IComparer
from src.utils.logging.i_logger import ILogger


class NetcdfComparer(IComparer):
    """Compare two netCDF files, according to the configuration in file_check."""

    def compare(
        self,
        left_path: str,
        right_path: str,
        file_check: FileCheck,
        testcase_name: str,
        logger: ILogger,
    ) -> List[Tuple[str, FileCheck, Parameter, ComparisonResult]]:
        """Compare two netCDF files, according to the configuration in file_check."""
        results = []
        local_error = False
        # For each parameter for this file:
        for parameters in file_check.parameters.values():
            for parameter in parameters:
                local_error = False
                result = ComparisonResult(error=local_error)

                min_ref_value = sys.float_info.max
                max_ref_value = sys.float_info.min

                plot_ref_val = np.array([])
                plot_cmp_val = np.array([])

                left_nc_root = self.open_netcdf_file(left_path, file_check.name)
                right_nc_root = self.open_netcdf_file(right_path, file_check.name)

                matchnumber = 0
                for variable_name in left_nc_root.variables.keys():
                    if re.match(f"^{parameter.name}$", variable_name) is None:
                        continue
                    try:
                        param_new: Parameter = copy.deepcopy(parameter)
                        param_new.name = variable_name
                        logger.debug(f"Checking parameter: {variable_name}")
                        matchnumber = matchnumber + 1
                        left_nc_var = left_nc_root.variables[variable_name]
                        right_nc_var = right_nc_root.variables[variable_name]
                        self.check_for_dimension_equality(left_nc_var, right_nc_var, variable_name)

                        min_ref_value = float(np.min(left_nc_var[:]))
                        max_ref_value = float(np.max(left_nc_var[:]))

                        # http://docs.scipy.org/doc/numpy/reference/generated/numpy.argmax.html
                        if left_nc_var.ndim == 1:
                            result.maxAbsDiff, result.maxAbsDiffCoordinates, result.maxAbsDiffValues = (
                                self.compare_1d_arrays(left_nc_var, right_nc_var)
                            )

                            plot_ref_val = left_nc_var[:]
                            plot_cmp_val = right_nc_var[:]

                        elif left_nc_var.ndim == 2:
                            # 2D array
                            # Search for the variable name which has cf_role 'timeseries_id'.
                            cf_role_time_series_vars = search_times_series_id(left_nc_root)

                            result_2d_array = self.compare_2d_arrays(
                                left_nc_var,
                                right_nc_var,
                                left_nc_root,
                                param_new,
                                variable_name,
                                cf_role_time_series_vars,
                            )

                            result.maxAbsDiff = result_2d_array.max_abs_diff
                            result.maxAbsDiffCoordinates = result_2d_array.max_abs_diff_coordinates
                            result.maxAbsDiffValues = result_2d_array.max_abs_diff_values
                            min_ref_value = result_2d_array.min_ref_value
                            max_ref_value = result_2d_array.max_ref_value
                            row_id = result_2d_array.row_id
                            column_id = result_2d_array.column_id

                            # - If variable name which has cf_role 'timeseries_id' cannot be found: it is more like a
                            #   map-file. Create a 2D plot of the point in time with
                            # - If variable name which has cf_role 'timeseries_id' can be found: it is more like a
                            #   history file, with stations. Plot the time series for the station with the largest
                            #   deviation.
                            if cf_role_time_series_vars.__len__() == 0:
                                observation_type = parameter.name
                                plot_ref_val = left_nc_var[row_id, :]
                                plot_cmp_val = right_nc_var[row_id, :]
                            else:
                                plot_ref_val = left_nc_var[:, column_id]
                                plot_cmp_val = right_nc_var[:, column_id]
                                observation_type = self.get_observation_type(left_nc_var, cf_role_time_series_vars)
                        else:
                            result.maxAbsDiff, result.maxAbsDiffCoordinates, result.maxAbsDiffValues = (
                                self.compare_nd_arrays(left_nc_var, right_nc_var)
                            )

                    except RuntimeError as e:
                        logger.error(str(e))
                        result.error = True

                    except Exception as e:
                        logger.error(f"Could not find parameter: {variable_name}, in file: {file_check.name}")
                        logger.error(str(e))
                        result.error = True

                    if np.ma.is_masked(
                        min_ref_value
                    ):  # if min_ref_value has no value (it is a  _FillValue) then the test is OK (presumed)
                        result.maxAbsDiff = 0.0
                        result.maxAbsDiffValues = 0.0
                        max_ref_value = 0.0
                        min_ref_value = 0.0

                    result.maxRelDiff = self.get_max_rel_diff(result.maxAbsDiff, min_ref_value, max_ref_value)

                    # Now we know the absolute and relative error, we can see whether the tolerance is exceeded (or test is in error).
                    result.isToleranceExceeded(
                        param_new.tolerance_absolute,
                        param_new.tolerance_relative,
                    )

                    if result.result == "NOK":
                        if left_nc_var.ndim == 1:
                            logger.info(f"Plotting of 1d-array not yet supported, variable name: {variable_name}")
                        if left_nc_var.ndim == 2:
                            result.error = self.plot_2d_array(
                                logger,
                                testcase_name,
                                variable_name,
                                plot_ref_val,
                                plot_cmp_val,
                                cf_role_time_series_vars,
                                left_nc_root,
                                left_nc_var,
                                row_id,
                                column_id,
                                right_path,
                                param_new.tolerance_absolute,
                                observation_type,
                            )
                    results.append((testcase_name, file_check, param_new, result))

                self.check_match_for_parameter_name(matchnumber, parameter.name, left_path, file_check.name)
        return results

    def compare_1d_arrays(self, left_nc_var: nc.Variable, right_nc_var: nc.Variable):
        """Compare two 1D arrays datasets and returns the maximum absolute difference."""
        diff_arr = self.get_difference(left_nc_var, right_nc_var)
        i_max = np.argmax(diff_arr)
        max_abs_diff = float(diff_arr[i_max])
        max_abs_diff_coordinates = (i_max,)
        max_abs_diff_values = (left_nc_var[i_max], right_nc_var[i_max])
        return max_abs_diff, max_abs_diff_coordinates, max_abs_diff_values

    def compare_2d_arrays(
        self,
        left_nc_var: nc.Variable,
        right_nc_var: nc.Variable,
        left_nc_root: nc.Dataset,
        param_new: Parameter,
        variable_name: str,
        cf_role_time_series_vars: List[str],
    ) -> Comparison2DArrayResult:
        """Compare two 2D arrays datasets and returns the comparison result."""
        result = Comparison2DArrayResult()

        diff_arr = self.get_difference(left_nc_var, right_nc_var)

        column_id, row_id = self.get_column_and_row_id(
            param_new.location, cf_role_time_series_vars, left_nc_root, diff_arr, variable_name
        )

        # This overrides the default min/max of all ref values.
        result.min_ref_value = np.min(left_nc_var[:, column_id])
        result.max_ref_value = np.max(left_nc_var[:, column_id])

        result.max_abs_diff = diff_arr[row_id, column_id]
        result.max_abs_diff_coordinates = (row_id, column_id)
        result.max_abs_diff_values = (
            left_nc_var[row_id, column_id],
            right_nc_var[row_id, column_id],
        )
        result.row_id = row_id
        result.column_id = column_id
        return result

    def get_difference(self, left_nc_var: nc.Variable, right_nc_var: nc.Variable) -> nc.ndarray:
        """Calculate the absolute difference between two NetCDF variables."""
        return np.abs(left_nc_var[:] - right_nc_var[:])

    def compare_nd_arrays(self, left_nc_var: nc.Variable, right_nc_var: nc.Variable):
        """
        Compare two n-dimensional arrays and find the maximum absolute difference.

        This method computes the absolute differences between corresponding elements
        of two n-dimensional arrays, identifies the maximum absolute difference, and
        returns the value of this difference along with its coordinates and the values
        at these coordinates in the original arrays.
        """
        diff_arr = self.get_difference(left_nc_var, right_nc_var)
        i_max = np.argmax(diff_arr)
        print(type(i_max))

        block_sizes = self.get_block_sizes(diff_arr)
        coordinates = self.get_coordinates_of_max_deviation(i_max, block_sizes)

        maxdiff = diff_arr
        left_at_maxdiff = left_nc_var
        right_at_maxdiff = right_nc_var
        try:
            for c in coordinates:
                maxdiff = maxdiff[c]
                left_at_maxdiff = left_at_maxdiff[c]
                right_at_maxdiff = right_at_maxdiff[c]
        except Exception as e:
            error_msg = (
                "Mismatch dimensions: len maxdiff and coordinates: " + str(len(maxdiff)) + " , " + str(len(coordinates))
            )
            raise RuntimeError(error_msg, e)

        max_abs_diff = float(maxdiff)
        max_abs_diff_coordinates = tuple(coordinates)
        max_abs_diff_values = (
            left_at_maxdiff,
            right_at_maxdiff,
        )
        return max_abs_diff, max_abs_diff_coordinates, max_abs_diff_values

    def plot_2d(
        self,
        testcase_name: str,
        variable_name: str,
        plot_ref_val: np.ndarray,
        plot_cmp_val: np.ndarray,
        right_path: str,
        left_nc_root: nc.Dataset,
        left_nc_var: nc.Variable,
        tolerance_absolute: float,
        subtitle: str,
    ) -> None:
        """Plot a 2D graph."""
        # search coordinates
        coords = left_nc_var.coordinates.split()
        x_coords = left_nc_root.variables[coords[0]][:]
        y_coords = left_nc_root.variables[coords[1]][:]

        plot.PlotDifferencesMap(
            right_path,
            x_coords,
            y_coords,
            plot_ref_val,
            plot_cmp_val,
            tolerance_absolute,
            testcase_name,
            variable_name,
            subtitle,
            "netcdf",
        )

    def plot_2d_array(
        self,
        logger: ILogger,
        testcase_name: str,
        variable_name: str,
        plot_ref_val: np.ndarray,
        plot_cmp_val: np.ndarray,
        cf_role_time_series_vars: List[str],
        left_nc_root: nc.Dataset,
        left_nc_var: nc.Variable,
        row_id: int,
        column_id: int,
        right_path: str,
        tolerance_absolute: float,
        observation_type: str,
    ) -> bool:
        """Plot a 2D array or time series based on the provided parameters."""
        try:
            time_var = search_time_variable(left_nc_root, variable_name)
            if cf_role_time_series_vars.__len__() == 0:
                subtitle = self.get_plot_subtitle(time_var, row_id)

                self.plot_2d(
                    testcase_name,
                    variable_name,
                    plot_ref_val,
                    plot_cmp_val,
                    right_path,
                    left_nc_root,
                    left_nc_var,
                    tolerance_absolute,
                    subtitle,
                )
            else:
                plot_location = self.determine_plot_location(left_nc_root, observation_type, column_id)
                self.plot_time_series(
                    testcase_name,
                    variable_name,
                    plot_ref_val,
                    plot_cmp_val,
                    right_path,
                    time_var,
                    plot_location,
                )

        except Exception as e:
            logger.error(f"Plotting of parameter {variable_name} failed")
            logger.error(str(e))
            return True
        return False

    def plot_time_series(
        self,
        testcase_name: str,
        variable_name: str,
        plot_ref_val: np.ndarray,
        plot_cmp_val: np.ndarray,
        right_path: str,
        time_var: nc.Dataset,
        plot_location: str,
    ) -> None:
        """Plot a time series graph."""
        unit_txt = "".join(time_var.units).strip()
        start_datetime, delta = interpret_time_unit(unit_txt)
        datetime_series = [start_datetime + int(t_i) * delta for t_i in time_var[:]]

        plot.PlotDifferencesTimeSeries(
            right_path,
            datetime_series,
            plot_ref_val,
            plot_cmp_val,
            testcase_name,
            variable_name,
            plot_location,
            "netcdf",
        )

    def determine_plot_location(self, left_nc_root: nc.Dataset, observation_type: str, column_id: int) -> str:
        """Determine location name, needed when no location is specified otherwise it is equal to parameter_location."""
        plot_location = left_nc_root.variables[observation_type][column_id][:]
        plot_location = b"".join(filter(None, plot_location)).decode("utf-8").strip()
        if plot_location == "":
            plot_location = "model_wide"
        return str(plot_location)

    def get_plot_subtitle(self, time_var: nc.Variable, row_id: int) -> str:
        """Compute datetime for which we are making a plot / scalar field."""
        unit_txt = "".join(time_var.units).strip()
        start_datetime, delta = interpret_time_unit(unit_txt)

        plot_datetime = start_datetime + delta * int(time_var[row_id])

        subtitle = datetime.strftime(plot_datetime, "%Y%m%d_%H%M%S")
        return subtitle

    def get_observation_type(self, left_nc_var: nc.Variable, cf_role_time_series_vars: List[str]) -> str:
        """Determine the observation type based on the coordinates attribute of the NetCDF variable."""
        if hasattr(left_nc_var, "coordinates"):
            location_types = left_nc_var.coordinates.split(" ")
            for variable in cf_role_time_series_vars:
                for location_type in location_types:
                    if location_type == variable:
                        return str(location_type)
        return str(cf_role_time_series_vars[0])

    def get_max_rel_diff(self, max_abs_diff: float, min_ref_value: float, max_ref_value: float) -> float:
        """
        Calculate the maximum relative difference.

        This method converts an absolute difference into a relative difference by dividing it by the difference between
        the maximum and minimum reference values. It handles edge cases where the differences are very small.
        """
        # Make the absolute difference in maxDiff relative, by dividing by (max_ref_value-min_ref_value).
        if max_abs_diff < 2 * sys.float_info.epsilon:
            # No difference found, so relative difference is set to 0.
            return 0.0
        elif max_ref_value - min_ref_value < 2 * sys.float_info.epsilon:
            # Very small difference found, so the denominator will be very small, so set relative difference to maximum.
            return 1.0
        else:
            return min(1.0, max_abs_diff / (max_ref_value - min_ref_value))

    def get_column_and_row_id(
        self,
        parameter_location: str,
        cf_role_time_series_vars: List[str],
        left_nc_root: nc.Dataset,
        diff_arr: np.ndarray,
        variable_name: str,
    ) -> Tuple[int, int]:
        """Find the column and row ID based on the given parameter location and difference array."""
        parameter_location_found = False
        column_id = 0
        row_id = 0

        if parameter_location != "":
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
            row_id = int(i_max / diff_arr.shape[1])  # diff_arr.shape = (nrows, ncolumns)

        return column_id, row_id

    def get_coordinates_of_max_deviation(self, i_max: np.int64, block_sizes: list) -> list:
        """Calculate the coordinates of the maximum deviation in a multi-dimensional array."""
        coordinates = []
        remainder = i_max
        for size in block_sizes:
            coordinates.append(remainder // size)
            remainder %= size
        return coordinates

    def get_block_sizes(self, diff_arr: np.ndarray) -> list:
        """Calculate block sizes based on the shape of the input array."""
        block_sizes = [1]
        for n in reversed(diff_arr.shape):
            block_sizes.append(block_sizes[-1] * n)
        block_sizes.pop()  # Last block size is irrelevant.
        block_sizes.reverse()
        return block_sizes

    def open_netcdf_file(self, path: str, filename: str) -> nc.Dataset:
        """Open NetCDF file and return netCDF dataset."""
        try:
            nc_root = nc.Dataset(os.path.join(path, filename), "r", format="NETCDF4_CLASSIC")
        except Exception as e:
            error_msg = f"Cannot open netcdf file {os.path.join(path, filename)}"
            raise RuntimeError(error_msg, e) from e
        return nc_root

    def check_match_for_parameter_name(
        self, matchnumber: int, parameter_name: str, left_path: str, filename: str
    ) -> None:
        """Ceck if a valid matchnumber is found, otherwise raise exception."""
        if matchnumber == 0:
            error_msg = f"No match for parameter name {parameter_name} in file {os.path.join(left_path, filename)}"
            raise Exception(error_msg)

    def check_for_dimension_equality(
        self, left_nc_var: nc.Variable, right_nc_var: nc.Variable, variable_name: str
    ) -> None:
        """Check dimension equility and raises exception if not correct."""
        if left_nc_var.shape != right_nc_var.shape:
            raise Exception(
                f"Shapes of parameter {variable_name} not compatible. Shape of reference: "
                + f"{left_nc_var.shape}. Shape of run data: {right_nc_var.shape}"
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


def interpret_time_unit(time_description):
    """Return a `(start_datetime, timedelta)` tuple.

    For instance, `'seconds since 1998-08-01 00:00:00'` yields the
    following tuple: `(datetime(1998, 8, 1, 0, 0, 0), timedelta(seconds=1))`.
    """
    try:
        words = time_description.lower().strip().split(" ")

        # Deduce timedelta
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

    except Exception:
        raise ValueError(
            "Can not interpret the following unit: "
            + str(time_description)
            + ". A correct example is: 'seconds since 1998-08-01 00:00:00'."
        )

    return start_datetime, delta
