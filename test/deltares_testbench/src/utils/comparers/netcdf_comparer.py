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

import src.utils.plot_differences as plot
from src.config.file_check import FileCheck
from src.config.parameter import Parameter
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
        results = []
        local_error = False
        # For each parameter for this file:
        for parameters in file_check.parameters.values():
            for parameter in parameters:
                local_error = False
                parameter_name = parameter.name
                result = ComparisonResult(error=local_error)

                min_ref_value = sys.float_info.max
                max_ref_value = sys.float_info.min

                plot_ref_val = None
                plot_cmp_val = None

                filename = file_check.name
                try:
                    left_nc_root = nc.Dataset(os.path.join(left_path, filename), "r", format="NETCDF4_CLASSIC")
                except Exception as e:
                    error_msg = f"Cannot open reference file {os.path.join(left_path, filename)}"
                    raise RuntimeError(error_msg, e)
                try:
                    right_nc_root = nc.Dataset(
                        os.path.join(right_path, filename),
                        "r",
                        format="NETCDF4_CLASSIC",
                    )
                except Exception as e:
                    error_msg = f"Cannot open tested file {os.path.join(right_path, filename)}"
                    raise RuntimeError(error_msg, e)

                matchnumber = 0
                for variable_name in left_nc_root.variables.keys():
                    if re.match(f"^{parameter_name}$", variable_name) is not None:
                        try:
                            param_new: Parameter = copy.deepcopy(parameter)
                            param_new.name = variable_name
                            logger.debug(f"Checking parameter: {variable_name}")
                            matchnumber = matchnumber + 1
                            left_nc_var = left_nc_root.variables[variable_name]
                            right_nc_var = right_nc_root.variables[variable_name]

                            # Check for dimension equality.
                            if left_nc_var.shape != right_nc_var.shape:
                                raise Exception(
                                    f"Shapes of parameter {variable_name} not compatible. Shape of reference: "
                                    + f"{left_nc_var.shape}. Shape of run data: {right_nc_var.shape}"
                                )

                            min_ref_value = float(np.min(left_nc_var[:]))
                            max_ref_value = float(np.max(left_nc_var[:]))

                            # http://docs.scipy.org/doc/numpy/reference/generated/numpy.argmax.html
                            if left_nc_var.ndim == 1:
                                # 1D array
                                diff_arr = np.abs(left_nc_var[:] - right_nc_var[:])
                                i_max = np.argmax(diff_arr)
                                result.maxAbsDiff = float(diff_arr[i_max])
                                result.maxAbsDiffCoordinates = (i_max,)
                                result.maxAbsDiffValues = (
                                    left_nc_var[i_max],
                                    right_nc_var[i_max],
                                )

                                plot_location = str(variable_name)
                                plot_ref_val = left_nc_var[:]
                                plot_cmp_val = right_nc_var[:]

                            elif left_nc_var.ndim == 2:
                                # 2D array
                                diff_arr = np.abs(left_nc_var[:] - right_nc_var[:])

                                parameter_location = param_new.location
                                # Search for the variable name which has cf_role 'timeseries_id'.
                                # - If it can be found: it is more like a history file, with stations. Plot the time series for the station with the largest deviation.
                                # - If it cannot be found: it is more like a map-file. Create a 2D plot of the point in time with
                                cf_role_time_series_vars = search_times_series_id(left_nc_root)
                                location_types = "empty"
                                if cf_role_time_series_vars.__len__() > 0:
                                    observation_type = cf_role_time_series_vars[0]
                                else:
                                    observation_type = parameter_name

                                if hasattr(left_nc_var, "coordinates"):
                                    location_types = left_nc_var.coordinates.split(" ")
                                    for cf_role_time_series_var in cf_role_time_series_vars:
                                        for location_type in location_types:
                                            if location_type == cf_role_time_series_var:
                                                observation_type = (
                                                    location_type  # observation point, cross-section, source_sink, etc
                                                )
                                                break

                                parameter_location_found = False
                                if parameter_location is not None:
                                    for cf_role_time_series_var in cf_role_time_series_vars:
                                        if cf_role_time_series_var is not None:
                                            location_var = left_nc_root.variables[cf_role_time_series_var]
                                            location_var_values = [
                                                b"".join(x).strip().decode("utf-8").strip("\x00")
                                                for x in location_var[:]
                                            ]
                                            if parameter_location in location_var_values:
                                                parameter_location_found = True
                                                column_id = location_var_values.index(parameter_location)
                                        else:
                                            parameter_location_found = True
                                            column_id = 0
                                    if not parameter_location_found:
                                        raise KeyError(
                                            "Cannot find parameter location "
                                            + parameter_location
                                            + " for variable "
                                            + variable_name
                                        )
                                    row_id = np.argmax(diff_arr[:, column_id])
                                else:
                                    # Compare ALL locations.
                                    if cf_role_time_series_vars.__len__() == 0:
                                        cf_role_time_series_var = None
                                    else:
                                        cf_role_time_series_var = "not None"
                                    i_max = np.argmax(diff_arr)
                                    column_id = i_max % diff_arr.shape[1]
                                    row_id = int(i_max / diff_arr.shape[1])  # diff_arr.shape = (nrows, ncolumns)

                                # This overrides the default min/max of all ref values.
                                min_ref_value = np.min(left_nc_var[:, column_id])
                                max_ref_value = np.max(left_nc_var[:, column_id])

                                plot_ref_val = left_nc_var[:, column_id]
                                plot_cmp_val = right_nc_var[:, column_id]

                                result.maxAbsDiff = diff_arr[row_id, column_id]
                                result.maxAbsDiffCoordinates = (row_id, column_id)
                                result.maxAbsDiffValues = (
                                    left_nc_var[row_id, column_id],
                                    right_nc_var[row_id, column_id],
                                )

                            else:
                                diff_arr = np.abs(left_nc_var[:] - right_nc_var[:])
                                i_max = np.argmax(diff_arr)

                                # Determine block sizes
                                block_sizes = [1]
                                for n in reversed(diff_arr.shape):
                                    block_sizes.append(block_sizes[-1] * n)
                                block_sizes.pop()  # Last block size is irrelevant.
                                block_sizes.reverse()

                                # Determine coordinates of maximum deviation
                                coordinates = []
                                remainder = i_max
                                for size in block_sizes:
                                    coordinates.append(remainder // size)
                                    remainder %= size

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
                                        "Mismatch dimensions: len maxdiff and coordinates: "
                                        + str(len(maxdiff))
                                        + " , "
                                        + str(len(coordinates))
                                    )
                                    raise RuntimeError(error_msg, e)

                                result.maxAbsDiff = float(maxdiff)
                                result.maxAbsDiffCoordinates = tuple(coordinates)
                                result.maxAbsDiffValues = (
                                    left_at_maxdiff,
                                    right_at_maxdiff,
                                )

                        except RuntimeError as e:
                            logger.error(e)
                            local_error = True
                            result.error = True

                        except Exception as e:
                            logger.error(f"Could not find parameter: {variable_name}, in file: {filename}")
                            logger.error(e)
                            local_error = True
                            result.error = True

                        if np.ma.is_masked(
                            min_ref_value
                        ):  # if min_ref_value has no value (it is a  _FillValue) then the test is OK (presumed)
                            result.maxAbsDiff = 0.0
                            result.maxAbsDiffValues = 0.0
                            max_ref_value = 0.0
                            min_ref_value = 0.0

                        # Make the absolute difference in maxDiff relative, by dividing by (max_ref_value-min_ref_value).
                        if result.maxAbsDiff < 2 * sys.float_info.epsilon:
                            # No difference found, so relative difference is set to 0.
                            result.maxRelDiff = 0.0
                        elif max_ref_value - min_ref_value < 2 * sys.float_info.epsilon:
                            # Very small difference found, so the denominator will be very small, so set relative difference to maximum.
                            result.maxRelDiff = 1.0
                        else:
                            result.maxRelDiff = min(1.0, result.maxAbsDiff / (max_ref_value - min_ref_value))

                        # Now we know the absolute and relative error, we can see whether the tolerance is exceeded (or test is in error).
                        result.isToleranceExceeded(
                            param_new.tolerance_absolute,
                            param_new.tolerance_relative,
                        )

                        if result.result == "NOK":
                            if left_nc_var.ndim == 1:
                                logger.info(f"Plotting of 1d-array not yet supported, variable name: {variable_name}")
                            if left_nc_var.ndim == 2:
                                try:
                                    # Search for the time variable of this parameter.
                                    time_var = search_time_variable(left_nc_root, variable_name)
                                    if time_var is None:
                                        raise ValueError(
                                            "Can not find the time variable. Plotting of non-time dependent parameters is not supported. Parameter name: '"
                                            + variable_name
                                            + "'."
                                        )

                                    unit_txt = "".join(time_var.units).strip()
                                    start_datetime, delta = interpret_time_unit(unit_txt)

                                    if cf_role_time_series_var is not None:  # Create time-series plot.
                                        unit_txt = "".join(time_var.units).strip()
                                        start_datetime, delta = interpret_time_unit(unit_txt)
                                        datetime_series = [start_datetime + int(t_i) * delta for t_i in time_var[:]]

                                        # determine location name, needed when no location is specified otherwise it is equal to parameter_location
                                        plot_location = left_nc_root.variables[observation_type][column_id][:]
                                        plot_location = b"".join(filter(None, plot_location)).decode("utf-8").strip()
                                        if plot_location == "":
                                            plot_location = "model_wide"

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
                                    elif cf_role_time_series_var is None:  # Create 2D plot.
                                        # Compute datetime for which we are making a plot / scalar field.
                                        plot_datetime = start_datetime + delta * int(time_var[row_id])

                                        plot_ref_val = left_nc_var[row_id, :]
                                        plot_cmp_val = right_nc_var[row_id, :]

                                        # search coordinates
                                        coords = left_nc_var.coordinates.split()
                                        x_coords = left_nc_root.variables[coords[0]][:]
                                        y_coords = left_nc_root.variables[coords[1]][:]

                                        subtitle = datetime.strftime(plot_datetime, "%Y%m%d_%H%M%S")
                                        plot.PlotDifferencesMap(
                                            right_path,
                                            x_coords,
                                            y_coords,
                                            plot_ref_val,
                                            plot_cmp_val,
                                            param_new.tolerance_absolute,
                                            testcase_name,
                                            variable_name,
                                            subtitle,
                                            "netcdf",
                                        )
                                except Exception as e:
                                    logger.error(f"Plotting of parameter {variable_name} failed")
                                    logger.error(e)
                                    local_error = True
                                    result.error = True
                        results.append((testcase_name, file_check, param_new, result))
                if matchnumber == 0:
                    error_msg = (
                        "No match for parameter name "
                        + parameter_name
                        + " in file "
                        + os.path.join(left_path, filename)
                    )
                    raise Exception(error_msg)
            return results


def search_time_variable(nc_root, var_name):
    """Return time dimension or `None`."""
    keywords = ("seconds", "minute", "hour", "days")
    for dim in nc_root.variables[var_name].dimensions:
        if dim in nc_root.variables:
            if any(keyword in nc_root.variables[dim].units for keyword in keywords):
                return nc_root.variables[dim]
    return None


def search_times_series_id(nc_root):
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
