"""Base class for timeseries set comparer.

Copyright (C)  Stichting Deltares, 2024
"""

import copy
import os
import re
import sys
from abc import ABC
from typing import List, Tuple

import numpy as np

import src.utils.plot_differences as plot
from src.config.file_check import FileCheck
from src.config.parameter import Parameter
from src.utils.comparers.comparison_result import ComparisonResult
from src.utils.comparers.end_result import EndResult
from src.utils.comparers.i_comparer import IComparer
from src.utils.logging.i_logger import ILogger


def branch(xmltree):
    newbranch = {}
    for child in xmltree.childNodes:
        tag = child.nodeName
        if tag == "#text":
            if "data" not in newbranch:
                newbranch["data"] = {}
            newbranch["data"]["nodeValue"] = child.nodeValue
        else:
            if tag not in newbranch:
                newbranch[tag] = []
            newsubbranch = branch(child)
            newbranch[tag].append(newsubbranch)
    if xmltree.attributes is not None:
        if "data" not in newbranch:
            newbranch["data"] = {}
        for attrib in xmltree.attributes.items():
            newbranch["data"][attrib[0]] = attrib[1]
    return newbranch


class TimeseriesSetComparer(IComparer, ABC):
    """Time series set comparer.

    Compare two Timeseries files, according to the configuration in file_check.
    input: left path (reference), right path (compare), file_check
    output: list of (file_check, parameter, file_check, ResultComparison) tuples.
    """

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

        filename = file_check.name
        left_TS = self.getTimeseriesSet(os.path.join(left_path, filename))
        right_TS = self.getTimeseriesSet(os.path.join(right_path, filename))

        for parameters in file_check.parameters.values():
            for parameter in parameters:
                local_error = False
                parameter_name = parameter.name
                result = ComparisonResult(error=local_error)

                min_ref_value = sys.float_info.max
                max_ref_value = sys.float_info.min

                plot_ref_val = None
                plot_cmp_val = None

                matchnumber = 0
                for variable_name in left_TS.keys():
                    if (
                        re.match("^" + parameter_name + "$", variable_name) is not None
                        or parameter_name == variable_name
                    ):
                        try:
                            paramnew = copy.deepcopy(parameter)
                            paramnew.name = variable_name
                            logger.debug("Checking parameter: " + str(variable_name))
                            matchnumber = matchnumber + 1

                            parameter_location = paramnew.location
                            left_var = left_TS[variable_name]
                            right_var = right_TS[variable_name]
                            if parameter_location is not None:
                                locationSet = [parameter_location]
                            else:
                                locationSet = left_var.keys()
                            locnr = 0
                            for location in locationSet:
                                if location not in left_var:
                                    raise KeyError(
                                        "Cannot find parameter location "
                                        + location
                                        + " for variable "
                                        + variable_name
                                        + "[LEFT]"
                                    )
                                if location not in right_var:
                                    raise KeyError(
                                        "Cannot find parameter location "
                                        + location
                                        + " for variable "
                                        + variable_name
                                        + "[RIGHT]"
                                    )
                                left_timeseries = left_var[location]
                                right_timeseries = right_var[location]

                                if left_timeseries["dates"] != right_timeseries["dates"]:
                                    pass  # todo: timing is off. Also a way of detecting that both series are not equal in size
                                DiffSeries = left_timeseries["values"] - right_timeseries["values"]
                                MaxDiffTime = np.argmax(abs(DiffSeries))
                                MaxDiff = DiffSeries[MaxDiffTime]
                                if abs(MaxDiff) > abs(result.max_abs_diff):
                                    result.max_abs_diff = MaxDiff
                                    result.max_abs_diff_coordinates = (locnr, MaxDiffTime)
                                    result.max_abs_diff_values = (
                                        left_timeseries["values"][MaxDiffTime],
                                        right_timeseries["values"][MaxDiffTime],
                                    )
                                    left_timeseries_worst = left_timeseries
                                    right_timeseries_worst = right_timeseries

                                    min_ref_value = min(
                                        min_ref_value,
                                        float(np.min(left_timeseries["values"][:])),
                                    )
                                    max_ref_value = max(
                                        max_ref_value,
                                        float(np.max(left_timeseries["values"][:])),
                                    )
                                    plot_location = location
                                    plot_ref_val = left_timeseries["values"][:]
                                    plot_cmp_val = right_timeseries["values"][:]

                        except RuntimeError as e:
                            logger.error(e)
                            local_error = True
                            result.error = True

                        result.max_abs_diff = abs(result.max_abs_diff)  # RL666: Lets make the error absolute
                        # Make the absolute difference in maxDiff relative, by dividing by (max_ref_value-min_ref_value).
                        if result.max_abs_diff < 2 * sys.float_info.epsilon:
                            # No difference found, so relative difference is set to 0.
                            result.max_rel_diff = 0.0
                        elif max_ref_value - min_ref_value < 2 * sys.float_info.epsilon:
                            # Very small difference found, so the denominator will be very small, so set relative difference to maximum.
                            result.max_rel_diff = 1.0
                        else:
                            result.max_rel_diff = min(1.0, result.max_abs_diff / (max_ref_value - min_ref_value))

                        # Now we know the absolute and relative error, we can see whether the tolerance is exceeded (or test is in error).
                        result.is_tolerance_exceeded(
                            paramnew.tolerance_absolute,
                            paramnew.tolerance_relative,
                        )

                        if result.result == EndResult.NOK:
                            datetime_series = left_timeseries_worst["dates"]
                            plot.PlotDifferencesTimeSeries(
                                right_path,
                                datetime_series,
                                plot_ref_val,
                                plot_cmp_val,
                                testcase_name,
                                variable_name,
                                plot_location,
                                "TimeSeries",
                            )

                        results.append((testcase_name, file_check, paramnew, result))
                if matchnumber == 0:
                    error_msg = (
                        "No match for parameter name "
                        + parameter_name
                        + " in file "
                        + os.path.join(left_path, filename)
                    )
                    raise Exception(error_msg)
        return results

    def getTimeseriesSet(self, datafile):
        # .....
        TSSet = {}
        return TSSet
