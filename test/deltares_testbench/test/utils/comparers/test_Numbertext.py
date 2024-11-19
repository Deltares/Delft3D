#  NUMBERTEXT file comparer
#
#  Copyright (C)  Stichting Deltares, 2024


import os

import pytest

from src.config.file_check import FileCheck
from src.config.parameter import Parameter
from src.config.types.file_type import FileType
from src.utils.comparers.number_text_comparer import NumberTextComparer
from test.utils.test_logger import TestLogger


def is_nan(number: float) -> bool:
    return number != number


class TestNumberTextComparer:
    def setup_method(self) -> None:
        self.testroot = os.path.abspath(os.path.dirname(__file__))
        self.testdata = os.path.join(self.testroot, "data")
        self.lp = os.path.join(self.testdata, "left")
        self.rp = os.path.join(self.testdata, "right")
        self.filename = "ComplexProjects01.txt"

    def test_identical(self) -> None:
        fc = FileCheck()
        pm = Parameter()
        pm.tolerance_absolute = 0.0001
        pm.tolerance_relative = 0.01
        fc.name = "Unit_test.fod"
        fc.type = FileType.NUMBERTEXT
        logger = TestLogger()
        results = NumberTextComparer().compare(self.lp, self.rp, fc, "test", logger)
        result_structure = results[0][3]

        # perform a set of asserts on the result structure
        assert result_structure.passed
        assert not result_structure.error
        assert result_structure.result == "OK"
        assert result_structure.max_abs_diff == 0.0
        assert result_structure.max_abs_diff_coordinates == ()
        assert pytest.approx(result_structure.max_rel_diff) == 0.0

    def test_number_differences(self) -> None:
        fc = FileCheck()
        pm = Parameter()
        pm.tolerance_absolute = 0.0001
        pm.tolerance_relative = 0.01
        fc.name = "ComplexProjects01.txt"
        fc.type = FileType.NUMBERTEXT
        logger = TestLogger()
        results = NumberTextComparer().compare(self.lp, self.rp, fc, "test", logger)
        result_structure = results[0][3]

        # perform a set of asserts on the result structure
        assert not result_structure.passed
        assert not result_structure.error
        assert result_structure.result == "NOK"
        assert result_structure.max_abs_diff == 100.0
        assert result_structure.max_abs_diff_coordinates == (51,)
        assert pytest.approx(result_structure.max_rel_diff) == 0.004644202966531087

    def test_comma_difference(self) -> None:
        fc = FileCheck()
        pm = Parameter()
        pm.tolerance_absolute = 0.0001
        pm.tolerance_relative = 0.01
        fc.name = "Tutorial-1c.log"
        fc.type = FileType.NUMBERTEXT
        logger = TestLogger()
        results = NumberTextComparer().compare(self.lp, self.rp, fc, "test", logger)
        result_structure = results[0][3]

        # perform a set of asserts on the result structure
        assert not result_structure.passed
        assert not result_structure.error
        assert result_structure.result == "NOK"
        assert is_nan(result_structure.max_abs_diff)
        assert result_structure.max_abs_diff_coordinates == (4, 4)
        assert pytest.approx(result_structure.max_rel_diff) == 1.0
