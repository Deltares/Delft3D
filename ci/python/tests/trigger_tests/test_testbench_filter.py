from typing import Dict, List

import pytest
from pyfakefs.fake_filesystem import FakeFilesystem

from ci_tools.trigger.testbench_filter import csv_to_dict, filter_config


# Sample CSV data for testing
@pytest.fixture
def csv_data() -> str:
    csv_data = """#name,#config,all,none,fm,rr,#comment
fm 1d parallel,dimr_dflowfm_1D_lnx64_parallel.xml,TRUE,FALSE,TRUE,FALSE,
fm 1d,dimr_dflowfm_1d_lnx64.xml,TRUE,FALSE,TRUE,FALSE,hello world
fm drr,dimr_dflowfm_drr_lnx64.xml,TRUE,FALSE,TRUE,TRUE,
"""
    return csv_data


@pytest.fixture
def csv_dict() -> Dict[str, List[str]]:
    csv_dict = {
        "#name": ["fm 1d parallel", "fm 1d", "fm drr"],
        "#config": [
            "dimr_dflowfm_1D_lnx64_parallel.xml",
            "dimr_dflowfm_1d_lnx64.xml",
            "dimr_dflowfm_drr_lnx64.xml",
        ],
        "all": ["TRUE", "TRUE", "TRUE"],
        "none": ["FALSE", "FALSE", "FALSE"],
        "fm": ["TRUE", "TRUE", "TRUE"],
        "rr": ["FALSE", "FALSE", "TRUE"],
        "#comment": ["", "hello world", ""],
    }
    return csv_dict


class TestFilterConfig:
    def test_csv_to_dict(self, csv_data: str, csv_dict: Dict[str, List[str]], fs: FakeFilesystem) -> None:
        path = "./csv_file.csv"
        fs.create_file(path, contents=csv_data)
        result = csv_to_dict(path)
        assert result == csv_dict

    @pytest.mark.parametrize(
        ("branch", "expected"),
        [
            ("none", []),
            ("rr", ["dimr_dflowfm_drr_lnx64.xml"]),
            ("all", ["dimr_dflowfm_1D_lnx64_parallel.xml", "dimr_dflowfm_1d_lnx64.xml", "dimr_dflowfm_drr_lnx64.xml"]),
        ],
        ids=["branch-none", "branch-rr", "branch-all"],
    )
    def test_filter_config(self, branch: str, expected: Dict[str, List[str]], csv_dict: Dict[str, List[str]]) -> None:
        result = filter_config("dummy_name.csv", csv_dict, branch)
        assert result == expected

    def test_filter_config_non_existing(self, csv_dict: Dict[str, List[str]]) -> None:
        branch_name = "banana"
        csv_table_path = "dummy_name.csv"
        with pytest.raises(ValueError, match=f"Branch name '{branch_name}' does not exist in file {csv_table_path}."):
            filter_config(csv_table_path, csv_dict, branch_name)
