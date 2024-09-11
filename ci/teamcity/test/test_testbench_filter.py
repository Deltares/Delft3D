import pytest
import csv
from io import StringIO

from scripts.testbench_filter import csv_to_dict
from scripts.testbench_filter import filter_config

# Sample CSV data for testing
csv_data = """#name,#config,all,none,fm,rr,#comment
fm 1d parallel,dimr_dflowfm_1D_lnx64_parallel.xml,TRUE,FALSE,TRUE,FALSE,
fm 1d,dimr_dflowfm_1d_lnx64.xml,TRUE,FALSE,TRUE,FALSE,hello world
fm drr,dimr_dflowfm_drr_lnx64.xml,TRUE,FALSE,TRUE,TRUE,
"""


@pytest.fixture
def csv_dict():
    csvfile = StringIO(csv_data)
    reader = csv.reader(csvfile)
    headers = next(reader)
    data_dict = {header: [] for header in headers}
    for row in reader:
        for header, value in zip(headers, row):
            data_dict[header].append(value)
    return data_dict


class TestFilterConfig:
    def test_csv_to_dict(self):
        csvfile = StringIO(csv_data)
        result = csv_to_dict(csvfile)
        expected = {
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
        assert result == expected

    def test_filter_config_main(self, csv_dict):
        result = filter_config("dummy_name.csv", csv_dict, "main")
        expected = [
            "fm 1d parallel=>dimr_dflowfm_1D_lnx64_parallel.xml",
            "fm 1d=>dimr_dflowfm_1d_lnx64.xml",
            "fm drr=>dimr_dflowfm_drr_lnx64.xml",
        ]
        assert result == expected

    def test_filter_config_none(self, csv_dict):
        result = filter_config("dummy_name.csv", csv_dict, "none")
        expected = []
        assert result == expected

    def test_filter_config_rr(self, csv_dict):
        result = filter_config("dummy_name.csv", csv_dict, "rr")
        expected = ["fm drr=>dimr_dflowfm_drr_lnx64.xml"]
        assert result == expected

    def test_filter_config_all(self, csv_dict):
        result = filter_config("dummy_name.csv", csv_dict, "all")
        expected = [
            "fm 1d parallel=>dimr_dflowfm_1D_lnx64_parallel.xml",
            "fm 1d=>dimr_dflowfm_1d_lnx64.xml",
            "fm drr=>dimr_dflowfm_drr_lnx64.xml",
        ]
        assert result == expected

    def test_filter_config_non_existing(self, csv_dict):
        branch_name = "banana"
        csv_table_path = "dummy_name.csv"
        with pytest.raises(ValueError, match=f"Branch name '{branch_name}' does not exist in file {csv_table_path}."):
            filter_config(csv_table_path, csv_dict, branch_name)
        
