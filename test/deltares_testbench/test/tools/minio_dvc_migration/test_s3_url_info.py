"""Unit tests for S3 utility functions."""

from pathlib import Path

import pytest

from tools.minio_dvc_migration.s3_url_info import S3UrlInfo


@pytest.mark.parametrize(
    ("s3_path", "expected"),
    [
        ("cases/doc", "data/cases/doc"),
        ("cases/e01_d3dflow/doc", "data/cases/e01_d3dflow/doc"),
        ("cases/e01_d3dflow/f50_validation_analytical/doc", "data/cases/e01_d3dflow/f50_validation_analytical/doc"),
    ],
)
def test_doc_to_local_path(s3_path: str, expected: str) -> None:
    # Arrange
    s3_info = S3UrlInfo(hostname="example.com", bucket="test-bucket", path=s3_path)

    # Act
    result = s3_info.to_local()

    # Assert
    assert result == Path(expected)


@pytest.mark.parametrize(
    ("s3_path", "expected"),
    [
        ("cases/e01_d3dflow/f01_general/c02-fff", "data/cases/e01_d3dflow/f01_general/c02-fff/input"),
        (
            "cases/e01_d3dflow/f50_validation_analytical/c31153_Simple-channel-flow(FP)_Chezy_3D_K10_non-eq3_ke_BCadapt",
            "data/cases/e01_d3dflow/f50_validation_analytical/c31153_Simple-channel-flow(FP)_Chezy_3D_K10_non-eq3_ke_BCadapt/input",
        ),
        (
            "references/lnx64/e01_d3dflow/f01_general/c02-fff",
            "data/cases/e01_d3dflow/f01_general/c02-fff/reference_lnx64",
        ),
        (
            "references/win64/e01_d3dflow/f01_general/c02-fff",
            "data/cases/e01_d3dflow/f01_general/c02-fff/reference_win64",
        ),
    ],
)
def test_case_and_ref_to_local_path(s3_path: str, expected: str) -> None:
    # Arrange
    s3_info = S3UrlInfo(hostname="example.com", bucket="test-bucket", path=s3_path)

    # Act
    result = s3_info.to_local()

    # Assert
    assert result == Path(expected)


@pytest.mark.parametrize(
    ("s3_path", "expected"),
    [
        ("references/win64/e500_FloodAdapt", "data/cases/e500_FloodAdapt/reference_win64"),
        ("references/lnx64/e999_minio_tool/c01_test_tool", "data/cases/e999_minio_tool/c01_test_tool/reference_lnx64"),
        ("cases/e09_RTC-Tools2/TeamCityScripts", "data/cases/e09_RTC-Tools2/TeamCityScripts"),
        (
            "cases/e100_dflowfm-dwaves/f02_delft3d_cases/matlabTools",
            "data/cases/e100_dflowfm-dwaves/f02_delft3d_cases/matlabTools",
        ),
        ("cases/e110_delft3dfm_suite/scripts", "data/cases/e110_delft3dfm_suite/scripts"),
        (
            "cases/e110_delft3dfm_suite/f05_verschilanalyse/models",
            "data/cases/e110_delft3dfm_suite/f05_verschilanalyse/models",
        ),
        ("cases/e32_openmi14_sobek3/test_models", "data/cases/e32_openmi14_sobek3/test_models"),
        ("cases/e400_iMOD5/Dbase_shared", "data/cases/e400_iMOD5/Dbase_shared"),
        (
            "cases/e700_smartnumerics/1d_non_linear_waves/delft3d-flow",
            "data/cases/e700_smartnumerics/1d_non_linear_waves/delft3d-flow",
        ),
        (
            "cases/e99_matlab_tools/f01_quickplot/Delft3D-PART - HIS - Nefis - parn",
            "data/cases/e99_matlab_tools/f01_quickplot/Delft3D-PART - HIS - Nefis - parn",
        ),
        ("cases/e07_sobek/f63_sobek2_import/Test_197", "data/cases/e07_sobek/f63_sobek2_import/Test_197"),
    ],
)
def test_deviating_folders_to_local_path(s3_path: str, expected: str) -> None:
    # Arrange
    s3_info = S3UrlInfo(hostname="example.com", bucket="test-bucket", path=s3_path)

    # Act
    result = s3_info.to_local()

    # Assert
    assert result == Path(expected)


@pytest.mark.parametrize(
    ("hostname", "bucket", "path", "expected"),
    [
        ("https://example.com/base", "bucket", "cases/e01", "https://example.com/base/bucket/cases/e01"),
        (
            "https://example.com/base",
            "bucket",
            "cases/e99_matlab_tools/f01_quickplot/Delft3D-PART - HIS - Nefis - parn",
            "https://example.com/base/bucket/cases/e99_matlab_tools/f01_quickplot/Delft3D-PART - HIS - Nefis - parn",
        ),
        ("https://example.com", "bucket", "", "https://example.com/bucket"),
        ("https://example.com/base?x=1", "bucket", "cases/e01", "https://example.com/base?x=1/bucket/cases/e01"),
        ("http://example.com", "bucket", "", "http://example.com/bucket"),
        ("s3://example.com", "bucket", "", "s3://example.com/bucket"),
        ("s3://example.com/", "bucket", "", "s3://example.com/bucket"),
        ("s3://example.com/", "/bucket", "", "s3://example.com/bucket"),
        ("s3://example.com/", "/bucket/", "", "s3://example.com/bucket"),
        ("localhost:9000", "bucket", "", "localhost:9000/bucket"),
        ("localhost:9000/base", "bucket", "cases/e01", "localhost:9000/base/bucket/cases/e01"),
        ("example.com", "bucket", "cases/e01/item", "example.com/bucket/cases/e01/item"),
        ("example.com", "bucket", "/cases/e01/item", "example.com/bucket/cases/e01/item"),
        ("example.com", "bucket", "cases//e01//item", "example.com/bucket/cases/e01/item"),
        ("example.com", "", "cases/e01/item", "example.com/cases/e01/item"),
    ],
)
def test_to_url_returns_url_string(hostname: str, bucket: str, path: str, expected: str) -> None:
    # Arrange
    s3_info = S3UrlInfo(hostname=hostname, bucket=bucket, path=path)

    # Act
    result = s3_info.to_url()

    # Assert
    assert result == expected
