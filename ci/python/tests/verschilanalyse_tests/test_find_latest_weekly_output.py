import itertools
from datetime import datetime, timedelta, timezone

from minio import Minio
from minio.datatypes import Object as MinioObject
from pytest_mock import MockerFixture

from ci_tools.verschilanalyse.find_latest_weekly_output import VerschilAnalyseReporter


def make_obj(
    key: str,
    bucket_name: str = "my-bucket",
    last_modified: datetime | None = None,
) -> MinioObject:
    """Make Minio objects with convenient defaults."""
    last_modified = last_modified or datetime.now(timezone.utc)
    return MinioObject(
        bucket_name=bucket_name,
        object_name=key,
        last_modified=last_modified,
    )


def test_find_weekly_reports__multiple_reports(mocker: MockerFixture) -> None:
    # Arrange
    now = datetime.now(timezone.utc)
    minio = mocker.Mock(spec=Minio)
    minio.list_objects.return_value = itertools.chain.from_iterable(
        [  # Return three reports. All with three models: foo, bar and baz.
            make_obj(f"output/weekly/weekly-tag-{i}/output/foo.zip", last_modified=now - timedelta(days=i)),
            make_obj(f"output/weekly/weekly-tag-{i}/output/bar.zip", last_modified=now - timedelta(days=i)),
            make_obj(f"output/weekly/weekly-tag-{i}/output/baz.zip", last_modified=now - timedelta(days=i)),
            make_obj(f"output/weekly/weekly-tag-{i}/report/report.zip", last_modified=now - timedelta(days=i)),
        ]
        for i in (1, 2, 3)
    )
    reporter = VerschilAnalyseReporter(minio, "my-bucket", "output/weekly")

    # Act
    reports = reporter.find_weekly_reports()

    # Assert
    assert len(reports) == 3
    for i, report in enumerate(sorted(reports, key=lambda report: report.tag), 1):
        assert report.tag == f"weekly-tag-{i}"
        assert sorted(report.model_names) == ["bar", "baz", "foo"]
        assert report.timestamp == now - timedelta(days=i)


def test_find_weekly_reports__skip_reports_without_report_zip(mocker: MockerFixture) -> None:
    # Arrange
    now = datetime.now(timezone.utc)
    minio = mocker.Mock(spec=Minio)
    minio.list_objects.return_value = [  # Only weekly-tag-2 has the report.zip
        make_obj("output/weekly/weekly-tag-1/output/foo.zip", last_modified=now - timedelta(days=1)),
        make_obj("output/weekly/weekly-tag-2/output/bar.zip", last_modified=now - timedelta(days=2)),
        make_obj("output/weekly/weekly-tag-2/report/report.zip", last_modified=now - timedelta(days=2)),
        make_obj("output/weekly/weekly-tag-3/output/baz.zip", last_modified=now - timedelta(days=3)),
    ]
    reporter = VerschilAnalyseReporter(minio, "my-bucket", "output/weekly")

    # Act
    [report, *other_reports] = reporter.find_weekly_reports()

    # Assert
    assert not other_reports
    assert report.tag == "weekly-tag-2"
    assert report.model_names == ["bar"]
    assert report.timestamp == now - timedelta(days=2)


def test_find_weekly_reports__skip_reports_without_models(mocker: MockerFixture) -> None:
    # Arrange
    now = datetime.now(timezone.utc)
    minio = mocker.Mock(spec=Minio)
    minio.list_objects.return_value = [  # Only weekly-tag-2 has a model.zip
        make_obj("output/weekly/weekly-tag-1/report/report.zip", last_modified=now - timedelta(days=1)),
        make_obj("output/weekly/weekly-tag-2/output/foo.zip", last_modified=now - timedelta(days=2)),
        make_obj("output/weekly/weekly-tag-2/report/report.zip", last_modified=now - timedelta(days=2)),
        make_obj("output/weekly/weekly-tag-3/report/report.zip", last_modified=now - timedelta(days=3)),
    ]
    reporter = VerschilAnalyseReporter(minio, "my-bucket", "output/weekly")

    # Act
    [report, *other_reports] = reporter.find_weekly_reports()

    # Assert
    assert not other_reports
    assert report.tag == "weekly-tag-2"
    assert report.model_names == ["foo"]
    assert report.timestamp == now - timedelta(days=2)


def test_get_latest_report__multiple_reports__get_last_modified_report(mocker: MockerFixture) -> None:
    # Arrange
    now = datetime.now(timezone.utc)
    minio = mocker.Mock(spec=Minio)
    minio.list_objects.return_value = itertools.chain.from_iterable(
        [  # Return three reports. All with three models: foo, bar and baz.
            make_obj(f"output/weekly/weekly-tag-{i}/output/foo.zip", last_modified=now - timedelta(days=i)),
            make_obj(f"output/weekly/weekly-tag-{i}/output/bar.zip", last_modified=now - timedelta(days=i)),
            make_obj(f"output/weekly/weekly-tag-{i}/output/baz.zip", last_modified=now - timedelta(days=i)),
            make_obj(f"output/weekly/weekly-tag-{i}/report/report.zip", last_modified=now - timedelta(days=i)),
        ]
        for i in (1, 2, 3)
    )
    reporter = VerschilAnalyseReporter(minio, "my-bucket", "output/weekly")

    # Act
    report = reporter.get_latest_report()

    # Assert
    assert report is not None
    assert report.tag == "weekly-tag-1"
    assert report.timestamp == now - timedelta(days=1)


def test_get_latest_report__ignore_tags_without_weekly(mocker: MockerFixture) -> None:
    # Arrange
    now = datetime.now(timezone.utc)
    yesterday = now - timedelta(days=1)
    minio = mocker.Mock(spec=Minio)
    minio.list_objects.return_value = [
        make_obj("output/weekly/weekly-tag-1/output/foo.zip", last_modified=yesterday),
        make_obj("output/weekly/weekly-tag-1/report/report.zip", last_modified=yesterday),
        make_obj("output/weekly/daily-tag-4/report/report.zip", last_modified=now),
        make_obj("output/weekly/daily-tag-4/output/foo.zip", last_modified=now),
    ]
    reporter = VerschilAnalyseReporter(minio, "my-bucket", "output/weekly")

    # Act
    report = reporter.get_latest_report()

    # Assert
    assert report is not None
    assert report.tag == "weekly-tag-1"
    assert report.timestamp == now - timedelta(days=1)


def test_get_latest_report__no_reports_found__return_none(mocker: MockerFixture) -> None:
    # Arrange
    minio = mocker.Mock(spec=Minio)
    minio.list_objects.return_value = [
        make_obj("output/weekly/some/unrelated/file.txt"),
        make_obj("output/weekly/weekly-tag-1/README.txt"),
    ]
    reporter = VerschilAnalyseReporter(minio, "my-bucket", "output/weekly")

    # Act
    report = reporter.get_latest_report()

    # Assert
    assert report is None
