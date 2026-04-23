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


def test_find_weekly_runs__multiple_runs(mocker: MockerFixture) -> None:
    # Arrange
    now = datetime.now(timezone.utc)
    minio = mocker.Mock(spec=Minio)
    minio.list_objects.return_value = itertools.chain.from_iterable(
        [  # Return three runs. All with three models: foo, bar and baz.
            make_obj(f"output/weekly/weekly-tag-{i}/output/foo.zip", last_modified=now - timedelta(days=i)),
            make_obj(f"output/weekly/weekly-tag-{i}/output/bar.zip", last_modified=now - timedelta(days=i)),
            make_obj(f"output/weekly/weekly-tag-{i}/output/baz.zip", last_modified=now - timedelta(days=i)),
            make_obj(f"output/weekly/weekly-tag-{i}/logs/logs.zip", last_modified=now - timedelta(days=i)),
        ]
        for i in (1, 2, 3)
    )
    reporter = VerschilAnalyseReporter(minio, "my-bucket", "output/weekly")

    # Act
    runs = reporter.find_weekly_runs()

    # Assert
    assert len(runs) == 3
    for i, run in enumerate(sorted(runs, key=lambda run: run.tag), 1):
        assert run.tag == f"weekly-tag-{i}"
        assert sorted(run.model_names) == ["bar", "baz", "foo"]
        assert run.timestamp == now - timedelta(days=i)


def test_find_weekly_runs__skip_reports_without_report_zip(mocker: MockerFixture) -> None:
    # Arrange
    now = datetime.now(timezone.utc)
    minio = mocker.Mock(spec=Minio)
    minio.list_objects.return_value = [  # Only weekly-tag-2 has the report.zip
        make_obj("output/weekly/weekly-tag-1/output/foo.zip", last_modified=now - timedelta(days=1)),
        make_obj("output/weekly/weekly-tag-2/output/bar.zip", last_modified=now - timedelta(days=2)),
        make_obj("output/weekly/weekly-tag-2/logs/logs.zip", last_modified=now - timedelta(days=2)),
        make_obj("output/weekly/weekly-tag-3/output/baz.zip", last_modified=now - timedelta(days=3)),
    ]
    reporter = VerschilAnalyseReporter(minio, "my-bucket", "output/weekly")

    # Act
    [run, *other_runs] = reporter.find_weekly_runs()

    # Assert
    assert not other_runs
    assert run.tag == "weekly-tag-2"
    assert run.model_names == ["bar"]
    assert run.timestamp == now - timedelta(days=2)


def test_find_weekly_runs__skip_reports_without_models(mocker: MockerFixture) -> None:
    # Arrange
    now = datetime.now(timezone.utc)
    minio = mocker.Mock(spec=Minio)
    minio.list_objects.return_value = [  # Only weekly-tag-2 has a model.zip
        make_obj("output/weekly/weekly-tag-1/logs/logs.zip", last_modified=now - timedelta(days=1)),
        make_obj("output/weekly/weekly-tag-2/output/foo.zip", last_modified=now - timedelta(days=2)),
        make_obj("output/weekly/weekly-tag-2/logs/logs.zip", last_modified=now - timedelta(days=2)),
        make_obj("output/weekly/weekly-tag-3/logs/logs.zip", last_modified=now - timedelta(days=3)),
    ]
    reporter = VerschilAnalyseReporter(minio, "my-bucket", "output/weekly")

    # Act
    [run, *other_runs] = reporter.find_weekly_runs()

    # Assert
    assert not other_runs
    assert run.tag == "weekly-tag-2"
    assert run.model_names == ["foo"]
    assert run.timestamp == now - timedelta(days=2)


def test_get_latest_run__multiple_reports__get_last_modified_run(mocker: MockerFixture) -> None:
    # Arrange
    now = datetime.now(timezone.utc)
    minio = mocker.Mock(spec=Minio)
    minio.list_objects.return_value = itertools.chain.from_iterable(
        [  # Return three runs. All with three models: foo, bar and baz.
            make_obj(f"output/weekly/weekly-tag-{i}/output/foo.zip", last_modified=now - timedelta(days=i)),
            make_obj(f"output/weekly/weekly-tag-{i}/output/bar.zip", last_modified=now - timedelta(days=i)),
            make_obj(f"output/weekly/weekly-tag-{i}/output/baz.zip", last_modified=now - timedelta(days=i)),
            make_obj(f"output/weekly/weekly-tag-{i}/logs/logs.zip", last_modified=now - timedelta(days=i)),
        ]
        for i in (1, 2, 3)
    )
    reporter = VerschilAnalyseReporter(minio, "my-bucket", "output/weekly")

    # Act
    run = reporter.get_latest_run()

    # Assert
    assert run is not None
    assert run.tag == "weekly-tag-1"
    assert run.timestamp == now - timedelta(days=1)


def test_get_latest_run__no_reports_found__return_none(mocker: MockerFixture) -> None:
    # Arrange
    minio = mocker.Mock(spec=Minio)
    minio.list_objects.return_value = [
        make_obj("output/weekly/some/unrelated/file.txt"),
        make_obj("output/weekly/weekly-tag-1/README.txt"),
    ]
    reporter = VerschilAnalyseReporter(minio, "my-bucket", "output/weekly")

    # Act
    run = reporter.get_latest_run()

    # Assert
    assert run is None
