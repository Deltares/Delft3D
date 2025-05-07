from junit_xml import TestCase

from ci_tools.verschilanalyse.util.junit_exporter import JUnitExporter
from tests.helpers import verschilanalyse as helper


class TestJUnitExporter:
    def test_make_test_suite__cases_sorted_by_name(self) -> None:
        logs = {model_name: helper.make_log_data() for model_name in ("foo", "bar", "baz")}

        suite = JUnitExporter.make_test_suite(
            suite_name="my-test-suite",
            logs_url="https://foo.bar/report.zip",
            logs=logs,
        )
        cases: list[TestCase] = suite.test_cases

        assert suite.name == "my-test-suite"
        assert suite.url == "https://foo.bar/report.zip"
        assert [case.name for case in cases] == ["bar", "baz", "foo"]

    def test_make_test_suite__errors_in_stderr(self) -> None:
        crash = helper.make_log_data(
            error_lines=[
                (42, "Explosion"),
                (101, "Fire"),
            ],
        )

        suite = JUnitExporter.make_test_suite(
            suite_name="post_mortem",
            logs_url="https://foo.bar/report.zip",
            logs={"crash": crash},
        )
        test_case, *other_cases = suite.test_cases
        error, *other_errors = test_case.errors

        assert not other_cases
        assert not other_errors
        assert test_case.name == "crash"
        assert isinstance(error.get("message"), str)
        assert all(error in str(test_case.stderr) for error in ("Explosion", "Fire"))

    def test_make_test_suite__info_in_stdout(self) -> None:
        success = helper.make_log_data(
            commit_id="3.14.badcode15bad",
            slurm_job_id="424242",
            slurm_nodelist="v-node42cpu0042",
            task_count=42,
            mean_computation_time=3.14,
            warning_lines=[(42, "mind the gap")],
        )

        suite = JUnitExporter.make_test_suite(
            suite_name="successful_suite",
            logs_url="https://foo.bar/report.zip",
            logs={"success": success},
        )
        test_case, *other_cases = suite.test_cases

        assert not other_cases
        assert test_case.name == "success"
        assert abs(test_case.elapsed_sec - 3.14) < 1e-9
        assert all(  # Check if the log info appears in the stdout.
            data in test_case.stdout
            for data in [
                "3.14.badcode15bad",
                "424242",
                "v-node42cpu0042",
            ]
        )
