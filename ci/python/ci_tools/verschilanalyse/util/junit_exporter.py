from junit_xml import TestCase, TestSuite

from ci_tools.verschilanalyse.util.slurm_log_data import SlurmLogData


class JUnitExporter:
    """Write jUnit xml report for the verschilanalyse reporting."""

    @classmethod
    def _make_test_case(cls, model_name: str, log_data: SlurmLogData) -> TestCase:
        test_case = TestCase(
            name=model_name,
            elapsed_sec=round(log_data.mean_computation_time, ndigits=3),
            stdout="\n".join(
                [
                    f"Commit ID: {log_data.commit_id}",
                    f"Hostname: {log_data.slurm_nodelist}",
                    f"Slurm job ID: {log_data.slurm_job_id}",
                    f"Task count: {log_data.task_count}",
                    f"WARNING lines: {len(log_data.warning_lines)}",
                    f"ERROR lines: {len(log_data.error_lines)}",
                ]
            ),
        )
        if log_data.is_crash():
            message = "The model run crashed. Either there were errors written to the log, or the log is truncated."
            test_case.add_error_info(message)
            if log_data.error_lines:
                indent = len(str(log_data.error_lines[-1][0]))
                test_case.stderr = "\n".join(
                    f"{str(line_nr).rjust(indent)}: {message}" for line_nr, message in log_data.error_lines
                )

        return test_case

    @classmethod
    def make_test_suite(cls, suite_name: str, logs_url: str, logs: dict[str, SlurmLogData]) -> TestSuite:
        """Make a `TestSuite` object from the `SlurmLogData` objects found for each model run.

        Parameters
        ----------
        suite_name : str
        logs_url : str
            The URL of the logs archive containing the logs used to generate the test suite.
        logs : dict[str, SlurmLogData]
            A map from model names to `SlurmLogData` instances, each containing the information
            found in the logs of a single model run.
        """
        test_cases = [cls._make_test_case(name, log_data) for name, log_data in sorted(logs.items())]
        return TestSuite(
            name=suite_name,
            url=logs_url,
            test_cases=test_cases,
        )
