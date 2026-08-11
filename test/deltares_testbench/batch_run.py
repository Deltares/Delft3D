import csv
from argparse import ArgumentParser, Namespace
from pathlib import Path

from src.config.credentials import Credentials
from src.config.test_case_config import TestCaseConfig
from src.suite.command_line_settings import CommandLineSettings
from src.utils.handlers.credential_handler import CredentialHandler
from src.utils.logging.log_level import LogLevel
from src.utils.logging.logger import Logger
from src.utils.xml_config_parser import XmlConfigParser


class BatchRunArgs:
    """Structure for batch run arguments."""

    def __init__(self):
        self.test_name: str = ""
        self.test_name_list: str = ""
        self.ci_csv: str = ""
        self.configs_root: str = ""
        self.cmd: str = ""


class TestWithConfig:
    """Structure for test with its configuration."""

    def __init__(self, test: TestCaseConfig, config_path: str):
        self.test: TestCaseConfig = test
        self.config_path: str = config_path


class Params:
    """Structure for parameters."""

    def __init__(self):
        self.test_name: str = ""
        self.test_path: Path | None = None
        self.test_config: str = ""


def parse_batch_run_arguments() -> BatchRunArgs:
    """Parse command line arguments for batch run.

    Returns
    -------
        BatchRunArgs: Parsed arguments structure
    """
    parser = ArgumentParser(description="Batch run utility for test execution")

    parser.add_argument(
        "--test-name",
        default="",
        help="Single test name to execute",
        dest="test_name",
    )
    parser.add_argument(
        "--test-name-list",
        default="",
        help="List of test names to execute",
        dest="test_name_list",
    )
    parser.add_argument(
        "--ci-csv",
        default="../../ci/teamcity/Delft3D/vars/dimr_testbench_table.csv",
        help="Path to CI CSV file",
        dest="ci_csv",
    )
    parser.add_argument(
        "--configs-root",
        default="configs",
        help="Path to configs root directory",
        dest="configs_root",
    )
    parser.add_argument(
        "--cmd",
        default="python TestBench.py --compare --config $test_config --filter testcase=$testname",
        help="Command to execute",
        dest="cmd",
    )

    args: Namespace = parser.parse_args()

    result = BatchRunArgs()
    result.test_name = args.test_name
    result.test_name_list = args.test_name_list
    result.ci_csv = args.ci_csv
    result.configs_root = args.configs_root
    result.cmd = args.cmd

    return result


def find_test_config(test_name: str, args: BatchRunArgs, platform: str = "lnx64") -> TestWithConfig:
    """Find the test configuration for a given test name and platform.

    Parameters
    ----------
    test_name : str
        The name of the test to find.
    platform : str, optional
        The platform to filter by (default is "lnx64").

    Returns
    -------
        TestCaseConfig: The matching test case configuration.

    Raises
    ------
        ValueError: If no matching test case configuration is found.
    """
    with open(args.ci_csv, "r", encoding="utf-8") as csvfile:
        reader = csv.DictReader(csvfile)
        for row in reader:
            config_path = row.get("#config", "").strip()
            if config_path.find(platform) != -1:
                settings = CommandLineSettings()
                credentials = Credentials()
                credentials.name = "commandline"
                credential_handler = CredentialHandler(credentials=credentials, log_level=LogLevel.INFO)
                credential_handler.setup_credentials(False)
                settings.credentials = credentials
                full_config_path = Path(args.configs_root) / config_path
                settings.config_file = str(full_config_path)
                xml_config = XmlConfigParser().load(settings, logger)

                for test_case in xml_config.testcase_configs:
                    if test_case.name == test_name:
                        return TestWithConfig(test_case, str(full_config_path))

    raise ValueError(f"No matching test case configuration found for {test_name} on {platform}.")


if __name__ == "__main__":
    args = parse_batch_run_arguments()
    print(
        f"Parsed arguments: test_name={args.test_name}, test_name_list={args.test_name_list}, ci_csv={args.ci_csv}, configs_root={args.configs_root}"
    )
    test_names = []
    if args.test_name_list:
        with open(args.test_name_list, "r") as f:
            test_names = [line.strip() for line in f if line.strip()]

    elif args.test_name:
        test_names = [args.test_name]

    print(f"Tests to process: {test_names}")
    logger = Logger(LogLevel.INFO, False)  # Adjust log level and teamcity flag as neededs

    for test_name in test_names:
        try:
            test_case_config = find_test_config(test_name, args, platform="lnx64")
            params = Params()
            params.test_name = test_case_config.test.name
            if test_case_config.test.path:
                params.test_path = Path("data") / "cates" / test_case_config.test.path.path

            if not params.test_path:
                print(
                    f"Warning: Test case {test_case_config.test.name} does not have a valid path specified. Path: {params.test_path}"
                )
                continue

            params.test_config = test_case_config.config_path

            print(f"Batch Params {params}")
        except ValueError as e:
            print(e)
