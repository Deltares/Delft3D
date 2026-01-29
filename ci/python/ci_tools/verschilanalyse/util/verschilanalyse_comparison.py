import logging
from dataclasses import dataclass
from pathlib import Path

import openpyxl

from ci_tools.verschilanalyse.util.slurm_log_data import LogComparison, SlurmLogData
from ci_tools.verschilanalyse.util.verschillentool import OutputType, VerschillentoolOutput


@dataclass
class VerschilanalyseComparison:
    """Contains all of the information gathered from the weekly verschilanalyse files."""

    s3_current_prefix: str
    s3_reference_prefix: str

    current_log_data: dict[str, SlurmLogData]
    reference_log_data: dict[str, SlurmLogData]

    his_outputs: dict[str, VerschillentoolOutput]
    map_outputs: dict[str, VerschillentoolOutput]

    def get_log_comparisons(self) -> dict[str, LogComparison]:
        """Compare the `SlurmLogData` from the current and reference verschilanalyse for each model.

        The result should contain a `LogComparison` for each model run in the
        "current" verschilanalyse. The "reference" verschilanalyse may or may not
        include the same model run. If the model was included in the current
        verschilanalyse, but not in the reference verschilanalyse, then the
        reference `SlurmLogData` is `None`.

        Returns
        -------
        dict[str, LogComparison]
        """
        return {
            model_name: LogComparison(current, self.reference_log_data.get(model_name))
            for model_name, current in self.current_log_data.items()
        }

    @classmethod
    def from_report_directories(
        cls,
        current_log_dir: Path,
        reference_log_dir: Path,
        verschillen_dir: Path,
        s3_current_prefix: str,
        s3_reference_prefix: str,
    ) -> "VerschilanalyseComparison":
        """Make a `VerschilanalyseComparison` from the logs and verschillentool output.

        The `log_dir`s and `verschillen_dir` directories should contain
        the Slurm logs of the model runs and the output files of the
        verschillentool. This factory reads all of the information from
        these files and constructs a `VerschilanalyseComparison` object.

        Returns
        -------
        VerschilanalyseComparison
            An object containing all of the information gathered in the
            Slurm logs and the verschillentool output files.
        """
        current_logs = cls._get_all_log_data(current_log_dir / "models")
        reference_logs = cls._get_all_log_data(reference_log_dir / "models")
        his_stats = cls._get_verschillentool_output(verschillen_dir, OutputType.HIS)
        map_stats = cls._get_verschillentool_output(verschillen_dir, OutputType.MAP)

        return VerschilanalyseComparison(
            s3_current_prefix=s3_current_prefix.rstrip("/"),
            s3_reference_prefix=s3_reference_prefix.rstrip("/"),
            current_log_data=current_logs,
            reference_log_data=reference_logs,
            his_outputs=his_stats,
            map_outputs=map_stats,
        )

    @staticmethod
    def _get_all_log_data(log_dir: Path) -> dict[str, SlurmLogData]:
        result: dict[str, SlurmLogData] = {}
        for path in log_dir.glob("*.out"):
            key = path.name.removesuffix(".out")
            with path.open("r") as stream:
                result[key] = SlurmLogData.from_slurm_log_lines(stream)
        if not result:
            logging.warning("No logs found in %s", log_dir)
        return result

    @staticmethod
    def _get_verschillentool_output(verschillen_dir: Path, output_type: OutputType) -> dict[str, VerschillentoolOutput]:
        result: dict[str, VerschillentoolOutput] = {}
        for path in verschillen_dir.rglob(f"{output_type.value}_output.xlsx"):
            key = path.parent.name.removeprefix("verschil_")
            with path.open("rb") as stream:
                workbook = openpyxl.load_workbook(stream)
                result[key] = VerschillentoolOutput.from_verschillentool_workbook(workbook, output_type)
        if not result:
            logging.warning("No %s file statistics found in %s", output_type.value, verschillen_dir)
        return result
