import re
import statistics
from dataclasses import dataclass
from enum import StrEnum
from typing import ClassVar, TextIO


@dataclass
class SlurmLogData:
    """Contains information scraped from the Slurm log of a model run."""

    PATTERN: ClassVar[re.Pattern] = re.compile(
        r"""
        (:? ^ \s* SLURM_JOB_ID \s* = \s* (?P<slurm_job_id>\d+) \s* $ ) |
        (:? ^ \s* SLURM_NODELIST \s* = \s* (?P<slurm_nodelist>[-,\[\]\w]+) \s* $ ) |
        (:?  # Match line that contains the commit id.
            ^ \s* Dimr \s* \[[^]]*\] \s* [#]0 \s* >> \s* Deltares, \s* DIMR_EXE \s* Version \s*
            \d+[.]\d+[.](?P<commit_id>[0-9a-fA-F]+)
            \s* , .* $
        ) |
        (:? ^ .* total \s+ computation \s+ time \s+ [(]s[)] \s* : \s* (?P<computation_time> \d* [.] \d* ) \s* $ ) |
        (:? ^ \s* [*][*] \s* WARNING \s* : \s* (?P<warning>.*) \s* $ ) |
        (:? ^ \s* [*][*] \s* ERROR \s* : \s* (?P<error>.*) \s* $ )
        """,
        flags=re.VERBOSE,
    )

    commit_id: str | None
    slurm_job_id: str | None
    slurm_nodelist: str | None
    task_count: int
    mean_computation_time: float
    stddev_computation_time: float
    warning_lines: list[tuple[int, str]]
    error_lines: list[tuple[int, str]]

    def is_crash(self) -> bool:
        """Return `True` if the model run crashed.

        Currently we think the model has crashed when either there's
        `ERROR` log lines or if there were no 'total computation time'
        lines found.
        """
        return bool(self.error_lines) or self.task_count == 0

    @classmethod
    def from_slurm_log_lines(cls, lines: TextIO) -> "SlurmLogData":
        """Collect information on the model run from a Slurm log.

        This is a factory method to create an instance of `SlurmLogData` from
        an opened Slurm log file.
        It uses a regular expression to match each line of the log. If a line
        matches the regex, the information in that line is stored in the instance
        variables. Some variables may end up to be empty if the log is truncated
        or if the format of the log is different than expected.

        Parameters
        ----------
        lines : TextIO
            A file opened for reading in 'text-mode'.

        Returns
        -------
        SlurmLogData
            The information found in the log.
        """
        slurm_job_id: str | None = None
        slurm_nodelist: str | None = None
        commit_id: str | None = None
        computation_times: list[float] = []
        warning_lines: list[tuple[int, str]] = []
        error_lines: list[tuple[int, str]] = []

        for linenr, line in enumerate(lines, 1):
            match_obj = cls.PATTERN.match(line)
            if match_obj is None:
                continue

            capture_group = next((item for item in match_obj.groupdict().items() if item[1] is not None), None)
            if not capture_group:
                raise RuntimeError("Found invalid match object. Check `PATTERN` because it shouldn't be possible.")
            group_id, value = capture_group
            match group_id:
                case "slurm_job_id":
                    slurm_job_id = value
                case "slurm_nodelist":
                    slurm_nodelist = value
                case "commit_id":
                    commit_id = value
                case "computation_time":
                    computation_times.append(float(value))
                case "warning":
                    warning_lines.append((linenr, value))
                case "error":
                    error_lines.append((linenr, value))

        return SlurmLogData(
            slurm_job_id=slurm_job_id,
            slurm_nodelist=slurm_nodelist,
            commit_id=commit_id,
            task_count=len(computation_times),
            mean_computation_time=statistics.mean(computation_times) if computation_times else 0.0,
            stddev_computation_time=statistics.stdev(computation_times) if len(computation_times) >= 2 else 0.0,
            warning_lines=warning_lines,
            error_lines=error_lines,
        )


class Status(StrEnum):
    """Indicates the status of a model run, or a comparison."""

    ERROR = "❌"
    WARNING = "⚠️"
    SUCCESS = "✅"


@dataclass
class LogComparison:
    """Contains the `SlurmLogData` of the current and reference run of a single model.

    The `reference` `SlurmLogData` can be `None` if the reference logs cannot be found.
    """

    current: SlurmLogData
    reference: SlurmLogData | None

    def get_comparison_status(self) -> tuple[Status, Status, Status]:
        """Get the status of the comparison, current run and reference run.

        Each `Status` can be either `ERROR`, `WARNING` or `SUCCESS`.
        This function returns three statuses: The status of the 'comparison',
        the status of the 'current' model run and the 'reference' model run.
        The 'comparison' can only succeed if neither the current nor the
        reference runs have failed (or if the reference run is missing).

        Returns
        -------
        tuple[Status, Status, Status]
            The status of the 'comparison', the 'current' run and the 'reference' run.
        """
        current = self.current
        reference = self.reference

        if current.is_crash():
            comp_status = cur_status = Status.ERROR
            if reference is None:
                ref_status = Status.WARNING
            elif reference.is_crash():
                ref_status = Status.ERROR
            else:
                ref_status = Status.SUCCESS
        elif reference is None:
            comp_status, cur_status, ref_status = (Status.WARNING, Status.SUCCESS, Status.WARNING)
        elif reference.is_crash():
            comp_status, cur_status, ref_status = (Status.WARNING, Status.SUCCESS, Status.ERROR)
        else:
            comp_status = cur_status = ref_status = Status.SUCCESS

        return (comp_status, cur_status, ref_status)

    def speedup(self) -> float | None:
        """Return the speedup of the current model run compared to the reference."""
        if self.reference is None or self.reference.is_crash():
            return None
        if self.current.is_crash() or self.current.mean_computation_time == 0:
            return None

        return self.reference.mean_computation_time / self.current.mean_computation_time

    def computation_time_difference(self) -> float | None:
        """Return the time difference from the reference to the current model."""
        if self.reference is None or self.reference.is_crash():
            return None
        if self.current.is_crash():
            return None

        return self.reference.mean_computation_time - self.current.mean_computation_time
