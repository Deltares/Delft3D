import io
import textwrap

import pytest

from ci_tools.verschilanalyse.util.slurm_log_data import LogComparison, SlurmLogData
from tests.helpers import verschilanalyse as helper


class TestSlurmLogData:
    def test_is_crash__false(self) -> None:
        log_data = helper.make_log_data(task_count=42, error_lines=[])
        assert not log_data.is_crash(), "Expected is_crash to return `False`"

    def test_is_crash__error_lines(self) -> None:
        log_data = helper.make_log_data(task_count=42, error_lines=[(42, "** ERROR: Explosion")])
        assert log_data.is_crash(), "Expected is_crash to return `True`"

    def test_is_crash__no_tasks(self) -> None:
        log_data = helper.make_log_data(task_count=0, error_lines=[])
        assert log_data.is_crash(), "Expected is_crash to return `True`"

    def test_from_slurm_log_lines__slurm_job_id(self) -> None:
        lines = "SLURM_JOB_ID = 314159"
        result = SlurmLogData.from_slurm_log_lines(io.StringIO(lines))
        assert result.slurm_job_id == "314159"

    def test_from_slurm_log_lines__slurm_node_list(self) -> None:
        lines = "SLURM_NODELIST = v-lcf16vcpu[0000-0041,0043]"
        result = SlurmLogData.from_slurm_log_lines(io.StringIO(lines))
        assert result.slurm_nodelist == "v-lcf16vcpu[0000-0041,0043]"

    def test_from_slurm_log_lines__commit_id(self) -> None:
        lines = "Dimr [1970-01-01 00:00:00] #0 >> Deltares, DIMR_EXE Version 3.00.decafc0ffeebad, Jan 1 1970, 00:00:00"
        result = SlurmLogData.from_slurm_log_lines(io.StringIO(lines))
        assert result.commit_id == "decafc0ffeebad"

    def test_from_slurm_log_lines__computation_time(self) -> None:
        lines = "** INFO : total computation time (s) : 3.14159"
        result = SlurmLogData.from_slurm_log_lines(io.StringIO(lines))
        assert result.mean_computation_time == 3.14159

    def test_from_slurm_log_lines__warning(self) -> None:
        lines = "** WARNING : mind the gap"
        result = SlurmLogData.from_slurm_log_lines(io.StringIO(lines))
        assert result.warning_lines == [(1, "mind the gap")]

    def test_from_slurm_log_lines__error(self) -> None:
        lines = "** ERROR : Oh no! You fell in the gap!"
        result = SlurmLogData.from_slurm_log_lines(io.StringIO(lines))
        assert result.error_lines == [(1, "Oh no! You fell in the gap!")]

    def test_from_slurm_log_lines__computation_time_statistics(self) -> None:
        lines = """
            ** INFO : total computation time (s) : 41.0
            ** INFO : total computation time (d) : 0.0
            ** INFO : total computation time (s) : 43.0
        """  # The second line is skipped because "(d)" does not match.

        result = SlurmLogData.from_slurm_log_lines(io.StringIO(lines))

        assert result.task_count == 2
        assert abs(result.mean_computation_time - 42.0) < 1e-9
        assert abs(result.stddev_computation_time**2 - 2.0) < 1e-9

    def test_from_slurm_log_lines__match_all_rules(self) -> None:
        lines = textwrap.dedent("""
            SLURM_JOB_ID = 314159
            SLURM_NODELIST = v-lcf16vcpu[0000-0041,0043]
            Dimr [1970-01-01 00:00:00] #0 >> Deltares, DIMR_EXE Version 3.00.decafc0ffeebad, Jan 1 1970, 00:00:00
            ** WARNING : mind the gap
            ** ERROR : Oh no! You fell in the gap!
            ** INFO : total computation time (s) : 3.14159
        """).strip()

        result = SlurmLogData.from_slurm_log_lines(io.StringIO(lines))

        assert result == SlurmLogData(
            commit_id="decafc0ffeebad",
            slurm_job_id="314159",
            slurm_nodelist="v-lcf16vcpu[0000-0041,0043]",
            task_count=1,
            mean_computation_time=3.14159,
            stddev_computation_time=0.0,
            warning_lines=[(4, "mind the gap")],
            error_lines=[(5, "Oh no! You fell in the gap!")],
        )

    def test_from_slurm_log_lines__match_no_rules(self) -> None:
        lines = textwrap.dedent("""
            SLURM_JERB_ID = 314159
            SLURM_NODELISP = v-lcf16vcpu[0000-0041,0043]
            Bimr [1970-01-01 00:00:00] #0 >> Deltares, DIMR_EYE Version 3.00.decafc0ffeebad, Jan 1 1970, 00:00:00
            ** DARLING : mind the gap
            ** ERORR : Oh no! You fell in the gap!
            ** INFO : total commutation time (s) : 3.14159
        """).strip()

        result = SlurmLogData.from_slurm_log_lines(io.StringIO(lines))

        assert result == SlurmLogData(
            commit_id=None,
            slurm_job_id=None,
            slurm_nodelist=None,
            task_count=0,
            mean_computation_time=0.0,
            stddev_computation_time=0.0,
            warning_lines=[],
            error_lines=[],
        )


class TestLogComparison:
    @pytest.mark.parametrize(
        ("current", "reference", "expected"),
        [
            pytest.param(
                helper.make_log_data(task_count=0), helper.make_log_data(task_count=0), "❌❌❌", id="both_fail"
            ),
            pytest.param(
                helper.make_log_data(task_count=0), helper.make_log_data(), "❌❌✅", id="cur_fail_ref_success"
            ),
            pytest.param(helper.make_log_data(task_count=0), None, "❌❌⚠️", id="cur_fail_ref_missing"),
            pytest.param(helper.make_log_data(), None, "⚠️✅⚠️", id="cur_success_ref_missing"),
            pytest.param(
                helper.make_log_data(), helper.make_log_data(task_count=0), "⚠️✅❌", id="cur_success_ref_fail"
            ),
            pytest.param(helper.make_log_data(), helper.make_log_data(), "✅✅✅", id="both_success"),
        ],
    )
    def test_get_comparison_status__reference_missing(
        self, current: SlurmLogData, reference: SlurmLogData | None, expected: str
    ) -> None:
        comparison = LogComparison(current=current, reference=reference)
        result = comparison.get_comparison_status()
        assert "".join(result) == expected

    @pytest.mark.parametrize(
        ("current", "reference", "expected"),
        [
            pytest.param(helper.make_log_data(task_count=0), helper.make_log_data(), None, id="cur_fail"),
            pytest.param(helper.make_log_data(), helper.make_log_data(task_count=0), None, id="ref_fail"),
            pytest.param(helper.make_log_data(), None, None, id="ref_missing"),
            pytest.param(
                helper.make_log_data(mean_computation_time=0.0), helper.make_log_data(), None, id="avoid_div_by_zero"
            ),
            pytest.param(
                helper.make_log_data(mean_computation_time=42.0),
                helper.make_log_data(mean_computation_time=84.0),
                2.0,
                id="speedup",
            ),
        ],
    )
    def test_speedup(self, current: SlurmLogData, reference: SlurmLogData | None, expected: float | None) -> None:
        comparison = LogComparison(current=current, reference=reference)
        assert comparison.speedup() == expected

    @pytest.mark.parametrize(
        ("current", "reference", "expected"),
        [
            pytest.param(helper.make_log_data(task_count=0), helper.make_log_data(), None, id="cur_fail"),
            pytest.param(helper.make_log_data(), helper.make_log_data(task_count=0), None, id="ref_fail"),
            pytest.param(helper.make_log_data(), None, None, id="ref_missing"),
            pytest.param(
                helper.make_log_data(mean_computation_time=42.0),
                helper.make_log_data(mean_computation_time=84.0),
                42.0,
                id="difference",
            ),
        ],
    )
    def test_time_difference(
        self, current: SlurmLogData, reference: SlurmLogData | None, expected: float | None
    ) -> None:
        comparison = LogComparison(current=current, reference=reference)
        assert comparison.computation_time_difference() == expected
