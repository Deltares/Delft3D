import pytest

from ci_tools.minio.progress_bar import ProgressBar
from tests.helpers import minio as helper


class TestProgressBar:
    def test_display__overwrite_bar_after_each_update(self, capsys: pytest.CaptureFixture) -> None:
        # Arrange
        progress = helper.make_progress_bar()

        # Act
        progress.display()
        progress.add_size(42)
        progress.display()
        progress.add_size(42)
        progress.display()

        # Assert
        stdout = capsys.readouterr().out
        empty, *bars = stdout.split("\r")
        assert empty == "", "The scrollbar must start at the beginning of a line"
        assert all(bar.startswith("Progress: [") for bar in bars)
        assert all(not bar.endswith("\n") for bar in bars), "No newlines should be printed to stdout"
        assert len(bars) == 3, "Three progress bars should be printed"

    def test_display__do_not_show_bar_when_show_progress_bar_is_false(self, capsys: pytest.CaptureFixture) -> None:
        # Arrange
        progress = ProgressBar(
            total_size=1000,
            object_count=10,
            show_progress_bar=False,
            terminal_size=helper.ConstTerminalSize(24, 80),
        )

        # Act
        progress.display()
        progress.add_size(42)
        progress.display()
        progress.add_size(42)
        progress.display()

        # Assert
        stdout = capsys.readouterr().out
        assert stdout == "", "No output should be printed to stdout when show_progress_bar is False"

    def test_display__show_download_speed_size_and_object_count(self, capsys: pytest.CaptureFixture) -> None:
        # Arrange
        clock = helper.ManualClock()
        progress = helper.make_progress_bar(total_size=42 * 1024**2, object_count=3, clock=clock)

        # Act: Download 21 MiB in 0.5 seconds, and complete 2 objects.
        clock.add(helper.timedelta(milliseconds=500))
        progress.add_size(21 * 1024**2)
        progress.add_object_count(2)
        progress.display()

        # Assert
        stdout = capsys.readouterr().out
        assert "42.000 MiB/s" in stdout
        assert "21.000/42.000 MiB" in stdout
        assert "2/3" in stdout

    @pytest.mark.parametrize(
        ("total_size", "displayed_unit"),
        [
            pytest.param(42, "B", id="bytes"),
            pytest.param(42 * 1024, "KiB", id="kilobytes"),
            pytest.param(42 * 1024**2, "MiB", id="megabytes"),
            pytest.param(42 * 1024**3, "GiB", id="gigabytes"),
            pytest.param(42 * 1024**4, "TiB", id="terabytes"),
        ],
    )
    def test_display__show_correct_units(
        self, total_size: int, displayed_unit: str, capsys: pytest.CaptureFixture
    ) -> None:
        # Arrange
        progress = helper.make_progress_bar(total_size=total_size)

        # Act: Download half of the total size.
        progress.add_size(total_size // 2)
        progress.display()

        # Assert
        stdout = capsys.readouterr().out
        assert f"21.000/42.000 {displayed_unit}" in stdout

    @pytest.mark.parametrize("percentage", range(0, 101, 20))
    def test_display__correct_bar_length(self, percentage: int, capsys: pytest.CaptureFixture) -> None:
        # Arrange
        progress = helper.make_progress_bar(total_size=100, object_count=1)

        # Act
        progress.add_size(percentage)
        progress.display()

        # Assert
        stdout = capsys.readouterr().out
        bar = stdout.rsplit("]", 1)[0].split("[", 1)[-1]
        expected_length = int(len(bar) * percentage / 100.0)
        assert bar == (expected_length * "*").ljust(len(bar)), f"Bar should be {percentage}% filled with '*'s"

    def test_print__bar_is_not_displayed__print_line(self, capsys: pytest.CaptureFixture) -> None:
        # Arrange
        progress = helper.make_progress_bar()

        # Act
        progress.print("Hello world!")

        # Assert
        stdout = capsys.readouterr().out
        assert stdout == "Hello world!\n", "The printed message should be displayed without the progress bar"

    def test_print__bar_is_displayed__print_line_and_then_redisplay_bar(self, capsys: pytest.CaptureFixture) -> None:
        # Arrange
        width = 80
        progress = helper.make_progress_bar(terminal_size=helper.ConstTerminalSize(24, width))

        # Act
        progress.display()
        progress.print("Hello world!")

        # Assert
        stdout = capsys.readouterr().out
        empty, bar_before, printed_message, bar_after = stdout.split("\r")
        assert empty == "", "The scrollbar must start at the beginning of a line"
        assert bar_before.startswith("Progress: [")
        assert not bar_before.endswith("\n")
        assert printed_message == "Hello world!".ljust(width) + "\n"
        assert bar_after.startswith("Progress: [")
        assert not bar_after.endswith("\n")

    def test_is_complete__not_complete(self) -> None:
        # Arrange
        progress = helper.make_progress_bar(total_size=100, object_count=10)

        # Act
        progress.add_size(50)
        progress.add_object_count(5)

        # Assert
        assert not progress.is_complete(), "Progress should not be complete"

    def test_is_complete__complete(self) -> None:
        # Arrange
        progress = helper.make_progress_bar(total_size=100, object_count=10)

        # Act
        progress.add_size(100)
        progress.add_object_count(10)

        # Assert
        assert progress.is_complete(), "Progress should be complete"

    def test_close__bar_is_displayed__newline(self, capsys: pytest.CaptureFixture) -> None:
        # Arrange
        progress = helper.make_progress_bar()

        # Act
        progress.display()
        progress.close()

        # Assert
        stdout = capsys.readouterr().out
        assert stdout.endswith("\n"), "A newline should be printed after closing the progress bar"

    def test_close__bar_is_not_displayed__no_newline(self, capsys: pytest.CaptureFixture) -> None:
        # Arrange
        progress = helper.make_progress_bar()

        # Act
        progress.close()

        # Assert
        stdout = capsys.readouterr().out
        assert stdout == "", "No newlines should be printed to stdout when the bar is not displayed"
