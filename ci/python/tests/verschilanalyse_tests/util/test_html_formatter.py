import io
import itertools
import textwrap
from dataclasses import dataclass
from html.parser import HTMLParser
from typing import Iterator

import pytest

from ci_tools.verschilanalyse.util.html_formatter import HtmlFormatter
from ci_tools.verschilanalyse.util.verschillentool import OutputType, Variable
from tests.helpers import verschilanalyse as helper


@dataclass
class Section:
    tag: str
    line_range: tuple[int, int]

    def get_lines(self, html_page: str) -> Iterator[str]:
        """Get the lines in the HTML page corresponding to a section."""
        line_iter = io.StringIO(html_page)
        return itertools.islice(line_iter, *self.line_range)

    def get_text(self, html_page: str) -> str:
        """Get the section in the HTML page as a string."""
        lines = self.get_lines(html_page)
        return textwrap.dedent("".join(lines)).strip()


class SummaryPageParser(HTMLParser):
    """Helper class to find sections in the generated html page."""

    def __init__(self) -> None:
        super().__init__()
        self.sections: dict[str, Section] = {}
        self._starting_lines: dict[str, int] = {}
        self._tag_stack: list[tuple[str, str | None]] = []
        self._current_section: str | None = None

    def handle_starttag(self, tag: str, attrs: list[tuple[str, str | None]]) -> None:
        section_id = next((val for key, val in attrs if key == "id" and val is not None), None)
        if section_id is not None:
            self._starting_lines[section_id] = self.getpos()[0] - 1
            self._current_section = section_id
        self._tag_stack.append((tag, section_id))

    def handle_endtag(self, tag: str) -> None:
        start_tag, section_id = self._tag_stack.pop()
        if tag != start_tag:
            raise ValueError(f"Invalid HTML: Expected closing tag {start_tag} but got {tag}")
        if section_id is not None:
            starting_line = self._starting_lines[section_id]
            current_line = self.getpos()[0]
            self.sections[section_id] = Section(tag=tag, line_range=(starting_line, current_line))

            last_section_id = next((id_ for _, id_ in reversed(self._tag_stack) if id_ is not None), None)
            self._current_section = last_section_id


def test_make_summary_page__find_all_sections_and_check_tags() -> None:
    # Arrange
    verschilanalyse_comparison = helper.make_verschilanalyse_comparison()

    # Act
    html_page = HtmlFormatter.make_summary_page(
        verschilanalyse=verschilanalyse_comparison,
        report_build_url="https://foo.bar/report",
        artifact_base_url="https://foo.bar/artifacts",
    )
    parser = SummaryPageParser()
    parser.feed(html_page)
    sections = parser.sections

    # Assert only that the expected sections are present and have the correct HTML tag.
    section_tags = {name: sec.tag for name, sec in sections.items()}
    assert section_tags == {
        "model-run-table": "table",
        "commit-id-list": "ul",
        "prefix-list": "ul",
        "his-water-level-tolerance-list": "ul",
        "his-flow-velocity-tolerance-list": "ul",
        "map-water-level-tolerance-list": "ul",
        "map-flow-velocity-tolerance-list": "ul",
        "links": "ul",
    }


def test_make_summary_page__prefix() -> None:
    # Arrange
    current_prefix = "s3://my-bucket/output/weekly/latest"
    reference_prefix = "s3://my-bucket/output/weekly/last-week"

    # Act
    html_page = HtmlFormatter.make_summary_page(
        verschilanalyse=helper.make_verschilanalyse_comparison(
            s3_current_prefix=current_prefix,
            s3_reference_prefix=reference_prefix,
        ),
        report_build_url="https://foo.bar/report",
        artifact_base_url="https://foo.bar/artifacts",
    )
    parser = SummaryPageParser()
    parser.feed(html_page)
    links = parser.sections["prefix-list"].get_text(html_page)

    # Assert
    assert current_prefix in links
    assert reference_prefix in links


def test_make_summary_page__dimr_version() -> None:
    # Arrange
    current_version = "decade0fbadcoffee"
    reference_version = "deadbeefc0decafe"
    verschilanalyse_comparison = helper.make_verschilanalyse_comparison(
        current_log_data={"foo": helper.make_log_data(commit_id=current_version)},
        reference_log_data={"foo": helper.make_log_data(commit_id=reference_version)},
    )

    # Act
    html_page = HtmlFormatter.make_summary_page(
        verschilanalyse=verschilanalyse_comparison,
        report_build_url="https://foo.bar/report",
        artifact_base_url="https://foo.bar/artifacts",
    )
    parser = SummaryPageParser()
    parser.feed(html_page)
    dimr_version_list = parser.sections["commit-id-list"].get_text(html_page)

    # Assert
    assert current_version in dimr_version_list
    assert reference_version in dimr_version_list


def test_make_summary_page__model_run_table() -> None:
    # Arrange
    current_log_data = {
        "foo": helper.make_log_data(error_lines=[(42, "Explosion")]),
        "bar": helper.make_log_data(),
    }
    reference_log_data = {"foo": helper.make_log_data(), "baz": helper.make_log_data()}
    verschilanalyse_comparison = helper.make_verschilanalyse_comparison(
        current_log_data=current_log_data, reference_log_data=reference_log_data
    )

    # Act
    html_page = HtmlFormatter.make_summary_page(
        verschilanalyse=verschilanalyse_comparison,
        report_build_url="https://foo.bar/report",
        artifact_base_url="https://foo.bar/artifacts",
    )
    parser = SummaryPageParser()
    parser.feed(html_page)
    table_lines = list(parser.sections["model-run-table"].get_lines(html_page))

    foo_line = next((line for line in table_lines if "foo" in line), "")
    bar_line = next((line for line in table_lines if "bar" in line), "")
    baz_line = next((line for line in table_lines if "baz" in line), "")

    # Assert
    assert "❌" in foo_line  # Because foo has an error in current run.
    assert "✅" in bar_line  # Because bar was successful in current run.
    assert "❓" in bar_line  # Because bar is not in reference run.
    assert not baz_line  # Because baz is not in current run.


@pytest.mark.parametrize("output_type", OutputType)
def test_make_summary_page__tolerance_list(output_type: OutputType) -> None:
    # Arrange
    foo_output = helper.make_verschillentool_output(  # All variables within tolerance.
        output_type=output_type,
        water_level=helper.tolerance_stats(output_type, Variable.WATER_LEVEL, -1e-6),
        flow_velocity=helper.tolerance_stats(output_type, Variable.FLOW_VELOCITY, -1e-6),
        row_count=42,
    )
    bar_output = helper.make_verschillentool_output(  # Flow velocity above tolerance.
        output_type=output_type,
        water_level=helper.tolerance_stats(output_type, Variable.WATER_LEVEL, -1e-6),
        flow_velocity=helper.tolerance_stats(output_type, Variable.FLOW_VELOCITY, 1e-6),
    )

    outputs = {"foo": foo_output, "bar": bar_output}
    kwargs = {f"{output_type.value}_outputs": outputs}
    verschilanalyse_comparison = helper.make_verschilanalyse_comparison(**kwargs)  # type: ignore

    # Act
    html_page = HtmlFormatter.make_summary_page(
        verschilanalyse=verschilanalyse_comparison,
        report_build_url="https://foo.bar/report",
        artifact_base_url="https://foo.bar/artifacts",
    )
    parser = SummaryPageParser()
    parser.feed(html_page)
    sections = parser.sections
    water_level = sections[f"{output_type.value}-water-level-tolerance-list"].get_text(html_page)
    flow_velocity = sections[f"{output_type.value}-flow-velocity-tolerance-list"].get_text(html_page)

    # Assert
    assert all(name not in water_level for name in ("foo", "bar"))
    assert "All water level differences are within tolerances" in water_level

    assert "foo" not in flow_velocity
    assert "bar" in flow_velocity  # Because bar flow velocity above tolerance


def test_make_summary_page__links() -> None:
    # Arrange
    report_build_url = "https://foo.bar/the-build-report-url"
    artifact_base_url = "https://foo.bar/path/to/my/artifacts"

    # Act
    html_page = HtmlFormatter.make_summary_page(
        verschilanalyse=helper.make_verschilanalyse_comparison(),
        report_build_url=report_build_url,
        artifact_base_url=artifact_base_url,
    )
    parser = SummaryPageParser()
    parser.feed(html_page)
    links = parser.sections["links"].get_text(html_page)

    # Assert
    assert all(
        needle in links
        for needle in (
            report_build_url,
            artifact_base_url,
            "current_logs.zip",
            "reference_logs.zip",
            "verschillen.zip",
        )
    )
