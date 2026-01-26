import io
import logging

import pytest

from ci_tools.teamcity.log import TeamCityFormatter, enter_test_context, escape_service_message


class TestTeamCityServiceMessageFormatter:
    def test_enter_test_context__flow_id_and_name_only_within_context(self) -> None:
        # Arrange
        formatter = TeamCityFormatter()
        logger = logging.getLogger()
        logger.setLevel(logging.DEBUG)

        stream = io.StringIO()
        handler = logging.StreamHandler(stream)
        handler.setFormatter(formatter)
        logger.addHandler(handler)

        # Act
        logger.info("foo")
        with enter_test_context("test1", logger):
            logger.warning("bar")
        logger.error("baz")

        # Assert
        stream.seek(0)
        lines = [line.rstrip("\n") for line in stream]
        test1_lines = lines[1:4]

        assert all((line.startswith("##teamcity[") and line[-1] == "]") for line in lines)

        assert not any("flowId" in line for line in (lines[0], lines[4]))
        assert all("flowId='test1'" in line for line in test1_lines)

        assert "text='foo'" in lines[0]

        assert "testStarted" in test1_lines[0]
        assert "name='test1'" in test1_lines[0]
        assert "text='bar'" in test1_lines[1]
        assert "testFinished" in test1_lines[2]
        assert "name='test1'" in test1_lines[2]

        assert "text='baz'" in lines[4]

    def test_enter_test_context__catch_exception_and_fail_test(self) -> None:
        # Arrange
        formatter = TeamCityFormatter()
        logger = logging.getLogger()
        logger.setLevel(logging.DEBUG)

        stream = io.StringIO()
        handler = logging.StreamHandler(stream)
        handler.setFormatter(formatter)
        logger.addHandler(handler)

        # Act
        with enter_test_context("test2", logger):
            logger.debug("qux")
            raise ValueError("Kaboom!")

        # Assert
        stream.seek(0)
        lines = [line.rstrip("\n") for line in stream]

        assert all((line.startswith("##teamcity[") and line[-1] == "]") for line in lines)
        assert all("flowId='test2'" in line for line in lines)
        assert "testStarted" in lines[0]
        assert "name='test2'" in lines[0]
        assert "status='NORMAL'" in lines[1]
        assert "testFailed" in lines[2]
        assert "name='test2'" in lines[2]
        assert "Kaboom!" in lines[2]
        assert "testFinished" in lines[3]
        assert "name='test2'" in lines[3]


@pytest.mark.parametrize(
    ("character", "expected"),
    list(
        zip("\n\r[]'|", ["|n", "|r", "|[", "|]", "|'", "||"], strict=True),
    ),
)
def test_escape_service_message(character: str, expected: str) -> None:
    assert escape_service_message(character) == expected


@pytest.mark.parametrize(
    ("character", "expected"),
    list(
        zip("\u0080\ubaad\uc0de\uffff", ["|0x0080", "|0xBAAD", "|0xC0DE", "|0xFFFF"], strict=True),
    ),
)
def test_escape_service_message__unicode(character: str, expected: str) -> None:
    assert escape_service_message(character) == expected


@pytest.mark.parametrize(
    ("message", "expected"),
    [
        pytest.param("foobarbaz", "foobarbaz", id="no_special_chars"),
        pytest.param("foo\nbar\rbaz", "foo|nbar|rbaz", id="multiple"),
        pytest.param("|qux|", "||qux||", id="start-end"),
        pytest.param("foo''bar", "foo|'|'bar", id="consecutive"),
    ],
)
def test_escape_service_message__replace_in_string(message: str, expected: str) -> None:
    assert escape_service_message(message) == expected
