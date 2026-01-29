import io

import pytest
from pytest_mock import MockerFixture

from tools.minio.prompt import Answer, DefaultParser, DefaultPrompt, InputParser, InteractivePrompt


class TestInteractivePrompt:
    @pytest.mark.parametrize("index", [1, 2, 3])
    def test_choose__choose_option__get_correct_answer(self, index: int) -> None:
        # Arrange
        options = ["foo", "bar", "baz"]
        in_stream = io.StringIO(f"{index}\n")
        out_stream = io.StringIO()
        prompt = InteractivePrompt(in_stream, out_stream)
        message = f"Expecting {index}"

        # Act
        choice = prompt.choose(message, options)

        # Assert
        assert choice == options[index - 1]
        out_stream.seek(0)
        output = out_stream.read()
        assert message in output

    @pytest.mark.parametrize("index", [0, 1, 2])
    def test_choose__user_hits_enter__get_default_choice(self, index: int) -> None:
        # Arrange
        options = ["foo", "bar", "baz"]
        in_stream = io.StringIO("\n")
        out_stream = io.StringIO()
        prompt = InteractivePrompt(in_stream, out_stream)

        # Act
        choice = prompt.choose("Choose!", options, default_idx=index)

        # Assert
        assert choice == options[index]

    def test_choose__user_hits_enter__get_first_option(self) -> None:
        # Arrange
        options = ["foo", "bar", "baz"]
        in_stream = io.StringIO("\n")
        out_stream = io.StringIO()
        prompt = InteractivePrompt(in_stream, out_stream)

        # Act
        choice = prompt.choose("Choose!", options)

        # Assert
        assert choice == "foo"

    @pytest.mark.parametrize(
        "input_",
        [
            pytest.param("-1\n", id="negative"),
            pytest.param("4\n", id="out_of_range"),
            pytest.param("spam\n", id="not_a_number"),
        ],
    )
    def test_choose__choose_non_existent_option__try_again(self, input_: str) -> None:
        # Arrange
        in_stream = io.StringIO(input_)
        out_stream = io.StringIO()
        prompt = InteractivePrompt(in_stream, out_stream)

        # Act
        choice = prompt.choose("Choose!", ["foo", "bar", "baz"])

        # Assert
        assert choice is None
        out_stream.seek(0)
        output = out_stream.read()
        assert "Please enter a number" in output

    def test_choose__in_stream_ends__return_none(self) -> None:
        # Arrange
        prompt = InteractivePrompt(io.StringIO("foo"), io.StringIO())

        # Act
        choice = prompt.choose("Choose!", ["foo", "bar", "baz"])

        # Assert
        assert choice is None

    def test_choose__no_options__raise_value_error(self) -> None:
        # Arrange
        prompt = InteractivePrompt(io.StringIO(), io.StringIO())

        # Act, Assert
        with pytest.raises(ValueError, match="must be non-empty"):
            prompt.choose("Choose!", [])

    @pytest.mark.parametrize("index", [-4, 3])
    def test_choose__default_idx_out_of_range__raise_value_error(self, index: int) -> None:
        # Arrange
        prompt = InteractivePrompt(io.StringIO("\n"), io.StringIO())

        # Act, Assert
        with pytest.raises(ValueError, match="out of range"):
            prompt.choose("Choose!", ["foo", "bar", "baz"], default_idx=index)

    @pytest.mark.parametrize("answer", [Answer.YES, Answer.NO])
    def test_yes_no__select_right_option__get_correct_bool_value(self, answer: Answer) -> None:
        # Arrange
        out_stream = io.StringIO()
        prompt = InteractivePrompt(io.StringIO(answer.value + "\n"), out_stream)
        message = f"Expecting {answer.value}"

        # Act
        result = prompt.yes_no(message)

        # Assert
        assert result == answer
        out_stream.seek(0)
        output = out_stream.read()
        assert message in output

    @pytest.mark.parametrize("default", Answer)
    def test_yes_no__user_hits_enter__get_default_choice(self, default: Answer) -> None:
        # Arrange
        in_stream = io.StringIO("\n")
        out_stream = io.StringIO()
        prompt = InteractivePrompt(in_stream, out_stream)

        # Act
        choice = prompt.yes_no("Choose!", default=default)

        # Assert
        assert choice == default

    def test_yes_no__neither_yes_or_no__try_again(self) -> None:
        # Arrange
        in_stream = io.StringIO("maybe\nyes\n")
        out_stream = io.StringIO()
        prompt = InteractivePrompt(in_stream, out_stream)

        # Act
        choice = prompt.yes_no("Choose!")

        # Assert
        assert choice == Answer.YES
        out_stream.seek(0)
        output = out_stream.read()
        assert "Please enter 'yes' or 'no'" in output

    def test_yes_no__no_newline__return_cancel(self) -> None:
        # Arrange
        prompt = InteractivePrompt(io.StringIO("..."), io.StringIO())

        # Act
        choice = prompt.yes_no("Choose!")

        # Assert
        assert choice == Answer.CANCEL

    @pytest.mark.parametrize(
        ("input_", "expected"),
        [
            pytest.param("I'm hitting enter\n", "I'm hitting enter", id="ends-in-newline"),
            pytest.param("I'm hitting CTRL-Z", None, id="end-of-file"),
        ],
    )
    def test_input__without_parser__use_default_parser(self, input_: str, expected: str | None) -> None:
        # Arrange
        in_stream = io.StringIO(input_)
        out_stream = io.StringIO()
        prompt = InteractivePrompt(in_stream, out_stream)

        # Act
        result = prompt.input("Type something and hit enter")

        # Assert
        assert result == expected

    def test_input__with_parser(self, mocker: MockerFixture) -> None:
        # Arrange
        input_helper = mocker.Mock(spec=InputParser[int])
        in_stream = io.StringIO(42 * "🏆")
        out_stream = io.StringIO()
        prompt = InteractivePrompt(in_stream, out_stream)

        input_helper.parse.side_effect = len
        input_helper.complete.return_value = iter([])  # Skip installing autocompleter.

        # Act
        result = prompt.input("Type something and hit enter", input_helper)

        # Assert
        assert result == 42

    def test_input__parser_raises_value_error__warn_user_try_again(self, mocker: MockerFixture) -> None:
        # Arrange
        input_helper = mocker.Mock(spec=InputParser[None])
        in_stream = io.StringIO("🍞")
        out_stream = io.StringIO()
        prompt = InteractivePrompt(in_stream, out_stream)

        input_helper.parse.side_effect = [ValueError("Quack!"), None]
        input_helper.complete.return_value = iter([])  # Skip installing autocompleter.

        # Act
        result = prompt.input("🦆: Feed me!", input_helper)

        # Assert
        out_stream.seek(0)
        output = out_stream.read()
        assert result is None
        assert "Invalid input: 🍞" in output


class TestDefaultPrompt:
    @pytest.mark.parametrize("default_idx", range(3))
    def test_choose__always_pick_default(self, default_idx: int) -> None:
        # Arrange
        prompt = DefaultPrompt()
        options = ["foo", "bar", "baz"]

        # Act
        choice = prompt.choose("Choose!", options, default_idx)

        # Assert
        assert choice == options[default_idx]

    def test_choose__always_pick_first(self) -> None:
        # Arrange
        prompt = DefaultPrompt()
        options = ["foo", "bar", "baz"]

        # Act
        choice = prompt.choose("Choose!", options)

        # Assert
        assert choice == options[0]

    @pytest.mark.parametrize("default", Answer)
    def test_yes_no__choose_default(self, default: Answer) -> None:
        # Arrange
        prompt = DefaultPrompt()

        # Act
        choice = prompt.yes_no("Choose!", default)

        # Assert
        assert choice == default

    @pytest.mark.parametrize("default", Answer)
    def test_yes_no__force_yes__yes_even_if_default_is_no(self, default: Answer) -> None:
        # Arrange
        prompt = DefaultPrompt(force_yes=True)

        # Act
        choice = prompt.yes_no("Choose!", default=default)

        # Assert
        assert choice == Answer.YES

    def test_input__parse_empty_line(self, mocker: MockerFixture) -> None:
        # Arrange
        prompt = DefaultPrompt()
        parser = mocker.Mock(spec=InputParser)

        def side_effect(line: str) -> None:
            # Assert
            assert line == "\n"

        parser.parse.side_effect = side_effect

        # Act
        prompt.input("Type something", parser)


class TestDefaultInputHelper:
    @pytest.mark.parametrize(
        ("input_", "expected"),
        [
            pytest.param("   I hit enter   \n", "I hit enter", id="strip-input-line"),
            pytest.param("I hit CTRL-Z to end the input", None, id="end-of-file"),
        ],
    )
    def test_default_input_helper(self, input_: str, expected: str | None) -> None:
        # Arrange
        helper = DefaultParser()

        # Act
        result = helper.parse(input_)

        # Assert
        assert result == expected
