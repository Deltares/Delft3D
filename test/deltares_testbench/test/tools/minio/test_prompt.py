import io

import pytest
from tools.minio.prompt import DefaultPrompt, InteractivePrompt


class TestInteractivePrompt:
    @pytest.mark.parametrize("option", ["foo", "bar", "baz"])
    def test_choose__choose_option__get_correct_answer(self, option) -> None:
        # Arrange
        options = ["foo", "bar", "baz"]
        in_stream = io.StringIO("".join(option + "\n" for option in options))
        out_stream = io.StringIO()
        prompt = InteractivePrompt(in_stream, out_stream)
        message = f"Expecting {option}"

        # Act
        choice = prompt.choose(message, option)

        # Assert
        assert choice == option
        out_stream.seek(0)
        output = out_stream.read()
        assert message in output

    @pytest.mark.parametrize("index", range(3))
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

    def test_choose__choose_non_existent_option__try_again(self) -> None:
        # Arrange
        in_stream = io.StringIO("qux\nfoo\n")
        out_stream = io.StringIO()
        prompt = InteractivePrompt(in_stream, out_stream)

        # Act
        choice = prompt.choose("Choose!", ["foo", "bar", "baz"])

        # Assert
        assert choice == "foo"
        out_stream.seek(0)
        output = out_stream.read()
        assert "Invalid option" in output and "qux" in output

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

    @pytest.mark.parametrize("option,expected_result", [("yes", True), ("no", False)])
    def test_yes_no__select_right_option__get_correct_bool_value(self, option: str, expected_result: bool) -> None:
        # Arrange
        out_stream = io.StringIO()
        prompt = InteractivePrompt(io.StringIO(option + "\n"), out_stream)
        message = f"Expecting {option}"

        # Act
        result = prompt.yes_no(message)

        # Assert
        assert isinstance(result, bool)
        assert result == expected_result
        out_stream.seek(0)
        output = out_stream.read()
        assert message in output

    @pytest.mark.parametrize("default_yes", [True, False])
    def test_yes_no__user_hits_enter__get_default_choice(self, default_yes: bool) -> None:
        # Arrange
        in_stream = io.StringIO("\n")
        out_stream = io.StringIO()
        prompt = InteractivePrompt(in_stream, out_stream)

        # Act
        choice = prompt.yes_no("Choose!", default_yes=default_yes)

        # Assert
        assert choice == default_yes

    def test_yes_no__neither_yes_or_no__try_again(self) -> None:
        # Arrange
        in_stream = io.StringIO("maybe\nyes\n")
        out_stream = io.StringIO()
        prompt = InteractivePrompt(in_stream, out_stream)

        # Act
        choice = prompt.yes_no("Choose!")

        # Assert
        assert choice
        out_stream.seek(0)
        output = out_stream.read()
        assert all(s in output for s in ("maybe", "Invalid option"))

    def test_yes_no__no_input__return_none(self) -> None:
        # Arrange
        prompt = InteractivePrompt(io.StringIO("..."), io.StringIO())

        # Act
        choice = prompt.yes_no("Choose!")

        # Assert
        assert choice is None


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

    @pytest.mark.parametrize("default_yes", [True, False])
    def test_yes_no__choose_default(self, default_yes: bool) -> None:
        # Arrange
        prompt = DefaultPrompt()

        # Act
        choice = prompt.yes_no("Choose!", default_yes)

        # Assert
        assert choice == default_yes

    @pytest.mark.parametrize("default_yes", [True, False])
    def test_yes_no__force_yes__yes_even_if_default_is_no(self, default_yes: bool) -> None:
        # Arrange
        prompt = DefaultPrompt(force_yes=True)

        # Act
        choice = prompt.yes_no("Choose!", default_yes=default_yes)

        # Assert
        assert choice
