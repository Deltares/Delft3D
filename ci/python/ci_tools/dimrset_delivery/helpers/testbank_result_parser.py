import re


class TestbankResultParser(object):
    """Object responsible for parsing a specific testbank result artifact."""

    def __init__(self, testbank_result: str) -> None:
        """
        Create a new instance of TestbankResultParser.

        Parameters
        ----------
        testbank_result : str
            The testbank result as a string.
        """
        self.testbank_result = testbank_result

    def get_percentage_total_passing(self) -> str:
        """Get the total percentage of passing tests."""
        start_index = self.testbank_result.find("Summary")
        substring = self.testbank_result[start_index:]  # get all text from "Summary" to end of file.
        matches = re.findall(r"Percentage\D*([0-9.]*)", substring)
        percentage: str = matches[0]
        return percentage

    def get_total_tests(self) -> str:
        """Get the total number of tests."""
        matches = re.findall(r"Total tests\D*([0-9.]*)", self.testbank_result)
        total_number: str = matches[0]
        return total_number

    def get_total_passing(self) -> str:
        """Get the total number of passing tests."""
        start_index = self.testbank_result.find("Summary")
        substring = self.testbank_result[start_index:]  # get all text from "Summary" to end of file.
        matches = re.findall(r"Passed\D*([0-9.]*)", substring)
        total_number: str = matches[0]
        return total_number

    def get_total_failing(self) -> str:
        """Get the total number of failing tests."""
        start_index = self.testbank_result.find("Summary")
        substring = self.testbank_result[start_index:]  # get all text from "Summary" to end of file.
        matches = re.findall(r"Not passed\D*([0-9.]*)", substring)
        total_number: str = matches[0]
        return total_number

    def get_total_exceptions(self) -> str:
        """Get the total number of exceptions that occurred."""
        matches = re.findall(r"Exception\D*:\D*([0-9.]*)", self.testbank_result)
        total_number: str = matches[0]
        return total_number
