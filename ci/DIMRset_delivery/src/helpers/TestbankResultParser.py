import re
from typing import List


class TestbankResultParser(object):
    """
    Object responsible for parsing a specific testbank result artifact.
    """
    def __init__(self, testbank_result: str):
        """
        Creates a new instance of TestbankResultParser.

        Args:
            testbank_result (str): The testbank result as a string.
        """
        self.testbank_result = testbank_result

    def get_percentage_total_passing(self) -> str:
        """ Gets the total percentage of passing tests. """
        start_index = self.testbank_result.find("Summary")
        substring = self.testbank_result[start_index:] # get all text from "Summary" to end of file.
        percentage = re.findall(r"Percentage\D*([0-9.]*)", substring)[0]
        return percentage

    def get_total_tests(self) -> str:
        """ Gets the total number of tests. """
        total_number = re.findall(r"Total tests\D*([0-9.]*)", self.testbank_result)[0]
        return total_number

    def get_total_passing(self) -> str:
        """ Gets the total number of passing tests. """
        start_index = self.testbank_result.find("Summary")
        substring = self.testbank_result[start_index:]  # get all text from "Summary" to end of file.
        total_number = re.findall(r"Passed\D*([0-9.]*)", substring)[0]
        return total_number

    def get_total_failing(self) -> str:
        """ Gets the total number of failing tests. """
        start_index = self.testbank_result.find("Summary")
        substring = self.testbank_result[start_index:]  # get all text from "Summary" to end of file.
        total_number = re.findall(r"Not passed\D*([0-9.]*)", substring)[0]
        return total_number

    def get_total_exceptions(self) -> str:
        """ Gets the total number of exceptions that occurred. """
        total_number = re.findall(r"Exception\D*:\D*([0-9.]*)", self.testbank_result)[0]
        return total_number

    def get_exceptions_for_testcase_group(self, testcase_group: str) -> List[str]:
        """ Gets all exceptions that occurred for the given testcase group. """
        exceptions = []

        substring = self.__get_substring_for_testcase_group(product=testcase_group)

        if substring is not None:
            lines = substring.splitlines()
            for line in lines:
                if line is not None and "Exception" in line:
                    exception = line.split("Exception ")[-1]
                    exceptions.append(exception)
        return exceptions

    def __get_substring_for_testcase_group(self, product: str) -> str:
        start_index = re.search(f"{product}[\\s]*{product}", self.testbank_result).end()
        end_index = re.search(r"Percentage\D*[0-9.]*", self.testbank_result[start_index:]).end() + start_index
        substring = self.testbank_result[start_index:end_index]
        return substring





