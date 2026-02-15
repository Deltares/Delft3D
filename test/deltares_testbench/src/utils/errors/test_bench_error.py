"""Test bench error.

Copyright (C)  Stichting Deltares, 2026
"""


class TestBenchError(Exception):
    """Generic test bench error."""

    def __init__(self, *args: object) -> None:
        super().__init__(*args)
