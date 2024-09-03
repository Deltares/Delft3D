class ComparisonResult:
    """Comparison result class.

    Two types of results are distinguished: maxAbsDiff and maxRelDiff.
    For both results, the following are administrated:
    - The maxDiff itself,
    - The original values of left and right that caused the maxDiff,
    - The coordinates in the table where the values were found.
    """

    def __init__(self, error: bool =False) -> None:
        self.passed = None
        self.error = error
        self.result = ""
        self.max_abs_diff = 0.0
        self.max_abs_diff_values = (0.0, 0.0)  # Left and right.
        self.max_abs_diff_coordinates = ()
        self.max_rel_diff = 0.0
        self.max_rel_diff_values = (0.0, 0.0)  # Left and right.
        self.max_rel_diff_coordinates = ()
        self.line_number = 0
        self.column_number = 0
        self.path = []

    # No getters and setters: use this object as if it were a struct.

    def is_tolerance_exceeded(self, maxAbsDiffTolerance=None, maxRelDiffTolerance=None) -> None:
        """Determine if the maximum absolute or relative difference exceeds the specified tolerances."""
        if self.error:
            self.passed = False
            self.result = "ERROR"
        elif maxAbsDiffTolerance is not None and maxRelDiffTolerance is not None:
            # The line below used to contain "and" instead of "or". "or" seems more useful
            if self.max_abs_diff <= maxAbsDiffTolerance or self.max_rel_diff <= maxRelDiffTolerance:
                self.passed = True
                self.result = "OK"
            else:
                self.passed = False
                self.result = "NOK"
        elif (
            (maxAbsDiffTolerance is not None and self.max_abs_diff <= maxAbsDiffTolerance)
            or (maxRelDiffTolerance is not None and self.max_rel_diff <= maxRelDiffTolerance)
            or (self.max_abs_diff == 0.0)
            or (self.max_rel_diff == 0.0)
        ):
            self.passed = True
            self.result = "OK"
        else:
            self.passed = False
            self.result = "NOK"
