class ComparisonResult:
    """Comparison result class.

    Two types of results are distinguished: maxAbsDiff and maxRelDiff.
    For both results, the following are administrated:
    - The maxDiff itself,
    - The original values of left and right that caused the maxDiff,
    - The coordinates in the table where the values were found.
    """

    def __init__(self, error=False) -> None:
        self.passed = None
        self.error = error
        self.result = ""
        self.maxAbsDiff = 0.0
        self.maxAbsDiffValues = (0.0, 0.0)  # Left and right.
        self.maxAbsDiffCoordinates = ()
        self.maxRelDiff = 0.0
        self.maxRelDiffValues = (0.0, 0.0)  # Left and right.
        self.maxRelDiffCoordinates = ()
        self.lineNumber = 0
        self.columnNumber = 0
        self.path = []

    # No getters and setters: use this object as if it were a struct.

    def isToleranceExceeded(self, maxAbsDiffTolerance=None, maxRelDiffTolerance=None) -> None:
        if self.error:
            self.passed = False
            self.result = "ERROR"
        elif maxAbsDiffTolerance is not None and maxRelDiffTolerance is not None:
            # The line below used to contain "and" instead of "or". "or" seems more useful
            if self.maxAbsDiff <= maxAbsDiffTolerance or self.maxRelDiff <= maxRelDiffTolerance:
                self.passed = True
                self.result = "OK"
            else:
                self.passed = False
                self.result = "NOK"
        elif maxAbsDiffTolerance is not None and self.maxAbsDiff <= maxAbsDiffTolerance:
            self.passed = True
            self.result = "OK"
        elif maxRelDiffTolerance is not None and self.maxRelDiff <= maxRelDiffTolerance:
            self.passed = True
            self.result = "OK"
        elif (self.maxAbsDiff == 0.0) or (self.maxRelDiff == 0.0):
            self.passed = True
            self.result = "OK"
        else:
            self.passed = False
            self.result = "NOK"
