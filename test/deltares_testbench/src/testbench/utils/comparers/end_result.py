from enum import Enum
from functools import total_ordering


@total_ordering
class EndResult(Enum):
    """Enum representing the result status."""

    ERROR = "ERROR"
    NOK = "NOK"
    OK = "OK"

    def __lt__(self, other: "EndResult") -> bool:
        """Compare EndResult instances based on their order."""
        if not isinstance(other, EndResult):
            raise ValueError(f"Cannot compare EndResult with {type(other)}")
        order = ["ERROR", "NOK", "OK"]
        return order.index(self.value) < order.index(other.value)

    def __eq__(self, other: "EndResult") -> bool:
        """Check if two EndResult instances are equal."""
        if not isinstance(other, EndResult):
            raise ValueError(f"Cannot compare EndResult with {type(other)}")
        return self.value == other.value

    @classmethod
    def from_string(cls, string: str) -> "EndResult":
        """Return the EndResult member corresponding to the given string."""
        try:
            return cls[string]
        except KeyError:
            raise ValueError(f"Unknown status: {string}")
