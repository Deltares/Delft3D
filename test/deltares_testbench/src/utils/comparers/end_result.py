from enum import Enum


class EndResult(Enum):
    """Enum representing the result status."""

    ERROR = "ERROR"
    NOK = "NOK"
    OK = "OK"

    def __lt__(self, other: "EndResult") -> bool:
        """Compare EndResult instances based on their order."""
        if not isinstance(other, EndResult):
            return NotImplemented
        order = ["ERROR", "NOK", "OK"]
        return order.index(self.value) < order.index(other.value)

    @classmethod
    def from_string(cls, string: str) -> "EndResult":
        """Return the EndResult member corresponding to the given string."""
        try:
            return cls[string]
        except KeyError:
            raise ValueError(f"Unknown status: {string}")
