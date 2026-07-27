"""S3 utility functions shared between modules."""

from datetime import datetime, timezone
from pathlib import Path


class S3UrlInfo:
    """Data class to hold parsed S3 URL information."""

    def __init__(self, hostname: str = "", bucket: str = "", path: str = "") -> None:
        self.hostname = hostname
        self.bucket = bucket
        self.path = path

    def to_url(self) -> str:
        """Return a normalized URL string for the S3 object."""
        prefix = (self.hostname or "").strip().rstrip("/")
        bucket = (self.bucket or "").strip("/")

        segments: list[str] = []
        if bucket:
            segments.append(bucket)
        if self.path:
            segments.extend(segment for segment in self.path.split("/") if segment)

        suffix = "/".join(segments)
        if not prefix:
            return f"/{suffix}" if suffix else ""

        return f"{prefix}/{suffix}" if suffix else prefix

    def to_local(self) -> Path:
        """Construct local path from S3 path by adding 'cases/' prefix and '/case' suffix."""
        normalized = self.path.strip("/")

        if normalized.endswith("/doc"):
            return Path("data") / normalized
        parts = normalized.split("/")
        if len(parts) >= 4 and parts[0].lower() == "cases":
            if (
                parts[1].lower().startswith("e")
                and parts[2].lower().startswith("f")
                and parts[3].lower().startswith("c")
            ):
                normalized = normalized + "/input"

        elif normalized.lower().startswith("references/"):
            rest_parts = normalized.split("/", 1)
            if len(rest_parts) == 2:
                reference_rest = rest_parts[1]
                platform, _, case_tail = reference_rest.partition("/")
                platform_lower = platform.lower()
                if case_tail:
                    if platform_lower == "lnx64":
                        normalized = f"cases/{case_tail}/reference_lnx64"
                    elif platform_lower == "win64":
                        normalized = f"cases/{case_tail}/reference_win64"

        return Path("data") / normalized


def rewind_timestep_2_datetime(timestamp: str) -> datetime | None:
    """
    Parse timestamp string for rewind functionality.

    Args:
        timestamp: Timestamp string to parse

    Returns
    -------
        Parsed datetime object or None if invalid
    """
    if not timestamp or timestamp == "NO VERSION":
        return None

    # Try different timestamp formats
    formats = [
        "%Y-%m-%dT%H:%M:%S.%f",  # With microseconds: 'e.g. 2025-09-11T13:20:21.667000'
        "%Y-%m-%dT%H:%M:%S",  # Without microseconds: 'e.g. 2025-09-11T13:20:21'
        "%Y-%m-%dT%H:%M",  # Without seconds: 'e.g. 2025-11-19T10:31'
    ]

    for fmt in formats:
        try:
            return datetime.strptime(timestamp, fmt).replace(tzinfo=timezone.utc)
        except ValueError:
            continue

    return None
