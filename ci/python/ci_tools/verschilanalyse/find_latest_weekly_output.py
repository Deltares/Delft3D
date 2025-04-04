import sys
from collections import defaultdict
from dataclasses import dataclass
from datetime import datetime

import minio
from minio.credentials import AWSConfigProvider

ENDPOINT = "s3.deltares.nl"
BUCKET_NAME = "devops-test-verschilanalyse"
PREFIX = "output/weekly"


@dataclass
class VerschilAnalyseReport:
    """Weekly verschilanalyse report from the bucket."""

    tag: str
    model_names: list[str]
    key: str
    timestamp: datetime


class VerschilAnalyseReporter:
    """Finds weekly verschilanalyse reports in the bucket."""

    def __init__(self, minio: minio.Minio, bucket_name: str, output_prefix: str) -> None:
        self._minio = minio
        self._bucket_name = bucket_name
        self._prefix = f"{output_prefix.rstrip('/')}/"

    def find_weekly_reports(self) -> list[VerschilAnalyseReport]:
        """Find all the weekly reports in the bucket with at least one model output."""
        reports: dict[str, tuple[str, datetime]] = {}
        models: defaultdict[str, list[str]] = defaultdict(list)

        for obj in self._minio.list_objects(bucket_name=self._bucket_name, prefix=self._prefix, recursive=True):
            suffix = obj.object_name.removeprefix(self._prefix)
            match suffix.split("/", maxsplit=2):
                case [tag, "report", _]:
                    if "weekly" in tag:
                        reports[tag] = (obj.object_name, obj.last_modified)
                case [tag, "output", postfix]:
                    if "weekly" in tag and postfix.endswith(".zip"):
                        models[tag].append(postfix.removesuffix(".zip"))

        return [
            VerschilAnalyseReport(
                tag=tag,
                model_names=models[tag],
                key=key,
                timestamp=timestamp,
            )
            for tag, (key, timestamp) in reports.items()
            if models[tag]
        ]

    def get_latest_report(self) -> VerschilAnalyseReport | None:
        """Get the latest successful report."""
        reports = self.find_weekly_reports()
        if not reports:
            return None
        return max(reports, key=lambda report: report.timestamp)


if __name__ == "__main__":
    """Command line program to find the latest weekly verschilanalyse output and report.

    Used in TeamCity pipeline to set the `reference_prefix` parameter.
    """
    client = minio.Minio(endpoint=ENDPOINT, credentials=AWSConfigProvider())
    reporter = VerschilAnalyseReporter(minio=client, bucket_name=BUCKET_NAME, output_prefix=PREFIX)

    report = reporter.get_latest_report()
    if report is None:
        print("No weekly reports found.", file=sys.stderr)
        exit(1)

    print("Found verschilanalyse report:")
    print(f"Apptainer tag: {report.tag}")
    print(f"Timestamp: {report.timestamp.isoformat()}")
    print(f"References found for {len(report.model_names)} models:")
    for model_name in report.model_names:
        print(f"- {model_name}")

    # Override the TeamCity `reference_prefix` parameter using a TeamCity service message.
    # See: https://www.jetbrains.com/help/teamcity/service-messages.html
    reference_prefix = f"output/weekly/{report.tag}/output"
    print(f"##teamcity[setParameter name='reference_prefix' value='{reference_prefix}']")
