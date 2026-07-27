import argparse
from pathlib import Path

from junit_xml import TestSuite

from ci_tools.verschilanalyse.util.excel_exporter import ExcelExporter
from ci_tools.verschilanalyse.util.html_formatter import HtmlFormatter
from ci_tools.verschilanalyse.util.junit_exporter import JUnitExporter
from ci_tools.verschilanalyse.util.verschilanalyse_comparison import VerschilanalyseComparison


def _parse_cli_args() -> argparse.Namespace:
    current_log_dir_help = "Directory containing the Slurm logs of the current verschilanalyse"
    reference_log_dir_help = "Directory containing the Slurm logs of the reference verschilanalyse"
    verschillen_dir_help = "Directory containing the verschillentool output"
    output_dir_help = "Directory to write the output of the generate_summaries script to"
    s3_current_prefix_help = "The prefix in MinIO where the current verschilanalyse output is stored."
    s3_reference_prefix_help = "The prefix in MinIO where the reference verschilanalyse output is stored."
    report_build_url_help = "The URL of the 'Report' build in TeamCity. Used to produce a link in the HTML page."
    artifact_base_url_help = "The URL to the artifacts of the 'Report' build. Used to produce links in the HTML page."

    parser = argparse.ArgumentParser("Summarize verschilanalyse logs and verschillentool output.")
    parser.add_argument("--current-log-dir", type=Path, required=True, help=current_log_dir_help)
    parser.add_argument("--reference-log-dir", type=Path, required=True, help=reference_log_dir_help)
    parser.add_argument("--verschillen-dir", type=Path, required=True, help=verschillen_dir_help)
    parser.add_argument("--output-dir", type=Path, default=Path("output"), help=output_dir_help)
    parser.add_argument("--s3-current-prefix", type=str, default="", help=s3_current_prefix_help)
    parser.add_argument("--s3-reference-prefix", type=str, default="", help=s3_reference_prefix_help)
    parser.add_argument("--report-build-url", type=str, default="", help=report_build_url_help)
    parser.add_argument("--artifact-base-url", type=str, default="", help=artifact_base_url_help)
    return parser.parse_args()


if __name__ == "__main__":
    """Script used to generate the summaries of the weekly verschilanalyse.

    This script generates three summaries to the `output_dir` directory:
    - `verschilanalyse_junit.xml`: A jUnit xml file used to summarize the
        model runs in the TeamCity build.
    - `verschillentool_summary.xlsx`: A verschillentool summary excel file.
        Contains statistics from the verschillentool output. Attached to the weekly email.
    - `email_content.html`: A HTML page containing the content of the weekly email.
    """
    args = _parse_cli_args()
    s3_current_prefix = args.s3_current_prefix.rstrip("/")
    s3_reference_prefix = args.s3_reference_prefix.rstrip("/")

    verschilanalyse = VerschilanalyseComparison.from_report_directories(
        current_log_dir=args.current_log_dir,
        reference_log_dir=args.reference_log_dir,
        verschillen_dir=args.verschillen_dir,
        s3_current_prefix=s3_current_prefix,
        s3_reference_prefix=s3_reference_prefix,
    )

    output_dir: Path = args.output_dir
    output_dir.mkdir(parents=True, exist_ok=True)

    suites = [
        JUnitExporter.make_test_suite(
            suite_name="Current verschilanalyse",
            logs_url=f"{s3_current_prefix}/logs/logs.zip",
            logs=verschilanalyse.current_log_data,
        ),
        JUnitExporter.make_test_suite(
            suite_name="Reference verschilanalyse",
            logs_url=f"{s3_reference_prefix}/logs/logs.zip",
            logs=verschilanalyse.reference_log_data,
        ),
    ]
    with open(output_dir / "verschilanalyse_junit.xml", "w") as out_file:
        TestSuite.to_file(out_file, test_suites=suites)

    log_comparisons = verschilanalyse.get_log_comparisons()
    workbook = ExcelExporter.make_summary_workbook(verschilanalyse)
    workbook.save(output_dir / "verschillentool_summary.xlsx")

    html_page = HtmlFormatter.make_summary_page(
        verschilanalyse=verschilanalyse,
        report_build_url=args.report_build_url,
        artifact_base_url=args.artifact_base_url,
    )
    (output_dir / "email_content.html").write_text(html_page)
