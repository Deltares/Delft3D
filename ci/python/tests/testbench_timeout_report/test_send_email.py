from pathlib import Path
from unittest.mock import MagicMock

from pytest_mock import MockerFixture

from ci_tools.testbench_timeout_report.send_email import main


def test_send_email_main(tmp_path: Path, mocker: MockerFixture) -> None:
    html_path = tmp_path / "email.html"
    html_path.write_text("<html><body>hi</body></html>", encoding="utf-8")
    smtp = MagicMock()
    smtp_cm = MagicMock()
    smtp_cm.__enter__.return_value = smtp
    smtp_cm.__exit__.return_value = None
    mocker.patch("ci_tools.testbench_timeout_report.send_email.smtplib.SMTP", return_value=smtp_cm)

    exit_code = main(
        [
            "--email-server",
            "smtp.example",
            "--email-port",
            "25",
            "--email-from",
            "from@example",
            "--email-to",
            "robin.vanwestrenen@deltares.nl",
            "--email-content",
            str(html_path),
        ]
    )

    assert exit_code == 0
    smtp.sendmail.assert_called_once()
    payload = smtp.sendmail.call_args.args[2]
    assert "TestBench duration vs maxRunTime" in payload
    assert "robin.vanwestrenen@deltares.nl" in payload
