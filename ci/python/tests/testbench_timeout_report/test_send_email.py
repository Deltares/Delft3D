from pathlib import Path
from unittest.mock import MagicMock

import pytest
from pytest_mock import MockerFixture

from ci_tools.testbench_timeout_report.send_email import EMAIL_FROM, SMTP_HOST, SMTP_PORT, send_report


def test_send_report(tmp_path: Path, mocker: MockerFixture) -> None:
    html_path = tmp_path / "email.html"
    html_path.write_text("<html><body>hi</body></html>", encoding="utf-8")
    smtp = MagicMock()
    smtp_cm = MagicMock()
    smtp_cm.__enter__.return_value = smtp
    smtp_cm.__exit__.return_value = None
    factory = mocker.patch("ci_tools.testbench_timeout_report.send_email.smtplib.SMTP", return_value=smtp_cm)

    send_report(html_path, "robin.vanwestrenen@deltares.nl, black-ops@deltares.nl")

    factory.assert_called_once_with(SMTP_HOST, SMTP_PORT)
    smtp.sendmail.assert_called_once()
    sender, recipients, payload = smtp.sendmail.call_args.args
    assert sender == EMAIL_FROM
    assert recipients == ["robin.vanwestrenen@deltares.nl", "black-ops@deltares.nl"]
    assert "TestBench duration vs maxRunTime" in payload
    assert "robin.vanwestrenen@deltares.nl" in payload


def test_send_report_rejects_empty_recipient(tmp_path: Path) -> None:
    html_path = tmp_path / "email.html"
    html_path.write_text("<html></html>", encoding="utf-8")
    with pytest.raises(ValueError, match="email_to is empty"):
        send_report(html_path, " , ")
