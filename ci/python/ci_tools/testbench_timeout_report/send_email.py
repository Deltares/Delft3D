"""Send the TestBench timeout report email via the Deltares SMTP relay."""

from __future__ import annotations

import logging
import smtplib
from email.mime.multipart import MIMEMultipart
from email.mime.text import MIMEText
from pathlib import Path

LOGGER = logging.getLogger(__name__)

SMTP_HOST = "smtp.directory.intra"
SMTP_PORT = 25
EMAIL_FROM = "black-ops@deltares.nl"
SUBJECT = "TestBench duration vs maxRunTime"


def send_report(html_path: Path, email_to: str) -> None:
    """Send ``email.html`` to ``email_to``. Multiple addresses may be comma-separated."""
    recipients = [part.strip() for part in email_to.split(",") if part.strip()]
    if not recipients:
        raise ValueError("email_to is empty")
    message = MIMEMultipart()
    message["Subject"] = SUBJECT
    message["From"] = EMAIL_FROM
    message["To"] = ", ".join(recipients)
    message.attach(MIMEText(html_path.read_text(encoding="utf-8"), "html"))
    with smtplib.SMTP(SMTP_HOST, SMTP_PORT) as server:
        server.sendmail(EMAIL_FROM, recipients, message.as_string())
    LOGGER.info("Sent timeout report to %s", ", ".join(recipients))
