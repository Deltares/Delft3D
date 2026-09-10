"""Send the TestBench timeout report email."""

from __future__ import annotations

import argparse
import logging
import smtplib
from email.mime.multipart import MIMEMultipart
from email.mime.text import MIMEText
from pathlib import Path

LOGGER = logging.getLogger(__name__)


def create_parser() -> argparse.ArgumentParser:
    """Build the command line parser.

    Returns
    -------
    argparse.ArgumentParser
        Parser for the send-email command.
    """
    parser = argparse.ArgumentParser(description="Send the TestBench timeout report email.")
    parser.add_argument("--email-server", required=True, help="Address of the email server.")
    parser.add_argument("--email-port", required=True, type=int, help="Port of the email server.")
    parser.add_argument("--email-from", required=True, help="Email address of the sender.")
    parser.add_argument("--email-to", required=True, help="Email address of the recipient.")
    parser.add_argument("--email-content", required=True, type=Path, help="Path to the HTML email body.")
    return parser


def create_email_message(email_from: str, email_to: str, html_path: Path) -> MIMEMultipart:
    """Create the email message.

    Parameters
    ----------
    email_from : str
        Sender address.
    email_to : str
        Recipient address.
    html_path : Path
        HTML body file.

    Returns
    -------
    MIMEMultipart
        Email message.
    """
    message = MIMEMultipart()
    message["Subject"] = "TestBench duration vs maxRunTime"
    message["From"] = email_from
    message["To"] = email_to
    html_content = html_path.read_text(encoding="utf-8")
    message.attach(MIMEText(html_content, "html"))
    return message


def send_email(
    message: MIMEMultipart,
    email_from: str,
    email_to: str,
    email_server: str,
    email_port: int,
) -> None:
    """Send the email.

    Parameters
    ----------
    message : MIMEMultipart
        Message to send.
    email_from : str
        Sender address.
    email_to : str
        Recipient address.
    email_server : str
        SMTP host.
    email_port : int
        SMTP port.
    """
    with smtplib.SMTP(email_server, email_port) as server:
        server.sendmail(email_from, email_to, message.as_string())
    LOGGER.info("Email sent successfully.")


def main(argv: list[str] | None = None) -> int:
    """Send the timeout report email.

    Parameters
    ----------
    argv : list[str] | None, optional
        Command line arguments.

    Returns
    -------
    int
        Process exit code.
    """
    logging.basicConfig(level=logging.INFO)
    arguments = create_parser().parse_args(argv)
    message = create_email_message(
        email_from=arguments.email_from,
        email_to=arguments.email_to,
        html_path=arguments.email_content,
    )
    send_email(
        message=message,
        email_from=arguments.email_from,
        email_to=arguments.email_to,
        email_server=arguments.email_server,
        email_port=arguments.email_port,
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
