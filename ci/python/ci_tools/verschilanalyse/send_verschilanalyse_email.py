import argparse
import logging
import smtplib
import textwrap
from email import encoders
from email.mime.base import MIMEBase
from email.mime.multipart import MIMEMultipart
from email.mime.text import MIMEText
from pathlib import Path


def create_parser() -> argparse.ArgumentParser:
    """Make command line argument parser for the send_mail command."""
    parser = argparse.ArgumentParser(description="Send the weekly verschilanalyse email.")
    parser.add_argument("--email-server", required=True, help="Adress of the email server.")
    parser.add_argument("--email-port", required=True, help="Port of the email server.")
    parser.add_argument("--email-from", required=True, help="Email address of the sender.")
    parser.add_argument("--email-to", required=True, help="email address of the recipient.")
    parser.add_argument("--email-content", required=True, type=Path, help="Path to file with HTML email content.")
    parser.add_argument("--attachment", required=True, type=Path, help="Path to summary attachment.")
    return parser


def encode_attachment(path: Path) -> MIMEBase:
    """Attach files to the email message."""
    part = MIMEBase("application", "octet-stream")
    part.set_payload(path.read_bytes())
    encoders.encode_base64(part)
    part.add_header("Content-Disposition", f"attachment; filename={path.name}")
    return part


def create_email_message(
    email_from: str,
    email_to: str,
    html_path: Path,
    attachment_path: Path,
) -> MIMEMultipart:
    """Create the email message."""
    message = MIMEMultipart()
    message["Subject"] = "Weekly automated verschilanalyse completed"
    message["From"] = email_from
    message["To"] = email_to

    if not html_path.is_file():
        logging.warning("Could not find file with email content.")
        html_content = textwrap.dedent(
            """
            <html>
                <body>
                    <h2>Verschilanalyse finished</h2>
                    <p>
                        Unfortunately something went wrong with generating summaries.
                        Please contact `black-ops@deltares.nl` if this issue persists.
                    </p>
                </body>
            </html>
            """
        ).strip()
    else:
        html_content = html_path.read_text(encoding="utf-8")

    message.attach(MIMEText(html_content, "html"))

    if not attachment_path.is_file():
        logging.warning("Could not find file with email attachment.")
    else:
        message.attach(encode_attachment(attachment_path))

    return message


def send_email(
    message: MIMEMultipart,
    email_from: str,
    email_to: str,
    email_server: str,
    email_port: int,
) -> None:
    """Send verschilanalyse email."""
    try:
        with smtplib.SMTP(email_server, email_port) as server:
            server.sendmail(email_from, email_to, message.as_string())
        logging.info("Email sent successfully.")
    except smtplib.SMTPException as exc:
        logging.exception("Failed to send email: %s", exc)
        raise


if __name__ == "__main__":
    """Command line program to send the weekly verschilanalyse email."""
    parser = create_parser()
    arguments = parser.parse_args()

    message = create_email_message(
        email_from=arguments.email_from,
        email_to=arguments.email_to,
        html_path=arguments.email_content,
        attachment_path=arguments.attachment,
    )

    send_email(
        message=message,
        email_from=arguments.email_from,
        email_to=arguments.email_to,
        email_server=arguments.email_server,
        email_port=int(arguments.email_port),
    )
