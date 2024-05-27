"""Minio command line tool.

Usage: from within `test/deltares_testbench`:
`python -m tools.minio --help`
"""

import sys
from pathlib import Path

from tools.minio.argument_parser import make_argument_parser
from tools.minio.minio_tool import ErrorCode, MinioAuthError, MinioToolError

if len(sys.argv) <= 1:
    sys.argv.append("--help")
parser = make_argument_parser()
args = parser.parse_args()

try:
    cred_path = (Path("~") / ".aws" / "credentials").expanduser()
    if not cred_path.exists():
        raise MinioAuthError("MinIO credentials file not found")
    args.tool(args)
except KeyboardInterrupt:
    print("\nReceived Keyboard interrupt.", file=sys.stderr)
    exit(1)
except MinioToolError as exc:
    if exc.code == ErrorCode.AUTH:
        print(f'Authentication error: {exc.message.rstrip(".")}.', file=sys.stderr)
        print(
            "\n".join(
                [
                    f"To authenticate, create a 'credentials' file in your home directory: {cred_path.expanduser()}",
                    "Example content of the credentials file:",
                    "[default]",
                    "aws_access_key_id = $ACCESS_KEY_ID",
                    "aws_secret_access_key = $SECRET_ACCESS_KEY",
                    "",
                    "Where $ACCESS_KEY_ID and $SECRET_ACCESS_KEY are your personal access and secret keys.",
                    "Create your own personal keys for MinIO at: https://s3-console.deltares.nl",
                    "Details: https://docs.aws.amazon.com/cli/latest/userguide/cli-configure-files.html",
                ]
            ),
            file=sys.stderr,
        )
    else:
        print(f'Error: {exc.message.rstrip(".")}.', file=sys.stderr)
    exit(1)
