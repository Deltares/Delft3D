#! /bin/bash
#SBATCH --job-name=va-sync-refs
#SBATCH --output=/p/devops-dsc/verschilanalyse/report/logs/va-sync-refs-%j.out
#SBATCH --time=04:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --partition=16vcpu
#SBATCH --cpus-per-task=16

set -eo pipefail

if [[ -z "$BUCKET" || -z "$REFERENCE_PREFIX" || -z "$VAHOME" ]]; then
    >&2 echo "Environment variables BUCKET, REFERENCE_PREFIX or VAHOME not set."
    exit 1
fi

ARCHIVE_DIR="${VAHOME}/archive"
export REFERENCE_DIR="${VAHOME}/reference"

function unzip_references {
    # Unzip a zip file in a directory with the same name in the references directory.
    # Expects the path to a zip file as its only argument.
    unzip "$1" -d "${REFERENCE_DIR}/$(basename -s .zip "$1")"
}
export -f unzip_references

# Clean up reference dir, but keep archive dir on the p-drive.
rm -rf "$REFERENCE_DIR"
mkdir -p "$ARCHIVE_DIR" "$REFERENCE_DIR"

aws --profile=verschilanalyse --endpoint-url=https://s3.deltares.nl \
    s3 sync --delete --no-progress "${BUCKET}/${REFERENCE_PREFIX}" "$ARCHIVE_DIR"

find "$ARCHIVE_DIR" -iname '*.zip' -print0 \
    | xargs -0 -I'{}' -P8 bash -c 'unzip_references "{}"'
