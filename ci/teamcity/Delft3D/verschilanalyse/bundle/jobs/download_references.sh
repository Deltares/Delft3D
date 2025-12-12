#! /bin/bash
#SBATCH --job-name=va-download-refs
#SBATCH --time=04:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=16
#SBATCH --partition=16vcpu_spot
#SBATCH --account=verschilanalyse
#SBATCH --qos=verschilanalyse

set -eo pipefail

if ! util.check_vars_are_set BUCKET REFERENCE_PREFIX VAHOME ; then
    >&2 echo "Abort"
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

docker run --rm \
    --volume="${HOME}/.aws:/root/.aws:ro" --volume="${ARCHIVE_DIR}:/data" \
    docker.io/amazon/aws-cli:2.32.14 \
    --profile=verschilanalyse --endpoint-url=https://s3.deltares.nl \
    s3 sync --delete --no-progress "${BUCKET}/${REFERENCE_PREFIX}/output" /data

find "$ARCHIVE_DIR" -iname '*.zip' -print0 \
    | xargs -0 -I'{}' -P8 bash -c 'unzip_references "{}"'
