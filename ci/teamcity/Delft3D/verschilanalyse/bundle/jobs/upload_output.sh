#! /bin/bash
#SBATCH --job-name=va-upload-output
#SBATCH --output=/p/devops-dsc/verschilanalyse/report/logs/va-upload-output-%j.out
#SBATCH --time=04:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --partition=16vcpu
#SBATCH --cpus-per-task=16

set -eo pipefail

if [[ -z "$BUCKET" || -z "$OUTPUT_PREFIX" || -z "$VAHOME" ]]; then
    >&2 echo "Environment variables BUCKET, OUTPUT_PREFIX or VAHOME not set."
    exit 1
fi

export TMP_ARCHIVE_DIR="${VAHOME}/tmp_archive"

function zip_output {
    # Go into a directory and zip its contents into the archive directory.
    # Expects the directory as it's only argument.
    pushd "$1"
    # Create zip
    ZIP_PATH="${TMP_ARCHIVE_DIR}/$(basename "$1").zip"
    zip -r "$ZIP_PATH" . \
        -i '*_map.nc' -i '*_his.nc' -i '*_fou.nc' \
        -i '*.dia' -i '*.out' -i '*.tek'
    
    # Remove zip if it is empty
    if ! zipinfo "$ZIP_PATH" | grep -q '^-' ; then
        echo "removing empty zip: $ZIP_PATH"
        rm -f "$ZIP_PATH"
    fi
    popd
}
export -f zip_output

rm -rf "$TMP_ARCHIVE_DIR"
mkdir -p "$TMP_ARCHIVE_DIR"

# Archive all output per individual (non-empty) model directory.
find "${VAHOME}/input" -mindepth 1 -maxdepth 1 -type d '!' -empty -print0 \
    | xargs -0 -P8 -I'{}' bash -c 'zip_output "{}"'

# Upload the archives to MinIO.
aws --profile=verschilanalyse --endpoint-url=https://s3.deltares.nl \
    s3 sync --delete --no-progress "$TMP_ARCHIVE_DIR" "${BUCKET}/${OUTPUT_PREFIX}/output"

rm -rf "$TMP_ARCHIVE_DIR"
