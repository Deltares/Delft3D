#! /bin/bash
#SBATCH --job-name=va-download-current-output
#SBATCH --time=04:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=16
#SBATCH --partition=16vcpu_spot
#SBATCH --account=verschilanalyse
#SBATCH --qos=verschilanalyse

set -eo pipefail

if ! util.check_vars_are_set BUCKET CURRENT_PREFIX VAHOME MODELS_PATH; then
    >&2 echo "Abort"
    exit 1
fi

export CURRENT_OUTPUT_ARCHIVE_DIR="${VAHOME}/current_output_archive"

function unzip_current_output {
    # Unzip one archived model output into its model directory.
    local zip_path="$1"
    local model_name
    model_name="$(basename -s .zip "$zip_path")"
    local model_dir
    model_dir="${VAHOME}/${MODELS_PATH}/${model_name}"

    mkdir -p "$model_dir"
    unzip -o "$zip_path" -d "$model_dir"
}
export -f unzip_current_output

rm -rf "$CURRENT_OUTPUT_ARCHIVE_DIR"
mkdir -p "$CURRENT_OUTPUT_ARCHIVE_DIR"

# Download archived output from CURRENT_PREFIX.
docker run --rm \
    --volume="${HOME}/.aws:/root/.aws:ro" --volume="${CURRENT_OUTPUT_ARCHIVE_DIR}:/data" \
    -e AWS_CA_BUNDLE="/etc/pki/tls/cert.pem" \
    docker.io/amazon/aws-cli:2.32.14 \
    --profile=verschilanalyse --endpoint-url=https://s3.deltares.nl \
    s3 sync --delete --no-progress "${BUCKET}/${CURRENT_PREFIX}/output" /data

# Unpack each model archive in the local model tree.
find "$CURRENT_OUTPUT_ARCHIVE_DIR" -iname '*.zip' -print0 |
    xargs -0 -I'{}' -P8 bash -c 'unzip_current_output "{}"'

#rm -rf "$CURRENT_OUTPUT_ARCHIVE_DIR"
