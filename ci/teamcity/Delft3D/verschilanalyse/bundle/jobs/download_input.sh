#! /bin/bash
#SBATCH --job-name=va-download-input
#SBATCH --time=04:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=16
#SBATCH --partition=16vcpu_spot
#SBATCH --account=verschilanalyse
#SBATCH --qos=verschilanalyse

set -eo pipefail

if ! util.check_vars_are_set BUCKET VAHOME MODELS_PATH JSON_CONFIGS_PATH; then
    >&2 echo "Abort"
    exit 1
fi

# Create the model input and verschillentool configs dirs
mkdir -p "${VAHOME}/${MODELS_PATH}" "${VAHOME}/${JSON_CONFIGS_PATH}"

# Get the model input and verschillentool configs from MinIO.
minio_paths=("${MODELS_PATH}" "${JSON_CONFIGS_PATH}")
for path in "${minio_paths[@]}"; do
    docker run \
        --rm \
        --volume="${HOME}/.aws:/root/.aws:ro" \
        --volume="${VAHOME}/${path}:/data" \
        --env AWS_CA_BUNDLE="/etc/pki/tls/cert.pem" \
        docker.io/amazon/aws-cli:2.32.14 \
        --profile=verschilanalyse \
        --endpoint-url=https://s3.deltares.nl \
        s3 sync --delete --no-progress "${BUCKET}/${path}/" /data
done
