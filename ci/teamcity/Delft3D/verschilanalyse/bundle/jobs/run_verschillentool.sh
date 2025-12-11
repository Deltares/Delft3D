#! /bin/bash
#SBATCH --job-name=va-run-verschillentool
#SBATCH --time=04:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=16
#SBATCH --partition=16vcpu_spot
#SBATCH --account=verschilanalyse
#SBATCH --qos=verschilanalyse

set -eo pipefail

if ! util.check_vars_are_set BUCKET VAHOME CURRENT_PREFIX REFERENCE_PREFIX MODEL_REGEX ; then
    >&2 echo "Abort"
    exit 1
fi

VERSCHILLENTOOL_DIR="${VAHOME}/verschillentool"
rm -rf "$VERSCHILLENTOOL_DIR"
mkdir "$VERSCHILLENTOOL_DIR"

docker login \
    --username="robot\$verschillentool+h7"  \
    --password-stdin \
    containers.deltares.nl < "${HOME}/.harbor/verschillentool"

# Run verschillentool (all configs).
find config -name '*.json' -iregex "$MODEL_REGEX" -exec docker run --rm \
    --volume="${VAHOME}/input:/data/input:ro" \
    --volume="${VAHOME}/reference:/data/reference:ro" \
    --volume="${PWD}/{}:/data/{}:ro" \
    --volume="${VERSCHILLENTOOL_DIR}:/data/verschillentool" \
    containers.deltares.nl/verschillentool/verschillentool:release_v1.0.2 --config "/data/{}" ';'

# Use the last part of the REFERENCE_PREFIX as the REFERENCE_TAG
REFERENCE_TAG="${REFERENCE_PREFIX##*/}"

# Create verschillen archive.
pushd "$VERSCHILLENTOOL_DIR"
zip -r "verschillen.zip" .
shopt -s extglob
rm -rf !(verschillen.zip)
popd

# Upload verschillen archive to MinIO.
docker run --rm \
    --volume="${HOME}/.aws:/root/.aws:ro" --volume="${VERSCHILLENTOOL_DIR}:/data:ro" \
    docker.io/amazon/aws-cli:2.32.14 \
    --profile=verschilanalyse --endpoint-url=https://s3.deltares.nl \
    s3 sync --delete --no-progress /data "${BUCKET}/${CURRENT_PREFIX}/verschillentool/${REFERENCE_TAG}"
