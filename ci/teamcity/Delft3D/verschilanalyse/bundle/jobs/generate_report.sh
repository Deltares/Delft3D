#! /bin/bash
#SBATCH --job-name=va-gen-report
#SBATCH --time=04:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=16
#SBATCH --partition=16vcpu_spot
#SBATCH --account=verschilanalyse
#SBATCH --qos=verschilanalyse

set -eo pipefail

if [[ -z "$BUCKET" || -z "$VAHOME" || -z "$OUTPUT_PREFIX" || -z "$REPORT_DIR" || -z "$MODEL_REGEX" ]]; then
    >&2 echo "Environment variables BUCKET, VAHOME, REPORT_DIR, MODEL_REGEX or OUTPUT_PREFIX not set."
    exit 1
fi

mkdir -p "${REPORT_DIR}/verschillentool_output"

# Write total computation time statistics to CSV.
./jobs/computation_times.py --directory "${VAHOME}/input" > "${REPORT_DIR}/total_computation_times.csv"

docker login \
    --username="robot\$delft3d+h7"  \
    --password-stdin \
    containers.deltares.nl < "${HOME}/.harbor/delft3d"

# Run verschillentool (all configs).
find config -name '*.json' -iregex "$MODEL_REGEX" -exec docker run --rm \
    --volume="${VAHOME}/input:/data/input:ro" \
    --volume="${VAHOME}/reference:/data/reference:ro" \
    --volume="${PWD}/{}:/app/{}:ro" \
    --volume="${REPORT_DIR}/verschillentool_output:/app/verschillentool_output" \
    containers.deltares.nl/delft3d/verschillentool:rename-station --config "{}" ';'

# Zip report dir.
pushd "$REPORT_DIR"
zip -r report.zip .
shopt -s extglob
rm -rf !(report.zip)
popd

# Upload report dir to MinIO.
docker run --rm \
    --volume="${HOME}/.aws:/root/.aws:ro" --volume="${REPORT_DIR}:/data:ro" \
    docker.io/amazon/aws-cli:2.22.7 \
    --profile=verschilanalyse --endpoint-url=https://s3.deltares.nl \
    s3 sync --delete --no-progress /data "${BUCKET}/${OUTPUT_PREFIX}/report"
