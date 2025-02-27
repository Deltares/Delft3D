#! /bin/bash

set -eo pipefail

function show_help {
    cat - <<EOF
Usage: $0 -a <apptainer-image> -r <s3-path-prefix> -o <s3-path-prefix> [-f <comma-separated list>]
-a|--apptainer oras://repo/image:tag
    Either a path to a '.sif' file or a link to a repository e.g. 'oras://<repo>/<image>:<tag>'.
-r|--reference-prefix path/to/references
    The reference output is read from this location in the verschilanalyse bucket.
-o|--output-prefix path/to/output
    The output of the verschilanalyse will be stored in this location in the verschilanalyse bucket
-f|--model-filter grevelingen,volkerakzoommeer
    Comma-separated list of patterns. Only models with paths matching one of these patterns will be run.
EOF
}


# Parse command line options
PARSED_OPTIONS=$(getopt -o 'a:r:o:f:' -l 'apptainer:,reference-prefix:,output-prefix:,model-filter:' -- "$@")
eval set -- "$PARSED_OPTIONS"

APPTAINER=
REFERENCE_PREFIX=
OUTPUT_PREFIX=
MODEL_FILTER=

while true; do
    case "$1" in
        -a|--apptainer)
            APPTAINER="$2"
            shift 2
            ;;
        -r|--reference-prefix)
            REFERENCE_PREFIX="$2"
            shift 2
            ;;
        -o|--output-prefix)
            OUTPUT_PREFIX="$2"
            shift 2
            ;;
        -f|--model-filter)
            MODEL_FILTER="$2"
            shift 2
            ;;
        --)
            shift
            break
            ;;
        *)
            show_help
            exit 1
            ;;
    esac
done

if [[ -z "$APPTAINER" || -z "$REFERENCE_PREFIX" || -z "$OUTPUT_PREFIX" ]]; then
    show_help
    exit 1
fi

if [[ -z "$MODEL_FILTER" ]]; then
    # This regex matches all models.
    MODEL_REGEX='^.*$'
else
    # Construct regex from MODEL_FILTER.
    MODEL_REGEX="^.*\\(${MODEL_FILTER//,/\\|}\\).*\$"
fi


export OUTPUT_PREFIX
export REFERENCE_PREFIX
export MODEL_REGEX
export BUCKET='s3://devops-test-verschilanalyse'
export VAHOME='/p/devops-dsc/verschilanalyse'
export REPORT_DIR="${VAHOME}/report"

DELFT3D_SIF="${HOME}/.cache/verschilanalyse/delft3dfm.sif"

module purge
module load apptainer/1.2.5

# Clean-up report dir
rm -rf "$REPORT_DIR"
mkdir -p "${REPORT_DIR}/logs"

# Get latest input data from MinIO.
srun --nodes=1 --ntasks=1 --cpus-per-task=4 --partition=4vcpu \
    docker run --rm --volume="${HOME}/.aws:/root/.aws:ro" --volume="${VAHOME}/input:/data" \
    docker.io/amazon/aws-cli:2.22.7 \
    --profile=verschilanalyse --endpoint-url=https://s3.deltares.nl \
    s3 sync --delete --no-progress "${BUCKET}/input/" /data

# Download reference output data.
SYNC_REFS_JOB_ID=$(sbatch --parsable ./jobs/sync_model_references.sh)

# Pull apptainer from Harbor and store it as a `.sif` in the home directory.
apptainer remote login \
    --username="robot\$delft3d+h7" \
    --password-stdin oras://containers.deltares.nl < "${HOME}/.harbor/delft3d"
mkdir -p "$(dirname "$DELFT3D_SIF")"
apptainer pull --force "$DELFT3D_SIF" "$APPTAINER"

# Submit all simulations.
JOB_IDS=()
SUBMIT_SCRIPTS=$(find "${VAHOME}/input" -type f -name submit_apptainer_h7.sh -iregex "$MODEL_REGEX")
for SCRIPT in $SUBMIT_SCRIPTS; do
    MODEL_DIR=$(realpath -s --relative-to="${VAHOME}/input" "$SCRIPT" | cut -d'/' -f1)
    JOB_ID=$( \
        sbatch --parsable --chdir="$(dirname "$SCRIPT")" --output="${REPORT_DIR}/logs/va-${MODEL_DIR}-%j.out" \
            "$SCRIPT" --apptainer "$DELFT3D_SIF" --model-dir "${VAHOME}/input/${MODEL_DIR}" \
    )
    JOB_IDS+=("$JOB_ID")
done

# Make colon-separated list of JOB_IDS.
JOB_ID_LIST=$(IFS=':'; echo "${JOB_IDS[*]}")

# Archive and upload new output.
UPLOAD_OUTPUT_JOB_ID=$(sbatch --parsable --dependency="afterany:${JOB_ID_LIST}" ./jobs/upload_output.sh)

# Generate report.
GEN_REPORT_JOB_ID=$( \
    sbatch --parsable \
        --dependency="afterany:${SYNC_REFS_JOB_ID}:${UPLOAD_OUTPUT_JOB_ID}" \
        ./jobs/generate_report.sh \
)

# Trigger report build on TeamCity
sbatch --dependency="afterany:${GEN_REPORT_JOB_ID}" ./jobs/trigger_teamcity_build.sh
