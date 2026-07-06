#! /bin/bash

set -eo pipefail

# Import bash utility functions.
# shellcheck source=ci/teamcity/Delft3D/verschilanalyse/bundle/util.sh
source util.sh

function show_help {
    cat - <<EOF
Usage: $0 -a <apptainer-image> -r <s3-path-prefix> -o <s3-path-prefix> [-m <models-path>] [-f <comma-separated list>] [-s true|false]
-a|--apptainer oras://repo/image:tag
    Either a path to a '.sif' file or a link to a repository e.g. 'oras://<repo>/<image>:<tag>'.
-c|--current-prefix path/to/output
    The output of the verschilanalyse will be stored in this location in the verschilanalyse bucket
-r|--reference-prefix path/to/references
    The reference output is read from this location in the verschilanalyse bucket.
-m|--models-path input
    The S3 path and local directory name for model data
-f|--model-filter grevelingen,volkerakzoommeer
    Comma-separated list of patterns. Only models with paths matching one of these patterns will be run.
-s|--run-models true|false
    If false, skip model execution and reuse already archived output from CURRENT_PREFIX.

EOF
}

# Parse command line options
PARSED_OPTIONS=$(getopt -o 'a:c:r:m:f:d:j:s:h' -l 'apptainer:,current-prefix:,reference-prefix:,models-path:,model-filter:,json-configs-path:,va-home:,run-models:,help' -- "$@")
eval set -- "$PARSED_OPTIONS"

APPTAINER=
REFERENCE_PREFIX=
CURRENT_PREFIX=
MODELS_PATH=
MODEL_FILTER=
JSON_CONFIGS_PATH=
VAHOME=
RUN_MODELS='true'

while true; do
    case "$1" in
    -a | --apptainer)
        APPTAINER="$2"
        shift 2
        ;;
    -c | --current-prefix)
        CURRENT_PREFIX="$2"
        shift 2
        ;;
    -r | --reference-prefix)
        REFERENCE_PREFIX="$2"
        shift 2
        ;;
    -m | --models-path)
        MODELS_PATH="$2"
        shift 2
        ;;
    -f | --model-filter)
        MODEL_FILTER="$2"
        shift 2
        ;;
    -j | --json-configs-path)
        JSON_CONFIGS_PATH="$2"
        shift 2
        ;;
    -d | --va-home)
        VAHOME="$2"
        shift 2
        ;;
    -s | --run-models)
        RUN_MODELS="$2"
        shift 2
        ;;
    -h | --help)
        show_help
        exit 0
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

if ! util.check_vars_are_set APPTAINER REFERENCE_PREFIX CURRENT_PREFIX VAHOME MODELS_PATH JSON_CONFIGS_PATH; then
    show_help
    exit 1
fi

RUN_MODELS="${RUN_MODELS,,}"
if [[ "$RUN_MODELS" != 'true' && "$RUN_MODELS" != 'false' ]]; then
    >&2 echo "Error: --run-models must be 'true' or 'false', got: $RUN_MODELS"
    show_help
    exit 1
fi
echo "RUN_MODELS=${RUN_MODELS}"

if [[ -z "$MODEL_FILTER" ]]; then
    # This regex matches all models.
    MODEL_REGEX='^.*$'
else
    # Construct regex from MODEL_FILTER.
    MODEL_REGEX="^.*\\(${MODEL_FILTER//,/\\|}\\).*\$"
fi
echo "Using MODEL_REGEX: ${MODEL_REGEX}"

export CURRENT_PREFIX
export REFERENCE_PREFIX
export MODELS_PATH
export MODEL_REGEX
export JSON_CONFIGS_PATH
export BUCKET='s3://devops-test-verschilanalyse'
export VAHOME
export LOG_DIR="${VAHOME}/logs"

DELFT3D_SIF="${HOME}/.cache/verschilanalyse/delft3dfm.sif"

module purge
module load apptainer/1.2.5

# Create log dir
mkdir -p "${LOG_DIR}/models"

# Download input data.
DOWNLOAD_INPUT_JOB_ID=$(
    sbatch --parsable \
        --output="${LOG_DIR}/va-download-input-%j.out" \
        ./jobs/download_input.sh
)

# Download reference output data.
DOWNLOAD_REFS_JOB_ID=$(
    sbatch --parsable \
        --dependency="afterany:${DOWNLOAD_INPUT_JOB_ID}" \
        --output="${LOG_DIR}/va-download-refs-%j.out" \
        ./jobs/download_references.sh
)

if [[ "$RUN_MODELS" = 'true' ]]; then
    # Pull apptainer from Harbor and store it as a `.sif` in the home directory.
    apptainer remote login \
        --username="robot\$delft3d+h7" \
        --password-stdin oras://containers.deltares.nl <"${HOME}/.harbor/delft3d"
    mkdir -p "$(dirname "$DELFT3D_SIF")"
    apptainer pull --force "$DELFT3D_SIF" "$APPTAINER"

    # Find and submit all 'submit_apptainer_h7.sh' scripts.
    JOB_IDS=()
    SUBMIT_SCRIPTS=$(find "${VAHOME}/${MODELS_PATH}" -type f -name submit_apptainer_h7.sh -iregex "$MODEL_REGEX")
    for SCRIPT in $SUBMIT_SCRIPTS; do
        MODEL_DIR=$(echo "$SCRIPT" | sed -n -e 's|^\([-/_0-9A-Za-z]*\)/computations/.*$|\1|p')

        # To run the simulation, the working directory must be the directory containing the submit script.
        # The model directory is bind-mounted inside the apptainer. It must contain all input files.
        echo "Submitting script ${SCRIPT}"
        echo "Model directory: ${MODEL_DIR}"
        JOB_ID=$(
            sbatch --parsable \
                --chdir="$(dirname "$SCRIPT")" \
                --output="${LOG_DIR}/models/$(basename "$MODEL_DIR").out" \
                "$SCRIPT" --apptainer "$DELFT3D_SIF" --model-dir "$MODEL_DIR"
        )
        JOB_IDS+=("$JOB_ID")
    done

    # Make colon-separated list of JOB_IDS.
    JOB_ID_LIST=$(
        IFS=':'
        echo "${JOB_IDS[*]}"
    )

    # Archive and upload new output.
    OUTPUT_READY_JOB_ID=$(
        sbatch --parsable \
            --output="${LOG_DIR}/va-upload-output-%j.out" \
            --dependency="afterany:${JOB_ID_LIST}" \
            ./jobs/upload_output.sh
    )
else
    # Reuse existing archived output from CURRENT_PREFIX.
    OUTPUT_READY_JOB_ID=$(
        sbatch --parsable \
            --output="${LOG_DIR}/va-download-current-output-%j.out" \
            ./jobs/download_current_output.sh
    )
fi

# Generate report.
RUN_VERSCHILLENTOOL_JOB_ID=$(
    sbatch --parsable \
        --output="${LOG_DIR}/va-run-verschillentool-%j.out" \
        --dependency="afterany:${DOWNLOAD_REFS_JOB_ID}:${OUTPUT_READY_JOB_ID}" \
        ./jobs/run_verschillentool.sh
)

# Trigger report build on TeamCity
sbatch --dependency="afterany:${RUN_VERSCHILLENTOOL_JOB_ID}" ./jobs/trigger_teamcity_build.sh
