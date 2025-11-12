#! /bin/bash

set -eo pipefail

# Import bash utility functions.
# shellcheck disable=SC1091
source "$(dirname "${BASH_SOURCE[0]}")/util.sh"

function show_help {
  cat - <<EOF
Usage: $0 -a <apptainer-image> -r <s3-path-prefix> -o <s3-path-prefix> [-m <models-path>] [-f <comma-separated list>]
-a|--apptainer oras://repo/image:tag
    Either a path to a '.sif' file or a link to a repository e.g. 'oras://<repo>/<image>:<tag>'.
-c|--current-prefix path/to/output
    The output of the verschilanalyse will be stored in this location in the verschilanalyse bucket
-r|--reference-prefix path/to/references
    The reference output is read from this location in the verschilanalyse bucket.
-m|--models-path input
    The S3 path and local directory name for model data (default: input)
-f|--model-filter grevelingen,volkerakzoommeer
    Comma-separated list of patterns. Only models with paths matching one of these patterns will be run.
EOF
}

APPTAINER=
REFERENCE_PREFIX=
CURRENT_PREFIX=
MODELS_PATH=
MODEL_FILTER=
JOB_IDS=()

parse_args() {
  local PARSED_OPTIONS
  PARSED_OPTIONS=$(getopt -o 'a:c:r:m:f:' -l 'apptainer:,current-prefix:,reference-prefix:,models-path:,model-filter:' -- "$@") || {
    show_help
    return 1
  }
  eval set -- "$PARSED_OPTIONS"

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
    --)
      shift
      break
      ;;
    *)
      show_help
      return 1
      ;;
    esac
  done
}

validate_inputs() {
  if ! util.check_vars_are_set APPTAINER REFERENCE_PREFIX CURRENT_PREFIX MODELS_PATH; then
    show_help
    return 1
  fi
}

build_model_regex() {
  if [[ -z "$MODELS_PATH" ]]; then
    MODELS_PATH="input"
  fi

  if [[ -z "$MODEL_FILTER" ]]; then
    # This regex matches all models.
    MODEL_REGEX='^.*$'
  else
    # Construct regex from MODEL_FILTER.
    MODEL_REGEX="^.*\\(${MODEL_FILTER//,/\\|}\\).*\$"
  fi
  echo "Using MODEL_REGEX: ${MODEL_REGEX}"
}

prepare_environment() {
  export CURRENT_PREFIX
  export REFERENCE_PREFIX
  export MODEL_REGEX
  export BUCKET='s3://devops-test-verschilanalyse'
  export BUILDS_DIR='/p/devops-dsc/verschilanalyse/builds'
  export VAHOME="${BUILDS_DIR}/${BUILD_ID:-latest}"
  export LOG_DIR="${VAHOME}/logs"
  export DELFT3D_SIF="${HOME}/.cache/verschilanalyse/delft3dfm.sif"
}

cleanup_old_builds() {
  # Create new build directory and remove old build directories to clear space.
  mkdir -p "$VAHOME"
  find "$BUILDS_DIR" -maxdepth 1 -type d -mtime +7 -execdir rm -rf {} +
}

prepare_modules() {
  module purge
  module load apptainer/1.2.5
}

prepare_directories() {
  if [[ -z "$LOG_DIR" || -z "$VAHOME" || -z "$MODELS_PATH" ]]; then
    echo "Error: LOG_DIR, VAHOME, or MODELS_PATH is empty" >&2
    return 1

  else
    # Create log dir and input dir.
    mkdir -p "${LOG_DIR}/models" "${VAHOME}/${MODELS_PATH}"
  fi
}

sync_input_data() {
  # Get latest input data from MinIO.
  srun --nodes=1 --ntasks=1 --cpus-per-task=16 --partition=16vcpu_spot \
    --account=verschilanalyse --qos=verschilanalyse \
    docker run --rm --volume="${HOME}/.aws:/root/.aws:ro" --volume="${VAHOME}/${MODELS_PATH}:/data" \
    docker.io/amazon/aws-cli:2.22.7 \
    --profile=verschilanalyse --endpoint-url=https://s3.deltares.nl \
    s3 sync --delete --no-progress "${BUCKET}/${MODELS_PATH}/" /data
}

download_output_data() {
  # Download reference output data.
  sbatch --parsable \
    --output="${LOG_DIR}/va-download-refs-%j.out" \
    ./jobs/download_references.sh
}

pull_apptainer_image() {
  # Pull apptainer from Harbor and store it as a `.sif` in the home directory.
  apptainer remote login \
    --username="robot\$delft3d+h7" \
    --password-stdin oras://containers.deltares.nl <"${HOME}/.harbor/delft3d"
  mkdir -p "$(dirname "$DELFT3D_SIF")"
  apptainer pull --force "$DELFT3D_SIF" "$APPTAINER"
}

submit_jobs() {
  # Find and submit all 'submit_apptainer_h7.sh' scripts.
  local SUBMIT_SCRIPTS
  SUBMIT_SCRIPTS="$(find "${VAHOME}/${MODELS_PATH}" -type f -name submit_apptainer_h7.sh -iregex "$MODEL_REGEX")"

  for SCRIPT in $SUBMIT_SCRIPTS; do
    local MODEL_DIR
    MODEL_DIR=$(echo "$SCRIPT" | sed -n -e 's|^\([-/_0-9A-Za-z]*\)/computations/.*$|\1|p')

    # To run the simulation, the working directory must be the directory containing the submit script.
    # The model directory is bind-mounted inside the apptainer. It must contain all input files.
    echo "Submitting script ${SCRIPT}"
    echo "Model directory: ${MODEL_DIR}"

    local JOB_ID
    JOB_ID=$(
      sbatch --parsable \
        --chdir="$(dirname "$SCRIPT")" \
        --output="${LOG_DIR}/models/$(basename "$MODEL_DIR").out" \
        "$SCRIPT" --apptainer "$DELFT3D_SIF" --model-dir "$MODEL_DIR"
    )
    JOB_IDS+=("$JOB_ID")
  done
}

archive_and_upload_output() {
  # Make colon-separated list of JOB_IDS.
  local JOB_ID_LIST
  JOB_ID_LIST=$(
    IFS=':'
    echo "${JOB_IDS[*]}"
  )

  # Archive and upload new output.
  sbatch --parsable \
    --output="${LOG_DIR}/va-upload-output-%j.out" \
    --dependency="afterany:${JOB_ID_LIST}" \
    ./jobs/upload_output.sh
}

run_verschillentool() {
  local DOWNLOAD_REFS_JOB_ID="$1"
  local UPLOAD_OUTPUT_JOB_ID="$2"

  sbatch --parsable \
    --output="${LOG_DIR}/va-run-verschillentool-%j.out" \
    --dependency="afterany:${DOWNLOAD_REFS_JOB_ID}:${UPLOAD_OUTPUT_JOB_ID}" \
    ./jobs/run_verschillentool.sh
}

trigger_teamcity_build() {
  local RUN_VERSCHILLENTOOL_JOB_ID="$1"
  #Trigger report build on TeamCity
  sbatch --dependency="afterany:${RUN_VERSCHILLENTOOL_JOB_ID}" ./jobs/trigger_teamcity_build.sh
}

main() {
  parse_args "$@"
  validate_inputs
  MODELS_PATH=${MODELS_PATH:-input}
  build_model_regex
  prepare_environment
  cleanup_old_builds
  prepare_modules
  prepare_directories
  sync_input_data
  DOWNLOAD_REFS_JOB_ID=$(download_output_data)
  pull_apptainer_image
  submit_jobs
  export JOB_IDS
  UPLOAD_OUTPUT_JOB_ID=$(archive_and_upload_output)
  RUN_VERSCHILLENTOOL_JOB_ID=$(run_verschillentool "$DOWNLOAD_REFS_JOB_ID" "$UPLOAD_OUTPUT_JOB_ID")
  trigger_teamcity_build "$RUN_VERSCHILLENTOOL_JOB_ID"
}

# Only run main when executed directly (not when sourced for tests)
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
  main "$@"
fi
