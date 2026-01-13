#!/usr/bin/env bash
# shellcheck disable=SC2329,SC2034,SC1091

function set_up_before_script() {
  source "$ROOT_DIR/../bundle/start_verschilanalyse.sh"

  # ------ Base vars ------ #
  declare -g HOME_DIR="$HOME"
  declare -g TMP_DIR
  TMP_DIR=$(mktemp -d)
  declare -g BUILD_ID="${BUILD_ID:-latest}"
  declare -g DELFT3D_SIF="${HOME_DIR}/.cache/verschilanalyse/delft3dfm.sif"
  declare -g JOB_ID=()

  # ------ Directory vars ------ #
  declare -g BUILDS_DIR="$TMP_DIR/builds"
  declare -g VAHOME="${BUILDS_DIR}/${BUILD_ID}"
  declare -g LOG_DIR="${VAHOME}/logs"

  # ------ Model input & prefix vars ------ #
  declare -g CURRENT_PREFIX="$TMP_DIR/current"
  declare -g REFERENCE_PREFIX="$TMP_DIR/reference"
  declare -g MODELS_PATH=""
  declare -g MODEL_FILTER=""

  # ------ External vars ------ #
  declare -g BUCKET='s3://devops-test-verschilanalyse'
  declare -g APPTAINER="oras://registry.local/image:tag"
  
  spy apptainer
  spy module
  spy sbatch
  spy srun

}

function tear_down_after_script() {
  rm -rf "$TMP_DIR"
}

# ------ parse_args tests ------ #
function test_parse_args_optional_unset_args() {
  parse_args \
    -a "$APPTAINER" \
    -c "$CURRENT_PREFIX" \
    -r "$REFERENCE_PREFIX" \
    -m "$MODELS_PATH" 

  assert_empty "$MODEL_FILTER"
  assert_exit_code 0
}

function test_parse_args_long_options() {
  parse_args \
    --apptainer "oras://repo/image:tag" \
    --current-prefix "$TMP_DIR/prefix/current" \
    --reference-prefix "$TMP_DIR/prefix/reference" \
    --models-path "$TMP_DIR/models" \
    --model-filter "grevelingen"

  assert_equals "oras://repo/image:tag" "$APPTAINER"
  assert_equals "$TMP_DIR/prefix/current" "$CURRENT_PREFIX"
  assert_equals "$TMP_DIR/prefix/reference" "$REFERENCE_PREFIX"
  assert_equals "$TMP_DIR/models" "$MODELS_PATH"
  assert_equals "grevelingen" "$MODEL_FILTER"
  assert_exit_code 0
}

function test_parse_args_order_independent() {
  parse_args \
    -m "$TMP_DIR/models" \
    -r "$TMP_DIR/prefix/reference" \
    -a "oras://repo/image:tag" \
    -f "grevelingen,volkerakzoommeer" \
    -c "$TMP_DIR/prefix/current"

  assert_equals "oras://repo/image:tag" "$APPTAINER"
  assert_equals "$TMP_DIR/prefix/current" "$CURRENT_PREFIX"
  assert_equals "$TMP_DIR/prefix/reference" "$REFERENCE_PREFIX"
  assert_equals "$TMP_DIR/models" "$MODELS_PATH"
  assert_equals "grevelingen,volkerakzoommeer" "$MODEL_FILTER"
  assert_exit_code 0
}


# ------ parse_args with validate_inputs tests ------ #
function test_parse_args_sets_expected_default_args() {
  parse_args \
    -a "oras://repo/image:tag" \
    -c "$TMP_DIR/prefix/current" \
    -r "$TMP_DIR/prefix/reference" \
    -m "$TMP_DIR/models" \
    -f ""

  validate_inputs
  assert_equals "oras://repo/image:tag" "$APPTAINER"
  assert_equals "$TMP_DIR/prefix/current" "$CURRENT_PREFIX"
  assert_equals "$TMP_DIR/prefix/reference" "$REFERENCE_PREFIX"
  assert_equals "$TMP_DIR/models" "$MODELS_PATH"
  assert_equals "" "$MODEL_FILTER"
  assert_exit_code 0

}

function test_parse_args_missing_required_args() {
  parse_args \
    -a "oras://repo/image:tag" \
    -c "$TMP_DIR/prefix/current" \
    -r "$TMP_DIR/prefix/reference" 

  validate_inputs
  assert_exit_code 1
}

function test_parse_args_missing_args_value() {
  parse_args \
    -a "" \
    -c "$TMP_DIR/prefix/current" \
    -r "$TMP_DIR/prefix/reference" \
    -m "$TMP_DIR/models" \
    -f "grevelingen,volkerakzoommeer"

  validate_inputs
  assert_exit_code 1
}

function test_parse_args_with_unexpected_args() {
  parse_args -x "unknown"
  validate_inputs
  assert_exit_code 1
}

function test_validate_inputs_fails_with_empty_vars() {
  validate_inputs
  assert_exit_code 1
}


# ------ build_model_regex tests ------ #
function test_build_model_regex_default() {
  build_model_regex
  assert_equals "^.*$" "$MODEL_REGEX"
  assert_exit_code 0
}

function test_build_model_regex_with_filter_containing_single_model() {
  local MODEL_FILTER="grevelingen"
  build_model_regex
  assert_equals "^.*\\($MODEL_FILTER\\).*\$" "$MODEL_REGEX"
  assert_exit_code 0
}

function test_build_model_regex_with_filter_containing_multiple_models() {
  local MODEL_FILTER="grevelingen,volkerakzoommeer"
  build_model_regex
  assert_equals "^.*\\(grevelingen\\|volkerakzoommeer\\).*\$" "$MODEL_REGEX"
  assert_exit_code 0
}

function test_build_model_regex_default_path(){
  build_model_regex
  assert_equals "input" "$MODELS_PATH"
  assert_exit_code 0
}


# ------ prepare_environment tests ------ #
function test_prepare_environment_exports_variables() {
  prepare_environment

  assert_equals "s3://devops-test-verschilanalyse" "$BUCKET"
  assert_equals "/p/devops-dsc/verschilanalyse/builds" "$BUILDS_DIR"
  assert_equals "/p/devops-dsc/verschilanalyse/builds/latest" "$VAHOME"
  assert_equals "$VAHOME/logs" "$LOG_DIR"
  assert_equals "$HOME/.cache/verschilanalyse/delft3dfm.sif" "$DELFT3D_SIF"
  assert_exit_code 0
}

function test_prepare_environment_with_build_id() {
  local BUILD_ID="12345"

  prepare_environment

  assert_equals "/p/devops-dsc/verschilanalyse/builds/$BUILD_ID" "$VAHOME"
  assert_equals "$VAHOME/logs" "$LOG_DIR"
  assert_exit_code 0
}

function test_prepare_environment_preserve_existing_exports() {
  # Load MODEL_REGEX and MODEL_PATH
  build_model_regex
  prepare_environment

  assert_equals "$TMP_DIR/current" "$CURRENT_PREFIX"
  assert_equals "$TMP_DIR/reference" "$REFERENCE_PREFIX"
  assert_equals "^.*$" "$MODEL_REGEX"
  assert_exit_code 0
}

function test_prepare_environment_with_single_model_filter() {
  local MODEL_FILTER="grevelingen"

  # Load MODEL_REGEX and MODEL_PATH
  build_model_regex
  prepare_environment

  assert_equals "$TMP_DIR/current" "$CURRENT_PREFIX"
  assert_equals "$TMP_DIR/reference" "$REFERENCE_PREFIX"
  assert_equals "^.*\\(grevelingen\\).*$" "$MODEL_REGEX"
  assert_exit_code 0
}

function test_prepare_environment_with_multiple_model_filter() {
  local MODEL_FILTER="grevelingen,volkerakzoommeer"

  # Load MODEL_REGEX and MODEL_PATH
  build_model_regex
  prepare_environment

  assert_equals "$TMP_DIR/current" "$CURRENT_PREFIX"
  assert_equals "$TMP_DIR/reference" "$REFERENCE_PREFIX"
  assert_equals "^.*\\(grevelingen\\|volkerakzoommeer\\).*$" "$MODEL_REGEX"
  assert_exit_code 0
}


# ------ cleanup_old_builds tests ------ #
function test_cleanup_old_builds_checks_existing_directories() {
  cleanup_old_builds

  assert_directory_exists "$VAHOME"
  assert_directory_exists "$BUILDS_DIR"
  assert_exit_code 0 
}

function test_cleanup_old_builds_checks_existing_directories_with_build_id() {
  local BUILD_ID="12345"

  cleanup_old_builds

  assert_directory_exists "$BUILDS_DIR"
  assert_directory_exists "$VAHOME"
  assert_exit_code 0 
}

function test_cleanup_old_builds_removes_old_directories() {
  local OLD_DIR="$BUILDS_DIR/old_dir"
  
  mkdir -p "$OLD_DIR"
  # Make directory older than 7 days
  touch -d '8 days ago' "$OLD_DIR"

  assert_directory_exists "$OLD_DIR"

  cleanup_old_builds

  assert_directory_not_exists "$OLD_DIR"
  assert_exit_code 0 
}

function test_cleanup_old_builds_keeps_recent_directories() {
  local RECENT_DIR="$BUILDS_DIR/recent_dir"
  
  mkdir -p "$RECENT_DIR"
  touch -d '4 days ago' "$RECENT_DIR"

  assert_directory_exists "$RECENT_DIR"

  cleanup_old_builds

  assert_directory_exists "$RECENT_DIR"
  assert_exit_code 0 
}

function test_cleanup_old_builds_creates_missing_builds_dir() {
  rm -rf "$BUILDS_DIR"

  cleanup_old_builds

  assert_directory_exists "$BUILDS_DIR"
  assert_directory_exists "$VAHOME"
  assert_exit_code 0 
}

function test_cleanup_old_builds_with_empty_builds_dir() {
  rm -rf "$BUILDS_DIR"
  mkdir -p "$BUILDS_DIR"

  cleanup_old_builds

  assert_exit_code 0
}

function test_cleanup_old_builds_with_non_existings_builds_dir() {
  local BUILDS_DIR=""
  local VAHOME=""

  cleanup_old_builds

  assert_exit_code 1
}

# ------ prepare_modules tests ------ #
function test_prepare_modules_calls_module_commands_mock() {
  local LOG_FILE="$TMP_DIR/module_mock.log"

  # Logs about what would have been executed.
  mock module "echo module \$* >> $LOG_FILE"

  prepare_modules

  local LOG
  LOG=$(cat "$LOG_FILE")

  assert_not_empty  "$LOG_FILE"
  assert_matches "purge" "$LOG"
  assert_matches "load apptainer/1.2.5" "$LOG"
  assert_exit_code 0
}

# ------ prepare_directories tests ------ #
function test_prepare_directories_with_empty_models_path() {
  # Build model args
  build_model_regex
  prepare_directories

  assert_directory_exists "$LOG_DIR/models"
  assert_directory_exists "$VAHOME/input"

}

function test_prepare_directories_with_build_id() {
  local BUILD_ID="12345"

  prepare_directories

  assert_directory_exists "$LOG_DIR/models"
  assert_directory_exists "$VAHOME/input"

}

function test_prepare_directories_with_empty_required_vars() {
  local LOG_DIR=""
  local VAHOME=""
  local MODELS_PATH=""

  prepare_directories

  assert_exit_code 1
}


# ------ sync_input_data tests ------ #
function test_sync_input_data_spy() {
  # Create input dir
  prepare_directories
  sync_input_data

  assert_have_been_called srun
  assert_have_been_called_with "--nodes=1 --ntasks=1 --cpus-per-task=16 --partition=16vcpu_spot --account=verschilanalyse --qos=verschilanalyse docker run --rm --volume=${HOME}/.aws:${HOME}/.aws:ro --volume=${VAHOME}/${MODELS_PATH}:/data docker.io/amazon/aws-cli:2.22.7 --profile=verschilanalyse --endpoint-url=https://s3.deltares.nl s3 sync --delete --no-progress ${BUCKET}/${MODELS_PATH}/ /data" srun
  assert_have_been_called_times 1 srun
  assert_exit_code 0
}

function test_sync_input_data_mock_srun() {
  local LOG_FILE="$TMP_DIR/sync.log"

  # Logs about what would have been executed.
  mock srun "echo srun \$* >> $LOG_FILE"

   # Build model args
  build_model_regex
  sync_input_data

  local LOG
  LOG=$(cat "$LOG_FILE")

  assert_not_empty  "$LOG_FILE"
  assert_matches "srun --nodes=1 --ntasks=1 --cpus-per-task=16 --partition=16vcpu_spot" "$LOG"
  assert_matches "--account=verschilanalyse --qos=verschilanalyse" "$LOG"
  assert_matches "docker run --rm --volume=${HOME}/.aws:${HOME}/.aws:ro" "$LOG"
  assert_matches "--volume=${VAHOME}/${MODELS_PATH}:/data docker.io/amazon/aws-cli:2.22.7" "$LOG"
  assert_matches "--profile=verschilanalyse --endpoint-url=https://s3.deltares.nl" "$LOG"
  assert_matches "s3 sync --delete --no-progress ${BUCKET}/${MODELS_PATH}/ /data" "$LOG"
  assert_exit_code 0
}

function test_sync_input_data_mock_docker() {
  local LOG_FILE="$TMP_DIR/sync.log"

  # Logs about what would have been executed.
  mock docker "echo docker \$* >> $LOG_FILE"

   # Build model args
  build_model_regex
  sync_input_data

  local LOG
  LOG=$(cat "$LOG_FILE")

  assert_not_empty  "$LOG_FILE"
  assert_matches "docker run --rm" "$LOG"
  assert_matches "--volume=${HOME_DIR}/.aws:/root/.aws:ro" "$LOG"
  assert_matches "s3 sync --delete --no-progress" "$LOG"
  assert_matches "${BUCKET}/${MODELS_PATH}/" "$LOG"
  assert_exit_code 0
}


# ------ download_output_data tests ------ #
function test_download_output_data_default_mock() {
  local LOG_FILE="$TMP_DIR/download_output_data_sbatch.log"

  mock sbatch "echo sbatch \$* >> $LOG_FILE"

  download_output_data

  local LOG
  LOG=$(cat "$LOG_FILE")

  assert_not_empty  "$LOG_FILE"
  assert_matches "--parsable" "$LOG"
  assert_matches "--output=$LOG_DIR/va-download-refs-%j.out" "$LOG"
  assert_matches "./jobs/download_references.sh" "$LOG"
  assert_exit_code 0
}


# ------ pull_apptainer_image tests ------ #
function test_pull_apptainer_image_spy() {
  mkdir -p "${HOME_DIR}/.harbor"
  echo "dummy-password" > "${HOME_DIR}/.harbor/delft3d"

  # Build model args
  build_model_regex

  pull_apptainer_image

  assert_have_been_called apptainer
  assert_have_been_called_with "pull --force ${DELFT3D_SIF} ${APPTAINER}" apptainer
  assert_have_been_called_times 2 apptainer
}

function test_pull_apptainer_image_mock() {
  local LOG_FILE="$TMP_DIR/apptainer.log"
  
  # Logs about what would have been executed.
  mock apptainer "echo apptainer \$* >> $LOG_FILE"

  # Build model args
  build_model_regex

  pull_apptainer_image

  local LOG
  LOG=$(cat "$LOG_FILE")

  assert_not_empty  "$LOG_FILE"
  assert_matches "remote login --username=robot\\\$delft3d\+h7" "$LOG"
  assert_matches "--password-stdin oras://containers.deltares.nl" "$LOG"
  assert_matches "pull --force ${DELFT3D_SIF} ${APPTAINER}" "$LOG"
  assert_exit_code 0
}


# ------ submit_jobs tests ------ #
function test_submit_jobs_spy_sbatch() {
  # Build model args
  build_model_regex

  local MODEL_DIR="${VAHOME}/${MODELS_PATH}/model1"
  local SCRIPT_DIR="$VAHOME/$MODELS_PATH/model1/computations/test"

  mkdir -p "$SCRIPT_DIR"
  echo "#!/bin/bash" > "$SCRIPT_DIR/submit_apptainer_h7.sh"
  mock sed "echo ${MODEL_DIR}"
  
  submit_jobs

  assert_have_been_called sbatch
  assert_have_been_called_times 1 sbatch
  assert_have_been_called_with "--parsable --chdir=$SCRIPT_DIR --output=$LOG_DIR/models/model1.out $SCRIPT_DIR/submit_apptainer_h7.sh --apptainer /root/.cache/verschilanalyse/delft3dfm.sif --model-dir $MODEL_DIR" sbatch
  assert_exit_code 0
}

function test_submit_jobs_mock_sbatch() {
  local LOG_FILE="$TMP_DIR/sbatch.log"

  # Logs about what would have been executed.
  mock sbatch "echo sbatch \$* >> $LOG_FILE"

  # Build model args
  build_model_regex

  local MODEL_DIR="${VAHOME}/${MODELS_PATH}/model1"
  local SCRIPT_DIR="$MODEL_DIR/computations/test"

  mkdir -p "$SCRIPT_DIR"
  echo "#!/bin/bash" > "$SCRIPT_DIR/submit_apptainer_h7.sh"
  mock sed "echo ${MODEL_DIR}"
  
  submit_jobs

  local LOG
  LOG=$(cat "$LOG_FILE")

  assert_not_empty  "$LOG_FILE"
  assert_matches "--parsable" "$LOG"
  assert_matches "--chdir=${SCRIPT_DIR}" "$LOG"
  assert_matches "--output=${LOG_DIR}/models/model1.out" "$LOG"
  assert_matches "submit_apptainer_h7.sh" "$LOG"
  assert_matches "--apptainer ${DELFT3D_SIF}" "$LOG"
  assert_matches "--model-dir ${MODEL_DIR}" "$LOG"
  assert_exit_code 0
}

function test_submit_jobs_spy_find() {
  spy find

  # Build model args
  build_model_regex

  local SCRIPT_DIR="$VAHOME/$MODELS_PATH/model1/computations/test"

  mkdir -p "$SCRIPT_DIR"
  echo "#!/bin/bash" > "$SCRIPT_DIR/submit_apptainer_h7.sh"
  submit_jobs

  assert_have_been_called find
  assert_have_been_called_times 1 find
  assert_exit_code 0
}

function test_submit_jobs_mock_find_empty_model_filter() {
  local MODEL_FILTER="" # Ensure MODEL_FILTER is empty
  local LOG_FILE="$TMP_DIR/find_empty_model_filter.log"
  
  # Logs about what would have been executed.
  mock find "echo find \$* >> $LOG_FILE"

  # Build model args
  build_model_regex

  local SCRIPT_DIR="$VAHOME/$MODELS_PATH/model1/computations/test"

  mkdir -p "$SCRIPT_DIR"
  echo "#!/bin/bash" > "$SCRIPT_DIR/submit_apptainer_h7.sh"
  submit_jobs

  local LOG
  LOG=$(cat "$LOG_FILE")

  assert_not_empty  "$LOG_FILE"
  assert_matches "${VAHOME}/${MODELS_PATH}" "$LOG"
  assert_matches "-type f" "$LOG"
  assert_matches "-name submit_apptainer_h7.sh" "$LOG"
  assert_matches '-iregex [\^]\.\*[$]' "$LOG"
  assert_exit_code 0
}

function test_submit_jobs_mock_find_single_model_filter() {
  local MODEL_FILTER="grevelingen"
  local LOG_FILE="$TMP_DIR/find_single_model_filter.log"
  
  # Logs about what would have been executed.
  mock find "echo find \$* >> $LOG_FILE"

  # Build model args
  build_model_regex

  local SCRIPT_DIR="$VAHOME/$MODELS_PATH/model1/computations/test"

  mkdir -p "$SCRIPT_DIR"
  echo "#!/bin/bash" > "$SCRIPT_DIR/submit_apptainer_h7.sh"
  submit_jobs

  local LOG
  LOG=$(cat "$LOG_FILE")

  assert_not_empty  "$LOG_FILE"
  assert_matches "${VAHOME}/${MODELS_PATH}" "$LOG"
  assert_matches "-type f" "$LOG"
  assert_matches "-name submit_apptainer_h7.sh" "$LOG"
  assert_contains "-iregex ^.*\\($MODEL_FILTER\\).*$" "$LOG"
  assert_exit_code 0
}

function test_submit_jobs_mock_find_multiple_model_filter() {
  local MODEL_FILTER="grevelingen,volkerakzoommeer"
  local LOG_FILE="$TMP_DIR/find_multiple_model_filter.log"
  
  # Logs about what would have been executed.
  mock find "echo find \$* >> $LOG_FILE"

  # Build model args
  build_model_regex

  local SCRIPT_DIR="$VAHOME/$MODELS_PATH/model1/computations/test"

  mkdir -p "$SCRIPT_DIR"
  echo "#!/bin/bash" > "$SCRIPT_DIR/submit_apptainer_h7.sh"
  submit_jobs

  local LOG
  LOG=$(cat "$LOG_FILE")

  assert_not_empty  "$LOG_FILE"
  assert_matches "${VAHOME}/${MODELS_PATH}" "$LOG"
  assert_matches "-type f" "$LOG"
  assert_matches "-name submit_apptainer_h7.sh" "$LOG"
  assert_contains "-iregex ^.*\\(grevelingen\\|volkerakzoommeer\\).*$" "$LOG"
  assert_exit_code 0
}


function test_submit_jobs_with_job_id() {
  local JOB_IDS=("12345")

  # Build model args
  build_model_regex

  local SCRIPT_DIR="$VAHOME/$MODELS_PATH/model1/computations/test"

  mkdir -p "$SCRIPT_DIR"
  echo "#!/bin/bash" > "$SCRIPT_DIR/submit_apptainer_h7.sh"
  submit_jobs

  assert_equals "12345" "${JOB_IDS[0]}"
  assert_exit_code 0
}

function test_submit_jobs_multiple_scripts() {
  # Build model args
  build_model_regex

  for model in model1 model2 model3; do
    local SCRIPT_DIR="$VAHOME/$MODELS_PATH/$model/computations/test"
    mkdir -p "$SCRIPT_DIR"
    echo "#!/bin/bash" > "$SCRIPT_DIR/submit_apptainer_h7.sh"
  done

  submit_jobs

  assert_have_been_called_times 3 sbatch
  assert_exit_code 0
}


# ------ archive_and_upload_output tests ------ #
function test_archive_and_upload_output_checks_sbatch_is_called_once_with_defaults() {

  archive_and_upload_output

  assert_have_been_called sbatch
  assert_have_been_called_times 1 sbatch
  assert_exit_code 0
}

function test_archive_and_upload_output_checks_sbatch_called_without_job_id() {
  submit_jobs
  archive_and_upload_output

  assert_have_been_called sbatch
  assert_have_been_called_with "--parsable --output=${LOG_DIR}/va-upload-output-%j.out --dependency=afterany: ./jobs/upload_output.sh" sbatch
  assert_have_been_called_times 1 sbatch
  assert_exit_code 0
}

function test_archive_and_upload_output_checks_sbatch_called_with_job_id() {
  JOB_ID=("12345")

  submit_jobs
  archive_and_upload_output

  assert_have_been_called sbatch
  assert_have_been_called_with "--parsable --output=${LOG_DIR}/va-upload-output-%j.out --dependency=afterany:${JOB_ID_LIST} ./jobs/upload_output.sh" sbatch
  assert_have_been_called_times 1 sbatch
  assert_exit_code 0
}


# ------ run_verschillentool tests ------ #
function test_run_verschillentool_check_called_with_defaults() {
  local DOWNLOAD_REFS_JOB_ID
  local UPLOAD_OUTPUT_JOB_ID
  DOWNLOAD_REFS_JOB_ID=$(download_output_data)
  UPLOAD_OUTPUT_JOB_ID=$(archive_and_upload_output)

  run_verschillentool "$DOWNLOAD_REFS_JOB_ID" "$UPLOAD_OUTPUT_JOB_ID"

  assert_have_been_called sbatch
  assert_have_been_called_with "--parsable --output=$LOG_DIR/va-run-verschillentool-%j.out --dependency=afterany:${DOWNLOAD_REFS_JOB_ID}:${UPLOAD_OUTPUT_JOB_ID} ./jobs/run_verschillentool.sh" sbatch
  assert_have_been_called_times 3 sbatch
  assert_equals "$DOWNLOAD_REFS_JOB_ID" ""
  assert_equals "$UPLOAD_OUTPUT_JOB_ID" ""
  assert_exit_code 0
}

function test_run_verschillentool_check_called_with_custom_job_ids() {
  local DOWNLOAD_REFS_JOB_ID
  local UPLOAD_OUTPUT_JOB_ID
  DOWNLOAD_REFS_JOB_ID="11111"
  UPLOAD_OUTPUT_JOB_ID="22222"

  run_verschillentool "$DOWNLOAD_REFS_JOB_ID" "$UPLOAD_OUTPUT_JOB_ID"

  assert_have_been_called sbatch
  assert_have_been_called_with "--parsable --output=$LOG_DIR/va-run-verschillentool-%j.out --dependency=afterany:${DOWNLOAD_REFS_JOB_ID}:${UPLOAD_OUTPUT_JOB_ID} ./jobs/run_verschillentool.sh" sbatch
  assert_have_been_called_times 1 sbatch
  assert_equals "$DOWNLOAD_REFS_JOB_ID" "11111"
  assert_equals "$UPLOAD_OUTPUT_JOB_ID" "22222"
  assert_exit_code 0
} 


# ------ trigger_teamcity_build tests ------ #
function test_trigger_teamcity_build_called_with_defaults() {
  local DOWNLOAD_REFS_JOB_ID
  local UPLOAD_OUTPUT_JOB_ID
  local RUN_VERSCHILLENTOOL_JOB_ID
  DOWNLOAD_REFS_JOB_ID=$(download_output_data)
  UPLOAD_OUTPUT_JOB_ID=$(archive_and_upload_output)
  RUN_VERSCHILLENTOOL_JOB_ID=$(run_verschillentool "$DOWNLOAD_REFS_JOB_ID" "$UPLOAD_OUTPUT_JOB_ID")

  trigger_teamcity_build "$RUN_VERSCHILLENTOOL_JOB_ID"

  assert_have_been_called sbatch
  assert_have_been_called_with "--dependency=afterany:${RUN_VERSCHILLENTOOL_JOB_ID} ./jobs/trigger_teamcity_build.sh" sbatch
  assert_have_been_called_times 4 sbatch
  assert_equals "$DOWNLOAD_REFS_JOB_ID" ""
  assert_equals "$UPLOAD_OUTPUT_JOB_ID" ""
  assert_equals "$RUN_VERSCHILLENTOOL_JOB_ID" ""
  assert_exit_code 0
}

function test_trigger_teamcity_build_called_with_custom_job_ids() {
  local DOWNLOAD_REFS_JOB_ID
  local UPLOAD_OUTPUT_JOB_ID
  local RUN_VERSCHILLENTOOL_JOB_ID
  DOWNLOAD_REFS_JOB_ID="11111"
  UPLOAD_OUTPUT_JOB_ID="22222"
  RUN_VERSCHILLENTOOL_JOB_ID=$(run_verschillentool "$DOWNLOAD_REFS_JOB_ID" "$UPLOAD_OUTPUT_JOB_ID")

  trigger_teamcity_build "$RUN_VERSCHILLENTOOL_JOB_ID"

  assert_have_been_called sbatch
  assert_have_been_called_with "--dependency=afterany:${RUN_VERSCHILLENTOOL_JOB_ID} ./jobs/trigger_teamcity_build.sh" sbatch
  assert_have_been_called_times 2 sbatch
  assert_equals "$DOWNLOAD_REFS_JOB_ID" "11111"
  assert_equals "$UPLOAD_OUTPUT_JOB_ID" "22222"
  assert_equals "$RUN_VERSCHILLENTOOL_JOB_ID" ""
  assert_exit_code 0
}

function test_trigger_teamcity_build_called_with_custom_run_verschillentool_job_id() {
  local RUN_VERSCHILLENTOOL_JOB_ID
  RUN_VERSCHILLENTOOL_JOB_ID="33333"

  trigger_teamcity_build "$RUN_VERSCHILLENTOOL_JOB_ID"

  assert_have_been_called sbatch
  assert_have_been_called_with "--dependency=afterany:${RUN_VERSCHILLENTOOL_JOB_ID} ./jobs/trigger_teamcity_build.sh" sbatch
  assert_have_been_called_times 1 sbatch
  assert_equals "$RUN_VERSCHILLENTOOL_JOB_ID" "33333"
  assert_exit_code 0
}
