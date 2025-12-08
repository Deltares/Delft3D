#!/usr/bin/env bash
# shellcheck disable=SC2329,SC2034,SC1091

function set_up_before_script() {
  source "$ROOT_DIR/../bundle/jobs/run_verschillentool.sh"
  source "$ROOT_DIR/../bundle/util.sh"

  declare -g TMP_DIR
  TMP_DIR=$(mktemp -d)
  declare -g HOME="$TMP_DIR/home"
  declare -g BUCKET="test-bucket"
  declare -g VAHOME="$TMP_DIR/vahome"
  declare -g CURRENT_PREFIX="current_run"
  declare -g REFERENCE_PREFIX="reference_run"
  declare -g MODEL_REGEX="^.*$"
  declare -g VERSCHILLENTOOL_DIR="${TMP_DIR}/verschillentool"
  declare -g LOG_DIR="${VAHOME}/logs"
  declare -g CREDS_FILE="${HOME}/.harbor/verschillentool"

  spy docker
  spy find
  spy zip
}

function tear_down_after_script() {
  rm -rf "$TMP_DIR"
}


# ------ validate_inputs tests ------ #
function test_validate_inputs_all_defined() {
  validate_inputs

  assert_equals "test-bucket" "$BUCKET"
  assert_equals "$TMP_DIR/vahome" "$VAHOME"
  assert_equals "current_run" "$CURRENT_PREFIX"
  assert_equals "reference_run" "$REFERENCE_PREFIX"
  assert_equals "^.*$" "$MODEL_REGEX"
  assert_exit_code 0
}

function test_validate_inputs_fails_when_missing_vars() {
  unset BUCKET
  validate_inputs
  assert_exit_code 1
}


# ------ create_verschillentool_dir tests ------ #
function test_create_verschillentool_dir_creates_directory() {
  create_verschillentool_dir
  assert_directory_exists "$VERSCHILLENTOOL_DIR"
  assert_exit_code 0
}

function test_create_verschillentool_dir_when_missing_directory() {
  rm -rf "$VERSCHILLENTOOL_DIR"
  assert_directory_not_exists "$VERSCHILLENTOOL_DIR"

  create_verschillentool_dir

  assert_directory_exists "$VERSCHILLENTOOL_DIR"
  assert_exit_code 0
}


# ------ docker_login tests ------ #
function test_docker_login_with_missing_credentials_file() {
  assert_directory_not_exists "$(dirname "$CREDS_FILE")"
  docker_login
  assert_exit_code 1
}

function test_docker_login_with_valid_credentials_file() {
  mkdir -p "$(dirname "$CREDS_FILE")"
  echo "password" > "$CREDS_FILE"

  docker_login
  assert_have_been_called docker
  assert_have_been_called_times 1 docker
  assert_have_been_called_with "login --username=robot\$verschillentool+h7 --password-stdin containers.deltares.nl" docker
  assert_exit_code 0
}

# ------ run_verschillentool tests ------ #
function test_run_verschillentool_spy_find_default_model_regex() {
  run_verschillentool

  assert_have_been_called find
  assert_have_been_called_times 1 find
  assert_have_been_called_with "config -name *.json -iregex ${MODEL_REGEX} -exec docker run --rm --volume=${VAHOME}/input:/data/input:ro --volume=${VAHOME}/reference:/data/reference:ro --volume=${PWD}/{}:/data/{}:ro --volume=${VERSCHILLENTOOL_DIR}:/data/verschillentool containers.deltares.nl/verschillentool/verschillentool:release_v1.0.2 --config /data/{} ;" find
  assert_exit_code 0
}

function test_run_verschillentool_spy_find_specific_model_regex() {
  local MODEL_FILTER="grevelingen,volkerakzoommeer"
  local MODEL_REGEX="^.*\\(${MODEL_FILTER//,/\\|}\\).*\$"
  run_verschillentool

  assert_have_been_called find
  assert_have_been_called_times 1 find
  assert_have_been_called_with "config -name *.json -iregex ${MODEL_REGEX} -exec docker run --rm --volume=${VAHOME}/input:/data/input:ro --volume=${VAHOME}/reference:/data/reference:ro --volume=${PWD}/{}:/data/{}:ro --volume=${VERSCHILLENTOOL_DIR}:/data/verschillentool containers.deltares.nl/verschillentool/verschillentool:release_v1.0.2 --config /data/{} ;" find
  assert_exit_code 0
}


# ------ create_verschillen_archive tests ------ #
function test_create_verschillen_archive_creates_zip_and_cleans_up() {
  mkdir -p "$VERSCHILLENTOOL_DIR"
  for file in file.txt file2.txt; do
    touch "$VERSCHILLENTOOL_DIR/$file"
  done
  
  # Mock zip to simulate success
  function zip() {
	touch "$VERSCHILLENTOOL_DIR/verschillen.zip"
	return 0
  }

  create_verschillen_archive "$VERSCHILLENTOOL_DIR"

  assert_file_exists "$VERSCHILLENTOOL_DIR/verschillen.zip"
  assert_file_not_exists "$VERSCHILLENTOOL_DIR/file.txt"
  assert_file_not_exists "$VERSCHILLENTOOL_DIR/file2.txt"
  assert_exit_code 0
}

function test_create_verschillen_archive_handles_zip_failure() {
  mkdir -p "$VERSCHILLENTOOL_DIR"
  touch "$VERSCHILLENTOOL_DIR/file.txt"

  # Mock zip to simulate error
  function zip() {
    return 1
  }

  create_verschillen_archive "$VERSCHILLENTOOL_DIR"

  assert_exit_code 1
}

function test_create_verschillen_archive_calls_zip_correctly() {
  mkdir -p "$VERSCHILLENTOOL_DIR"
  touch "$VERSCHILLENTOOL_DIR/file.txt"

  create_verschillen_archive "$VERSCHILLENTOOL_DIR"

  assert_have_been_called zip
  assert_have_been_called_times 1 zip
  assert_have_been_called_with "-r verschillen.zip ." zip
  assert_exit_code 0
}


# ------ upload_verschillen tests ------ #
function test_upload_verschillen_docker_spy() {
  local REFERENCE_TAG=""${REFERENCE_PREFIX##*/}
  mkdir -p "$VERSCHILLENTOOL_DIR"
  touch "$VERSCHILLENTOOL_DIR/verschillen.zip"

  upload_verschillen

  assert_have_been_called docker
  assert_have_been_called_times 1 docker
  assert_have_been_called_with "run --rm --volume=${HOME}/.aws:/root/.aws:ro --volume=${VERSCHILLENTOOL_DIR}:/data:ro docker.io/amazon/aws-cli:2.22.7 --profile=verschilanalyse --endpoint-url=https://s3.deltares.nl s3 sync --delete --no-progress /data ${BUCKET}/${CURRENT_PREFIX}/verschillentool/${REFERENCE_TAG}" docker
  assert_exit_code 0
}
