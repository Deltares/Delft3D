#!/usr/bin/env bash
# shellcheck disable=SC2329,SC2034,SC1091

function set_up_before_script() {
  source "$ROOT_DIR/../bundle/jobs/run_verschillentool.sh"

#   # ------ Base vars ------ #
#   declare -g TMP_DIR
#   TMP_DIR=$(mktemp -d)
#   declare -g VERSCHILLENTOOL_DIR
#   VERSCHILLENTOOL_DIR="${TMP_DIR}/verschillentool"

}

# function tear_down_after_script() {
#   rm -rf "$TMP_DIR"
# }

# function test_validate_inputs_all_defined() {
#   local BUCKET="some-bucket"
#   local VAHOME="/some/path/vahome"
#   local CURRENT_PREFIX="current_run"
#   local REFERENCE_PREFIX="reference_run"
#   local MODEL_REGEX="^.*$"

#   validate_inputs

#   assert_equals "some-bucket" "$BUCKET"
#   assert_equals "/some/path/vahome" "$VAHOME"
#   assert_equals "current_run" "$CURRENT_PREFIX"
#   assert_equals "reference_run" "$REFERENCE_PREFIX"
#   assert_equals "^.*$" "$MODEL_REGEX"
#   assert_exit_code 0
# }