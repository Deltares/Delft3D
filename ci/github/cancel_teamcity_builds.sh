#!/bin/bash

# set -eou pipefail

set -o errexit
set -o errtrace

# Globals to be set by parse_args
TEAMCITY_TOKEN=""
TEAMCITY_PROJECT_ID=""
BRANCH=""
COMMIT_HASH=""

TEAMCITY_URL="https://dpcbuild.deltares.nl"
TEAMCITY_BUILDS="${TEAMCITY_URL}/app/rest/builds"

function catch() {
  local exit_code=$1
  if [ "${exit_code}" != "0" ]; then
    printf "\n** An error occurred **\n"
    printf "  Exit code: %s\n" "${exit_code}"
    printf "  Command: %s\n" "${BASH_COMMAND}"
    printf "  Traceback (most recent call first):\n"
    # Loop through the stack
    local i
    for ((i = 1; i < ${#FUNCNAME[@]}; i++)); do

      local lineno="${BASH_LINENO[$((i - 1))]}"
      local func="${FUNCNAME[$i]}"
      local src="${BASH_SOURCE[$i]}"
      printf "    at %s() in %s:%s\n" "$func" "$src" "$lineno"
    done
  fi
}

trap 'catch $?' ERR

function usage() {
  cat <<EOF
Usage: $0 [OPTIONS]

Options:
  --teamcity-token TOKEN         TeamCity access token
  --teamcity-project-id ID       TeamCity project ID
  --branch BRANCH                Branch name to monitor (will be URL-encoded automatically)
  --commit-hash HASH             Commit HASH (Optional: if not specified all builds on the branch wil be cancelled)
  --help                         Show this help message
EOF
}

function parse_args() {
  local long_options="help,teamcity-token:,teamcity-project-id:,branch:,commit-hash:"
  local parsed_options
  if ! parsed_options=$(getopt --name "$(basename "$0")" --options "" --long ${long_options} -- "$@"); then
    printf "parse_args: failed to parse arguments.\n"
    return 1
  fi
  eval set -- "${parsed_options}"

  while true; do
    case "$1" in
    --help)
      usage
      exit 0
      ;;
    --teamcity-token)
      TEAMCITY_TOKEN="$2"
      shift 2
      ;;
    --teamcity-project-id)
      TEAMCITY_PROJECT_ID="$2"
      shift 2
      ;;
    --branch)
      BRANCH="$2"
      shift 2
      ;;
    --commit-hash)
      COMMIT_HASH="$2"
      shift 2
      ;;
    --)
      shift
      break
      ;;
    *)
      printf "Parsing error!\n"
      usage
      exit 1
      ;;
    esac
  done

  # Validate required params
  if [[ -z "${TEAMCITY_TOKEN}" ||
    -z "${TEAMCITY_PROJECT_ID}" ||
    -z "${BRANCH}" ]]; then
    printf "One or more required arguments were not provided.\n"
    usage
    exit 1
  fi
}

function print_header() {
  printf "\n%s was invoked with\n" "$0" 
  printf "Project ID    : %s\n" "${TEAMCITY_PROJECT_ID}"
  printf "Branch name   : %s\n" "${BRANCH}"
  printf "Commit SHA    : %s\n" "${COMMIT_HASH}"
}

function encode_branch_name() {
  BRANCH="$(jq -rn --arg v "${BRANCH}" '$v|@uri')"
}

function teamcity_get_request() {
  local url="$1"
  curl \
    --silent \
    --fail \
    --show-error \
    --request GET \
    --header "Authorization: Bearer ${TEAMCITY_TOKEN}" \
    --header "Accept: application/json" \
    --header "Content-Type: application/json" \
    "${url}"
}

function teamcity_post_request() {
  local url="$1"
  local payload="$2"
  curl \
    --silent \
    --fail \
    --show-error \
    --request POST \
    --header "Authorization: Bearer ${TEAMCITY_TOKEN}" \
    --header "Accept: application/json" \
    --header "Content-Type: application/json" \
    --data "${payload}" \
    "${url}"
}

function get_build_ids() {
  local locator="$1"
  teamcity_get_request "${TEAMCITY_BUILDS}?locator=${locator}" | jq -r '.build[]?.id'
}

function get_build_state() {
  local build_id="$1"
  teamcity_get_request "${TEAMCITY_BUILDS}/id:${build_id}" | jq -r '.state'
}

function cancel_build() {
  local build_id="$1"
  local payload='{ "buildCancelRequest": { "comment": "Build cancelled from GitHub", "readdIntoQueue": false } }'
  printf "Stopping build %d... " "${build_id}"
  teamcity_post_request "${TEAMCITY_BUILDS}/id:${build_id}" "${payload}"
  printf "done.\n"
}

function cancel_all_builds() {
  printf "Looking up root builds for project %s on branch %s... " "${PROJECT_ID}" "${BRANCH}"
  local locator="project:${PROJECT_ID},branch:${BRANCH},state:any"
  if [[ -n "${COMMIT_HASH}" ]]; then
    locator="${locator},revision:${COMMIT_HASH}"
  fi
  root_build_ids=$(get_build_ids "${locator}")
  printf "done.\n"

  if [[ -z "${root_build_ids}" ]]; then
    printf "No builds found for this commit. Nothing to cancel."
    exit 0
  fi

  printf "Root builds:\n%s\n" "${root_build_ids}"

  for root_build_id in $root_build_ids; do
    state=$(get_build_state "${root_build_id}")
    printf "Build %d is in state: %s.\n" "${root_build_id}" "${state}"

    case "${state}" in
    queued | pending)
      cancel_build "${root_build_id}"
      ;;

    running | finished)
      dep_build_ids=$(get_build_ids "snapshotDependency:(from:(id:${root_build_id}),state:any,defaultFilter:false")

      if [[ -z "${dep_build_ids}" ]]; then
        printf "No dependent builds for %d.\n" "${root_build_id}"
        continue
      fi

      printf "Dependent builds for root build with id %d: \n%s\n" "${root_build_id}" "${dep_build_ids}"

      for dep_build_id in ${dep_build_ids}; do
        dep_state=$(get_build_state "${dep_build_id}")

        if [[ "${dep_state}" == "running" ||
          "${dep_state}" == "queued" ||
          "${dep_state}" == "pending" ]]; then
          cancel_build "${dep_build_id}"
        fi
      done
      ;;

    *)
      printf "Unknown state '%s' for build %d, skipping.\n" "${state}" "${root_build_id}"
      ;;
    esac
  done
}

function main() {
  parse_args "$@"
  print_header
  encode_branch_name
  cancel_all_builds
}

main "$@"
