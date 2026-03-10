#!/bin/bash

# set -eou pipefail

set -o errexit
set -o errtrace

# Globals to be set by parse_args
TEAMCITY_TOKEN=""
TEAMCITY_PROJECT_ID=""
BRANCH=""
COMMIT_HASH=""
VERBOSE=false

TEAMCITY_BASE_URL="https://dpcbuild.deltares.nl"
TEAMCITY_BUILDS="${TEAMCITY_BASE_URL}/app/rest/builds"

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
  --teamcity-base-url URL        TeamCity base URL
  --teamcity-token TOKEN         TeamCity access token
  --teamcity-project-id ID       TeamCity project ID
  --branch BRANCH                Branch name to monitor (will be URL-encoded automatically)
  --commit-hash HASH             Commit HASH (Optional: if not specified all builds on the branch wil be cancelled)
  --help                         Show this help message
EOF
}

function parse_args() {
  local long_options="help,teamcity-base-url:,teamcity-token:,teamcity-project-id:,branch:,commit-hash:,verbose"
  local parsed_options
  if ! parsed_options=$(getopt --name "$(basename "$0")" --options "" --long "${long_options}" -- "$@"); then
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
    --teamcity-base-url)
      TEAMCITY_BASE_URL="$2"
      TEAMCITY_BUILDS="${TEAMCITY_BASE_URL}/app/rest/builds"
      shift 2
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
    --verbose)
      VERBOSE=true
      shift 1
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
  if [[ -z "${TEAMCITY_BASE_URL}" ||
    -z "${TEAMCITY_TOKEN}" ||
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
  local commit
  if [[ -z "${COMMIT_HASH}" ]]; then
    commit="All commits"
  else
    commit="${COMMIT_HASH}"
  fi
  printf "Commit SHA    : %s\n" "${commit}"
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
    --output /dev/null \
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

function get_build_info() {
  local build_id="$1"
  teamcity_get_request "${TEAMCITY_BUILDS}/id:${build_id}" |
    jq -r '[.buildTypeId, .state, .webUrl] | @tsv'
}

function cancel_build() {
  local build_id="$1"
  local payload='{ "buildCancelRequest": { "comment": "Build cancelled from GitHub", "readdIntoQueue": false } }'
  teamcity_post_request "${TEAMCITY_BUILDS}/id:${build_id}" "${payload}"
}

function cancel_all_builds() {
  printf "Looking up root builds for project %s on branch %s... " "${TEAMCITY_PROJECT_ID}" "${BRANCH}"
  local locator="project:${TEAMCITY_PROJECT_ID},branch:${BRANCH},state:any"
  if [[ -n "${COMMIT_HASH}" ]]; then
    locator="${locator},revision:${COMMIT_HASH}"
  fi
  local raw_root_build_ids
  raw_root_build_ids=$(get_build_ids "${locator}")
  printf "done.\n"

  if [[ -z "${raw_root_build_ids}" ]]; then
    printf "No builds found. Nothing to cancel."
    exit 0
  fi

  local root_build_ids=()
  mapfile -t root_build_ids < <(printf '%s' "${raw_root_build_ids}" | tr -d '\r')
  for root_build_id in "${root_build_ids[@]}"; do
    read -r root_build_type_id root_build_state root_build_web_url <<<"$(get_build_info "${root_build_id}")"
    printf "\nFound root build %s (id: %s | state: %s | %s)\n" \
      "${root_build_type_id}" \
      "${root_build_id}" \
      "${root_build_state}" \
      "${root_build_web_url}" >&2

    case "${root_build_state}" in
    pending | queued)
      cancel_build "${root_build_id}"
      ;;
    running | finished)
      local raw_dep_build_ids
      locator="snapshotDependency:(from:(id:${root_build_id}),includeInitial:true),state:any,defaultFilter:false"

      raw_dep_build_ids=$(get_build_ids "${locator}")

      if [[ -z "${raw_dep_build_ids}" ]]; then
        printf "No dependent builds for root build with id %s.\n" "${root_build_id}"
        continue
      fi

      local dep_build_ids=()
      mapfile -t dep_build_ids < <(printf '%s' "${raw_dep_build_ids}" | tr -d '\r')
      printf "Dependent builds for root build with id %s:\n" "${root_build_id}"
      for dep_build_id in "${dep_build_ids[@]}"; do
        read -r dep_build_type_id dep_build_state dep_build_web_url <<<"$(get_build_info "${dep_build_id}")"
        printf "  Found \"%s\" [id: %s | state: %s | %s]\n" \
          "${dep_build_type_id}" \
          "${dep_build_id}" \
          "${dep_build_state}" \
          "${dep_build_web_url}" >&2

        case "${dep_build_state}" in
        pending | queued | running)
          printf "    Stopping build..."
          cancel_build "${dep_build_id}"
          printf " done.\n"
          ;;
        finished)
          printf "    Build finished, nothing to cancel.\n"
          ;;
        *)
          printf "    Unknown state '%s', skipping.\n" "${root_build_state}"
          ;;
        esac
      done
      ;;
    *)
      printf "Unknown state '%s' for build %s, skipping.\n" "${root_build_state}" "${root_build_id}"
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
