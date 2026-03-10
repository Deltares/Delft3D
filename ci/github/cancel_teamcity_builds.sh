#!/bin/bash

# set -eou pipefail

set -o errexit
set -o errtrace

# Globals to be set by parse_args
TEAMCITY_BASE_URL=""
TEAMCITY_TOKEN=""
TEAMCITY_PROJECT_ID=""
BRANCH=""
COMMIT_HASH=""

# unicode definitions
UNICODE_FINISHED="\U1F3C1"
UNICODE_SUCCESS='\U2705'
UNICODE_UNKNOWN="\U2753"

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
      printf "    at %s() in %s:%s\n" "${func}" "${src}" "${lineno}"
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
  --branch BRANCH                Branch name
  --commit-hash HASH             Commit hash
  --help                         Show this help message
EOF
}

function parse_args() {
  local long_options="help,teamcity-base-url:,teamcity-token:,teamcity-project-id:,branch:,commit-hash:"
  local parsed_options
  if ! parsed_options=$(getopt --name "$(basename "$0")" --options "" --long "${long_options}" -- "$@"); then
    printf "parse_args: failed to parse arguments.\n" >&2
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
    --)
      shift
      break
      ;;
    *)
      printf "Parsing error!\n" >&2
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
    printf "One or more required arguments were not provided.\n" >&2
    usage
    exit 1
  fi
}

function print_header() {
  printf "\n%s was invoked with\n" "$0" >&2
  printf "TeamCity base URL   : %s\n" "${TEAMCITY_BASE_URL}" >&2
  printf "TeamCity Project ID : %s\n" "${TEAMCITY_PROJECT_ID}" >&2
  printf "Branch name         : %s\n" "${BRANCH}" >&2
  printf "Commit hash         : %s\n\n" "${COMMIT_HASH}" >&2
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

readonly BUILD_CANCEL_PAYLOAD='
  {
    "buildCancelRequest": {
      "comment": "Build cancelled from GitHub",
      "readdIntoQueue": false
    }
  }
'

function cancel_build() {
  local build_id="$1"
  teamcity_post_request "${TEAMCITY_BUILDS}/id:${build_id}" "${BUILD_CANCEL_PAYLOAD}"
}

function query_trigger() {

  printf "Querying Trigger...\n\n" >&2

  local build_type="${TEAMCITY_PROJECT_ID}_Trigger"

  local request_url
  printf -v request_url \
    "%s?locator=project:%s,buildType:%s,branch:%s,revision:%s,state:any,count:1" \
    "${TEAMCITY_BUILDS}" \
    "${TEAMCITY_PROJECT_ID}" \
    "${build_type}" \
    "${BRANCH}" \
    "${COMMIT_HASH}"

  local trigger
  trigger="$(teamcity_get_request "${request_url}")"

  local build_id build_state build_web_url
  read -r build_id build_state build_web_url < <(
    jq -r '.build[0] | "\(.id) \(.state) \(.webUrl)"' <<<"${trigger}" | tr -d '\r'
  )

  printf ">> \"%s\"\n     id: %s\n     state: %s\n     link: %s\n" \
    "${build_type}" \
    "${build_id}" \
    "${build_state}" \
    "${build_web_url}" >&2

  local kill_em_all
  case "${build_state}" in
  pending | queued)
    cancel_build "${build_id}"
    printf "     %b Cancelled while in %s state. Nothing left to do. \n" "${UNICODE_SUCCESS}" "${build_state}" >&2
    kill_em_all=0
    ;;
  running)
    cancel_build "${build_id}"
    printf "     %b Cancelled while in %s state. Additional builds may need to be cancelled.\n" "${UNICODE_SUCCESS}" "${build_state}" >&2
    kill_em_all=1
    ;;
  finished)
    printf "     %b Build finished. Additional builds may need to be cancelled.\n" ${UNICODE_FINISHED} >&2
    kill_em_all=1
    ;;
  *)
    printf "     %b Unknown state '%s'. Cannot proceed.\n" "${UNICODE_UNKNOWN}" "${build_state}" >&2
    kill_em_all=0
    ;;
  esac

  printf "%s\t%s\n" "${kill_em_all}" "${build_id}"
}

function cancel_all_builds() {

  local trigger_id="$1"

  printf "\nLooking up additional builds configurations..." >&2

  local locator="affectedProject:${TEAMCITY_PROJECT_ID},branch:${BRANCH},revision:${COMMIT_HASH},sinceBuild:${trigger_id},state:any,count:1000,lookupLimit:5000,defaultFilter:false&fields=build(id)"

  local raw_build_ids
  raw_build_ids=$(get_build_ids "${locator}")
  printf " done. " >&2

  if [[ -z "${raw_build_ids}" ]]; then
    printf "No configurations found. Nothing to cancel.\n" >&2
    exit 0
  fi

  local build_ids=()
  mapfile -t build_ids < <(printf '%s' "${raw_build_ids}" | tr -d '\r')
  printf "Found %d configuration(s): \n\n" ${#build_ids[@]}
  for build_id in "${build_ids[@]}"; do
    local build_type_id build_state build_web_url
    read -r build_type_id build_state build_web_url <<<"$(get_build_info "${build_id}")"
    printf ">> \"%s\"\n     id: %s\n     state: %s\n     link: %s\n" \
      "${build_type_id}" \
      "${build_id}" \
      "${build_state}" \
      "${build_web_url}" >&2

    case "${build_state}" in
    pending | queued | running)
      cancel_build "${build_id}"
      printf "     %b Cancelled.\n" "${UNICODE_SUCCESS}" >&2
      ;;
    finished)
      printf "     %b Build finished, nothing to cancel.\n" ${UNICODE_FINISHED} >&2
      ;;
    *)
      printf "     %b Unknown state '%s', skipping.\n" "${UNICODE_UNKNOWN}" "${build_state}" >&2
      ;;
    esac
  done
}

function main() {
  parse_args "$@"
  print_header
  encode_branch_name
  IFS=$'\t' read -r kill_em_all trigger_id < <(query_trigger)
  if [[ "${kill_em_all}" == 1 ]]; then
    cancel_all_builds "${trigger_id}"
  fi
}

main "$@"
