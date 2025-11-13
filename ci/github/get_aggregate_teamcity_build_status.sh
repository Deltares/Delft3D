#!/usr/bin/env bash

set -o errexit
set -o errtrace

# Globals to be set by parse_args
GITHUB_TOKEN=""
TEAMCITY_URL="https://dpcbuild.deltares.nl"
TEAMCITY_TOKEN=""
TEAMCITY_PROJECT_ID=""
TEAMCITY_BUILD_CONFIG_ID=""
BRANCH_NAME=""
COMMIT_SHA=""
POLL_INTERVAL=30

ENCODED_BRANCH_NAME=""
TEAMCITY_BUILDS="${TEAMCITY_URL}/app/rest/builds"

# unicode definitions
# states
UNICODE_QUEUED="\U23F3"
UNICODE_PENDING="\U1F551"
UNICODE_RUNNING="\U1F680"
UNICODE_FINISHED="\U1F3C1"
# statuses
UNICODE_SUCCESS='\U2705'
UNICODE_FAILURE='\U274C'
UNICODE_UNKNOWN="\U2753"
# misc
UNICODE_WAIT='\U23F3'

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
  --github-token TOKEN           GitHub access token
  --teamcity-token TOKEN         TeamCity access token
  --teamcity-project-id ID       TeamCity project ID
  --teamcity-build-config-id ID  ID of the TeamCity build config invoking the script  
  --branch-name NAME             Branch name to monitor (will be URL-encoded automatically)
  --commit-sha SHA               Commit SHA
  --poll-interval SECONDS        Polling interval in seconds (default: 30)
  --help                         Show this help message
EOF
}

function parse_args() {
  local long_options="help,github-token:,teamcity-token:,teamcity-project-id:,teamcity-build-config-id:,branch-name:,commit-sha:,poll-interval:"
  local parsed_options
  if ! parsed_options=$(getopt --name "$(basename "$0")" --options "" --long ${long_options} -- "$@"); then
    printf "parse_args: failed to parse arguments."
    return 1
  fi
  eval set -- "${parsed_options}"

  while true; do
    case "$1" in
    --help)
      usage
      exit 0
      ;;
    --github-token)
      GITHUB_TOKEN="$2"
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
    --teamcity-build-config-id)
      TEAMCITY_BUILD_CONFIG_ID="$2"
      shift 2
      ;;
    --branch-name)
      BRANCH_NAME="$2"
      shift 2
      ;;
    --commit-sha)
      COMMIT_SHA="$2"
      shift 2
      ;;
    --poll-interval)
      POLL_INTERVAL="$2"
      shift 2
      ;;
    --)
      shift
      break
      ;;
    *)
      printf "Internal error!\n" >&2
      exit 1
      ;;
    esac
  done

  # Validate required params
  if [[ -z "${GITHUB_TOKEN}" || 
    -z "${TEAMCITY_TOKEN}" ||
    -z "${TEAMCITY_PROJECT_ID}" ||
    -z "${TEAMCITY_BUILD_CONFIG_ID}" ||
    -z "${BRANCH_NAME}" ||
    -z "${COMMIT_SHA}" ]]; then
    printf "%s\n" "One or more required arguments were not provided." >&2
    usage
    exit 1
  fi
}

function print_header() {
  printf "\n%s was invoked with\n" "$0"
  printf "TeamCity URL  : %s\n" "${TEAMCITY_URL}"
  printf "Project ID    : %s\n" "${TEAMCITY_PROJECT_ID}"
  printf "Branch name   : %s\n" "${BRANCH_NAME}"
  printf "Commit SHA    : %s\n" "${COMMIT_SHA}"
  printf "Poll interval : %s\n\n" "${POLL_INTERVAL}"
}

function encode_branch_name() {
  local branch_name="$1"
  local encoded_branch_name
  encoded_branch_name="$(jq -rn --arg v "${branch_name}" '$v|@uri')"
  printf "%s" "${encoded_branch_name}"
}

function teamcity_get_request() {
  local request_url="$1"
  curl \
    --silent \
    --fail \
    --show-error \
    --request "GET" \
    "${request_url}" \
    --header "Authorization: Bearer ${TEAMCITY_TOKEN}" \
    --header "Accept: application/json"
}

function count_sheep() {
  sleep "${POLL_INTERVAL}"
}

function trigger() {
  declare -n id="$1"

  local build_type="${TEAMCITY_PROJECT_ID}_Trigger"
  local waiting=false

  local request_url="${TEAMCITY_BUILDS}?locator="
  request_url+="project:${TEAMCITY_PROJECT_ID},"
  request_url+="buildType:${build_type},"
  request_url+="branch:${ENCODED_BRANCH_NAME},"
  request_url+="revision:${COMMIT_SHA},"
  request_url+="state:any,"
  request_url+="count:1"

  while true; do
    local trigger
    trigger="$(teamcity_get_request "${request_url}")"

    local state
    state=$(printf "%s" "${trigger}" | jq -r '.build[0].state')
    local status
    status=$(printf "%s" "${trigger}" | jq -r '.build[0].status')

    if [ "${state}" != "finished" ]; then
      if [ "${waiting}" = false ]; then
        printf "%b Trigger is not finished yet. Polling for updates every %d seconds...\n" "${UNICODE_WAIT}" "${POLL_INTERVAL}"
        waiting=true
      fi
      count_sheep
      continue
    elif [ "${status}" != "SUCCESS" ]; then
      printf "%b Trigger failed. Tracking of the remaining jobs is no longer possible.\n" "${UNICODE_FAILURE}"
      return 1
    fi

    # shellcheck disable=SC2034
    id=$(printf "%s" "${trigger}" | jq -r '.build[0].id')

    printf "%b Trigger (id: %d) finished successfully!\n" "${UNICODE_SUCCESS}" "${id}"

    return 0
  done
}

function get_state_unicode() {
  local state="$1"
  local unicode=""
  if [ "${state}" = "pending" ]; then
    unicode="${UNICODE_PENDING}"
  elif [ "${state}" = "queued" ]; then
    unicode="${UNICODE_QUEUED}"
  elif [ "${state}" = "running" ]; then
    unicode="${UNICODE_RUNNING}"
  elif [ "${state}" = "finished" ]; then
    unicode="${UNICODE_FINISHED}"
  fi
  printf "%b" "${unicode}"
}

function get_status_unicode() {
  local status="$1"
  local unicode=""
  if [ "${status}" = "SUCCESS" ]; then
    unicode="${UNICODE_SUCCESS}"
  elif [ "${status}" = "FAILURE" ]; then
    unicode="${UNICODE_FAILURE}"
  elif [ "${status}" = "UNKNOWN" ]; then
    unicode="${UNICODE_UNKNOWN}"
  fi
  printf "%b" "${unicode}"
}

function publish_state() {
  local state="$1"

  local payload
  payload=$(
    jq -n \
      --arg state "${state}" \
      '{
        "state": $state,
        "description": "Dynamic status that is continuously updated until all jobs finish",
        "context": "TeamCity aggregate build status"
      }'
  )

  curl \
    --silent \
    --fail \
    --show-error \
    --location \
    --request POST \
    --header "Accept: application/vnd.github+json" \
    --header "X-GitHub-Api-Version: 2022-11-28" \
    --header "Authorization: token ${GITHUB_TOKEN}" \
    "https://api.github.com/repos/Deltares/Delft3D/statuses/${COMMIT_SHA}" \
    -d "${payload}"
}

function get_aggregate_teamcity_build_status() {
  local trigger_id="$1"

  local request_url="${TEAMCITY_BUILDS}?locator=snapshotDependency:(from:(id:${trigger_id}),includeInitial:true),defaultFilter:false"

  # local request_url="${TEAMCITY_BUILDS}?locator="
  # request_url+="affectedProject:${TEAMCITY_PROJECT_ID},"
  # request_url+="branch:${ENCODED_BRANCH_NAME},"
  # request_url+="revision:${COMMIT_SHA},"
  # request_url+="state:any,"
  # request_url+="canceled:any,"
  # request_url+="sinceBuild:${trigger_id},"
  # request_url+="count:1000"

  local jobs
  jobs="$(teamcity_get_request "${request_url}")"

  #printf "%b" "\nSNAPSHOT DEPS\n=============\n"
  #printf "%s" "${jobs}" | jq -r

  local tracked_build_ids=""
  tracked_build_ids="$(jq -r '.build[]?.id' <<<"${jobs}" | tr -d '\r' | sort -nu | xargs)"

  if [ -z "${tracked_build_ids}" ]; then
    printf "%b No builds detected.\n" ${UNICODE_SUCCESS}
    return 0
  fi

  # Check status of all tracked builds

  local ids=()
  IFS=' ' read -r -a ids <<<"${tracked_build_ids}"
  local states=()
  local statuses=()
  local web_urls=()
  local build_type_ids=()

  for id in "${ids[@]}"; do
    local request_url="${TEAMCITY_BUILDS}/id:${id}"
    local build_info
    build_info="$(teamcity_get_request "${request_url}")"
    build_type_ids+=("$(printf "%s" "${build_info}" | jq -r '.buildTypeId')")
    states+=("$(printf "%s" "${build_info}" | jq -r '.state')")
    statuses+=("$(printf "%s" "${build_info}" | jq -r '.status')")
    web_urls+=("$(printf "%s" "${build_info}" | jq -r '.webUrl')")
  done

  for i in "${!ids[@]}"; do
    messages+="- Build ${build_type_ids[$i]}\n"
    messages+="  id: ${ids[$i]}\n"
    messages+="  State: ${states[$i]} $(get_state_unicode "${states[$i]}")\n"
    messages+="  Status: ${statuses[$i]} $(get_status_unicode "${statuses[$i]}")\n"
    messages+="  URL: ${web_urls[$i]}\n"
  done
  printf "%b" "${messages}"

  for state in "${states[@]}"; do
    if [[ "${state}" != "finished" ]]; then
      # Some jobs have not finished yet (sufficient condition for blocking the merging of the PR).
      # The status can be anything. The status become relevant when all finish.
      printf "\n%b Not all tracked builds have finished." "${UNICODE_FAILURE}"
      publish_state "pending"
      return 0
    fi
  done

  # All jobs have finished. Were they successful?
  # Note: An unknown status is treated as a failure
  for status in "${statuses[@]}"; do
    if [[ "${status}" != "SUCCESS" ]]; then
      printf "\n%b All tracked builds have finished but one or more were not successful.\n" "${UNICODE_FAILURE}"
      publish_state "failure"
      return 0 # finished with errors, do not fail the step though
    fi
  done
  printf "\n%b All tracked builds have finished successfully!\n" "${UNICODE_SUCCESS}"
  publish_state "success"
  return 0 # finished successfully
}

function main() {
  parse_args "$@"
  print_header
  ENCODED_BRANCH_NAME=$(encode_branch_name "${BRANCH_NAME}")
  local trigger_id
  trigger trigger_id
  get_aggregate_teamcity_build_status "${trigger_id}"


  request_url="${TEAMCITY_BUILDS}?locator="
  request_url+="buildType:${TEAMCITY_BUILD_CONFIG_ID},"
  request_url+="branch:${ENCODED_BRANCH_NAME},"
  request_url+="revision:${COMMIT_SHA},"
  request_url+="count:1"
  build_info="$(teamcity_get_request "${request_url}")"
  build_id="$(printf "%s" "${build_info}" | jq -r '.build[0].id')"


  # From the doc:
  # The teamcity.build.step.status.<step_ID> parameters appear only after their corresponding steps finish,
  # and are not available right from the moment a build starts. This means neither steps that are still running, 
  # nor skipped steps have their teamcity.build.step.status.<step_ID> parameters available.
  # impact: step calling this script doe not report itself, which is good.
  request_url="${TEAMCITY_BUILDS}/id:${build_id}/resulting-properties"
  teamcity_get_request "${request_url}" | #jq -r .
    jq -r '
    .property[] | select(.name | test("^teamcity.build.step.status\\.")) |
    "\(.name) = \(.value)"
  '

}

main "$@"
