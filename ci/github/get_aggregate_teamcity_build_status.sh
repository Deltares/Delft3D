#!/usr/bin/env bash
set -euo pipefail

# Globals to be set by parse_args
TEAMCITY_URL="https://dpcbuild.deltares.nl"
TEAMCITY_TOKEN=""
PROJECT_ID=""
BRANCH_NAME=""
COMMIT_SHA=""
POLL_INTERVAL=30
INTERACTIVE=false

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

function usage() {
  cat <<EOF
Usage: $0 [OPTIONS]

Options:
  --teamcity-token TOKEN    TeamCity access token or password
  --branch-name NAME        Branch name to monitor (will be URL-encoded automatically)
  --commit-sha SHA          Commit SHA
  --poll-interval SECONDS   Polling interval in seconds (default: 30)
  --interactive             Interactive build info switch
  --help                    Show this help message
EOF
}

parse_args() {
  local long_options="help,teamcity-token:,project-id:,branch-name:,commit-sha:,poll-interval:,interactive"
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
    --teamcity-token)
      TEAMCITY_TOKEN="$2"
      shift 2
      ;;
    --project-id)
      PROJECT_ID="$2"
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
    --interactive)
      INTERACTIVE=true
      shift
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
  if [[ -z "${TEAMCITY_TOKEN}" ||
    -z "${PROJECT_ID}" ||
    -z "${BRANCH_NAME}" ||
    -z "${COMMIT_SHA}" ]]; then
    printf "%s\n" "Missing required arguments." >&2
    usage
    exit 1
  fi
}

function print_header() {
  printf "\n%s was invoked with\n" "$0"
  printf "TeamCity URL  : %s\n" "${TEAMCITY_URL}"
  printf "Project ID    : %s\n" "${PROJECT_ID}"
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

function get_request() {
  local request_url="$1"
  curl \
    --silent \
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

  local build_type="${PROJECT_ID}_Trigger"
  local waiting=false

  local request_url="${TEAMCITY_BUILDS}?locator="
  request_url+="project:${PROJECT_ID},"
  request_url+="buildType:${build_type},"
  request_url+="branch:${ENCODED_BRANCH_NAME},"
  request_url+="revision:${COMMIT_SHA},"
  request_url+="state:any,"
  request_url+="count:1"

  while true; do
    local trigger
    trigger="$(get_request "${request_url}")"

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

# store the number of lines from the last message
declare -i LAST_UPDATE_MESSAGE_LINES=0

function update_messages() {
  local messages="$1"

  # Count lines in the new message
  local new_lines
  new_lines=$(printf "%b" "${messages}" | wc --lines)

  # If there is a previous message, move cursor up and clear all previous lines
  if ((LAST_UPDATE_MESSAGE_LINES > 0)); then
    # Move cursor up LAST_UPDATE_MESSAGE_LINES
    printf "\033[%dA" "${LAST_UPDATE_MESSAGE_LINES}"
    # Clear each line
    for ((i = 0; i < LAST_UPDATE_MESSAGE_LINES; i++)); do
      printf "\033[2K\n"
    done
    # Move cursor back up to the top line
    printf "\033[%dA" "${LAST_UPDATE_MESSAGE_LINES}"
  fi

  # Print the new message
  printf "%b\n" "${messages}"

  # Update line count
  LAST_UPDATE_MESSAGE_LINES=$((new_lines + 1))
}

function get_aggregate_teamcity_build_status() {
  local trigger_id="$1"

  local old_ids=()

  local target_builds_request_url="${TEAMCITY_BUILDS}?locator="
  target_builds_request_url+="affectedProject:${PROJECT_ID},"
  target_builds_request_url+="branch:${ENCODED_BRANCH_NAME},"
  target_builds_request_url+="revision:${COMMIT_SHA},"
  target_builds_request_url+="state:any,"
  target_builds_request_url+="canceled:any,"
  target_builds_request_url+="sinceBuild:${trigger_id},"
  target_builds_request_url+="count:1000"

  while true; do

    # Fetch all builds since last trigger
    local jobs

    jobs="$(get_request "${target_builds_request_url}")"

    local tracked_build_ids=""
    tracked_build_ids="$(jq -r '.build[]?.id' <<<"${jobs}" | tr -d '\r' | sort -nu | xargs)"

    if [ -z "${tracked_build_ids}" ]; then
      printf "%b No builds detected.\n" ${UNICODE_SUCCESS}
      return 0
    fi

    # Check status of all tracked builds

    local ids=()
    IFS=' ' read -r -a ids <<<"$tracked_build_ids"
    local states=()
    local statuses=()
    local web_urls=()
    local build_type_ids=()

    for id in "${ids[@]}"; do
      local request_url="${TEAMCITY_BUILDS}/id:${id}"
      local build_info
      build_info="$(get_request "${request_url}")"
      build_type_ids+=("$(printf "%s" "${build_info}" | jq -r '.buildTypeId')")
      states+=("$(printf "%s" "${build_info}" | jq -r '.state')")
      statuses+=("$(printf "%s" "${build_info}" | jq -r '.status')")
      web_urls+=("$(printf "%s" "${build_info}" | jq -r '.webUrl')")
    done

    local all_done=true
    for state in "${states[@]}"; do
      if [[ "${state}" != "finished" ]]; then
        all_done=false
        break
      fi
    done

    function get_progress() {
      local title="$1"
      local messages="\n${title}\n=======\n"
      for i in "${!ids[@]}"; do
        messages+="- Build ${build_type_ids[$i]}\n"
        messages+="  id: ${ids[$i]}\n"
        messages+="  State: ${states[$i]} $(get_state_unicode "${states[$i]}")\n"
        messages+="  Status: ${statuses[$i]} $(get_status_unicode "${statuses[$i]}")\n"
        messages+="  URL: ${web_urls[$i]}\n"
      done
      printf "%b" "${messages}"
    }

    if ${INTERACTIVE}; then
      local messages=""
      if [[ "${all_done}" = false ]]; then
        messages+="${UNICODE_WAIT} Waiting for builds to finish. Polling for updates every ${POLL_INTERVAL} seconds...\n"
        messages+="$(get_progress "Updates")"
        update_messages "${messages}"
      fi
    else
      if [[ "${old_ids[*]}" != "${ids[*]}" ]]; then
        old_ids=("${ids[@]}")
        printf "\n%b Monitoring the following build configurations (the list will be updated if new builds are triggered):\n" "${UNICODE_WAIT}"
        for i in "${!ids[@]}"; do
          printf "   -%s (id: %d): %s\n" "${build_type_ids[$i]}" "${ids[$i]}" "${web_urls[$i]}"
        done
      fi

    fi

    if [[ "${all_done}" = true ]]; then
      get_progress "Results"
      for status in "${statuses[@]}"; do
        if [[ "${status}" != "SUCCESS" ]]; then
          printf "\n%b One or more tracked builds were not successful.\n" "${UNICODE_FAILURE}"
          return 1
        fi
      done
      printf "\n%b All tracked builds finished successfully!\n" "${UNICODE_SUCCESS}"
      return 0
    fi

    count_sheep

  done
}

function main() {
  parse_args "$@"
  print_header
  ENCODED_BRANCH_NAME=$(encode_branch_name "${BRANCH_NAME}")
  local trigger_id
  trigger trigger_id
  get_aggregate_teamcity_build_status "${trigger_id}"
}

main "$@"
