#!/usr/bin/env bash
set -euo pipefail

# Globals to be set by parse_args
TEAMCITY_URL="https://dpcbuild.deltares.nl"
TEAMCITY_TOKEN=""
PROJECT_ID="Delft3D"
BRANCH_NAME=""
COMMIT_SHA=""
POLL_INTERVAL=30
TIMEOUT=1800

ENCODED_BRANCH_NAME=""
TEAMCITY_BUILDS="${TEAMCITY_URL}/app/rest/builds"

function usage() {
  cat <<EOF
Usage: $0 [OPTIONS]

Options:
  --teamcity-token TOKEN      TeamCity access token or password
  --branch-name NAME          Branch name to monitor (will be URL-encoded automatically)
  --commit-sha SHA            Commit SHA
  --poll-interval SECONDS     Polling interval in seconds (default: 30)
  --timeout SECONDS           Timeout in seconds (default: 1800)
  --help                      Show this help message
EOF
}

parse_args() {
  local long_options="help,teamcity-token:,branch-name:,commit-sha:,poll-interval:,timeout:"
  local parsed_options
  if ! parsed_options=$(getopt --name "$(basename "$0")" --options "" --long ${long_options} -- "$@"); then
    echo "parse_args: failed to parse arguments."
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
    --timeout)
      TIMEOUT="$2"
      shift 2
      ;;
    --)
      shift
      break
      ;;
    *)
      echo "Internal error!" >&2
      exit 1
      ;;
    esac
  done

  # Validate required params
  if [[ -z "${TEAMCITY_TOKEN}" || -z "${BRANCH_NAME}" || -z "${COMMIT_SHA}" ]]; then
    echo "Missing required arguments." >&21
    usage
    exit 1
  fi
}

function print_header() {
  echo -e "\n$0 invoked with"
  echo "TeamCity URL  : ${TEAMCITY_URL}"
  echo "Project ID    : ${PROJECT_ID}"
  echo "Branch name   : ${BRANCH_NAME}"
  echo "Commit SHA    : ${COMMIT_SHA}"
  echo "Poll interval : ${POLL_INTERVAL}"
  echo "Timeout       : ${TIMEOUT}"
  echo -e "--\n"
}

function encode_branch_name() {
  local branch_name="$1"
  local encoded_branch_name
  encoded_branch_name="$(jq -rn --arg v "${branch_name}" '$v|@uri')"
  echo "${encoded_branch_name}"
}

function count_sheep() {
  sleep "${POLL_INTERVAL}"
}

function get_build_info() {
  local id="$1"
  curl \
    --silent \
    --request "GET" \
    "${TEAMCITY_BUILDS}/id:${id}" \
    --header "Authorization: Bearer ${TEAMCITY_TOKEN}" \
    --header "Accept: application/json"
}

function trigger() {
  declare -n id="$1"

  local build_type="Delft3D_Trigger"
  local waiting=false
  while true; do
    local trigger
    trigger=$(
      curl \
        --silent \
        --request "GET" \
        "${TEAMCITY_BUILDS}?locator=project:${PROJECT_ID},buildType:${build_type},branch:${ENCODED_BRANCH_NAME},revision:${COMMIT_SHA},state:any,count:1" \
        --header "Authorization: Bearer ${TEAMCITY_TOKEN}" \
        --header "Accept: application/json"
    )

    local state
    state=$(echo "${trigger}" | jq -r '.build[0].state')
    local status
    status=$(echo "${trigger}" | jq -r '.build[0].status')

    if [ "${state}" != "finished" ]; then
      if [ "${waiting}" = false ]; then
        echo "⏳ Trigger not finished yet. Polling every ${POLL_INTERVAL} seconds..."
        waiting=true
      fi
      count_sheep
      continue
    elif [ "${status}" != "SUCCESS" ]; then
      echo "❌ Trigger failed. Tracking of the remaining jobs is no longer possible."
      return 1
    fi

    echo "✅ Trigger finished successfully!"

    # shellcheck disable=SC2034
    id=$(echo "${trigger}" | jq -r '.build[0].id')

    return 0
  done

}

function get_aggregate_teamcity_build_status() {
  local trigger_id="$1"

  local tracked_build_ids=""
  while true; do

    # Fetch all builds since last trigger
    local jobs
    jobs=$(
      curl \
        --silent \
        --request "GET" \
        "${TEAMCITY_BUILDS}?locator=affectedProject:${PROJECT_ID},branch:${ENCODED_BRANCH_NAME},revision:${COMMIT_SHA},state:any,canceled:any,sinceBuild:${trigger_id},count:1000&fields=build(id)" \
        --header "Authorization: Bearer ${TEAMCITY_TOKEN}" \
        --header "Accept: application/json"
    )

    tracked_build_ids=$(jq -r '.build[]?.id' <<<"${jobs}" | tr -d '\r' | sort -nu | xargs)

    if [ -z "${tracked_build_ids}" ]; then
      echo "✅ No builds detected for branch ${BRANCH_NAME}."
      return 0
    fi

    # Check status of all tracked builds
    local states=()
    local statuses=()

    for id in ${tracked_build_ids}; do
      local build_info
      build_info="$(get_build_info "${id}")"
      local state
      state=$(echo "${build_info}" | jq -r '.state')
      states+=("${state}")
      local status
      status=$(echo "${build_info}" | jq -r '.status')
      statuses+=("${status}")
      local web_url
      web_url=$(echo "${build_info}" | jq -r '.webUrl')
      echo "Build ${id} → State: ${state} | Status: ${status} | URL: ${web_url}"
    done

    local all_done=true
    for state in "${states[@]}"; do
      if [[ "${state}" != "finished" ]]; then
        all_done=false
        break
      fi
    done

    if [ "${all_done}" = true ]; then
      for status in "${statuses[@]}"; do
        if [[ "${status}" != "SUCCESS" ]]; then
          echo "❌ One or more builds failed."
          return 1
        fi
      done
      echo "✅ All builds for branch ${BRANCH_NAME} finished successfully!"
      return 0
    fi

    echo "⏳ Still waiting for builds. Will poll again in ${POLL_INTERVAL} seconds..."
    count_sheep

  done
}

main() {
  parse_args "$@"
  print_header
  ENCODED_BRANCH_NAME=$(encode_branch_name "${BRANCH_NAME}")
  # sleep 60 # delay to allow the trigger job to queue
  local trigger_id
  trigger trigger_id
  get_aggregate_teamcity_build_status "${trigger_id}"
}

main "$@"
