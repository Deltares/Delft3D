#!/usr/bin/env bash
set -eo pipefail

# Globals to be set by parse_args
SOURCE_BRANCH=
TARGET_BRANCH=
FILTER=

function usage() {
    cat << EOF
Usage: determineChangedFileNames.sh [OPTIONS]

Options:
--target-branch <branch_name>: required, the name or ref of the target branch you want to compare 
--source-branch <branch_name>: required, the name or ref of the source branch to comapre to target
--filter=<pattern>: a grep pattern that files much match to be returned.  example: --file-pattern="md\|f90" will show only markdown and fortran 90 files.

EOF

}

function parse_args() {
  local long_options="help::,target-branch:,source-branch:,filter::"
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
    --source-branch)
      SOURCE_BRANCH="$2"
      shift 2
      ;;
    --target-branch)
      TARGET_BRANCH="$2"
      shift 2
      ;;
    --filter)
      FILTER="$2"
      shift 2
      ;;
    --debug)
      DEBUG=true
      shift 1
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
  if [[ -z "${SOURCE_BRANCH}" || -z "${TARGET_BRANCH}"  ]]; then
    printf "One or more required arguments were not provided.\n" >&2
    usage
    exit 1
  fi
}

function main() {
  parse_args "$@"

  local all_files="$(git diff "$TARGET_BRANCH" "$SOURCE_BRANCH" --name-only)"
  local files_filtered="$( printf  "%s" "$all_files" | grep -i "$FILTER")"

  printf "%s\n" "$files_filtered"
}

main "$@"
