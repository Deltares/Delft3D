#!/usr/bin/env bash
set -eo pipefail

# Globals to be set by parse_args
SOURCE_BRANCH=
TARGET_BRANCH=
FILE_PATTERN=
TEAMCITY_FORMAT=

function usage() {
    cat << EOF
Usage: determineChangedFileNames.sh [OPTIONS]

Options:
--target-branch <branch_name>: required, the name or ref of the target branch you want to compare 
--source-branch <branch_name>: required, the name or ref of the source branch to comapre to target
--file-pattern=<pattern>: a grep pattern that files much match to be returned.  example: --file-pattern="md\|f90" will show only markdown and fortran 90 files.
--teamcity: output the files in teamcity format so it can set them as output parameters for other build steps

EOF

}

function parse_args() {
  local long_options="help::,target-branch:,source-branch:,file-pattern::,teamcity"
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
      echo "setting source branch"
      shift 2
      ;;
    --target-branch)
      TARGET_BRANCH="$2"
      shift 2
      ;;
    --file-pattern)
      FILE_PATTERN="$2"
      shift 2
      ;;
    --teamcity)
      TEAMCITY_FORMAT=true
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
  if [[ -z "${SOURCE_BRANCH}" ||
    -z "${TARGET_BRANCH}" 
    ]]; then
    printf "One or more required arguments were not provided.\n" >&2
    usage
    exit 1
  fi
}

# for debugging only
function _debug_print() {
    echo "source branch: $SOURCE_BRANCH"
    echo "target branch: $TARGET_BRANCH"
    echo "file pattern: $FILE_PATTERN"
    local merge_base="$(git merge-base $SOURCE_BRANCH $TARGET_BRANCH)"
    if [[ -z "$merge_base" ]]; then
        echo "could not determine merge_base"
    else
        echo "merge_base: $merge_base"
    fi

    echo ""
}


function main() {
  parse_args "$@"
  _debug_print
  local files="$(git diff "$TARGET_BRANCH" "$SOURCE_BRANCH" --name-only | grep "$FILE_PATTERN")"
  if [ "$TEAMCITY_FORMAT" ]; then
      printf "##teamcity[setParameter name='changedFiles' value='%s']" "$files"
  else
      printf "%s" "$files"
  fi
 }

main "$@"
