#!/usr/bin/env bash
set -eo pipefail

USAGE_STRING="Usage: postDvcDiffReport.sh <target_branch> [<PULL_REQUEST_NUMBER> <GITHUB_BARER_TOKEN>]"
if [ "$#" -lt 1 ]; then 
    echo "Not enough arguments provided. $USAGE_STRING"
fi

if [ "$#" -gt 3 ]; then 
    echo "Too many arguments provided. $USAGE_STRING"
fi

TARGET_BRANCH="$1" 
PULL_REQUEST_NUMBER="$2"
GITHUB_BARER_TOKEN="$3"

SOURCE_BRANCH="$(git rev-parse HEAD)"
MERGE_BASE_COMMIT_HASH=$(git merge-base "$TARGET_BRANCH" HEAD)
POST_URL="https://api.github.com/repos/deltares/delft3d/issues/$PULL_REQUEST_NUMBER/comments"

# setup env
uv venv --python=3.11 .venv
uv pip sync test/deltares_testbench/pip/lnx-dev-requirements.txt
source .venv/bin/activate
uv pip install jinja2-cli

# Generate the report
dvc diff "$MERGE_BASE_COMMIT_HASH" "$SOURCE_BRANCH" --json > diff.json
jinja2 ci/teamcity/Delft3D/ciUtilities/scripts/diff-report-template.jinja diff.json --lstrip-blocks --trim-blocks -o report.md

# 
if [ -z "$PULL_REQUEST_NUMBER" ]; then
    echo "PULL_REQUEST_NUMBER was not provided, so report was generated but not posted to github"
    exit 0
fi 

if [ -z "$GITHUB_BARER_TOKEN" ]; then
    echo "GITHUB_BARER_TOKEN was not provided, so report was generated but not posted to github"
    exit 0
fi

# check if report.md is empty. if not then there were definitely no dvc changes
if [ -s report.md ]; then
    # use jq to format the generated report as a valid JSON payload
    PAYLOAD="$(jq -c -n --rawfile body report.md '$ARGS.named')"

    # Post the report
    curl -L \
        --fail \
        -X POST \
        -H "Accept: application/vnd.github+json" \
        -H "Authorization: Bearer $GITHUB_BARER_TOKEN" \
        -H "X-GitHub-Api-Version: 2022-11-28" \
        "$POST_URL" \
        -d "$PAYLOAD"
else
    echo "No dvc changes detected, therefore no report was generated"
    exit 0
fi

            