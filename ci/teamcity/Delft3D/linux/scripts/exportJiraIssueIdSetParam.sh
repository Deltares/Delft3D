#!/usr/bin/env bash
set -eo pipefail

BUILD_BRANCH="${BUILD_BRANCH:-%teamcity.build.branch%}"
PULL_REQUEST_BRANCH="${PULL_REQUEST_BRANCH:-%teamcity.pullRequest.source.branch%}"
PARAM_NAME="${PARAM_NAME:-%param_name%}"
DEFAULT_JIRA_ISSUE_ID="UNKNOWN-0000"

if [[ "${BUILD_BRANCH}" == *merge-request* ]]; then
    BRANCH_NAME="${PULL_REQUEST_BRANCH}"
else
    BRANCH_NAME="${BUILD_BRANCH}"
fi

JIRA_ISSUE_ID=$(echo "$BRANCH_NAME" | sed -n -e 's|^[a-z]\+/[a-z]\+/\([a-zA-Z0-9]\+-[0-9]\+\).*$|\1|p')
if [[ -z "${JIRA_ISSUE_ID}" ]]; then
    echo "Failed to extract the Jira issue ID from the branch. Using default value: ${DEFAULT_JIRA_ISSUE_ID}"
    JIRA_ISSUE_ID="${DEFAULT_JIRA_ISSUE_ID}"
fi

echo "##teamcity[setParameter name='${PARAM_NAME}' value='${JIRA_ISSUE_ID}']"
