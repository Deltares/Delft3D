#!/usr/bin/env bash
set -eo pipefail

HARBOR_REPO="${HARBOR_REPO:-%harbor_repo%}"
PYTHON_VERSION="${PYTHON_VERSION:-%python_version%}"
ALMALINUX_BASE_VERSION="${ALMALINUX_BASE_VERSION:-%almalinux-base-version%}"

IMAGE_TAG="alma${ALMALINUX_BASE_VERSION}-python${PYTHON_VERSION}"
CACHE_FROM_ARGS="--cache-from type=registry,ref=${HARBOR_REPO}:${IMAGE_TAG}-cache"
if [[ -n "$JIRA_ISSUE_ID" ]]; then
    IMAGE_TAG="${JIRA_ISSUE_ID}-${IMAGE_TAG}"
    CACHE_FROM_ARGS="--cache-from type=registry,ref=${HARBOR_REPO}:${IMAGE_TAG}-cache ${CACHE_FROM_ARGS}"
fi

echo "##teamcity[setParameter name='env.IMAGE_TAG' value='$IMAGE_TAG']"
echo "##teamcity[setParameter name='env.CACHE_FROM_ARGS' value='$CACHE_FROM_ARGS']"
