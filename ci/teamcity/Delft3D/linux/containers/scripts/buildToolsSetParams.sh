#!/usr/bin/env bash
set -eo pipefail

HARBOR_REPO="${HARBOR_REPO:-%harbor_repo%}"
INTEL_ONEAPI_VERSION="${INTEL_ONEAPI_VERSION:-%intel_oneapi_version%}"

IMAGE_TAG="oneapi-${INTEL_ONEAPI_VERSION}"
CACHE_FROM_ARGS="--cache-from type=registry,ref=${HARBOR_REPO}:${IMAGE_TAG}-cache"
if [[ -n "$JIRA_ISSUE_ID" ]]; then
    IMAGE_TAG="${JIRA_ISSUE_ID}-${IMAGE_TAG}"
    CACHE_FROM_ARGS="--cache-from type=registry,ref=${HARBOR_REPO}:${IMAGE_TAG}-cache ${CACHE_FROM_ARGS}"
fi

echo "##teamcity[setParameter name='env.IMAGE_TAG' value='$IMAGE_TAG']"
echo "##teamcity[setParameter name='env.CACHE_FROM_ARGS' value='$CACHE_FROM_ARGS']"
