#!/usr/bin/env bash
set -eo pipefail

HARBOR_REPO="${HARBOR_REPO:-%harbor_repo%}"
BUILD_TYPE="${BUILD_TYPE:-%build_type%}"
INTEL_ONEAPI_VERSION="${INTEL_ONEAPI_VERSION:-%intel_oneapi_version%}"
INTEL_FORTRAN_COMPILER="${INTEL_FORTRAN_COMPILER:-%intel_fortran_compiler%}"

BUILDTOOLS_IMAGE_TAG="oneapi-${INTEL_ONEAPI_VERSION}"
IMAGE_TAG="oneapi-${INTEL_ONEAPI_VERSION}-${INTEL_FORTRAN_COMPILER}-${BUILD_TYPE,,}"
CACHE_FROM_ARGS="--cache-from type=registry,ref=${HARBOR_REPO}:${IMAGE_TAG}-cache"
if [[ -n "$JIRA_ISSUE_ID" ]]; then
    BUILDTOOLS_IMAGE_TAG="${JIRA_ISSUE_ID}-${BUILDTOOLS_IMAGE_TAG}"
    IMAGE_TAG="${JIRA_ISSUE_ID}-${IMAGE_TAG}"
    CACHE_FROM_ARGS="--cache-from type=registry,ref=${HARBOR_REPO}:${IMAGE_TAG}-cache ${CACHE_FROM_ARGS}"
fi

DEBUG=0
if [[ "$BUILD_TYPE" == 'Debug' ]]; then
    DEBUG=1
fi

echo "##teamcity[setParameter name='env.DEBUG' value='$DEBUG']"
echo "##teamcity[setParameter name='env.BUILDTOOLS_IMAGE_TAG' value='$BUILDTOOLS_IMAGE_TAG']"
echo "##teamcity[setParameter name='env.IMAGE_TAG' value='$IMAGE_TAG']"
echo "##teamcity[setParameter name='env.CACHE_FROM_ARGS' value='$CACHE_FROM_ARGS']"
