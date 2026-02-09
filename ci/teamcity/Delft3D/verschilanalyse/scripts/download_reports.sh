#!/usr/bin/env bash
set -eo pipefail

CURRENT_PREFIX='s3://devops-test-verschilanalyse/%current_prefix%'
REFERENCE_PREFIX='s3://devops-test-verschilanalyse/%reference_prefix%'
REFERENCE_TAG="${REFERENCE_PREFIX##*/}"

aws --endpoint-url=https://s3.deltares.nl \
    s3 cp "${CURRENT_PREFIX}/logs/logs.zip" current_logs.zip
aws --endpoint-url=https://s3.deltares.nl \
    s3 cp "${REFERENCE_PREFIX}/logs/logs.zip" reference_logs.zip
aws --endpoint-url=https://s3.deltares.nl \
    s3 cp "${CURRENT_PREFIX}/verschillentool/${REFERENCE_TAG}/verschillen.zip" verschillen.zip
