#! /bin/bash
#SBATCH --job-name=va-trigger-teamcity-build
#SBATCH --time=00:10:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --partition=1vcpu

set -eo pipefail

if [[ \
    -z "$TEAMCITY_SERVER_URL" || -z "${REPORT_BUILD_TYPE_ID}" \
    || -z "${START_BUILD_TYPE_ID}" || -z "${BUILD_ID}" \
    || -z "${VCS_ROOT_ID}" || -z "${VCS_REVISION}" || -z "${BRANCH_NAME}" \
]]; then
    >&2 echo "One of TEAMCITY_SERVER_URL, REPORT_BUILD_TYPE_ID, START_BUILD_TYPE_ID,"
    >&2 echo "BUILD_ID, VCS_ROOT_ID, VCS_REVISION or BRANCH_NAME not set."
    exit 1
fi

curl --fail --silent --show-error -X POST \
    --header "Authorization: Bearer $(cat "${HOME}/.teamcity/verschilanalyse-token")" \
    --header "Accept: application/json" \
    --header "Content-Type: application/json" \
    --data-binary @- <<EOF \
    "${TEAMCITY_SERVER_URL}/app/rest/buildQueue"
{
    "buildTypeId": "${REPORT_BUILD_TYPE_ID}",
    "revisions": {
        "count": 1,
        "revision": [
            {
                "version": "${VCS_REVISION}",
                "vcsBranchName": "${BRANCH_NAME}",
                "vcs-root-instance": {
                    "vcs-root-id": "${VCS_ROOT_ID}"
                }
            }
        ]
    },
    "properties": {
        "property": [
            {
                "name": "report_prefix",
                "value": "${OUTPUT_PREFIX}/report"
            }
        ]
    },
    "snapshot-dependencies": {
        "count": 1,
        "build": [
            {
                "id": ${BUILD_ID},
                "buildTypeId": "${START_BUILD_TYPE_ID}"
            }
        ]
    }
}
EOF
