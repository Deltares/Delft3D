#! /bin/bash
#SBATCH --job-name=va-trigger-teamcity-build
#SBATCH --time=00:10:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --partition=4vcpu
#SBATCH --cpus-per-task=4

set -eo pipefail

VARIABLES=( \
    "BUCKET" "CURRENT_PREFIX" "REFERENCE_PREFIX" "LOG_DIR" "SEND_EMAIL" \
    "TEAMCITY_SERVER_URL" "REPORT_BUILD_TYPE_ID" "START_BUILD_TYPE_ID" \
    "BUILD_ID" "VCS_ROOT_ID" "VCS_REVISION" "BRANCH_NAME" \
)
if ! util.check_vars_are_set "${VARIABLES[@]}" ; then
    >&2 echo "Abort"
    exit 1
fi

pushd "$LOG_DIR"
zip -r logs.zip .
shopt -s extglob
rm -rf !(logs.zip)
popd

# Upload logs to MinIO.
docker run --rm \
    --volume="${HOME}/.aws:/root/.aws:ro" --volume="${LOG_DIR}:/data:ro" \
    docker.io/amazon/aws-cli:2.22.7 \
    --profile=verschilanalyse --endpoint-url=https://s3.deltares.nl \
    s3 sync --delete --no-progress /data "${BUCKET}/${CURRENT_PREFIX}/logs"

# Trigger teamcity 'Report' build.
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
        "count": 3,
        "property": [
            {
                "name": "current_prefix",
                "value": "${CURRENT_PREFIX}"
            },
            {
                "name": "reference_prefix",
                "value": "${REFERENCE_PREFIX}"
            },
            {
                "name": "send_email",
                "value": "${SEND_EMAIL}"
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
