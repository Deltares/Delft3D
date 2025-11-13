#!/usr/bin/env bash
#SBATCH --job-name=va-run-verschillentool
#SBATCH --time=04:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=16
#SBATCH --partition=16vcpu_spot
#SBATCH --account=verschilanalyse
#SBATCH --qos=verschilanalyse

set -eo pipefail
shopt -s extglob


VERSCHILLENTOOL_DIR="${VAHOME}/verschillentool"

function validate_inputs() {
	if ! util.check_vars_are_set BUCKET VAHOME CURRENT_PREFIX REFERENCE_PREFIX MODEL_REGEX ; then
		>&2 echo "Abort"
		return 1
	fi
}

function create_verschillentool_dir() {
	if [ ! -d "$VERSCHILLENTOOL_DIR" ]; then 
		mkdir -p "$VERSCHILLENTOOL_DIR"
	else
		rm -rf "$VERSCHILLENTOOL_DIR"
		mkdir -p "$VERSCHILLENTOOL_DIR"
	fi
}

function docker_login() {
    local registry_url="containers.deltares.nl"
    local credentials_file="${HOME}/.harbor/verschillentool"
    local username="robot\$verschillentool+h7"

    if [[ ! -f "$credentials_file" ]]; then
        echo "Error: credentials file not found at $credentials_file" >&2
        return 1
    fi

    echo "Logging into Docker registry: $registry_url"
    if ! docker login \
        --username="$username" \
        --password-stdin \
        "$registry_url" < "$credentials_file"; then
        echo "Error: Docker login failed for $registry_url" >&2
        return 1
    fi
}

# Run verschillentool (all configs).
function run_verschillentool() {
	find config -name '*.json' -iregex "$MODEL_REGEX" -exec docker run --rm \
		--volume="${VAHOME}/input:/data/input:ro" \
		--volume="${VAHOME}/reference:/data/reference:ro" \
		--volume="${PWD}/{}:/data/{}:ro" \
		--volume="${VERSCHILLENTOOL_DIR}:/data/verschillentool" \
		containers.deltares.nl/verschillentool/verschillentool:release_v1.0.2 --config "/data/{}" ';'
}

# Create verschillen archive.
function create_verschillen_archive() {
	local DIR=$1
	pushd "$DIR" > /dev/null || return 1

	#Create archive
    zip -r "verschillen.zip" . || {
        echo "Failed to create archive" >&2
        popd > /dev/null
        return 1
    }

    # Remove everything except the archive
    rm -rf !(verschillen.zip)

    popd > /dev/null
}

# Upload verschillen archive to MinIO.
function upload_verschillen() {
	# Use the last part of the REFERENCE_PREFIX as the REFERENCE_TAG
	local REFERENCE_TAG="${REFERENCE_PREFIX##*/}"
	docker run --rm \
		--volume="${HOME}/.aws:/root/.aws:ro" --volume="${VERSCHILLENTOOL_DIR}:/data:ro" \
		docker.io/amazon/aws-cli:2.22.7 \
		--profile=verschilanalyse --endpoint-url=https://s3.deltares.nl \
		s3 sync --delete --no-progress /data "${BUCKET}/${CURRENT_PREFIX}/verschillentool/${REFERENCE_TAG}"
}

function main() {
	validate_inputs
	create_verschillentool_dir
	docker_login
	run_verschillentool
	create_verschillen_archive "$VERSCHILLENTOOL_DIR"
	upload_verschillen
}

# Only run main when executed directly (not when sourced for tests)
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
  main "$@"
fi
