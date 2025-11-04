#! /bin/bash
#SBATCH --job-name=va-run-verschillentool
#SBATCH --time=04:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=16
#SBATCH --partition=16vcpu_spot
#SBATCH --account=verschilanalyse
#SBATCH --qos=verschilanalyse

set -eo pipefail

if ! util.check_vars_are_set BUCKET VAHOME CURRENT_PREFIX REFERENCE_PREFIX MODELS_PATH MODEL_REGEX ; then
    >&2 echo "Abort"
    exit 1
fi

VERSCHILLENTOOL_DIR="${VAHOME}/verschillentool"
rm -rf "$VERSCHILLENTOOL_DIR"
mkdir "$VERSCHILLENTOOL_DIR"

docker login \
    --username="robot\$delft3d+h7"  \
    --password-stdin \
    containers.deltares.nl < "${HOME}/.harbor/delft3d"

# Template JSON config files to use the configurable models path
find config -name '*.json' -exec sed -i "s|/data/input/|/data/${MODELS_PATH}/|g" {} \;

# Function to check if all NetCDF files referenced in a JSON config exist
check_model_files_exist() {
    local json_file="$1"
    
    # Extract file paths from JSON (looking for .nc files)
    local nc_paths=$(grep -o '"/data/[^"]*\.nc"' "$json_file" | tr -d '"' | sed "s|^/data/|${VAHOME}/|")
    
    for nc_path in $nc_paths; do
        if [[ ! -f "$nc_path" ]]; then
            echo "Missing file: $nc_path (from config: $json_file)"
            return 1
        fi
    done
    return 0
}

# Run verschillentool only for configs with available model data
for json_config in $(find config -name '*.json' -iregex "$MODEL_REGEX"); do
    if check_model_files_exist "$json_config"; then
        echo "Running verschillentool for: $json_config"
        docker run --rm \
            --volume="${VAHOME}/${MODELS_PATH}:/data/${MODELS_PATH}:ro" \
            --volume="${VAHOME}/reference:/data/reference:ro" \
            --volume="${PWD}/${json_config}:/data/${json_config}:ro" \
            --volume="${VERSCHILLENTOOL_DIR}:/data/verschillentool" \
            containers.deltares.nl/delft3d/verschillentool:release_v1.0.2 --config "/data/${json_config}"
    else
        echo "Skipping verschillentool for: $json_config (missing model files)"
    fi
done

# Use the last part of the REFERENCE_PREFIX as the REFERENCE_TAG
REFERENCE_TAG="${REFERENCE_PREFIX##*/}"

# Create verschillen archive.
pushd "$VERSCHILLENTOOL_DIR"
zip -r "verschillen.zip" .
shopt -s extglob
rm -rf !(verschillen.zip)
popd

# Upload verschillen archive to MinIO.
docker run --rm \
    --volume="${HOME}/.aws:/root/.aws:ro" --volume="${VERSCHILLENTOOL_DIR}:/data:ro" \
    docker.io/amazon/aws-cli:2.22.7 \
    --profile=verschilanalyse --endpoint-url=https://s3.deltares.nl \
    s3 sync --delete --no-progress /data "${BUCKET}/${CURRENT_PREFIX}/verschillentool/${REFERENCE_TAG}"
