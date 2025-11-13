#!/usr/bin/env bash

path=$(dirname "$(realpath "$0")")
bash_baton_image="containers.deltares.nl/bashbaton/bashbaton:release_v1.0.0"

container_name="VA-tests"
test_dir="${path}/../bashunit"
bundle_dir="${path}/../bundle"
destination_test_dir="/workspace/bashunit"
destination_bundle_dir="/workspace/bundle"

# Disable Git Bash path rewriting
MSYS_NO_PATHCONV=1 \
docker run \
  --rm \
  --name "${container_name}" \
  --volume "${test_dir}:${destination_test_dir}:ro" \
  --volume "${bundle_dir}:${destination_bundle_dir}:ro" \
  "${bash_baton_image}" \
  "${destination_test_dir}/main.sh"\
