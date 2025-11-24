#!/usr/bin/env bash
set -e

while [[ $# -gt 0 ]]; do
    case "$1" in
        --brand)
            BRAND="$2"
            shift 2
            ;;
        --release-version)
            RELEASE_VERSION="$2"
            shift 2
            ;;
        --commit-id-short)
            COMMIT_ID_SHORT="$2"
            shift 2
            ;;
        *)
            echo "Unknown option: $1"
            exit 1
            ;;
    esac
done

ZIP_FILE="/opt/Testdata/DIMR/DIMR_collectors/DIMRset_lnx64_Docker/docker_${BRAND}_${RELEASE_VERSION}-${COMMIT_ID_SHORT}.zip"
pushd examples/dflowfm
    rm -f *.*
    mkdir examples
    shopt -s extglob
    mv -v !(examples) examples
    rm -vf examples/*/run.{sh,bat}
    rm -vf examples/*/run_apptainer.sh
    rm -vf "${ZIP_FILE}"
    zip -vr "${ZIP_FILE}" examples
popd
pushd ci/teamcity/Delft3D/linux/docker
    zip -g "${ZIP_FILE}" readme.txt
popd
zip -g "${ZIP_FILE}" "${BRAND}"_*.tar
