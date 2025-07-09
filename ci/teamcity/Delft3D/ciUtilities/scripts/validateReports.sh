#!/usr/bin/env bash
set -eo pipefail

if [[ ! -e 'report/ruff_check.xml' || ! -e 'report/pytest.xml' ]]; then
    >&2 echo "ERROR: Missing jUnit XML test reports."
    exit 1
fi
# The XmlReport build feature complains if the files are 'outdated',
# i.e. when the files' `mtime` is older than the current build. Because we
# use docker build with the `--output` option, these files may be outdated 
# because they may be fetched from the layer cache.
touch report/*.xml

# Fail the build if the formatter patch does not exist or is not empty.
if [[ ! -e 'report/ruff_format.patch' || -s 'report/ruff_format.patch' ]]; then
    >&2 echo "ERROR: The source files are not formatted properly."
    >&2 echo "Please check 'report/ruff_format.patch' for the desired changes."
    exit 1
fi