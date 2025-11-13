#!/usr/bin/env bash

ROOT_DIR="$(dirname "$(realpath "$0")")"

export ROOT_DIR
export TEST_DIR="${ROOT_DIR}/tests"
export BASHUNIT_DIR="${ROOT_DIR}/bashunit"
