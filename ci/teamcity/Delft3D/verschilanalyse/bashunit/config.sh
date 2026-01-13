#!/usr/bin/env bash

ROOT_DIR="$(dirname "$(realpath "$0")")"

export ROOT_DIR
export TEST_DIR="${ROOT_DIR}/tests"
export SRC_DIR="${ROOT_DIR}/src"
