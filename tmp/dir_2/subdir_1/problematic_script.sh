#!/usr/bin/env bash

function problematic_function() {
  # SC2155 : Declare and assign separately to avoid masking return values.
  local path=$(pwd)
  # SC2086: Double quote to prevent globbing and word splitting.
  echo ${path}
}
