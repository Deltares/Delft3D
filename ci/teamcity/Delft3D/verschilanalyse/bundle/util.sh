#! /usr/bin/env bash

# Check if the all of the passed variable names are set to a non-empty value.
# Returns a zero exit code if all variables are set, otherwise return a non-zero exit code.
# Example usage:
# ```bash
# export FOO=foo BAR=bar BAZ=baz
#
# # Zero exit code.
# util.check_vars_are_set FOO BAR BAZ
#
# # Error: QUX is not set.
# util.check_vars_are_set FOO BAR BAZ QUX
# ```
function util.check_vars_are_set {
    local vars=("$@")
    local return_non_zero=false

    for var in "${vars[@]}"; do
        if [[ -z "${!var}" ]]; then
            >&2 echo "Error: Variable ${var} is not set."
            return_non_zero=true
        fi
    done

    if [[ "${return_non_zero}" = true ]]; then
        return 1
    fi
}
export -f util.check_vars_are_set
