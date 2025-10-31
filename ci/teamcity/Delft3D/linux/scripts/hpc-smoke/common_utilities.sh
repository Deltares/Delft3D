#!/bin/bash

# Common utilities for Delft3D model processing scripts
# This file contains shared functions used by prepare_all_models.sh and run_all_models.sh

print_help_prepare() {
    echo "Usage: $0 [h7|delftblue|snellius] [--config CONFIG_FILE]"
    echo "  h7         : Run for H7"
    echo "  delftblue  : Run for DelftBlue"
    echo "  snellius   : Run for Snellius"
    echo "  --config CONFIG_FILE : Optional JSON configuration file specifying nodes and tasks for models"
    echo "                         If not provided or model not found in config, interactive mode will be used"
}

print_help_run() {
    echo "Usage: $0 [h7|delftblue|snellius] [--run_dependent]"
    echo "  h7         : Run for H7"
    echo "  delftblue  : Run for DelftBlue"
    echo "  snellius   : Run for Snellius"
    echo "  --run_dependent : Optional flag to schedule a dependent TeamCity job"
    echo "                      (if provided, a dependent TeamCity job will be scheduled)"
}

find_dimr_directories() {
    find . -type f \( -name "dimr.xml" -o -name "dimr_config.xml" \) -exec dirname {} \; | sort -u
}

is_supported_platform() {
    local platform="$1"
    case "$platform" in
        "h7"|"delftblue"|"snellius")
            return 0
            ;;
        *)
            return 1
            ;;
    esac
}

# Argument parsing for run script (with --run_dependent option)
parse_run_arguments() {
    local platform="$1"
    shift  # Remove platform from arguments
    
    local run_dependent=""
    
    while [[ $# -gt 0 ]]; do
        case "$1" in
            --run_dependent)
                run_dependent="true"
                shift
                ;;
            *)
                echo "Unknown option: $1"
                print_help_run
                exit 1
                ;;
        esac
    done
    
    echo "$run_dependent"
}

# Argument parsing for prepare script (no --run_dependent option)
parse_prepare_arguments() {
    local platform="$1"
    shift  # Remove platform from arguments
    
    while [[ $# -gt 0 ]]; do
        case "$1" in
            --config)
                if [ -n "$2" ] && [ "${2:0:1}" != "-" ]; then
                    CONFIG_FILE="$2"
                    shift 2
                else
                    echo "Error: --config requires a file path argument"
                    print_help_prepare
                    exit 1
                fi
                ;;
            *)
                echo "Unknown option: $1"
                print_help_prepare
                exit 1
                ;;
        esac
    done
    
    # Always return empty string since prepare doesn't support run_dependent
    echo ""
}

# Common validation and help handling
validate_and_handle_help_prepare() {
    local platform="$1"
    
    # Handle help request
    if [ "$platform" = "-h" ] || [ "$platform" = "--help" ]; then
        print_help_prepare
        exit 0
    fi
    
    # Validate platform
    if ! is_supported_platform "$platform"; then
        echo "Unknown system: '$platform'"
        echo ""
        print_help_prepare
        exit 1
    fi
}

validate_and_handle_help_run() {
    local platform="$1"
    
    # Handle help request
    if [ "$platform" = "-h" ] || [ "$platform" = "--help" ]; then
        print_help_run
        exit 0
    fi
    
    # Validate platform
    if ! is_supported_platform "$platform"; then
        echo "Unknown system: '$platform'"
        echo ""
        print_help_run
        exit 1
    fi
}

# Common initialization logic
# Common initialization for prepare script
prepare_init() {
    local platform="$1"
    shift
    
    # Validate and handle help
    validate_and_handle_help_prepare "$platform"
    
    # Parse arguments (no --run_dependent option)
    local run_dependent
    run_dependent=$(parse_prepare_arguments "$platform" "$@")
    
    echo "Running for platform: $platform" >&2
    
    # Return empty string since prepare doesn't support run_dependent
    echo ""
}

# Common initialization for run script
run_init() {
    local platform="$1"
    shift
    
    # Validate and handle help
    validate_and_handle_help_run "$platform"
    
    # Parse arguments
    local run_dependent
    run_dependent=$(parse_run_arguments "$platform" "$@")
    
    echo "Running for platform: $platform" >&2
    
    if [ -n "$run_dependent" ]; then
        echo "A dependent TeamCity job will be scheduled after all case jobs complete" >&2
    fi
    
    # Return the run_dependent flag for the calling script
    echo "$run_dependent"
}