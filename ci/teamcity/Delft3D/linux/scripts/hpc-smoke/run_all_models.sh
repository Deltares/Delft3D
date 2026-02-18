#!/bin/bash

# Source common utilities
# When running under SLURM, change to the submission directory first
if [ -n "$SLURM_SUBMIT_DIR" ]; then
    cd "$SLURM_SUBMIT_DIR"
fi

if [ -n "$SLURM_SUBMIT_DIR" ]; then
    source "$SLURM_SUBMIT_DIR/common_utilities.sh"
else
    script_dir="$(dirname "${BASH_SOURCE[0]}")"
    source "$script_dir/common_utilities.sh"
fi

submit_all_jobs() {
    local platform="$1"
    local submit_script_name="submit_$platform.sh"
    
    local dirs=$(find_dimr_directories)
    local job_ids=()
    
    for dir in $dirs; do
        echo "Found directory with dimr file: $dir, try to submit job" >&2
       
        # Submit job for this directory
        echo "    Submitting job using templated script '$submit_script_name' in directory '$(realpath "$dir")'" >&2
        cd "$dir" || { echo "    Failed to change directory to $dir" >&2; continue; }
        
        # Capture job ID from sbatch output
        local job_output=$(sbatch "$submit_script_name")
        local job_id=$(echo "$job_output" | grep -oE '[0-9]+')
        
        if [ -n "$job_id" ]; then
            job_ids+=("$job_id")
            echo "    Job submitted with ID: $job_id" >&2
        else
            echo "    Warning: Could not extract job ID from: $job_output" >&2
        fi
        
        cd - > /dev/null || { echo "    Failed to return to previous directory" >&2; exit 1; }
    done
    
    # Return job IDs as space-separated string
    echo "${job_ids[*]}"
}

submit_dependent_tc_job() {
    local job_ids_string="$1"
    
    # Convert space-separated string back to array
    local job_ids_array=($job_ids_string)
    
    if [ ${#job_ids_array[@]} -eq 0 ]; then
        echo "No job IDs captured, cannot submit dependent job"
        return 1
    fi
    
    # Create dependency string for sbatch
    local dependency_list=$(IFS=:; echo "${job_ids_array[*]}")
        
    # Submit the dependent job
    local sbatch_cmd="sbatch --dependency=afterany:${dependency_list} schedule_teamcity_receive_job_wrapper.sh"
    echo "Submitting TeamCity job with command: ${sbatch_cmd}"
    local tc_job_output=$(${sbatch_cmd})
    local tc_job_id=$(echo "$tc_job_output" | grep -oE '[0-9]+')
    
    if [ -n "$tc_job_id" ]; then
        echo "TeamCity scheduler job submitted with Slurm ID: $tc_job_id, dependent on jobs: ${dependency_list}"
    else
        echo "Warning: Could not extract job ID from scheduler submission: $tc_job_output"
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

print_help_run() {
    echo "Usage: $0 [h7|delftblue|snellius] [--run_dependent]"
    echo "  h7         : Run for H7"
    echo "  delftblue  : Run for DelftBlue"
    echo "  snellius   : Run for Snellius"
    echo "  --run_dependent : Optional flag to schedule a dependent TeamCity job"
    echo "                      (if provided, a dependent TeamCity job will be scheduled)"
}

main() {
    local platform="$1"
    
    # Validate and handle help first
    validate_and_handle_help_run "$platform"
    
    # Parse arguments for run_dependent flag
    local run_dependent
    run_dependent=$(parse_run_arguments "$@")
    
    echo "Running for platform: $platform"
    
    if [ -n "$run_dependent" ]; then
        echo "A dependent TeamCity job will be scheduled after all case jobs complete"
    fi
    
    # Submit jobs, capture job IDs
    local submitted_job_ids=$(submit_all_jobs "$platform")
    
    # Submit dependent TeamCity job
    if [ -n "$run_dependent" ]; then
        submit_dependent_tc_job "$submitted_job_ids"
    fi
    
    echo "All jobs processed successfully"
}

# Call main function with all arguments
main "$@"