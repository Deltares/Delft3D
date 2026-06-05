#!/bin/bash

# Source common utilities
script_dir="$(dirname "${BASH_SOURCE[0]}")"
source "$script_dir/common_utilities.sh"

readonly TEMPLATE_SUBMIT_SCRIPT="template_submit.sh"
readonly APPTAINER_FOLDER="~/apptainer"

# Global variables for configuration
declare -A MODEL_CONFIG
declare -A PLATFORM_CONFIG
CONFIG_FILE=""

get_platform_config() {
    local platform="$1"
    
    case "$platform" in
        "h7")
            PLATFORM_CONFIG=(
                ["platform"]="h7"
                ["partition"]="4vcpu"
                ["memory"]=""
                ["module_load"]="module purge\\nmodule load intelmpi/2021.11.0"
                ["execute_script"]="execute_singularity_h7.sh"
                ["partitioning_cmd_template"]="srun -n 1 -N 1 \$apptainerFolder/\$execute_script --containerfolder \$apptainerFolder --modelfolder \$modelFolder dflowfm --partition:ndomains=\$SLURM_NTASKS:icgsolver=6 \$mduFile"
                ["simulation_cmd_template"]="srun \$apptainerFolder/\$execute_script --containerfolder \$apptainerFolder --modelfolder \$modelFolder dimr \$dimrFile"
            )
            ;;
        "delftblue")
            PLATFORM_CONFIG=(
                ["platform"]="delftblue"
                ["partition"]="compute"
                ["memory"]="#SBATCH --mem-per-cpu=2G"
                ["module_load"]="module purge\\nmodule load slurm\\nmodule load 2023rl\\nmodule load intel/oneapi-all\\nexport I_MPI_PMI_LIBRARY=/cm/shared/apps/slurm/current/lib64/libpmi2.so\\nexport FI_PROVIDER=tcp\\nexport I_MPI_FABRICS=shm:tcp"				
                ["execute_script"]="execute_singularity_delftblue.sh"
                ["partitioning_cmd_template"]="srun -n 1 -N 1 \$apptainerFolder/\$execute_script --containerfolder \$apptainerFolder --modelfolder \$modelFolder dflowfm --partition:ndomains=\$SLURM_NTASKS:icgsolver=6 \$mduFile"
                ["simulation_cmd_template"]="srun \$apptainerFolder/\$execute_script --containerfolder \$apptainerFolder --modelfolder \$modelFolder dimr \$dimrFile"
            )
            ;;
        "snellius")
            PLATFORM_CONFIG=(
                ["platform"]="snellius"
                ["partition"]="rome"
                ["memory"]=""
                ["module_load"]="module purge\\nmodule load 2023\\nmodule load intel/2023a"
                ["execute_script"]="execute_singularity_snellius.sh"
                ["partitioning_cmd_template"]="\$apptainerFolder/\$execute_script \$modelFolder run_dflowfm.sh --partition:ndomains=\$SLURM_NTASKS:icgsolver=6 \$mduFile"
                ["simulation_cmd_template"]="\$apptainerFolder/\$execute_script \$modelFolder run_dimr.sh -m \$dimrFile"
            )
            ;;
        *)
            return 1
            ;;
    esac
    
    return 0
}

parse_json_config() {
    local config_file="$1"
    
    if [ ! -f "$config_file" ]; then
        echo "Configuration file $config_file not found" >&2
        return 1
    fi
    
    if ! command -v jq &> /dev/null; then
        echo "Error: jq is required to parse JSON configuration but is not installed" >&2
        return 1
    fi
    
    # Validate JSON syntax
    if ! jq empty "$config_file" 2>/dev/null; then
        echo "Error: Invalid JSON syntax in $config_file" >&2
        return 1
    fi
    
    # Clear existing configuration
    MODEL_CONFIG=()
    
    # Parse models from JSON
    local models
    models=$(jq -r '.models | keys[]' "$config_file" 2>/dev/null)
    
    if [ $? -ne 0 ] || [ -z "$models" ]; then
        echo "Warning: No models found in configuration file or invalid format" >&2
        return 1
    fi
    
    # Load model configurations
    while IFS= read -r model; do
        local nodes tasks
        nodes=$(jq -r ".models[\"$model\"].nodes" "$config_file" 2>/dev/null)
        tasks=$(jq -r ".models[\"$model\"].tasks_per_node" "$config_file" 2>/dev/null)
        
        # Validate nodes and tasks
        if [[ "$nodes" =~ ^[0-9]+$ ]] && [ "$nodes" -gt 0 ] && \
           [[ "$tasks" =~ ^[0-9]+$ ]] && [ "$tasks" -gt 0 ]; then
            MODEL_CONFIG["$model"]="$nodes $tasks"
            echo "Loaded configuration for model '$model': $nodes nodes, $tasks tasks per node"
        else
            echo "Warning: Invalid nodes/tasks configuration for model '$model', will use interactive mode" >&2
        fi
    done <<< "$models"
    
    return 0
}

get_model_config_from_json() {
    local model_name="$1"
    
    if [ -n "${MODEL_CONFIG[$model_name]:-}" ]; then
        echo "${MODEL_CONFIG[$model_name]}"
        return 0
    fi
    
    return 1
}

get_nodes_and_tasks() {
    local model_name="$1"
    
    # First, try to get configuration from JSON
    if [ -n "$CONFIG_FILE" ]; then
        local config_result
        if config_result=$(get_model_config_from_json "$model_name"); then
            local nodes tasks
            read nodes tasks <<< "$config_result"
            echo "    Using configuration from JSON: $nodes nodes, $tasks tasks per node"
            echo "$config_result"
            return 0
        fi
    fi
    
    # Fall back to interactive mode
    if [ -n "$CONFIG_FILE" ]; then
        echo "    Model '$model_name' not found in configuration file, using interactive mode"
    else
        echo "    Using interactive mode"
    fi
    while true; do
        read -p "    Enter number of nodes and tasks per node '<nodes> <tasks>' (e.g.: '2 4', default if nothing is entered is '1 1'): " user_input
        
        # Use default values if no input provided
        if [ -z "$user_input" ]; then
            user_input="1 1"
        fi
        
        # Parse the input into two values
        read user_nodes user_tasks_per_node <<< "$user_input"
        
        # Validate both inputs
        if [[ "$user_nodes" =~ ^[0-9]+$ ]] && [ "$user_nodes" -gt 0 ] && \
           [[ "$user_tasks_per_node" =~ ^[0-9]+$ ]] && [ "$user_tasks_per_node" -gt 0 ]; then
            echo "$user_nodes $user_tasks_per_node"
            return 0
        else
            echo "    Error: Please enter two valid positive integers separated by a space (nodes tasks_per_node)"
        fi
    done
}

find_dimr_file() {
    local dir="$1"
    
    if [ -f "$dir/dimr_config.xml" ]; then
        echo "dimr_config.xml"
    elif [ -f "$dir/dimr.xml" ]; then
        echo "dimr.xml"
    else
        return 1
    fi
}

extract_working_dir() {
    local dir="$1"
    local dimr_file="$2"
    
    local working_dir=$(grep -o '<workingDir>[^<]*</workingDir>' "$dir/$dimr_file" | sed 's/<workingDir>//;s/<\/workingDir>//' | head -n1)
    
    if [ -z "$working_dir" ] || [ "$working_dir" = "." ]; then
        echo "\${PWD}"
    else
        echo "\${PWD}/$working_dir"
    fi
}

extract_mdu_file() {
    local dir="$1"
    local dimr_file="$2"
    
    grep -o '<inputFile>[^<]*</inputFile>' "$dir/$dimr_file" | sed 's/<inputFile>//;s/<\/inputFile>//' | head -n1
}

substitute_template() {
    local file="$1"
    local pattern="$2"
    local replacement="$3"
    
    sed -i "s|$pattern|$replacement|g" "$file"
}

setup_submit_script() {
    local source_template="$1"
    local target_script="$2"
    
    if [ -f "$source_template" ]; then
        cp "$source_template" "$target_script"
        chmod +x "$target_script"
        return 0
    else
        echo "    $source_template not found!"
        return 1
    fi
}

substitute_all_templates() {
    local script_file="$1"
    local dimr_file="$2"
    local mdu_file_folder="$3"
    local mdu_file="$4"
    local job_name="$5"
    local user_nodes="$6"
    local user_tasks_per_node="$7"
    
    # Basic substitutions
    substitute_template "$script_file" "{SUBMIT_SCRIPT_NAME}" "$(basename "$script_file")"
    substitute_template "$script_file" "{NUMBER_OF_NODES}" "$user_nodes"
    substitute_template "$script_file" "{TASKS_PER_NODE}" "$user_tasks_per_node"
    substitute_template "$script_file" "{APPTAINER_FOLDER}" "$APPTAINER_FOLDER"
    substitute_template "$script_file" "{JOB_NAME}" "$job_name"
    substitute_template "$script_file" "{DIMR_FILE}" "$dimr_file"
    substitute_template "$script_file" "{MDU_FILE_FOLDER}" "$mdu_file_folder"
    substitute_template "$script_file" "{MDU_FILE}" "$mdu_file"
    
    # Platform-specific substitutions
    substitute_template "$script_file" "{PARTITION_NAME}" "${PLATFORM_CONFIG[partition]}"
    substitute_template "$script_file" "{SBATCH_MEMORY_RESERVATION}" "${PLATFORM_CONFIG[memory]}"
    substitute_template "$script_file" "{MODULE_LOAD_SECTION}" "${PLATFORM_CONFIG[module_load]}"
    substitute_template "$script_file" "{EXECUTE_SCRIPT}" "${PLATFORM_CONFIG[execute_script]}"
    substitute_template "$script_file" "{PARTITIONING_COMMAND}" "${PLATFORM_CONFIG[partitioning_cmd_template]}"
    substitute_template "$script_file" "{SIMULATION_COMMAND}" "${PLATFORM_CONFIG[simulation_cmd_template]}"
    substitute_template "$script_file" "{PLATFORM_NAME}" "${PLATFORM_CONFIG[platform]^^}"
}

prepare_directory() {
    local dir="$1"
    local platform="$2"
    local submit_script_name="$3"

    echo "    Preparing directory: $dir"

    # Setup submit script
    if ! setup_submit_script "$TEMPLATE_SUBMIT_SCRIPT" "$dir/$submit_script_name"; then
        return 1
    fi
    
    echo "    Copied $TEMPLATE_SUBMIT_SCRIPT to $dir/$submit_script_name"
    
    # Find dimr file
    local dimr_file
    if ! dimr_file=$(find_dimr_file "$dir"); then
        echo "    No dimr_config.xml or dimr.xml found in $dir"
        rm -f "$dir/$submit_script_name"
        return 1
    fi
    
    # Get user input for nodes and tasks
    local model_name=$(basename "$dir")
    echo "    Model: $model_name"
    local nodes_tasks_output
    nodes_tasks_output=$(get_nodes_and_tasks "$model_name")
    
    # Display all lines except the last one (status messages)
    echo "$nodes_tasks_output" | head -n -1
    
    # Get only the last line (the actual values)
    local nodes_tasks
    nodes_tasks=$(echo "$nodes_tasks_output" | tail -n 1)
    
    local user_nodes user_tasks_per_node
    read user_nodes user_tasks_per_node <<< "$nodes_tasks"
    echo "    Using $user_nodes nodes and $user_tasks_per_node tasks per node"
    
    # Extract file information
    local mdu_file_folder=$(extract_working_dir "$dir" "$dimr_file")
    local mdu_file=$(extract_mdu_file "$dir" "$dimr_file")
    local job_name=$(basename "$dir")
    
    # Get platform configuration
    get_platform_config "$platform"
    
    # Perform template substitutions
    substitute_all_templates "$dir/$submit_script_name" "$dimr_file" "$mdu_file_folder" "$mdu_file" "$job_name" "$user_nodes" "$user_tasks_per_node"
    
    echo "    Branded $dir/$submit_script_name using $TEMPLATE_SUBMIT_SCRIPT"
    return 0
}

prepare_all_directories() {
    local platform="$1"
    local submit_script_name="submit_$platform.sh"
    
    local dirs=$(find_dimr_directories)
    local job_ids=()
    
    for dir in $dirs; do
        echo "Found directory with dimr file: $dir, try to prepare for submitting job"
        # Prepare directory first
        if ! prepare_directory "$dir" "$platform" "$submit_script_name"; then
            echo "    Failed to prepare directory $dir" >&2
            continue
        fi
    done
}

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

print_help_prepare() {
    echo "Usage: $0 [h7|delftblue|snellius] [--config CONFIG_FILE]"
    echo "  h7         : Run for H7"
    echo "  delftblue  : Run for DelftBlue"
    echo "  snellius   : Run for Snellius"
    echo "  --config CONFIG_FILE : Optional JSON configuration file specifying nodes and tasks for models"
    echo "                         If not provided or model not found in config, interactive mode will be used"
}

main() {
    local platform="$1"
    
    # Validate and handle help first
    validate_and_handle_help_prepare "$platform"
    
    # Parse arguments (no --run_dependent option available)
    parse_prepare_arguments "$@"
    
    echo "Running for platform: $platform"
    
    # Parse configuration file if provided
    if [ -n "$CONFIG_FILE" ]; then
        echo "Loading configuration from: $CONFIG_FILE"
        if parse_json_config "$CONFIG_FILE"; then
            echo "Configuration loaded successfully"
        else
            echo "Warning: Failed to parse configuration file, falling back to interactive mode"
            CONFIG_FILE=""  # Clear config file to ensure interactive mode
        fi
    else
        echo "No configuration file provided, using interactive mode for all models"
    fi
    
    prepare_all_directories "$platform"

    echo "All jobs prepared successfully"
}

# Call main function with all arguments
main "$@"