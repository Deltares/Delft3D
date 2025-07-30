#!/usr/bin/env bash
#
# This script activates a specific DIMRset to use by setting system paths.
#

# Check if the script is sourced
if [ "${BASH_SOURCE[0]}" == "${0}" ]; then
    echo "This script must be sourced to work properly. Usage: source $0 [--deactivate]"
    exit 1
fi

_activator() {
    # Get the directory of this script (bin directory)
    SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )" || {
        echo "Error: Could not determine script directory"
        return 1
    }

    # Get the parent directory (DIMRset root)
    PARENT_DIR="$(dirname "$SCRIPT_DIR")"

    # Get the lib directory
    LIB_DIR="$PARENT_DIR/lib"

    # Get the PROC_DEF_DIR
    PROC_DEF_DIR="$PARENT_DIR/share/delft3d"

    # Validate required directories exist
    if [ ! -d "$SCRIPT_DIR" ] || [ ! -d "$LIB_DIR" ]; then
        echo "Error: Required directories (bin or lib) not found in $PARENT_DIR"
        return 1
    fi

    # Function to remove a directory from a path variable
    remove_from_path() {
        local var_name="$1"
        local dir_to_remove="$2"
        local current_value="${!var_name}"

        # Remove if in the middle
        current_value="${current_value//:$dir_to_remove:/:}"
        # Remove if at the beginning
        current_value="${current_value#"$dir_to_remove:"}"
        # Remove if at the end
        current_value="${current_value%":$dir_to_remove"}"

        export "$var_name"="$current_value"
    }

    # Helper function to deactivate a DIMRset
    _deactivate_dimrset() {
        local active_dir="$1"
        local message_prefix="$2"
        echo "$message_prefix from $active_dir..."

        ACTIVE_BIN_DIR="$active_dir/bin"
        ACTIVE_LIB_DIR="$active_dir/lib"
        ACTIVE_PROC_DEF_DIR="$active_dir/share/delft3d"

        # Remove old bin from PATH
        remove_from_path "PATH" "$ACTIVE_BIN_DIR"

        # Remove old lib from LD_LIBRARY_PATH
        remove_from_path "LD_LIBRARY_PATH" "$ACTIVE_LIB_DIR"

        # Unset tracking variables and alias
        unset ACTIVE_DIMRSET_DIR
        unset PROC_DEF_DIR
        unalias deactivate_dimrset 2>/dev/null

        echo "  - Removed $ACTIVE_BIN_DIR from PATH"
        echo "  - Removed $ACTIVE_LIB_DIR from LD_LIBRARY_PATH"
        echo "  - Removed $ACTIVE_PROC_DEF_DIR as PROC_DEF_DIR"
        echo "$message_prefix completed."
    }

    # Helper function to activate a DIMRset
    _activate_dimrset() {
        local parent_dir="$1"
        local script_dir="$parent_dir/bin"
        local lib_dir="$parent_dir/lib"
        local proc_def_dir="$parent_dir/share/delft3d"
        echo "Activating DIMRset at $parent_dir..."

        # Add the bin directory to PATH (prepend)
        export PATH="$script_dir:$PATH"

        # Add the lib directory to LD_LIBRARY_PATH (prepend)
        export LD_LIBRARY_PATH="$lib_dir${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"

        # Set tracking variables
        export ACTIVE_DIMRSET_DIR="$parent_dir"
        export PROC_DEF_DIR="$proc_def_dir"

        # Add deactivation alias
        alias deactivate_dimrset="source \"$script_dir/activate.sh\" --deactivate"

        echo "  - Added $script_dir to PATH"
        echo "  - Added $lib_dir to LD_LIBRARY_PATH"
        echo "  - Set $proc_def_dir as PROC_DEF_DIR"
        echo "  - Added 'deactivate_dimrset' alias for deactivation"
    }

    # Check for deactivation parameter
    if [ "$1" == "--deactivate" ]; then
        if [ -n "$ACTIVE_DIMRSET_DIR" ]; then
            _deactivate_dimrset "$ACTIVE_DIMRSET_DIR" "Deactivating DIMRset"
        else
            echo "No DIMRset is currently active."
        fi
        return 0
    fi

    # Check if a different DIMRset is already active
    if [ -n "$ACTIVE_DIMRSET_DIR" ] && [ "$ACTIVE_DIMRSET_DIR" != "$PARENT_DIR" ]; then
        _deactivate_dimrset "$ACTIVE_DIMRSET_DIR" "Unloading previous DIMRset"
    elif [ -n "$ACTIVE_DIMRSET_DIR" ] && [ "$ACTIVE_DIMRSET_DIR" == "$PARENT_DIR" ]; then
        echo "This DIMRset is already active."
        echo "  - $SCRIPT_DIR in PATH"
        echo "  - $LIB_DIR in LD_LIBRARY_PATH"
        echo "  - $PROC_DEF_DIR as PROC_DEF_DIR"
        return 0
    fi

    # Activate the new DIMRset
    _activate_dimrset "$PARENT_DIR"
}

# Execute the activator function with any provided arguments
_activator "$@"
