#!/usr/bin/env bash

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
    PARENT_DIR="$(dirname "$BIN_DIR")"

    # Get the lib directory
    LIB_DIR="$PARENT_DIR/lib"

    # Get the PROC_DEF_DIR
    PROC_DEF_DIR="$PARENT_DIR/share/delft3d"

    # Validate required directories exist
    if [ ! -d "$BIN_DIR" ] || [ ! -d "$LIB_DIR" ]; then
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

    # Check for deactivation parameter
    if [ "$1" == "--deactivate" ]; then
        if [ -n "$ACTIVE_DIMRSET_DIR" ]; then
            echo "Deactivating DIMRset from $ACTIVE_DIMRSET_DIR..."

            ACTIVE_BIN_DIR="$ACTIVE_DIMRSET_DIR/bin"
            ACTIVE_LIB_DIR="$ACTIVE_DIMRSET_DIR/lib"
            ACTIVE_PROC_DEF_DIR="$ACTIVE_DIMRSET_DIR/share/delft3d"

            # Remove old bin from PATH
            remove_from_path "PATH" "$ACTIVE_BIN_DIR"

            # Remove old lib from LD_LIBRARY_PATH
            remove_from_path "LD_LIBRARY_PATH" "$ACTIVE_LIB_DIR"

            # Unset the tracking variable and alias
            unset ACTIVE_DIMRSET_DIR
            unset PROC_DEF_DIR
            unset -f deactivate_dimrset 2>/dev/null

            echo "  - Removed $ACTIVE_BIN_DIR from PATH"
            echo "  - Removed $ACTIVE_LIB_DIR from LD_LIBRARY_PATH"
            echo "  - Removed $ACTIVE_PROC_DEF_DIR as PROC_DEF_DIR"
            echo "DIMRset deactivated."
        else
            echo "No DIMRset is currently active."
        fi
        return 0
    fi

    # Check if a different DIMRset is already active
    if [ -n "$ACTIVE_DIMRSET_DIR" ] && [ "$ACTIVE_DIMRSET_DIR" != "$PARENT_DIR" ]; then
        echo "Unloading previous DIMRset from $ACTIVE_DIMRSET_DIR..."

        ACTIVE_BIN_DIR="$ACTIVE_DIMRSET_DIR/bin"
        ACTIVE_LIB_DIR="$ACTIVE_DIMRSET_DIR/lib"
        ACTIVE_PROC_DEF_DIR="$ACTIVE_DIMRSET_DIR/share/delft3d"

        # Remove old bin from PATH
        remove_from_path "PATH" "$ACTIVE_BIN_DIR"

        # Remove old lib from LD_LIBRARY_PATH
        remove_from_path "LD_LIBRARY_PATH" "$ACTIVE_LIB_DIR"

        # Unset the old tracking variable and alias
        unset ACTIVE_DIMRSET_DIR
        unset PROC_DEF_DIR
        unset -f deactivate_dimrset 2>/dev/null

        echo "  - Removed $ACTIVE_BIN_DIR from PATH"
        echo "  - Removed $ACTIVE_LIB_DIR from LD_LIBRARY_PATH"
        echo "  - Removed $ACTIVE_PROC_DEF_DIR as PROC_DEF_DIR"
        echo "Previous DIMRset unloaded."
    elif [ -n "$ACTIVE_DIMRSET_DIR" ] && [ "$ACTIVE_DIMRSET_DIR" == "$PARENT_DIR" ]; then
        echo "This DIMRset is already active."
        echo "  - $BIN_DIR in PATH"
        echo "  - $LIB_DIR in LD_LIBRARY_PATH"
        return 0
    fi

    # Activate the new DIMRset
    echo "Activating DIMRset at $PARENT_DIR..."

    # Add the bin directory to PATH (prepend)
    export PATH="$BIN_DIR:$PATH"

    # Add the lib directory to LD_LIBRARY_PATH (prepend)
    export LD_LIBRARY_PATH="$LIB_DIR:$LD_LIBRARY_PATH"

    # Set tracking variable
    export ACTIVE_DIMRSET_DIR="$PARENT_DIR"

    # Add deactivation alias
    alias deactivate_dimrset="source \"$BIN_DIR/activate.sh\" --deactivate"

    echo "  - Added $BIN_DIR to PATH"
    echo "  - Added $LIB_DIR to LD_LIBRARY_PATH"
    echo "  - Set $PROC_DEF_DIR as PROC_DEF_DIR"
    echo "  - Run 'deactivate_dimrset' alias for deactivation"
}

# Execute the activator function with any provided arguments
_activator "$@"