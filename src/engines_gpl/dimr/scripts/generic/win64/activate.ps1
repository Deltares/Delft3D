# This script is used to activate a specific DIMRset by loading the bin and lib folder into PATH

# Ensure script is sourced
if ($MyInvocation.InvocationName -ne '.' -and $MyInvocation.InvocationName -ne 'source') {
    Write-Host "This script must be sourced to work properly. Usage: . $MyInvocation.InvocationName [--deactivate]"
    return
}

function _activator {
    param ([string]$Option)

    # Get the directory of this script (bin directory)
    try {
        $SCRIPT_DIR = Split-Path -Parent $MyInvocation.ScriptName -ErrorAction Stop
        $SCRIPT_DIR = (Resolve-Path $SCRIPT_DIR).Path
    }
    catch {
        Write-Host "Error: Could not determine script directory"
        return 1
    }

    # Get the parent directory (DIMRset root)
    $PARENT_DIR = Split-Path -Parent $SCRIPT_DIR

    # Get the lib directory
    $LIB_DIR = Join-Path $PARENT_DIR 'lib'

    # Validate required directories exist
    if (-not (Test-Path $SCRIPT_DIR) -or -not (Test-Path $LIB_DIR)) {
        Write-Host "Error: Required directories (bin or lib) not found in $PARENT_DIR"
        return 1
    }

    # Check for deactivation parameter
    if ($Option -eq '--deactivate') {
        if ($env:ACTIVE_DIMRSET_DIR) {
            Write-Host "Deactivating DIMRset from $env:ACTIVE_DIMRSET_DIR..."

            $ACTIVE_BIN_DIR = Join-Path $env:ACTIVE_DIMRSET_DIR 'bin'
            $ACTIVE_LIB_DIR = Join-Path $env:ACTIVE_DIMRSET_DIR 'lib'

            # Remove old paths from PATH
            $env:PATH = ($env:PATH -split ';' | Where-Object { $_ -ne $ACTIVE_BIN_DIR -and $_ -ne $ACTIVE_LIB_DIR }) -join ';'

            # Remove the deactivation function
            Remove-Item -Path function:deactivate_dimrset -ErrorAction SilentlyContinue

            # Unset tracking variable
            Remove-Item -Path Env:ACTIVE_DIMRSET_DIR -ErrorAction SilentlyContinue

            Write-Host "  - Removed $ACTIVE_BIN_DIR from PATH"
            Write-Host "  - Removed $ACTIVE_LIB_DIR from PATH"
            Write-Host "DIMRset deactivated."
        }
        else {
            Write-Host "No DIMRset is currently active."
        }
        return 0
    }

    # Check if a different DIMRset is active
    if ($env:ACTIVE_DIMRSET_DIR -and $env:ACTIVE_DIMRSET_DIR -ne $PARENT_DIR) {
        Write-Host "Unloading previous DIMRset from $env:ACTIVE_DIMRSET_DIR..."

        $ACTIVE_BIN_DIR = Join-Path $env:ACTIVE_DIMRSET_DIR 'bin'
        $ACTIVE_LIB_DIR = Join-Path $env:ACTIVE_DIMRSET_DIR 'lib'

        # Remove old paths from PATH
        $env:PATH = ($env:PATH -split ';' | Where-Object { $_ -ne $ACTIVE_BIN_DIR -and $_ -ne $ACTIVE_LIB_DIR }) -join ';'

        # Remove the old deactivation function
        Remove-Item -Path function:deactivate_dimrset -ErrorAction SilentlyContinue

        # Unset old tracking variable
        Remove-Item -Path Env:ACTIVE_DIMRSET_DIR -ErrorAction SilentlyContinue

        Write-Host "  - Removed $ACTIVE_BIN_DIR from PATH"
        Write-Host "  - Removed $ACTIVE_LIB_DIR from PATH"
        Write-Host "Previous DIMRset unloaded."
    }
    elseif ($env:ACTIVE_DIMRSET_DIR -and $env:ACTIVE_DIMRSET_DIR -eq $PARENT_DIR) {
        Write-Host "This DIMRset is already active."
        Write-Host "  - $SCRIPT_DIR in PATH"
        Write-Host "  - $LIB_DIR in PATH"
        return 0
    }

    # Activate the new DIMRset
    Write-Host "Activating DIMRset at $PARENT_DIR..."

    # Add bin and lib directories to PATH (prepend)
    $env:PATH = "$SCRIPT_DIR;$LIB_DIR;$env:PATH"

    # Set tracking variable
    $env:ACTIVE_DIMRSET_DIR = $PARENT_DIR

    # Define deactivation function
    function deactivate_dimrset {
        . "$SCRIPT_DIR\activate.ps1" --deactivate
    }

    Write-Host "  - Added $SCRIPT_DIR to PATH"
    Write-Host "  - Added $LIB_DIR to PATH"
    Write-Host "  - Added 'deactivate_dimrset' function for deactivation"
}

# Execute the activator function with any provided arguments
_activator @args