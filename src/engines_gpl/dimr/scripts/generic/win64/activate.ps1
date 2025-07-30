# This script activates a specific DIMRset to use by setting system paths.

# Check if the script is being run directly instead of being dot-sourced
if ($MyInvocation.InvocationName -ne '.' -and $MyInvocation.InvocationName -ne 'source') {
    Write-Error "This script must be dot-sourced to work properly. Usage: . $($MyInvocation.MyCommand.Path) [--deactivate]"
    return
}

function Invoke-DIMRsetActivator {
    param (
        [Parameter(Mandatory = $false)]
        [string]$Deactivate
    )

    # Get the directory of this script (bin directory)
    try {
        $SCRIPT_DIR = Split-Path -Parent $MyInvocation.MyCommand.Path
    }
    catch {
        Write-Error "Could not determine script directory"
        return 1
    }

    # Get the parent directory (DIMRset root)
    $PARENT_DIR = Split-Path -Parent $SCRIPT_DIR

    # Get the lib directory
    $LIB_DIR = Join-Path $PARENT_DIR "lib"

    # Get the PROC_DEF_DIR
    $PROC_DEF_DIR = Join-Path $PARENT_DIR "share\delft3d"

    # Validate required directories exist
    if (-not (Test-Path $SCRIPT_DIR) -or -not (Test-Path $LIB_DIR)) {
        Write-Error "Required directories (bin or lib) not found in $PARENT_DIR"
        return 1
    }

    # Function to remove a directory from PATH
    function Remove-FromPath {
        param (
            [string]$VarName,
            [string]$DirToRemove
        )
        $currentValue = [Environment]::GetEnvironmentVariable($VarName, "Process")
        if ($currentValue) {
            # Split the path, remove the directory, and rejoin
            $newValue = ($currentValue -split ';' | Where-Object { $_ -ne $DirToRemove -and $_ -ne "" }) -join ';'
            [Environment]::SetEnvironmentVariable($VarName, $newValue, "Process")
        }
    }

    # Helper function to deactivate a DIMRset
    function Deactivate-DIMRset {
        param (
            [string]$ActiveDir,
            [string]$MessagePrefix
        )
        Write-Host "$MessagePrefix from $ActiveDir..."

        $ACTIVE_BIN_DIR = Join-Path $ActiveDir "bin"
        $ACTIVE_LIB_DIR = Join-Path $ActiveDir "lib"
        $ACTIVE_PROC_DEF_DIR = Join-Path $ActiveDir "share\delft3d"

        # Remove old bin and lib from PATH
        Remove-FromPath -VarName "PATH" -DirToRemove $ACTIVE_BIN_DIR
        Remove-FromPath -VarName "PATH" -DirToRemove $ACTIVE_LIB_DIR

        # Unset tracking variables and function
        Remove-Item -Path Env:ACTIVE_DIMRSET_DIR -ErrorAction SilentlyContinue
        Remove-Item -Path Env:PROC_DEF_DIR -ErrorAction SilentlyContinue
        Remove-Item -Path Function:Deactivate-DIMRset -ErrorAction SilentlyContinue

        Write-Host "  - Removed $ACTIVE_BIN_DIR from PATH"
        Write-Host "  - Removed $ACTIVE_LIB_DIR from PATH"
        Write-Host "  - Removed $ACTIVE_PROC_DEF_DIR as PROC_DEF_DIR"
        Write-Host "$MessagePrefix completed."
    }

    # Helper function to activate a DIMRset
    function Activate-DIMRset {
        param (
            [string]$ParentDir
        )
        $scriptDir = Join-Path $ParentDir "bin"
        $libDir = Join-Path $ParentDir "lib"
        $procDefDir = Join-Path $ParentDir "share\delft3d"
        Write-Host "Activating DIMRset at $ParentDir..."

        # Add the bin and lib directories to PATH (prepend)
        $currentPath = [Environment]::GetEnvironmentVariable("PATH", "Process")
        $newPath = "$scriptDir;$libDir;$currentPath"
        [Environment]::SetEnvironmentVariable("PATH", $newPath, "Process")

        # Set tracking variables
        [Environment]::SetEnvironmentVariable("ACTIVE_DIMRSET_DIR", $ParentDir, "Process")
        [Environment]::SetEnvironmentVariable("PROC_DEF_DIR", $procDefDir, "Process")

        # Add deactivation function
        New-Item -Path Function: -Name Deactivate-DIMRset -Value {
            . $SCRIPT_DIR\activate.ps1 --deactivate
        } -Force | Out-Null

        Write-Host "  - Added $scriptDir to PATH"
        Write-Host "  - Added $libDir to PATH"
        Write-Host "  - Set $procDefDir as PROC_DEF_DIR"
        Write-Host "  - Added 'Deactivate-DIMRset' function for deactivation"
    }

    # Check for deactivation parameter
    if ($Deactivate -eq "--deactivate") {
        if ($env:ACTIVE_DIMRSET_DIR) {
            Deactivate-DIMRset -ActiveDir $env:ACTIVE_DIMRSET_DIR -MessagePrefix "Deactivating DIMRset"
        }
        else {
            Write-Host "No DIMRset is currently active."
        }
        return
    }

    # Check if a different DIMRset is already active
    if ($env:ACTIVE_DIMRSET_DIR -and $env:ACTIVE_DIMRSET_DIR -ne $PARENT_DIR) {
        Deactivate-DIMRset -ActiveDir $env:ACTIVE_DIMRSET_DIR -MessagePrefix "Unloading previous DIMRset"
    }
    elseif ($env:ACTIVE_DIMRSET_DIR -and $env:ACTIVE_DIMRSET_DIR -eq $PARENT_DIR) {
        Write-Host "This DIMRset is already active."
        Write-Host "  - $SCRIPT_DIR in PATH"
        Write-Host "  - $LIB_DIR in PATH"
        Write-Host "  - $PROC_DEF_DIR as PROC_DEF_DIR"
        return
    }

    # Activate the new DIMRset
    Activate-DIMRset -ParentDir $PARENT_DIR
}

# Execute the activator function with any provided arguments
Invoke-DIMRsetActivator @args