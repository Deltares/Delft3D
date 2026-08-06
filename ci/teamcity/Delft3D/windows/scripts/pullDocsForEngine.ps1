# Legacy PowerShell entrypoint. TeamCity documentation builds now use
# pull_docs_for_engine.py via a Python venv step (build-side dvc install).
# This script keeps the same behaviour for local/manual runs: install dvc into
# a local venv if needed, then pull.

param(
    [string]$EngineDir = $env:engine_dir
)

$ErrorActionPreference = "Stop"

$REPO_ROOT = (Get-Location).Path
$BASE_PATH = Join-Path $REPO_ROOT "test\deltares_testbench\data\cases\$EngineDir"

Write-Host "=== DVC doc pull started for engine_dir: $EngineDir ==="

if (-not (Test-Path $BASE_PATH)) {
    Write-Host "[ERROR] Base path not found: $BASE_PATH"
    "##teamcity[buildProblem description='DVC base path not found: $BASE_PATH' identity='dvc_base_path_missing']"
    exit 1
}

function Get-PythonCommand {
    foreach ($candidate in @("python", "py", "python3")) {
        $cmd = Get-Command $candidate -ErrorAction SilentlyContinue
        if ($cmd) {
            if ($candidate -eq "py") {
                return @("py", "-3")
            }
            return @($cmd.Source)
        }
    }
    return $null
}

function Install-DvcInVenv {
    $venvDir = Join-Path $REPO_ROOT ".venv-dvc-docs"
    $dvcExe = Join-Path $venvDir "Scripts\dvc.exe"
    if (Test-Path $dvcExe) {
        Write-Host "[INFO] Reusing DVC venv: $dvcExe"
        return $dvcExe
    }

    $pythonCmd = Get-PythonCommand
    if (-not $pythonCmd) {
        Write-Host "[ERROR] No Python found on PATH; cannot install dvc"
        "##teamcity[buildProblem description='Python not found for build-side dvc install' identity='dvc_python_missing']"
        exit 1
    }

    Write-Host "[INFO] Creating venv at $venvDir with: $($pythonCmd -join ' ')"
    if ($pythonCmd.Count -gt 1) {
        & $pythonCmd[0] $pythonCmd[1..($pythonCmd.Length - 1)] -m venv $venvDir
    } else {
        & $pythonCmd[0] -m venv $venvDir
    }
    if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }

    $venvPython = Join-Path $venvDir "Scripts\python.exe"
    $reqFile = Join-Path $REPO_ROOT "ci\teamcity\Delft3D\windows\scripts\dvc-docs-requirements.txt"

    Write-Host "[INFO] Installing dvc from $reqFile"
    & $venvPython -m pip install --upgrade pip
    if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
    & $venvPython -m pip install -r $reqFile
    if ($LASTEXITCODE -ne 0) {
        "##teamcity[buildProblem description='pip install dvc failed' identity='dvc_pip_install_failed']"
        exit $LASTEXITCODE
    }

    if (-not (Test-Path $dvcExe)) {
        Write-Host "[ERROR] dvc.exe not found after install: $dvcExe"
        "##teamcity[buildProblem description='dvc.exe missing after pip install' identity='dvc_exe_missing']"
        exit 1
    }

    Write-Host "[INFO] Installed $(& $dvcExe --version)"
    return $dvcExe
}

$env:AWS_EC2_METADATA_DISABLED = "true"
$DvcExe = Install-DvcInVenv

Write-Host "[INFO] Pulling root doc.dvc (brings /doc/functionalities/) + all under fNNN folders..."

$allDocDvc = @()

# 1. Root doc.dvc (critical for functionalities/)
$rootDocDvc = Join-Path $BASE_PATH "doc.dvc"
if (Test-Path $rootDocDvc) {
    $allDocDvc += Get-Item $rootDocDvc
    Write-Host "[ROOT] doc.dvc included"
} else {
    Write-Host "[WARNING] Root doc.dvc not found on disk"
}

# 2. All doc.dvc under fNNN folders
$featureDocs = Get-ChildItem -Path $BASE_PATH -Recurse -Filter "doc.dvc" -ErrorAction SilentlyContinue | Where-Object {
    $fullName = $_.FullName
    if ($fullName -match 'doc\\doc\.dvc$') { return $false }
    $segments = $fullName -split '[\\\/]'
    $segments | Where-Object { $_ -match '^f\d' }
}
$allDocDvc += $featureDocs

$totalDetected = $allDocDvc.Count
$batch = [System.Collections.Generic.List[string]]::new()
$batchCount = 0

foreach ($file in $allDocDvc) {
    Write-Host "[INCLUDED] $($file.FullName)"
    $batch.Add($file.FullName) | Out-Null

    if ($batch.Count -eq 100) {
        $batchCount++
        Write-Host "[BATCH $batchCount] Pulling next 100..."
        & $DvcExe pull @($batch.ToArray())
        if ($LASTEXITCODE -ne 0) {
            Write-Host "[ERROR] Failed to pull batch $batchCount"
            "##teamcity[buildProblem description='DVC pull failed: batch $batchCount ($EngineDir)' identity='dvc_pull_batch_$batchCount']"
            exit 1
        }
        Write-Host "[PULL OK] Batch $batchCount completed"
        $batch.Clear()
    }
}

if ($batch.Count -gt 0) {
    $batchCount++
    Write-Host "[BATCH $batchCount] Pulling remaining files..."
    & $DvcExe pull @($batch.ToArray())
    if ($LASTEXITCODE -ne 0) {
        Write-Host "[ERROR] Failed to pull final batch"
        "##teamcity[buildProblem description='DVC pull failed: final batch ($EngineDir)' identity='dvc_pull_final']"
        exit 1
    }
    Write-Host "[PULL OK] Final batch $batchCount completed"
}

Write-Host "[DETECTION END] Total processed: $totalDetected (root + fNNN)"

# === VERIFICATION PHASE ===
Write-Host "[VERIFICATION START]"
$verified = 0
$missing = 0

foreach ($file in $allDocDvc) {
    $docFolder = Join-Path (Split-Path $file.FullName -Parent) "doc"
    if (Test-Path $docFolder) {
        Write-Host "[VERIFIED] $($file.FullName)"
        $verified++
    } else {
        Write-Host "[MISSING] $($file.FullName)"
        $missing++
    }
}

Write-Host "[VERIFICATION END] Verified: $verified   Missing: $missing"
Write-Host "=== DVC doc pull completed ==="

if ($missing -gt 0) {
    "##teamcity[buildProblem description='$missing doc folders failed to materialize ($EngineDir)' identity='dvc_missing_folders']"
    exit 1
}
