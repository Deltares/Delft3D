param(
    [string]$EngineDir = $env:engine_dir
)

$REPO_ROOT = (Get-Location).Path
$BASE_PATH = Join-Path $REPO_ROOT "test\deltares_testbench\data\cases\$EngineDir"

Write-Host "=== DVC doc pull started for engine_dir: $EngineDir ==="

if (-not (Test-Path $BASE_PATH)) {
    Write-Host "[ERROR] Base path not found: $BASE_PATH"
    "##teamcity[buildProblem description='DVC base path not found: $BASE_PATH' identity='dvc_base_path_missing']"
    exit 1
}

Push-Location $BASE_PATH

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
$featureDocs = Get-ChildItem -Recurse -Filter "doc.dvc" | Where-Object {
    $fullName = $_.FullName
    if ($fullName -match 'doc\\doc\.dvc$') { return $false }
    $segments = $fullName -split '[\\\/]'
    $segments | Where-Object { $_ -match '^f\d' }
}
$allDocDvc += $featureDocs

$totalDetected = $allDocDvc.Count
$batch = @()
$batchCount = 0

foreach ($file in $allDocDvc) {
    Write-Host "[INCLUDED] $($file.FullName)"
    $batch += "`"$($file.FullName)`""

    if ($batch.Count -eq 100) {
        $batchCount++
        Write-Host "[BATCH $batchCount] Pulling next 100..."
        & dvc pull $batch
        if ($LASTEXITCODE -ne 0) { 
            Write-Host "[ERROR] Failed to pull batch $batchCount"
            "##teamcity[buildProblem description='DVC pull failed: batch $batchCount ($EngineDir)' identity='dvc_pull_batch_$batchCount']"
            exit 1
        }
        Write-Host "[PULL OK] Batch $batchCount completed"
        $batch = @()
    }
}

if ($batch.Count -gt 0) {
    $batchCount++
    Write-Host "[BATCH $batchCount] Pulling remaining files..."
    & dvc pull $batch
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

Pop-Location
Write-Host "=== DVC doc pull completed ==="

if ($missing -gt 0) {
    "##teamcity[buildProblem description='$missing doc folders failed to materialize ($EngineDir)' identity='dvc_missing_folders']"
    exit 1
}