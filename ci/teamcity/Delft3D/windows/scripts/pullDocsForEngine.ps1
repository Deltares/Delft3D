param(
    [string]$EngineDir = $env:engine_dir
)

Write-Host "=== DVC doc pull started for engine_dir: $EngineDir ==="

$BASE_PATH = "test\deltares_testbench\data\cases\$EngineDir"

if (-not (Test-Path $BASE_PATH)) {
    Write-Host "[ERROR] Base path not found: $BASE_PATH"
    "##teamcity[buildProblem description='DVC base path not found: $BASE_PATH' identity='dvc_base_path_missing']"
    exit 1
}

Push-Location $BASE_PATH

Write-Host "[INFO] Pulling doc.dvc files ONLY from f[0-9] directories (skipping fxxx and everything else)..."

# === DETECTION PHASE ===
Write-Host "[DETECTION START] Looking for doc.dvc files..."
$allDocDvc = Get-ChildItem -Recurse -Filter "doc.dvc" | Where-Object {
    $featureDirName = Split-Path (Split-Path $_.FullName -Parent) -Leaf
    $featureDirName -match '^f[0-9]+$' -and $_.FullName -notmatch 'doc\\doc\.dvc'
}

$totalDetected = $allDocDvc.Count
$batch = @()
$batchCount = 0

foreach ($file in $allDocDvc) {
    Write-Host "[INCLUDED] $($file.FullName)"
    $batch += "`"$($file.FullName)`""

    if ($batch.Count -eq 100) {
        $batchCount++
        Write-Host "[BATCH $batchCount] Pulling next 100 doc.dvc files..."
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

Write-Host "[DETECTION END] Total processed: $totalDetected (only f[0-9] folders)"

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