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

Write-Host "[INFO] Pulling engine root doc.dvc + ALL doc.dvc under any fNNN folder (fxxx + non-fNNN skipped)..."

# === DETECTION PHASE ===
Write-Host "[DETECTION START] Looking for doc.dvc files..."
$allDocDvc = Get-ChildItem -Recurse -Filter "doc.dvc" | Where-Object {
    $fullName = $_.FullName

    $isRootDoc = $fullName -eq (Join-Path $BASE_PATH "doc.dvc")

    # 2. Anything under an fNNN folder
    $segments = $fullName -split '[\\\/]'
    $hasFNNN = $segments | Where-Object { $_ -match '^f\d' }

    $isRootDoc -or $hasFNNN
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

Write-Host "[DETECTION END] Total processed: $totalDetected (root doc.dvc + fNNN)"

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