# Install git hooks from scripts/hooks/ to .git/hooks/

$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$HooksDir = Join-Path $ScriptDir "hooks"
$GitHooksDir = Join-Path $ScriptDir ".." ".git" "hooks"

if (-not (Test-Path $GitHooksDir)) {
    Write-Error ".git/hooks directory not found"
    exit 1
}

# Install pre-commit hook
$PreCommitSrc = Join-Path $HooksDir "pre-commit"
$PreCommitDst = Join-Path $GitHooksDir "pre-commit"

if (Test-Path $PreCommitSrc) {
    Copy-Item $PreCommitSrc $PreCommitDst -Force
    Write-Host "Installed pre-commit hook"
} else {
    Write-Warning "pre-commit hook not found in $HooksDir"
}

Write-Host "Git hooks installation complete"
