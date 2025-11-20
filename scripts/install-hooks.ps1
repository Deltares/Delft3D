# Install git hook wrappers that call scripts from hooks/ directory
# This allows easy updates to hook logic without reinstalling

$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$RepoRoot = Join-Path $ScriptDir ".."
$HooksSrcDir = Join-Path $RepoRoot "hooks"
$GitHooksDir = Join-Path $RepoRoot ".git" "hooks"

if (-not (Test-Path $GitHooksDir)) {
    Write-Error ".git/hooks directory not found"
    exit 1
}

Write-Host "Installing git hook wrappers..."

# Install pre-commit wrapper
$PreCommitWrapper = @'
#!/bin/bash
# Git hook wrapper - calls the actual hook script from hooks/ directory

REPO_ROOT="$(git rev-parse --show-toplevel)"
HOOK_SCRIPT="$REPO_ROOT/hooks/pre-commit.sh"

if [ -f "$HOOK_SCRIPT" ]; then
    exec "$HOOK_SCRIPT" "$@"
else
    echo "Warning: Hook script not found: $HOOK_SCRIPT"
    exit 0
fi
'@

$PreCommitDst = Join-Path $GitHooksDir "pre-commit"
Set-Content -Path $PreCommitDst -Value $PreCommitWrapper -NoNewline
Write-Host "✓ Installed pre-commit hook wrapper"

Write-Host ""
Write-Host "Git hooks installation complete!"
Write-Host "Hook scripts in hooks/ can now be updated without reinstalling."
