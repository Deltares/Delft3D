#!/bin/bash

# Install git hook wrappers that call scripts from hooks/ directory
# This allows easy updates to hook logic without reinstalling

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$SCRIPT_DIR/.."
HOOKS_SRC_DIR="$REPO_ROOT/hooks"
GIT_HOOKS_DIR="$REPO_ROOT/.git/hooks"

if [ ! -d "$GIT_HOOKS_DIR" ]; then
    echo "Error: .git/hooks directory not found"
    exit 1
fi

echo "Installing git hook wrappers..."

# Install pre-commit wrapper
cat > "$GIT_HOOKS_DIR/pre-commit" <<'EOF'
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
EOF
chmod +x "$GIT_HOOKS_DIR/pre-commit"
echo "✓ Installed pre-commit hook wrapper"

echo ""
echo "Git hooks installation complete!"
echo "Hook scripts in hooks/ can now be updated without reinstalling."
