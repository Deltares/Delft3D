#!/bin/bash

# Install git hooks from scripts/hooks/ to .git/hooks/

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
HOOKS_DIR="$SCRIPT_DIR/hooks"
GIT_HOOKS_DIR="$SCRIPT_DIR/../.git/hooks"

if [ ! -d "$GIT_HOOKS_DIR" ]; then
    echo "Error: .git/hooks directory not found"
    exit 1
fi

# Install pre-commit hook
if [ -f "$HOOKS_DIR/pre-commit.sh" ]; then
    cp "$HOOKS_DIR/pre-commit.sh" "$GIT_HOOKS_DIR/pre-commit"
    chmod +x "$GIT_HOOKS_DIR/pre-commit"
    echo "Installed pre-commit hook"
else
    echo "Warning: pre-commit hook not found in $HOOKS_DIR"
fi

echo "Git hooks installation complete"
