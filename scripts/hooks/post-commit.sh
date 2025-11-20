#!/bin/bash
# Post-commit hook script for DVC tracking
# This is the actual logic that can be debugged

DVC_BIN="/workspaces/Delft3D/test/deltares_testbench/.venv/bin/dvc"

echo ""
echo "=== DVC Tracked Changes (last commit) ==="
$DVC_BIN diff HEAD~1 HEAD 2>/dev/null || echo "No DVC changes detected"

exit 0
