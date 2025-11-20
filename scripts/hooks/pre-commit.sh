#!/bin/bash

DVC_BIN="/workspaces/Delft3D/test/deltares_testbench/.venv/bin/dvc"

# Auto-update .dvc files for modified directories
for dvc_file in $(git ls-files '*.dvc'); do
    tracked_dir="${dvc_file%.dvc}"
    
    if [ -d "$tracked_dir" ]; then
        if $DVC_BIN status "$dvc_file" 2>/dev/null | grep -q "modified:"; then
            echo "Auto-updating DVC tracking for '$tracked_dir'..."
            $DVC_BIN add "$tracked_dir"
            git add "$dvc_file"
        fi
    fi
done
