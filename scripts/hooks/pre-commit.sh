#!/bin/bash

DVC_BIN="/workspaces/Delft3D/test/deltares_testbench/.venv/bin/dvc"


# Check modified .dvc files and prompt user before updating
for dvc_file in $(git ls-files '*.dvc'); do
    tracked_dir="${dvc_file%.dvc}"

    if [ -d "$tracked_dir" ]; then
        if $DVC_BIN status "$dvc_file" 2>/dev/null | grep -q "modified:"; then
            echo "Detected changes in '$tracked_dir' (DVC file: $dvc_file)"
            echo "Showing DVC diff:"
            $DVC_BIN diff HEAD --targets "$tracked_dir" || echo "No detailed diff available."
            echo
            read -r -p "Do you want to update DVC tracking for '$tracked_dir'? [y/N]: " confirm
            if [[ "$confirm" =~ ^[Yy]$ ]]; then
                echo "Updating DVC tracking for '$tracked_dir'..."
                $DVC_BIN add "$tracked_dir"
                git add "$dvc_file"
            else
                echo "Skipped updating '$tracked_dir'."
            fi
            echo
        fi
    fi
done
