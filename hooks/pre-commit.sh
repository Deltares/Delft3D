#!/bin/bash

# Restore stdin for interactive prompts in git hooks
exec < /dev/tty || exec < /proc/$$/fd/0 || true

# Check modified .dvc files and prompt user before updating
for dvc_file in $(git ls-files '*.dvc'); do
    tracked_dir="${dvc_file%.dvc}"

    if [ -d "$tracked_dir" ]; then
        if dvc status "$dvc_file" 2>/dev/null | grep -q "modified:"; then
            echo "Detected changes in '$tracked_dir' (DVC file: $dvc_file)"
            echo "Showing DVC diff:"
            dvc diff HEAD --targets "$tracked_dir" || echo "No detailed diff available."
            echo
            echo -n "Do you want to update DVC tracking for '$tracked_dir'? [y/N]: "
            read -r confirm
            if [[ "$confirm" =~ ^[Yy]$ ]]; then
                echo "Updating DVC tracking for '$tracked_dir'..."
                dvc add "$tracked_dir" --verbose
                if [ $? -eq 0 ]; then
                    git add "$dvc_file"
                    echo "Successfully updated DVC tracking for '$tracked_dir'"
                else
                    echo "Error: Failed to update DVC tracking for '$tracked_dir'"
                    exit 1
                fi
            else
                echo "Skipped updating '$tracked_dir'."
            fi
            echo
        fi
    fi
done
