#!/bin/bash

# Restore stdin for interactive prompts in git hooks
exec < /dev/tty || exec < /proc/$$/fd/0 || true

# Check modified .dvc files and prompt user before updating
for dvc_file in $(git ls-files '*.dvc'); do
    tracked_dir="${dvc_file%.dvc}"

    if [ -d "$tracked_dir" ]; then
        # Check if there are local changes not yet tracked by DVC
        dvc_status_output=$(dvc status "$dvc_file" 2>/dev/null)
        if echo "$dvc_status_output" | grep -q "modified:"; then
            echo "Detected changes in '$tracked_dir' (DVC file: $dvc_file)"
            echo "$dvc_status_output"
            echo
            
            # Show summary of directory contents
            echo "Directory summary:"
            file_count=$(find "$tracked_dir" -type f 2>/dev/null | wc -l)
            dir_size=$(du -sh "$tracked_dir" 2>/dev/null | cut -f1)
            echo "  Total files: $file_count"
            echo "  Directory size: $dir_size"
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
