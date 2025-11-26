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
            dir_size_bytes=$(du -sb "$tracked_dir" 2>/dev/null | cut -f1)
            dir_size=$(du -sh "$tracked_dir" 2>/dev/null | cut -f1)
            
            # Get cached file count and size from .dvc file
            cached_nfiles=$(grep 'nfiles:' "$dvc_file" 2>/dev/null | sed 's/.*nfiles: *\([0-9]*\).*/\1/')
            cached_size=$(grep 'size:' "$dvc_file" 2>/dev/null | sed 's/.*size: *\([0-9]*\).*/\1/')
            
            # Calculate diffs
            if [ -n "$cached_nfiles" ] && [ -n "$cached_size" ]; then
                file_diff=$((file_count - cached_nfiles))
                size_diff=$((dir_size_bytes - cached_size))
                
                # Format file diff
                if [ "$file_diff" -gt 0 ]; then
                    file_diff_str=" (+$file_diff)"
                elif [ "$file_diff" -lt 0 ]; then
                    file_diff_str=" ($file_diff)"
                else
                    file_diff_str=""
                fi
                
                # Format size diff
                if [ "$size_diff" -gt 0 ]; then
                    size_diff_human=$(numfmt --to=iec-i --suffix=B "$size_diff" 2>/dev/null || echo "$size_diff bytes")
                    size_diff_str=" (+$size_diff_human)"
                elif [ "$size_diff" -lt 0 ]; then
                    size_diff_human=$(numfmt --to=iec-i --suffix=B "$((size_diff * -1))" 2>/dev/null || echo "$((size_diff * -1)) bytes")
                    size_diff_str=" (-$size_diff_human)"
                else
                    size_diff_str=""
                fi
                
                echo "  Total files: $file_count$file_diff_str (was: $cached_nfiles)"
                echo "  Directory size: $dir_size$size_diff_str (was: $(numfmt --to=iec-i --suffix=B "$cached_size" 2>/dev/null || echo "$cached_size bytes"))"
            else
                echo "  Total files: $file_count"
                echo "  Directory size: $dir_size"
            fi
            echo
            
            # Show added/deleted/modified files by comparing with DVC cache
            echo "File changes:"
            
            # Get the cached MD5 hash from the .dvc file
            cached_md5=$(grep 'md5:' "$dvc_file" | head -1 | sed 's/.*md5: *\([^ ]*\).*/\1/')
            cache_dir_file=".dvc/cache/files/md5/${cached_md5:0:2}/${cached_md5:2}"
            
            if [ -f "$cache_dir_file" ]; then
                # Create temp files for comparison
                tmp_cached=$(mktemp)
                tmp_current=$(mktemp)
                
                # Extract file list from cache
                python3 -c "import json; data=json.load(open('$cache_dir_file')); print('\n'.join(sorted([f['relpath'] for f in data])))" > "$tmp_cached" 2>/dev/null
                
                # Get current file list
                (cd "$tracked_dir" && find . -type f -printf '%P\n' | sort) > "$tmp_current"
                
                # Show deleted files
                deleted=$(comm -23 "$tmp_cached" "$tmp_current")
                if [ -n "$deleted" ]; then
                    echo "  Deleted files:"
                    echo "$deleted" | head -20 | sed 's/^/    - /'
                    deleted_count=$(echo "$deleted" | wc -l)
                    if [ "$deleted_count" -gt 20 ]; then
                        echo "    ... and $((deleted_count - 20)) more"
                    fi
                fi
                
                # Show added files
                added=$(comm -13 "$tmp_cached" "$tmp_current")
                if [ -n "$added" ]; then
                    echo "  Added files:"
                    echo "$added" | head -20 | sed 's/^/    + /'
                    added_count=$(echo "$added" | wc -l)
                    if [ "$added_count" -gt 20 ]; then
                        echo "    ... and $((added_count - 20)) more"
                    fi
                fi
                
                # Show modified files (files in both but with different checksums)
                common_files=$(comm -12 "$tmp_cached" "$tmp_current")
                if [ -n "$common_files" ]; then
                    modified_files=""
                    while IFS= read -r file; do
                        # Get cached MD5 for this file
                        cached_file_md5=$(python3 -c "import json; data=json.load(open('$cache_dir_file')); print([f['md5'] for f in data if f['relpath']=='$file'][0])" 2>/dev/null)
                        # Get current MD5
                        current_file_md5=$(md5sum "$tracked_dir/$file" 2>/dev/null | cut -d' ' -f1)
                        
                        if [ "$cached_file_md5" != "$current_file_md5" ] && [ -n "$cached_file_md5" ] && [ -n "$current_file_md5" ]; then
                            modified_files="${modified_files}${file}\n"
                        fi
                    done <<< "$common_files"
                    
                    if [ -n "$modified_files" ]; then
                        echo "  Modified files:"
                        echo -e "$modified_files" | grep -v '^$' | head -20 | sed 's/^/    M /'
                        modified_count=$(echo -e "$modified_files" | grep -v '^$' | wc -l)
                        if [ "$modified_count" -gt 20 ]; then
                            echo "    ... and $((modified_count - 20)) more"
                        fi
                    fi
                fi
                
                rm -f "$tmp_cached" "$tmp_current"
            else
                echo "  (unable to determine file-level changes - cache not found)"
            fi
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
