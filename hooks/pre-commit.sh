#!/bin/bash

# Restore stdin for interactive prompts in git hooks
exec < /dev/tty || exec < /proc/$$/fd/0 || true

readonly MAX_FILES_TO_SHOW=20

# Extract value from .dvc file
get_dvc_value() {
    local dvc_file=$1
    local key=$2
    grep "${key}:" "$dvc_file" 2>/dev/null | head -1 | sed "s/.*${key}: *\([^ ]*\).*/\1/"
}

# Format a numeric diff with sign
format_diff() {
    local diff=$1
    if [ "$diff" -gt 0 ]; then
        echo " (+$diff)"
    elif [ "$diff" -lt 0 ]; then
        echo " ($diff)"
    fi
}

# Format size in human-readable format
format_size() {
    local bytes=$1
    numfmt --to=iec-i --suffix=B "$bytes" 2>/dev/null || echo "$bytes bytes"
}

# Show directory statistics
show_directory_summary() {
    local tracked_dir=$1
    local dvc_file=$2
    
    echo "Directory summary:"
    local file_count=$(find "$tracked_dir" -type f 2>/dev/null | wc -l)
    local dir_size_bytes=$(du -sb "$tracked_dir" 2>/dev/null | cut -f1)
    local dir_size=$(du -sh "$tracked_dir" 2>/dev/null | cut -f1)
    
    local cached_nfiles=$(get_dvc_value "$dvc_file" "nfiles")
    local cached_size=$(get_dvc_value "$dvc_file" "size")
    
    if [ -n "$cached_nfiles" ] && [ -n "$cached_size" ]; then
        local file_diff=$((file_count - cached_nfiles))
        local size_diff=$((dir_size_bytes - cached_size))
        
        echo "  Total files: $file_count$(format_diff $file_diff) (was: $cached_nfiles)"
        
        local size_diff_str=""
        if [ "$size_diff" -ne 0 ]; then
            local abs_diff=$((size_diff < 0 ? -size_diff : size_diff))
            local sign=$([[ $size_diff -gt 0 ]] && echo "+" || echo "-")
            size_diff_str=" (${sign}$(format_size $abs_diff))"
        fi
        echo "  Directory size: $dir_size$size_diff_str (was: $(format_size $cached_size))"
    else
        echo "  Total files: $file_count"
        echo "  Directory size: $dir_size"
    fi
    echo
}

# Show a list of files with a prefix
show_file_list() {
    local title=$1
    local files=$2
    local prefix=$3
    
    if [ -n "$files" ]; then
        echo "  $title:"
        echo "$files" | head -n $MAX_FILES_TO_SHOW | sed "s/^/    $prefix /"
        local count=$(echo "$files" | wc -l)
        if [ "$count" -gt $MAX_FILES_TO_SHOW ]; then
            echo "    ... and $((count - MAX_FILES_TO_SHOW)) more"
        fi
    fi
}

# Get file MD5 from cache
get_cached_file_md5() {
    local cache_file=$1
    local relpath=$2
    python3 -c "import json; data=json.load(open('$cache_file')); print([f['md5'] for f in data if f['relpath']=='$relpath'][0])" 2>/dev/null
}

# Show file-level changes
show_file_changes() {
    local tracked_dir=$1
    local dvc_file=$2
    
    echo "File changes:"
    
    local cached_md5=$(get_dvc_value "$dvc_file" "md5")
    local cache_dir_file=".dvc/cache/files/md5/${cached_md5:0:2}/${cached_md5:2}"
    
    if [ ! -f "$cache_dir_file" ]; then
        echo "  (unable to determine file-level changes - cache not found)"
        echo
        return
    fi
    
    local tmp_cached=$(mktemp)
    local tmp_current=$(mktemp)
    
    # Get file lists
    python3 -c "import json; data=json.load(open('$cache_dir_file')); print('\n'.join(sorted([f['relpath'] for f in data])))" > "$tmp_cached" 2>/dev/null
    (cd "$tracked_dir" && find . -type f -printf '%P\n' | sort) > "$tmp_current"
    
    # Show deleted, added, and modified files
    show_file_list "Deleted files" "$(comm -23 "$tmp_cached" "$tmp_current")" "-"
    show_file_list "Added files" "$(comm -13 "$tmp_cached" "$tmp_current")" "+"
    
    # Find modified files
    local modified_files=""
    while IFS= read -r file; do
        local cached_md5=$(get_cached_file_md5 "$cache_dir_file" "$file")
        local current_md5=$(md5sum "$tracked_dir/$file" 2>/dev/null | cut -d' ' -f1)
        
        if [ -n "$cached_md5" ] && [ -n "$current_md5" ] && [ "$cached_md5" != "$current_md5" ]; then
            modified_files="${modified_files}${file}\n"
        fi
    done < <(comm -12 "$tmp_cached" "$tmp_current")
    
    show_file_list "Modified files" "$(echo -e "$modified_files" | grep -v '^$')" "M"
    
    rm -f "$tmp_cached" "$tmp_current"
    echo
}

# Prompt user and update DVC tracking
prompt_and_update() {
    local tracked_dir=$1
    local dvc_file=$2
    
    echo -n "Do you want to update DVC tracking for '$tracked_dir'? [y/N]: "
    read -r confirm
    
    if [[ "$confirm" =~ ^[Yy]$ ]]; then
        echo "Updating DVC tracking for '$tracked_dir'..."
        if dvc add "$tracked_dir" --verbose; then
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
}

# Main loop
for dvc_file in $(git ls-files '*.dvc'); do
    tracked_dir="${dvc_file%.dvc}"
    
    [ ! -d "$tracked_dir" ] && continue
    
    dvc_status_output=$(dvc status "$dvc_file" 2>/dev/null)
    echo "$dvc_status_output" | grep -q "modified:" || continue
    
    echo "Detected changes in '$tracked_dir' (DVC file: $dvc_file)"
    echo "$dvc_status_output"
    echo
    
    show_directory_summary "$tracked_dir" "$dvc_file"
    show_file_changes "$tracked_dir" "$dvc_file"
    prompt_and_update "$tracked_dir" "$dvc_file"
done
