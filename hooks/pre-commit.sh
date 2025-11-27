#!/bin/bash

set -e  # Exit on error

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
    local tmpfile=$3
    
    # Check the command for this directory (accept both full and short forms)
    local push_line=$(grep "^\(push\|p\) $tracked_dir" "$tmpfile" 2>/dev/null)
    local revert_line=$(grep "^\(revert\|r\) $tracked_dir" "$tmpfile" 2>/dev/null)
    
    if [ -n "$push_line" ]; then
        echo "Updating DVC tracking for '$tracked_dir'..."
        if dvc add "$tracked_dir" --verbose; then
            git add -f "$dvc_file"
            echo "Successfully updated DVC tracking for '$tracked_dir'"
        else
            echo "Error: Failed to update DVC tracking for '$tracked_dir'"
            exit 1
        fi
    elif [ -n "$revert_line" ]; then
        echo "Reverting DVC directory '$tracked_dir' to last tracked state..."
        if dvc checkout "$dvc_file" --force; then
            echo "Successfully reverted DVC directory '$tracked_dir'"
        else
            echo "Error: Failed to revert DVC directory '$tracked_dir'"
            exit 1
        fi
    else
        echo "Skipped updating DVC directory '$tracked_dir'."
    fi
}

# Main loop
# First, collect all changed DVC directories
changed_dirs=()
for dvc_file in $(git ls-files '*.dvc'); do
    tracked_dir="${dvc_file%.dvc}"
    
    [ ! -d "$tracked_dir" ] && continue
    
    dvc_status_output=$(dvc status "$dvc_file" 2>/dev/null)
    if echo "$dvc_status_output" | grep -q "modified:"; then
        changed_dirs+=("$tracked_dir|$dvc_file")
    fi
done

# Exit if no changes
if [ ${#changed_dirs[@]} -eq 0 ]; then
    exit 0
fi

# Create a single prompt file for all directories
prompt_file=$(mktemp /tmp/git-dvc-prompt-all.XXXXXX)

# Add summary section for each directory
for entry in "${changed_dirs[@]}"; do
    tracked_dir="${entry%|*}"
    
    echo "push $tracked_dir" >> "$prompt_file"
done

cat >> "$prompt_file" << 'EOF'

# ==============================================================================
# Commands:
# ==============================================================================
# p, push <directory> = update a directory
# s, skip <directory> = skip a directory
# r, revert <directory> = revert a directory to last tracked state
#
# - Save and close this file to continue

EOF

# Add detailed information for each directory
idx=1
for entry in "${changed_dirs[@]}"; do
    tracked_dir="${entry%|*}"
    dvc_file="${entry#*|}"
    
    {
        echo "# =============================================================================="
        echo "# [$idx] DETAILS FOR: $tracked_dir"
        echo "# DVC file: $dvc_file"
        echo "# =============================================================================="
        echo "#"
        
        show_directory_summary "$tracked_dir" "$dvc_file" | sed 's/^/# /'
        show_file_changes "$tracked_dir" "$dvc_file" | sed 's/^/# /'
    } >> "$prompt_file"
    
    idx=$((idx + 1))
done

# Open the file in editor
if [ -n "$VISUAL" ]; then
    $VISUAL "$prompt_file"
elif [ -n "$EDITOR" ]; then
    $EDITOR "$prompt_file"
elif command -v code &> /dev/null; then
    code --wait "$prompt_file" 2>/dev/null || nano "$prompt_file" 2>/dev/null || vi "$prompt_file"
elif command -v nano &> /dev/null; then
    nano "$prompt_file"
else
    vi "$prompt_file"
fi

# Process answers for each directory
for entry in "${changed_dirs[@]}"; do
    tracked_dir="${entry%|*}"
    dvc_file="${entry#*|}"
    
    prompt_and_update "$tracked_dir" "$dvc_file" "$prompt_file"
done

# Clean up
rm -f "$prompt_file"
