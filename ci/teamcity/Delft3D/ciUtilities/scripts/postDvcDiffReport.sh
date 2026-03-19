#!/usr/bin/env bash
set -euo pipefail

USAGE_STRING="Usage: postDvcDiffReport.sh <target_branch> [<PULL_REQUEST_NUMBER> <GITHUB_BARER_TOKEN>]"
if [ "$#" -lt 1 ]; then 
    echo "Not enough arguments provided. $USAGE_STRING"
fi

if [ "$#" -gt 3 ]; then 
    echo "Too many arguments provided. $USAGE_STRING"
fi

TEMP_DIR="$(mktemp --directory)"

MAX_BYTES_DVC_FILE=1048576

TARGET_BRANCH="$1" 
PULL_REQUEST_NUMBER="$2"
GITHUB_BARER_TOKEN="$3"

SOURCE_BRANCH="$(git rev-parse HEAD)"
MERGE_BASE_COMMIT_HASH=$(git merge-base "$TARGET_BRANCH" HEAD)

POST_URL="https://api.github.com/repos/deltares/delft3d/issues/$PULL_REQUEST_NUMBER/comments"

function generate_dvc_diff() {
    # Generate the report
    dvc diff "$MERGE_BASE_COMMIT_HASH" "$SOURCE_BRANCH" --show-hash --json > "$TEMP_DIR/diff.json"
}

function fetch_dvc_files() {    
    # we might have to tune this, but dvc doesn't seem to have functionality to 
    # e.g. fetch only from one branch and fetching from all commits is too slow
    dvc fetch -R --max-size $MAX_BYTES_DVC_FILE -v

    dvc checkout --allow-missing -v 
}

# Now we'll start adding a `diff` field to the json objects produced by the `dvc diff` command 
# so that we can display them in the jinja template later

# The code between added, modified, renamed and removed is almost the same, but dissimilar enough
# that we decided to just write seperate loops instead of one function

function generate_added_files_diffs() {
    local num_added_files
    num_added_files="$(jq '.added | length - 1' "$TEMP_DIR/diff.json")"

    for added_idx in $(seq 0 "$num_added_files"); do
        local added_file_path
        added_file_path="$(jq -c -r --arg idx "$added_idx" '.added | .[$idx | tonumber] | .path' "$TEMP_DIR/diff.json")"
        
        if [ -s "$added_file_path" ]; then

            local mime
            mime="$(file --mime-type "$added_file_path" | cut -d: -f2 )"
            
            local mime_type
            mime_type="$(echo "$mime" | cut -d/ -f2 | tr -d ' ')"

            if [ "$mime_type" != "text" ]; then
                echo "Skipping $added_file_path because it was not a text file"
                continue
            fi
            # get the language of the file from `file` so that we can use the correct syntax hilighting in the comment
            local lang
            lang="$( echo "$mime" | cut -d/ -f2 | tr -d ' ')"
        
            # jq recommended way of doing this. see https://github.com/jqlang/jq/wiki/FAQ#general-questions
            jq -c -r --arg idx "$added_idx" --rawfile diff_content "$added_file_path" --arg lang "$lang"  '.added[$idx | tonumber] += {"diff":$diff_content, "lang":$lang}' "$TEMP_DIR/diff.json" > "$TEMP_DIR/tmp.json"
            mv "$TEMP_DIR/tmp.json" "$TEMP_DIR/diff.json" 
        else 
            echo "skipping adding the diff of $added_file_path since it was not present"
        fi
    done 
}

function generate_modified_files_diffs() {
    local num_modified_files
    num_modified_files="$(jq '.modified | length - 1' "$TEMP_DIR/diff.json")"
    
    local cache_dir
    cache_dir="$(dvc cache dir)"
    for modified_idx in $(seq 0 "$num_modified_files"); do
            local modified_file_path
            modified_file_path="$(jq -c -r --arg idx "$modified_idx" '.modified | .[$idx | tonumber] | .path' "$TEMP_DIR/diff.json")"

            echo "checking $modified_file_path"
            local old_dir, old_file, old_dir, old_path

            old_hash="$(jq -c -r --arg idx "$modified_idx" '.modified | .[$idx | tonumber] | .hash.old' "$TEMP_DIR/diff.json")"
            old_dir="$(echo "$old_hash" | cut -c1-2)"
            old_file="$(echo "$old_hash" | cut -c3-)"
            old_path="$cache_dir/files/md5/$old_dir/$old_file"
            echo "OLD_PATH: $old_path"

            local new_hash, new_dir, new_file, new_path
            new_hash="$(jq -c -r --arg idx "$modified_idx" '.modified | .[$idx | tonumber] | .hash.new' "$TEMP_DIR/diff.json")"
            new_dir="$(echo "$new_hash" | cut -c1-2)"
            new_file="$(echo "$new_hash" | cut -c3-)" 
            new_path="$cache_dir/files/md5/$new_dir/$new_file"
            echo "new_PATH: $new_path"
            


            if [ ! -s "$old_path" ]; then
                echo "skpping because OLD_PATH did not exist in cache" 
            elif [ ! -s "$new_path" ]; then 
                echo "skpping because NEW_PATH did not exist in cache" 
            else
                
                local mime, mime_type
                mime="$(file --mime-type "$old_path" | cut -d: -f2 )"
                mime_type="$(echo "$mime" | cut -d/ -f2 | tr -d ' ')"
                
                if [ "$mime_type" != "text" ]; then
                    echo "Skipping $modified_file_path because it was not a text file"
                    continue
                fi

                # git diff will exit 1 if there are chagnes, and because we set -eo pipefiail ath the start, 
                # the script will stop if we don't add the || true at the end
                # We already know there will be changes because of dvc diff, so we're not creating false possitives here
                git diff --no-index --output "$TEMP_DIR/diff.txt" "$old_path" "$new_path" || true

                # jq recommended way of doing this. see https://github.com/jqlang/jq/wiki/FAQ#general-questions
                jq -c -r --arg idx "$modified_idx" --rawfile diff_content "$TEMP_DIR/diff.txt"  '.modified[$idx | tonumber] += {"diff":$diff_content}' "$TEMP_DIR/diff.json" > "$TEMP_DIR/tmp.json"
                mv "$TEMP_DIR/tmp.json" "$TEMP_DIR/diff.json"            
            fi
            
            # For modified files we don't add a language because these will alway displayed using the `diff` syntax
    done 
}

function debug_logs() {

# debugging logs
echo "$TEMP_DIR/diff.json"

jq '.' "$TEMP_DIR/diff.json"

}

function generate_template_from_json {
    jinja2 ci/teamcity/Delft3D/ciUtilities/scripts/diff-report-template.jinja "$TEMP_DIR/diff.json" --lstrip-blocks --trim-blocks -o "$TEMP_DIR/report.md"
}

function post_report_to_github() {
    # check if report.md is empty. if it is and we got to this point there were no dvc changes
    if [ -s "$TEMP_DIR/report.md" ]; then

        # debugging logs
        echo "Report contents: "
        cat "$TEMP_DIR/report.md"
        # use jq to format the generated report as a valid JSON payload
        PAYLOAD="$(jq -c -n --rawfile body "$TEMP_DIR/report.md" '$ARGS.named')"

        # Post the report
        curl --location \
            --fail \
            --show-error \
            --silent \
            --request POST \
            --header "Accept: application/vnd.github+json" \
            --header "Authorization: Bearer $GITHUB_BARER_TOKEN" \
            --header "X-GitHub-Api-Version: 2022-11-28" \
            "$POST_URL" \
            --data "$PAYLOAD"
    else
        echo "No dvc changes detected, therefore no report was generated"
        exit 0
    fi
}

function main {
    generate_dvc_diff
    fetch_dvc_files
    generate_added_files_diffs
    generate_modified_files_diffs
    debug_logs
    generate_template_from_json
  
    if [ -z "$PULL_REQUEST_NUMBER" ]; then
        echo "PULL_REQUEST_NUMBER was not provided, so report was generated but not posted to github"
        exit 0
    fi 

    if [ -z "$GITHUB_BARER_TOKEN" ]; then
        echo "GITHUB_BARER_TOKEN was not provided, so report was generated but not posted to github"
        exit 0
    fi
    
    post_report_to_github
}

main "$@"