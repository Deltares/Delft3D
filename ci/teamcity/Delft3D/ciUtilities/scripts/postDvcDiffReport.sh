#!/usr/bin/env bash
set -eo pipefail

USAGE_STRING="Usage: postDvcDiffReport.sh <target_branch> [<PULL_REQUEST_NUMBER> <GITHUB_BARER_TOKEN>]"
if [ "$#" -lt 1 ]; then 
    echo "Not enough arguments provided. $USAGE_STRING"
fi

if [ "$#" -gt 3 ]; then 
    echo "Too many arguments provided. $USAGE_STRING"
fi

echo "STARTING SCRIPT"

MAX_BYTES_DVC_FILE=1048576

TARGET_BRANCH="$1" 
PULL_REQUEST_NUMBER="$2"
GITHUB_BARER_TOKEN="$3"

SOURCE_BRANCH="$(git rev-parse HEAD)"
MERGE_BASE_COMMIT_HASH=$(git merge-base "$TARGET_BRANCH" HEAD)

POST_URL="https://api.github.com/repos/deltares/delft3d/issues/$PULL_REQUEST_NUMBER/comments"

# Generate the report
dvc diff "$MERGE_BASE_COMMIT_HASH" "$SOURCE_BRANCH" --show-hash --json > diff.json

# we might have to tune this, but dvc doesn't seem to have functionality to 
# e.g. fetch only from one branch and fetching from all commits is too slow
dvc fetch -R --max-size $MAX_BYTES_DVC_FILE -v

dvc checkout --allow-missing -v 

# Now we'll start adding a `diff` field to the json objects produced by the `dvc diff` command 
# so that we can display them in the jinja template later

# The code between added, modified, renamed and removed is almost the same, but dissimilar enough
# that we decided to just write seperate loops instead of one function

NUM_ADDED_FILES="$(jq '.added | length - 1' diff.json)"

for added_idx in $(seq 0 "$NUM_ADDED_FILES"); do
    ADDED_FILE_PATH="$(jq -c -r --arg idx "$added_idx" '.added | .[$idx | tonumber] | .path' diff.json)"
    if [ -s "$ADDED_FILE_PATH" ]; then

        MIME=$(file --mime-type "$ADDED_FILE_PATH" | cut -d: -f2 )
        MIME_TYPE=$(echo "$MIME" | cut -d/ -f2 | tr -d ' ')

        if [ "$MIME_TYPE" != "text" ]; then
            echo "Skipping $ADDED_FILE_PATH because it was not a text file"
            continue
        fi
        # get the language of the file from `file` so that we can use the correct syntax hilighting in the comment
        LANG=$( echo "$MIME" | cut -d/ -f2 | tr -d ' ')
       
        # jq recommended way of doing this. see https://github.com/jqlang/jq/wiki/FAQ#general-questions
        jq -c -r --arg idx "$added_idx" --rawfile diff_content "$ADDED_FILE_PATH" --arg lang "$LANG"  '.added[$idx | tonumber] += {"diff":$diff_content, "lang":$lang}' diff.json > tmp.json
        mv tmp.json diff.json
    else 
        echo "skipping adding the diff of $ADDED_FILE_PATH since it was not present"
    fi
done 

NUM_MODIFIED_FILES="$(jq '.modified | length - 1' diff.json)"
CACHE_DIR="$(dvc cache dir)"
for modified_idx in $(seq 0 "$NUM_MODIFIED_FILES"); do
        MODIFIED_FILE_PATH="$(jq -c -r --arg idx "$modified_idx" '.modified | .[$idx | tonumber] | .path' diff.json)"

        echo "checking $MODIFIED_FILE_PATH"
       
        OLD_HASH="$(jq -c -r --arg idx "$modified_idx" '.modified | .[$idx | tonumber] | .hash.old' diff.json)"
        OLD_DIR="$(echo "$OLD_HASH" | cut -c1-2)"
        OLD_FILE="$(echo "$OLD_HASH" | cut -c3-)"
        OLD_PATH="$CACHE_DIR/files/md5/$OLD_DIR/$OLD_FILE"
        echo "OLD_PATH: $OLD_PATH"

        NEW_HASH="$(jq -c -r --arg idx "$modified_idx" '.modified | .[$idx | tonumber] | .hash.new' diff.json)"
        NEW_DIR="$(echo "$NEW_HASH" | cut -c1-2)"
        NEW_FILE="$(echo "$NEW_HASH" | cut -c3-)" 
        NEW_PATH="$CACHE_DIR/files/md5/$NEW_DIR/$NEW_FILE"
        echo "new_PATH: $NEW_PATH"
        


        if [ ! -s "$OLD_PATH" ]; then
            echo "skpping because OLD_PATH did not exist in cache" 
        elif [ ! -s "$NEW_PATH" ]; then 
            echo "skpping because NEW_PATH did not exist in cache" 
        else
            
            MIME=$(file --mime-type "$OLD_PATH" | cut -d: -f2 )
            MIME_TYPE=$(echo "$MIME" | cut -d/ -f2 | tr -d ' ')
            
            if [ "$MIME_TYPE" != "text" ]; then
                echo "Skipping $MODIFIED_FILE_PATH because it was not a text file"
                continue
            fi

            # git diff will exit 1 if there are chagnes, and because we set -eo pipefiail ath the start, 
            # the script will stop if we don't add the || true at the end
            # We already know there will be changes because of dvc diff, so we're not creating false possitives here
            git diff --no-index --output diff.txt "$OLD_PATH" "$NEW_PATH" || true

            # jq recommended way of doing this. see https://github.com/jqlang/jq/wiki/FAQ#general-questions
            jq -c -r --arg idx "$modified_idx" --rawfile diff_content diff.txt  '.modified[$idx | tonumber] += {"diff":$diff_content}' diff.json > tmp.json
            mv tmp.json diff.json            
        fi
        
        # For modified files we don't add a language because these will alway displayed using the `diff` syntax
done 

# debugging logs
echo "diff.json:"

jq '.' diff.json

jinja2 ci/teamcity/Delft3D/ciUtilities/scripts/diff-report-template.jinja diff.json --lstrip-blocks --trim-blocks -o report.md

# 
if [ -z "$PULL_REQUEST_NUMBER" ]; then
    echo "PULL_REQUEST_NUMBER was not provided, so report was generated but not posted to github"
    exit 0
fi 

if [ -z "$GITHUB_BARER_TOKEN" ]; then
    echo "GITHUB_BARER_TOKEN was not provided, so report was generated but not posted to github"
    exit 0
fi

# check if report.md is empty. if it is and we got to this point there were no dvc changes
if [ -s report.md ]; then

    # debugging logs
    echo "Report contents: "
    cat report.md
    # use jq to format the generated report as a valid JSON payload
    PAYLOAD="$(jq -c -n --rawfile body report.md '$ARGS.named')"

    # Post the report
    curl -L \
        --fail \
        -X POST \
        -H "Accept: application/vnd.github+json" \
        -H "Authorization: Bearer $GITHUB_BARER_TOKEN" \
        -H "X-GitHub-Api-Version: 2022-11-28" \
        "$POST_URL" \
        -d "$PAYLOAD"
else
    echo "No dvc changes detected, therefore no report was generated"
    exit 0
fi

            