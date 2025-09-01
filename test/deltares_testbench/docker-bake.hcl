variable "IMAGE_NAME" {
    default = "containers.deltares.nl/delft3d/testbench"
}

variable "DEFAULT_BRANCH" {
    default = "main"
}

variable "BUILD_BRANCH" {
    default = "none/task/UNKNOWN-0000-unknown"
}

variable "PULL_REQUEST_SOURCE_BRANCH" {
    default = "none/task/UNKNOWN-0000-unknown"
}

variable "DEFAULT_TAG_PREFIX" {
    default = "UNKNOWN-0000"
}

target "validate" {
    dockerfile = "ci/dockerfiles/testbench.Dockerfile"
    target = "report"
    cache-from = cache_from(IMAGE_NAME, tag_prefix(branch(BUILD_BRANCH, PULL_REQUEST_SOURCE_BRANCH)))
    cache-to = cache_to(IMAGE_NAME, tag_prefix(branch(BUILD_BRANCH, PULL_REQUEST_SOURCE_BRANCH)))
    output = ["type=local,dest=report"]
}   

// Get the branch name. If the build branch is a 'merge head', return the source branch of the pull request.
function "branch" {
    params = [build_branch, pull_request_source_branch]
    result = (
        length(regexall("^pull/.*$", build_branch)) > 0 
        ? pull_request_source_branch
        : build_branch
    )
}

// Determine the prefix of the image tag based on the branch.
function "tag_prefix" {
    params = [branch]
    result = coalesce(
        (branch == DEFAULT_BRANCH) ? DEFAULT_BRANCH : null,
        release_tag(branch),
        jira_issue_id(branch),
        DEFAULT_TAG_PREFIX,
    )
}

// Try to extract the JIRA issue ID from the branch name. If not found, return null.
function "jira_issue_id" {
    params = [branch]
    result = try(
        regex("^\\w+/\\w+/([A-Z0-9]+-[0-9]+).*$", branch)[0], 
        null,
    )
}

// Try to extract the release tag from the branch name. If it fails, return null.
function "release_tag" {
    params = [branch]
    result = try(
        regex("^\\w+/release/([-\\w.]+)$", branch)[0],
        null,
    )
}

// On feature branches use the JIRA issue ID in the image tag. Use the default layer cache as a fallback.
function "cache_from" {
    params = [image_name, tag_prefix]
    result = concat(
        ["type=registry,ref=${image_name}:${tag_prefix}-validate-cache"],
        (
            tag_prefix != DEFAULT_BRANCH 
            ? ["type=registry,ref=${image_name}:${DEFAULT_BRANCH}-validate-cache"]
            : []
        )
    )
}

// Use the JIRA issue ID in the image tag for feature branches.
function "cache_to" {
    params = [image_name, tag_prefix]
    result = ["type=registry,mode=max,ref=${image_name}:${tag_prefix}-validate-cache,image-manifest=true"]
}