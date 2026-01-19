#!/bin/bash

# Script to schedule a build on TeamCity server
# Usage: ./schedule_teamcity_receive_job.sh <configuration_id> [--depend-on-build <build_id>] [param_name=param_value] ...

# Check if at least configuration ID is provided
if [ $# -lt 1 ]; then
    echo "Usage: $0 <configuration_id> [--depend-on-build <build_id>] [param_name=param_value] ..."
    echo "Examples:"
    echo "  $0 MyProject_BuildConfiguration"
    echo "  $0 MyProject_BuildConfiguration --depend-on-build 12345"
    echo "  $0 MyProject_BuildConfiguration --depend-on-build 12345 env.BUILD_NUMBER=123"
    echo "  $0 MyProject_BuildConfiguration env.BRANCH=feature/test system.debug=true"
    exit 1
fi

CONFIGURATION_ID="$1"
shift  # Remove configuration_id from arguments, leaving only parameters

# Parse dependency build ID if provided
SUMBIT_BUILD_ID=""
if [ "$1" = "--depend-on-build" ] && [ -n "$2" ]; then
    SUMBIT_BUILD_ID="$2"
    shift 2  # Remove --depend-on-build and build_id from arguments
    echo "Will create snapshot dependency on build ID: $SUMBIT_BUILD_ID"
fi

# TeamCity server configuration
TC_SERVER="${TC_SERVER:-https://dpcbuild.deltares.nl}"

# Parse credentials from ~/.teamcity/credentials file
CREDENTIALS_FILE="$HOME/.teamcity/credentials"

if [ ! -f "$CREDENTIALS_FILE" ]; then
    echo "Error: Credentials file not found at $CREDENTIALS_FILE"
    echo "Please create the file with format:"
    echo "[teamcity]"
    echo "tc_username=your_username"
    echo "tc_secret=your_password"
    exit 1
fi

# Parse username and password from credentials file
TC_USERNAME=$(grep "tc_username=" "$CREDENTIALS_FILE" | cut -d= -f2-)
TC_PASSWORD=$(grep "tc_secret=" "$CREDENTIALS_FILE" | cut -d= -f2-)

# Check if credentials were found
if [ -z "$TC_USERNAME" ] || [ -z "$TC_PASSWORD" ]; then
    echo "Error: Could not parse credentials from $CREDENTIALS_FILE"
    echo "Expected format:"
    echo "[teamcity]"
    echo "tc_username=your_username"
    echo "tc_secret=your_password"
    exit 1
fi

# Validate required configuration
if [ -z "$TC_SERVER" ]; then
    echo "Error: TeamCity server URL not specified. Set TC_SERVER environment variable."
    exit 1
fi

# Set up authentication credentials
AUTH_CREDENTIALS="-u $TC_USERNAME:$TC_PASSWORD"
echo "Using username/password authentication for user: $TC_USERNAME"

# API endpoint for triggering builds
API_ENDPOINT="$TC_SERVER/app/rest/buildQueue"

# Build XML payload with parameter overrides
PROPERTIES_XML=""
HAS_PARAMS=false

# Start properties section if we have parameters or dependencies
if [ $# -gt 0 ] || [ -n "$SUMBIT_BUILD_ID" ]; then
    PROPERTIES_XML="<properties>"
    HAS_PARAMS=true
fi

# Add user-provided parameters
if [ $# -gt 0 ]; then
    echo "Parameter overrides:"
    for param in "$@"; do
        if [[ "$param" == *"="* ]]; then
            param_name=$(echo "$param" | cut -d= -f1)
            param_value=$(echo "$param" | cut -d= -f2-)
            echo "  $param_name = $param_value"
            PROPERTIES_XML="$PROPERTIES_XML<property name=\"$param_name\" value=\"$param_value\"/>"
        else
            echo "Warning: Skipping invalid parameter format: $param (expected name=value)"
        fi
    done
fi

# Add dependency parameters if specified
if [ -n "$SUMBIT_BUILD_ID" ]; then
    echo "Fetching build type for build ID: $SUMBIT_BUILD_ID"
    
    # Get the build configuration ID from the build
    BUILD_INFO_RESPONSE=$(curl -s $AUTH_CREDENTIALS \
        -H "Accept: application/xml" \
        "$TC_SERVER/app/rest/builds/id:$SUMBIT_BUILD_ID")
    
    # Extract buildType id from the response
    SOURCE_BUILD_TYPE=$(echo "$BUILD_INFO_RESPONSE" | grep -o 'buildType id="[^"]*"' | sed 's/buildType id="//;s/"//')
    
    if [ -n "$SOURCE_BUILD_TYPE" ]; then
        echo "Source build type: $SOURCE_BUILD_TYPE"
        echo "Adding dependency information as build parameters:"
        echo "  dependency.build.id = $SUMBIT_BUILD_ID"
        echo "  dependency.buildtype.id = $SOURCE_BUILD_TYPE"
        
        # Add dependency parameters to properties
        PROPERTIES_XML="$PROPERTIES_XML<property name=\"dependency.build.id\" value=\"$SUMBIT_BUILD_ID\"/>"
        PROPERTIES_XML="$PROPERTIES_XML<property name=\"dependency.buildtype.id\" value=\"$SOURCE_BUILD_TYPE\"/>"
        
        if [ "$HAS_PARAMS" = false ]; then
            PROPERTIES_XML="<properties>$PROPERTIES_XML"
            HAS_PARAMS=true
        fi
    else
        echo "Warning: Could not determine source build type for build ID $SUMBIT_BUILD_ID"
    fi
fi

# Close properties section if we opened it
if [ "$HAS_PARAMS" = true ]; then
    PROPERTIES_XML="$PROPERTIES_XML</properties>"
fi

echo "Triggering build for configuration: $CONFIGURATION_ID"
echo "TeamCity server: $TC_SERVER"

# Build properties JSON
PROPERTIES_JSON=""
if [ $# -gt 0 ]; then
    PARAM_COUNT=0
    PROPERTY_ARRAY=""
    for param in "$@"; do
        if [[ "$param" == *"="* ]]; then
            param_name=$(echo "$param" | cut -d= -f1)
            param_value=$(echo "$param" | cut -d= -f2-)
            if [ $PARAM_COUNT -gt 0 ]; then
                PROPERTY_ARRAY="$PROPERTY_ARRAY,"
            fi
            PROPERTY_ARRAY="$PROPERTY_ARRAY { \"name\": \"$param_name\", \"value\": \"$param_value\" }"
            PARAM_COUNT=$((PARAM_COUNT + 1))
        fi
    done
    if [ $PARAM_COUNT -gt 0 ]; then
        PROPERTIES_JSON="\"properties\": { \"count\": $PARAM_COUNT, \"property\": [ $PROPERTY_ARRAY ] },"
    fi
fi

# Build JSON payload
if [ -n "$SUMBIT_BUILD_ID" ]; then
    echo "Creating JSON payload with snapshot dependency on build ID: $SUMBIT_BUILD_ID"
    
    # Create JSON payload with snapshot dependency
    JSON_PAYLOAD="{
        \"buildTypeId\": \"$CONFIGURATION_ID\",
        $PROPERTIES_JSON
        \"snapshot-dependencies\": {
            \"count\": 1,
            \"build\": [
                {
                    \"id\": $SUMBIT_BUILD_ID,
                    \"buildTypeId\": \"$SOURCE_BUILD_TYPE\"
                }
            ]
        }
    }"
else
    echo "Creating JSON payload without dependencies"
    
    # Remove trailing comma from properties if it's the last element
    PROPERTIES_JSON_CLEAN="${PROPERTIES_JSON%,}"
    
    # Create JSON payload without dependencies
    JSON_PAYLOAD="{
        \"buildTypeId\": \"$CONFIGURATION_ID\"$([ -n "$PROPERTIES_JSON_CLEAN" ] && echo ",
        $PROPERTIES_JSON_CLEAN" || echo "")
    }"
fi

CONTENT_TYPE="application/json"
PAYLOAD="$JSON_PAYLOAD"

echo ""
echo "=== Payload being sent ==="
echo "$PAYLOAD"
echo "=========================="
echo ""

# Make the API call
RESPONSE=$(curl -s -w "\nHTTP_STATUS:%{http_code}\n" \
    -X POST \
    -H "Content-Type: $CONTENT_TYPE" \
    $AUTH_CREDENTIALS \
    -d "$PAYLOAD" \
    "$API_ENDPOINT")

# Extract HTTP status code
HTTP_STATUS=$(echo "$RESPONSE" | grep "HTTP_STATUS" | cut -d: -f2)
RESPONSE_BODY=$(echo "$RESPONSE" | sed '/HTTP_STATUS/d')

# Check if the request was successful
if [ "$HTTP_STATUS" -eq 200 ] || [ "$HTTP_STATUS" -eq 201 ]; then
    echo "✓ Build triggered successfully!"
    echo -e "Response: \n$RESPONSE_BODY\n"
    
    # Extract build ID if available
    BUILD_ID=$(echo "$RESPONSE_BODY" | grep -o 'id="[0-9]*"' | head -1 | sed 's/id="//;s/"//')
    if [ -n "$BUILD_ID" ]; then
        echo "Build ID: $BUILD_ID"
        echo "Build URL: $TC_SERVER/viewLog.html?buildId=$BUILD_ID"
        echo "Dependency on build: $SUMBIT_BUILD_ID"
    fi
else
    echo "✗ Failed to trigger build. HTTP Status: $HTTP_STATUS"
    echo "Response: $RESPONSE_BODY"
    exit 1
fi