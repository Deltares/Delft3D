import sys

import requests
from requests.auth import HTTPBasicAuth

if __name__ == "__main__":
    # Check if the container_id argument is provided
    if len(sys.argv) != 5:
        print("Usage: python script.py <tc_configuration> <parameter_id:value> <teamcity_user> <teamcity_pass>")
        sys.exit(1)

    # Build configuration ID
    build_configuration_id = sys.argv[1]

    # Parse parameters
    try:
        parameters = sys.argv[2].split(",")
        parameter_dict = [
            {"name": key, "value": value} for key, value in (parameter.split(":") for parameter in parameters)
        ]
    except ValueError:
        print("Error: Parameters should be in the format 'parameter_id:value'")
        sys.exit(1)

    # TeamCity server URL and credentials
    teamcity_url = "https://dpcbuild.deltares.nl/httpAuth/app/rest/buildQueue"
    username = sys.argv[3]
    password = sys.argv[4]

    # JSON payload for the build request
    json_payload = {
        "buildType": {"id": build_configuration_id},
        "properties": {"property": parameter_dict},
    }

    # Send the build start request to TeamCity API
    response = requests.post(teamcity_url, json=json_payload, auth=HTTPBasicAuth(username, password))

    # Check if the request was successful
    if response.status_code == 200 and "<build" in response.text:
        print("Build started successfully.")
    else:
        print(f"Failed to start build. Response: {response.text}")
