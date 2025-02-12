#!/bin/bash
# Get the commit time of the current commit
GIT_HEAD_TIME=$(git show -s --format=%ci HEAD)

# Set the TeamCity parameter
echo "##teamcity[setParameter name='env.TIME_ISO_8601' value='${GIT_HEAD_TIME}']"