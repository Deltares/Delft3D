#!/bin/bash

# Script to submit TeamCity job after all case submits are completed
# This script depends on all the case submit jobs finishing first

#SBATCH --job-name=tc_job_scheduler
#SBATCH --time=00:10:00
#SBATCH --partition=4vcpu
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --chdir=./

CONFIGURATION_ID="$1"
DEPENDENCY_BUILD_ID="$2"
VCS_COMMIT_HASH="$3"
VCS_HASH_SHORT="${VCS_COMMIT_HASH:0:7}"

# Check if required arguments are provided
if [ -z "$CONFIGURATION_ID" ] || [ -z "$DEPENDENCY_BUILD_ID" ] || [ -z "$VCS_COMMIT_HASH" ]; then
    echo "Usage: $0 <configuration_id> <dependency_build_id> <vcs_commit_hash>"
    echo "Error: All three arguments are required and cannot be empty"
    echo "  CONFIGURATION_ID: '$CONFIGURATION_ID'"
    echo "  DEPENDENCY_BUILD_ID: '$DEPENDENCY_BUILD_ID'"
    echo "  VCS_COMMIT_HASH: '$VCS_COMMIT_HASH'"
    exit 1
fi

echo "Starting TeamCity job scheduler..."
echo "Configuration ID: $CONFIGURATION_ID"
echo "Dependency Build ID: $DEPENDENCY_BUILD_ID"
echo "VCS Commit Hash: $VCS_COMMIT_HASH"
echo "VCS Commit Hash short: $VCS_HASH_SHORT"

# Execute the TeamCity scheduler script
./schedule_teamcity_receive_job.sh "$CONFIGURATION_ID" --depend-on-build "$DEPENDENCY_BUILD_ID" build.vcs.number="$VCS_COMMIT_HASH" build.revisions.short="$VCS_HASH_SHORT"

echo "TeamCity job scheduler completed at: $(date)"