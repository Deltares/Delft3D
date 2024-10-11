#! /bin/bash
 
#--- Specify Slurm SBATCH directives ------------------------------------------------------------------------
#SBATCH --nodes=1               # Number of nodes.
#SBATCH --ntasks-per-node=1     # The number of tasks to be invoked on each node.
                                # For sequential runs, the number of tasks should be '1'.
                                # Note: SLURM_NTASKS is equal to "--nodes" multiplied by "--ntasks-per-node".
#SBATCH --job-name=upload       # Specify a name for the job allocation.
#SBATCH --time 00:10:00         # Set a limit on the total run time of the job allocation.
#SBATCH --partition=1vcpu       # Request a specific partition for the resource allocation.

container="$1"
teamcity_config="$2"
harbor_password="$3"
tc_account="$4"
tc_password="$5"
container_id=${container#*-}

docker login --username="robot\$delft3d+h7" --password="$harbor_password" containers.deltares.nl
docker pull "$container"
docker run --rm \
           -v .:/data/upload/TeamcityMinioStorage/h7_results \
           -v=$HOME/.aws:/root/.aws:ro \
           "$container" python3.9 -m tools.h7.upload_folder_to_minio devops-teamcity-artifacts

docker run --rm \
           -v=$HOME/.aws:/root/.aws:ro \
           "$container" python3.9 tools/h7/start_tc_build.py "$teamcity_config" container_id:"$container_id" "$tc_account" "$tc_password"
