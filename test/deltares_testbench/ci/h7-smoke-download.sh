#! /bin/bash
 
#--- Specify Slurm SBATCH directives ------------------------------------------------------------------------
#SBATCH --nodes=1               # Number of nodes.
#SBATCH --ntasks-per-node=1     # The number of tasks to be invoked on each node.
                                # For sequential runs, the number of tasks should be '1'.
                                # Note: SLURM_NTASKS is equal to "--nodes" multiplied by "--ntasks-per-node".
#SBATCH --job-name=download     # Specify a name for the job allocation.
#SBATCH --time 00:10:00         # Set a limit on the total run time of the job allocation.
#SBATCH --partition=1vcpu       # Request a specific partition for the resource allocation.

container="$1"
apptainer_tag="$2"
harbor_password="$3"
tc_account="$4"
tc_password="$5"
tc_config="$6"

# Download test input
docker login --username="robot\$delft3d+h7" --password="$harbor_password" containers.deltares.nl
docker pull "$container"
docker run --rm \
           -v .:/data/data/cases \
           -v=$HOME/.aws:/root/.aws:ro \
           "$container" python3.9 TestBench.py --filter "testcase=e02_f014_c001,e109_f01_c010,e112_f01_c11" --reference --skip-run --skip-post-processing --config configs/singularity/dimr/dimr_smoke_test_lnx64.xml --log-level INFO --parallel

# Pull the Apptainer
module purge
module load apptainer/1.2.5 

export APPTAINER_DOCKER_USERNAME="robot\$delft3d+h7"
export APPTAINER_DOCKER_PASSWORD="$harbor_password"
apptainer pull -F --disable-cache "$apptainer_tag.sif" "oras://containers.deltares.nl/delft3d/apptainer/delft3dfm:$apptainer_tag"

folders=$(ls -d ./*/)

# Loop through each folder
for folder in $folders; do
    # Check if the folder name ends with '_mv'
    if [[ $folder == *_mv/ ]]; then
        # Get the base name of the folder without '_mv'
        target_folder="${folder%_mv/}"
        
        # Check if the target folder exists
        if [ -d "$target_folder" ]; then
            # Move all files from the '_mv' folder to the target folder
            mv "$folder"* "$target_folder"
            echo "Moved files from $folder to $target_folder"
        else
            echo "Target folder $target_folder does not exist"
        fi
    fi
done

# Array to store job IDs
job_ids=()

# Loop through each folder in the base directory
for folder in $folders; do
	if [[ $folder != *_mv/ ]]; then
		# Submit the apptainer to the slurm queue with sbatch and store the job ID
        if [ -f "$folder/dimr_config.xml" ]; then
            replacement="<library>dflowfm</library>\n        <process>0</process>\n        <mpiCommunicator>DFM_COMM_DFMWORLD</mpiCommunicator>"
            sed -i "s|<library>dflowfm</library>|$replacement|g" "$folder/dimr_config.xml"
        fi
		job_id=$(sbatch --parsable "$folder/submit_singularity_h7.sh")
		job_ids+=("$job_id")
		echo "Submitted $folder/submit_singularity.sh with job ID $job_id"
	fi
done

# Create a comma-separated list of job IDs for dependencies
dependency_list=$(IFS=,; echo "${job_ids[*]}")

# Submit a dependent job that runs after all previous jobs are completed
sbatch --dependency=afterany:"$dependency_list" h7-smoke-upload.sh "$container" "$tc_config" "$harbor_password" "$tc_account" "$tc_password"
