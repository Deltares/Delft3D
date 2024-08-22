#!/bin/bash

IMAGE="containers.deltares.nl/delft3d/legacy/delft3dfm:latest"

MPI_DIR=/opt/apps/intelmpi/2021.10.0/mpi/2021.10.0
container_PATH=$MPI_DIR/bin:/usr/local/bin:/usr/bin:/usr/local/sbin:/usr/sbin:/sbin 
container_LD_LIBRARY_PATH=$MPI_DIR/lib:$MPI_DIR/lib/release

# Pull the Docker image
docker pull $IMAGE

# List all subfolders in the current directory and run the script if it exists
for dir in */; do
    echo "Checking folder: $dir"
    if [ -f "$dir/run_docker.sh" ]; then
		cd $dir
		echo "##teamcity[testStarted name='$dir' captureStandardOutput='true']"
		docker run \
			-v ".:/data" \
			-v "$MPI_DIR:$MPI_DIR" \
			-v "/usr/:/host" \
			-e PATH=$container_PATH \
			-e LD_LIBRARY_PATH=$container_LD_LIBRARY_PATH \
			$IMAGE
		echo "##teamcity[testFinished name='$dir']"
		cd ..
    else
        echo "No run script in $(pwd)"
    fi
done


