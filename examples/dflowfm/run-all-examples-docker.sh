#!/bin/bash

IMAGE="containers.deltares.nl/delft3d/legacy/delft3dfm:latest"

MPI_DIR=/opt/apps/intelmpi/2021.10.0/mpi/2021.10.0
PATH=$MPI_DIR/bin:/usr/local/bin:/usr/bin:/usr/local/sbin:/usr/sbin:/sbin:/opt/intel/mpi/bin:/opt/intel/mpi/libfabric/bin
LD_LIBRARY_PATH=$MPI_DIR/lib:$MPI_DIR/lib/release:/opt/intel/mpi/lib:/opt/intel/mpi/libfabric/lib

# Pull the Docker image
docker pull $IMAGE

# List all subfolders in the current directory and run the script if it exists
for dir in */; do
    echo "Checking folder: $dir"
    if [ -f "$dir/run_docker.sh" ]; then
		cd $dir
		echo "##teamcity[testStarted name='$dir' captureStandardOutput='true']"
		docker run \
			-v "$(pwd):/data" \
			-v "$MPI_DIR:$MPI_DIR" \
			-v "/usr/:/host" \
			-e PATH=$PATH \
			-e LD_LIBRARY_PATH=$LD_LIBRARY_PATH \
			-e FI_PROVIDER_PATH=/opt/intel/mpi/libfabric/lib/prov \
			-e I_MPI_FABRICS=shm \
			--shm-size 8G \
			$IMAGE
		echo "##teamcity[testFinished name='$dir']"
		cd ..
    else
        echo "No run script in $(pwd)"
    fi
done