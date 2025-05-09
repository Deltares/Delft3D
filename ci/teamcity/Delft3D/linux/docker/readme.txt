Delft3D FM computation using Docker 
===================================
This document describes the steps to perform a single-node Delft3D FM computation in Docker.
Note: For multinode calculations (on High Performance Computing platforms) our Apptainer solution should be used.

1 Loading the Docker image
=============================

    From tarball:
    $ docker load -i delft3dfm_<version>-<commit-id>.tar

    After this command load, the image should appear in your local Docker container image registry:

    $ docker image ls
    REPOSITORY                               TAG
    containers.deltares.nl/delft3d/delft3dfm release-<version>

    Note: Using this method, the image is stored locally and can be used for running the Docker container.

2 Run an example test case
==================================

    2.1 Run the example case that is provided in the container
    $ docker run -t --workdir /example containers.deltares.nl/delft3d/delft3dfm:release-<version> ./run_example.sh

    2.2 Run a provided example using the container; you can find example models in the examples folder next to this readme.txt
    Note: This run_docker.sh script is for running on Linux or within WSL2 (Windows Subsystem for Linux)
    $ examples/01_dflowfm_sequential/run_docker.sh

3 Your Delft3D FM computation with your own model
=================================================

Note: This solution with the run_docker.sh script is for running on Linux or within WSL2 (Windows Subsystem for Linux).
      Non-WSL2 Windows users can use the run_docker.sh script as a template for their own solution.

    Preparation:
      - Copy a run_docker.sh and run_example.sh files from an example folder into your own model directory.
          The intended locations:
            run_docker.sh   Root of your model folder; i.e. the directory containing ALL files and subfolders that are necessary for running your model.
            run_example.sh  Folder that contains the DIMR config XML.
      - Change the variable values inside your run_docker.sh where necessary; alternative example values are commented out:
            image:          Is already set for the specific release.
            docker_options: e.g. Add '--shm-size 4G' (or more for heavy models) to reserve shared memory for parallel and coupled runs.
            model_dir:      The directory containing ALL files and subfolders that are necessary for running your model.
            work_dir:       The directory that contains your DIMR config XML, relative to your ${model_dir}.
            command:        The shell command (e.g. a run script) to run inside your container from within ${work_dir}.

    Starting the computation:
      - Run:
        $ <path_to_your_model>/run_docker.sh
