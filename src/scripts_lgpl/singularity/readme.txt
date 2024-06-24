Note: Apptainer is the replacement for Singularity; the names are used interchangeably.
Note: The scripts "submit_singularity_h7.sh" and "execute_singularity_h7.sh" are suitable for submitting computations to a Slurm workload manager.

delft3dfm_202x.0x_lnx64_sif1xxx.tar.gz:
1. Download and install Apptainer: https://apptainer.org/docs/admin/main/installation.html
2. Unpack the delft3dfm-Singularity-sif-file and the runscript "execute_singularity_h7.sh". Keep them together in the same directory.
   Do not place multiple sif-files in the same directory.
3. Edit "execute_singularity_h7.sh": it must refer to an existing IntelMPI installation on your system. Refer to the comments in the script.
4. Copy "submit_singularity_h7.sh" into your working folder.
   This is an example script for executing Singularity computations. You'll need to modify it for your own needs.
   "Working folder" is the location of your dimr configuration file.
   Refer to the comments in the script.

More information: see User manual and Installation manual, see https://download.deltares.nl/en/download/delft3d-fm/
