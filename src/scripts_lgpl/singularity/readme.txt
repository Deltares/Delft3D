delft3dfm_202x.0x_lnx64_sif1xxx.tar.gz:
1. Download and install Singularity: https://sylabs.io/guides/3.8/user-guide/introduction.html
2. Unpack the delft3dfm-Singularity-sif-file and the runscript "execute_singularity.sh". Keep them together in the same directory.
   Do not place multiple sif-files in the same directory.
3. Edit "execute_singularity.sh": it must refer to an existing IntelMPI installation on your system. Check the comments in there.
4. Copy into your working folder: "run_singularity.sh" and/or "submit_singularity.sh"
   They are example scripts for executing Singularity computations. You'll need to modify them for your own needs.
   "Working folder" is the location of your dimr configuration file.
   Check the comments in there.

More information: see User manual and Installation manual, see https://download.deltares.nl/en/download/delft3d-fm/
