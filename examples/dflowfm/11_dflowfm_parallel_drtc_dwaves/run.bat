@ echo off
    rem When using Intel MPI for parallel execution:
    rem 
    rem For LOCAL parallel execution (most common case):
    rem No special setup is required. The -localonly flag allows MPI to run
    rem without the hydra service.
    rem 
    rem For REMOTE/CLUSTER parallel execution:
    rem Modern Intel MPI integrates with cluster resource managers and cloud platforms.
    rem 
    rem For HPC clusters:
    rem   Use your cluster's job scheduler (Slurm, PBS, etc.) - no additional setup needed
    rem   Example: sbatch, qsub, or similar cluster commands
    rem
    rem For cloud environments:
    rem   Intel MPI has built-in support for AWS, Azure, and Google Cloud
    rem
    rem For ad-hoc multi-node execution:
    rem   Use -hosts option: mpiexec -hosts host1,host2,host3 -n 6 your_program
    rem   Or use host file: mpiexec -f hostfile -n 6 your_program
    rem   Requires SSH access between nodes (no hydra service installation needed)

set dimrdir=\\directory.intra\PROJECT\d-hydro\dimrset\latest


cd fm
call "%dimrdir%\x64\bin\run_dflowfm.bat" "--partition:ndomains=3:icgsolver=6" weirtimeseries.mdu
cd ..

call "%dimrdir%\x64\bin\run_dimr_parallel.bat" 3 dimr_config.xml


    rem To prevent the DOS box from disappearing immediately: remove the rem on the following line
rem pause
