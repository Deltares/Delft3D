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




    rem This example testcase is that small, that it can run with a maximum of 5 partitions
set NPROC=%NUMBER_OF_PROCESSORS%
if %NPROC% gtr 5 set NPROC=5

    rem At present, this runscript will only work after having executed the following command in a DOS-box, at the top folder of the source tree:
    rem build.bat all
    rem See README.md there for more information

set build_configuration=build_delft3d4
set script_path=..\..\..\%build_configuration%\x64\dflow2d3d\scripts
call %script_path%\run_dflow2d3d_parallel.bat %NPROC%


    rem To prevent the DOS box from disappearing immediately: remove the rem on the following line
rem pause
