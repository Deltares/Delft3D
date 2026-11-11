# Slurm MPI Orchestration for MorMerge

## 1. Purpose and current implementation

This note documents the Slurm implementation in `mormerge.tcl` for MorMerge calculations with MPI-partitioned DIMR models. The implementation uses one Slurm allocation for the complete MorMerge calculation and starts every condition as a direct DIMR job step. Local, SGE, and Torque runs use separate launch paths.

All MorMerge conditions must run concurrently because they exchange data with the MorMerge server at morphological communication times. The scheduler can reduce the number of physical nodes by placing several concurrent condition steps on one node.

- `mormerge.tcl` derives both the per-condition MPI geometry and the allocation-wide task density.
- Each condition launches `<dimrexedir>/dimr` directly in an exclusive, exact `srun` job step.
- Slurm selects the physical nodes and CPU sets. The launcher leaves `--nodelist` unset for each condition.
- Several condition steps may share one allocation node when the request provides enough CPU and memory capacity.
- A failed DIMR or `srun` process produces the final `mormerge.tcl : Failed` status.

## 2. Execution sequence

When started outside a Slurm allocation with `queueSystem = slurm`, `mormerge.tcl` generates an `sbatch` script for the complete calculation. That script re-enters `mormerge.tcl` inside the allocation with the internal node count.

Inside the allocation, the launcher takes these steps.

1. Clone the model input into one directory per condition.
2. Clear the synchronization directory.
3. Start the serial MorMerge process for every run ID.
4. Wait for the MorMerge synchronization markers.
5. Start one direct DIMR job step for every condition.
6. Wait for all condition and MorMerge processes.
7. Return a nonzero status when a condition launcher fails.

The MorMerge processes run as ordinary serial child processes of the batch script. DIMR owns the MPI model execution inside the Slurm job step.

## 3. User-facing configuration

Set `nodes`, `queueSystem`, `queue`, `timeLimit`, `condition`, `weight`, and the model-directory settings in the `.mm` file. The following settings control Slurm MPI runs.

| Setting | Description |
| --- | --- |
| `nodes` | Total number of nodes in the Slurm allocation. |
| `dimrexedir` | Directory containing the `dimr` executable. |
| `dimrargs` | DIMR geometry metadata and configuration file. `mormerge.tcl` checks that `-c` equals `slurmTasksPerNode` and `--NNODES` equals `slurmNodesPerCondition`. It passes the configuration filename and supported DIMR options to `dimr`. |
| `slurmControlNodes` | Node-equivalent capacity excluded when deriving the packing factor. This setting only affects the packing calculation. The default is `0`. |
| `slurmNodesPerCondition` | Number of nodes each condition job step requests. A value of `0` defaults to one node in the optimized scheduler. |
| `slurmConditionsPerNode` | Maximum number of concurrent condition-step slices on one allocation node. The default of `0` selects the minimum packing needed to fit all conditions in the requested allocation. |
| `slurmTasksPerNode` | MPI ranks used by one condition on each node assigned to that condition. The default is `1`. |
| `slurmCpusPerTask` | CPUs reserved for each MPI rank. The launcher sets `OMP_NUM_THREADS` to the same value. The default is `1`. |
| `slurmMPI` | Site-supported value for `srun --mpi`, for example `pmix` or `pmi2`. A multi-rank direct DIMR condition requires a value compatible with the site's Slurm and MPI builds. |
| `slurmModuleCommand` | Optional site-specific command written into the generated batch script, such as a command to load compiler and MPI modules. The default is no command. |

## 4. Resource geometry

The optimized scheduler calculates model geometry and allocation packing separately.

| Symbol | Definition |
| --- | --- |
| `C` | Number of MorMerge conditions |
| `N` | `slurmNodesPerCondition` |
| `P` | `slurmConditionsPerNode` |
| `T` | `slurmTasksPerNode` |
| `H` | `slurmCpusPerTask` |
| `A` | `nodes - slurmControlNodes` |

Each condition consumes `N` condition-node slots, so the required number of slots is `C * N`. When `P = 0`, the scheduler derives `P = ceiling((C * N) / A)`. An explicit or derived packing factor is valid when `A * P >= C * N`.

| Quantity | Formula |
| --- | --- |
| MPI ranks per condition | `N * T` |
| MPI ranks across conditions | `C * N * T` |
| Allocation tasks per node | `P * T` |
| Allocation CPUs per node | `P * T * H` |

The generated batch script requests these resources.

```text
#SBATCH --nodes=<nodes>
#SBATCH --ntasks-per-node=<P * T>
#SBATCH --cpus-per-task=<H>
```

Slurm verifies whether it can allocate the requested CPU layout. Choose `P` so the concurrent conditions fit in node memory, since their memory requirements are outside the MorMerge packing calculation.

## 5. Direct DIMR job step

The generated launcher creates the following job step for each condition. Square brackets indicate an optional argument.

```text
srun --exclusive --exact --kill-on-bad-exit=1 \
     [--mpi=<slurmMPI>] \
     --nodes=<N> \
     --ntasks=<N * T> \
     --ntasks-per-node=<T> \
     --cpus-per-task=<H> \
     --export=ALL,OMP_NUM_THREADS=<H> \
     "<dimrexedir>/dimr" <DIMR options> "<dimr_config.xml>"
```

`--exclusive` prevents concurrent condition steps from using the same CPU set. `--exact` restricts a step to its requested resources so that other condition steps can use the remaining CPUs on the node. The target Slurm installation must support exact job-step resource allocation for packed runs.

For multi-rank conditions, the launcher first verifies that the configured Slurm MPI plugin exports an MPI rank environment recognized by DIMR. This detects an incompatible Slurm/MPI combination before the model starts. `srun` launches DIMR directly and serves as the only MPI launcher.

## 6. Supported MPI layouts

### 6.1. One condition per node

Use one node per condition when a complete model fits on one node and the allocation contains at least one node for every condition.

```text
nodes                  = 24
slurmNodesPerCondition = 1
slurmConditionsPerNode = 1
slurmTasksPerNode      = 10
slurmCpusPerTask       = 1
dimrargs               = -c 10 --NNODES 1 dimr_config.xml
```

For 24 conditions, this layout runs 10 MPI ranks per condition, requests 10 tasks per node, and runs 240 MPI ranks in total.

### 6.2. Several conditions per node

Use packing when a node has enough CPU and memory capacity for several complete models. Setting `slurmConditionsPerNode = 0` lets the scheduler derive the minimum packing needed for the requested node count.

```text
nodes                  = 12
slurmNodesPerCondition = 1
slurmConditionsPerNode = 0
slurmTasksPerNode      = 10
slurmCpusPerTask       = 1
dimrargs               = -c 10 --NNODES 1 dimr_config.xml
```

For 24 conditions, the derived packing factor is 2. The allocation requests 20 tasks per node, and Slurm places two exclusive 10-rank condition steps on each node, for a total of 240 MPI ranks.

The same calculation on six sufficiently large nodes derives `slurmConditionsPerNode = 4` and requests 40 tasks per node.

### 6.3. One condition across several nodes

Use multiple nodes per condition when a partitioned model does not fit on one node or requires more distributed MPI capacity.

```text
nodes                  = 8
slurmNodesPerCondition = 2
slurmConditionsPerNode = 1
slurmTasksPerNode      = 16
slurmCpusPerTask       = 1
dimrargs               = -c 16 --NNODES 2 dimr_config.xml
```

With four conditions, each condition uses 32 MPI ranks across two nodes. The allocation runs 128 MPI ranks in total.

### 6.4. Packed multi-node conditions

Packing also applies when every condition spans several nodes. Each condition consumes one condition slice on every node assigned to its job step.

```text
nodes                  = 4
slurmNodesPerCondition = 2
slurmConditionsPerNode = 0
slurmTasksPerNode      = 8
slurmCpusPerTask       = 1
dimrargs               = -c 8 --NNODES 2 dimr_config.xml
```

Four conditions require eight condition-node slots. Four allocation nodes therefore give a packing factor of 2. Each condition uses 16 MPI ranks, the allocation requests 16 tasks per node, and the total rank count is 64.

### 6.5. Hybrid MPI and OpenMP

Set `slurmCpusPerTask` above `1` when each MPI rank should use OpenMP threads. This setting controls the CPU reservation and thread environment independently of the MPI rank count.

For example, the 24-condition, 12-node packed layout with `slurmCpusPerTask = 2` launches 20 MPI ranks per node and requests 40 CPUs per node. The generated scripts set `OMP_NUM_THREADS` to `2`.

### 6.6. SWAN OpenMP threads

For Slurm runs that launch DIMR directly or start a separate WAVE step, MorMerge defaults `OMP_NUM_THREADS_SWAN` to `1` when it is unset or empty. The SWAN executable wrapper uses this value for its OpenMP thread count. `SwanMode = lib` calls the MPI SWAN library directly and bypasses the wrapper.

To override the default, export `OMP_NUM_THREADS_SWAN` before starting MorMerge. For a MorMerge-generated batch job, `slurmModuleCommand` can also export it. Use a single positive integer no greater than `slurmCpusPerTask`. MorMerge rejects invalid values before starting the condition and MorMerge processes. For example, `export OMP_NUM_THREADS_SWAN=2` requires `slurmCpusPerTask` of at least `2`.

## 7. Intel MPI example

The following example runs 24 conditions for a 10-domain model on 12 nodes. It assumes one MPI rank per model domain and two complete models per node. Module names and the Slurm MPI plugin are site-specific.

```text
queueSystem            = slurm
queue                  = normal
timeLimit              = 12:00:00
nodes                  = 12

dimrexedir             = /opt/delft3dfm/lnx64/bin
dimrargs               = -c 10 --NNODES 1 dimr_config.xml
mormergeexedir         = /opt/delft3dfm/lnx64/bin

slurmControlNodes      = 0
slurmNodesPerCondition = 1
slurmConditionsPerNode = 0
slurmTasksPerNode      = 10
slurmCpusPerTask       = 1
slurmMPI               = pmi2
slurmModuleCommand     = module load <intel-module> <intel-mpi-module>

condition:weight       = condition01 : <weight>
...
condition:weight       = condition24 : <weight>
```

| Allocation and condition geometry | Value |
| --- | ---: |
| Conditions | 24 |
| Condition steps per node | 2 |
| Nodes per condition | 1 |
| MPI ranks per condition | 10 |
| Allocation tasks per node | 20 |
| Total MPI ranks | 240 |

The example uses `pmi2`. Use the plugin and module combination supported by the cluster and compatible with the MPI library used to build DIMR.

## 8. Completion and failure semantics

MorMerge uses a stream protocol in which the server waits for the next frame. The current protocol has no dedicated client-completion frame. After successful condition models close their streams, the server can end with this message.

```text
GetPutArrayError: ABORT Recv ... returns 0 bytes
```

This is the expected server teardown after a condition stream closes. `mormerge.tcl` reports that known terminal status as informational. A failed condition DIMR job step, MPI preflight, or other child process is fatal and produces a nonzero final status.

A condition launched with one task is a serial DIMR run and normally reports `MPI: no`. A condition with more than one task should report MPI as enabled and show the expected rank and OpenMP thread counts.

## 9. Operational verification

For the first run on a cluster, set `debug = 1` in the `.mm` file to retain the generated batch, condition, and MorMerge scripts for inspection.

Check the job output for the following details.

- Total allocation nodes
- Derived condition steps per allocation node
- Allocation tasks and CPUs per node
- Nodes, MPI tasks, and CPUs per task for every condition
- Selected Slurm MPI plugin and loaded MPI module
- DIMR reporting of MPI and OpenMP status
- Nonzero condition-step exits or failed MPI environment preflights

If a job remains pending, compare `slurmConditionsPerNode * slurmTasksPerNode * slurmCpusPerTask` with the usable CPUs per node and check the site's memory request policy. If DIMR does not recognize MPI, verify that `slurmMPI` matches the loaded MPI library and the Slurm plugins installed on the cluster.

## 10. More info

Johan.Reyns@deltares.nl
