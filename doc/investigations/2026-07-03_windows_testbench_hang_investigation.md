# Investigation: Delft3D_WindowsTest_virtual builds hanging on TeamCity

**Date:** 2026-07-03
**Project:** `Delft3D_WindowsTest_virtual` (dpcbuild.deltares.nl)
**Author:** Investigation performed with GitHub Copilot CLI, agent-assisted analysis of TeamCity build history, `perfmon.csv` artifacts, and a full memory dump (`procdump -ma`) of a live hung build.

## 1. Summary

Over the last week, roughly 162 builds failed in `Delft3D_WindowsTest_virtual`. These fall into three distinct
categories:

| Category | TeamCity problem type(s) | Count (last 7 days) | Verdict |
|---|---|---|---|
| Numerical/tolerance test failures | `TC_FAILED_TESTS` | majority | **Normal/expected** — not investigated further per user instruction. |
| Native crash | `TC_EXIT_CODE -1073741819` (`STATUS_ACCESS_VIOLATION`) | small number | Separate, unrelated issue — fast native crash (~2 min build lifetime), not a hang. Out of scope for this investigation. |
| **Build hangs / timeouts** | `TC_EXECUTION_TIMEOUT` + `TC_EXIT_CODE` (1) + `TC_ERROR_MESSAGE` | ~22 | **Root cause found — see below.** |

This document covers only the hang/timeout category.

## 2. Investigation steps

### 2.1 Ruling out a bad commit
Early hanging builds were correlated against their VCS revision (e.g. commit `a6c8e6b9`) to check for a single
regressing change. This theory was **ruled out**:
- The same build configuration (`[fm 3d]`) hung earlier the same day on an *older* commit.
- The originally suspected commit later **passed** on the same build agent.

Conclusion: hangs are not tied to one bad commit — they look like an intermittent/non-deterministic issue.

### 2.2 Ruling out CPU/memory exhaustion
TeamCity auto-collects a `perfmon.csv` artifact (`.teamcity/perfmon/perfmon.csv`) with 1-second-granularity
CPU/RAM samples for every build. This was pulled and bucketed (5-minute intervals) for two representative hung
builds (#7485053, #7481285):

- CPU usage dropped to **~1–2%** and stayed there.
- RAM usage was **perfectly flat** for 70+ minutes.

This is inconsistent with a memory leak, a CPU-bound computation, or agent resource contention/oversubscription.
It **is** consistent with a genuine deadlock: a process sitting idle, blocked on a synchronization primitive that
will never be signaled.

### 2.3 Reading build logs
Full build logs for representative hangs (#7473140, #7485053, #7481285) were pulled via the TeamCity REST API.
Two distinct hang "shapes" were seen:
- (a) Builds legitimately still running 100+ parallel test cases when the 90-minute timeout hit (just slow, not
  actually stuck) — not interesting.
- (b) Builds that went **completely silent** immediately after the line:
  ```
  Creating <N> processes to run test cases on.
  ```
  No further test-case start/finish lines were logged at all for the rest of the 90-minute window. This is the
  pattern that matched the perfmon evidence (idle CPU/RAM) and became the focus of the investigation.

### 2.4 Catching a hang live
The TeamCity REST API was queried for `running:true` builds in the project. Build **#7485927**
(`[fm drtc dwaves]`, agent `c-teamcity33130`) was found with `running-info.currentStageText`:
```
Step 4/5: BrokenPipeError: [WinError 232] The pipe is being closed
```
This was a concrete, live diagnostic clue and became the target for live process debugging before TeamCity's
90-minute auto-kill fired.

### 2.5 Capturing a memory dump
The test runs inside a Windows Docker container
(`containers.deltares.nl/delft3d-dev/test/delft3d-test-environment-windows:test-environment`) on agent
`c-teamcity33130`, using **Hyper-V isolation**, which meant:
- `docker top` does not expose host-visible PIDs.
- `docker cp` fails with *"filesystem operations not supported against hyperv containers"*.

Workaround: used the container's existing bind-mounted, writable directories (e.g.
`C:\BuildAgent\temp\agentTmp`, `C:\BuildAgent\work\...`) together with `docker exec` to run ProcDump *inside* the
container and write the dump to a bind-mounted path, making it visible on the host without needing `docker cp`.

```
procdump64.exe -accepteula -ma <pid> C:\BuildAgent\temp\agentTmp\stuck.dmp
```

The file was then extracted from the agent (`c-teamcity33130`) to the analysis workstation via an **RDP session
as `svc-teamcity-ansible`** (WinRM/PSRemoting was blocked by TrustedHosts/non-domain-join restrictions), using
RDP's local drive redirection (`\\tsclient\...`) to copy the 523 MB dump file out.

### 2.6 Analyzing the dump
Debugging Tools for Windows (`cdb.exe`) were installed directly (Windows SDK bootstrap installer, feature
`OptionId.WindowsDesktopDebuggers`, no winget/full SDK required) and run against the dump:

```
cdb -z stuck.dmp -c "!analyze -v; q"
cdb -z stuck.dmp -c "~*kb; q"
```

**`!analyze -v` result:** exception code `80000003` (breakpoint) — this is **ProcDump's own intentional
breakpoint injection** used to trigger the dump, not evidence of an actual crash. The process (`python.exe`,
Python 3.12.7) was alive and blocked, not crashed.

**`~*kb` (all thread stacks) result — 9 threads total:**

| Thread(s) | Stack (top frames) | Interpretation |
|---|---|---|
| Thread 0 (main) | `ntdll!NtWaitForSingleObject` → `KERNELBASE!WaitForSingleObjectEx` → `python312!PyThread_acquire_lock_timed` → ... → `PyEval_EvalFrameDefault` → `Py_RunMain`/`Py_Main` | **Main thread blocked acquiring a Python-level lock**, indefinitely — no timeout ever fires. |
| Threads 1–7 (7 identical stacks) | `KERNELBASE!WaitForMultipleObjects` ← `libopenblas64...!openblas_get_parallel_64_` | NumPy/SciPy's OpenBLAS internal thread pool, **idling normally** waiting for work — a red herring, not the cause. |
| Thread 8 | `KERNELBASE!WaitForMultipleObjects` deep inside `PyEval_EvalFrameDefault`/`PyFunction_Vectorcall`/`PyObject_Call` chains (symbol resolution imprecise here) | A second Python-managed thread also parked waiting on multiple handles — consistent with CPython multiprocessing's internal result-handling/worker-monitor thread. |

**Key finding:** the dump shows **no thread blocked in `WriteFile`/`NtWriteFile`**. This means the
`BrokenPipeError: [WinError 232]` seen in TeamCity's live status text was a **red herring / symptom logged
before the hang**, not the actual blocking mechanism. The real blocking mechanism is a **lock/handle wait
deadlock** — the main thread is waiting on a lock that is never released because the corresponding worker never
finishes and never signals back.

## 3. Root cause

`test/deltares_testbench/src/suite/test_set_runner.py::run_tests_in_parallel()` dispatches test cases to a
`multiprocessing.Pool`, using a `multiprocessing.Manager().Value`/`Condition` pair (`in_use` / `idle_process`) to
throttle how many OS processes are used concurrently (respecting each test case's configured `process_count`):

```python
with idle_process:
    while in_use.value + config.process_count > max_processes:
        idle_process.wait()          # <-- BLOCKS FOREVER, no timeout
    in_use.value += config.process_count
...
pool.close()
pool.join()                          # <-- BLOCKS FOREVER, no timeout
...
for result in result_futures:
    results.append(result.get())     # <-- BLOCKS FOREVER, no timeout
```

The `in_use` counter is only decremented, and `idle_process.notify_all()` only called, **after** a worker's
`run_test_case()` returns (see the block at the end of `run_test_case`). If a worker never returns — e.g. because
the engine subprocess it launched (DIMR / D-Flow FM, etc.) hangs — **none of the three waits above have a
timeout**, so:

1. The stuck worker's slot is never freed → `idle_process.wait()` in the main thread blocks forever once all
   process slots are exhausted, matching the **main thread's `PyThread_acquire_lock_timed`** stack seen in the
   dump (a `Condition.wait()` internally acquires/waits on a lock).
2. Even if slots weren't the issue, `result.get()` / `pool.join()` have no timeout either, so the whole build
   would hang waiting for a result that will never arrive.
3. TeamCity's only backstop is the blind, project-wide **90-minute execution timeout**, which kills the build
   with no indication of *which* test case was actually stuck — exactly the symptom observed
   (`TC_EXECUTION_TIMEOUT` + silence after "Creating N processes to run test cases on").

Separately (found via the same investigation, and already partially fixed on another branch,
`task/UNST-testbench-timeout-enforcement`, commit `5efb847f38`): a per-program `<maxRunTime>` was not being
propagated from `TestCase`'s computed effective max run time down into `Program.__start_process`'s
`subprocess.run(..., timeout=...)` call, meaning individual engine subprocess invocations without their own
explicit `maxRunTime` had **no timeout at all** at the subprocess level either — compounding the problem above,
since a single hung engine subprocess had no bound on how long it could hang before dragging the whole
parallel-runner down with it.

## 4. Fix implemented

Branch: `all/task/DEVOPDSC-testbench-timeout-enforcement` (based on `main`)

Two changes, both structural (preventing the deadlock from being possible) rather than a policy-level
timeout/enforcement bolted onto the parallel runner:

### 4.1 Bound the engine subprocess itself (the actual root cause fix)
Files: `test/deltares_testbench/src/suite/program.py`, `test/deltares_testbench/src/suite/test_case.py`

`TestCase.__init__` already computed an effective `maxRunTime` from the testcase's `<maxRunTime>` XML value, but
it was only ever used for logging — it was never wired into the actual subprocess execution. The real timeout
enforcement in `Program.__start_process` uses a separate, per-`<program>` `max_run_time` that defaults to `0`
(interpreted by `subprocess.run()` as **"no timeout"**) whenever a `<program>` element doesn't specify its own
`maxRunTime`. This is what let a program invocation (e.g. `dimr.exe`) block `subprocess.run()` indefinitely,
which is exactly what makes `run_test_case()` (the pool worker function) never return — the true origin of the
deadlock seen in the dump.

- `TestCase.__initializeProgramList__` now falls back `program_config.max_run_time` to the testcase's computed
  `maxRunTime` whenever the program itself doesn't specify one, so `subprocess.run(..., timeout=...)` actually
  enforces a bound.
- `Program.__execute__` now has an explicit `except subprocess.TimeoutExpired` branch that logs a clear timeout
  message and records the error (previously it fell through to the generic `except Exception` handler, which
  also had a latent bug: it referenced `e.filename`, an attribute that doesn't exist on `TimeoutExpired` or most
  other exception types, so it would itself raise a masking `AttributeError` instead of logging the real error).

With this in place, `run_test_case()` is guaranteed to return within a bounded time even if the engine hangs,
because the `subprocess.run()` call it depends on can no longer block forever. That means the process-slot
`Condition`/`Value` wait and `AsyncResult.get()`/`Pool.join()` in `run_tests_in_parallel()` can no longer block
forever either — the deadlock is prevented at its source, rather than being detected-and-failed after the fact.

### 4.2 Harden process-slot release against any worker exit path
File: `test/deltares_testbench/src/suite/test_set_runner.py`

`run_test_case()`'s process-slot release (`in_use.value -= config.process_count; idle_process.notify_all()`)
previously ran as a plain statement after the `try`/`except Exception` block. That means it was skipped — and
the slot leaked forever, permanently reducing capacity for the rest of the parallel run — if:
- an exception occurred that is not a subclass of `Exception` (e.g. `KeyboardInterrupt`, `SystemExit`), or
- `logger.test_finished()` itself raised.

The release logic is now wrapped in a `finally` block, so it unconditionally runs on every exit path out of
`run_test_case()`, closing off this leak regardless of what causes the worker to stop.

### Not fixed by this change (follow-up items)
- On Windows, killing a subprocess via `subprocess.run(timeout=...)` does not kill grandchild processes (e.g.,
  MPI ranks launched via a `cmd /c` wrapper), so those may be orphaned and keep running after the Python-level
  timeout fires. Process-tree termination (`CREATE_NEW_PROCESS_GROUP` + `taskkill /T /F`, or Job Objects) is a
  recommended follow-up.
- If a pool worker's OS process is killed outright (e.g. OOM, native crash) rather than merely hanging, raw
  `multiprocessing.Pool` has a known limitation where the corresponding `AsyncResult` can still never complete,
  since nothing survives to push a result. This wasn't the mechanism observed in the captured dump (the worker
  process was alive, just blocked), so it's out of scope here; if it turns out to matter in practice, migrating
  to `concurrent.futures.ProcessPoolExecutor` (which raises `BrokenProcessPool` in this situation) would be the
  more robust fix.
