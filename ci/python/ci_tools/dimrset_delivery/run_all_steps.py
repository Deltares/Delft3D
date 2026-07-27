#!/usr/bin/env python3
"""
Master script to run all DIMR delivery steps in sequence.

This script discovers and executes all step scripts for the DIMR delivery pipeline.
"""

import importlib.util
import inspect
import re
import sys
import time
from datetime import datetime, timezone
from pathlib import Path
from typing import List, Optional

from ci_tools.dimrset_delivery.arg_parsing import create_context_from_args, parse_common_arguments
from ci_tools.dimrset_delivery.dimr_context import DimrAutomationContext
from ci_tools.dimrset_delivery.services import Services
from ci_tools.example_utils.logger import LogLevel


class StepResult:
    """
    Result of running a single step.

    Stores the outcome, duration, and error information for a pipeline step.
    """

    def __init__(
        self,
        step_name: str,
        success: bool,
        duration: float,
        exit_code: int = 0,
        error: Optional[str] = None,
    ) -> None:
        self.step_name = step_name
        self.success = success
        self.duration = duration
        self.exit_code = exit_code
        self.error = error


class DimrDeliveryPipeline:
    """
    Master pipeline to run all DIMR delivery steps.

    Discovers, runs, and summarizes all step scripts for the DIMR delivery process.
    """

    def __init__(self, context: DimrAutomationContext, services: Services) -> None:
        self.context = context
        self.services = services
        self.results: List[StepResult] = []
        self.script_dir = Path(__file__).parent
        self.total_steps = 0

    def run_all_steps(self) -> bool:
        """
        Run all delivery steps in sequence using direct method calls.

        Returns
        -------
        bool
            True if all steps succeed, False otherwise.
        """
        start_time = datetime.now(timezone.utc)
        self.context.log(f"🚀 Starting DIMR delivery pipeline at {start_time}")
        self.context.log("=" * 60)

        # Discover step scripts
        step_scripts = []
        for file in sorted(self.script_dir.glob("step_*.py")):
            match = re.match(r"step_(\d+)_([\w-]+)\.py", file.name)
            if match:
                step_num = match.group(1)
                step_desc = match.group(2).replace("_", " ").replace("-", " ").title()
                step_name = f"Step {step_num}: {step_desc}"
                step_scripts.append((step_name, file))
        self.total_steps = len(step_scripts)
        if not step_scripts:
            self.context.log("No step scripts found in directory.", severity=LogLevel.ERROR)
            self._print_summary(start_time, failed_early=True)
            return False

        # Run each step by importing and calling execute_step
        for step_name, file_path in step_scripts:
            if not self._run_single_step_direct(step_name, file_path):
                self._print_summary(start_time, failed_early=True)
                return False

        self._print_summary(start_time, failed_early=False)
        return True

    def _run_single_step_direct(self, step_name: str, file_path: Path) -> bool:
        """
        Import step module and call StepExecutorInterface.execute_step directly.

        Parameters
        ----------
        step_name : str
            Name of the step to run.
        file_path : Path
            Path to the step script file.

        Returns
        -------
        bool
            True if the step succeeds, False otherwise.

        Raises
        ------
        Exception
            If the step script cannot be loaded or executed.
        """
        print(f"\n🚀 Starting {step_name}")
        step_start = time.time()
        module_name = file_path.stem
        spec = importlib.util.spec_from_file_location(module_name, str(file_path))
        try:
            spec = importlib.util.spec_from_file_location(module_name, str(file_path))
            if spec is None or spec.loader is None:
                raise RuntimeError(f"Could not load module spec for {file_path}")

            module = importlib.util.module_from_spec(spec)
            spec.loader.exec_module(module)

            # Find class implementing StepExecutorInterface
            step_class = None
            for _, obj in inspect.getmembers(module, inspect.isclass):
                # Check for interface by name or inheritance
                if "StepExecutorInterface" in [base.__name__ for base in obj.__bases__]:
                    step_class = obj
                    break
            if not step_class:
                raise RuntimeError("No class implementing StepExecutorInterface found in step script.")

            # Instantiate and execute
            # Pass context and services if constructor accepts them
            try:
                instance = step_class(self.context, self.services)
            except TypeError:
                instance = step_class(self.context)
            result = instance.execute_step()
            duration = time.time() - step_start

            # Assume execute_step returns True/False for success
            if result is True:
                self.results.append(StepResult(step_name, True, duration, 0))
                print(f"✅ {step_name} completed successfully in {duration:.2f}s")
                return True
            else:
                error_msg = f"execute_step returned {result}"
                self.results.append(StepResult(step_name, False, duration, 1, error_msg))
                print(f"❌ {step_name} failed: {error_msg}")
                return False
        except Exception as e:
            duration = time.time() - step_start
            error_msg = str(e)
            self.results.append(StepResult(step_name, False, duration, -3, error_msg))
            print(f"❌ {step_name} failed: {error_msg}")
            return False

    def _print_summary(self, start_time: datetime, failed_early: bool) -> None:
        """
        Print execution summary.

        Parameters
        ----------
        start_time : datetime
            When the pipeline started.
        failed_early : bool
            Whether the pipeline failed before completing all steps.
        """
        end_time = datetime.now(timezone.utc)
        total_duration = (end_time - start_time).total_seconds()

        print("\n" + "=" * 60)
        print("📊 EXECUTION SUMMARY")
        print("=" * 60)

        successful_steps = sum(1 for result in self.results if result.success)
        executed_steps = len(self.results)

        for result in self.results:
            status = "✅ SUCCESS" if result.success else "❌ FAILED"
            exit_info = f"(exit {result.exit_code})" if not result.success else ""
            print(f"{status:12} | {result.step_name:40} | {result.duration:6.2f}s {exit_info}")
            if result.error:
                print(f"{'':12} | Error: {result.error}")

        if failed_early:
            remaining = self.total_steps - executed_steps
            for i in range(remaining):
                print(f"⏭️  SKIPPED   | Step {executed_steps + i + 1}: (skipped due to previous failure)")

        print("-" * 60)
        print(f"Total time: {total_duration:.2f}s")
        print(f"Steps completed: {successful_steps}/{self.total_steps}")

        if successful_steps == self.total_steps:
            print("✅ All steps completed successfully!")
        elif failed_early:
            print(f"❌ Script failed at step {executed_steps}")
        else:
            print(f"❌ Script completed with {self.total_steps - successful_steps} failures")


if __name__ == "__main__":
    try:
        args = parse_common_arguments()
        context = create_context_from_args(args)
        services = Services(context)

        pipeline = DimrDeliveryPipeline(context, services)
        success = pipeline.run_all_steps()

        sys.exit(0 if success else 1)

    except KeyboardInterrupt:
        print("\n🛑 Script interrupted by user")
        sys.exit(130)

    except (ValueError, AssertionError) as e:
        print(f"❌ Script failed: {e}")
        sys.exit(1)

    except Exception as e:
        print(f"❌ Script failed with unexpected error: {e}")
        sys.exit(2)
