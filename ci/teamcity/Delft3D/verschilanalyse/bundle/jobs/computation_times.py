#! /usr/bin/env python3
import re
import argparse
from itertools import groupby
from typing import Iterator, Iterable, Tuple
from statistics import mean, stdev
from pathlib import Path


PATTERN = re.compile(r'^.*total computation time\s+[(]s[)]\s*:\s*(?P<number>\d*[.]\d*)\s*$')

def find_computation_times(paths: Iterable[Path]) -> Iterator[Tuple[Path, float]]:
    for path in paths:
        with path.open('r') as lines:
            matches = filter(None, (PATTERN.match(line) for line in lines))
            yield from ((path, float(match.group("number"))) for match in matches)


if __name__ == "__main__":
    """Report total computation times in seconds for DIMR simulation runs.

    This script searches for `.dia` files in a given directory. It extracts
    the `total computation time` (in seconds) from each `.dia` file. The total
    computation times are grouped by the parent directory (so simulations that
    run with multiple tasks are grouped together). The script prints a CSV file,
    with each row printing the statistics for each simulation. These include:
    - The path to the results directory.
    - The total number of tasks.
    - The mean total computation time in seconds per task.
    - The standard deviation of the total computation time in seconds per task.
    """
    parser = argparse.ArgumentParser()
    parser.add_argument("-d", "--directory", type=Path, default='.')
    args = parser.parse_args()
    directory: Path = args.directory

    def groupby_parent_dir(tup: Tuple[Path, float]) -> Path:
        return tup[0].parent.relative_to(directory)

    dias = sorted(directory.rglob("*_[0-9][0-9][0-9][0-9].dia"))
    
    print("results_dir,task_count,mean_computation_time,stdev_computation_time")
    for result_dir, values in groupby(find_computation_times(dias), key=groupby_parent_dir):
        times = [t for _, t in values]
        print(f"{result_dir},{len(times)},{mean(times):.3f},{stdev(times):.3f}")
