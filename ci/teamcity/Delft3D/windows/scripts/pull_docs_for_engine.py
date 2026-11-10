"""Pull doc folders for one engine via DVC (TeamCity documentation builds).

Expects to run from the repo root with dvc + dvc-s3 already installed
(e.g. TeamCity python step venv with dvc-docs-requirements.txt).
"""

from __future__ import annotations

import argparse
import os
import re
import subprocess
import sys
from pathlib import Path


def _teamcity_build_problem(description: str, identity: str) -> None:
    # Escape values for TeamCity service messages.
    safe = (
        description.replace("|", "||")
        .replace("'", "|'")
        .replace("\n", "|n")
        .replace("\r", "|r")
        .replace("[", "|[")
        .replace("]", "|]")
    )
    print(f"##teamcity[buildProblem description='{safe}' identity='{identity}']")


def _collect_doc_dvc_files(base_path: Path) -> list[Path]:
    """Root doc.dvc plus feature-folder doc.dvc files (f*)."""
    found: list[Path] = []

    root_doc = base_path / "doc.dvc"
    if root_doc.is_file():
        found.append(root_doc)
        print(f"[ROOT] doc.dvc included: {root_doc}")
    else:
        print("[WARNING] Root doc.dvc not found on disk")

    feature_re = re.compile(r"^f\d", re.IGNORECASE)
    for doc_dvc in sorted(base_path.rglob("doc.dvc")):
        if doc_dvc == root_doc:
            continue
        # Skip nested doc/doc.dvc if any
        if doc_dvc.parent.name.lower() == "doc":
            continue
        if any(feature_re.match(part) for part in doc_dvc.relative_to(base_path).parts):
            found.append(doc_dvc)
            print(f"[INCLUDED] {doc_dvc}")

    return found


def _dvc_pull(targets: list[Path], repo_root: Path, batch_label: str, engine_dir: str) -> None:
    cmd = ["dvc", "pull", *[str(t) for t in targets]]
    print(f"[BATCH {batch_label}] Running: {' '.join(cmd)}")
    # Run from repo root so DVC finds .dvc/config regardless of target paths.
    result = subprocess.run(cmd, cwd=repo_root)
    if result.returncode != 0:
        print(f"[ERROR] Failed to pull batch {batch_label}", file=sys.stderr)
        _teamcity_build_problem(
            f"DVC pull failed: batch {batch_label} ({engine_dir})",
            f"dvc_pull_batch_{batch_label}",
        )
        sys.exit(result.returncode or 1)
    print(f"[PULL OK] Batch {batch_label} completed")


def main() -> int:
    """CLI entrypoint: parse args and pull documentation via DVC."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--engine-dir",
        required=True,
        help="Engine directory name under cases/, e.g. e02_dflowfm",
    )
    parser.add_argument(
        "--batch-size",
        type=int,
        default=100,
        help="Number of doc.dvc files per dvc pull invocation",
    )
    args = parser.parse_args()

    # Avoid IMDS stalls on non-EC2 agents.
    os.environ.setdefault("AWS_EC2_METADATA_DISABLED", "true")

    repo_root = Path.cwd().resolve()
    base_path = repo_root / "test" / "deltares_testbench" / "data" / "cases" / args.engine_dir

    print(f"=== DVC doc pull started for engine_dir: {args.engine_dir} ===")
    print(f"[INFO] repo_root={repo_root}")
    print(f"[INFO] base_path={base_path}")

    if not base_path.is_dir():
        print(f"[ERROR] Base path not found: {base_path}", file=sys.stderr)
        _teamcity_build_problem(
            f"DVC base path not found: {base_path}",
            "dvc_base_path_missing",
        )
        return 1

    # Confirm dvc is on PATH (venv from TeamCity python step).
    which = subprocess.run(["dvc", "--version"], capture_output=True, text=True)
    if which.returncode != 0:
        print("[ERROR] dvc is not available in this environment", file=sys.stderr)
        print(which.stderr, file=sys.stderr)
        _teamcity_build_problem(
            "dvc not found after build-side install",
            "dvc_not_found",
        )
        return 1
    print(f"[INFO] Using {which.stdout.strip() or which.stderr.strip()}")

    all_doc_dvc = _collect_doc_dvc_files(base_path)
    total = len(all_doc_dvc)
    print(f"[DETECTION END] Total doc.dvc files: {total} (root + f*)")

    if total == 0:
        print("[WARNING] No doc.dvc files found; nothing to pull")
        return 0

    batch_size = max(1, args.batch_size)
    batch_count = 0
    for i in range(0, total, batch_size):
        batch_count += 1
        chunk = all_doc_dvc[i : i + batch_size]
        _dvc_pull(chunk, repo_root, str(batch_count), args.engine_dir)

    print("[VERIFICATION START]")
    verified = 0
    missing = 0
    for doc_dvc in all_doc_dvc:
        doc_folder = doc_dvc.parent / "doc"
        if doc_folder.is_dir():
            print(f"[VERIFIED] {doc_dvc}")
            verified += 1
        else:
            print(f"[MISSING] {doc_dvc} -> expected {doc_folder}")
            missing += 1
    print(f"[VERIFICATION END] Verified: {verified}   Missing: {missing}")

    print("=== DVC doc pull completed ===")
    if missing > 0:
        _teamcity_build_problem(
            f"{missing} doc folders failed to materialize ({args.engine_dir})",
            "dvc_missing_folders",
        )
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
