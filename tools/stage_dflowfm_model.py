#!/usr/bin/env python3
"""Copy the input files needed by a D-Flow FM MDU to a local directory."""

from __future__ import annotations

import argparse
import fnmatch
import json
import os
import re
import shlex
import shutil
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import Callable, Iterable


REFERENCE_KEY = re.compile(r"file(?:new|old|s|name)?$", re.IGNORECASE)
ASSIGNMENT = re.compile(r"^\s*([^#;=]+?)\s*=\s*(.*?)\s*$")
# Only formats whose schema can point to additional input files belong here.
# BC files contain inline forcing metadata/data; the D-Flow FM BC schema has no
# file-valued header key, so scanning a large BC body is unnecessary.
REFERENCE_CONTAINER_FORMATS = {
    ".arl",
    ".cld",
    ".cmp",
    ".dad",
    ".ext",
    ".fou",
    ".ini",
    ".mdw",
    ".mdu",
    ".mor",
    ".sed",
    ".ttd",
    ".xml",
}
OUTPUT_KEYS = {"hisfile", "mapfile"}
SHAPEFILE_SIDECARS = {".cpg", ".dbf", ".prj", ".qpj", ".shx"}
REFERENCE_SUFFIXES = REFERENCE_CONTAINER_FORMATS | {
    ".asc",
    ".bc",
    ".bmp",
    ".csv",
    ".dat",
    ".dep",
    ".grd",
    ".nc",
    ".pol",
    ".pli",
    ".pliz",
    ".shp",
    ".tek",
    ".tim",
    ".xy",
    ".xyn",
    ".xyz",
}
COPY_BUFFER_SIZE = 8 * 1024 * 1024


@dataclass(frozen=True)
class MissingReference:
    referrer: Path
    key: str
    value: str


@dataclass(frozen=True)
class ExcludedReference:
    referrer: Path
    key: str
    value: str
    path: Path


class ModelCollector:
    def __init__(
        self,
        mdu: Path,
        exclude_patterns: Iterable[str] = (),
        on_scan: Callable[[int, int, Path], None] | None = None,
    ) -> None:
        self.mdu = mdu.resolve()
        self.run_directory = self.mdu.parent
        self.exclude_patterns = tuple(
            pattern.replace("\\", "/").lower() for pattern in exclude_patterns
        )
        self.on_scan = on_scan
        self._expansion_cache: dict[Path, tuple[Path, ...]] = {}
        self.paths_relative_to_parent = self._read_parent_relative_setting()
        self.files: set[Path] = set()
        self.file_sizes: dict[Path, int] = {}
        self.copied_files = 0
        self.copied_bytes = 0
        self.skipped_files = 0
        self.missing: set[MissingReference] = set()
        self.excluded: set[ExcludedReference] = set()

    def collect(self) -> set[Path]:
        pending = [self.mdu]
        while pending:
            path = pending.pop()
            path = path.resolve()
            if path in self.files:
                continue
            self.files.add(path)
            if self.on_scan:
                self.on_scan(len(self.files), len(pending), path)
            self._add_sidecars(path, pending)
            if path.suffix.lower() not in REFERENCE_CONTAINER_FORMATS:
                continue
            for referenced in self._references_in(path):
                if referenced not in self.files:
                    pending.append(referenced)
        return self.files

    def _read_parent_relative_setting(self) -> bool:
        for key, value in self._assignments(self.mdu):
            if key.lower() == "pathsrelativetoparent":
                return value.strip().lower() in {"1", "true", "yes"}
        return False

    def _references_in(self, path: Path) -> Iterable[Path]:
        for key, value in self._assignments(path):
            normalized_key = key.replace(" ", "").lower()
            if normalized_key in OUTPUT_KEYS or not (
                REFERENCE_KEY.search(normalized_key)
                or self._value_has_reference_suffix(value)
            ):
                continue
            for token in self._split_reference_value(value, path):
                resolved = self._resolve(token, path)
                matches = self._expand(resolved)
                if matches:
                    for match in matches:
                        if self._is_excluded(match):
                            self.excluded.add(ExcludedReference(path, key, token, match))
                        else:
                            yield match
                elif self._looks_like_path(token):
                    self.missing.add(MissingReference(path, key, token))

    def _is_excluded(self, path: Path) -> bool:
        normalized = str(path).replace("\\", "/").lower()
        return any(
            fnmatch.fnmatchcase(normalized, pattern) for pattern in self.exclude_patterns
        )

    def _resolve(self, token: str, referrer: Path) -> Path:
        candidate = Path(os.path.expandvars(os.path.expanduser(token)))
        if candidate.is_absolute():
            return candidate
        if referrer == self.mdu or self.paths_relative_to_parent:
            return referrer.parent / candidate
        return self.run_directory / candidate

    def _expand(self, path: Path) -> list[Path]:
        if path in self._expansion_cache:
            return list(self._expansion_cache[path])
        text = str(path)
        if any(character in text for character in "*?["):
            import glob

            matches = tuple(
                Path(match).resolve() for match in glob.glob(text) if Path(match).is_file()
            )
        else:
            matches = (path.resolve(),) if path.is_file() else ()
        self._expansion_cache[path] = matches
        return list(matches)

    def _split_reference_value(self, value: str, referrer: Path) -> list[str]:
        value = value.strip().strip('"\'')
        if not value:
            return []

        try:
            parts = [part.strip('"\'') for part in shlex.split(value, posix=False)]
        except ValueError:
            return [value]

        if len(parts) > 1:
            all_have_directories = all("/" in part or "\\" in part for part in parts)
            all_exist = all(
                self._expand(self._resolve(part, referrer)) for part in parts
            )
            if all_have_directories or all_exist:
                return parts
        if self._expand(self._resolve(value, referrer)):
            return [value]
        return parts

    @staticmethod
    def _looks_like_path(value: str) -> bool:
        value = value.rstrip(".,;")
        return bool(Path(value).suffix) or "/" in value or "\\" in value

    @staticmethod
    def _value_has_reference_suffix(value: str) -> bool:
        try:
            parts = shlex.split(value.strip(), posix=False)
        except ValueError:
            parts = [value]
        return any(
            Path(part.strip('"\'')).suffix.lower() in REFERENCE_SUFFIXES
            for part in parts
        )

    @staticmethod
    def _assignments(path: Path) -> Iterable[tuple[str, str]]:
        try:
            text = path.read_text(encoding="utf-8-sig", errors="replace")
        except OSError as error:
            raise RuntimeError(f"Could not read {path}: {error}") from error

        for line in text.splitlines():
            line = line.split("#", 1)[0].split(";", 1)[0]
            match = ASSIGNMENT.match(line)
            if match:
                yield match.group(1).strip(), match.group(2).strip()

    @staticmethod
    def _add_sidecars(path: Path, pending: list[Path]) -> None:
        if path.suffix.lower() != ".shp":
            return
        for suffix in SHAPEFILE_SIDECARS:
            sidecar = path.with_suffix(suffix)
            if sidecar.is_file():
                pending.append(sidecar)


def common_source_root(files: Iterable[Path]) -> Path:
    paths = [str(path) for path in files]
    try:
        return Path(os.path.commonpath(paths))
    except ValueError as error:
        raise ValueError(
            "Referenced files span multiple drives. Supply --source-root and move or "
            "rewrite references outside it first."
        ) from error


def format_size(size: int) -> str:
    value = float(size)
    for unit in ("B", "KiB", "MiB", "GiB", "TiB"):
        if value < 1024 or unit == "TiB":
            return f"{value:.2f} {unit}"
        value /= 1024
    raise AssertionError("unreachable")


def file_inventory(
    files: Iterable[Path], sizes: dict[Path, int] | None = None
) -> list[str]:
    sized_files = (
        (sizes[path] if sizes is not None else path.stat().st_size, path) for path in files
    )
    return [
        f"{format_size(size):>10}  {path}"
        for size, path in sorted(sized_files, key=lambda item: (-item[0], str(item[1])))
    ]


class ScanProgress:
    def __init__(self, enabled: bool) -> None:
        self.enabled = enabled

    def update(self, found: int, queued: int, path: Path) -> None:
        if not self.enabled:
            return
        filename = _short_filename(path)
        print(
            f"\rScanning references: {found} found, {queued} queued  {filename:<35}",
            end="",
            file=sys.stderr,
            flush=True,
        )

    def finish(self) -> None:
        if self.enabled:
            print(file=sys.stderr)


class SizingProgress:
    def __init__(self, total_files: int, enabled: bool) -> None:
        self.total_files = total_files
        self.enabled = enabled

    def update(self, file_number: int, path: Path) -> None:
        if not self.enabled:
            return
        fraction = file_number / self.total_files if self.total_files else 1.0
        width = 30
        filled = int(width * fraction)
        bar = "#" * filled + "-" * (width - filled)
        print(
            f"\rSizing files: [{bar}] {file_number}/{self.total_files}  "
            f"{_short_filename(path):<35}",
            end="",
            file=sys.stderr,
            flush=True,
        )

    def finish(self) -> None:
        if self.enabled:
            print(file=sys.stderr)


def _short_filename(path: Path) -> str:
    filename = path.name
    return filename if len(filename) <= 35 else "..." + filename[-32:]


class CopyProgress:
    def __init__(self, total_bytes: int, total_files: int, enabled: bool) -> None:
        self.total_bytes = total_bytes
        self.total_files = total_files
        self.enabled = enabled
        self.copied_bytes = 0
        self.file_number = 0
        self.current_file = ""

    def skip_file(self, path: Path, byte_count: int) -> None:
        self.file_number += 1
        self.current_file = f"{path.name} (exists)"
        self.copied_bytes += byte_count
        self._render()

    def start_file(self, path: Path) -> None:
        self.file_number += 1
        self.current_file = path.name
        self._render()

    def advance(self, byte_count: int) -> None:
        self.copied_bytes += byte_count
        self._render()

    def finish(self) -> None:
        if self.enabled:
            self._render()
            print(file=sys.stderr)

    def _render(self) -> None:
        if not self.enabled:
            return
        fraction = self.copied_bytes / self.total_bytes if self.total_bytes else 1.0
        fraction = min(fraction, 1.0)
        width = 30
        filled = int(width * fraction)
        bar = "#" * filled + "-" * (width - filled)
        filename = _short_filename(Path(self.current_file))
        print(
            f"\r[{bar}] {fraction:6.1%}  "
            f"{format_size(self.copied_bytes)} / {format_size(self.total_bytes)}  "
            f"file {self.file_number}/{self.total_files}  {filename:<35}",
            end="",
            file=sys.stderr,
            flush=True,
        )


def copy_file(source: Path, target: Path, on_chunk: Callable[[int], None]) -> None:
    with source.open("rb") as source_file, target.open("wb") as target_file:
        while chunk := source_file.read(COPY_BUFFER_SIZE):
            target_file.write(chunk)
            on_chunk(len(chunk))
    shutil.copystat(source, target)


def stage_model(
    mdu: Path,
    destination: Path,
    source_root: Path | None = None,
    dry_run: bool = False,
    exclude_patterns: Iterable[str] = (),
    show_progress: bool = False,
    overwrite: bool = False,
) -> tuple[ModelCollector, Path]:
    scan_progress = ScanProgress(show_progress)
    collector = ModelCollector(mdu, exclude_patterns, scan_progress.update)
    files = collector.collect()
    scan_progress.finish()
    source_root = source_root.resolve() if source_root else common_source_root(files)
    destination = destination.resolve()

    try:
        destination.relative_to(source_root)
    except ValueError:
        pass
    else:
        raise ValueError("Destination must not be inside the source model tree.")

    sizing_progress = SizingProgress(len(files), show_progress)
    for file_number, path in enumerate(sorted(files), start=1):
        sizing_progress.update(file_number, path)
        collector.file_sizes[path] = path.stat().st_size
    sizing_progress.finish()

    copies: list[dict[str, str]] = []
    total_bytes = sum(collector.file_sizes.values())
    progress = CopyProgress(total_bytes, len(files), show_progress and not dry_run)
    copied_files = 0
    skipped_files = 0
    for source in sorted(files):
        try:
            relative = source.relative_to(source_root)
        except ValueError as error:
            raise ValueError(f"Referenced file is outside --source-root: {source}") from error
        target = destination / relative
        source_size = collector.file_sizes[source]
        target_matches = (
            not overwrite and target.is_file() and target.stat().st_size == source_size
        )
        status = "skipped" if target_matches else "copy"
        copies.append({"source": str(source), "target": str(target), "status": status})
        if target_matches:
            skipped_files += 1
        else:
            copied_files += 1
            collector.copied_bytes += source_size
        if not dry_run:
            if target_matches:
                progress.skip_file(source, source_size)
            else:
                target.parent.mkdir(parents=True, exist_ok=True)
                progress.start_file(source)
                copy_file(source, target, progress.advance)
    progress.finish()
    collector.copied_files = copied_files
    collector.skipped_files = skipped_files

    manifest = {
        "mdu": str(collector.mdu),
        "source_root": str(source_root),
        "destination": str(destination),
        "paths_relative_to_parent": collector.paths_relative_to_parent,
        "copied_files": copied_files,
        "skipped_files": skipped_files,
        "files": copies,
        "missing_references": [
            {"referrer": str(item.referrer), "key": item.key, "value": item.value}
            for item in sorted(collector.missing, key=lambda item: (str(item.referrer), item.key, item.value))
        ],
        "excluded_references": [
            {
                "referrer": str(item.referrer),
                "key": item.key,
                "value": item.value,
                "path": str(item.path),
            }
            for item in sorted(
                collector.excluded,
                key=lambda item: (str(item.path), str(item.referrer), item.key),
            )
        ],
    }
    if not dry_run:
        destination.mkdir(parents=True, exist_ok=True)
        (destination / "stage_manifest.json").write_text(
            json.dumps(manifest, indent=2), encoding="utf-8"
        )
    return collector, source_root


def parse_arguments() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description=(
            "Recursively collect and copy files referenced by a D-Flow FM MDU "
            "while preserving their directory structure."
        ),
        epilog=(
            "examples:\n"
            "  Preview files, sizes, and unresolved references:\n"
            "    stage_dflowfm_model.py model.mdu D:\\models\\case --dry-run\n\n"
            "  Copy or resume a previously interrupted staging run:\n"
            "    stage_dflowfm_model.py model.mdu D:\\models\\case\n\n"
            "  Omit meteo inputs intentionally (references remain in copied inputs):\n"
            "    stage_dflowfm_model.py model.mdu D:\\models\\case "
            "--exclude \"*/meteo/*\" --allow-missing\n\n"
            "  Refresh every destination file regardless of size:\n"
            "    stage_dflowfm_model.py model.mdu D:\\models\\case --overwrite"
        ),
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    parser.add_argument("mdu", type=Path, help="Source D-Flow FM .mdu file")
    parser.add_argument(
        "destination",
        type=Path,
        help=(
            "Local staging root; existing same-sized files are skipped, and a "
            "stage_manifest.json is written here"
        ),
    )
    parser.add_argument(
        "--source-root",
        type=Path,
        help=(
            "Source directory whose relative layout is reproduced below destination "
            "(default: common ancestor of all discovered files)"
        ),
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help=(
            "Scan only; print a largest-first file inventory and planned skips without "
            "copying or writing a manifest"
        ),
    )
    parser.add_argument(
        "--no-progress",
        action="store_true",
        help="Disable live scan, sizing, and copy progress output",
    )
    parser.add_argument(
        "--overwrite",
        action="store_true",
        help="Copy all files even when a same-sized destination file already exists",
    )
    parser.add_argument(
        "--exclude",
        action="append",
        default=[],
        metavar="GLOB",
        help=(
            "Do not copy references whose case-insensitive normalized full path matches "
            "GLOB; repeat for multiple patterns. Copied input files retain excluded "
            "references and may need local editing (example: */meteo/ECMWF_2013-2017/*)"
        ),
    )
    parser.add_argument(
        "--allow-missing",
        action="store_true",
        help=(
            "Exit successfully after reporting unresolved path-like references; without "
            "this option the exit status is 2"
        ),
    )
    return parser.parse_args()


def main() -> int:
    arguments = parse_arguments()
    try:
        collector, source_root = stage_model(
            arguments.mdu,
            arguments.destination,
            arguments.source_root,
            arguments.dry_run,
            arguments.exclude,
            not arguments.no_progress,
            arguments.overwrite,
        )
    except (OSError, RuntimeError, ValueError) as error:
        print(f"error: {error}", file=sys.stderr)
        return 1

    total_bytes = sum(collector.file_sizes.values())
    action = "Would transfer" if arguments.dry_run else "Transferred"
    print(
        f"{action} {collector.copied_files} of {len(collector.files)} files "
        f"({format_size(collector.copied_bytes)})"
    )
    if collector.skipped_files:
        verb = "Would skip" if arguments.dry_run else "Skipped"
        print(f"{verb} {collector.skipped_files} existing same-sized files")
    print(f"Source root: {source_root}")
    print(f"Staged MDU: {arguments.destination.resolve() / collector.mdu.relative_to(source_root)}")
    if arguments.dry_run:
        print("Files (largest first):")
        for line in file_inventory(collector.files, collector.file_sizes):
            print(f"  {line}")
    if collector.excluded:
        print(f"Intentionally excluded references: {len(collector.excluded)}")
        for excluded in sorted(collector.excluded, key=lambda item: str(item.path)):
            print(f"  {excluded.path}")
        print("Warning: copied input files retain these references; edit them locally before running.")
    if collector.missing:
        print(f"Unresolved path-like references: {len(collector.missing)}", file=sys.stderr)
        for missing in sorted(collector.missing, key=lambda item: str(item.referrer)):
            print(f"  {missing.referrer}: {missing.key} = {missing.value}", file=sys.stderr)
        if not arguments.allow_missing:
            return 2
    return 0


if __name__ == "__main__":
    raise SystemExit(main())