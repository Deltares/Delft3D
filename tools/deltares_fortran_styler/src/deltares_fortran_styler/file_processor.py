"""
file_processor.py - File processing orchestration for Fortran style converters

This module handles:
1. Reading and writing Fortran files
2. Running multiple converters on files
3. Collecting and reporting conversion statistics
4. Managing check vs. convert modes
"""

import time
from pathlib import Path
from typing import List, Tuple, Optional, Dict

try:
    from deltares_fortran_styler.base_converter import FortranConverter
except ImportError:
    from base_converter import FortranConverter


class FileProcessor:
    """
    Orchestrates multiple style converters on Fortran files.

    This class provides a clean interface for running style conversions
    on individual files or directories, with support for both check and
    convert modes.
    """

    def __init__(self, converters: List[FortranConverter]):
        """
        Initialize the file processor with a list of converters.

        Args:
            converters: List of FortranConverter instances to apply
        """
        self.converters = converters
        self.timing_stats: Dict[str, float] = {converter.get_name(): 0.0 for converter in converters}

    def process_file(self, file_path: Path, check_mode: bool = False) -> Tuple[bool, Dict]:
        """
        Process a single Fortran file with all converters.

        Args:
            file_path: Path to the Fortran file
            check_mode: If True, only check for issues without modifying

        Returns:
            Tuple of (needs_conversion, stats_dict)
            where needs_conversion is True if changes are needed/were made
            and stats_dict contains statistics from all converters
        """
        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                content = f.read()
        except Exception as e:
            print(f"Error reading {file_path}: {e}")
            return False, {}

        if check_mode:
            return self._check_file_content(file_path, content)
        else:
            return self._convert_file_content(file_path, content)

    def _check_file_content(self, file_path: Path, content: str) -> Tuple[bool, Dict]:
        """
        Check file content for style issues without modifying.

        Returns:
            Tuple of (has_issues, stats_dict)
        """
        all_issues = []
        stats = {}

        # Run each converter's check
        for converter in self.converters:
            start_time = time.perf_counter()
            issues = converter.check_text(content)
            elapsed = time.perf_counter() - start_time

            # Accumulate timing for this converter
            self.timing_stats[converter.get_name()] += elapsed

            all_issues.extend(issues)

            # Get stats if converter provides them (optional method)
            if hasattr(converter, 'get_conversion_stats'):
                converter_stats = converter.get_conversion_stats(content)  # type: ignore
                stats[converter.get_name()] = converter_stats

        # Report all issues
        if all_issues:
            # Sort by line number for cleaner output
            all_issues.sort(key=lambda x: x.line_number)

            for issue in all_issues:
                print(f"{file_path}({issue.line_number}): error {issue.error_code}: {issue.message}")

            # Add a note about auto-fixing
            print(f"{file_path}(1): note: Run 'python tools/deltares_fortran_styler/src/deltares_fortran_styler/fortran_styler.py \"{file_path}\"' to automatically fix these errors")

            return True, stats

        return False, stats

    def _convert_file_content(self, file_path: Path, content: str) -> Tuple[bool, Dict]:
        """
        Convert file content with all converters and write back.

        Returns:
            Tuple of (was_converted, stats_dict)
        """
        any_changes = False
        stats = {}

        # Apply each converter sequentially
        for converter in self.converters:
            # Get stats before conversion (optional method)
            if hasattr(converter, 'get_conversion_stats'):
                converter_stats = converter.get_conversion_stats(content)  # type: ignore
                if any(converter_stats.values()):  # Only store if there are conversions
                    stats[converter.get_name()] = converter_stats

            # Apply conversion
            content, was_converted = converter.convert_text(content)

            if was_converted:
                any_changes = True
                # Add any required imports
                if converter.needs_import():
                    content = converter.add_required_imports(content, was_converted)

        # Only write if there were changes
        if any_changes:
            try:
                with open(file_path, 'w', encoding='utf-8') as f:
                    f.write(content)

                # Report what was converted
                self._report_conversions(file_path, stats)

                return True, stats
            except Exception as e:
                print(f"Error writing {file_path}: {e}")
                return False, stats
        else:
            print(f"No conversions needed in {file_path}")
            return False, stats

    def _report_conversions(self, file_path: Path, stats: Dict):
        """Report conversion statistics for a file."""
        print(f"Converted {file_path}:")

        for converter_name, converter_stats in stats.items():
            if converter_stats:
                details = ', '.join(f"{count} {name}" for name, count in converter_stats.items() if count > 0)
                if details:
                    print(f"  [{converter_name}] {details}")

    def process_directory(self, directory: Path,
                         extensions: Optional[List[str]] = None,
                         check_mode: bool = False) -> Tuple[int, int]:
        """
        Process all Fortran files in a directory recursively.

        Args:
            directory: Path to the directory
            extensions: List of file extensions to process (default: Fortran extensions)
            check_mode: If True, only check without modifying

        Returns:
            Tuple of (files_processed, files_with_issues_or_changes)
        """
        if extensions is None:
            extensions = ['.f90', '.f95', '.f03', '.f08', '.F90', '.F95', '.F03', '.F08']

        files = self._get_fortran_files(directory, extensions)

        if not files:
            print(f"No Fortran files found in {directory}")
            return 0, 0

        files_processed = 0
        files_with_changes = 0

        for file_path in files:
            files_processed += 1
            needs_change, _ = self.process_file(file_path, check_mode)
            if needs_change:
                files_with_changes += 1

        return files_processed, files_with_changes

    def _get_fortran_files(self, directory: Path, extensions: List[str]) -> List[Path]:
        """
        Get all unique Fortran files in a directory, avoiding duplicates.

        This handles case-insensitive filesystems where .f90 and .F90
        might refer to the same file.
        """
        processed_files = set()
        unique_files = []

        for ext in extensions:
            for file_path in directory.rglob(f"*{ext}"):
                # Use resolved path to handle case-insensitive duplicates
                resolved_path = file_path.resolve()
                if resolved_path in processed_files:
                    continue
                processed_files.add(resolved_path)
                unique_files.append(file_path)

        return unique_files

    def validate_file(self, file_path: Path) -> bool:
        """
        Validate that a file exists and is readable.

        Args:
            file_path: Path to validate

        Returns:
            True if file is valid, False otherwise (with error message printed)
        """
        if file_path.is_dir():
            print(f"{file_path}(1): error: Directory specified. Use --directory flag to process directories.")
            return False

        if not file_path.exists():
            print(f"{file_path}(1): error: File does not exist.")
            return False

        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                f.read()
            return True
        except PermissionError:
            print(f"{file_path}(1): error: Permission denied accessing file.")
            return False
        except UnicodeDecodeError:
            print(f"{file_path}(1): error: Cannot decode file as UTF-8 (binary file?).")
            return False
        except Exception as e:
            print(f"{file_path}(1): error: Unexpected error validating file: {e}")
            return False

    def get_timing_stats(self) -> Dict[str, float]:
        """
        Get timing statistics for each converter.

        Returns:
            Dictionary mapping converter names to accumulated time in seconds
        """
        return self.timing_stats.copy()

    def reset_timing_stats(self):
        """Reset timing statistics for all converters."""
        for converter in self.converters:
            self.timing_stats[converter.get_name()] = 0.0
