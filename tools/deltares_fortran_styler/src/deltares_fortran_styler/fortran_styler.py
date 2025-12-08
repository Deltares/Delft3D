"""
fortran_styler.py - Main entry point for the Fortran style checker and converter

This script provides a modular framework for checking and converting Fortran code
according to the Deltares style guide. It supports:
- Multiple independent style converters
- Check mode for CI/CD pipelines
- Convert mode for automatic fixing
- Directory and individual file processing

Usage:
    # Convert files in-place
    python fortran_styler.py file1.f90 file2.f90
    python fortran_styler.py --directory path/to/fortran/files

    # Check mode - validate without converting
    python fortran_styler.py --check file1.f90 file2.f90
    python fortran_styler.py --check --directory path/to/fortran/files

    # Enable/disable specific converters
    python fortran_styler.py --converters double_precision array_delimiter file.f90
"""

import sys
import argparse
from pathlib import Path
from typing import List

# Support both installed package and direct script execution
try:
    from deltares_fortran_styler.file_processor import FileProcessor
    from deltares_fortran_styler.double_precision_converter import DoublePrecisionConverter
    from deltares_fortran_styler.array_delimiter_converter import ArrayDelimiterConverter
    from deltares_fortran_styler.semicolon_separator_converter import SemicolonSeparatorConverter
except ImportError:
    from file_processor import FileProcessor
    from double_precision_converter import DoublePrecisionConverter
    from array_delimiter_converter import ArrayDelimiterConverter
    from semicolon_separator_converter import SemicolonSeparatorConverter


# Registry of available converters
AVAILABLE_CONVERTERS = {
    'double_precision': DoublePrecisionConverter,
    'array_delimiter': ArrayDelimiterConverter,
    'semicolon_separator': SemicolonSeparatorConverter,
}


def get_converters(converter_names: List[str]):
    """
    Instantiate the requested converters.

    Args:
        converter_names: List of converter names to enable

    Returns:
        List of instantiated converter objects
    """
    converters = []
    for name in converter_names:
        if name not in AVAILABLE_CONVERTERS:
            print(f"Warning: Unknown converter '{name}'. Available: {', '.join(AVAILABLE_CONVERTERS.keys())}")
            continue
        converters.append(AVAILABLE_CONVERTERS[name]())
    return converters


def main():
    parser = argparse.ArgumentParser(
        description="Fortran style checker and converter for Deltares style guide",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  python fortran_styler.py input.f90 another.f90
  python fortran_styler.py --directory src/
  python fortran_styler.py --check input.f90
  python fortran_styler.py --check --directory src/
  python fortran_styler.py --converters double_precision --directory src/

Available converters:
  double_precision    - Convert double precision literals and declarations
  array_delimiter     - Convert old-style array constructors (/ /) to [...]
  semicolon_separator - Split semicolon-separated statements onto separate lines

By default, all converters are enabled. Use --converters to specify a subset.

Note: All conversions are done in-place. Use git for version control safety.
        """
    )

    parser.add_argument('files', nargs='*', help='Input Fortran files to process')
    parser.add_argument('-d', '--directory', type=Path,
                       help='Process all Fortran files in directory recursively')
    parser.add_argument('-c', '--check', action='store_true',
                       help='Check if files need conversion without modifying them (returns error code if conversion needed)')
    parser.add_argument('--extensions', nargs='+',
                       default=['.f90', '.f95', '.f03', '.f08', '.F90', '.F95', '.F03', '.F08'],
                       help='File extensions to process (default: Fortran extensions)')
    parser.add_argument('--converters', nargs='+',
                       choices=list(AVAILABLE_CONVERTERS.keys()),
                       help='Specify which converters to enable (default: all)')

    args = parser.parse_args()

    # Determine which converters to use
    if args.converters:
        converter_names = args.converters
    else:
        # Default: enable all converters
        converter_names = list(AVAILABLE_CONVERTERS.keys())

    converters = get_converters(converter_names)

    if not converters:
        print("Error: No valid converters enabled")
        return 1

    # Create file processor with the selected converters
    processor = FileProcessor(converters)

    # Process files or directory
    if args.check:
        # Check mode - don't convert, just report what needs conversion
        conversion_needed = False

        if args.directory:
            print(f"Checking directory: {args.directory}")
            files_processed, files_needing_conversion = processor.process_directory(
                args.directory, args.extensions, check_mode=True
            )
            print(f"Checked {files_processed} files, {files_needing_conversion} need conversion")
            if files_needing_conversion > 0:
                conversion_needed = True

        elif args.files:
            for file_str in args.files:
                file_path = Path(file_str)
                if not processor.validate_file(file_path):
                    conversion_needed = True
                    continue
                needs_conversion, _ = processor.process_file(file_path, check_mode=True)
                if needs_conversion:
                    conversion_needed = True

        else:
            parser.print_help()
            return 1

        if conversion_needed:
            print("\nStyle check failed: Issues found that need conversion. See error list above.")
            return 1
        else:
            print("All files conform to style guide.")
            return 0

    else:
        # Normal conversion mode
        if args.directory:
            print(f"Processing directory: {args.directory}")
            files_processed, files_converted = processor.process_directory(
                args.directory, args.extensions, check_mode=False
            )
            print(f"Processed {files_processed} files, converted {files_converted} files")

        elif args.files:
            for file_str in args.files:
                file_path = Path(file_str)
                if not processor.validate_file(file_path):
                    continue
                processor.process_file(file_path, check_mode=False)

        else:
            parser.print_help()
            return 1

        return 0


if __name__ == "__main__":
    sys.exit(main())
