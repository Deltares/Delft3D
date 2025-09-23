"""
convert_double.py - A script to convert Fortran double precision literals and declarations

This script converts:
1. Fortran double precision literals from `1d0` to `1.0_dp` format
2. Variable declarations from `double precision :: var` to `real(kind=dp) :: var`

It focuses on literal constants with exponent-letter 'D' as specified in the Fortran standard.

All conversions are done in-place. Use git for version control safety.

Usage:
    # Convert files in-place
    python convert_double.py file1.f90 file2.f90
    python convert_double.py --directory path/to/fortran/files

    # Check mode - validate without converting (returns error code if conversion needed)
    python convert_double.py --check file1.f90 file2.f90
    python convert_double.py --check --directory path/to/fortran/files

Check mode is useful for CI/CD pipelines or pre-commit hooks to ensure code is already converted.
Returns exit code 0 if no conversion needed, exit code 1 if conversion is required.
"""

import sys
import re
import argparse
from pathlib import Path
from typing import List, Tuple, Optional

class FortranDoubleConverter:
    """
    Converter for Fortran double precision literals and declarations from D format to _dp format.

    Based on Fortran standard syntax:
    signed-real-literal-constant : [ sign ] real-literal-constant
    real-literal-constant : significand [ exponent-letter exponent ] [ '_' kind-param ]
                            or digit-string exponent-letter exponent [ '_' kind-param ]
    significand : digit-string '.' [ digit-string ]
                  or '.' digit-string
    exponent-letter : E or D
    exponent : signed-digit-string

    We focus on exponent-letter being D and double precision declarations.
    """

    def __init__(self):
        # Pattern to match Fortran double precision literals with D exponent
        # This matches patterns like: 1d0, 1.0d0, .5d0, 123d-5, 0.123d+10, etc.
        self.d_literal_pattern = re.compile(
            r'''
            (?<!\w)                     # Not preceded by word character (negative lookbehind)
            (?P<sign>[+-]?)             # Optional sign
            (?P<significand>
                (?:\.\d+)               # . digit-string (e.g., .5, .123)
                |                       # OR
                (?:\d+\.?\d*)           # digit-string . [digit-string] (e.g., 1.0, 1., 123.456)
                |                       # OR
                (?:\d+)                 # digit-string only (e.g., 123) - for cases like 123d0
            )
            [dD]                        # Exponent letter D or d
            (?P<exponent>[+-]?\d+)      # Signed exponent
            (?!_)                       # Not followed by underscore (avoid already converted)
            (?!\w)                      # Not followed by word character (negative lookahead)
            ''',
            re.VERBOSE
        )

        # Pattern to match double precision variable declarations
        # Matches any line starting with double precision
        self.double_precision_pattern = re.compile(
            r'''
            (?P<indent>\s*)             # Capture leading whitespace
            double\s+precision          # "double precision" keywords
            (?P<rest>.*)                # Everything else on the line
            ''',
            re.VERBOSE | re.IGNORECASE
        )

        # Pattern to detect if we're inside a comment or string
        self.comment_pattern = re.compile(r'!.*$', re.MULTILINE)
        self.string_patterns = [
            re.compile(r"'([^']*(?:''[^']*)*)'"),  # Single quotes
            re.compile(r'"([^"]*(?:""[^"]*)*)"'),  # Double quotes
        ]

        # Pattern to find module declaration
        self.module_pattern = re.compile(r'^(\s*module\s+\w+\s*)$', re.MULTILINE | re.IGNORECASE)

        # Pattern to find existing precision import
        self.precision_import_pattern = re.compile(
            r'^\s*use\s+precision\b',
            re.MULTILINE | re.IGNORECASE
        )

        # Pattern to find the end of use statements section
        self.use_section_end_pattern = re.compile(
            r'^\s*(?:implicit\s+none|contains|private|public|integer|real|character|logical|type|interface)\s*',
            re.MULTILINE | re.IGNORECASE
        )

    def _is_in_string_or_comment(self, text: str, pos: int) -> bool:
        """Check if position is inside a string literal or comment."""
        # Check for comments first (everything after ! to end of line)
        lines = text[:pos].split('\n')
        current_line = lines[-1] if lines else ""
        comment_pos = current_line.find('!')
        if comment_pos != -1:
            pos_in_line = len(current_line)
            if pos_in_line > comment_pos:
                return True

        # Check for string literals
        for pattern in self.string_patterns:
            for match in pattern.finditer(text):
                if match.start() <= pos <= match.end():
                    return True

        return False

    def _convert_literal(self, match) -> str:
        """Convert a single D literal to _dp format."""
        sign = match.group('sign')
        significand = match.group('significand')
        exponent = match.group('exponent')

        # Handle significand formatting
        if '.' not in significand:
            # Add .0 if no decimal point (e.g., 1d0 -> 1.0)
            significand = significand + '.0'
        elif significand.endswith('.'):
            # Add 0 after trailing decimal (e.g., 1. -> 1.0)
            significand = significand + '0'
        elif significand.startswith('.'):
            # Add 0 before leading decimal (e.g., .5 -> 0.5)
            significand = '0' + significand

        # Convert D exponent to E exponent and add _dp kind
        if exponent == '0':
            # Simple case: no exponent needed
            return f"{sign}{significand}_dp"
        else:
            # Include exponent with E
            return f"{sign}{significand}e{exponent}_dp"

    def _convert_double_precision_declaration(self, match) -> str:
        """Convert a double precision declaration to real(kind=dp) format."""
        indent = match.group('indent')
        rest = match.group('rest').strip()

        # Build the new declaration
        return f"{indent}real(kind=dp) {rest}"

    def _add_precision_import(self, text: str) -> str:
        """Add 'use precision, only: dp' import if not already present."""
        # Check if precision import already exists
        if self.precision_import_pattern.search(text):
            return text

        # Find module declaration
        module_match = self.module_pattern.search(text)
        if not module_match:
            # No module found, return text unchanged
            return text

        # Find where to insert the use statement
        module_end = module_match.end()

        # Look for existing use statements after the module declaration
        remaining_text = text[module_end:]
        lines = remaining_text.split('\n')

        insert_line_idx = 0
        use_statements_found = False

        for i, line in enumerate(lines):
            stripped_line = line.strip()

            # Skip empty lines and comments at the beginning
            if not stripped_line or stripped_line.startswith('!'):
                continue

            # Check if this is a use statement
            if re.match(r'^\s*use\s+', line, re.IGNORECASE):
                use_statements_found = True
                insert_line_idx = i + 1  # Insert after this use statement
                continue

            # If we've found use statements and now hit something else, insert here
            if use_statements_found:
                insert_line_idx = i
                break

            # If this is 'implicit none' or other declaration, insert before it
            if self.use_section_end_pattern.match(line):
                insert_line_idx = i
                break

            # If we haven't found any use statements yet, this might be the first declaration
            insert_line_idx = i
            break

        # Determine indentation (use the same as surrounding use statements or module)
        indent = "   "  # Default indentation
        if use_statements_found and insert_line_idx > 0:
            # Use indentation from previous use statement
            prev_line = lines[insert_line_idx - 1]
            indent = prev_line[:len(prev_line) - len(prev_line.lstrip())]
        elif insert_line_idx < len(lines):
            # Use indentation from the next line
            next_line = lines[insert_line_idx]
            if next_line.strip():
                indent = next_line[:len(next_line) - len(next_line.lstrip())]

        # Insert the precision import
        precision_import = f"{indent}use precision, only: dp"

        # Reconstruct the text
        before_module = text[:module_end]
        if insert_line_idx == 0:
            # Insert right after module declaration
            after_insert = '\n'.join(lines)
            new_text = before_module + '\n' + precision_import + '\n' + after_insert
        else:
            before_insert = '\n'.join(lines[:insert_line_idx])
            after_insert = '\n'.join(lines[insert_line_idx:])
            new_text = before_module + '\n' + before_insert + '\n' + precision_import + '\n' + after_insert

        return new_text

    def convert_text(self, text: str) -> Tuple[str, bool]:
        """Convert all D literals and double precision declarations in the given text. Returns (converted_text, was_converted)."""
        original_text = text
        conversions_made = False

        # Convert D literals
        def replace_literal_match(match):
            # Check if this match is inside a string or comment
            if self._is_in_string_or_comment(text, match.start()):
                return match.group(0)  # Return unchanged
            return self._convert_literal(match)

        text = self.d_literal_pattern.sub(replace_literal_match, text)
        if text != original_text:
            conversions_made = True

        # Convert double precision declarations
        def replace_declaration_match(match):
            # Check if this match is inside a string or comment
            if self._is_in_string_or_comment(text, match.start()):
                return match.group(0)  # Return unchanged
            return self._convert_double_precision_declaration(match)

        original_after_literals = text
        text = self.double_precision_pattern.sub(replace_declaration_match, text)
        if text != original_after_literals:
            conversions_made = True

        # If any conversions were made, add precision import
        if conversions_made:
            text = self._add_precision_import(text)

        return text, conversions_made

    def convert_file(self, input_path: Path) -> bool:
        """Convert a single file in-place."""
        try:
            with open(input_path, 'r', encoding='utf-8') as f:
                original_content = f.read()

            converted_content, was_converted = self.convert_text(original_content)

            # Only write if there were changes
            if was_converted:
                with open(input_path, 'w', encoding='utf-8') as f:
                    f.write(converted_content)

                # Count conversions for reporting
                original_literal_matches = len(self.d_literal_pattern.findall(original_content))
                converted_literal_matches = len(self.d_literal_pattern.findall(converted_content))
                literal_conversions = original_literal_matches - converted_literal_matches

                original_declaration_matches = len(self.double_precision_pattern.findall(original_content))
                converted_declaration_matches = len(self.double_precision_pattern.findall(converted_content))
                declaration_conversions = original_declaration_matches - converted_declaration_matches

                print(f"Converted {literal_conversions} literals and {declaration_conversions} declarations in {input_path}")

                # Check if precision import was added
                if not self.precision_import_pattern.search(original_content) and \
                   self.precision_import_pattern.search(converted_content):
                    print("  Added 'use precision, only: dp' import")

                return True
            else:
                print(f"No conversions needed in {input_path}")
                return False

        except Exception as e:
            print(f"Error processing {input_path}: {e}")
            return False

    def _get_fortran_files(self, directory: Path, extensions: Optional[List[str]] = None) -> List[Path]:
        """Get all unique Fortran files in a directory, avoiding duplicates from case-insensitive extensions."""
        if extensions is None:
            extensions = ['.f90', '.f95', '.f03', '.f08', '.F90', '.F95', '.F03', '.F08']

        processed_files = set()  # Track processed files to avoid duplicates
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

    def convert_directory(self, directory: Path, extensions: Optional[List[str]] = None) -> Tuple[int, int]:
        """Convert all Fortran files in a directory in-place."""
        files_processed = 0
        files_converted = 0

        for file_path in self._get_fortran_files(directory, extensions):
            files_processed += 1
            if self.convert_file(file_path):
                files_converted += 1

        return files_processed, files_converted

    def check_file(self, input_path: Path) -> bool:
        """Check if a file needs conversion without modifying it. Returns True if conversion is needed."""
        try:
            if input_path.is_dir():
                print(f"ERROR: {input_path} is a directory. Use --directory flag to process directories.")
                return False

            if not input_path.exists():
                print(f"ERROR: {input_path} does not exist.")
                return False

            with open(input_path, 'r', encoding='utf-8') as f:
                content = f.read()

            _, needs_conversion = self.convert_text(content)

            if needs_conversion:
                # Count what would be converted for reporting
                literal_matches = []
                for match in self.d_literal_pattern.finditer(content):
                    if not self._is_in_string_or_comment(content, match.start()):
                        literal_matches.append(match)

                declaration_matches = []
                for match in self.double_precision_pattern.finditer(content):
                    if not self._is_in_string_or_comment(content, match.start()):
                        declaration_matches.append(match)

                print(f"ERROR: {input_path} needs conversion:")
                if literal_matches:
                    print(f"  - {len(literal_matches)} double precision literals found")
                if declaration_matches:
                    print(f"  - {len(declaration_matches)} double precision declarations found")

                return True

            return False

        except PermissionError:
            print(f"ERROR: Permission denied accessing {input_path}")
            return False
        except UnicodeDecodeError:
            print(f"ERROR: Cannot decode {input_path} as UTF-8 (binary file?)")
            return False
        except Exception as e:
            print(f"ERROR: Unexpected error checking {input_path}: {e}")
            return False

    def check_directory(self, directory: Path, extensions: Optional[List[str]] = None) -> Tuple[int, int]:
        """Check all Fortran files in a directory for needed conversions. Returns (files_processed, files_needing_conversion)."""
        files_processed = 0
        files_needing_conversion = 0

        for file_path in self._get_fortran_files(directory, extensions):
            files_processed += 1
            if self.check_file(file_path):
                files_needing_conversion += 1

        return files_processed, files_needing_conversion

def main():
    parser = argparse.ArgumentParser(
        description="Convert Fortran double precision literals and declarations (in-place)",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  python convert_double.py input.f90 another.f90
  python convert_double.py --directory src/
  python convert_double.py --check input.f90
  python convert_double.py --check --directory src/

Conversions performed:
  - Literals: 1d0 -> 1.0_dp, 2.5d-3 -> 2.5e-3_dp, etc.
  - Declarations: double precision :: var -> real(kind=dp) :: var

Note: All conversions are done in-place. Use git for version control safety.
The script automatically adds 'use precision, only: dp' when conversions are made.
        """
    )

    parser.add_argument('files', nargs='*', help='Input Fortran files to convert in-place')
    parser.add_argument('-d', '--directory', type=Path,
                       help='Process all Fortran files in directory recursively (in-place)')
    parser.add_argument('-c', '--check', action='store_true',
                       help='Check if files need conversion without modifying them (returns error code if conversion needed)')
    parser.add_argument('--extensions', nargs='+',
                       default=['.f90', '.f95', '.f03', '.f08', '.F90', '.F95', '.F03', '.F08'],
                       help='File extensions to process (default: Fortran extensions)')

    args = parser.parse_args()

    converter = FortranDoubleConverter()

    if args.check:
        # Check mode - don't convert, just report what needs conversion
        conversion_needed = False

        if args.directory:
            print(f"Checking directory: {args.directory}")
            files_processed, files_needing_conversion = converter.check_directory(
                args.directory, args.extensions
            )
            print(f"Checked {files_processed} files, {files_needing_conversion} need conversion")
            if files_needing_conversion > 0:
                conversion_needed = True

        elif args.files:
            for file_str in args.files:
                input_path = Path(file_str)
                if converter.check_file(input_path):
                    conversion_needed = True

        else:
            parser.print_help()
            return 1

        if conversion_needed:
            print("\nERROR: Files found that need double precision conversion!")
            return 1
        else:
            print("All files are already converted.")
            return 0

    else:
        # Normal conversion mode
        if args.directory:
            print(f"Processing directory: {args.directory}")
            files_processed, files_converted = converter.convert_directory(
                args.directory, args.extensions
            )
            print(f"Processed {files_processed} files, converted {files_converted} files")

        elif args.files:
            for file_str in args.files:
                input_path = Path(file_str)
                converter.convert_file(input_path)

        else:
            parser.print_help()
            return 1

        return 0

if __name__ == "__main__":
    sys.exit(main())
