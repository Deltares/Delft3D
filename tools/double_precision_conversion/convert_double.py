"""
convert_double.py - A script to convert Fortran double precision literals and declarations

This script converts:
1. Fortran double precision literals from `1d0` to `1.0_dp` format
2. Variable declarations from `double precision :: var` to `real(kind=dp) :: var`
3. dble() function calls from `dble(x)` to `real(x, kind=dp)`

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
        self.string_patterns = [
            re.compile(r"'([^']*(?:''[^']*)*)'"),  # Single quotes
            re.compile(r'"([^"]*(?:""[^"]*)*)"'),  # Double quotes
        ]

        # Pattern to find module declaration
        self.module_pattern = re.compile(r'^\s*module\s+\w+', re.MULTILINE | re.IGNORECASE)

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

        # Pattern to match dble() function calls
        # Matches dble(expression) where expression can contain nested parentheses
        self.dble_pattern = re.compile(
            r'\bdble\s*\(',
            re.IGNORECASE
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

    def _convert_dble_calls(self, text: str) -> str:
        """Convert dble(expr) to real(expr, kind=dp)."""
        result = []
        i = 0
        while i < len(text):
            # Check if we're at the start of a dble call
            match = self.dble_pattern.match(text, i)
            if match and not self._is_in_string_or_comment(text, match.start()):
                # Found dble( - need to find matching closing parenthesis
                paren_start = match.end() - 1  # Position of opening (
                paren_count = 1
                j = paren_start + 1

                # Find the matching closing parenthesis
                while j < len(text) and paren_count > 0:
                    if text[j] == '(' and not self._is_in_string_or_comment(text, j):
                        paren_count += 1
                    elif text[j] == ')' and not self._is_in_string_or_comment(text, j):
                        paren_count -= 1
                    j += 1

                if paren_count == 0:
                    # Successfully found matching parenthesis
                    inner_expr = text[paren_start + 1:j - 1]
                    result.append(f"real({inner_expr}, kind=dp)")
                    i = j
                else:
                    # Unmatched parenthesis, keep original
                    result.append(text[i])
                    i += 1
            else:
                result.append(text[i])
                i += 1

        return ''.join(result)

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
        # Capture the current text for this stage
        text_for_literal_check = text
        def replace_literal_match(match):
            # Check if this match is inside a string or comment
            if self._is_in_string_or_comment(text_for_literal_check, match.start()):
                return match.group(0)  # Return unchanged
            return self._convert_literal(match)

        text = self.d_literal_pattern.sub(replace_literal_match, text)
        if text != original_text:
            conversions_made = True

        # Convert double precision declarations
        # Capture the current text for this stage
        text_for_declaration_check = text
        def replace_declaration_match(match):
            # Check if this match is inside a string or comment
            if self._is_in_string_or_comment(text_for_declaration_check, match.start()):
                return match.group(0)  # Return unchanged
            return self._convert_double_precision_declaration(match)

        original_after_literals = text
        text = self.double_precision_pattern.sub(replace_declaration_match, text)
        if text != original_after_literals:
            conversions_made = True

        # Convert dble() function calls
        original_after_declarations = text
        text = self._convert_dble_calls(text)
        if text != original_after_declarations:
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

                # Count conversions for reporting (only count matches not in strings/comments)
                literal_conversions = 0
                for match in self.d_literal_pattern.finditer(original_content):
                    if not self._is_in_string_or_comment(original_content, match.start()):
                        literal_conversions += 1

                declaration_conversions = 0
                for match in self.double_precision_pattern.finditer(original_content):
                    if not self._is_in_string_or_comment(original_content, match.start()):
                        declaration_conversions += 1

                dble_conversions = 0
                for match in self.dble_pattern.finditer(original_content):
                    if not self._is_in_string_or_comment(original_content, match.start()):
                        dble_conversions += 1

                print(f"Converted {literal_conversions} literals, {declaration_conversions} declarations, and {dble_conversions} dble() calls in {input_path}")

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
                print(f"{input_path}(1): error: Directory specified. Use --directory flag to process directories.")
                return False

            if not input_path.exists():
                print(f"{input_path}(1): error: File does not exist.")
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

                dble_matches = []
                for match in self.dble_pattern.finditer(content):
                    if not self._is_in_string_or_comment(content, match.start()):
                        dble_matches.append(match)

                # Report each literal with line number in Visual Studio format
                for match in literal_matches:
                    line_num = content[:match.start()].count('\n') + 1
                    literal_text = match.group(0)
                    print(f"{input_path}({line_num}): error LINT001: Double precision literal found: '{literal_text}' should be converted to _dp format")

                # Report each declaration with line number in Visual Studio format
                for match in declaration_matches:
                    line_num = content[:match.start()].count('\n') + 1
                    print(f"{input_path}({line_num}): error LINT002: Double precision declaration found, should be 'real(kind=dp)'")

                # Report each dble() function call with line number in Visual Studio format
                for match in dble_matches:
                    line_num = content[:match.start()].count('\n') + 1
                    print(f"{input_path}({line_num}): error LINT003: dble() function found, should be 'real(..., kind=dp)'")

                # Add tip for auto-fixing after all errors for this file
                if literal_matches or declaration_matches or dble_matches:
                    print(f"{input_path}(1): note: Run 'python tools/double_precision_conversion/convert_double.py \"{input_path}\"' to automatically fix these errors")

                return True

            return False

        except PermissionError:
            print(f"{input_path}(1): error: Permission denied accessing file.")
            return False
        except UnicodeDecodeError:
            print(f"{input_path}(1): error: Cannot decode file as UTF-8 (binary file?).")
            return False
        except Exception as e:
            print(f"{input_path}(1): error: Unexpected error checking file: {e}")
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
  - Function calls: dble(x) -> real(x, kind=dp)

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
            print("\nBuild failed: Double precision literals and/or type declarations found that need conversion. See error list above.")
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
