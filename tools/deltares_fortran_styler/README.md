# Deltares Fortran Styler

A modular, extensible Fortran style checker and converter for the Deltares style guide.

## Architecture

The styler is built with a modular architecture that allows for independent style converters that don't interfere with each other.

### Core Components

```
deltares_fortran_styler/
├── fortran_styler.py              # Main entry point
├── base_converter.py              # Abstract base class for converters
├── file_processor.py              # File I/O and orchestration
├── double_precision_converter.py  # Double precision converter
└── array_delimiter_converter.py   # Array delimiter converter
```

### Design Principles

1. **Modularity**: Each converter is independent and focuses on one aspect of style
2. **Performance**: Converters process text in a single pass where possible
3. **Extensibility**: New converters can be added by implementing the `FortranConverter` interface
4. **Independence**: Converters don't interfere with each other's logic

### Adding a New Converter

To add a new style rule:

1. Create a new file `your_converter.py`
2. Import and extend `FortranConverter` from `base_converter.py`
3. Implement the required methods:
   - `get_name()`: Return converter name for logging
   - `convert_text()`: Apply transformations to text
   - `check_text()`: Check for issues without modifying
   - `needs_import()`: Whether converter adds imports
   - `add_required_imports()`: Add necessary imports if changes were made
4. Register your converter in `fortran_styler.py`:
   ```python
   from your_converter import YourConverter

   AVAILABLE_CONVERTERS = {
       'double_precision': DoublePrecisionConverter,
       'array_delimiter': ArrayDelimiterConverter,
       'your_converter': YourConverter,  # Add here
   }
   ```

Example minimal converter:

```python
from base_converter import FortranConverter, ConversionIssue
from typing import Tuple, List

class MyConverter(FortranConverter):
    def get_name(self) -> str:
        return "MyConverter"

    def convert_text(self, text: str) -> Tuple[str, bool]:
        # Your conversion logic here
        return text, False  # Return (modified_text, was_modified)

    def check_text(self, text: str) -> List[ConversionIssue]:
        # Your checking logic here
        return []  # Return list of issues found

    def needs_import(self) -> bool:
        return False  # True if you add import statements

    def add_required_imports(self, text: str, was_converted: bool) -> str:
        return text  # Add imports if needed
```

## Usage

### Convert Files (In-Place)

```bash
# Convert individual files
python fortran_styler.py file1.f90 file2.f90

# Convert all files in a directory
python fortran_styler.py --directory src/

# Use only specific converters
python fortran_styler.py --converters double_precision --directory src/
```

### Check Mode (CI/CD)

```bash
# Check without modifying - returns exit code 1 if issues found
python fortran_styler.py --check file1.f90

# Check directory
python fortran_styler.py --check --directory src/
```

### Available Converters

#### `double_precision`
Converts double precision code to modern format:
- Literals: `1d0` → `1.0_dp`, `2.5d-3` → `2.5e-3_dp`
- Declarations: `double precision :: var` → `real(kind=dp) :: var`
- Functions: `dble(x)` → `real(x, kind=dp)`
- Automatically adds `use precision, only: dp` import

#### `array_delimiter`
Converts old-style array constructors to modern syntax:
- `(/ 1, 2, 3 /)` → `[1, 2, 3]`

## Performance Considerations

The styler is designed for good performance:

1. **Single-pass processing**: Each converter processes text in one pass
2. **Sequential converters**: Converters run sequentially, not nested
3. **Efficient regex**: Patterns are compiled once at initialization
4. **Smart I/O**: Files are only written if changes are made
5. **Context checking**: String/comment detection is optimized

## Testing

### Running Tests

The project uses pytest for unit testing. To run the tests:

```bash
# Create and activate a virtual environment
python -m venv .venv # (first time only)
.venv\Scripts\activate  # Windows
source .venv/bin/activate  # Linux/Mac

# Install the package with development dependencies
pip install -e ".[dev]"

# Run all tests
pytest

# Run tests with verbose output
pytest -v

# Run a specific test file
pytest tests/test_double_precision_converter.py
```

### Test Structure

Tests are organized following standard pytest conventions:
- Test files are in the `tests/` directory
- Test files are named `test_*.py`
- Test classes are named `Test*`
- Test functions are named `test_*`

The test suite includes:
- Unit tests for all converter functionality
- Tests for edge cases (comments, strings, nested structures)
- Tests for module/submodule handling
- Parametrized tests for format variations

## Error Codes

- `STYLE001`: Double precision literal needs conversion
- `STYLE002`: Double precision declaration needs conversion
- `STYLE003`: `dble()` function needs conversion
- `STYLE004`: Old-style array constructor needs conversion

## Development Notes

### Code Organization

- **base_converter.py**: Defines the `FortranConverter` abstract base class and `ConversionIssue` data class
- **file_processor.py**: Handles file I/O, orchestrates multiple converters, manages check vs convert modes
- **fortran_styler.py**: Main entry point, argument parsing, converter registration
- **Individual converters**: Self-contained modules implementing specific style rules

### Thread Safety

The current implementation is single-threaded. For large codebases, consider adding multiprocessing to `FileProcessor.process_directory()`.

### Memory Usage

Files are processed one at a time. The entire file content is loaded into memory, converted, and written back. This is acceptable for typical Fortran source files but could be optimized for very large files if needed.
