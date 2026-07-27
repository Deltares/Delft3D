import shutil
import sys
from argparse import ArgumentParser, Namespace
from pathlib import Path

from ci_tools.example_utils.logger import Logger, LogLevel

EXAMPLES_RELATIVE_DIR = Path("examples/dflowfm")
APPTAINER_RELATIVE_DIR = Path("src/scripts_lgpl/singularity")


def parse_arguments() -> Namespace:
    """Set up argument parser and parse args to namespace.

    Returns
    -------
        Namespace: Parsed arugments
    """
    parser = ArgumentParser(
        description="Copy example cases and Apptainer scripts to the specified destination directory."
        "If the destination directory does not exist, it will be created."
        "If the destination directory exists, its contents will be cleared before copying."
    )
    parser.add_argument("--dest_dir", help="The destination directory where examples will be copied.")
    parser.add_argument("--tc_logging", help="Add additional logging for TeamCity.", action="store_true")
    arguments = parser.parse_args()

    return arguments


def create_destination_directory(dest_dir: Path, logger: Logger) -> bool:
    """Create the destination directory.

    If the directory does not exist, it will be created. If the directory already exists, its contents will be deleted.

    Returns
    -------
        bool: True if successful, False if there were errors
    """
    if not dest_dir:
        logger.log("DEST_DIR is not defined.", LogLevel.ERROR)
        return False

    success = True

    if not dest_dir.exists():
        logger.log("Directory does not exist, creating...")
        try:
            dest_dir.mkdir(parents=True, exist_ok=True)
        except OSError:
            logger.log(f"Unable to create directory - {dest_dir}", LogLevel.ERROR)
            success = False
    else:
        logger.log("Directory exists, deleting contents...")
        for item in dest_dir.iterdir():
            try:
                if item.is_file():
                    logger.log(f"Removing: {item}")
                    item.unlink()
                elif item.is_dir():
                    logger.log(f"Removing: {item}")
                    shutil.rmtree(item)
            except (PermissionError, OSError) as e:
                logger.log(f"Error deleting {item}: {e}", LogLevel.ERROR)
                success = False

    return success


def copy_examples(example_directory: Path, apptainer_directory: Path, dest_dir: Path, logger: Logger) -> bool:
    """
    Copy example files and Apptainer scripts to the destination directory.

    Parameters
    ----------
    example_directory : Path
        Directory containing example files.
    apptainer_directory : Path
        Directory containing Apptainer scripts.
    dest_dir : Path
        Destination directory for copied files.
    logger : Logger
        Logger for logging messages.

    Returns
    -------
        bool: True if all operations succeed, False otherwise.
    """
    h7_scripts = {"run_native_h7.sh", "submit_singularity_h7.sh"}
    exclude_patterns = {"run-all-examples-*"}
    success = True

    # Copy example files
    for src_item in example_directory.rglob("*"):
        try:
            if any(src_item.match(pattern) for pattern in exclude_patterns):
                logger.log(f"Excluding file: {src_item}")
                continue

            dest_file = dest_dir / src_item.relative_to(example_directory)

            if src_item.is_dir():
                dest_file.mkdir(parents=True, exist_ok=True)
                logger.log(f"Created directory: {dest_file}")
            elif src_item.is_file():
                dest_file.parent.mkdir(parents=True, exist_ok=True)
                shutil.copy2(src_item, dest_file)
                logger.log(f"Copied file: {src_item} to {dest_file}")
        except (FileNotFoundError, PermissionError, OSError) as e:
            logger.log(f"Error copying {src_item} to {dest_file}: {e}", LogLevel.ERROR)
            success = False

    # Copy Apptainer scripts to each subdirectory in dest_dir
    for subdir in dest_dir.iterdir():
        if not subdir.is_dir():
            continue
        for script in apptainer_directory.glob("*.sh"):
            if script.name in h7_scripts:
                try:
                    dest_script = subdir / script.name
                    shutil.copy2(script, dest_script)
                    logger.log(f"Copied script: {script} to {dest_script}")
                except (FileNotFoundError, PermissionError, OSError) as e:
                    logger.log(f"Error copying script {script} to {dest_script}: {e}", LogLevel.ERROR)
                    success = False

    logger.log("Copy operation completed.")
    return success


def get_base_directory_ci_python_scripts(relative_path: str) -> Path:
    """Get the base directory of the repository.

    Returns
    -------
        Path: returns the base directory of the repository
    """
    full_file_path = Path(relative_path).resolve()
    # Split the path on 'ci/python' and take the left-hand part
    base_dir = Path(str(full_file_path).split("\\ci\\python")[0])
    return base_dir


if __name__ == "__main__":
    arguments = parse_arguments()
    dest_dir = Path(arguments.dest_dir)
    logger = Logger(arguments.tc_logging)

    logger.log(f"Copy examples to location {dest_dir}")

    base_dir = get_base_directory_ci_python_scripts(__file__)
    examples_dir = base_dir / EXAMPLES_RELATIVE_DIR
    apptainer_dir = base_dir / APPTAINER_RELATIVE_DIR
    logger.log(f"Examples directory: {examples_dir}")
    logger.log(f"Apptainer directory: {apptainer_dir}")

    has_errors = False
    logger.log("Create destination directory.")
    if not create_destination_directory(dest_dir, logger):
        has_errors = True

    logger.log("Copy files from the examples directory to the destination.")
    if not copy_examples(examples_dir, apptainer_dir, dest_dir, logger):
        has_errors = True

    if has_errors:
        sys.exit(1)
