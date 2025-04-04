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
    parser.add_argument("dest_dir", help="The destination directory where examples will be copied.")
    parser.add_argument("--tc_logging", help="Add additional logging for TeamCity.", action="store_true")
    arguments = parser.parse_args()

    return arguments


def create_destination_directory(dest_dir: Path, logger: Logger) -> int:
    """Create_destination_directory, if the directory is already there then make sure it is empty.

    Returns
    -------
        int: 1 if error, 0 if success
    """
    if not dest_dir:
        logger.log("DEST_DIR is not defined.", LogLevel.ERROR)
        return 1

    if not dest_dir.exists():
        logger.log("Directory does not exist, creating...")
        try:
            dest_dir.mkdir(parents=True, exist_ok=True)
        except OSError:
            logger.log(f"Unable to create directory - {dest_dir}", LogLevel.ERROR)
            return 1
    else:
        logger.log("Directory exists, deleting contents...", LogLevel.WARNING)
        try:
            for item in dest_dir.iterdir():
                if item.is_file():
                    logger.log(f"Removing: {item}")
                    item.unlink()
                elif item.is_dir():
                    logger.log(f"Removing: {item}")
                    shutil.rmtree(item)
        except OSError as e:
            logger.log(f"Failed to delete contents of directory - {dest_dir}: {e}", LogLevel.ERROR)
            return 1
    return 0


def copy_examples(example_directory: Path, apptainer_directory: Path, dest_dir: Path, logger: Logger) -> int:
    """Copy all files to the destination directory.

    Take example cases from example_directory and the apptainer scripts from the apptainer_directory.
    Copy all files to the destination directory.

    Returns
    -------
        int: 1 if error, 0 if success
    """
    h7_scripts = ["run_native_h7.sh", "submit_singularity_h7.sh"]
    exclude_patterns = ["run-all-examples-*"]

    try:
        for src_item in example_directory.rglob("*"):
            if any(src_item.match(pattern) for pattern in exclude_patterns):
                logger.log(f"Excluding file: {src_item}")
                continue

            dest_file = dest_dir / src_item.relative_to(example_directory)
            if src_item.is_file():
                dest_file.parent.mkdir(parents=True, exist_ok=True)
                logger.log(f"Copying file: {src_item} to {dest_file}")
                shutil.copy2(src_item, dest_file)
            elif src_item.is_dir():
                dest_file.mkdir(parents=True, exist_ok=True)
                logger.log(f"Creating directory: {dest_file}")

        for subdir in dest_dir.iterdir():
            if subdir.is_dir():
                for file in apptainer_directory.glob("*.sh"):
                    if file.name in h7_scripts:
                        dest_file = subdir / file.name
                        logger.log(f"Copying file: {file} to {dest_file}")
                        shutil.copy2(file, dest_file)
    except shutil.Error as e:
        logger.log(f"Copy failed: {e}", LogLevel.ERROR)
        return 1
    return 0


def get_base_directory_ci_python_scripts(relative_path: str) -> Path:
    """Get the base directory of the repository.

    Returns
    -------
        Path: returns the base directory of the repository
    """
    full_file_path = Path(relative_path).resolve()
    module_file_path = Path(relative_path)
    remove_part = "\\ci\\python\\" + str(module_file_path)
    base_dir = Path(str(full_file_path).replace(remove_part, ""))
    return base_dir


if __name__ == "__main__":
    arguments = parse_arguments()
    dest_dir = Path(arguments.dest_dir)
    logger = Logger(arguments.tc_logging)

    logger.log(f"Copy examples to location {dest_dir}")

    base_dir = get_base_directory_ci_python_scripts(__file__)
    examples_dir = base_dir / EXAMPLES_RELATIVE_DIR
    apptainer_dir = base_dir / APPTAINER_RELATIVE_DIR

    logger.log("Create destination directory.")
    if create_destination_directory(dest_dir, logger):
        sys.exit(1)

    logger.log("Copy files from the examples directory to the destination.")
    if copy_examples(examples_dir, apptainer_dir, dest_dir, logger):
        sys.exit(1)

    logger.log("Copy completed.")
