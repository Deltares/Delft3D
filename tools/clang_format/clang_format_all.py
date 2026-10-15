from pathlib import Path
import subprocess
from concurrent.futures import ProcessPoolExecutor, as_completed
import multiprocessing

# File extensions to check
EXTENSIONS = {".h", ".hpp", ".c", ".cpp"}

# Skip certain files to speed up the process significant, even though there is a clang-format-ignore file.
# WARNING: these names are filtered at ANY nesting level!
# TODO: Remove filtering logic when third party code is no longer in <repo_root>/src
IGNORE_DIRS = {"third_party", "third_party_open", "thirdParty"}


def should_format(path: Path) -> bool:
    return path.suffix.lower() in EXTENSIONS


def run_clang_format(file_path: Path):
    try:
        subprocess.run(["clang-format", "-i", str(file_path)], check=True)
        return None
    except subprocess.CalledProcessError as e:
        return f"Error formatting {file_path}: {e}"


def find_source_files(root: Path):
    def recurse(directory: Path):
        for entry in directory.iterdir():
            if entry.is_dir():
                if entry.name not in IGNORE_DIRS:
                    yield from recurse(entry)
            else:
                # Use pathlib's suffix instead of os.path.splitext
                if entry.suffix.lower() in EXTENSIONS:
                    yield entry

    yield from recurse(root)


def is_clang_format_available():
    try:
        subprocess.run(["clang-format", "--version"], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL, check=True)
        return True
    except (subprocess.CalledProcessError, FileNotFoundError):
        return False


def main():
    if is_clang_format_available() == False:
        raise Exception(
            "clang-format not available. Make sure it is in your path or make it available using pip install clang-format."
        )

    # Execute clang-format in <repo_root>/src dir
    script_dir = Path(__file__).resolve().parent
    src_dir = (script_dir / ".." / ".." / "src").resolve()
    root_dir = src_dir
    files = list(find_source_files(root_dir))

    print(f"Found {len(files)} files to format.")
    cpu_count = multiprocessing.cpu_count()
    print(f"Using {cpu_count} parallel jobs.")

    with ProcessPoolExecutor(max_workers=cpu_count) as executor:
        futures = {executor.submit(run_clang_format, f): f for f in files}
        for future in as_completed(futures):
            result = future.result()
            print(".", end="", flush="true") if result == None else print(f"\n{result}")


if __name__ == "__main__":
    main()
