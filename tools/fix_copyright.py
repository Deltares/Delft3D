"""Script to update the copyright end year in source files.

Copyright (C)  Stichting Deltares, 2025
"""

import re
import logging
from pathlib import Path
import time

year = 2025  # The year to update the copyright to
year_to_replace = f"(?!{year})\\d{{4}}"  # The year to replace in the copyright

path = Path(".")  # The path to the directory containing the files to update
exclude_dirs = {"third_party_open", ".git"}  # Directories to exclude from the search

copyright_pattern = r"(?:Copyright|\(C\)|CPACK_PACKAGE_VENDOR)"
regex_deltares_before = f"(?P<before>{copyright_pattern}.*Deltares.*[-\W+])({year_to_replace})(?P<after>[^-\/].*)"
regex_deltares_after = f"(?P<before>{copyright_pattern}.*[-\W+])({year_to_replace})(?P<after>[^-\/].*Deltares.*)"

# Create a list of regex patterns to search for the copyright year (only "Deltares" copyrights not third-party)
regex_to_search = [re.compile(regex_deltares_before, re.IGNORECASE),
                   re.compile(regex_deltares_after, re.IGNORECASE)]

log = logging.getLogger(__name__)
logging.basicConfig(level=logging.INFO, format="'\r%(levelname)s: %(message)s")

file_list = [
    f
    for f in path.glob("**/*.*")
    if not any(excluded in f.parts for excluded in exclude_dirs)
]

start_time = time.time()
total_files = len(file_list)
change_count = 0

log.info("Updating copyright year to %d", year)

for idx, file in enumerate(file_list, 1):
    print(f"\rProcessing file {idx} of {total_files}: {file.name}".rjust(200), end="")

    if not file.is_file():
        continue

    try:
        file_content = file.read_text()
    except UnicodeDecodeError:
        log.debug("Skipping file due to encoding issues: %s", file)
        continue

    for search_regex in regex_to_search:

        if not search_regex.search(file_content):
            continue

        log.info("Updating file: %s", file)
        new_content = re.sub(search_regex, f"\g<before>{year}\g<after>", file_content)
        file.write_text(new_content)
        change_count += 1
        break

elapsed_time = time.time() - start_time
print(f"\nProcessing complete. Elapsed time: {elapsed_time:.2f} seconds, {change_count} files updated.")
