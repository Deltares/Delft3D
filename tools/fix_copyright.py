"""Script to update the copyright end year in source files.

Copyright (C)  Stichting Deltares, 2026
"""

import re
import logging
from pathlib import Path
import time

year = 2026  # The year to update the copyright to
year_to_replace = f"(?!{year})\\d{{4}}"  # The year to replace in the copyright

path = Path(".")  # The path to the directory containing the files to update
exclude_dirs = {"third_party_open", ".git"}  # Directories to exclude from the search

# Create comprehensive regex patterns to handle all copyright formats
regex_patterns = [
    # Original patterns: Copyright (C) Deltares, 2026
    r"(?P<before>(?:Copyright|\(C\)|CPACK_PACKAGE_VENDOR).*Deltares.*[-\W+])(?P<year>(?!"
    + str(year)
    + r")\d{4})(?P<after>[^-\/].*)",
    # Original patterns: Copyright (C) 2026 Deltares
    r"(?P<before>(?:Copyright|\(C\)|CPACK_PACKAGE_VENDOR).*[-\W+])(?P<year>(?!"
    + str(year)
    + r")\d{4})(?P<after>[^-\/].*Deltares.*)",
    # Resource files: VALUE "LegalCopyright", "Copyright © 2011-2026\0"
    r"(?P<before>VALUE\s+\"LegalCopyright\",\s*\"Copyright\s*©\s*\d{4}-)(?P<year>(?!"
    + str(year)
    + r")\d{4})(?P<after>\\0\")",
    # C# AssemblyInfo: [assembly: AssemblyCopyright("Copyright © Deltares 2026")]
    r"(?P<before>\[assembly:\s*AssemblyCopyright\(\"Copyright\s*©\s*Deltares\s+)(?P<year>(?!"
    + str(year)
    + r")\d{4})(?P<after>\"\)\])",
    # CMake: set(CPACK_PACKAGE_VENDOR "Deltares 2026")
    r"(?P<before>set\(CPACK_PACKAGE_VENDOR\s+\"Deltares\s+)(?P<year>(?!"
    + str(year)
    + r")\d{4})(?P<after>\"\))",
    # Stichting Deltares with year ranges: Copyright (C)  Stichting Deltares, 2011-2026.
    r"(?P<before>Copyright\s*\(C\)\s*Stichting\s+Deltares,\s*\d{4}-)(?P<year>(?!"
    + str(year)
    + r")\d{4})(?P<after>\.)",
    # Fortran comments: !!! (c) Deltares, 2026
    r"(?P<before>!!!\s*\(c\)\s*Deltares,\s*)(?P<year>(?!"
    + str(year)
    + r")\d{4})(?P<after>)",
    # Standard format: Copyright (C) 2026 Deltares
    r"(?P<before>Copyright\s*\(C\)\s*)(?P<year>(?!"
    + str(year)
    + r")\d{4})(?P<after>\s+Deltares)",
    # Year ranges ending with old year: 2011-2025, 2012-2025, etc.
    r"(?P<before>\d{4}-)(?P<year>(?!" + str(year) + r")\d{4})(?P<after>.*Deltares)",
    # !    (c) Copyright 2026 Deltares
    r"(?P<before>!\s*\(c\)\s*Copyright\s+)(?P<year>(?!"
    + str(year)
    + r")\d{4})(?P<after>\s+Deltares)",
    # !  Copyright (C) 2026 Geert Prinsen  WL|Deltares
    r"(?P<before>!\s*Copyright\s*\(C\)\s*)(?P<year>(?!"
    + str(year)
    + r")\d{4})(?P<after>\s+.*Deltares)(?!.*\d{4})",
    # !     (C) 2026 Deltares
    r"(?P<before>!\s*\(C\)\s*)(?P<year>(?!"
    + str(year)
    + r")\d{4})(?P<after>\s+Deltares)",
    # Shell script comments: # (c) Deltares, 2026
    r"(?P<before>#\s*\(c\)\s*Deltares,\s*)(?P<year>(?!"
    + str(year)
    + r")\d{4})(?P<after>)",
    # Copyright (C) 2007-2026 UNESCO-IHE, Deltares
    r"(?P<before>Copyright\s*\(C\)\s*\d{4}-)(?P<year>(?!"
    + str(year)
    + r")\d{4})(?P<after>.*Deltares)",
    # Version files with year ranges: "Copyright (C) company, 2023-2026"
    r"(?P<before>\"Copyright\s*\(C\)\s*[^\"]*[-,\s]+)(?P<year>(?!"
    + str(year)
    + r")\d{4})(?P<after>\")",
    # Fortran character parameter: "Copyright (C) "//company//", 2023-2026"
    r"(?P<before>\"Copyright\s*\(C\)\s*\"//[^/]+//\",\s*\d{4}-)(?P<year>(?!"
    + str(year)
    + r")\d{4})(?P<after>\")",  # Asterisk comments: *  Copyright (C) 2026, Deltares
    r"(?P<before>\*\s*Copyright\s*\(C\)\s*)(?P<year>(?!"
    + str(year)
    + r")\d{4})(?P<after>[,\s]+.*Deltares)",
    # Fortran write statements with copyright: '    Copyright (c) 2026              DELTARES        '
    r"(?P<before>'[^']*Copyright\s*\(c\)\s*)(?P<year>(?!"
    + str(year)
    + r")\d{4})(?P<after>[^']*DELTARES[^']*')",
    # Rijkswaterstaat copyright: Copyright © 2026, Rijkswaterstaat, All Rights Reserved. (with or without ! prefix)
    r"(?P<before>(?:!\s*)?Copyright\s*©\s*)(?P<year>(?!"
    + str(year)
    + r")\d{4})(?P<after>,\s*Rijkswaterstaat,\s*All\s*Rights\s*Reserved\.)",
    # C define with copyright: #define LEGAL_COPYRIGHT "Copyright \xA9 2023-2026"
    r"(?P<before>#define\s+LEGAL_COPYRIGHT\s+\"Copyright\s*\\xA9\s*\d{4}-)(?P<year>(?!"
    + str(year)
    + r")\d{4})(?P<after>\")",
]

# Compile all regex patterns
regex_to_search = [re.compile(pattern, re.IGNORECASE) for pattern in regex_patterns]

log = logging.getLogger(__name__)
logging.basicConfig(level=logging.INFO, format="'\r%(levelname)s: %(message)s")

file_list = [
    f
    for f in path.glob("**/*.*")
    if not any(excluded in f.parts for excluded in exclude_dirs)
]

file_list.extend(
    [
        f
        for f in path.glob("**/*")
        if f.is_file()
        and "." not in f.name
        and not any(excluded in f.parts for excluded in exclude_dirs)
    ]
)

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
        new_content = re.sub(search_regex, f"\\g<before>{year}\\g<after>", file_content)
        file.write_text(new_content)
        change_count += 1
        break

elapsed_time = time.time() - start_time
print(
    f"\nProcessing complete. Elapsed time: {elapsed_time:.2f} seconds, {change_count} files updated."
)
