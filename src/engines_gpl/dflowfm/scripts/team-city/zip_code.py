"""Source zipper to produce a zip file with 'dflowfm-related' source code.


This script should be run on a clean svn checkout of the oss delft3d root.
The resulting zip file is intended for code analysis on only dflowfm-
related files.

See 'get_relevant_files' for a list of subdirs that will be included in
the final zip file.
"""

# Original source: https://repos.deltares.nl/repos/delft-tools/branches/FeatureBranches/NGHS_Delft3D_Beheer_en_Onderhoud/tools/zip_code.py

from pathlib import Path
import shutil
import argparse
import zipfile
import datetime
from itertools import chain


# Currently Assumes Cleaned repository folder.
def get_relevant_files(svn_root: Path):
    """
    Get all files from the folders relevant for dflowfm.

    :param svn_root: The root path of the svn.
    """
    src_path = svn_root / Path("src")

    return chain((src_path / 'engines_gpl' / 'dflowfm').glob("**/*"),
                 (src_path / 'engines_gpl' / 'waq').glob("**/*"),
                 (src_path / 'test' / 'engines_gpl').glob("**/*"),
                 (src_path / 'test' / 'utils_gpl').glob("**/*"),
                 (src_path / 'utils_gpl' / 'flow1d').glob("**/*"),
                 (src_path / 'utils_gpl' / 'morphology').glob("**/*"),
                 (src_path / 'utils_gpl' / 'trachytopes').glob("**/*"),
                 (src_path / 'utils_lgpl' / 'deltares_common').glob("**/*"),
                 (src_path / 'utils_lgpl' / 'ec_module').glob("**/*"),
                 (src_path / 'utils_lgpl' / 'ftnunit').glob("**/*"),
                 (src_path / 'utils_lgpl' / 'gridgeom').glob("**/*"),
                 (src_path / 'utils_lgpl' / 'io_netcdf').glob("**/*"),
                 (src_path / 'utils_lgpl' / 'kdtree_wrapper').glob("**/*"),
                 (src_path / 'utils_lgpl' / 'metistools').glob("**/*"),
                 (src_path / 'utils_lgpl' / 'unittests').glob("**/*"))


def write_zipfile(content_paths, zip_path: Path) -> None:
    """
    Write the specified content paths to the specified zip.

    :param content_paths: The collection of paths which need to be written to
                         the zipfile.
    :param zip_path: The path to zip file that will be written.
    """
    with zipfile.ZipFile(str(zip_path), 'w') as w_zip:
        for p in content_paths:
            w_zip.write(str(p))


def compose_argument_parser() -> argparse.ArgumentParser:
    """
    Compose the parser to be used in the main function of this script

    :returns: An argument parser with the correct settings.
    """
    parser = argparse.ArgumentParser()
    parser.add_argument("root_path", help="Path svn repository.", type=str)
    parser.add_argument("zip_name", help="zip name, will be changed to '<zip_name>_YYYYMMDD.zip'.")

    return parser


if __name__ == "__main__":
    parser = compose_argument_parser()
    args = parser.parse_args()

    svn_src_path = Path(args.root_path)

    today = datetime.date.today()
    zip_file_name = "{}_{}.zip".format(args.zip_name, today.strftime("%Y%m%d"))

    write_zipfile(get_relevant_files(svn_src_path), svn_src_path / Path(zip_file_name))
