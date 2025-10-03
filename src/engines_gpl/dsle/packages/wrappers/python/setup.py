"""Model for salt intrusion through shipping locks.

pyzsf calculates the salt intrusion through shipping locks based on
easy-to-estimate operational parameters.
"""
import sys

from setuptools import find_packages, setup


DOCLINES = __doc__.split("\n")

CLASSIFIERS = """\
Development Status :: 3 - Alpha
Intended Audience :: Science/Research
Intended Audience :: Information Technology
License :: OSI Approved :: GNU Lesser General Public License v3 (LGPLv3)
Programming Language :: Other
Topic :: Scientific/Engineering :: GIS
Topic :: Scientific/Engineering :: Mathematics
Topic :: Scientific/Engineering :: Physics
Operating System :: Microsoft :: Windows
Operating System :: POSIX
Operating System :: Unix
Operating System :: MacOS
"""

if sys.version_info < (3, 6):
    sys.exit("Sorry, Python 3.6 or newer is required.")

try:
    version = open("../git_describe.txt").read().strip()
    if not version:
        raise ValueError("Version was empty")
except (FileNotFoundError, ValueError):
    version = "0.0.1dev0"

setup(
    name="pyzsf",
    version=version,
    description=DOCLINES[0],
    classifiers=[_f for _f in CLASSIFIERS.split("\n") if _f],
    url="https://www.deltares.nl",
    author="Jack Vreeken",
    author_email="jack@vreeken.me",
    maintainer="Jack Vreeken",
    license="LGPLv3",
    keywords="zsf shipping locks salt intrusion",
    platforms=["Windows", "Linux", "Mac OS-X", "Unix"],
    packages=find_packages("src"),
    package_dir={"": "src"},
    setup_requires=["cffi >= 1.0.0"],
    cffi_modules=["src/_pyzsf_build.py:ffibuilder"],
    install_requires=["cffi >= 1.0.0"],
    tests_require=["pytest", "pytest-runner", "numpy"],
    python_requires=">=3.6",
)
