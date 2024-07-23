"""Enum for comparer type.

Copyright (C)  Stichting Deltares, 2024
"""

from enum import Enum


class FileType(Enum):
    """Type of comparer."""

    NONE = 0
    ASCII = 1
    NEFIS = 2
    NETCDF = 3
    CONTENT = 4
    HIS = 5
    NUMBERTEXT = 6
    DSERIESREGRESSION = 7
    DSERIESVERIFICATION = 8
    PITIMESERIES = 9
    CSVTIMESERIES = 10
