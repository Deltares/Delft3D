"""Comparer Factory.

Copyright (C)  Stichting Deltares, 2024
"""

from abc import ABC
from typing import List

from src.config.file_check import FileCheck
from src.config.types.file_type import FileType
from src.suite.program import Program
from src.utils.comparers.ascii_comparer import AsciiComparer
from src.utils.comparers.content_comparer import ContentComparer
from src.utils.comparers.csv_time_series_comparer import CSVTimeseriesComparer
from src.utils.comparers.d_series_benchmark_comparer import DSeriesBenchmarkComparer
from src.utils.comparers.d_series_comparer import DSeriesComparer
from src.utils.comparers.his_comparer import HisComparer
from src.utils.comparers.i_comparer import IComparer
from src.utils.comparers.nefis_comparer import NefisComparer
from src.utils.comparers.netcdf_comparer import NetcdfComparer
from src.utils.comparers.number_text_comparer import NumberTextComparer
from src.utils.comparers.pi_time_series_comparer import PiTimeseriesComparer
from src.utils.logging.i_logger import ILogger


class ComparerFactory(ABC):
    """Chooses which comparer should be used for the cases."""

    @classmethod
    def select_comparer(cls, file_check: FileCheck, programs: List[Program], logger: ILogger) -> IComparer:
        """Create a comparer instance based on file_check type.

        Parameters
        ----------
        file_check : FileCheck
            File check used.
        programs : List[Program]
            List of all registered programs.

        Returns
        -------
        IComparer
            Comparer to use for the provided file check.
        """
        file_type = file_check.type

        if file_type == FileType.ASCII:
            logger.debug("comparer selected for ASCII")
            return AsciiComparer()
        if file_type == FileType.NEFIS:
            logger.debug("comparer selected for NEFIS")
            vs_program = next(p for p in programs if p.name == "vs")
            return NefisComparer(vs_program)
        if file_type == FileType.NETCDF:
            logger.debug("comparer selected for NetCDF")
            return NetcdfComparer()
        if file_type == FileType.HIS:
            logger.debug("comparer selected for HIS")
            return HisComparer()
        if file_type == FileType.NUMBERTEXT:
            logger.debug("comparer selected for numbers and text")
            return NumberTextComparer()
        if file_type == FileType.DSERIESREGRESSION:
            logger.debug("comparer selected for DSERIES REGRESSION")
            return DSeriesComparer()
        if file_type == FileType.DSERIESVERIFICATION:
            logger.debug("comparer selected for DSERIES VERIFICATION")
            return DSeriesBenchmarkComparer()
        if file_type == FileType.PITIMESERIES:
            logger.debug("comparer selected for Pi XML Timeseries set")
            return PiTimeseriesComparer()
        if file_type == FileType.CSVTIMESERIES:
            logger.debug("comparer selected for CSV Timeseries set")
            return CSVTimeseriesComparer()

        logger.warning(f"Unknown comparer specified for file: {file_check.name}; raw content comparer selected")
        return ContentComparer()
