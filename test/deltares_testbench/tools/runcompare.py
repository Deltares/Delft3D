#!/usr/bin/env python
import pprint as pp
import sys

from src.config.file_check import FileCheck
from src.config.parameter import Parameter
from src.config.types.file_type import FileType
from src.utils.comparers.comparer_factory import ComparerFactory
from src.utils.logging.log_level import LogLevel
from src.utils.logging.logger import Logger

##################################################
# filename = 'str_his.nc'
defined_file_types = {
    "ascii": FileType.ASCII,
    "nefis": FileType.NEFIS,
    "his": FileType.HIS,
    "netcdf": FileType.NETCDF,
    "numbertext": FileType.NUMBERTEXT,
    "dseriesregression": FileType.DSERIESREGRESSION,
    "dseriesverification": FileType.DSERIESVERIFICATION,
    "timeseries_pi": FileType.PITIMESERIES,
    "timeseries_csv": FileType.CSVTIMESERIES,
}

# runcompare.py <filename> <variable> <right_path> <left_path> <filetype>

filename = sys.argv[1]
parname = sys.argv[2]
rp = sys.argv[3]
lp = sys.argv[4]
filetype = defined_file_types[sys.argv[5]]

abstol = 0.0001
reltol = 0.0001
##################################################

fc = FileCheck()
pm = Parameter()

fc.type = filetype
fc.name = filename
pm.name = parname
pm.tolerance_absolute = abstol
pm.tolerance_relative = reltol
fc.parameters = {"par1": [pm]}

logger = Logger(LogLevel.DEBUG, False)
cmp = ComparerFactory.select_comparer(fc, [], logger)

tpls = cmp.compare(lp, rp, fc, "testX", logger)
if len(tpls) == 0:
    print("Empty .... ")
else:
    pp.pprint(tpls[0][3])
print("DONE !!!")
