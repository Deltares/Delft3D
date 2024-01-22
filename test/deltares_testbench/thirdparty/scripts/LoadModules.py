#%% load libraries

# import miscellaneous libraries
import matplotlib.pyplot as plt
import matplotlib.dates as mdt
import matplotlib.image as mpimg
from matplotlib import style
from matplotlib import use
from matplotlib import rc
import datetime as dt
import numpy as np
import pandas as pd
from netCDF4 import Dataset
import sys
import os
import glob
import math
from matplotlib.patches import Circle, Wedge, Polygon
from matplotlib.collections import PatchCollection

# error stats
from scipy.stats.stats import pearsonr
from sklearn.metrics import mean_squared_error

# import skillbed functions
import sys
sys.path.append(r'..\scripts')
import skillbed

