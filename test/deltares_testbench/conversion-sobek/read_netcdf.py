
 
from netCDF4 import *
import numpy
import os
import os.path

def find_varname (nc_filename) :
    # Actual evaporation (unp)-d65b4d28-ac4b-4959-830d-792369110fb3.nc -> Actual evaporation (unp)
    return os.path.basename(nc_filename)[:-40]

def read_netcdf_files (data_dir) : 
    
    netcdf_files = [ os.path.join(data_dir, filename) for filename in os.listdir (data_dir) 
                     if filename.lower().endswith(".nc") and os.path.isfile(os.path.join(data_dir, filename)) ]
    d = {}
                     
    for filename in netcdf_files : 
        nc_root = Dataset (filename)
        varname = find_varname(filename)
        
        """        
        In the case of RTC, some variables might be double. For instance, there might be two 
        'Crest level (s)' files, one from Flow1d and one from RTC. The RTC timestep equals the 
        Flow1d model time step. However, the flow1d model might have an OUTPUT time step that 
        is larger than the COMPUTATIONAL time step. We need to store the flow1d output. 
        
        Example: 
        Flow1d: t_flow = (0,10,20)                length =  3
        RTC: t_rtc = (2,4,6,8,10,12,14,16,18,20)  length = 10
        Now it holds: len(t_rtc) % (len(t_flow) - 1) == 0
        
        """
        
        if varname not in d : 
            d[varname] = nc_root
        else : 
            nc_root_old = d[varname]
            try : 
                time_var_new = nc_root.variables['time']
                n_times_new = time_var_new.shape[0]
                
                time_var_old = nc_root_old.variables['time']
                n_times_old = time_var_old.shape[0]
                
                if n_times_old % (n_times_new - 1) == 0 : 
                    # The new file is from flow1d, so replace the nc_root. 
                    d[varname] = nc_root
            except KeyError : 
                print "KeyError when processing filename + " + filename
    
    return d
        
    