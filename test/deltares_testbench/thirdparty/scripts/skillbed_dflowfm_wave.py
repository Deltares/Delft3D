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

def getTimesFM(modelData):
    """
    Loads model time output form netCDF file for DFlow FM ouput in UNIX time (days after 01/01/1970 in days)
    """
    Tinfo=modelData.variables['time'] # load variable info
    Ttm = Tinfo[:] # load variable data
    inidatstr=str(Tinfo.units)[-19:]
    inidat=mdt.date2num(dt.datetime.strptime(inidatstr, "%Y-%m-%d %H:%M:%S"))
    if str(Tinfo.units).find('seconds') != -1:
        T= Ttm / 60 / 60 / 24 + inidat
    elif str(Tinfo.units).find('minutes') != -1:
        T= Ttm / 60 / 24 + inidat
    else:
        print('No dateformat recognized (minutes and seconds after inidate are implemented')
    return T
   
    
def Delft3DFM_his_get_stations(nc_file):
    """
    loads output locations from *_his.nc into a list called stn
    
    input = nc_file name including path
    
    output = stn (list with names output locations/stations)
    
    @author: moerman, visser
    """
    data_read = Dataset(nc_file,'r')    
    stats=data_read.variables['station_name'][:]
    stn = []
    
    for aa in range(len(stats[:,0])): 
        st_tm=stats[aa,:].tolist()
        stn.append("".join(st_tm))
    return stn
    
def Delft3DFM_his_time2datetime(nc_file):
    """
    loads time parameter from *_his.nc and converts into python datetime
    
    input = nc_file name including path
    
    output = stn (list with names output locations/stations)
    
    @author: moerman
    """
    data_read = Dataset(nc_file,'r')    
 
    mod_time_info=data_read.variables['time']
    mod_time_data=mod_time_info[:]

    # get reference time and units
    mod_ref_time_string = data_read.variables['time'].units

    mod_ref_time_string = str(mod_ref_time_string) # covert to string first
    mod_tt = mod_ref_time_string.split() # split string
    mod_ref_time_unit = mod_tt[0] # get units since

    # re-join time string
    ll = [mod_tt[2]]
    ll.append(mod_tt[3])
    mod_ref_time_string = ' '.join(ll)

    # convert ref_time from string to datenum
    mod_ref_time = dt.datetime.strptime(mod_ref_time_string,"%Y-%m-%d %H:%M:%S")

    # convert netcdf time units to python datetime
    mod_time = []
    if mod_ref_time_unit in ['seconds']:
       for ii in xrange(0,len(mod_time_data)):
           mod_time.append(mod_ref_time+dt.timedelta(seconds=mod_time_data[ii]))
    elif mod_ref_time_unit in ['minutes']:
       for ii in xrange(len(mod_time_data)):
           mod_time.append(mod_ref_time+dt.timedelta(minutes=mod_time_data[ii]))
    else:    
       print('No dateformat recognized (minutes and seconds after inidate are implemented')
    
    return mod_time    

def Delft3DFM_his_data2structure(nc_file,ids,*argv):
    """
    loads variables from *_his.nc and converts into pandas Dataframe structure for further processing
    
    input = nc_file name including path and list of variable names
    
    output = mod (pandas dataframe structure)
    
    @author: moerman
    """
    
    data_read = Dataset(nc_file,'r')    
 
    # %% TIME
    mod_time_info=data_read.variables['time']
    mod_time_data=mod_time_info[:]

    # get reference time and units
    mod_ref_time_string = data_read.variables['time'].units

    mod_ref_time_string = str(mod_ref_time_string) # covert to string first
    mod_tt = mod_ref_time_string.split() # split string
    mod_ref_time_unit = mod_tt[0] # get units since

    # re-join time string
    ll = [mod_tt[2]]
    ll.append(mod_tt[3])
    mod_ref_time_string = ' '.join(ll)

    # convert ref_time from string to datenum
    mod_ref_time = dt.datetime.strptime(mod_ref_time_string,"%Y-%m-%d %H:%M:%S")

    # convert netcdf time units to python datetime
    mod_time = []
    if mod_ref_time_unit in ['seconds']:
       for ii in xrange(0,len(mod_time_data)):
           mod_time.append(mod_ref_time+dt.timedelta(seconds=mod_time_data[ii]))
    elif mod_ref_time_unit in ['minutes']:
       for ii in xrange(len(mod_time_data)):
           mod_time.append(mod_ref_time+dt.timedelta(minutes=mod_time_data[ii]))
    else:    
       print('No dateformat recognized (minutes and seconds after inidate are implemented')
    
    # %% DATA (combined with time)
    mod_data = []
    dtmp = {'time' : pd.Series(mod_time)}
    # loop over parameters
    for ii in range(len(argv)):
        print argv[ii]
        tmp=data_read.variables[argv[ii]]
        mod_data.append(tmp[:,ids])
        dtmp[argv[ii]] = pd.Series(mod_data[ii])
    
    # convert to panda dataframe format
    clm = ['time']
    for jj in range(len(argv)):
        clm.append(argv[jj])

    mod = pd.DataFrame(dtmp, columns=[clm])
    # close the data_read and clean up
    data_read.close()

    return mod        
        
def uv2dir(u, v, convention='cartesian'):
    ''' 
    uv2dir derives the direction (in cartesian or nautical convention) from the u and v data
    u = u or x vector of parameter
    v = v or y vector of parameter
    convention = either 'cartesian' or 'nautical'
 
    where;
    'cartesian', the direction to where the vector points
    'nautical', the direction where the vector comes from
    if no convention is assigned, 'cartesian'  convention is applied
    note that both convention are considered clockwise from geographic North 
    '''

    if convention == 'cartesian':
        dr = np.mod(np.arctan2(u,v)*180 / np.pi, 360);
    elif convention == 'nautical':
        dr = np.mod(np.arctan2(-u,-v)*180 / np.pi, 360);
    dr[(u==0) & (v==0)] = np.nan
    return dr

def plotWaterdepth(networkfile, timestep=0, doutput='water_depth.png', cmap='jet', clb='Water depth [MSL m]', xlb = 'Easting', ylb = 'Northing', km='false', title='Water depth', alpha=1, **kwargs):

    """
    Plotting depth Delft3D Flexible Mesh
    
    
    variables:
    
    netwerkfile =  path of netCDF network file 
        

    Optional:
    
    timestep =  # output time index, 0 by default [0]
    
        
    doutput=  output directory and figure name, example: '/p/12304/water_depth.png', default: water_depth.png (in cwd)
        
    ldb = landboundary: list consisting landboundary coordinates [x,y]
    
    cmap = colormap: jet by default 'gist_earth'
    
    clm =  colorlimits: [cmin, cmax, ctick(interval)], example: [0, 10, 2]
    
    xlm = limits x axis [xmin, xmax, ytick(interval)], example: [110, 115, 1]
    
    ylm = limits y axis [ymin, ymax, ytick(interval)], example: [110, 115, 1]
    
    clb = colorbar label, defaut: 'Water depth [MSL m]'
    
    xlb = xlabel, example: 'Easting [UTM40N km]', default: 'Easting'
    
    ylb = ylabel, example: 'Northing [UTM40N km]', default: 'Northing'
    
    km = true / false:  tranform meters to kilomters on axes, default true
    
    title = title, example: 'Water depth run01', default: 'Water Depth level'
    
    geo_im = satellite image as background file: image (*.jpg, *.png)
    
    geo_info = georef information, worldfile (*.jgw, *.pgw)
    
    alpha =  tranparency of the map plot: default: no transparency, alpha=1
    
    
    2016/01/13: PVisser
    """
    netw=Dataset(networkfile)
    #    locals().update(kwargs) # return variables from kwargs dictionary
    
    print('Loading data, please wait...')
    # load initial bedlevel file
    x=netw.variables['NetNode_x'][:]
    y=netw.variables['NetNode_y'][:]
    elem=netw.variables['NetElemNode'][:]
    #    z=netw.variables['NetNode_z'][:]
    z=netw.variables['waterdepth'][timestep,:]
    netw.close()
    
    #   Create polygons       
    polygon=[]; patches = []
    for i in range(len(elem)):
        ind_tm=elem[i,:].nonzero() # find nonzero values form masked array 
        polx = x[elem[i, ind_tm]-1] # get xnodes per polygon, correct index with -1 (python)
        poly = y[elem[i, ind_tm]-1] # get xnodes per polygon, correct index with -1 (python)
        polxy = np.vstack((polx, poly)).transpose()
        polygon = Polygon(polxy, True)
        patches.append(polygon)
    
    # create collection
    p = PatchCollection(patches)
    p.set_array(np.array(z))
   
    # Create figure
    fs=[10, 10] # figsize
    
    if 'xlm' in kwargs and 'ylm' in kwargs: # if limits are defined, overwrite figsize to xy ratio
        xlm=np.array(kwargs['xlm']).astype(float)
        ylm=np.array(kwargs['ylm']).astype(float)
        xsz=xlm[1]-xlm[0]
        ysz=ylm[1]-ylm[0]
        xy_ratio=xsz/ysz
        if xy_ratio > 1:
            fs[0]=fs[0]*xy_ratio
        else:
            fs[1]=fs[1]/xy_ratio
    
    print('Creating figure, please wait...')
    fig=plt.figure(num=0, figsize=fs, dpi=80)
    ax = plt.axes([0.1, 0.1, 0.80, 0.80]) #Create axes for map plot [left, bottom, width, height]

   # plot satellite image
    if 'geo_im' in kwargs:
        geo_info=kwargs['geo_info']
        geo_im=kwargs['geo_im']
        xmn=geo_info[4]
        xmx=geo_info[4]+geo_info[0]*np.shape(geo_im)[1]
        ymn=geo_info[5]+geo_info[3]*np.shape(geo_im)[0]
        ymx=geo_info[5]
        plt.imshow(geo_im, extent=[xmn, xmx, ymn, ymx])

    # plot landboundary
    if 'ldb' in kwargs:
        ldb=kwargs['ldb']
        ax.plot(ldb[:, 0], ldb[:, 1], color='k', ls='-', marker='')

    # plot bathymetry
#    pl=plt.tripcolor(x, y, z, shading='gouraud', alpha=alpha)
    pl=ax.add_collection(p)
    pl.set_cmap(cmap)
    pl.set_edgecolor('none')
    pl.set_alpha(alpha)
    
    # scale axes
    ax.axis('scaled')
    
    # set axes limits and ticks
    if 'xlm' in kwargs:
        plt.xlim(xlm[0], xlm[1])
        if km=='true':
            plt.xticks(np.arange(xlm[0], xlm[1]+1, xlm[2]), np.arange(xlm[0]/1000, (xlm[1]+1)/1000, xlm[2]/1000)) # xtick
        else:
            plt.xticks(np.arange(xlm[0], xlm[1]+1, xlm[2])) # xtick
            
    if 'ylm' in kwargs:       
        plt.ylim(ylm[0], ylm[1])           
        if km=='true':
            plt.yticks(np.arange(ylm[0], ylm[1]+1, ylm[2]), np.arange(ylm[0]/1000, (ylm[1]+1)/1000, ylm[2]/1000)) # xtick
        else:
            plt.yticks(np.arange(ylm[0], ylm[1]+1, ylm[2])) # xtick
    
    # set colorlimits    
    if 'clm' in kwargs:    
        clm=kwargs['clm']
        p.set_clim(clm[0], clm[1])

    # set labels and title 
    plt.xlabel(xlb)
    plt.ylabel(ylb)
    plt.title(title)
    plt.grid('on')    
    
    # plot colorbar    
    cax= plt.axes([0.88, 0.10, 0.03, 0.80]) #Create axes for colorbar (inline) [left, bottom, width, height]
    cbar=plt.colorbar(mappable=pl, cax=cax) # plot colorbar
    cbar.set_label(clb) # colorbarlabel
    
    # set colobar ticks
    if 'clm' in kwargs:    
        cbar.set_ticks([np.arange(clm[0], clm[1]+1, clm[2])])   

    plt.show()
    print('Saving figure, please wait...')
    plt.savefig(doutput)
    plt.close(0)
    print('Finished, thank you for waiting')
    
def plotGrid(networkfile, doutput='grid.png', clr='k', lw=.1, xlb = 'Easting', ylb = 'Northing', km='false', title='Grid', **kwargs):

    """
    Plotting grid Flexible Mesh
    
    
    variables:
    
    netwerkfile =  path of netCDF network file 
        

    Optional:
       
    doutput=  output directory and figure name, example: '/p/12304/initial_bed.png', default: bed_level.png (in cwd)
        
    ldb = landboundary: list consisting landboundary coordinates [x,y]
    
    lw=linewidts, 0.1 by default
    
    xlm = limits x axis [xmin, xmax, ytick(interval)], example: [110, 115, 1]
    
    ylm = limits y axis [ymin, ymax, ytick(interval)], example: [110, 115, 1]
        
    xlb = xlabel, example: 'Easting [UTM40N km]', default: 'Easting'
    
    ylb = ylabel, example: 'Northing [UTM40N km]', default: 'Northing'
    
    km = true / false:  tranform meters to kilomters on axes, default true
    
    title = title, example: 'Water depth run01', default: 'Water Depth level'
    
    geo_im = satellite image as background file: image (*.jpg, *.png)
    
    geo_info = georef information, worldfile (*.jgw, *.pgw)
    
    alpha =  tranparency of the map plot: default: no transparency, alpha=1
    
    
    2016/01/13: PVisser
    """
    netw=Dataset(networkfile)
    #    locals().update(kwargs) # return variables from kwargs dictionary
    
    print('Loading data, please wait...')
    # load initial bedlevel file
    x=netw.variables['NetNode_x'][:]
    y=netw.variables['NetNode_y'][:]
    elem=netw.variables['NetElemNode'][:]
    #    z=netw.variables['NetNode_z'][:]
    netw.close()
    
    #   Create polygons       
    polygon=[]; poly_all = []
    for i in range(len(elem)):
        ind_tm=elem[i,:].nonzero() # find nonzero values form masked array 
        polx = x[elem[i, ind_tm]-1] # get xnodes per polygon, correct index with -1 (python)
        poly = y[elem[i, ind_tm]-1] # get xnodes per polygon, correct index with -1 (python)
        polxy = np.vstack((polx, poly)).transpose()
        polygon = Polygon(polxy, True)
        poly_all.append(polygon)
    
    # create collection
#    p=RegularPolyCollection(poly, edgecolor=clr, facecolor='none', linewidths=2)
    p=PatchCollection(poly_all, facecolor='none', linewidths=lw, edgecolor=clr)
    # Create figure
    fs=[10, 10] # figsize
    
    if 'xlm' in kwargs and 'ylm' in kwargs: # if limits are defined, overwrite figsize to xy ratio
        xlm=np.array(kwargs['xlm']).astype(float)
        ylm=np.array(kwargs['ylm']).astype(float)
        xsz=xlm[1]-xlm[0]
        ysz=ylm[1]-ylm[0]
        xy_ratio=xsz/ysz
        if xy_ratio > 1:
            fs[0]=fs[0]*xy_ratio
        else:
            fs[1]=fs[1]/xy_ratio
    
    print('Creating figure, please wait...')
    fig=plt.figure(num=0, figsize=fs, dpi=80)
    ax = plt.axes([0.1, 0.1, 0.8, 0.8]) #Create axes for map plot [left, bottom, width, height]

   # plot satellite image
    if 'geo_im' in kwargs:
        geo_info=kwargs['geo_info']
        geo_im=kwargs['geo_im']
        xmn=geo_info[4]
        xmx=geo_info[4]+geo_info[0]*np.shape(geo_im)[1]
        ymn=geo_info[5]+geo_info[3]*np.shape(geo_im)[0]
        ymx=geo_info[5]
        plt.imshow(geo_im, extent=[xmn, xmx, ymn, ymx])

    # plot landboundary
    if 'ldb' in kwargs:
        ldb=kwargs['ldb']
        ax.plot(ldb[:, 0], ldb[:, 1], color='k', ls='-', marker='')

    # plot grid
    pl=ax.add_collection(p)
 
    # scale axes
    ax.axis('scaled')
    
    # set axes limits and ticks
    if 'xlm' in kwargs:
        plt.xlim(xlm[0], xlm[1])
        if km=='true':
            plt.xticks(np.arange(xlm[0], xlm[1]+1, xlm[2]), np.arange(xlm[0]/1000, (xlm[1]+1)/1000, xlm[2]/1000)) # xtick
        else:
            plt.xticks(np.arange(xlm[0], xlm[1]+1, xlm[2])) # xtick
            
    if 'ylm' in kwargs:       
        plt.ylim(ylm[0], ylm[1])           
        if km=='true':
            plt.yticks(np.arange(ylm[0], ylm[1]+1, ylm[2]), np.arange(ylm[0]/1000, (ylm[1]+1)/1000, ylm[2]/1000)) # xtick
        else:
            plt.yticks(np.arange(ylm[0], ylm[1]+1, ylm[2])) # xtick
 
    # set labels and title 
    plt.xlabel(xlb)
    plt.ylabel(ylb)
    plt.title(title)
    plt.grid('on')    
    
    plt.show()
    print('Saving figure, please wait...')
    plt.savefig(doutput)
    plt.close(0)
    print('Finished, thank you for waiting')
    
def errorStats_skillbed(obs,mod,obs_parameters,mod_parameters):

    """
    compute error stats and write Latex table for skillbed report
    
    variables:
    
    obs = pandas dataframe structure of observation data
    
    mod = pandas dataframe structure of model data, following function:  Delft3DFM_his_data2structure
    
    obs_parameters = parameters names of
    
    mod_parameters =
    
    
    2017/02/03: EMoerman
    """
    
    # set time to index first    
    obs = obs.set_index(['time'])  
    mod = mod.set_index(['time'])
    
    # concatenate, sort, interpolate and reindex based on observation times
    D = pd.concat([mod, obs], axis=1).sort_index().interpolate(limit=1).reindex(obs.index)
    
    mod[mod.index.duplicated()]
    obs.index.is_unique
    
    # remove NaNs
    D.dropna(inplace=True)

    D1 = D[[obs_parameters,mod_parameters]]
    D1.columns = ['obs','mod']
    
    # mean values
    m1 = D1['obs'].mean()
    m2 = D1['mod'].mean()
    
    # bias
    bias = m2-m1
    
    # root-mean-square error
    rms = np.sqrt(mean_squared_error(D1['obs'],D1['mod']))
    
    # Pearsons correlation coefficient
    R = pearsonr(D1['obs'],D1['mod'])
    
    return (D, m1, m2, bias, rms, R)
        
