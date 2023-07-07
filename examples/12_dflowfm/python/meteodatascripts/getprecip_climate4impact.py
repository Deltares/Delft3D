#!/usr/bin/env python
# http://opendap.knmi.nl/knmi/thredds/fileServer/radarprecipclim/RAD_NL21_RAC_MFBS_5min_NC/2003/04/RAD_NL21_RAC_MFBS_5min_200304302255.nc
import datetime as dt
import sys
import numpy as np
import netCDF4 as nc
import os
import pprint as pp
import pyproj
#import urllib2 as ul
import urllib as ul

############################################################################
# Dataset Description:
# http://opendap.knmi.nl/knmi/thredds/fileServer/radarprecipclim/Description/Description_RAD_NL25_RAC_MFBS_5min.pdf
# See the Climate4Impact site:
# https://climate4impact.eu/impactportal/data/catalogbrowser.jsp?catalog=http://opendap.knmi.nl/knmi/thredds/radarprecipclim.xml

baseURL = "http://opendap.knmi.nl/knmi/thredds/fileServer/radarprecipclim"
productname = "RAD_NL21_RAC_MFBS_5min"
outname = "./"+productname+".nc"

# Case:
# 27,28 juli 2014
# longitude: 4.9 to 5.05 (or 5.1)           # Kockengen area 
# latitude: 52.1 to 52.2 

# Area around Kockengen
#poly = np.array([       [4.9, 52.1], 
#                        [5.05,52.1],
#                        [5.05,52.2],
#                        [4.9, 52.2]]);


# Netherlands
poly = np.array([        [ 3.364970,51.589386],
                         [ 3.985135,52.017689],
                         [ 4.420055,52.363309],
                         [ 4.556974,52.735517],
                         [ 4.726110,53.152899],
                         [ 5.064382,53.364888],
                         [ 5.789249,53.475283],
                         [ 6.715469,53.499244],
                         [ 7.126227,53.451308],
                         [ 7.255092,53.259025],
                         [ 7.029578,51.953207],
                         [ 6.425522,51.774155],
                         [ 6.191953,51.749230],
                         [ 6.256386,51.594389],
                         [ 6.296656,51.278081],
                         [ 6.087250,50.689931],
                         [ 5.563735,50.756216],
                         [ 5.660384,50.995077],
                         [ 5.612059,51.136796],
                         [ 5.016057,51.288156],
                         [ 4.420055,51.318369],
                         [ 4.178433,51.227672],
                         [ 3.920702,51.177207],
                         [ 3.485781,51.217583],
                         [ 3.179726,51.428977]]);



xdimname = 'x'
ydimname = 'y'
timedimname = 'time'
outformat="NETCDF3_CLASSIC"      # explicit classic format 
# outformat="NETCDF4"              # explicit NC4 format
#============================================================================
def find_unlimited_dimension(dsin):
    unlimdim = None
    unlimdimname = ''
    for dimname,dimobj in dsin.dimensions.iteritems():
        if (dimobj.isunlimited()):
            if (unlimdim is None):
               sys.stderr.write("Additional unlimited dimension [%s] !\n" % (dimname))
            else:
                unlimdim = dimobj
                unlimdimname = dimname
    return (dimname,dimobj)
#============================================================================
# Amersfoortse coordinaten zijn EPSG:28992

def get_projection(dsin):
    try:
       projvar=dsin.variables['projection']     # retrieve the projection variable
    except:
       sys.stderr.write("   Where is the projection variables?\n\n")
       raise
    try:
       projparam=projvar.getncattr('proj4_params')   # get projection params string
       projection=pyproj.Proj(str(projparam))
    except:
       sys.stderr.write("   Failed to create projection from proj4_params, trying proj4_origin ...\n")
       try:
          projorigin=projvar.getncattr('proj4_origin')   # get projection origin string (don't know yet what we need)
          projection=pyproj.Proj(str(projorigin))
       except:
          sys.stderr.write("   Failed to create projection from proj4_origin, !! Quitting ...\n")
          raise
    return(projection) 



def make_lat_lon(dsin,poly):
    # 1 - Check for x and y projected coordinates
    # todo: more flexible way to infer dimensions and x and y and dimensions to match the variable of interest
    polylon=  poly[:,0] 
    polylat=  poly[:,1] 
    xname='x'
    yname='y'

    x=dsin.variables[xname]                  # retrieve x-variable, assumed one dimensional
    y=dsin.variables[yname]                  # retrieve y-variable

    # 2 - Make sure we have 2D X and Y arrays
    if (len(x.dimensions)!=len(y.dimensions)):
       sys.stderr.write("   x and y have different dimensions ...\n")
       raise
    projection = get_projection(dsin)
    [polyx,polyy] = projection(polylon,polylat)
    polyxmax=np.max(polyx)
    polyxmin=np.min(polyx)
    polyymax=np.max(polyy)
    polyymin=np.min(polyy)
   
    if (len(x.dimensions)==1):                 # if one-dimensional x and y (orthogonal)
       # project lat-lon polygon of interest and find the range of spatial indices
       i1 = np.argmin(np.ma.masked_array(x,mask=(x<polyxmax)))
       i0 = np.argmax(np.ma.masked_array(x,mask=(x>polyxmin)))
       j1 = np.argmin(np.ma.masked_array(y,mask=(y<polyymax)))
       j0 = np.argmax(np.ma.masked_array(y,mask=(y>polyymin)))

       [xx,yy] = np.meshgrid(x[min(i0,i1):max(i0,i1)],y[min(j0,j1):max(j0,j1)])              # make a meshgrid (multiply)
    else:                                      # assume two-dimensional
       if (np.shape(x[:])!=np.shape(y[:])):
          sys.stderr.write("   x and y have different shapes ...\n")
          raise
       i0 = 0
       i1 = np.shape(x[:])[0]
       i0 = 0
       j1 = np.shape(x[:])[1]
       xx = x[i0:i1,j0:j1]
       yy = y[i0:i1,j0:j1]

    # 3 - Find the projection -> proj4 parameter string and create a projection instance
    projection=get_projection(dsin)

    # 4 - Apply inverse transformation (produce lat and lon values)
    [lon,lat] = projection(xx,yy,inverse=True)
    return([lon,lat,min(i0,i1),max(i0,i1),min(j0,j1),max(j0,j1)])


def add_lat_lon(dsout,lon,lat,ni,nj,xdimname,ydimname):
    # 1 - Check for x and y projected coordinates
    # todo: more flexible way to infer dimensions and x and y and dimensions to match the variable of interest
    latlondatatype = np.float64              # f4, f8 or d

    # 5 - Create lon and lat variables for the output
    dims=(ydimname,xdimname)
    try:
       ncatts={}
       ncatts['_FillValue'] = 9.96921e+36

       lonvar = dsout.createVariable('lon', latlondatatype,  dimensions=dims)
       ncatts['units'] = "degrees_east"
       ncatts['long_name'] = "longitude"
       lonvar.setncatts(ncatts)
       lonvar[:] = lon

       latvar = dsout.createVariable('lat', latlondatatype,  dimensions=dims)
       ncatts['units'] = "degrees_north"
       ncatts['long_name'] = "latitude"
       latvar.setncatts(ncatts)
       latvar[:] = lat
       sys.stderr.write("Lon and lat variables succesfully added ...\n")
    except:
       sys.stderr.write("Creating the lon and lat variables has failed ...\n")
       raise

#============================================================================

def find_extents(lon, lat):
    if (np.shape(lon)!=np.shape(lat)):
        raise Exception("Lon and Lat have incompatible shapes!")
        return

    n,m = np.shape(lon)             # assume same shape
#============================================================================

#def download(url):
#    webFile = ul.urlopen(url)
#    localFile = open(url.split('/')[-1], 'w')
#    localFile = open("./temp_nc4.nc",'w')
#    localFile.write(webFile.read())
#    webFile.close()
#    localFile.close()

def download(url):
    opener = ul.URLopener()
    opener.retrieve(url, "./temp_nc4.nc")

def clone(dsin, dsout, lon, lat, i0, i1, j0, j1):
    # Copy dimensions
    global xdimname
    global ydimname
    global timedimname
    global outformat
    xdimlen = i1 - i0
    ydimlen = j1 - j0
    for dname, the_dim in dsin.dimensions.iteritems():
        dimlen = len(the_dim)
        if (dname==xdimname):
            dimlen = xdimlen
        if (dname==ydimname):
            dimlen = ydimlen
        if (dname==timedimname):
            dimlen = None
        dsout.createDimension(dname, dimlen if not the_dim.isunlimited() else None)
    
    # Copy global attributes
    global_atts = ({k: dsin.getncattr(k) for k in dsin.ncattrs()})
    dsout.setncatts(global_atts)
    
    # Copy variables

    varslices_in = {}                           # store slice objects for variables in dictionary under varname
    varslices_out = {}                          # store slice objects for variables in dictionary under varname
    for v_name, varin in dsin.variables.iteritems():
        v_name_out = v_name
        varout_datatype = varin.datatype
        if (outformat=='NETCDF3_CLASSIC'):
           if (varin.datatype=='uint16'): varout_datatype='int16'

        outVar = dsout.createVariable(v_name_out, varout_datatype, varin.dimensions)
        # create ranges for this variable
        slce_in=[]
        slce_out=[]
        for dimname in varin.dimensions:
           if (dimname==xdimname):
               slce_in.append(np.s_[i0:i1])     # trim the 'x'-dimension
           elif (dimname==ydimname):
               slce_in.append(np.s_[j0:j1])     # trim the 'y'-dimension        
           else:
               slce_in.append(np.s_[:])         # something else, keep original range

           if (dimname==timedimname):
               slce_out.append(np.s_[-1])        # time-dimension, mark
           else:
               slce_out.append(np.s_[:])         # something else, keep original range

        has_x_dimension = (xdimname in varin.dimensions)
        has_y_dimension = (ydimname in varin.dimensions)
        is_spatial = has_x_dimension and has_y_dimension
        is_temporal = (timedimname in varin.dimensions)
        if (is_temporal):          # copy contents to the new file
           varslices_in[v_name] = slce_in  # Only keep slices of time-dependent variables, as lists 
           varslices_out[v_name] = slce_out  # Only keep slices of time-dependent variables, as lists 
        else:
           outVar[:] = varin[tuple(slce_in)]   # Only copy data for time-INdependent variables in this stage
        if (is_spatial):
           print v_name+" is a spatial var with dimensions "
           print varin.dimensions

        # Copy variable attributes
       
        ncatts = {}
        for attrib_name in varin.ncattrs():
           attrib_value = varin.getncattr(attrib_name)
           # Fillvalue and offset in the same type as the newly created datatype
           if (attrib_name in ['_FillValue','add_offset']):
              attrib_value = attrib_value.astype(outVar.datatype.name)
           ncatts[attrib_name] = attrib_value
        outVar.setncatts(ncatts)

    # adding lat-lon
    sys.stderr.write("Adding lat-lon coordinates!\n")
    add_lat_lon(dsout,lon,lat,xdimlen,ydimlen,xdimname,ydimname)

    return (varslices_in,varslices_out)
           
    
####################################################################################################

first = sys.argv[1]
last = sys.argv[2]
#dminutes = int(sys.argv[3])
dminutes = 5                        # related to the file type

#first=dt.datetime.strptime("20060606:1223","%Y%m%d:%H%M")
date0=dt.datetime.strptime(first,"%Y%m%d:%H%M")
date1=dt.datetime.strptime(last,"%Y%m%d:%H%M")

#URL = baseURL+"/%s_NC/%04d/%02d/%s_%04d%02d%02d%02d%02d.nc" % (productname,year,month,productname,year,month,day,hour,minute) 
dtime = dt.timedelta(minutes=dminutes)
a = date0 
filecount = 0

dsout = nc.Dataset(outname,"w",format=outformat)

#imedimname = 'time'
timendx = 0
while(a<date1):
    # get data from this date
    a = a + dtime
    URL = baseURL+"/%s_NC/%04d/%02d/%s_%04d%02d%02d%02d%02d.nc" % (productname,a.year,a.month,productname,a.year,a.month,a.day,a.hour,a.minute) 
#   sys.stdout.write("%s\n" % (URL))
    sys.stdout.write("Adding %s ...\n" % (os.path.basename(URL)))

    download(URL)
    dsin = nc.Dataset("./temp_nc4.nc","r")
 
    
    #start processing the temporary file, add coordinates and add the field to a 
    if (filecount==0):
        print "Setting up output, first file"
        [lon,lat,i0,i1,j0,j1] = make_lat_lon(dsin,poly)
        varslices_in, varslices_out = clone(dsin,dsout,lon,lat,i0,i1,j0,j1)
    ntime = len(dsin.dimensions[timedimname])
    timeslice = np.s_[timendx:timendx+ntime]

    for v_name, slc in varslices_out.iteritems():
        slc_out = [timeslice if x==-1 else x for x in slc]
        slc_in = varslices_in[v_name]
        outVar=dsout.variables[v_name]
        inVar=dsin.variables[v_name]
        outVar[slc_out] = inVar[slc_in]
         
    filecount=filecount+1
    timendx+=ntime
    dsin.close()
 
dsout.close()
sys.stdout.write("Done!\n")

    
    



