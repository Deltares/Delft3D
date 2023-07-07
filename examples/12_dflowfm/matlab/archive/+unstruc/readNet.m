function udata = readNet(filename)
%unstruc.readNet Reads an unstructured net.
%   udata = unstruc.readNet(filename)
%   reads the net data from a netCDF file and returns them in a struct.

%   $Id: readNet.m 9875 2009-12-10 15:30:25Z dam_ar $

inetfile = netcdf.open(filename, 'NOWRITE');

udata=struct();
udata.NetNode_x   = nc_getVarByName(inetfile, 'NetNode_x');
udata.NetNode_y   = nc_getVarByName(inetfile, 'NetNode_y');
udata.NetLink     = nc_getVarByName(inetfile, 'NetLink');
udata.NetLinkType = nc_getVarByName(inetfile, 'NetLinkType');

netcdf.close(inetfile);
end