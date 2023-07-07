function udata = readMap(filename)
%unstruc.readMap Reads solution data on an unstructured net.


%   udata = unstruc.readNet(filename) reads the net and flow data from a
%   netCDF file.

%   $Id: readMap.m 27177 2013-03-25 16:25:05Z pijl $

imapfile = netcdf.open(filename, 'NOWRITE');

udata=struct();
udata.NetNode_x   = nc_getVarByName(imapfile, 'NetNode_x');
udata.NetNode_y   = nc_getVarByName(imapfile, 'NetNode_y');
udata.NetLink     = nc_getVarByName(imapfile, 'NetLink');
udata.NetLinkType = nc_getVarByName(imapfile, 'NetLinkType');


udata.FlowElem_xcc      = nc_getVarByName(imapfile, 'FlowElem_xcc');
udata.FlowElem_ycc      = nc_getVarByName(imapfile, 'FlowElem_ycc');
%udata.NetCellNode      = nc_getVarByName(imapfile, 'NetCellNode');
udata.FlowElemContour_x = nc_getVarByName(imapfile, 'FlowElemContour_x');
udata.FlowElemContour_y = nc_getVarByName(imapfile, 'FlowElemContour_y');
udata.FlowLink          = nc_getVarByName(imapfile, 'FlowLink');
%udata.NetCellLink1D   = nc_getVarByName(imapfile, 'NetCellLink1D');
udata.FlowElemDomain    = double(nc_getVarByName(imapfile, 'FlowElemDomain')');
udata.FlowLinkDomain    = double(nc_getVarByName(imapfile, 'FlowLinkDomain')');

udata.time  = nc_getVarByName(imapfile, 'time'); % Time
udata.bl    = nc_getVarByName(imapfile, 'FlowElem_bl'); % Bottom level
udata.s1    = nc_getVarByName(imapfile, 's1');   % Waterlevel
udata.sa1   = nc_getVarByName(imapfile, 'sa1');  % Salinity
udata.ucx   = nc_getVarByName(imapfile, 'ucx');  % Horizontal velocity at cell center
udata.ucy   = nc_getVarByName(imapfile, 'ucy');  % Vertical velocity at cell center

netcdf.close(imapfile);
end