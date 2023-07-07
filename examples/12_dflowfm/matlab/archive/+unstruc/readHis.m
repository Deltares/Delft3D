function hisdata = readHis(varargin)
%unstruc.readHis    Read timeseries data from an Unstruc history file.
%   hisdata = unstruc.readHis(filename) reads all station names, locations
%       and waterlevel data into a struct.
%   hisdata = unstruc.readHis(filename, stationname) reads only one
%       specific station into a similar struct.

%   $Id: readHis.m 23738 2012-09-03 12:14:47Z pijl $

filename = varargin{1};

if nargin >= 2
    statname = varargin{2};
else
    statname = [];
end

ihisfile = netcdf.open(filename, 'NOWRITE');

hisdata=struct();
hisdata.time       = nc_getVarByName(ihisfile, 'time');

station_name       = nc_getVarByName(ihisfile, 'station_name');
station_x_coord    = nc_getVarByName(ihisfile, 'station_x_coordinate');
station_y_coord    = nc_getVarByName(ihisfile, 'station_y_coordinate');

if isempty(statname)
    hisdata.station_name    = station_name;
    hisdata.station_x_coord = station_x_coord;
    hisdata.station_y_coord = station_y_coord;
	hisdata.waterlevel      = nc_getVarByName(ihisfile, 'waterlevel');
	hisdata.x_velocity      = nc_getVarByName(ihisfile, 'x_velocity');
	hisdata.y_velocity      = nc_getVarByName(ihisfile, 'y_velocity');
    hisdata.cross_section_discharge = nc_getVarByName(ihisfile, 'cross_section_discharge');
    hisdata.cross_section_area      = nc_getVarByName(ihisfile, 'cross_section_area');
    hisdata.cross_section_velocity  = nc_getVarByName(ihisfile, 'cross_section_velocity');
else
    idx = [];
    for i=1:size(station_name,2)
        if strcmpi(deblank(station_name(:,i)'), statname)
            idx = i;
        end
    end
    if isempty(idx)
        warning('Station ''%s'' not found.', statname);
    else
        waterlevel              = nc_getVarByName(ihisfile, 'waterlevel');
        hisdata.station_name    = deblank(station_name(:,idx)');
        hisdata.station_x_coord = station_x_coord(idx);
        hisdata.station_y_coord = station_y_coord(idx);
        hisdata.waterlevel = waterlevel(idx,:);
    end
end
netcdf.close(ihisfile);

end