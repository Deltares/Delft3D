%% Create netCDF-CF of curvilinear lat-lon grid
%
%  example of how to make a netCDF file with CF conventions of a 
%  variable that is defined on a grid that is curvi-linear
%  in a lat-lon coordinate system. In this case 
%  the dimensions (m,n) do not coincide with the coordinate axes.
%
%  This case is described in:
%  http://cf-pcmdi.llnl.gov/documents/cf-conventions/1.4/cf-conventions.html#id2984605
%  as "Two-Dimensional Latitude, Longitude, Coordinate Variables".
%
%  An example of a curvi-linear lat,lon grid is for instance a satellite 
%  image: a digital image with a constant width in terms of number of 
%  pixels ncols (swath), and a length nrows determined by how long a ground 
%  station can receive the satellite above the (electro-magnetic) horizon.
%
%    ^     latitude (degrees_north)    
%    |            
%    |              /  /\                       
%    |ncols (Track)/  /15\      \               
%    |            /  /    \      \                    
%    |           /  /10    \      \                  
%    |          /  /        \      \                
%    |            (5       14\      \              
%    |             \    9     \      \            
%    |              \4         \      |           
%    |               \          \     |           
%    |                )3   8   13)    | nrows (Xtrack)    
%    |               /          /     |           
%    |              /2         /      |           
%    |             /    7     /      /            
%    |            (1       12/      /              
%    |             \        /      /                
%    |              \6     /      /                  
%    |               \    /      /                    
%    |                \11/      / 
%    |                 \/
%    +----------------------> longitude 
%                        (degrees_east) 
%
% In MATLAB, a 2D variable varies fastest along the rows, so we will borrow 
% from HDF-EOS swath terminology and call that dimension 'Xtrack'.  The 
% along track dimension would then be along the columns and we will call 
% this 'Track'.  This will determine our order of dimensions.  Note that if
% we use row-major order conventions, the order of dimensions will be
% reversed.
%
% Note that ncBrowse does not contain plot support for 
% curvi-linear grids, so ncBrowse will display the same 
% rectangular plot as for the netCDF file created by
% NC_CF_GRID_WRITE_LAT_LON_ORTHOGONAL_TUTORIAL, albeit with
% different axes annotations (col/row instead of lat/lon).
%
%See also: SNCTOOLS, NC_CF_GRID_WRITE_LAT_LON_ORTHOGONAL_TUTORIAL, 

%% Define meta-info: global

   OPT.title                  = 'Example of curvilinear grid conforming to CF conventions.';
   OPT.institution            = 'TU Delpht';
   OPT.source                 = 'example data';
   OPT.history                = ['tranformation to netCDF: $HeadURL$'];
   OPT.references             = 'http://cf-pcmdi.llnl.gov/documents/cf-conventions/1.4/cf-conventions.html';
   OPT.email                  = 'john.g.evans.ne@gmail.com';
   OPT.comment                = 'Adapted from example provided by Gerben de Boer.';
   OPT.version                = '1.0';
   OPT.acknowledge            =['These data can be used freely for research purposes provided that the following source is acknowledged: ',OPT.institution];
   OPT.disclaimer             = 'This data is made available in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.';
   
%% Define dimensions/coordinates: lat,lon matrices

   lon1                       = [2 4 6];
   lat1                       = [50 51 52 53 54];
  [lat2,lon2]                 = ndgrid(lat1,lon1);
   ang                        = [-15 -15 -15; 0 0 0; 15 15 15; 30 30 30; 45 45 45];
   OPT.lat                    = lat2 + sind(ang).*lon2./2;
   OPT.lon                    = lon2 + cosd(ang).*lon2./2; clear lon1 lon2 lat1 lat2

   OPT.ncols                  = size(OPT.lon,2);
   OPT.nrows                  = size(OPT.lat,1);
   OPT.lat_type               = 'single'; % 'single', 'double' for high-resolution data (eps 1m)
   OPT.lon_type               = 'single'; % 'single', 'double' for high-resolution data (eps 1m)

   OPT.wgs84.code             = 4326;     % epsg code of global grid: http://www.epsg-registry.org/
   OPT.wgs84.name             = 'WGS 84';
   OPT.wgs84.semi_major_axis  = 6378137.0;
   OPT.wgs84.semi_minor_axis  = 6356752.314247833;
   OPT.wgs84.inv_flattening   = 298.2572236;
      
%% Define variable (define some data)

   OPT.val                    = [1 2 3 4 5;6 7 8 9 10;11 12 nan 14 15]'; % use ncols as 1st array dimension to get correct plot in ncBrowse (snctools swaps for us)
   OPT.varname                = 'depth';       % free to choose: will appear in netCDF tree
   OPT.units                  = 'm';           % from UDunits package: http://www.unidata.ucar.edu/software/udunits/
   OPT.long_name              = 'bottom depth';% free to choose: will appear in plots
   OPT.standard_name          = 'sea_floor_depth_below_geoid'; % or 'altitude'
   OPT.val_type               = 'single';      % 'single' or 'double'
   OPT.fillvalue              = single(-9999);
      
%% 1.a Create netCDF file

   ncfile = fullfile(fileparts(mfilename('fullpath')),[mfilename,'.nc']);
   
   nc_create_empty (ncfile)
   
%% 1.b Add overall meta info
%  http://cf-pcmdi.llnl.gov/documents/cf-conventions/1.4/cf-conventions.html#description-of-file-contents
   
   nc_attput(ncfile, nc_global, 'title'         , OPT.title);
   nc_attput(ncfile, nc_global, 'institution'   , OPT.institution);
   nc_attput(ncfile, nc_global, 'source'        , OPT.source);
   nc_attput(ncfile, nc_global, 'history'       , OPT.history);
   nc_attput(ncfile, nc_global, 'references'    , OPT.references);
   nc_attput(ncfile, nc_global, 'email'         , OPT.email);
   
   nc_attput(ncfile, nc_global, 'comment'       , OPT.comment);
   nc_attput(ncfile, nc_global, 'version'       , OPT.version);
   					   
   nc_attput(ncfile, nc_global, 'Conventions'   , 'CF-1.4');
   nc_attput(ncfile, nc_global, 'CF:featureType', 'Grid');  % https://cf-pcmdi.llnl.gov/trac/wiki/PointObservationConventions
   
   nc_attput(ncfile, nc_global, 'terms_for_use' , OPT.acknowledge);
   nc_attput(ncfile, nc_global, 'disclaimer'    , OPT.disclaimer);
      
%% 2 Create matrix span dimensions
%    http://cf-pcmdi.llnl.gov/documents/cf-conventions/1.4/cf-conventions.html#dimensions   

   nc_adddim(ncfile, 'Xtrack', OPT.nrows); % !!! use this as 1st array dimension to get correct plot in ncBrowse (snctools swaps for us)
   nc_adddim(ncfile, 'Track', OPT.ncols); % !!! use this as 2nd array dimension to get correct plot in ncBrowse (snctools swaps for us)
   
   % You might insert a vector 'col' that runs [OPT.ncols:-1:1] to have
   % the arcGIS ASCII file approach of having upper-left corner of 
   % the data matrix at index (1,1) rather than the default of having the 
   % lower-left corner of the data matrix  at index (1,1).

%  nc_add_dimension(ncfile, 'time', 1); % if you would like to include more instances of the same grid, 
                                        % you can optionally use 'time' as a 3rd dimension.
                                        % T,(Z),Y,X is the recommended dimension CF order.          

%% 3.a Create coordinate variables: longitude
%      http://cf-pcmdi.llnl.gov/documents/cf-conventions/1.4/cf-conventions.html#longitude-coordinate

   clear nc;ifld = 1;
   nc(ifld).Name             = 'lon';
   nc(ifld).Datatype         = OPT.lon_type;
   nc(ifld).Dimension        = {'Xtrack','Track'}; 
   nc(ifld).Attribute(    1) = struct('Name', 'long_name'      ,'Value', 'longitude');
   nc(ifld).Attribute(end+1) = struct('Name', 'units'          ,'Value', 'degrees_east');
   nc(ifld).Attribute(end+1) = struct('Name', 'standard_name'  ,'Value', 'longitude');
   nc(ifld).Attribute(end+1) = struct('Name', 'actual_range'   ,'Value', [min(OPT.lon(:)) max(OPT.lon(:))]);
   nc(ifld).Attribute(end+1) = struct('Name', 'coordinates'    ,'Value', 'lat lon'); % !!! lon matrix can be plotted as a function of lat and itself
   nc(ifld).Attribute(end+1) = struct('Name', 'grid_mapping'   ,'Value', 'wgs84');

%% 3.b Create coordinate variables: latitude
%      http://cf-pcmdi.llnl.gov/documents/cf-conventions/1.4/cf-conventions.html#latitude-coordinate
   
   ifld = ifld + 1;
   nc(ifld).Name             = 'lat';
   nc(ifld).Datatype         = OPT.lat_type;
   nc(ifld).Dimension        = {'Xtrack','Track'}; 
   nc(ifld).Attribute(    1) = struct('Name', 'long_name'      ,'Value', 'latitude');
   nc(ifld).Attribute(end+1) = struct('Name', 'units'          ,'Value', 'degrees_north');
   nc(ifld).Attribute(end+1) = struct('Name', 'standard_name'  ,'Value', 'latitude');
   nc(ifld).Attribute(end+1) = struct('Name', 'actual_range'   ,'Value', [min(OPT.lat(:)) max(OPT.lat(:))]);
   nc(ifld).Attribute(end+1) = struct('Name', 'coordinates'    ,'Value', 'lat lon'); % !!! lat matrix can be plotted as a function of lon and itself
   nc(ifld).Attribute(end+1) = struct('Name', 'grid_mapping'   ,'Value', 'wgs84');

%% 3.c Create coordinate variables: coordinate system: WGS84 default
%      global ellispes: WGS 84, ED 50, INT 1924, ETRS 89 and the upcoming ETRS update etc.
%      http://cf-pcmdi.llnl.gov/documents/cf-conventions/1.4/cf-conventions.html#grid-mappings-and-projections
%      http://cf-pcmdi.llnl.gov/documents/cf-conventions/1.4/cf-conventions.html#appendix-grid-mappings

   ifld = ifld + 1;
   nc(ifld).Name         = 'wgs84'; % preferred
   nc(ifld).Datatype     = nc_int;
   nc(ifld).Dimension    = {};
   nc(ifld).Attribute    = struct('Name', ...
    {'name',...
     'epsg',...
     'grid_mapping_name',...
     'semi_major_axis', ...
     'semi_minor_axis', ...
     'inverse_flattening', ...
     'comment'}, ...
     'Value', ...
     {OPT.wgs84.name,...
      OPT.wgs84.code,...
     'latitude_longitude',...
      OPT.wgs84.semi_major_axis, ...
      OPT.wgs84.semi_minor_axis, ...
      OPT.wgs84.inv_flattening,  ...
     'value is equal to EPSG code'});

%% 4   Create dependent variable
%      http://cf-pcmdi.llnl.gov/documents/cf-conventions/1.4/cf-conventions.html#variables
%      Parameters with standard names:
%      http://cf-pcmdi.llnl.gov/documents/cf-standard-names/standard-name-table/current/

   ifld = ifld + 1;
   nc(ifld).Name             = OPT.varname;
   nc(ifld).Datatype         = OPT.val_type;
   nc(ifld).Dimension        = {'Xtrack','Track'}; % {'time','col','row'}
   nc(ifld).Attribute(    1) = struct('Name', 'long_name'      ,'Value', OPT.long_name    );
   nc(ifld).Attribute(end+1) = struct('Name', 'units'          ,'Value', OPT.units        );
   nc(ifld).Attribute(end+1) = struct('Name', '_FillValue'     ,'Value', OPT.fillvalue    );
   nc(ifld).Attribute(end+1) = struct('Name', 'actual_range'   ,'Value', [min(OPT.val(:)) max(OPT.val(:))]);
   nc(ifld).Attribute(end+1) = struct('Name', 'coordinates'    ,'Value', 'lat lon');
   nc(ifld).Attribute(end+1) = struct('Name', 'grid_mapping'   ,'Value', 'wgs84');
   if ~isempty(OPT.standard_name)
   nc(ifld).Attribute(end+1) = struct('Name', 'standard_name'  ,'Value', OPT.standard_name);
   end
      
%% 5.a Create all variables with attributes
   
   for ifld=1:length(nc)
      nc_addvar(ncfile, nc(ifld));   
   end
      
%% 5.b Fill all variables

   nc_varput(ncfile, 'lon'          , OPT.lon       );
   nc_varput(ncfile, 'lat'          , OPT.lat       );
   nc_varput(ncfile, 'wgs84'        , OPT.wgs84.code);
   nc_varput(ncfile, OPT.varname    , OPT.val       );
      
%% 6   Check file summary
   
   nc_dump(ncfile);
   fid = fopen(fullfile(fileparts(mfilename('fullpath')),[mfilename,'.cdl']),'w');
   nc_dump(ncfile,fid);
   fclose(fid)

%% 7   Load the data: using the variable names from nc_dump

   Da.dep   = nc_varget(ncfile,'depth');
   Da.lat   = nc_varget(ncfile,'lon');
   Da.lon   = nc_varget(ncfile,'lat')
