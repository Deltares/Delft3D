function fileinfo = nc_info ( ncfile )
%NC_INFO  Return information about a NetCDF file.
%   fileinfo = nc_info(ncfile) returns metadata about the netCDF file
%   ncfile.  fileinfo is a structure with four fields.
%
%       Filename  - a string containing the name of the file.
%       Format    - a string describing the file format.
%       Dimension - a struct array describing the netCDF dimensions.
%       Dataset   - a struct array describing the netCDF variables.
%       Attribute - a struct array describing the global attributes.
%
%   Each Dimension element is itself a struct containing the following
%   fields.
%       
%        Name      - A string containing the name of the dimension.
%        Length    - A scalar value, the size of this dimension
%        Unlimited - Set to 1 if the dimension is the record dimension, set 
%                    to 0 otherwise.
%
%   Each Dataset element is itself a struct containing the following
%   fields.
%
%        Name      - A string containing the name of the variable.
%        Nctype    - A number specifying the NetCDF datatype of this
%                    variable.
%        Dimension - A cell array with the names of the dimensions upon 
%                    which this variable depends.
%        Unlimited - Either 1 if the variable has an unlimited dimension
%                    or 0 if not.
%        Size      - Array that describes the size of each dimension upon 
%                    which this dataset depends.
%        Attribute - A struct array describing the variable attributes.
%                        
%    Each Attribute element is itself a struct and contains the following 
%    fields.
%
%        Name      - A string containing the name of the attribute.
%        Value     - Either a string or a double precision value 
%                    corresponding to the value of the attribute.
%
%   Example:  Retrieve the metadata in the example file that ships with
%   R2008b.
%       info = nc_info('example.nc');
%
%   See also nc_dump.



[backend,fmt] = snc_read_backend(ncfile);

switch(backend)
	case 'tmw' 
		fileinfo = nc_info_tmw(ncfile);
	case 'java'
		fileinfo = nc_info_java(ncfile);
	case 'mexnc'
		fileinfo = nc_info_mexnc(ncfile);
    case 'tmw_hdf4'
        fileinfo = nc_info_hdf4(ncfile);
    case 'tmw_hdf4_2011a'
        fileinfo = nc_info_hdf4_sd(ncfile);
	otherwise
		error('snctools:unhandledBackend', ...
		      '%s is not a recognized backend.', backend );
end

if ~isfield(fileinfo,'Format')
    % Only supply this if not already supplied.
    fileinfo.Format = fmt;
end

return













