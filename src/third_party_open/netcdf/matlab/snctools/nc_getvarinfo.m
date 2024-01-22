function info = nc_getvarinfo(ncfile,varname,field)
%NC_GETVARINFO  Returns metadata about a specific NetCDF variable.
%
%   INFO = NC_GETVARINFO(NCFILE,VARNAME) returns a metadata structure 
%   about the variable VARNAME in the netCDF file NCFILE.
%
%   INFO will have the following fields:
%
%       Name      - A string containing the name of the variable.
%       Datatype  - The datatype of the variable.
%       Unlimited - Either 1 if the variable has an unlimited dimension or 
%                   0 if not.
%       Dimension - a cell array with the names of the dimensions upon 
%                   which this variable depends.
%       Size      - Size of the variable.
%       Attribute - An array of structures corresponding to the attributes 
%                   defined for the specified variable.
%                         
%   INFO = NC_GETVARINFO((NCFILE,VARNAME,<'field'>) returns only 
%   one of the above fields: Datatype, Unlimited, Dimension, 
%   Size, Attribute. Handy for use in expressions.
%
%   Each "Attribute" element is a struct itself and contains the following 
%   fields.
%
%       Name      - A string containing the name of the attribute.
%       Datatype  - The datatype of the variable.
%       Value     - Value of the attribute.
%
%   Example:  requires R2008b or higher.
%       info = nc_getvarinfo('example.nc','temperature')
%
%   Example:  requires R2008b or higher.
%       info = nc_getvarinfo('example.nc','temperature','Unlimited')
%
%   See also:  NC_INFO, NC_GETDIMINFO.

if isnumeric(ncfile)
    warning('snctools:nc_getvarinfo:deprecatedSyntax', ...
            'Using numeric IDs as arguments to NC_GETVARINFO is a deprecated syntax.');
end

backend = snc_read_backend(ncfile);
switch(backend)
	case {'tmw', 'tmw_enhanced_h5'}
		info = nc_getvarinfo_tmw(ncfile,varname);
	case 'java'
		info = nc_getvarinfo_java(ncfile,varname);
	case 'mexnc'
		info = nc_getvarinfo_mexnc(ncfile,varname);
    case 'tmw_hdf4'
        info = nc_getvarinfo_hdf4(ncfile,varname);
    case 'tmw_hdf4_2011a'
        info = nc_getvarinfo_hdf4_2011a(ncfile,varname);
	otherwise
		error('snctools:unhandledBackend', ...
		      '%s is not a recognized backend.', backend);
end


if nargin > 2
   info = info.(field); 
end
