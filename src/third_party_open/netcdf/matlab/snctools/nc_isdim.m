function tf = nc_isdim(ncfile,dimname)
%NC_ISDIM  Determine if variable is present in file.
%
%   BOOL = NC_ISDIM(NCFILE,DIMNAME) returns true if the dimension DIMNAME is 
%   present in the file NCFILE and false if it is not.
%
%   Example (requires R2008b):
%       bool = nc_isdim('example.nc','temperature')
%       
%   See also nc_isatt, nc_isvar

try
    nc_getdiminfo(ncfile,dimname);
    tf = true;
catch
    tf = false;
end
