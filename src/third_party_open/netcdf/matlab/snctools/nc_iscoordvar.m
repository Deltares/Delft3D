function bool = nc_iscoordvar ( ncfile, varname )
%NC_ISCOORDVAR  Determine if variable is a coordinate variable.
%
%   BOOL = NC_ISCOORDVAR(NCFILE,VARNAME) determines if VARNAME is a
%   coordinate variable.  A coordinate variable is a variable with just one
%   dimension.  That dimension has the same name as the variable itself.
%   BOOL will be either true or false.
%
%   Example (requires 2008b or higher):
%       nc_dump('example.nc');
%       bool = nc_iscoordvar('example.nc','peaks')
%
%   See also NC_ISUNLIMITEDVAR, NC_DUMP.


% Assume that the answer is no until we know that it is yes.
bool = false;

ncvar = nc_getvarinfo ( ncfile, varname );

% Check that it's not a singleton.  If it is, then the answer is no.
if isempty(ncvar.Dimension)
	bool = false;
	return
end

% Check that the names are the same.
if strcmp ( ncvar.Dimension{1}, varname )
	bool = true;
end

return;

