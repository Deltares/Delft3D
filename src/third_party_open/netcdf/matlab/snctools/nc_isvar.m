function tf = nc_isvar(ncfile,varname)
%NC_ISVAR  Determine if variable is present in file.
%
%   BOOL = NC_ISVAR(NCFILE,VARNAME) returns true if the variable VARNAME is 
%   present in the file NCFILE and false if it is not.
%
%   Example (requires R2008b):
%       bool = nc_isvar('example.nc','temperature')
%       
%   See also nc_isatt, nc_isdim.

% Both inputs must be character
if nargin ~= 2
	error ( 'snctools:isvar:badInput', 'must have two inputs' );
end
if ~ ( ischar(ncfile) || isa(ncfile,'ucar.nc2.NetcdfFile') || isa(ncfile,'ucar.nc2.dods.DODSNetcdfFile') )
	error ( 'snctools:isvar:badInput', 'first argument must be character or a JAVA netCDF file object.' );
end
if ~ischar(varname)
	error ( 'snctools:isvar:badInput', 'second argument must be character.' );
end


retrieval_method = snc_read_backend(ncfile);

switch(retrieval_method)
	case 'tmw'
		tf = nc_isvar_tmw(ncfile,varname);
	case 'java'
		tf = nc_isvar_java(ncfile,varname);
	case 'mexnc'
		tf = nc_isvar_mexnc(ncfile,varname);
    case 'tmw_hdf4'
        tf = nc_isvar_hdf4(ncfile,varname);
    case 'tmw_hdf4_2011a'
        tf = nc_isvar_hdf4(ncfile,varname);
	otherwise
		error ( 'snctools:isvar:unrecognizedCase', ...
		        '%s is not recognized method for NC_ISVAR.', retrieval_method );
end






%--------------------------------------------------------------------------
function bool = nc_isvar_hdf4_2011a(hfile,varname)
import matlab.io.hdf4.*

sd_id = sd.start(hfile,'read');

try 
    idx = sd.nameToIndex(sd_id,varname);
    bool = true;
catch
    bool = false;
end

sd.close(sd_id);


 

%--------------------------------------------------------------------------
function bool = nc_isvar_hdf4(hfile,varname)
bool = true;
sd_id = hdfsd('start',hfile,'read');
if sd_id < 0
    error('snctools:attget:hdf4:start', 'START failed on %s.', hfile);
end


idx = hdfsd('nametoindex',sd_id,varname);
if idx < 0
    bool = false;
end

hdfsd('end',sd_id);


 

%-----------------------------------------------------------------------
function bool = nc_isvar_mexnc ( ncfile, varname )

[ncid,status] = mexnc('open',ncfile, nc_nowrite_mode );
if status ~= 0
	ncerr = mexnc ( 'STRERROR', status );
	error('snctools:isvar:mexnc:open', ncerr );
end


[varid,status] = mexnc('INQ_VARID',ncid,varname);
if ( status ~= 0 )
	bool = false;
else 
	bool = true;
end

mexnc('close',ncid);
return








%--------------------------------------------------------------------------
function bool = nc_isvar_java ( ncfile, varname )
% assume false until we know otherwise
bool = false;

import ucar.nc2.dods.*     
import ucar.nc2.*         
                         
close_it = true;


% Try it as a local file.  If not a local file, try as
% via HTTP, then as dods
if isa(ncfile,'ucar.nc2.NetcdfFile')
	jncid = ncfile;
	close_it = false;
elseif isa(ncfile,'ucar.nc2.dods.DODSNetcdfFile')
	jncid = ncfile;
	close_it = false;
elseif exist(ncfile,'file')
	jncid = NetcdfFile.open(ncfile);
else
	try 
		jncid = NetcdfFile.open ( ncfile );
	catch %#ok<CTCH>
		try
			jncid = DODSNetcdfFile(ncfile);
		catch %#ok<CTCH>
			error ( 'snctools:isvar:fileOpenFailure', ...
                 'Could not open ''%s'' as either a local file, a regular URL, or as a DODS URL.', ...
                 ncfile);
		end
	end
end




jvarid = jncid.findVariable(varname);

%
% Did we find anything?
if ~isempty(jvarid)
	bool = true;
end

if close_it
	close(jncid);
end

return

