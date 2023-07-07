function tf = nc_isatt(ncfile,varname,attrname)
%NC_ISATT:  determines if an attribute is present in a netCDF file.
%
%   BOOL = NC_ISATT(NCFILE,VARNAME,ATTRNAME) returns true if the attribute
%   specified by VARNAME and ATTRNAME is present in the given file.   Use
%   nc_global to specify a global attribute.
%
%   Example:  Determine if the global attribute 'creation_date' is present
%   in the example file shipped with R2008b.
%       bool = nc_isatt('example.nc',nc_global,'creation_date);
%
%   See also nc_isvar, nc_isdim.

retrieval_method = snc_read_backend(ncfile);
switch(retrieval_method)
    case 'tmw'
        tf = nc_isatt_tmw(ncfile,varname,attrname);
    case 'tmw_hdf4'
        tf = nc_isatt_tmw_hdf4(ncfile,varname,attrname);
    case 'tmw_hdf4_2011a'
        tf = nc_isatt_tmw_hdf4_2011a(ncfile,varname,attrname);
    case 'java'
        tf = nc_isatt_java(ncfile,varname,attrname);
    case 'mexnc'
        tf = nc_isatt_mexnc(ncfile,varname,attrname);
    otherwise
        error ('snctools:isatt:unrecognizedCase', ...
               '%s is not recognized backend for NC_ISATT.', retrieval_method );
end





%-----------------------------------------------------------------------
function bool = nc_isatt_tmw_hdf4(hfile,varname,attrname)
sd_id = hdfsd('start',hfile,'read');
if sd_id < 0
    error('snctools:isatt:hdf4:start', 'START failed on %s.', hfile);
end

if isnumeric(varname);
    obj_id = sd_id;
else

    idx = hdfsd('nametoindex',sd_id,varname);
    if idx < 0
        error('snctools:isatt:hdf4:nametoindex', ...
              'NAMETOINDEX failed on %s, %s.', varname, hfile);
    end

    sds_id = hdfsd('select',sd_id,idx);
    if sds_id < 0
        error('snctools:isatt:hdf4:select', ...
              'SELECT failed on %s, %s.', varname, hfile);
    end

    obj_id = sds_id;
end

attr_idx = hdfsd('findattr',obj_id,attrname);
if attr_idx < 0
    bool = false;
else
    bool = true;
end

if ischar(varname);
    status = hdfsd('endaccess',sds_id);
    if status < 0
      error('snctools:isatt:hdf4:endaccess', ...
            'ENDACCESS failed on %s.', varname);
    end
end

status = hdfsd('end', sd_id);
if status < 0
    error('snctools:isatt:hdf4:end', 'END failed on %s.', hfile);
end

return

%-----------------------------------------------------------------------
function bool = nc_isatt_tmw_hdf4_2011a(hfile,varname,attrname)

import matlab.io.hdf4.*

sd_id = sd.start(hfile,'read');

if isnumeric(varname);
    obj_id = sd_id;
else

    try
        idx = sd.nameToIndex(sd_id,varname);
        sds_id = sd.select(sd_id,idx);
        obj_id = sds_id;
    catch
        obj_id = sd_id;
    end

end

try
    attr_idx = sd.findAttr(obj_id,attrname);
    bool = true;
catch
    bool = false;
end

if exist('sds_id','var')
    sd.endAccess(sds_id);
end
sd.close(sd_id);

return

%-----------------------------------------------------------------------
function bool = nc_isatt_tmw(ncfile,varname,attrname)

bool = true;

ncid = netcdf.open(ncfile,'nowrite');
try
    if ischar(varname)
        varid = netcdf.inqVarID(ncid,varname);
    else
        varid = varname;
    end
catch me
    netcdf.close(ncid);
    rethrow(me);
end

try
    attid = netcdf.inqAttID(ncid,varid,attrname);
catch me
    bool = false;
end

netcdf.close(ncid);
return

%-----------------------------------------------------------------------
function bool = nc_isatt_mexnc(ncfile,varname,attrname)

[ncid,status] = mexnc('open',ncfile, nc_nowrite_mode );
if status ~= 0
    ncerr = mexnc ( 'STRERROR', status );
    error ( 'snctools:isatt:mexnc:open', ncerr );
end

if ischar(varname)
    [varid,status] = mexnc('INQ_VARID',ncid,varname);
    if status ~= 0
        mexnc('close',ncid);
        ncerr = mexnc ( 'STRERROR', status );
        error ( 'snctools:isatt:mexnc:inqVaridFailed', ncerr );
    end
else
    varid = varname;
end

[att_id, status] = mexnc('INQ_ATTID',ncid,varid,attrname);
if status == 0
    bool = true;
else
    bool = true;
end


status = mexnc('close',ncid);
if status ~= 0
    ncerr = mexnc ( 'STRERROR', status );
    error ( 'snctools:isatt:mexnc:closeFailed', ncerr );
end
return







%--------------------------------------------------------------------------
function bool = nc_isatt_java(ncfile,varname,attrname)


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
            error('snctools:isatt:fileOpenFailure', ...
                  'Could not open ''%s'' as either a local file, a regular URL, or as a DODS URL.', ...
                  ncfile);
        end
    end
end




if ischar ( varname )

    jvarid = jncid.findVariable(varname);

    % Did we find anything?
    if isempty(jvarid)
        if close_it
            close(jncid);
        end
        error('snctools:isatt:java:varNotFound', ...
              'Could not find attribute %s for variable %s in file %s.', ...
              attrname, varname, ncfile);
    end

    % Ok, it was just a regular variable.
    jvarid = jncid.findVariable(varname);
    jatt = jvarid.findAttribute(attrname);


else

    % The user passed a numeric identifier for the variable.
    % Assume that this means a global attribute.
    jatt = jncid.findGlobalAttribute(attrname);
end


if isempty(jatt)
    bool = false;
else
    bool = true;
end



if close_it
    close(jncid);
end

return
