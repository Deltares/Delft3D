function dinfo = nc_getdiminfo_tmw ( arg1, arg2 )

% If we are here, then we must have been given something local.
if ischar(arg1) && ischar(arg2)
    dinfo = handle_char_nc_getdiminfo_tmw(arg1,arg2);
elseif isnumeric ( arg1 ) && isnumeric ( arg2 )
	dinfo = handle_numeric_nc_getdiminfo_tmw(arg1,arg2);
else
	error ( 'snctools:getdiminfo:tmw:badInputDatatypes', ...
	        'Must supply either two character or two numeric arguments.' );
end

return



%--------------------------------------------------------------------------
function dinfo = handle_char_nc_getdiminfo_tmw ( ncfile, dimname )

ncid=netcdf.open(ncfile,'NOWRITE');
try
    dimid = netcdf.inqDimID(ncid, dimname);
    dinfo = handle_numeric_nc_getdiminfo_tmw(ncid,dimid);
catch me
    netcdf.close(ncid);
    rethrow(me);
end
netcdf.close(ncid);






%--------------------------------------------------------------------------
function dinfo = handle_numeric_nc_getdiminfo_tmw ( ncid, dimid )

[dimname, dimlength] = netcdf.inqDim(ncid, dimid);
dinfo.Name = dimname;
dinfo.Length = dimlength;

v = netcdf.inqLibVers();
if v(1) == '3'
    % there can only be one unlimited dimension in v3.
    [dud,dud,dud,unlimdim] = netcdf.inq(ncid); %#ok<ASGLU>
    if dimid == unlimdim
    	dinfo.Unlimited = true;
    else
    	dinfo.Unlimited = false;
    end
else
    % there can be many unlimited dimensions in v4.
    unlimDimIDs = netcdf.inqUnlimDims(ncid);
    if ismember(dimid,unlimDimIDs)
        dinfo.Unlimited = true;
    else
        dinfo.Unlimited = false;
    end
end

return
