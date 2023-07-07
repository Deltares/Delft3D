function dinfo = nc_getdiminfo_mexnc(arg1,arg2)
% MEXNC backend for NC_GETDIMINFO

% If we are here, then we must have been given something local.
if ischar(arg1) && ischar(arg2)
    dinfo = handle_char_nc_getdiminfo(arg1,arg2);
elseif isnumeric(arg1) && isnumeric(arg2)
	dinfo = handle_numeric_nc_getdiminfo(arg1,arg2);
else
	error('snctools:getdiminfo_MEX:badInputDatatypes', ...
	      'Must supply either two character or two numeric arguments.');
end

return


%--------------------------------------------------------------------------
function dinfo = handle_char_nc_getdiminfo(ncfile,dimname)

[ncid,status ]=mexnc('open', ncfile, nc_nowrite_mode );
if status ~= 0
	ncerror = mexnc ( 'strerror', status );
	error ( 'snctools:getdiminfo:mexnc:openFailed', ncerror );
end


[dimid, status] = mexnc('INQ_DIMID', ncid, dimname);
if ( status ~= 0 )
	mexnc('close',ncid);
	ncerror = mexnc ( 'strerror', status );
	error ( 'snctools:getdiminfo:mexnc:inq_dimidFailed', ncerror );
end


dinfo = handle_numeric_nc_getdiminfo(ncid,dimid);

mexnc('close',ncid);






%--------------------------------------------------------------------------
function dinfo = handle_numeric_nc_getdiminfo(ncid,dimid)

[unlimdim, status] = mexnc ( 'inq_unlimdim', ncid );
if status ~= 0
	mexnc('close',ncid);
	ncerror = mexnc ( 'strerror', status );
	error ( 'snctools:getdiminfo:MEXNC:inq_ulimdimFailed', ncerror );
end

[dimname, dimlength, status] = mexnc('INQ_DIM', ncid, dimid);
if status ~= 0
	mexnc('close',ncid);
	ncerror = mexnc ( 'strerror', status );
	error ( 'snctools:getdiminfo:MEXNC:inq_dimFailed', ncerror );
end

dinfo.Name = dimname;
dinfo.Length = dimlength;

if dimid == unlimdim
	dinfo.Unlimited = true;
else
	dinfo.Unlimited = false;
end

return


