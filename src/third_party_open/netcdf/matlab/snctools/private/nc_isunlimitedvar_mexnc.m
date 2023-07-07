function tf = nc_isunlimitedvar_mexnc(ncfile,varname)     
% mexnc backend for NC_ISUNLIMITEDVAR
try
    info = nc_getvarinfo(ncfile,varname);
catch 
    e = lasterror;
    switch ( e.identifier )
        case 'snctools:getVarInfo:mexnc:inqVarID'
            tf = false;
            return
        otherwise
            rethrow(e);
    end
end

tf = info.Unlimited;

