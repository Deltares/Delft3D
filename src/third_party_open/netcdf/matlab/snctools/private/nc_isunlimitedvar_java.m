function tf = nc_isunlimitedvar_java(ncfile,varname)     
% java backend for NC_ISUNLIMITEDVAR

try
    info = nc_getvarinfo(ncfile,varname);
catch 
    e = lasterror;
    switch ( e.identifier )
        case 'snctools:getVarInfo:badVariableName'
            tf = false;
            return
        otherwise
            error('snctools:isUnlimitedVar:unhandledCondition', e.message );
    end
end

tf = info.Unlimited;

