function tf = nc_isunlimitedvar_tmw(ncfile,varname)     
% TMW backend for NC_ISUNLIMITEDVAR
try
    info = nc_getvarinfo(ncfile,varname);
catch me
    switch(me.identifier)
        case {'snctools:getvarinfo:tmw:badTypes'}
            % Some sort of bad input happened.
            rethrow(me);
    end
    tf = false; 
    return
end

tf = info.Unlimited;

