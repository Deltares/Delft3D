function Dataset = nc_getvarinfo_java(ncfile,varname)
% Java backend for NC_GETVARINFO.

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
    fid = fopen(ncfile);
    ncfile = fopen(fid);
    fclose(fid);
    jncid = NetcdfFile.open(ncfile);
else
    try 
        jncid = NetcdfFile.open ( ncfile );
    catch %#ok<CTCH>
        try
            jncid = snc_opendap_open(ncfile);
        catch %#ok<CTCH>
            error ( 'snctools:getVarInfo:fileOpenFailure', ...
                'Could not open ''%s'' with java backend.' , ncfile);
        end
    end
end

if isa(varname,'ucar.nc2.Variable')
    jvarid = varname;
else
    jvarid = jncid.findVariable(varname);
    if isempty(jvarid)
        close(jncid);
        error ( 'snctools:getVarInfo:badVariableName', ...
            'Could not locate variable %s', varname );
    end
end

% All the details are hidden here because we need the exact same
% functionality in nc_info.
Dataset = nc_getvaridinfo_java(jvarid);

% If we were passed a java file id, don't close it upon exit.
if close_it
    close ( jncid );
end

return



