function jncid = snc_opendap_open_2007b(ncfile,protocol,credentials)
% SNC_OPENDAP_OPEN_2007b Open a connection to an OPeNDAP URL for 2007 and 
%     for more recent versions.  
import ucar.nc2.dods.*  

% The latest netcdf-java accepts http for https?
if strcmp(protocol,'https')
    ncfile = strrep(ncfile,'https','http');
end
    
try

    % The new way, going forward from a 4.2 release version of netcdf-java
	% coming from somewhere between v4.2.17 (2010-11-30) and v4.2.30 
	% (2012-01-27).  Best guess is that it was 4.2.19.
    ucar.nc2.util.net.HttpClientManager.init(credentials,'snctools');
    jncid = DODSNetcdfFile(ncfile);
    return

catch me

    % Try the pre 4.2.19 method. 
    if strcmp(me.identifier,'MATLAB:Java:GenericException')
        client = ucar.nc2.util.net.HttpClientManager.init(credentials,'snctools');
        opendap.dap.DConnect2.setHttpClient(client);
        ucar.unidata.io.http.HTTPRandomAccessFile.setHttpClient(client);
        ucar.nc2.dataset.NetcdfDataset.setHttpClient(client);
        jncid = DODSNetcdfFile(ncfile);
    else
        rethrown(me);
    end
    
end

