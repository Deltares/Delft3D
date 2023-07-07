function jncid = snc_opendap_open(ncfile)
% SNC_OPENDAP_OPEN Open a connection to an OPeNDAP URL.  If the URL is
% password-protected, we will have to coerce netcdf-java to supply the
% credentials.

import ucar.nc2.dods.*  

% Is it a username/password protected URL?
pat = '(?<protocol>https{0,1})://(?<username>[^:]+):(?<password>[^@]+)@(?<host>[^/]+)(?<relpath>.*)';
parts = regexp(ncfile,pat,'names');
if numel(parts) == 0
    jncid = DODSNetcdfFile(ncfile);
    return
end

% Ok, username and password was detected.  We need to satisfy the mechanism
% for netcdf-java authentication.

% SncCreds is a custom java class supplied with SNCTOOLS.
credentials = SncCreds(parts.username,parts.password);

v = version('-release');
switch(v)
    case {'14','2006a','2006b','2007a'}
        client = ucar.nc2.util.net.HttpClientManager.init(credentials,'snctools');
        opendap.dap.DConnect2.setHttpClient(client);
        ucar.unidata.io.http.HTTPRandomAccessFile.setHttpClient(client);
        ucar.nc2.dataset.NetcdfDataset.setHttpClient(client);
        jncid = DODSNetcdfFile(ncfile);
        
    otherwise
        jncid = snc_opendap_open_2007b(ncfile,parts.protocol,credentials);
             
end

