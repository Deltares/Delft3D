function attval = nc_attget_tmw(ncfile,varname,attrname)
% Native netcdf package backend for NC_ATTGET.

try
    attval = get_att_nc(ncfile,varname,attrname);
catch me
    switch me.identifier
        case 'snctools:backendSwitchToJava'
            attval = nc_attget_java(ncfile,varname,attrname);

        case 'snctools:backendSwitchToHDF5'
            % Since the netcdf file handle is guaranteed to be closed here,
            % it is save to try to retrieve as an HDF5 attribute (10b,
            % 11a).
            attval = nc_attget_hdf5(ncfile,varname,attrname);

        otherwise
            rethrow(me);
    end
end

%--------------------------------------------------------------------------
function attval = get_att_nc(ncfile,varname,attrname)

ncid=netcdf.open(ncfile,'NOWRITE');

try
    % Encapsulate all the netcdf functionality here so we can reliably
    % close the file if necessary.
    attval = get_att(ncid,varname,attrname);
catch me
    netcdf.close(ncid);
    handle_error(me);
end

netcdf.close(ncid);




%-------------------------------------------------------------
function values = get_att(ncid,varname,attrname)

% If the library is > 4 and the format is unrestricted netcdf-4, then we
% may need to drill down thru the groups.
lv = netcdf.inqLibVers;
if (lv(1) == '4') ...
        && strcmp(netcdf.inqFormat(ncid),'FORMAT_NETCDF4') ...
        && (numel(strfind(varname,'/')) > 0)
    
    % Enhanced model, not in root group.
    gid = ncid;
    
    varpath = regexp(varname,'/','split');
    for k = 2:numel(varpath)-1
        gid = netcdf.inqNcid(gid,varpath{k});
    end
    
    % Is it a group or a variable?
    try
        gid_last = netcdf.inqNcid(gid,varpath{end});
        % It's a group.
        gid = gid_last;
        varid = -1;
    catch me %#ok<NASGU>
        % It's a variable.
        varid = netcdf.inqVarID(gid,varpath{end});
    end
    
    
    
else
    
    % For netcdf-3 files or variables in the root group.
    switch class(varname)
        case 'double'
            varid = varname;
            
        case 'char'
            varid = figure_out_varid_tmw ( ncid, varname );
            
        otherwise
            error('snctools:attget:badType', ...
                'Must specify either a variable name or NC_GLOBAL' );
    end
    
    gid = ncid;
    
end

xtype = netcdf.inqAtt(gid,varid,attrname);
switch( xtype )
    case {0,1,2,3,4,5,6,7,8,9,10,11}
        % No worries.
    otherwise
        switch(version('-release'))
            case '2010b'
                error('snctools:backendSwitchToJava', ...
                    'Unsupported attribute type on this release of MATLAB.');
            otherwise
                 error('snctools:backendSwitchToHDF5', ...
                    'Unsupported attribute type on this release of MATLAB.');  
        end
end

values = netcdf.getAtt(gid,varid,attrname);
return








%--------------------------------------------------------------------------
function values = nc_attget_hdf5(ncfile,varname,attname)
if isa(varname,'double') && varname == -1
    varname = '/';
end
values = h5readatt(ncfile,['/' varname],attname);
if isa(values,'cell')
    % Strings, enums?
    values = values';
end

%--------------------------------------------------------------------------
function varid = figure_out_varid_tmw(ncid,varname)
% Did the user do something really stupid like say 'global' when they meant
% NC_GLOBAL?
if isempty(varname)
    varid = nc_global;
    return;
end

if ( strcmpi(varname,'global') )
    try 
        varid = netcdf.inqVarID(ncid,varname);
        return
    catch %#ok<CTCH>
        % Ok, the user meant NC_GLOBAL
        warning ( 'snctools:attget:doNotUseGlobalString', ...
                  ['Please consider using either NC_GLOBAL or -1 ' ...
                   'instead of the string ''%s''.'], varname );
        varid = nc_global;
        return;
    end
else
    varid = netcdf.inqVarID(ncid,varname);
end



%--------------------------------------------------------------------------
function handle_error(e)
v = version('-release');
switch(e.identifier)
    
    case 'MATLAB:imagesci:netcdf:libraryFailure'   
        
        switch(v)
            case '2011b'
                if strfind(e.message,'enotatt:attributeNotFound')
                    error(e.identifier, ...
                        'Attribute not found.');
                elseif strfind(e.message,'enotvar:variableNotFound')
                    error(e.identifier, ...
                        'Variable not found.');
                else
                    rethrow(e);
                end
            otherwise
                rethrow(e);
        end
     
    otherwise
        rethrow(e);
        
end
