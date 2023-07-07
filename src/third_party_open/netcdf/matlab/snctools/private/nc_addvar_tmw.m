function nc_addvar_tmw(ncfile,varstruct,preserve_fvd)
% TMW backend for addvar.

ncid = netcdf.open(ncfile, nc_write_mode );

try
	addvar(ncid,varstruct,preserve_fvd);
catch me
    netcdf.close(ncid);
    rethrow(me);
end

netcdf.close(ncid );





%--------------------------------------------------------------------------
function addvar(ncid,varstruct,preserve_fvd)

% determine the dimids of the named dimensions
num_dims = length(varstruct.Dimension);
dimids = zeros(1,num_dims);
for j = 1:num_dims
    dimids(1,j) = netcdf.inqDimID(ncid, varstruct.Dimension{j} );
end

% If we are old school, we need to flip the dimensions.
if ~preserve_fvd
    dimids = fliplr(dimids);
end

% go into define mode
netcdf.reDef(ncid);

% Prefer to use Datatype instead of Nctype.
if isfield(varstruct,'Datatype')
    varid = netcdf.defVar(ncid, varstruct.Name, varstruct.Datatype, dimids );
else
    % Backwards compatible mode.
    varid = netcdf.defVar(ncid, varstruct.Name, varstruct.Nctype, dimids );
end
  

if ~isempty(varstruct.Chunking)
    
    if ~preserve_fvd
        chunking = fliplr(varstruct.Chunking);
    else
        chunking = varstruct.Chunking;
    end
    if ( numel(chunking) ~= num_dims)
        error ( 'snctools:addvar:tmw:defVarChunking', ...
            'Chunking size does not jive with number of dimensions.');
    end
    
    netcdf.defVarChunking(ncid,varid,'CHUNKED',chunking);
end

if (varstruct.Shuffle || varstruct.Deflate)
   if varstruct.Shuffle
       shuffle = true;
   else
       shuffle = false;
   end
   if varstruct.Deflate
       deflate = true;
   else
       deflate = false;
   end
   netcdf.defVarDeflate(ncid,varid,shuffle,deflate,varstruct.Deflate);
end

handle_attributes(ncid,varid,varstruct);


%--------------------------------------------------------------------------
function handle_attributes(ncid,varid,varstruct)

if isempty(varstruct.Attribute)
    return
end

% Check for _FillValue.  Netcdf-4 is a special case.
v = netcdf.inqLibVers();
idx = find(strcmp({varstruct.Attribute.Name},'_FillValue'));
if ~isempty(idx) && str2double(v(1)) > 3
    fmt = netcdf.inqFormat(ncid);
    if strcmp(fmt,'FORMAT_NETCDF4') || strcmp(fmt,'FORMAT_NETCDF4_CLASSIC')
        
        attval = varstruct.Attribute(idx).Value;
        
        [name,xtype] = netcdf.inqVar(ncid,varid); %#ok<ASGLU>
        switch(xtype)
            case nc_double
                attval = double(attval);
            case nc_float
                attval = single(attval);
            case nc_int
                attval = int32(attval);
            case nc_short
                attval = int16(attval);
            case nc_byte
                attval = int8(attval);
            case nc_char
                attval = char(attval);
        end
        netcdf.defVarFill(ncid,varid,false,attval);
        
        % Remove the attribute name/pair value now.
        varstruct.Attribute(idx) = [];
    end
end


% Now just use nc_attput to put in the attributes
for j = 1:numel(varstruct.Attribute)
    attname = varstruct.Attribute(j).Name;
    attval = varstruct.Attribute(j).Value;
    nc_attput_tmw(ncid,varid,attname,attval);
end

netcdf.endDef(ncid );
    


