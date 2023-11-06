function [varargout] = mexnc_tmw(varargin)
% MEXNC_TMW:  this translation layer channels mexnc calls into the 
% mathworks netcdf package
varargout = cell(1,nargout);
op = lower(varargin{1});

% If the leading three chars are 'nc_', then strip it.
if (numel(op) > 3) && strcmp(op(1:3),'nc_')
    op = op(4:end);
end

v = version('-release');

% If the leading three chars are 'nc', and the 3rd char is NOT '_', then 
% strip the first two chars.
if (numel(op) > 3) && strcmp(op(1:2),'nc') && (op(3) ~= '_')
    op = op(3:end);
end

switch op
	case { 'def_var_deflate', 'def_var_chunking', 'inq_var_deflate', 'inq_var_chunking' }
        switch(v)
            case { '14', '2006a','2006b','2007a','2007b','2008a','2008b', ...
                    '2009a','2009b','2010a'}
                error ('MEXNC:netcdf4:notSupported', ...
                    '%s is not supported in release %s.', op, v );
            otherwise
                handler = eval ( ['@handle_' op] );
        end

    case { 'close', 'copy_att', 'create', '_create', 'def_dim', 'def_var', 'del_att'}
        handler = eval ( ['@handle_' op] );

    case {'enddef', 'end_def', '_enddef'}
        handler = @handle_enddef;

    case {'get_att_double', 'get_att_float', 'get_att_int', ...
          'get_att_short', 'get_att_schar', 'get_att_uchar', ...
          'get_att_text'}
        handler = @handle_get_att;

    case { 'get_var_double', 'get_var_float', 'get_var_int', ...
           'get_var_short', 'get_var_schar', 'get_var_uchar', ...
           'get_var_text' }
        handler = @handle_get_var;

    case { 'get_var1_double', 'get_var1_float', 'get_var1_int', ...
           'get_var1_short', 'get_var1_schar', 'get_var1_uchar', ...
           'get_var1_text' }
        handler = @handle_get_var1;

    case { 'get_vara_double', 'get_vara_float', 'get_vara_int', ...
           'get_vara_short', 'get_vara_schar', 'get_vara_uchar', ...
           'get_vara_text' }
        handler = @handle_get_vara;

    case { 'get_vars_double', 'get_vars_float', 'get_vars_int', ...
           'get_vars_short', 'get_vars_schar', 'get_vars_uchar', ...
           'get_vars_text' }
        handler = @handle_get_vars;

    case { 'get_varm_double', 'get_varm_float', 'get_varm_int', ...
           'get_varm_short', 'get_varm_schar', 'get_varm_uchar', ...
           'get_varm_text' }
        error ('MEXNC:getVarm:notSupported', ...
            '%s is not supported by the netCDF package.', op );

    case {'inq', 'inq_ndims' , ...
          'inq_nvars', 'inq_natts', 'inq_dim', 'inq_dimlen', ...
          'inq_dimname', 'inq_attid', 'inq_dimid', 'inq_libvers', 'inq_var', ...
          'inq_varname', 'inq_vartype', 'inq_varndims', 'inq_vardimid', ...
          'inq_varnatts', 'inq_varid', 'inq_att', 'inq_atttype', 'inq_attlen', ...
          'inq_attname', 'inq_unlimdim', 'open', '_open' }
        handler = eval ( ['@handle_' op] );

    case { 'put_att_double', 'put_att_float', 'put_att_int', 'put_att_short', ...
           'put_att_schar', 'put_att_uchar', 'put_att_text' }
        handler = @handle_put_att;

    case { 'put_var_double', 'put_var_float', 'put_var_int', ...
           'put_var_short', 'put_var_schar', 'put_var_uchar', ...
           'put_var_text' }
        handler = @handle_put_var;

    case { 'put_var1_double', 'put_var1_float', 'put_var1_int', ...
           'put_var1_short', 'put_var1_schar', 'put_var1_uchar', ...
           'put_var1_text' }
        handler = @handle_put_var1;

    case { 'put_vara_double', 'put_vara_float', 'put_vara_int', ...
           'put_vara_short', 'put_vara_schar', 'put_vara_uchar', ...
           'put_vara_text' }
        handler = @handle_put_vara;

    case { 'put_vars_double', 'put_vars_float', 'put_vars_int', ...
           'put_vars_short', 'put_vars_schar', 'put_vars_uchar', ...
           'put_vars_text' }
        handler = @handle_put_vars;

    case { 'put_varm_double', 'put_varm_float', 'put_varm_int', ...
           'put_varm_short', 'put_varm_schar', 'put_varm_uchar', ...
           'put_varm_text' }
        error ('MEXNC:putVarm:notSupported', ...
            '%s is not supported by the netCDF package.', op );

    case {'redef', 'rename_att', 'rename_dim', 'rename_var', 'set_fill', ...  
          'strerror', 'sync' }
        handler = eval ( ['@handle_' op] );

    % NETCDF-2 functions
    case { 'attcopy', 'attdel', 'attget', 'attinq', 'attname', 'attput', ...
           'attrename', 'dimdef', 'dimid', 'diminq', 'dimrename', 'endef',  ...
           'inquire', 'parameter', 'typelen', 'vardef', 'varid', 'varinq', ...
           'varget1',  'varput1', 'varget', 'varput', 'vargetg', 'varputg',  ...
           'varrename', 'setopts' }
        handler = eval ( ['@handle_' op] );

    otherwise
        error('MEXNC:TMW:unrecognizedFuncstr',...
              'Function string ''%s'' is not recognized.\n',op);
end

if nargout > 0
    [varargout{:}] = handler ( varargin{:} );
else
    handler ( varargin{:} );
end


%--------------------------------------------------------------------------
function varargout = handle_parameter ( varargin )  %#ok<DEFNU>
%      status = mexnc('PARAMETER', name);


error(nargchk(2,2,nargin,'struct'));

varargout = cell(1,nargout);
switch ( lower(varargin{2}) )
    case 'max_nc_name'
        output = netcdf.getConstant('nc_max_name');
    case 'max_nc_dims'
        output = netcdf.getConstant('nc_max_dims');
    case 'max_nc_vars'
        output = netcdf.getConstant('nc_max_vars');
    case 'max_nc_attrs'
        output = netcdf.getConstant('nc_max_attrs');
    otherwise
        output = netcdf.getConstant(varargin{2});
end
if nargout > 0
    varargout{1} = output;
end



%------------------------------------------------------------------------------------------
function varargout = handle_inq_libvers ( varargin )  %#ok<DEFNU>
%  status = mexnc('CLOSE',ncid);


varargout = cell(1,nargout);
output = netcdf.inqLibVers();
if nargout > 0
    varargout{1} = output;
end



%------------------------------------------------------------------------------------------
function varargout = handle_close(op,ncid) %#ok<DEFNU,INUSL>
% status = mexnc('CLOSE',ncid);

varargout = cell(1,nargout);

try
    netcdf.close(ncid);
    status = 0;
catch myException
    status = exception2status(myException);
end

switch nargout
    case 1
        varargout{1} = status;
        
end




%--------------------------------------------------------------------------
function varargout = handle_copy_att(op,ncid_in,varid_in,attname,ncid_out,varid_out) %#ok<INUSL>
% status = mexnc('COPY_ATT',ncid_in,varid_in,attname,ncid_out,varid_out);

varargout = cell(1,nargout);

try
    netcdf.copyAtt(ncid_in,varid_in,attname,ncid_out,varid_out);
    status = 0;
catch myException
    status = exception2status(myException);
end

switch nargout
    case 1
        varargout{1} = status;
        
end




%------------------------------------------------------------------------------------------
function varargout = handle_create(op,filename,mode) %#ok<DEFNU,INUSL>
% [ncid,status] = mexnc ('CREATE',filename,access_mode );
% [ncid,status] = mexnc ('CREATE',filename);

varargout = cell(1,nargout);

% Sometimes this is called with just two inputs arguments.
% In that case, the default for the 3rd parameter is 'NC_NOWRITE'
if nargin == 2
    mode = 'NC_NOWRITE';
end

try
    ncid = netcdf.create(filename,mode);
    status = 0;
catch myException
    ncid = -1;
    status = exception2status(myException);
end


switch nargout
    case 1
        varargout{1} = ncid; 
        
    case 2
        varargout{1} = ncid;
        varargout{2} = status;

end





%--------------------------------------------------------------------------
function varargout = handle__create(op,filename,mode,initsize,chunksize) %#ok<INUSL,DEFNU>
% [chunksz_out,ncid,status] = mexnc ('_CREATE',filename,mode,initialsize,chunksz_in);

varargout = cell(1,nargout);

% There is a bug in mexnc where chunksize is an optional argument.

if nargin < 5
    chunksize = 0;
end

try
    [czout,ncid] = netcdf.create(filename,mode,initsize,chunksize);
    status = 0;
catch myException
    czout = -1;
    ncid = -1;
    status = exception2status(myException);
end


switch nargout
    case 1
        varargout{1} = czout; 
        
    case 2
        varargout{1} = czout;
        varargout{2} = ncid;

    case 3
        varargout{1} = czout;
        varargout{2} = ncid; 
        varargout{3} = status;
end



%--------------------------------------------------------------------------
function varargout = handle_def_dim(op,ncid,name,dimlen) %#ok<INUSL>
% [dimid,status] = mexnc('DEF_DIM',ncid,name,length);
% [dimid,status] = mexnc('DEF_DIM',ncid,name,'NC_UNLIMITED');


% If 'NC_UNLIMITED' was passed, turn it into char
if ischar(dimlen)
	dimlen = netcdf.getConstant(dimlen);
end

varargout = cell(1,nargout);

try
    dimid = netcdf.defDim(ncid,name,dimlen);
    status = 0;
catch myException
    dimid = -1;
    status = exception2status(myException);
end

switch nargout
    case 1
        varargout{1} = dimid; 
        
    case 2
        varargout{1} = dimid;
        varargout{2} = status;
end





%--------------------------------------------------------------------------
function varargout = handle_def_var(op,ncid,name,xtype,arg1,arg2) %#ok<INUSL>
% [varid,status] = mexnc('DEF_VAR',ncid,name,xtype,dimids);
% [varid,status] = mexnc('DEF_VAR',ncid,name,xtype,ndims,dimids);

varargout = cell(1,nargout);

if nargin == 5
    dimids = arg1;
elseif (nargin == 6) && (arg1 ~= numel(arg2))
    error('MEXNC:handle_def_var', ...
          'Mismatch between number of dimensions and length of dimension list.');
else
    dimids = arg2;
end


% Mexnc and the netcdf package differ wrt the ordering of the 
% dimensions.
if (ndims(dimids) == 2) && (size(dimids,2) == 1)
    dimids = flipud(dimids);
else
    dimids = fliplr(dimids);
end


try
    varid = netcdf.defVar(ncid,name,xtype,dimids);
    status = 0;
catch myException
    varid = -1;
    status = exception2status(myException);
end

switch nargout
    case 1
        varargout{1} = varid; 
        
    case 2
        varargout{1} = varid;
        varargout{2} = status;
end




%------------------------------------------------------------------------------------------
function varargout = handle_del_att ( varargin )
%     status = mexnc('DEL_ATT',ncid,varid,attname);

varargout = cell(1,nargout);

try
    netcdf.delAtt(varargin{2:end});
    status = 0;
catch myException
    status = exception2status(myException);
end

switch nargout
    case 1
        varargout{1} = status; 
end




%------------------------------------------------------------------------------------------
function varargout = handle_enddef ( varargin )
%      status = mexnc('ENDDEF',ncid);
%      status = mexnc('_ENDDEF',ncid,h_minfree,v_align,v_minfree,r_align);

varargout = cell(1,nargout);

try
    netcdf.endDef(varargin{2:end});
    status = 0;
catch myException
    status = exception2status(myException);
end

switch nargout
    case 1
        varargout{1} = status; 
        
end



%------------------------------------------------------------------------------------------
function varargout = handle_get_att ( varargin )
%     [att_value,status] = mexnc('GET_ATT_DOUBLE',ncid,varid,attname);
%     [att_value,status] = mexnc('GET_ATT_FLOAT', ncid,varid,attname);
%     [att_value,status] = mexnc('GET_ATT_INT',   ncid,varid,attname);
%     [att_value,status] = mexnc('GET_ATT_SHORT', ncid,varid,attname);
%     [att_value,status] = mexnc('GET_ATT_SCHAR', ncid,varid,attname);
%     [att_value,status] = mexnc('GET_ATT_UCHAR', ncid,varid,attname);
%     [att_value,status] = mexnc('GET_ATT_TEXT',  ncid,varid,attname);

error(nargchk(4,4,nargin,'struct'));

varargout = cell(1,nargout);

switch ( upper(varargin{1}) ) 
    case 'GET_ATT_DOUBLE'
        outClass = 'double';
    case 'GET_ATT_FLOAT'
        outClass = 'single';
    case 'GET_ATT_INT'
        outClass = 'int';
    case 'GET_ATT_SHORT'
        outClass = 'short';
    case 'GET_ATT_SCHAR'
        outClass = 'schar';
    case 'GET_ATT_UCHAR'
        outClass = 'uchar';
    case 'GET_ATT_TEXT'
        outClass = 'text';

end

try
    attval = netcdf.getAtt(varargin{2:end}, outClass);
    status = 0;
catch myException
    attval = -1;
    status = exception2status(myException);
end

switch nargout
    case 1
        varargout{1} = attval; 
    case 2
        varargout{1} = attval; 
        varargout{2} = status; 
        
end



%------------------------------------------------------------------------------------------
function varargout = handle_inq ( varargin )
% [ndims,nvars, ngatts, unlimdim, status] = mexnc('INQ',ncid);

varargout = cell(1,nargout);

try
    [ndims,nvars,ngatts,unlimdim] = netcdf.inq(varargin{2:end});
    status = 0;
catch myException
    ndims = -1;
    nvars = -1;
    ngatts = -1;
    unlimdim = -1;
    status = exception2status(myException);
end

switch nargout
    case 1
        varargout{1} = ndims; 
    case 2
        varargout{1} = ndims; 
        varargout{2} = nvars; 
    case 3
        varargout{1} = ndims; 
        varargout{2} = nvars; 
        varargout{3} = ngatts; 
    case 4
        varargout{1} = ndims; 
        varargout{2} = nvars; 
        varargout{3} = ngatts; 
        varargout{4} = unlimdim; 
    case 5
        varargout{1} = ndims; 
        varargout{2} = nvars; 
        varargout{3} = ngatts; 
        varargout{4} = unlimdim; 
        varargout{5} = status; 
        
end



%------------------------------------------------------------------------------------------
function varargout = handle_inq_dim(op,ncid,dimid) %#ok<INUSL>
%      [name,length,status] = mexnc('INQ_DIM',ncid,dimid);

validateattributes(ncid,{'numeric'},{'real','nonempty','finite'});
validateattributes(dimid,{'numeric'},{'real','nonempty','finite'});

varargout = cell(1,nargout);

try
    [name,dimlen] = netcdf.inqDim(ncid,dimid);
    status = 0;
catch myException
    name = '';
    dimlen = -1;
    status = exception2status(myException);
end

switch nargout
    case 1
        varargout{1} = name; 
    case 2
        varargout{1} = name; 
        varargout{2} = dimlen; 
    case 3
        varargout{1} = name; 
        varargout{2} = dimlen; 
        varargout{3} = status; 
end



%--------------------------------------------------------------------------
function varargout = handle_inq_dimlen(op,ncid,dimid) %#ok<INUSL,DEFNU>
%      [dimlength,status] = mexnc('INQ_DIMLEN',ncid,dimid);

validateattributes(ncid,{'numeric'},{'real','nonempty','finite'});
validateattributes(dimid,{'numeric'},{'real','nonempty','finite'});

varargout = cell(1,nargout);

try
    [dud,dimlen] = netcdf.inqDim(ncid,dimid); %#ok<ASGLU>
    status = 0;
catch myException
    dimlen = -1;
    status = exception2status(myException);
end

switch nargout
    case 1
        varargout{1} = dimlen; 
    case 2
        varargout{1} = dimlen; 
        varargout{2} = status; 
end



%--------------------------------------------------------------------------
function varargout = handle_inq_dimname(op,ncid,dimid) %#ok<INUSL,DEFNU>
%      [dimname,status] = mexnc('INQ_DIMNAME',ncid,dimid);

validateattributes(ncid,{'numeric'},{'real','nonempty','finite'});
validateattributes(dimid,{'numeric'},{'real','nonempty','finite'});

varargout = cell(1,nargout);

try
    name = netcdf.inqDim(ncid,dimid);
    status = 0;
catch myException
    name = '';
    status = exception2status(myException);
end

switch nargout
    case 1
        varargout{1} = name; 
    case 2
        varargout{1} = name; 
        varargout{2} = status; 
end



%--------------------------------------------------------------------------
function varargout = handle_inq_ndims(op,ncid) %#ok<INUSL,DEFNU>
%      [ndims,status] = mexnc('INQ_NDIMS',ncid);

validateattributes(ncid,{'numeric'},{'real','nonempty','finite'});

varargout = cell(1,nargout);

try
    ndims = netcdf.inq(ncid);
    status = 0;
catch myException
    ndims = -1;
    status = exception2status(myException);
end

switch nargout
    case 1
        varargout{1} = ndims; 
    case 2
        varargout{1} = ndims; 
        varargout{2} = status; 
end



%--------------------------------------------------------------------------
function varargout = handle_inq_nvars(op,ncid) %#ok<INUSL,DEFNU>
%      [nvars,status] = mexnc('INQ_NVARS',ncid);

varargout = cell(1,nargout);

try
    [dud,nvars] = netcdf.inq(ncid); %#ok<ASGLU>
    status = 0;
catch myException
    nvars = -1;
    status = exception2status(myException);
end

switch nargout
    case 1
        varargout{1} = nvars; 
    case 2
        varargout{1} = nvars; 
        varargout{2} = status; 
end



%--------------------------------------------------------------------------
function varargout = handle_inq_natts(op,ncid) %#ok<INUSL,DEFNU>
%      [natts,status] = mexnc('INQ_NATTS',ncid);

varargout = cell(1,nargout);

try
    [dud,dud,natts] = netcdf.inq(ncid); %#ok<ASGLU>
    status = 0;
catch myException
    natts = -1;
    status = exception2status(myException);
end

switch nargout
    case 1
        varargout{1} = natts; 
    case 2
        varargout{1} = natts; 
        varargout{2} = status; 
end



%--------------------------------------------------------------------------
function varargout = handle_inq_dimid(op,ncid,name) %#ok<INUSL>
% [dimid,status] = mexnc('INQ_DIMID',ncid,name);

validateattributes(ncid,{'numeric'},{'real','nonempty','finite'});
validateattributes(name,{'char'},{'row'});

varargout = cell(1,nargout);

try
    dimid = netcdf.inqDimID(ncid,name);
    status = 0;
catch myException
    dimid = -1;
    status = exception2status(myException);
end

switch nargout
    case 1
        varargout{1} = dimid; 
    case 2
        varargout{1} = dimid; 
        varargout{2} = status; 
        
end



%--------------------------------------------------------------------------
function varargout = handle_inq_attid(op,ncid,varid,attname) %#ok<INUSL,DEFNU>
%     [attid,status] = mexnc('INQ_ATTID',ncid,varid,attname);

validateattributes(ncid,{'numeric'},{'real','nonempty','finite'});
validateattributes(varid,{'numeric'},{'real','nonempty','finite'});
validateattributes(attname,{'char'},{'row'});

varargout = cell(1,nargout);

try
    attId = netcdf.inqAttID(ncid,varid,attname);
    status = 0;
catch myException
    attId = -1;
    status = exception2status(myException);
end

switch nargout
    case 1
        varargout{1} = attId; 
    case 2
        varargout{1} = attId; 
        varargout{2} = status; 
        
end



%--------------------------------------------------------------------------
function varargout = handle_inq_var(op,ncid,varid) %#ok<INUSL>
% [varname,xtype,ndims,dimids,natts,status] = mexnc('INQ_VAR',ncid,varid);

validateattributes(ncid,{'numeric'},{'real','nonempty','finite'});
validateattributes(varid,{'numeric'},{'real','nonempty','finite'});

varargout = cell(1,nargout);

try
    [varname,xtype,dimids,natts] = netcdf.inqVar(ncid,varid);
    ndims = numel(dimids);
    status = 0;
catch myException
    varname = '';
    xtype = -1;
    ndims = -1;
    dimids = -1;
    natts = -1;
    status = exception2status(myException);
end

switch nargout
    case 1
        varargout{1} = varname; 
    case 2
        varargout{1} = varname; 
        varargout{2} = xtype; 
    case 3
        varargout{1} = varname; 
        varargout{2} = xtype; 
        varargout{3} = ndims; 
    case 4
        varargout{1} = varname; 
        varargout{2} = xtype; 
        varargout{3} = ndims; 
        varargout{4} = fliplr(dimids); 
    case 5
        varargout{1} = varname; 
        varargout{2} = xtype; 
        varargout{3} = ndims; 
        varargout{4} = fliplr(dimids); 
        varargout{5} = natts; 
    case 6
        varargout{1} = varname; 
        varargout{2} = xtype; 
        varargout{3} = ndims; 
        varargout{4} = fliplr(dimids); 
        varargout{5} = natts; 
        varargout{6} = status; 
end



%--------------------------------------------------------------------------
function varargout = handle_inq_varname(op,ncid,varid) %#ok<INUSL,DEFNU>
%      [varname,status] = mexnc('INQ_VARNAME',ncid,varid);

validateattributes(ncid,{'numeric'},{'real','nonempty','finite'});
validateattributes(varid,{'numeric'},{'real','nonempty','finite'});

varargout = cell(1,nargout);

try
    varname = netcdf.inqVar(ncid,varid);
    status = 0;
catch myException
    varname = '';
    status = exception2status(myException);
end

switch nargout
    case 1
        varargout{1} = varname; 
    case 2
        varargout{1} = varname; 
        varargout{2} = status; 
end



%--------------------------------------------------------------------------
function varargout = handle_inq_vartype(op,ncid,varid) %#ok<INUSL,DEFNU>
%      [vartype,status] = mexnc('INQ_VARTYPE',ncid,varid);

validateattributes(ncid,{'numeric'},{'real','nonempty','finite'});
validateattributes(varid,{'numeric'},{'real','nonempty','finite'});

varargout = cell(1,nargout);

try
    [dud,xtype] = netcdf.inqVar(ncid,varid); %#ok<ASGLU>
    status = 0;
catch myException
    xtype = -1;
    status = exception2status(myException);
end

switch nargout
    case 1
        varargout{1} = xtype; 
    case 2
        varargout{1} = xtype; 
        varargout{2} = status; 
end



%--------------------------------------------------------------------------
function varargout = handle_inq_varndims(op,ncid,varid) %#ok<INUSL,DEFNU>
%      [varndims,status] = mexnc('INQ_VARNDIMS',ncid,varid);

validateattributes(ncid,{'numeric'},{'real','nonempty','finite'});
validateattributes(varid,{'numeric'},{'real','nonempty','finite'});

varargout = cell(1,nargout);

try
    [dud,dud,dimids] = netcdf.inqVar(ncid,varid); %#ok<ASGLU>
    ndims = numel(dimids);
    status = 0;
catch myException
    ndims = -1;
    status = exception2status(myException);
end

switch nargout
    case 1
        varargout{1} = ndims; 
    case 2
        varargout{1} = ndims; 
        varargout{2} = status; 
end


%--------------------------------------------------------------------------
function varargout = handle_inq_vardimid(op,ncid,varid) %#ok<INUSL,DEFNU>
%      [dimids,status] = mexnc('INQ_VARDIMID',ncid,varid);

validateattributes(ncid,{'numeric'},{'real','nonempty','finite'});
validateattributes(varid,{'numeric'},{'real','nonempty','finite'});

varargout = cell(1,nargout);

try

    [dud,dud,dimids] = netcdf.inqVar(ncid,varid); %#ok<ASGLU>

    % Flip the dimids for mexnc.   The netcdf package
    % uses fortran-style ordering of dimensions.
    dimids = fliplr(dimids);
    status = 0;

catch myException
    dimids = -1;
    status = exception2status(myException);
end

switch nargout
    case 1
        varargout{1} = dimids; 
    case 2
        varargout{1} = dimids; 
        varargout{2} = status; 
end



%--------------------------------------------------------------------------
function varargout = handle_inq_varnatts(op,ncid,varid) %#ok<INUSL,DEFNU>
% [varnatts,status] = mexnc('INQ_VARNATTS',ncid,varid);
% [varname,xtype,ndims,dimids,natts,status] = mexnc('INQ_VAR',ncid,varid);

validateattributes(ncid,{'numeric'},{'real','nonempty','finite'});
validateattributes(varid,{'numeric'},{'real','nonempty','finite'});

varargout = cell(1,nargout);

try
    [dud,dud,dud,natts] = netcdf.inqVar(ncid,varid); %#ok<ASGLU>
    status = 0;
catch myException
    natts = -1;
    status = exception2status(myException);
end

switch nargout
    case 1
        varargout{1} = natts; 
    case 2
        varargout{1} = natts; 
        varargout{2} = status; 
end


%--------------------------------------------------------------------------
function varargout = handle_inq_varid(op,ncid,varname) %#ok<INUSL>
% [varid,status] = mexnc('INQ_VARID',ncid,varname);

validateattributes(ncid,{'numeric'},{'real','nonempty','finite'});
validateattributes(varname,{'char'},{'row'});

varargout = cell(1,nargout);

try
    varid = netcdf.inqVarID(ncid,varname);
    status = 0;
catch myException
    varid = -1;
    status = exception2status(myException);
end

switch nargout
    case 1
        varargout{1} = varid; 
    case 2
        varargout{1} = varid; 
        varargout{2} = status; 
end


%--------------------------------------------------------------------------
function varargout = handle_inq_att(op,ncid,varid,attname) %#ok<INUSL>
%     [datatype,attlen,status] = mexnc('INQ_ATT',ncid,varid,attname);

varargout = cell(1,nargout);

try
    [xtype,len] = netcdf.inqAtt(ncid,varid,attname);
    status = 0;
catch myException
    xtype = -1;
    len = -1;
    status = exception2status(myException);
end

switch nargout
    case 1
        varargout{1} = xtype; 
    case 2
        varargout{1} = xtype; 
        varargout{2} = len; 
    case 3
        varargout{1} = xtype; 
        varargout{2} = len; 
        varargout{3} = status; 
end


%--------------------------------------------------------------------------
function varargout = handle_inq_atttype(op,ncid,varid,attname) %#ok<INUSL,DEFNU>
%     [datatype,attlen,status] = mexnc('INQ_ATT',ncid,varid,attname);
%     [att_type,status] = mexnc('INQ_ATTTYPE',ncid,varid,attname);

validateattributes(ncid,{'numeric'},{'real','nonempty','finite'});
validateattributes(varid,{'numeric'},{'real','nonempty','finite'});
validateattributes(attname,{'char'},{'row'});

varargout = cell(1,nargout);

try
    xtype = netcdf.inqAtt(ncid,varid,attname);
    status = 0;
catch myException
    xtype = -1;
    status = exception2status(myException);
end

switch nargout
    case 1
        varargout{1} = xtype; 
    case 2
        varargout{1} = xtype; 
        varargout{2} = status; 
end


%--------------------------------------------------------------------------
function varargout = handle_inq_attlen(op,ncid,varid,attname) %#ok<INUSL,DEFNU>
%     [datatype,attlen,status] = mexnc('INQ_ATT',ncid,varid,attname);
%     [att_len,status] = mexnc('INQ_ATTLEN',ncid,varid,attname);

validateattributes(ncid,{'numeric'},{'real','nonempty','finite'});
validateattributes(varid,{'numeric'},{'real','nonempty','finite'});
validateattributes(attname,{'char'},{'row'});

varargout = cell(1,nargout);

try
    [dud,len] = netcdf.inqAtt(ncid,varid,attname); %#ok<ASGLU>
    status = 0;
catch myException
    len = -1;
    status = exception2status(myException);
end

switch nargout
    case 1
        varargout{1} = len; 
    case 2
        varargout{1} = len; 
        varargout{2} = status; 
end


%--------------------------------------------------------------------------
function varargout = handle_inq_attname(op,ncid,varid,attid) %#ok<INUSL>
%     [attname,status] = mexnc('INQ_ATTNAME',ncid,varid,attid);

validateattributes(ncid,{'numeric'},{'real','nonempty','finite'});
validateattributes(varid,{'numeric'},{'real','nonempty','finite'});
validateattributes(attid,{'numeric'},{'real','nonempty','finite'});

varargout = cell(1,nargout);

try
    attname = netcdf.inqAttName(ncid,varid,attid);
    status = 0;
catch myException
    attname = '';
    status = exception2status(myException);
end

switch nargout
    case 1
        varargout{1} = attname; 
    case 2
        varargout{1} = attname; 
        varargout{2} = status; 
end


%--------------------------------------------------------------------------
function varargout = handle_inq_unlimdim(op,ncid) %#ok<INUSL,DEFNU>
%      [ndims,nvars, ngatts, unlimdim, status] = mexnc('INQ',ncid);
%      [unlimdim,status] = mexnc ('INQ_UNLIMDIM',ncid);

validateattributes(ncid,{'numeric'},{'real','nonempty','finite'});

varargout = cell(1,nargout);

try
    [dud,dud,dud,unlimdim] = netcdf.inq(ncid); %#ok<ASGLU>
    status = 0;
catch myException
    unlimdim = -1;
    status = exception2status(myException);
end

switch nargout
    case 1
        varargout{1} = unlimdim; 
    case 2
        varargout{1} = unlimdim; 
        varargout{2} = status; 
end


%------------------------------------------------------------------------------------------
function varargout = handle_open(op,filename,mode) %#ok<INUSL,DEFNU>
%  [ncid,status] = mexnc('OPEN',filename,access_mode);

varargout = cell(1,nargout);

% Mexnc allowed for a default NOWRITE mode.
if nargin == 2
    mode = netcdf.getConstant('NC_NOWRITE');
end
try
    ncid = netcdf.open(filename,mode);
    status = 0;
catch myException
    ncid = -1;
    status = exception2status(myException);
end

switch nargout
    case 1
        varargout{1} = ncid; 
    case 2
        varargout{1} = ncid; 
        varargout{2} = status; 
        
end



%--------------------------------------------------------------------------
function varargout = handle__open(op,filename,mode,czin) %#ok<INUSL,DEFNU>
%  [ncid,chunksizehint,status] 
%      = mexnc('_OPEN',filename,access_mode,chunksizehint);

varargout = cell(1,nargout);

try
    [ncid,czout] = netcdf.open(filename,mode,czin);
    status = 0;
catch myException
    ncid = -1;
    czout = -1;
    status = exception2status(myException);
end


switch nargout
    case 1
        varargout{1} = ncid; 
    case 2
        varargout{1} = ncid; 
        varargout{2} = czout; 
    case 3
        varargout{1} = ncid; 
        varargout{2} = czout; 
        varargout{3} = status; 
end




%------------------------------------------------------------------------------------------
function varargout = handle_put_att ( varargin )
%     status = mexnc('PUT_ATT_DOUBLE',ncid,varid,attname,datatype,attvalue);
%     status = mexnc('PUT_ATT_FLOAT', ncid,varid,attname,datatype,attvalue);
%     status = mexnc('PUT_ATT_INT',   ncid,varid,attname,datatype,attvalue);
%     status = mexnc('PUT_ATT_SHORT', ncid,varid,attname,datatype,attvalue);
%     status = mexnc('PUT_ATT_SCHAR', ncid,varid,attname,datatype,attvalue);
%     status = mexnc('PUT_ATT_UCHAR', ncid,varid,attname,datatype,attvalue);
%     status = mexnc('PUT_ATT_TEXT',  ncid,varid,attname,datatype,attvalue);
%
% or
%
%     status = mexnc('PUT_ATT_DOUBLE',ncid,varid,attname,datatype,nelt,attvalue);
%     status = mexnc('PUT_ATT_FLOAT', ncid,varid,attname,datatype,nelt,attvalue);
%     status = mexnc('PUT_ATT_INT',   ncid,varid,attname,datatype,nelt,attvalue);
%     status = mexnc('PUT_ATT_SHORT', ncid,varid,attname,datatype,nelt,attvalue);
%     status = mexnc('PUT_ATT_SCHAR', ncid,varid,attname,datatype,nelt,attvalue);
%     status = mexnc('PUT_ATT_UCHAR', ncid,varid,attname,datatype,nelt,attvalue);
%     status = mexnc('PUT_ATT_TEXT',  ncid,varid,attname,datatype,nelt,attvalue);

varargout = cell(1,nargout);

% Don't bother with the number of elements or datatype.
if nargin == 7
    neededInputs = [2:4 7];
else
    neededInputs = [2:4 6];
end

try
    netcdf.putAtt(varargin{neededInputs});
    status = 0;
catch myException
    status = exception2status(myException);
end

switch nargout
    case 1
        varargout{1} = status; 
        
end






%------------------------------------------------------------------------------------------
function varargout = handle_get_var(op,ncid,varid)
%     [data,status] = mexnc('GET_VAR_DOUBLE',ncid,varid);
%     [data,status] = mexnc('GET_VAR_FLOAT', ncid,varid);
%     [data,status] = mexnc('GET_VAR_INT',   ncid,varid);
%     [data,status] = mexnc('GET_VAR_SHORT', ncid,varid);
%     [data,status] = mexnc('GET_VAR_SCHAR', ncid,varid);
%     [data,status] = mexnc('GET_VAR_UCHAR', ncid,varid);
%     [data,status] = mexnc('GET_VAR_TEXT',  ncid,varid);

validateattributes(ncid,{'numeric'},{'real','nonempty','finite'});
validateattributes(varid,{'numeric'},{'real','nonempty','finite'});

varargout = cell(1,nargout);

try
    if strcmpi(op,'get_var_uchar')
           data = netcdf.getVar(ncid,varid,'uint8');
    else
           data = netcdf.getVar(ncid,varid);
    end
    status = 0;
catch myException
    data = -1;
    status = exception2status(myException);
end

switch nargout
    case 1
        varargout{1} = data; 
    case 2
        varargout{1} = data; 
        varargout{2} = status; 
        
end



%------------------------------------------------------------------------------------------
function varargout = handle_get_var1(op,ncid,varid,start)
%     [data,status] = mexnc('GET_VAR1_DOUBLE',ncid,varid,start);
%     [data,status] = mexnc('GET_VAR1_FLOAT', ncid,varid,start);
%     [data,status] = mexnc('GET_VAR1_INT',   ncid,varid,start);
%     [data,status] = mexnc('GET_VAR1_SHORT', ncid,varid,start);
%     [data,status] = mexnc('GET_VAR1_SCHAR', ncid,varid,start);
%     [data,status] = mexnc('GET_VAR1_UCHAR', ncid,varid,start);
%     [data,status] = mexnc('GET_VAR1_TEXT',  ncid,varid,start);

validateattributes(ncid,{'numeric'},{'real','nonempty','finite'});
validateattributes(varid,{'numeric'},{'real','nonempty','finite'});
validateattributes(start,{'numeric'},{'real','nonempty','finite'});

varargout = cell(1,nargout);

% If the op is get_var1_x, and if the variable is a singleton, then we have to remap
% the operation as 'get_var_x'.
[varname,xtype,dimids] = netcdf.inqVar(ncid,varid); %#ok<ASGLU>
if ( numel(dimids) == 0 ) 
    new_op = ['get_var_' op(10:end)];
    [varargout{:}] = handle_get_var(new_op,ncid,varid);
    return
end

% Must flip the indices.
start = fliplr(start(:)');
%varargin{4} = varargin{4}(:)';

try
    if strcmpi(op,'get_var1_uchar')
           data = netcdf.getVar(ncid,varid,start,'uint8');
    else
           data = netcdf.getVar(ncid,varid,start);
    end
    status = 0;
catch myException
    data = -1;
    status = exception2status(myException);
end

switch nargout
    case 1
        varargout{1} = data; 
    case 2
        varargout{1} = data; 
        varargout{2} = status; 
        
end




%------------------------------------------------------------------------------------------
function varargout = handle_get_vara(op,ncid,varid,start,count)
%     [data,status] = mexnc('GET_VARA_DOUBLE',ncid,varid,start,count);
%     [data,status] = mexnc('GET_VARA_FLOAT', ncid,varid,start,count);
%     [data,status] = mexnc('GET_VARA_INT',   ncid,varid,start,count);
%     [data,status] = mexnc('GET_VARA_SHORT', ncid,varid,start,count);
%     [data,status] = mexnc('GET_VARA_SCHAR', ncid,varid,start,count);
%     [data,status] = mexnc('GET_VARA_UCHAR', ncid,varid,start,count);
%     [data,status] = mexnc('GET_VARA_TEXT',  ncid,varid,start,count);

validateattributes(ncid,{'numeric'},{'real','nonempty','finite'});
validateattributes(varid,{'numeric'},{'real','nonempty','finite'});
validateattributes(start,{'numeric'},{'real','nonempty','finite'});
validateattributes(count,{'numeric'},{'real','nonempty','finite'});

varargout = cell(1,nargout);

% Must flip the indices.
start = fliplr(start(:)');
count = fliplr(count(:)');

% If the variable is a singleton, just use get_var instead.

try
    if strcmpi(op,'get_vara_uchar')
           data = netcdf.getVar(ncid,varid,start,count,'uint8');
    else
           data = netcdf.getVar(ncid,varid,start,count);
    end
    status = 0;
catch myException
    data = -1;
    status = exception2status(myException);
end

switch nargout
    case 1
        varargout{1} = data; 
    case 2
        varargout{1} = data; 
        varargout{2} = status; 
        
end




%------------------------------------------------------------------------------------------
function varargout = handle_get_vars (op,ncid,varid,start,count,stride)
%     [data,status] = mexnc('GET_VARS_DOUBLE',ncid,varid,start,count,stride);
%     [data,status] = mexnc('GET_VARS_FLOAT', ncid,varid,start,count,stride);
%     [data,status] = mexnc('GET_VARS_INT',   ncid,varid,start,count,stride);
%     [data,status] = mexnc('GET_VARS_SHORT', ncid,varid,start,count,stride);
%     [data,status] = mexnc('GET_VARS_SCHAR', ncid,varid,start,count,stride);
%     [data,status] = mexnc('GET_VARS_UCHAR', ncid,varid,start,count,stride);
%     [data,status] = mexnc('GET_VARS_TEXT',  ncid,varid,start,count,stride);

validateattributes(ncid,{'numeric'},{'real','nonempty','finite'});
validateattributes(varid,{'numeric'},{'real','nonempty','finite'});
validateattributes(start,{'numeric'},{'real','nonempty','finite'});
validateattributes(count,{'numeric'},{'real','nonempty','finite'});
validateattributes(stride,{'numeric'},{'real','nonempty','finite'});

varargout = cell(1,nargout);

% Must flip the indices.
start = fliplr(start(:)');
count = fliplr(count(:)');
stride= fliplr(stride(:)');


try
    if strcmpi(op,'get_vars_uchar')
           data = netcdf.getVar(ncid,varid,start,count,stride,'uint8');
    else
           data = netcdf.getVar(ncid,varid,start,count,stride);
    end
    status = 0;
catch myException
    data = -1;
    status = exception2status(myException);
end

switch nargout
    case 1
        varargout{1} = data; 
    case 2
        varargout{1} = data; 
        varargout{2} = status; 
        
end





%------------------------------------------------------------------------------------------
function varargout = handle_put_var(op,ncid,varid,data) %#ok<INUSL>
%     status = mexnc('PUT_VAR_DOUBLE',ncid,varid,data);
%     status = mexnc('PUT_VAR_FLOAT', ncid,varid,data);
%     status = mexnc('PUT_VAR_INT',   ncid,varid,data);
%     status = mexnc('PUT_VAR_SHORT', ncid,varid,data);
%     status = mexnc('PUT_VAR_SCHAR', ncid,varid,data);
%     status = mexnc('PUT_VAR_UCHAR', ncid,varid,data);
%     status = mexnc('PUT_VAR_TEXT',  ncid,varid,data);
%     status = mexnc('PUT_VAR1_DOUBLE',ncid,varid,start,data);

validateattributes(ncid,{'numeric'},{'real','nonempty','finite'});
validateattributes(varid,{'numeric'},{'real','nonempty','finite'});

varargout = cell(1,nargout);

try
    netcdf.putVar(ncid,varid,data)
    status = 0;
catch myException
    status = exception2status(myException);
end

switch nargout
    case 1
        varargout{1} = status; 
        
end



%------------------------------------------------------------------------------------------
function varargout = handle_put_var1(op,ncid,varid,start,data) %#ok<INUSL>
%     status = mexnc('PUT_VAR1_DOUBLE',ncid,varid,start,data);
%     status = mexnc('PUT_VAR1_FLOAT', ncid,varid,start,data);
%     status = mexnc('PUT_VAR1_INT',   ncid,varid,start,data);
%     status = mexnc('PUT_VAR1_SHORT', ncid,varid,start,data);
%     status = mexnc('PUT_VAR1_SCHAR', ncid,varid,start,data);
%     status = mexnc('PUT_VAR1_UCHAR', ncid,varid,start,data);
%     status = mexnc('PUT_VAR1_TEXT',  ncid,varid,start,data);
%         These routines write a single value to the location at the given
%         starting index.
%

validateattributes(ncid,{'numeric'},{'real','nonempty','finite'});
validateattributes(varid,{'numeric'},{'real','nonempty','finite'});
validateattributes(start,{'numeric'},{'real','nonempty','finite'});

varargout = cell(1,nargout);

% Must switch the order of the start index.
start = fliplr(start(:)');


try
    [varname,xtype,dimids] = netcdf.inqVar(ncid,varid); %#ok<ASGLU>
    if isempty(dimids)
        % Don't use a start argument for singletons.
        netcdf.putVar(ncid,varid,data);
    else
        netcdf.putVar(ncid,varid,start,data);
    end
    status = 0;
catch myException
    status = exception2status(myException);
end

switch nargout
    case 1
        varargout{1} = status; 
        
end



%------------------------------------------------------------------------------------------
function varargout = handle_put_vara (op,ncid,varid,start,count,data) %#ok<INUSL>
%     status = mexnc('PUT_VARA_DOUBLE',ncid,varid,start,count,data);
%     status = mexnc('PUT_VARA_FLOAT', ncid,varid,start,count,data);
%     status = mexnc('PUT_VARA_INT',   ncid,varid,start,count,data);
%     status = mexnc('PUT_VARA_SHORT', ncid,varid,start,count,data);
%     status = mexnc('PUT_VARA_SCHAR', ncid,varid,start,count,data);
%     status = mexnc('PUT_VARA_UCHAR', ncid,varid,start,count,data);
%     status = mexnc('PUT_VARA_TEXT',  ncid,varid,start,count,data);
%

validateattributes(ncid,{'numeric'},{'real','nonempty','finite'});
validateattributes(varid,{'numeric'},{'real','nonempty','finite'});
validateattributes(start,{'numeric'},{'real','nonempty','finite'});
validateattributes(count,{'numeric'},{'real','nonempty','finite'});

varargout = cell(1,nargout);

% Must switch the order of the indices.
start = fliplr((start(:))');
count = fliplr((count(:))');
if any(count<0)
    idx = find(count<0);
    [varname,xtype,dimids] = netcdf.inqVar(ncid,varid); %#ok<ASGLU>
    for j = 1:numel(idx)
        [dud,len] = netcdf.inqDim(ncid,dimids(idx(j))); %#ok<ASGLU>
        count(idx(j)) = len;
    end
end


try
    netcdf.putVar(ncid,varid,start,count,data)
    status = 0;
catch myException
    status = exception2status(myException);
end

switch nargout
    case 1
        varargout{1} = status; 
        
end


%------------------------------------------------------------------------------------------
function varargout = handle_put_vars(op,ncid,varid,start,count,stride,data) %#ok<INUSL>
%     status = mexnc('PUT_VARS_DOUBLE',ncid,varid,start,count,stride,data);
%     status = mexnc('PUT_VARS_FLOAT', ncid,varid,start,count,stride,data);
%     status = mexnc('PUT_VARS_INT',   ncid,varid,start,count,stride,data);
%     status = mexnc('PUT_VARS_SHORT', ncid,varid,start,count,stride,data);
%     status = mexnc('PUT_VARS_SCHAR', ncid,varid,start,count,stride,data);
%     status = mexnc('PUT_VARS_UCHAR', ncid,varid,start,count,stride,data);
%     status = mexnc('PUT_VARS_TEXT',  ncid,varid,start,count,stride,data);
%

validateattributes(ncid,{'numeric'},{'real','nonempty','finite'});
validateattributes(varid,{'numeric'},{'real','nonempty','finite'});
validateattributes(start,{'numeric'},{'real','nonempty','finite'});
validateattributes(count,{'numeric'},{'real','nonempty','finite'});
validateattributes(stride,{'numeric'},{'real','nonempty','finite'});

varargout = cell(1,nargout);

% Must switch the order of the start, count, and stride indices.
start = fliplr((start(:))');
count = fliplr((count(:))');
stride = fliplr((stride(:))');

try
    netcdf.putVar(ncid,varid,start,count,stride,data)
    status = 0;
catch myException
    status = exception2status(myException);
end

switch nargout
    case 1
        varargout{1} = status; 
        
end



%--------------------------------------------------------------------------
function varargout = handle_redef(op,ncid) %#ok<INUSL,DEFNU>
%      status = mexnc('REDEF',ncid);

varargout = cell(1,nargout);

try
    netcdf.reDef(ncid);
    status = 0;
catch myException
    status = exception2status(myException);
end

switch nargout
    case 1
        varargout{1} = status; 
        
end






%--------------------------------------------------------------------------
function varargout = handle_rename_att(op,ncid,dimid,oldName,newName) %#ok<INUSL>
%      status = mexnc('RENAME_ATT',ncid,dimid,oldName,newName);

validateattributes(ncid,{'numeric'},{'real','nonempty','finite'});
validateattributes(dimid,{'numeric'},{'real','nonempty','finite'});
validateattributes(oldName,{'char'},{'row'});
validateattributes(newName,{'char'},{'row'});

varargout = cell(1,nargout);

try
    netcdf.renameAtt(ncid,dimid,oldName,newName);
    status = 0;
catch myException
    status = exception2status(myException);
end

switch nargout
    case 1
        varargout{1} = status; 
        
end






%--------------------------------------------------------------------------
function varargout = handle_rename_dim(op,ncid,dimid,name) %#ok<INUSL>
%      status = mexnc('RENAME_DIM',ncid,dimid,name);

validateattributes(ncid,{'numeric'},{'real','nonempty','finite'});
validateattributes(dimid,{'numeric'},{'real','nonempty','finite'});
validateattributes(name,{'char'},{'row'});

varargout = cell(1,nargout);

try
    netcdf.renameDim(ncid,dimid,name);
    status = 0;
catch myException
    status = exception2status(myException);
end

switch nargout
    case 1
        varargout{1} = status; 
        
end






%--------------------------------------------------------------------------
function varargout = handle_rename_var(op,ncid,varid,newname) %#ok<INUSL>
%      status = mexnc('RENAME_VAR',ncid,varid,new_varname);

validateattributes(ncid,{'numeric'},{'real','nonempty','finite'});
validateattributes(varid,{'numeric'},{'real','nonempty','finite'});
validateattributes(newname,{'char'},{'row'});

varargout = cell(1,nargout);

try
    netcdf.renameVar(ncid,varid,newname);
    status = 0;
catch myException
    status = exception2status(myException);
end

switch nargout
    case 1
        varargout{1} = status; 
        
end






%--------------------------------------------------------------------------
function varargout = handle_set_fill(op,ncid,mode) %#ok<INUSL,DEFNU>
%      [old_fill_mode,status] = mexnc('SET_FILL',ncid,new_fill_mode)

validateattributes(ncid,{'numeric'},{'real','nonempty','finite'});
validateattributes(mode,{'numeric'},{'real','nonempty','finite'});

varargout = cell(1,nargout);

try
    old_fill_mode = netcdf.setFill(ncid,mode);
    status = 0;
catch myException
    old_fill_mode = [];
    status = exception2status(myException);
end

switch nargout
    case 1
        varargout{1} = old_fill_mode; 
    case 2
        varargout{1} = old_fill_mode; 
        varargout{2} = status; 
        
end





%--------------------------------------------------------------------------
function varargout = handle_setopts(op,ncopts) %#ok<INUSD,DEFNU>
%      old_ncopts = mexnc('SETOPTS', ncopts)
%
% This is now a no-op.

varargout = cell(1,nargout);
if ( nargout > 0 )
    varargout{1} = 0;
end






%--------------------------------------------------------------------------
function varargout = handle_strerror(op,error_code) %#ok<INUSL,DEFNU>
%      error_message = mexnc('STRERROR',error_code);

varargout = cell(1,nargout);

if ~isnumeric(error_code)
    error ( 'MEXNC:strerror:inputMustBeNumeric', ...
        'Input to strerror must be numeric.');
end
switch ( error_code )
    case 0 
        msg = 'No Error';
    case -1 
        msg = 'NC2 Error'; 

    case -33
        % #define    NC_EBADID    (-33)    
        msg = 'Not a netcdf id';

    case -34
        % #define    NC_ENFILE    (-34)    /* Too many netcdfs open */
        msg = 'NetCDF: Too many files open';

    case -35
        msg = 'NetCDF: File exists && NC_NOCLOBBER';
    case -36
        msg = 'NetCDF: Invalid argument';
    case -37
        msg = 'NetCDF: Write to read only';
    case -38
        msg = 'NetCDF: Operation not allowed in data mode';
    case -39
        msg = 'NetCDF: Operation not allowed in define mode';
    case -40
        msg = 'NetCDF: Index exceeds dimension bound';
    case -41
        msg = 'NetCDF: NC_MAX_DIMS exceeded';
    case -42
        msg = 'NetCDF: String match to name in use';
    case -43
        msg = 'NetCDF: Attribute not found';
    case -44
        msg = 'NetCDF: NC_MAX_ATTRS exceeded';
    case -45
        msg = 'NetCDF: Not a valid data type or _FillValue type mismatch';
    case -46
        msg = 'NetCDF: Invalid dimension ID or name';
    case -47
        msg = 'NetCDF: NC_UNLIMITED in the wrong index';
    case -48
        msg = 'NetCDF: NC_MAX_VARS exceeded';
    case -49
        msg = 'NetCDF: Variable not found';
    case -50
        msg = 'NetCDF: Action prohibited on NC_GLOBAL varid';
    case -51
        msg = 'NetCDF: Unknown file format';
    case -52
        msg = 'NetCDF: In Fortran, string too short';
    case -53
        msg = 'NetCDF: NC_MAX_NAME exceeded';
    case -54
        msg = 'NetCDF: NC_UNLIMITED size already in use';
    case -55
        msg = 'NetCDF: nc_rec op when there are no record vars';
    case -56
        msg = 'NetCDF: Attempt to convert between text & numbers';
    case -57
        msg = 'NetCDF: Start+count exceeds dimension bound';
    case -58
        msg = 'NetCDF: Illegal stride';
    case -59
        msg = 'NetCDF: Name contains illegal characters';
    case -60
        msg = 'NetCDF: Numeric conversion not representable';
    case -61
        msg = 'NetCDF: Memory allocation (malloc) failure';
    case -62
        msg = 'NetCDF: One or more variable sizes violate format constraints';
    case -63
        msg = 'NetCDF: Invalid dimension size';
    case -64
        msg = 'NetCDF: File likely truncated or possibly corrupted';

    otherwise
        msg = 'Unknown Error';
end
varargout{1} = msg; 



%--------------------------------------------------------------------------
function varargout = handle_sync(op,ncid) %#ok<INUSL,DEFNU>
%      status = mexnc('SYNC',ncid );

validateattributes(ncid,{'numeric'},{'real','nonempty','finite'});

varargout = cell(1,nargout);

try
    netcdf.sync(ncid);
    status = 0;
catch myException
    status = exception2status(myException);
end

switch nargout
    case 1
        varargout{1} = status; 
        
end



%------------------------------------------------------------------------------------------
function varargout = handle_varget(op,ncid,varid,start,count,autoscale) %#ok<DEFNU,INUSL>
%      [value, status] = mexnc('VARGET', cdfid, varid, start, count, autoscale)


% Unless it's a char variable, we wish to return the data in double precision.
if ischar(varid)
	try
    	varid = netcdf.inqVarID(ncid,varid);
	catch me %#ok<NASGU>
    	varargout{1} = NaN;
	    varargout{2} = -1;
		return;
	end
end

try
	[varname,xtype,dimids] = netcdf.inqVar(ncid,varid); %#ok<ASGLU>
catch me %#ok<NASGU>
   	varargout{1} = NaN;
    varargout{2} = -1;
	return;
end

if ( xtype ~= netcdf.getConstant('NC_CHAR'))
    outputDatatype = 'double';
else
    outputDatatype = 'char';
end

% Must flip the start and count arguments.
if (nargin >= 4)
    start = fliplr(start(:)');
    count = fliplr(count(:)');
end

idx = find(count<0);
if any(idx)
    for j = 1:numel(idx)
        bad_dimid = dimids(idx(j));
        [dimname,dimlen] = netcdf.inqDim(ncid,bad_dimid); %#ok<ASGLU>
        count(idx(j)) = dimlen - start(idx(j));
    end
end



try
    data = netcdf.getVar(ncid,varid,start,count,outputDatatype);
    status = 0;
catch me %#ok<NASGU>
    varargout{1} = NaN;
    varargout{2} = -1;
	return;
end



if (nargin == 6) && (autoscale == 1)
    data = handle_nc2_output_scaling(ncid,varid,data);
end

if numel(dimids) == 1
    data = data';
end
switch nargout
    case 1
        varargout{1} = data;
    case 2
        varargout{1} = data;
        varargout{2} = status;
end


%--------------------------------------------------------------------------
function varargout = handle_varputg(op,ncid,varid,start,count,stride,imap,value,autoscale) %#ok<DEFNU,INUSL>
% status = mexnc('VARPUTG', cdfid, varid, start, count, stride, [], value, autoscale)

error(nargchk(8,9,nargin,'struct'));

if ischar(varid)
    varid = netcdf.inqVarID(ncid,varid);
end


switch(class(value))
    case { 'double', 'char' }
        
    otherwise
        error('MEXCDF:mexnc:badDatatype', ...
              'VARPUTG required either double or char data.' );
end



% Scale the input if necessary.
if (nargin == 9) && (autoscale == 1)
    value = handle_nc2_input_scaling(ncid,varid,value);
end

% Must flip the start and count arguments.
if (nargin >= 4)
    start = fliplr((start(:))');
    count = fliplr((count(:))');
    stride = fliplr((stride(:))');
end

% Skip over that empty argument.  Would have been the imap thingie.


try
    netcdf.putVar(ncid,varid,start,count,stride,value);
    status = 0;
catch %#ok<CTCH>
    status = -1;
end

switch nargout
    case 1
        varargout{1} = status;
end




%--------------------------------------------------------------------------
function varargout = handle_vargetg (op,ncid,varid,start,count,stride,imap,autoscale) %#ok<DEFNU,INUSL>
%      [value, status] = mexnc('VARGETG', cdfid, varid, start, count, stride, [], autoscale)

try
    
    if ischar(varid)
        varid = netcdf.inqVarID(ncid,varid);
    end

    % Unless it's a char variable, we wish to return the data in double precision.
    [varname,xtype,dimids] = netcdf.inqVar(ncid,varid); %#ok<ASGLU>

    % Must flip the start and count arguments.
    if (nargin >= 4)
        start = fliplr(start(:)');
        count = fliplr(count(:)');
        stride = fliplr(stride(:)');
    end

    % If any count arguments are negative, replace them by the appropriate
    % value.
    idx = find(count<0);
    if any(idx)
        for j = 1:numel(idx)
            bad_dimid = dimids(idx(j));
            [dimname,dimlen] = netcdf.inqDim(ncid,bad_dimid); %#ok<ASGLU>
            count(idx(j)) = ceil((dimlen-start(idx(j)))/stride(idx(j)));
        end
    end


    if ( xtype ~= netcdf.getConstant('NC_CHAR'))
        data = netcdf.getVar(ncid,varid,start,count,stride,'double');
    else
        data = netcdf.getVar(ncid,varid,start,count,stride);
    end
    status = 0;

    if (nargin == 8) && (autoscale == 1)
        data = handle_nc2_output_scaling ( ncid, varid, data );
    end

    % Permute col vectors into rows.  Why?  Well, that's just the way that 
    % it was done.
    if (ndims(data) == 2) && (size(data,2) == 1)
        data = data';   
    end

catch %#ok<CTCH>
    data = NaN;
    status = -1;
end

switch nargout
    case 1
        varargout{1} = data;
    case 2
        varargout{1} = data;
        varargout{2} = status;
end

%--------------------------------------------------------------------------
function status = exception2status ( myException )
% Translate an exception to an error status.
% The netcdf package issues exceptions when there is an error condition, but mexnc expects
% status numbers.

switch ( myException.identifier )

    case 'MATLAB:netcdf:open:noSuchFile'
        status = 2;
        return


    % NC2 error
    case {'MATLAB:netcdf:negativeSize'}
        status = -1;
        return

        
    otherwise
        status = -1;
        return

end







%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function data = handle_nc2_input_scaling(ncid,varid,data)
% HANDLE_NC2_INPUT_SCALING
%     If there is a scale factor and/or  add_offset attribute, convert the 
%     data to double precision and apply the scaling.
%

try
    scale_factor = netcdf.getAtt(ncid,varid,'scale_factor');
catch me %#ok<NASGU>
    scale_factor = 1.0;
end

try
    add_offset = netcdf.getAtt(ncid,varid,'add_offset');
catch me %#ok<NASGU>
    add_offset = 0.0;
end

data = (double(data) - add_offset) / scale_factor + 0.5;


return




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% HANDLE_NC2_OUTPUT_SCALING
%     If there is a scale factor and/or  add_offset attribute, convert the 
%     data to double precision and apply the scaling.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function values = handle_nc2_output_scaling ( ncid, varid, values )

try
    scale_factor = netcdf.getAtt(ncid,varid,'scale_factor');
catch me %#ok<NASGU>
    scale_factor = 1.0;
end
try
    add_offset = netcdf.getAtt(ncid,varid,'add_offset');
catch me %#ok<NASGU>
    add_offset = 0.0;
end

values = double(values) * scale_factor + add_offset;



%--------------------------------------------------------------------------
% NetCDF-2 functions.
%--------------------------------------------------------------------------

%--------------------------------------------------------------------------
function varargout = handle_attcopy(op,ncid_in,varid_in,attname,ncid_out,varid_out) %#ok<DEFNU>
% status = mexnc('COPY_ATT',ncid_in,varid_in,attname,ncid_out,varid_out);
% status = mexnc('ATTCOPY', incdf, invar, 'name', outcdf, outvar)

varargout = cell(1,nargout);

status = handle_copy_att(op,ncid_in,varid_in,attname,ncid_out,varid_out);
if status ~= 0
    status = -1;
end
switch nargout
    case 1
        varargout{1} = status;
        
end




%--------------------------------------------------------------------------
function varargout = handle_attdel(op,ncid,varid,attname) %#ok<DEFNU>
%      status = mexnc('ATTDEL', cdfid, varid, 'name')
%     status = mexnc('DEL_ATT',ncid,varid,attname);

varargout = cell(1,nargout);

status = handle_del_att(op,ncid,varid,attname);
if status ~= 0
    status = -1;
end
switch nargout
    case 1
        varargout{1} = status;
        
end




%--------------------------------------------------------------------------
function varargout = handle_attget(op,ncid,varid,attname) %#ok<INUSL,DEFNU>
%     [att_value,status] = mexnc('GET_ATT_DOUBLE',ncid,varid,attname);
%     [att_value,status] = mexnc('GET_ATT_FLOAT', ncid,varid,attname);
%     [att_value,status] = mexnc('GET_ATT_INT',   ncid,varid,attname);
%     [att_value,status] = mexnc('GET_ATT_SHORT', ncid,varid,attname);
%     [att_value,status] = mexnc('GET_ATT_SCHAR', ncid,varid,attname);
%     [att_value,status] = mexnc('GET_ATT_UCHAR', ncid,varid,attname);
%     [att_value,status] = mexnc('GET_ATT_TEXT',  ncid,varid,attname);
%      [value, status] = mexnc('ATTGET', cdfid, varid, 'name')


if ischar(varid)
	if strcmpi(varid,'global')
		% needed for backwards compatibility.
		varid = -1;
	else
    	varid = netcdf.inqVarID(ncid,varid);
	end
end


varargout = cell(1,nargout);

% NETCDF-2 only returned double precision or char attributes.
xtype = netcdf.inqAtt(ncid,varid,attname);
if ( xtype == netcdf.getConstant('NC_CHAR') )
    op = 'GET_ATT_TEXT';
else
    op = 'GET_ATT_DOUBLE';
end


[varargout{:}] = handle_get_att(op,ncid,varid,attname);
if (nargout == 2) && (varargout{2} ~= 0)
    varargout{2} = -1;
end




%--------------------------------------------------------------------------
function varargout = handle_attinq(op,ncid,varid,attname) %#ok<DEFNU>
%      [datatype, len, status] = mexnc('ATTINQ', cdfid, varid, 'name')
%     [datatype,attlen,status] = mexnc('INQ_ATT',ncid,varid,attname);

varargout = cell(1,nargout);

[xtype,attlen,status] = handle_inq_att(op,ncid,varid,attname);
if status ~= 0
    status = -1;
end
switch nargout
    case 1
        varargout{1} = xtype;
    case 2
        varargout{1} = xtype;
        varargout{2} = attlen;
    case 3
        varargout{1} = xtype;
        varargout{2} = attlen;
        varargout{3} = status;
        
end




%--------------------------------------------------------------------------
function varargout = handle_attname(op,ncid,varid,attid) %#ok<DEFNU>
%     [attname,status] = mexnc('INQ_ATTNAME',ncid,varid,attid);
%      [name, status] = mexnc('ATTNAME', cdfid, varid, attnum)

varargout = cell(1,nargout);

[attname,status] = handle_inq_attname(op,ncid,varid,attid);
if status ~= 0
    status = -1;
end
switch nargout
    case 1
        varargout{1} = attname;
    case 2
        varargout{1} = attname;
        varargout{2} = status;
        
end




%--------------------------------------------------------------------------
function varargout = handle_attput(op,ncid,varid,attname,xtype,len,value) %#ok<DEFNU>
% status = mexnc('ATTPUT', cdfid, varid, 'name', datatype, value) 
% status = mexnc('ATTPUT', cdfid, varid, 'name', datatype, len, value) 
% status = mexnc('ATTPUT', cdfid, 'global', 'name', datatype, len, value) 

if nargin < 7
    value = len;
    len = numel(value); %#ok<NASGU>
end

if ischar(varid)
	if strcmpi(varid,'global')
		% needed for backwards compatibility.
		varid = -1;
	else
    	varid = netcdf.inqVarID(ncid,varid);
	end
end

% Don't need the length.
varargout = cell(1,nargout);


if ischar(xtype)
    xtype = lower(xtype);
    switch xtype
        case 'byte'
            xtype = nc_byte;
        case 'char'
            xtype = nc_char;
        case 'short'
            xtype = nc_short;
        case {'int', 'long'}
            xtype = nc_int;
        case 'float'
            xtype = nc_float;
        case 'double'
            xtype = nc_double;
        otherwise
            error('MEXNC:handle_attput:unhandledDatatype', ...
                  '%s is not a recognized datatype.', xtype );
    end
end
% Must cast the data to the intended datatype.
if (( xtype == 1 ) && ~(isa(value,'uint8') || isa(value,'int8')))
    value = int8(value);
elseif ( xtype == 3 ) && ~isa(value,'int16')
    value = int16(value);
elseif ( xtype == 4 ) && ~isa(value,'int32')
    value = int32(value);
elseif ( xtype == 5 ) && ~isa(value,'single')
    value = single(value);
elseif ( xtype == 6 ) && ~isa(value,'double')
    value = double(value);
end

status = handle_put_att(op,ncid,varid,attname,xtype,value);
if status ~= 0
    status = -1;
end
switch nargout
    case 1
        varargout{1} = status;
        
end




%--------------------------------------------------------------------------
function varargout = handle_attrename(op,ncid,varid,oldname,newname) %#ok<DEFNU>
%      status = mexnc('ATTRENAME', cdfid, varid, 'name', 'newname')
%     status = mexnc('RENAME_ATT',ncid,varid,old_attname,new_attname);

status = handle_rename_att(op,ncid,varid,oldname,newname);
if status ~= 0
    status = -1;
end
switch nargout
    case 1
        varargout{1} = status;
        
end



%--------------------------------------------------------------------------
function varargout = handle_dimdef(op,ncid,name,dimlen) %#ok<DEFNU>
%      status = mexnc('DIMDEF', cdfid, 'name', length)
%      [dimid,status] = mexnc('DEF_DIM',ncid,name,length);

[dimid,status] = handle_def_dim(op,ncid,name,dimlen);
if status ~= 0
    status = -1;
end
switch nargout
    case 1
        varargout{1} = dimid;
    case 2
        varargout{1} = dimid;
        varargout{2} = status;
        
end



%--------------------------------------------------------------------------
function varargout = handle_dimid(op,ncid,name) %#ok<DEFNU>
%      [dimid,status] = mexnc('INQ_DIMID',ncid,name);
%      [dimid, rcode] = mexnc('DIMID', cdfid, 'name')

[dimid,status] = handle_inq_dimid(op,ncid,name);
if status ~= 0
    status = -1;
end
switch nargout
    case 1
        varargout{1} = dimid;
    case 2
        varargout{1} = dimid;
        varargout{2} = status;
        
end



%--------------------------------------------------------------------------
function varargout = handle_diminq(op,ncid,dimid) %#ok<DEFNU>
%      [name, length, status] = mexnc('DIMINQ', cdfid, dimid)
%      [name, length,status] = mexnc('INQ_DIM',ncid,dimid);

% Turn a character dimid into the real dimid
if ischar(dimid)
	dimid = mexnc('inq_dimid',ncid,dimid);
end
[name,dimlen,status] = handle_inq_dim(op,ncid,dimid);
if status ~= 0
    status = -1;
end
switch nargout
    case 1
        varargout{1} = name;
    case 2
        varargout{1} = name;
        varargout{2} = dimlen;
    case 3
        varargout{1} = name;
        varargout{2} = dimlen;
        varargout{3} = status;
        
end




%--------------------------------------------------------------------------
function varargout = handle_dimrename(op,ncid,dimid,name) %#ok<DEFNU>
%      status = mexnc('DIMRENAME', cdfid, 'name')
%      status = mexnc('RENAME_DIM',ncid,dimid,name);

status = handle_rename_dim(op,ncid,dimid,name);
if status ~= 0
    status = -1;
end
switch nargout
    case 1
        varargout{1} = status;
end



%--------------------------------------------------------------------------
function varargout = handle_endef(op,ncid) %#ok<DEFNU>
%      status = mexnc('ENDEF', cdfid)
%      status = mexnc('ENDDEF',ncid);

status = handle_enddef(op,ncid);
if status ~= 0
    status = -1;
end
switch nargout
    case 1
        varargout{1} = status;
end




%----------------------------------------------------------------
function varargout = handle_typelen(op,datatype) %#ok<INUSL,DEFNU>
%      len = mexnc('TYPELEN', datatype)

switch ( datatype )
    case 0
        len = -1;
        status = 1;
    case 1
        len = 1;
        status = 0;
    case 2
        len = 1;
        status = 0;
    case 3
        len = 2;
        status = 0;
    case 4
        len = 4;
        status = 0;
    case 5
        len = 4;
        status = 0;
    case 6
        len = 8;
        status = 0;
    otherwise
        len = -1;
        status = 1;
end


switch nargout
    case 1
        varargout{1} = len;
    case 2
        varargout{1} = len;
        varargout{2} = status;
end





%----------------------------------------------------------------
function varargout = handle_inquire(op,ncid) %#ok<DEFNU>
%      [ndims, nvars, natts, recdim, status] = mexnc('INQUIRE', cdfid)
%      [ndims,nvars, ngatts, unlimdim, status] = mexnc('INQ',ncid);

global use_tmw;

% Get all five outputs.
if use_tmw
    [ndims,nvars,ngatts,unlimdim,status] = handle_inq(op,ncid);
else
    [ndims,nvars,ngatts,unlimdim,status] = mexnc('INQ',ncid);
end

switch nargout
    case 1
        % In this case, return all the outputs as a single vector.
        % This is special to this function only.
        varargout{1}(1) = ndims;
        varargout{1}(2) = nvars;
        varargout{1}(3) = ngatts;
        varargout{1}(4) = unlimdim;
        varargout{1}(5) = status;

    case 2
        varargout{1} = ndims;
        varargout{2} = nvars;
    case 3
        varargout{1} = ndims;
        varargout{2} = nvars;
        varargout{3} = ngatts;
    case 4
        varargout{1} = ndims;
        varargout{2} = nvars;
        varargout{3} = ngatts;
        varargout{4} = unlimdim;
    case 5
        varargout{1} = ndims;
        varargout{2} = nvars;
        varargout{3} = ngatts;
        varargout{4} = unlimdim;
        varargout{5} = status;
end









%--------------------------------------------------------------------------
function varargout = handle_vardef(op,ncid,name,xtype,ndims,dimids) %#ok<DEFNU>
%      [varid,status] = mexnc('DEF_VAR',ncid,name,xtype,dimids);
%      [varid,status] = mexnc('DEF_VAR',ncid,name,xtype,ndims,dimids);
%      status = mexnc('VARDEF', cdfid, 'name', datatype, ndims, [dim])


if (nargin < 6)
    dimids = ndims;
    ndims = numel(dimids);
end

% Don't pass ndims, but tell the user if they are wrong!
% Stupid netcdf toolbox let users pass -1 as the length.
% if ndims is 0, don't bother checking it against the number of elements.
if (ndims == 0) && ~isempty(dimids)
    % Stupid user.  They are saying that the number of dimensions is zero, yet they
    % give a list of dimension IDS.  We assume they really meant zero dimensions.
    dimids = [];
    ndims = 0;
elseif (ndims == -1)
    % Stupid user.  -1 is their way of saying to compute the number of dimensions
    % for me.  TMW already does this automatically
    ndims = numel(dimids);
elseif (ndims ~= numel(dimids)) 
    error('MEXNC:handle_def_var:numDimsMismatch', ...
          'The given number of dimensions was not the same as the length of the dimids.');
end

[varid,status] = handle_def_var(op,ncid,name,xtype,ndims,dimids);
if status ~= 0
    status = -1;
end
switch nargout
    case 1
        varargout{1} = varid;
    case 2
        varargout{1} = varid;
        varargout{2} = status;
end




%--------------------------------------------------------------------------
function varargout = handle_varid(op,ncid,varname) %#ok<DEFNU>
%      [varid,status] = mexnc('INQ_VARID',ncid,varname);
%      [varid, rcode] = mexnc('VARID', cdfid, 'name')

[varid,status] = handle_inq_varid(op,ncid,varname);
if status ~= 0
    status = -1;
end
switch nargout
    case 1
        varargout{1} = varid;
    case 2
        varargout{1} = varid;
        varargout{2} = status;
end




%--------------------------------------------------------------------------
function varargout = handle_varinq(op,ncid,varid) %#ok<DEFNU>
% [name, datatype, ndims, dimids, natts, status] = mexnc('VARINQ', cdfid, varid)
% [varname,xtype,  ndims, dimids, natts, status] = mexnc('INQ_VAR',ncid,varid);

[varname,xtype,ndims,dimids,natts,status] = handle_inq_var(op,ncid,varid);
if status ~= 0
    status = -1;
end
switch nargout
    case 1
        varargout{1} = varname;
    case 2
        varargout{1} = varname;
        varargout{2} = xtype;
    case 3
        varargout{1} = varname;
        varargout{2} = xtype;
        varargout{3} = ndims;
    case 4
        varargout{1} = varname;
        varargout{2} = xtype;
        varargout{3} = ndims;
        varargout{4} = dimids;
    case 5
        varargout{1} = varname;
        varargout{2} = xtype;
        varargout{3} = ndims;
        varargout{4} = dimids;
        varargout{5} = natts;
    case 6
        varargout{1} = varname;
        varargout{2} = xtype;
        varargout{3} = ndims;
        varargout{4} = dimids;
        varargout{5} = natts;
        varargout{6} = status;
end




%--------------------------------------------------------------------------
function varargout = handle_varrename(op,ncid,varid,newname) %#ok<DEFNU>
%      status = mexnc('VARRENAME', cdfid, varid, 'name')
%      status = mexnc('RENAME_VAR',ncid,varid,new_varname);

status = handle_rename_var(op,ncid,varid,newname);
if status ~= 0
    status = -1;
end
switch nargout
    case 1
        varargout{1} = status;
end




%--------------------------------------------------------------------------
function varargout = handle_varput1(op,ncid,varid,start,data,autoscale) %#ok<INUSL,DEFNU>
%     status = mexnc('VARPUT1',        ncid,varid,start,value, autoscale)
%
%     status = mexnc('PUT_VAR1_DOUBLE',ncid,varid,start,data);
%     status = mexnc('PUT_VAR1_FLOAT', ncid,varid,start,data);
%     status = mexnc('PUT_VAR1_INT',   ncid,varid,start,data);
%     status = mexnc('PUT_VAR1_SHORT', ncid,varid,start,data);
%     status = mexnc('PUT_VAR1_SCHAR', ncid,varid,start,data);
%     status = mexnc('PUT_VAR1_UCHAR', ncid,varid,start,data);
%     status = mexnc('PUT_VAR1_TEXT',  ncid,varid,start,data);

error(nargchk(5,6,nargin,'struct'));

if ischar(varid)
    varid = netcdf.inqVarID(ncid,varid);
end

switch(class(data))
    case { 'double', 'char' }
        
    otherwise
        error('MEXCDF:mexnc:badDatatype', ...
              'VARPUT1 required either double or char data.' );
end

try
    [varname,xtype,dimids] = netcdf.inqVar(ncid,varid); %#ok<ASGLU>
catch %#ok<CTCH>
    varargout{1} = 1;
    return
end


% Scale the input if necessary.
if (nargin == 6) && (autoscale == 1)
    data = handle_nc2_input_scaling(ncid,varid,data);
end

% Must flip the start and count arguments.
if (nargin >= 4)
    start = fliplr((start(:))');
end

try
    if isempty(dimids)
        % Singleton case, write ALL the data.
        netcdf.putVar(ncid,varid,data);
    else
        netcdf.putVar(ncid,varid,start,data);
    end
    status = 0;
catch %#ok<CTCH>
    status = 1;
end

switch nargout
    case 1
        varargout{1} = status;
end




%--------------------------------------------------------------------------
function varargout = handle_varget1(op,ncid,varid,start,autoscale) %#ok<INUSL,DEFNU>
%      [value, status] = mexnc('VARGET1', cdfid, varid, coords, autoscale)
%     [data,status] = mexnc('GET_VAR1_DOUBLE',ncid,varid,start);
%     [data,status] = mexnc('GET_VAR1_FLOAT', ncid,varid,start);
%     [data,status] = mexnc('GET_VAR1_INT',   ncid,varid,start);
%     [data,status] = mexnc('GET_VAR1_SHORT', ncid,varid,start);
%     [data,status] = mexnc('GET_VAR1_SCHAR', ncid,varid,start);
%     [data,status] = mexnc('GET_VAR1_UCHAR', ncid,varid,start);
%     [data,status] = mexnc('GET_VAR1_TEXT',  ncid,varid,start);

error(nargchk(4,5,nargin,'struct'));

if ischar(varid)
    varid = netcdf.inqVarID(ncid,varid);
end

% If there was an autoscale argument, we need to get rid of it before
% passing it into the netcdf package.
try
    [varname,xtype,dimids] = netcdf.inqVar(ncid,varid); %#ok<ASGLU>
catch %#ok<CTCH>
    varargout{2} = 1;
    return
end

% Make sure we only return double or char data.
try
    if ( xtype == netcdf.getConstant('NC_CHAR') )
        output_type = 'char';
    else
        output_type = 'double';
    end
catch %#ok<CTCH>
    varargout{1} = NaN;
    varargout{2} = -1;
    return;
end


% Must flip the start arguments.
if (nargin >= 4)
    start = fliplr(start);
end


try
    if isempty(dimids)
        % singleton case, don't supply the index.
        data = netcdf.getVar(ncid,varid,output_type);
    else
        data = netcdf.getVar(ncid,varid,start,output_type);
    end
    status = 0;
catch %#ok<CTCH>
    data = NaN;
    status = -1;
end




if (nargin == 5) && (autoscale == 1)
    data = handle_nc2_output_scaling(ncid,varid,data);
end

switch nargout
   case 1
        varargout{1} = data;
    case 2
        varargout{1} = data;
        varargout{2} = status;
end

%------------------------------------------------------------------------------------------
function varargout = handle_varput(op,ncid,varid,start,count,data,autoscale) %#ok<DEFNU,INUSL>
%      status = mexnc('VARPUT', cdfid, varid, start, count, value, autoscale)



if ischar(varid)
    varid = netcdf.inqVarID(ncid,varid);
end

switch(class(data))
    case { 'double', 'char' }
        
    otherwise
        error('MEXCDF:mexnc:badDatatype', ...
              'VARPUT required either double or char data.' );
end

% Scale the input if necessary.
if (nargin == 7) && (autoscale == 1)
    data = handle_nc2_input_scaling(ncid,varid,data);
end


% Must flip the start and count arguments.
if (nargin >= 4)
    start = fliplr((start(:))');
    count = fliplr((count(:))');
end

% account for any negative counts.
if any(count<0)
    idx = find(count<0);
    [varname,xtype,dimids] = netcdf.inqVar(ncid,varid);  %#ok<ASGLU>
    for j = 1:numel(idx)
        [dud,dimlen] = netcdf.inqDim(ncid,dimids(idx(j))); %#ok<ASGLU>
        count(idx(j)) = dimlen - start(idx(j));
    end
end

try
    netcdf.putVar(ncid,varid,start,count,data);
    status = 0;
catch %#ok<CTCH>
    status = 1;
end

switch nargout
    case 1
        varargout{1} = status;
end


%------------------------------------------------------------------------------------------
function [storage,chunksize,status] = handle_inq_var_chunking(op,ncid,varid) %#ok<DEFNU,INUSL>
% [storage,chunksize,status] = mexnc('inq_var_chunking',ncid,xdvarid);

validateattributes(ncid,{'numeric'},{'real','nonempty','finite'});
validateattributes(varid,{'numeric'},{'real','nonempty','finite'});

try
    [storage,chunksize] = netcdf.inqVarChunking(ncid,varid);
    status = 0;
catch me %#ok<NASGU>
    status = -1;
end
%------------------------------------------------------------------------------------------
function status = handle_def_var_chunking(op,ncid,varid,storage,chunksize) %#ok<DEFNU,INUSL>
% status = mexnc('DEF_VAR_CHUNKING',ncid,varid,storage,chunksize)

validateattributes(ncid,{'numeric'},{'real','nonempty','finite'});
validateattributes(varid,{'numeric'},{'real','nonempty','finite'});
validateattributes(storage,{'char'},{'row'});
if strcmpi(storage,'chunked')
    validateattributes(chunksize,{'numeric'},{'real','nonempty','finite'});
else
    validateattributes(chunksize,{'numeric'},{'real','finite'});
end

try
    netcdf.defVarChunking(ncid,varid,storage,chunksize);
    status = 0;
catch me %#ok<NASGU>
    status = -1;
end


%------------------------------------------------------------------------------------------
function status = handle_def_var_deflate(op,ncid,varid,shuffle,deflate,deflate_level) %#ok<DEFNU,INUSL>
% status = mexnc('DEF_VAR_CHUNKING',ncid,varid,storage,chunksize)

validateattributes(ncid,{'numeric'},{'real','nonempty','finite'});
validateattributes(varid,{'numeric'},{'real','nonempty','finite'});
validateattributes(shuffle,{'numeric'},{'real','nonempty','finite'});
validateattributes(deflate,{'numeric'},{'real','nonempty','finite'});
validateattributes(deflate_level,{'numeric'},{'real','nonempty','finite'});


try
    netcdf.defVarDeflate(ncid,varid,logical(shuffle),logical(deflate),deflate_level);
    status = 0;
catch me %#ok<NASGU>
    status = -1;
end

%------------------------------------------------------------------------------------------
function [shuffle,deflate,deflate_level,status] = handle_inq_var_deflate(op,ncid,varid) %#ok<DEFNU,INUSL>
% [shuffle,deflate,deflate_level,status] = mexnc('INQ_VAR_DEFLATE',ncid,varid)

validateattributes(ncid,{'numeric'},{'real','nonempty','finite'});
validateattributes(varid,{'numeric'},{'real','nonempty','finite'});

shuffle = false;
deflate = false;
deflate_level = 0;
try
    [shuffle,deflate,deflate_level] = netcdf.inqVarDeflate(ncid,varid);
    status = 0;
catch me %#ok<NASGU>
    status = -1;
end
