function test_inq_var ( ncfile )
% TEST_INQ_VAR
%
% Tests number of dimensions, variables, global attributes, record dimension for
% foo.nc.  Also tests helper routines, "nc_inq_ndims", "nc_inq_nvars", "nc_inq_ncatts".
%
% Test 1:  INQ_VAR:  Normal retrieval
% Test 2:  INQ_VAR:  Bad ncid.
% Test 3:  INQ_VAR:  Empty set ncid.
% Test 4:  INQ_VAR:  Non numeric ncid
% Test 5:  INQ_VAR:  Bad varid.
% Test 6:  INQ_VAR:  Empty set varid.
% Test 7:  INQ_VAR:  Non numeric varid
% Test 11:  INQ_VARNAME:  Normal retrieval
% Test 12:  INQ_VARNAME:  Bad ncid.
% Test 13:  INQ_VARNAME:  Empty set ncid.
% Test 14:  INQ_VARNAME:  Non numeric ncid
% Test 15:  INQ_VARNAME:  Bad varid.
% Test 16:  INQ_VARNAME:  Empty set varid.
% Test 17:  INQ_VARNAME:  Non numeric varid
% Test 21:  INQ_VARTYPE:  Normal retrieval
% Test 22:  INQ_VARTYPE:  Bad ncid.
% Test 23:  INQ_VARTYPE:  Empty set ncid.
% Test 24:  INQ_VARTYPE:  Non numeric ncid
% Test 25:  INQ_VARTYPE:  Bad varid.
% Test 26:  INQ_VARTYPE:  Empty set varid.
% Test 27:  INQ_VARTYPE:  Non numeric varid
% Test 31:  INQ_VARNDIMS:  Normal retrieval
% Test 32:  INQ_VARNDIMS:  Bad ncid.
% Test 33:  INQ_VARNDIMS:  Empty set ncid.
% Test 34:  INQ_VARNDIMS:  Non numeric ncid
% Test 35:  INQ_VARNDIMS:  Bad varid.
% Test 36:  INQ_VARNDIMS:  Empty set varid.
% Test 37:  INQ_VARNDIMS:  Non numeric varid
% Test 41:  INQ_VARDIMID:  Normal retrieval
% Test 42:  INQ_VARDIMID:  Bad ncid.
% Test 43:  INQ_VARDIMID:  Empty set ncid.
% Test 44:  INQ_VARDIMID:  Non numeric ncid
% Test 45:  INQ_VARDIMID:  Bad varid.
% Test 46:  INQ_VARDIMID:  Empty set varid.
% Test 47:  INQ_VARDIMID:  Non numeric varid
% Test 51:  INQ_VARNATTS:  Normal retrieval
% Test 52:  INQ_VARNATTS:  Bad ncid.
% Test 53:  INQ_VARNATTS:  Empty set ncid.
% Test 54:  INQ_VARNATTS:  Non numeric ncid
% Test 55:  INQ_VARNATTS:  Bad varid.
% Test 56:  INQ_VARNATTS:  Empty set varid.
% Test 57:  INQ_VARNATTS:  Non numeric varid

error_condition = 0;

if nargin < 1
    ncfile = 'foo.nc';
end

% Create a netcdf file with
[ncid, status] = mexnc ( 'create', ncfile, nc_clobber_mode );
if ( status ~= 0 )
	error ( 'CREATE failed' );
end


%
% DIMDEF
[xdimid, status] = mexnc ( 'def_dim', ncid, 'x', 20 );
if ( status ~= 0 )
	error ( 'DEF_DIM failed on X' );
end
[ydimid, status] = mexnc ( 'def_dim', ncid, 'y', 24 );
if ( status ~= 0 )
	error ( 'DEF_DIM failed on y' );
end
[zdimid, status] = mexnc ( 'def_dim', ncid, 'z', 32 );
if ( status ~= 0 )
	error ( 'DEF_DIM failed on z' );
end


%
% VARDEF
[xdvarid, status] = mexnc ( 'def_var', ncid, 'x_double', 'double', 1, xdimid );
if ( status ~= 0 )
	error ( 'DEF_VAR failed on x_double' );
end


%
% Define some attributes
attvalue = 'this is a test';
attlen = length(attvalue);
status = mexnc ( 'put_att_text', ncid, xdvarid, 'test_variable_attributes', 'char', attlen, attvalue );
if ( status ~= 0 )
	ncerr = mexnc ( 'strerror', status );
	err_msg = sprintf ( '%s:  put_att_double failed on variable attribute, ''%s''\n', mfilename, ncerr );
	error ( err_msg );
end


%
% ENDEF
[status] = mexnc ( 'enddef', ncid );
if ( status ~= 0 )
	error ( 'ENDEF failed with write' );
end



%
% INQ_VAR
testid = 'Test 1';
[name, datatype, ndims, dimids, natts, status] = mexnc('INQ_VAR', ncid, xdvarid);
if ( status ~= 0 )
	msg = sprintf ( '%s:  INQ_VAR failed return status\n', mfilename );
	error ( msg );
end
if ~strcmp ( 'x_double', name )
	msg = sprintf ( '%s:  INQ_VARNAME failed\n', mfilename, ncerr );
	error ( msg );
end
if ( datatype ~= 6 )
	msg = sprintf ( '%s:  INQ_VAR failed on datatype\n', mfilename );
	error ( msg );
end
if ( ndims ~= 1 )
	msg = sprintf ( '%s:  INQ_VAR failed on ndims\n', mfilename );
	error ( msg );
end
if ( dimids ~= 0 )
	msg = sprintf ( '%s:  INQ_VAR failed on dimids\n', mfilename );
	error ( msg );
end
if ( natts ~= 1 )
	msg = sprintf ( '%s:  INQ_VAR failed on variable attributes\n', mfilename );
	error ( msg );
end





% Test 2:  Bad ncid.
testid = 'Test 2';
[name, datatype, ndims, dimids, natts, status] = mexnc('INQ_VAR', -20000, xdvarid);
if ( status == 0 )
	err_msg = sprintf ( '%s:  %s:  Succeeded when it should have failed\n', mfilename, testid );
	error ( err_msg );
end




% Test 3:  Empty set ncid.
testid = 'Test 3';
try
	[name, datatype, ndims, dimids, natts, status] = mexnc('INQ_VAR', [], xdvarid);
	error_condition = 1;
end
if error_condition == 1
	err_msg = sprintf ( '%s:  %s:  Succeeded when it should have failed\n', mfilename, testid );
	error ( err_msg );
end






% Test 4:  Non numeric ncid
testid = 'Test 4';
try
	[name, datatype, ndims, dimids, natts, status] = mexnc('INQ_VAR', 'ncid', xdvarid);
	error_condition = 1;
end
if error_condition == 1
	err_msg = sprintf ( '%s:  %s:  Succeeded when it should have failed\n', mfilename, testid );
	error ( err_msg );
end





% Test 5:  Bad varid.
testid = 'Test 5';
[name, datatype, ndims, dimids, natts, status] = mexnc('INQ_VAR', ncid, -20000);
if ( status == 0 )
	err_msg = sprintf ( '%s:  %s:  Succeeded when it should have failed\n', mfilename, testid );
	error ( err_msg );
end




% Test 6:  Empty set varid.
testid = 'Test 6';
try
	[name, datatype, ndims, dimids, natts, status] = mexnc('INQ_VAR', ncid, []);
	error_condition = 1;
end
if error_condition == 1
	err_msg = sprintf ( '%s:  %s:  Succeeded when it should have failed\n', mfilename, testid );
	error ( err_msg );
end






% Test 7:  Non numeric dimid
testid = 'Test 7';
try
	[name, datatype, ndims, dimids, natts, status] = mexnc('INQ_VAR', ncid, 'xdvarid');
	error_condition = 1;
end
if error_condition == 1
	err_msg = sprintf ( '%s:  %s:  Succeeded when it should have failed\n', mfilename, testid );
	error ( err_msg );
end





%
% INQ_VARNAME
testid = 'Test 11';
[name, status] = mexnc('INQ_VARNAME', ncid, xdvarid);
if ( status ~= 0 )
	msg = sprintf ( '%s:  INQ_VARNAME failed return status\n', mfilename );
	error ( msg );
end
if ~strcmp ( 'x_double', name )
	msg = sprintf ( '%s:  INQ_VARNAMENAME failed\n', mfilename, ncerr );
	error ( msg );
end





%--------------------------------------------------------------------------
% Test 12:  Bad ncid.
[name, status] = mexnc('INQ_VARNAME', -20000, xdvarid); %#ok<ASGLU>
if ( status == 0 )
	error('Succeeded when it should have failed');
end




%--------------------------------------------------------------------------
% Test 13:  Empty set ncid.
try %#ok<TRYNC>
	name = mexnc('INQ_VARNAME', [], xdvarid); %#ok<NASGU>
	error_condition = 1;
end
if error_condition == 1
	error('Succeeded when it should have failed');
end






%--------------------------------------------------------------------------
% Test 14:  Non numeric ncid

try %#ok<TRYNC>
	name = mexnc('INQ_VARNAME', 'ncid', xdvarid); %#ok<NASGU>
	error_condition = 1;
end
if error_condition == 1
	error('Succeeded when it should have failed');
end




%--------------------------------------------------------------------------
% Test 15:  Bad varid.
testid = 'Test 15';
[name, status] = mexnc('INQ_VARNAME', ncid, -20000);
if ( status == 0 )
	err_msg = sprintf ( '%s:  %s:  Succeeded when it should have failed\n', mfilename, testid );
	error ( err_msg );
end




% Test 16:  Empty set varid.
testid = 'Test 16';
try
	[name, status] = mexnc('INQ_VARNAME', ncid, []);
	error_condition = 1;
end
if error_condition == 1
	err_msg = sprintf ( '%s:  %s:  Succeeded when it should have failed\n', mfilename, testid );
	error ( err_msg );
end






% Test 17:  Non numeric dimid
testid = 'Test 17';
try
	[name, status] = mexnc('INQ_VARNAME', ncid, 'xdvarid');
	error_condition = 1;
end
if error_condition == 1
	err_msg = sprintf ( '%s:  %s:  Succeeded when it should have failed\n', mfilename, testid );
	error ( err_msg );
end




%
% INQ_VARTYPE
testid = 'Test 21';
[datatype, status] = mexnc('INQ_VARTYPE', ncid, xdvarid);
if ( status ~= 0 )
	msg = sprintf ( '%s:  INQ_VARTYPE failed return status\n', mfilename );
	error ( msg );
end
if ( datatype ~= nc_double )
	msg = sprintf ( '%s:  INQ_VARTYPE failed on datatype\n', mfilename );
	error ( msg );
end




%--------------------------------------------------------------------------
% Test 22:  Bad ncid.
[datatype, status] = mexnc('INQ_VARTYPE', -20000, xdvarid); %#ok<ASGLU>
if ( status == 0 )
	error('Succeeded when it should have failed');
end




%--------------------------------------------------------------------------
% Test 23:  Empty set ncid.
try %#ok<TRYNC>
	mexnc('INQ_VARTYPE', [], xdvarid);
	error_condition = 1;
end
if error_condition == 1
	error('Succeeded when it should have failed');
end






%--------------------------------------------------------------------------
% Test 24:  Non numeric ncid
try %#ok<TRYNC>
	mexnc('INQ_VARTYPE', 'ncid', xdvarid);
	error_condition = 1;
end
if error_condition == 1
	error('Succeeded when it should have failed');
end





%--------------------------------------------------------------------------
% Test 25:  Bad varid.
[datatype, status] = mexnc('INQ_VARTYPE', ncid, -20000); %#ok<ASGLU>
if ( status == 0 )
	error('Succeeded when it should have failed');
end




% Test 26:  Empty set varid.
testid = 'Test 26';
try
	[datatype, status] = mexnc('INQ_VARTYPE', ncid, []);
	error_condition = 1;
end
if error_condition == 1
	err_msg = sprintf ( '%s:  %s:  Succeeded when it should have failed\n', mfilename, testid );
	error ( err_msg );
end






% Test 27:  Non numeric dimid
testid = 'Test 27';
try
	[datatype, status] = mexnc('INQ_VARTYPE', ncid, 'xdvarid');
	error_condition = 1;
end
if error_condition == 1
	err_msg = sprintf ( '%s:  %s:  Succeeded when it should have failed\n', mfilename, testid );
	error ( err_msg );
end





%
% INQ_VARNDIMS
testid = 'Test 31';
[ndims, status] = mexnc('INQ_VARNDIMS', ncid, xdvarid);
if ( status ~= 0 )
	msg = sprintf ( '%s:  INQ_VARNDIMS failed return status\n', mfilename );
	error ( msg );
end
if ( ndims ~= 1 )
	msg = sprintf ( '%s:  INQ_VARNDIMS failed on number of dimensions\n', mfilename );
	error ( msg );
end




%--------------------------------------------------------------------------
% Test 32:  Bad ncid.
[ndims, status] = mexnc('INQ_VARNDIMS', -20000, xdvarid); %#ok<ASGLU>
if ( status == 0 )
	error('Succeeded when it should have failed');
end



%--------------------------------------------------------------------------
% Test 33:  Empty set ncid.
try %#ok<TRYNC>
	mexnc('INQ_VARNDIMS', [], xdvarid);
	error_condition = 1;
end
if error_condition == 1
	error('Succeeded when it should have failed');
end






%--------------------------------------------------------------------------
% Test 34:  Non numeric ncid
try %#ok<TRYNC>
	mexnc('INQ_VARNDIMS', 'ncid', xdvarid);
	error_condition = 1;
end
if error_condition == 1
	error('Succeeded when it should have failed');
end





%--------------------------------------------------------------------------
% Test 35:  Bad varid.
[ndims, status] = mexnc('INQ_VARNDIMS', ncid, -20000); %#ok<ASGLU>
if ( status == 0 )
	error('Succeeded when it should have failed');
end




% Test 36:  Empty set varid.
testid = 'Test 36';
try
	[ndims, status] = mexnc('INQ_VARNDIMS', ncid, []);
	error_condition = 1;
end
if error_condition == 1
	err_msg = sprintf ( '%s:  %s:  Succeeded when it should have failed\n', mfilename, testid );
	error ( err_msg );
end






% Test 37:  Non numeric dimid
testid = 'Test 37';
try
	[ndims, status] = mexnc('INQ_VARNDIMS', ncid, 'xdvarid');
	error_condition = 1;
end
if error_condition == 1
	err_msg = sprintf ( '%s:  %s:  Succeeded when it should have failed\n', mfilename, testid );
	error ( err_msg );
end





%
% INQ_VARDIMID
testid = 'Test 41';
[dimids, status] = mexnc('INQ_VARDIMID', ncid, xdvarid);
if ( status ~= 0 )
	msg = sprintf ( '%s:  INQ_VARDIMID failed return status\n', mfilename );
	error ( msg );
end
if ( dimids ~= 0 )
	msg = sprintf ( '%s:  INQ_VAR failed on dimids\n', mfilename );
	error ( msg );
end




%--------------------------------------------------------------------------
% Test 42:  Bad ncid.
[dimids, status] = mexnc('INQ_VARDIMID', -20000, xdvarid); %#ok<ASGLU>
if ( status == 0 )
	error('Succeeded when it should have failed');
end



%--------------------------------------------------------------------------
% Test 43:  Empty set ncid.
try %#ok<TRYNC>
	mexnc('INQ_VARDIMID', [], xdvarid);
	error_condition = 1;
end
if error_condition == 1
	error('Succeeded when it should have failed');
end





%--------------------------------------------------------------------------
% Test 44:  Non numeric ncid
try %#ok<TRYNC>
	mexnc('INQ_VARDIMID', 'ncid', xdvarid);
	error_condition = 1;
end
if error_condition == 1
	error('Succeeded when it should have failed');
end





%--------------------------------------------------------------------------
% Test 45:  Bad varid.
[dimids, status] = mexnc('INQ_VARDIMID', ncid, -20000); %#ok<ASGLU>
if ( status == 0 )
	error('Succeeded when it should have failed');
end




% Test 46:  Empty set varid.
testid = 'Test 46';
try
	[dimids, status] = mexnc('INQ_VARDIMID', ncid, []);
	error_condition = 1;
end
if error_condition == 1
	err_msg = sprintf ( '%s:  %s:  Succeeded when it should have failed\n', mfilename, testid );
	error ( err_msg );
end






% Test 47:  Non numeric dimid
testid = 'Test 47';
try
	[dimids, status] = mexnc('INQ_VARDIMID', ncid, 'xdvarid');
	error_condition = 1;
end
if error_condition == 1
	err_msg = sprintf ( '%s:  %s:  Succeeded when it should have failed\n', mfilename, testid );
	error ( err_msg );
end





%
% INQ_VARNATTS
testid = 'Test 51';
[varnatts, status] = mexnc('INQ_VARNATTS', ncid, xdvarid);
if ( status ~= 0 )
	msg = sprintf ( '%s:  INQ_VARNATTS failed return status\n', mfilename );
	error ( msg );
end
if ( varnatts ~= 1 )
	msg = sprintf ( '%s:  INQ_VARNATTS failed on number of attributes\n', mfilename );
	error ( msg );
end




%--------------------------------------------------------------------------
% Test 52:  Bad ncid.
[varnatts, status] = mexnc('INQ_VARNATTS', -20000, xdvarid); %#ok<ASGLU>
if ( status == 0 )
	error('Succeeded when it should have failed\n');
end



%--------------------------------------------------------------------------
% Test 53:  Empty set ncid.
try %#ok<TRYNC>
	mexnc('INQ_VARNATTS', [], xdvarid);
	error_condition = 1;
end
if error_condition == 1
	error('Succeeded when it should have failed');
end





%--------------------------------------------------------------------------
% Test 54:  Non numeric ncid
try %#ok<TRYNC>
	mexnc('INQ_VARNATTS', 'ncid', xdvarid);
	error_condition = 1;
end
if error_condition == 1
	error('Succeeded when it should have failed');
end




%--------------------------------------------------------------------------
% Test 55:  Bad varid.
[varnatts, status] = mexnc('INQ_VARNATTS', ncid, -20000); %#ok<ASGLU>
if ( status == 0 )
	error('Succeeded when it should have failed');
	error ( err_msg );
end




% Test 56:  Empty set varid.
testid = 'Test 56';
try
	[varnatts, status] = mexnc('INQ_VARNATTS', ncid, []);
	error_condition = 1;
end
if error_condition == 1
	err_msg = sprintf ( '%s:  %s:  Succeeded when it should have failed\n', mfilename, testid );
	error ( err_msg );
end






% Test 57:  Non numeric dimid
testid = 'Test 57';
try
	[varnatts, status] = mexnc('INQ_VARNATTS', ncid, 'xdvarid');
	error_condition = 1;
end
if error_condition == 1
	err_msg = sprintf ( '%s:  %s:  Succeeded when it should have failed\n', mfilename, testid );
	error ( err_msg );
end





fprintf ( 1, 'INQ_VAR succeeded\n' );
fprintf ( 1, 'INQ_VARNAME succeeded\n' );
fprintf ( 1, 'INQ_VARTYPE succeeded\n' );
fprintf ( 1, 'INQ_VARNDIMS succeeded\n' );
fprintf ( 1, 'INQ_VARDIMID succeeded\n' );
fprintf ( 1, 'INQ_VARNATTS succeeded\n' );







status = mexnc ( 'close', ncid );
if ( status < 0 )
	ncerr = mexnc ( 'strerror', status );
	msg = sprintf ( '%s:  %s\n', mfilename, ncerr );
	error ( msg );
end

return











