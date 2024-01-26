function test_VARINQ ( ncfile )
% TEST_VARINQ
%
% Tests number of dimensions, variables, global attributes, record dimension for
% foo.nc.  
%
% Tests bad ncid as well.
%
% Test 1:  Normal retrieval
% Test 2:  Bad ncid
% Test 3:  Bad varid
% Test 4:  Non numeric ncid
% Test 5:  Non numeric varid

%
% Create a netcdf file with
[ncid, status] = mexnc ( 'create', ncfile, nc_clobber_mode );
if ( status < 0 )
	ncerr = mexnc ( 'strerror', status );
	err_msg = sprintf ( '%s:  ''%s''\n', mfilename, ncerr );
	error ( err_msg );
end


%
% DIMDEF
[xdimid, status] = mexnc ( 'def_dim', ncid, 'x', 20 );
if ( status < 0 )
	ncerr = mexnc ( 'strerror', status );
	err_msg = sprintf ( '%s:  ''%s''\n', mfilename, ncerr );
	error ( err_msg );
end
[ydimid, status] = mexnc ( 'def_dim', ncid, 'y', 24 );
if ( status < 0 )
	ncerr = mexnc ( 'strerror', status );
	err_msg = sprintf ( '%s:  ''%s''\n', mfilename, ncerr );
	error ( err_msg );
end
[zdimid, status] = mexnc ( 'def_dim', ncid, 'z', 32 );
if ( status < 0 )
	ncerr = mexnc ( 'strerror', status );
	err_msg = sprintf ( '%s:  ''%s''\n', mfilename, ncerr );
	error ( err_msg );
end


%
% VARDEF
[xdvarid, status] = mexnc ( 'def_var', ncid, 'x_double', 'double', 1, xdimid );
if ( status < 0 )
	ncerr = mexnc ( 'strerror', status );
	err_msg = sprintf ( '%s:  ''%s''\n', mfilename, ncerr );
	error ( err_msg );
end


%
% Define some attributes
attvalue = 'this is a test';
attlen = length(attvalue);
status = mexnc ( 'put_att_text', ncid, xdvarid, 'test_variable_attributes', 'char', attlen, attvalue );
if ( status < 0 )
	ncerr = mexnc ( 'strerror', status );
	err_msg = sprintf ( '%s:  put_att_double failed on variable attribute, ''%s''\n', mfilename, ncerr );
	error ( err_msg );
end


%
% ENDEF
[status] = mexnc ( 'enddef', ncid );
if ( status < 0 )
	ncerr = mexnc ( 'strerror', status );
	err_msg = sprintf ( '%s:  ''%s''\n', mfilename, ncerr );
	error ( err_msg );
end



%
% VARINQ
[name, datatype, ndims, dimids, natts, status] = mexnc('VARINQ', ncid, xdvarid);
if ( status < 0 )
	ncerr = mexnc ( 'strerror', status );
	err_msg = sprintf ( '%s:  ''%s''\n', mfilename, ncerr );
	error ( err_msg );
end
if ~strcmp ( 'x_double', name )
	msg = sprintf ( '%s:  VARINQ failed\n', mfilename, ncerr );
	error ( msg );
end
if ( datatype ~= 6 )
	msg = sprintf ( '%s:  VARINQ failed on datatype\n', mfilename );
	error ( msg );
end
if ( ndims ~= 1 )
	msg = sprintf ( '%s:  VARINQ failed on ndims\n', mfilename );
	error ( msg );
end
if ( dimids ~= 0 )
	msg = sprintf ( '%s:  VARINQ failed on dimids\n', mfilename );
	error ( msg );
end
if ( natts ~= 1 )
	msg = sprintf ( '%s:  VARINQ failed on variable attributes\n', mfilename );
	error ( msg );
end


%
% Try a bogus case
[name, datatype, ndims, dimids, natts, status] = mexnc('VARINQ', -1, 0);
if ( status >= 0 )
	error ( 'VARINQ return status did not signal an error on bogus ncid case' );
end
[name, datatype, ndims, dimids, natts, status] = mexnc('VARINQ', ncid, -1);
if ( status >= 0 )
	error ( 'VARINQ return status did not signal an error on bogus varid case' );
end


error_condition = 0;


% Test 4:  Non numeric ncid
testid = 'Test 4';
try
	[name, datatype, ndims, dimids, natts, status] = mexnc('VARINQ', 'ncid', varid);
	error_condition = 1;
end
if error_condition == 1
	err_msg = sprintf ( '%s:  %s:  Succeeded when it should have failed\n', mfilename, testid );
	error ( err_msg );
end




% Test 5:  Non char and non double varid
testid = 'Test 5';
try
	[name, datatype, ndims, dimids, natts, status] = mexnc('VARINQ', ncid, int32(5));
	err_msg = sprintf ( '%s:  %s:  Succeeded when it should have failed\n', mfilename, testid );
	error ( err_msg );
end









fprintf ( 1, 'VARINQ succeeded\n' );



status = mexnc ( 'close', ncid );
if ( status < 0 )
	ncerr = mexnc ( 'strerror', status );
	msg = sprintf ( '%s:  %s\n', mfilename, ncerr );
	error ( msg );
end

return












