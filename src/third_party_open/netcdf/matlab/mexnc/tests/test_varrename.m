function test_varrename ( ncfile )
% TEST_VARRENAME
%

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
	err_msg = sprintf ( '%s:  put_att_double failed on variable attribute, ''%s''\n', mfilename, ncerr );
	error ( err_msg );
end


%status = mexnc ( 'sync', ncid );
%if ( status < 0 )
%	ncerr = mexnc ( 'strerror', status );
%	err_msg = sprintf ( '%s:  ''%s''\n', mfilename, ncerr );
%	error ( err_msg );
%end


[status] = mexnc('VARRENAME', ncid, xdvarid, 'newname');
if ( status < 0 )
	ncerr = mexnc ( 'strerror', status );
	err_msg = sprintf ( '%s:  ''%s''\n', mfilename, ncerr );
	error ( err_msg );
end


[nvarid, status] = mexnc('INQ_VARID', ncid, 'newname');
if ( status < 0 )
	ncerr = mexnc ( 'strerror', status );
	err_msg = sprintf ( '%s:  ''%s''\n', mfilename, ncerr );
	error ( err_msg );
end


%
% Are they the same?
if ( nvarid ~= xdvarid )
	error ( 'INQ_VARID did not return the correct varid.  So VARRENAME failed.\n' );
end



%
% Bogus ncid
[status] = mexnc('VARRENAME', -4, xdvarid, 'newname');
if ( status >= 0 )
	error ( 'Bogus ncid case succeeded for VARRENAME.\n' );
end

%
% Bogus varid
[status] = mexnc('VARRENAME', ncid, -5, 'newname');
if ( status >= 0 )
	error ( 'Bogus varid case succeeded for VARRENAME.\n' );
end



%
% ENDEF
[status] = mexnc ( 'enddef', ncid );
if ( status < 0 )
	ncerr = mexnc ( 'strerror', status );
	err_msg = sprintf ( '%s:  ''%s''\n', mfilename, ncerr );
	error ( err_msg );
end




status = mexnc ( 'close', ncid );
if ( status < 0 )
	ncerr = mexnc ( 'strerror', status );
	msg = sprintf ( '%s:  %s\n', mfilename, ncerr );
	error ( msg );
end


fprintf ( 1, 'VARRENAME succeeded.\n' );

return












