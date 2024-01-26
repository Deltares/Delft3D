function test_create ( ncfile )
% Tests run are open with
% Test 1:   nc_clobber_mode
% Test 2:   nc_noclobber_mode
% Test 3:   clobber and share and 64 bit offset
% Test 4:  share mode.  Should also clobber it.
% Test 5:  share | 64bit_offset
% Test 6:  64 bit offset.  Should also clobber it.
% Test 7:  noclobber mode.  Should not succeed.
% Test 8:  only one input, should not succeed
% Test 9:  Filename is empty
% Test 10:  mode argument not supplied
% Test 11:  mode argument is 'clobber'.  Deprecated, please don't use this.
% Test 12:  mode argument is 'noclobber'.  Deprecated, please don't use this.
% Test 13:  mode argument is nc_netcdf4_classic
% Test 14:  mode argument is 4096, which is for enhanced netcdf-4
%

if nargin == 0
	ncfile = 'foo.nc';
end


test_clobber_mode(ncfile);                   % #1
test_noclobber_and_share_mode(ncfile);       % #2
test_clobber_and_share_and_64bit(ncfile);    % #3
test_share_mode(ncfile);                     % #4        
test_share_and_64bit_mode(ncfile);           % #5
test_64bit_mode(ncfile);                     % #6
test_noclobber_mode(ncfile);                 % #7
test_only_one_input(ncfile);                 % #8
test_empty_filename(ncfile);                 % #9
test_no_mode(ncfile);                       % #10
%test_char_clobber(ncfile);                  % #11
%test_char_noclobber(ncfile);                % #12
test_netcdf4_classic(ncfile);                % #13
test_netcdf4_enhanced(ncfile);               % #13

fprintf ( 'CREATE succeeded.\n' );

return


%--------------------------------------------------------------------------
function test_clobber_mode(ncfile)

[ncid, status] = mexnc ( 'create', ncfile, nc_clobber_mode );
if status, error(mexnc('strerror',status)), end;

status = mexnc ( 'close', ncid );
if status, error(mexnc('strerror',status)), end;

return





%--------------------------------------------------------------------------
function test_noclobber_and_share_mode(ncfile)
% Test 2:   nc_noclobber_mode | nc_share_mode
mode = bitor ( nc_clobber_mode, nc_share_mode );
[ncid, status] = mexnc ( 'create', ncfile, mode );
if status, error(mexnc('strerror',status)), end;
status = mexnc ( 'close', ncid );
if status, error(mexnc('strerror',status)), end;
return






%--------------------------------------------------------------------------
function test_clobber_and_share_and_64bit(ncfile)
% Test 3:   clobber and share and 64 bit offset
mode = bitor ( nc_clobber_mode, nc_share_mode );
mode = bitor ( mode, nc_64bit_offset_mode );
[ncid, status] = mexnc ( 'create', ncfile, mode );
if status, error(mexnc('strerror',status)), end;
status = mexnc ( 'close', ncid );
if status, error(mexnc('strerror',status)), end;

return




%--------------------------------------------------------------------------
function test_share_mode(ncfile)
% Test 4:  share mode.  Should also clobber it.
[ncid, status] = mexnc ( 'create', ncfile, nc_share_mode );
if status, error(mexnc('strerror',status)), end;
status = mexnc ( 'close', ncid );
if status, error(mexnc('strerror',status)), end;
return


%--------------------------------------------------------------------------
function test_share_and_64bit_mode(ncfile)
% Test 5:  share | 64bit_offset
mode = bitor ( nc_share_mode, nc_64bit_offset_mode );
[ncid, status] = mexnc ( 'create', ncfile, mode );
if status, error(mexnc('strerror',status)), end;
status = mexnc ( 'close', ncid );
if status, error(mexnc('strerror',status)), end;
return


%--------------------------------------------------------------------------
function test_64bit_mode(ncfile)
% 64 bit offset.  Should also clobber it.
[ncid, status] = mexnc ( 'create', ncfile, nc_64bit_offset_mode );
if status, error(mexnc('strerror',status)), end;
status = mexnc ( 'close', ncid );
if status, error(mexnc('strerror',status)), end;
return


%--------------------------------------------------------------------------
function test_noclobber_mode(ncfile)
%
% Test 7:  noclobber mode.  Should not succeed, because the file already
% exists.
[ncid, status] = mexnc ( 'create', ncfile, nc_noclobber_mode ); %#ok<ASGLU>
if ( status == 0 )
	error ( '''create'' succeeded on nc_noclobber_mode, should have failed' );
end

return



%--------------------------------------------------------------------------
function test_only_one_input(ncfile)
%
% Test 8:  only one input, should not succeed.  Throws an exception, 
%          because there are way too few arguments.
try
	mexnc ( 'create' );
	error('succeeded when it should have failed');
catch	 %#ok<CTCH>
	
end

return




%--------------------------------------------------------------------------
function test_empty_filename(ncfile)
%
% Test 9:  Filename is empty
try %#ok<TRYNC>
	mexnc ( 'create', '', nc_clobber_mode );
	error ( 'succeeded when it should have failed' );
end

return







%--------------------------------------------------------------------------
function test_no_mode(ncfile)

if exist('foo2.nc','file')
	delete('foo2.nc');
end

% Test 10:  Only two arguments.  Mode should default to no clobber
[ncid, status] = mexnc ( 'create', 'foo2.nc' );
if status, error(mexnc('strerror',status)), end;
status = mexnc ( 'close', ncid );
if status, error ( mexnc ( 'strerror', status ) ), end

return





%--------------------------------------------------------------------------
function test_char_clobber ( ncfile )

[ncid, status] = mexnc ( 'create', ncfile, 'clobber' );
if ( status ~= 0 ), error ( mexnc ( 'strerror', status ) ), end

status = mexnc ( 'close', ncid );
if ( status ~= 0 ), error ( mexnc ( 'strerror', status ) ), end

return






%--------------------------------------------------------------------------
function test_char_noclobber ( ncfile )

delete ( ncfile );

[ncid, status] = mexnc ( 'create', ncfile, 'noclobber' );
if ( status ~= 0 ), error ( mexnc ( 'strerror', status ) ), end

status = mexnc ( 'close', ncid );
if ( status ~= 0 ), error ( mexnc ( 'strerror', status ) ), end

return




%--------------------------------------------------------------------------
function test_netcdf4_classic(ncfile)

delete(ncfile);

[ncid, status] = mexnc ( 'create', ncfile, nc_netcdf4_classic );
if status, error(mexnc('strerror',status)), end;
status = mexnc ( 'close', ncid );
if status, error(mexnc('strerror',status)), end;


return




%--------------------------------------------------------------------------
function test_netcdf4_enhanced(ncfile)

delete(ncfile);
v = version('-release');
switch(v)
    case { '14', '2006a', '2006b', '2007a', '2007b', '2008a' }
%       [ncid, status] = mexnc ( 'create', ncfile, 'netcdf4' );
% 		if status == 0
% 			error('Should not have succeeded in creating a file in netcdf4 mode.');
% 		end
%       mexnc('close',ncid);
        throw_error = 0;
        try
            [ncid, status] = mexnc('create',ncfile,'netcdf4');
            throw_error = 1;          
        end
        if throw_error
            error('failed');
        end
    case {  '2008b', '2009a', '2009b', '2010a'}
        [ncid, status] = mexnc ( 'create', ncfile, 'netcdf4' );
		if status == 0
			error('Should not have succeeded in creating a file in netcdf4 mode.');
		end
        mexnc('close',ncid);
    otherwise
        % 10b and higher, this should work.
        [ncid, status] = mexnc ( 'create', ncfile, 'netcdf4' );
        if status < 0, error('failed'), end
        mexnc('close',ncid);
end




return




