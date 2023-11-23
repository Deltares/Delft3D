function test_lfs ( ncfile )
% TEST_LFS:  test large file support


fprintf ( 1, 'Do you wish to test large file support?\n' );
fprintf ( 1, '\n' );
fprintf ( 1, 'If you choose to proceed, we will create a NetCDF file \n' );
fprintf ( 1, 'with a short int variable with two dimensions, one\n' );
fprintf ( 1, 'of which is a record dimension, the other of length 5,000,000.\n' );
fprintf ( 1, 'You will then be asked for the number of records to write.\n' );
fprintf ( 1, 'Each record will be about 10MB, so if you specify 500,\n' );
fprintf ( 1, 'you should end up with a 5GB file.  You need to specify at least\n' );
fprintf ( 1, '215 to adequately test large file support, and if your disk I/O\n' );
fprintf ( 1, 'is on the pokey side, you might as well go brew up a batch of \n' );
fprintf ( 1, 'coffee, check your stock quotes, and read up on Slashdot while \n' );
fprintf ( 1, 'you are waiting.  Make sure you have enough disk space for this! \n' );
fprintf ( 1, '\n' );
answer = input ( 'Do you wish to proceed? [y/n]\n', 's' );
if strcmp ( lower(answer), 'y' )
	;
else
	fprintf ( 1, 'Skipping LFS test.\n' );
	return;
end

fprintf ( 1, '\n' );
fprintf ( 1, '\n' );
answer = input ( 'How many records to you wish to write?  1 record == 10MB \n' );

num_recs =  answer;

fprintf ( 1, '\n' );

%
% ok, first create this baby.
[ncid, status] = mexnc ( 'create', ncfile, bitor ( nc_clobber_mode, nc_64bit_offset_mode ) );
if ( status ~= 0 )
	ncerr_msg = mexnc ( 'strerror', status );
	msg = sprintf ( '%s:  ''create'' failed on %s, error message '' %s ''\n', mfilename, ncfile, ncerr_msg );
	error ( msg );
end


%
% Create the unlimited dimension
[tdimid, status] = mexnc ( 'def_dim', ncid, 't', 0 );
if ( status ~= 0 )
	ncerr_msg = mexnc ( 'strerror', status );
	msg = sprintf ( '%s:  ''def_dim'' failed on dim t, file %s, error message '' %s ''\n', mfilename, ncfile, ncerr_msg );
	error ( msg );
end


%
% Create the fixed dimension.  
len_x = 5000000;
[xdimid, status] = mexnc ( 'def_dim', ncid, 'x', len_x );
if ( status ~= 0 )
	ncerr_msg = mexnc ( 'strerror', status );
	msg = sprintf ( '%s:  ''def_dim'' failed on dim x, file %s, error message '' %s ''\n', mfilename, ncfile, ncerr_msg );
	error ( msg );
end


%
% Each record will have 1MB of data.
[varid, status] = mexnc ( 'def_var', ncid, 'bigboy', 'NC_SHORT', 2, [tdimid xdimid] );
if ( status ~= 0 )
	ncerr_msg = mexnc ( 'strerror', status );
	msg = sprintf ( '%s:  ''def_var'' failed on var bigboy, file %s, error message '' %s ''\n', mfilename, ncfile, ncerr_msg );
	error ( msg );
end

status = mexnc ( 'enddef', ncid );
if ( status ~= 0 )
	ncerr_msg = mexnc ( 'strerror', status );
	msg = sprintf ( '%s:  ''enddef'' failed on file %s, error message '' %s ''\n', mfilename,  ncerr_msg );
	error ( msg );
end



for j = 0:num_recs-1
	data = j * ones(len_x,1);

	start_coord = [j 0];
	count_coord = [1 len_x];

	status = mexnc ( 'put_vara_double', ncid, varid, start_coord, count_coord, data );
	if ( status ~= 0 )
		ncerr_msg = mexnc ( 'strerror', status );
		msg = sprintf ( '%s:  ''put_vara_double'' failed on var bigboy, record %d, file %s, error message '' %s ''\n', mfilename, j, ncfile, ncerr_msg );
		error ( msg );
	end

	fprintf ( 1, '  Wrote record %d\n', j );

end


status = mexnc ( 'close', ncid );
if ( status ~= 0 )
	ncerr_msg = mexnc ( 'strerror', status );
	msg = sprintf ( '%s:  ''close'' failed on %s, error message '' %s ''\n', mfilename, ncfile, ncerr_msg );
	error ( msg );
end


fprintf ( 1, 'Large File Support test succeeded\n' );

return
