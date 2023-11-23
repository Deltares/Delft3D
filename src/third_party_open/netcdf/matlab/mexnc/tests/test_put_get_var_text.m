function test_put_get_var_text ( ncfile )
% TEST_GET_PUT_VAR_TEXT
%
% Tests expected to succeed.
%     [PUT,GET]_VAR_TEXT: Write a 6x4 array of monotonically increasing values, read them back.
%     [PUT,GET]_VAR1_TEXT:  write and read 255 to disk
%     [PUT,GET]_VARA_TEXT:  write and read a 3x3 chunk
%     [PUT,GET]_VARS_TEXT:  write and read the 6x4 array, subset by factor of 2, stride of 2
%     [PUT,GET]_VARM_TEXT:  normally we want to transpose the data before writing with
%                             mexnc.  However, this is makes for a good test of the VARM_XXX
%                             routine, as that routine can actually handle the transpose for us.
%                             So this test writes the same array of data as [put,get]_var_xxx,
%                             but we don't bother to transpose the array.  Yes, these routines
%                             really are that hard to wrap one's head around.
%

%
% ok, first create this baby.
[ncid, status] = mexnc ( 'create', ncfile, nc_clobber_mode );
if ( status ~= 0 )
	ncerr_msg = mexnc ( 'strerror', status );
	msg = sprintf ( '%s:  ''create'' failed on %s, error message '' %s ''\n', mfilename, ncfile, ncerr_msg );
	error ( msg );
end



%
% Create the fixed dimension.  
len_x = 4;
[xdimid, status] = mexnc ( 'def_dim', ncid, 'x', len_x );
if ( status ~= 0 )
	ncerr_msg = mexnc ( 'strerror', status );
	msg = sprintf ( '%s:  ''def_dim'' failed on dim x, file %s, error message '' %s ''\n', mfilename, ncfile, ncerr_msg );
	error ( msg );
end


len_y = 6;
[ydimid, status] = mexnc ( 'def_dim', ncid, 'y', len_y );
if ( status ~= 0 )
	ncerr_msg = mexnc ( 'strerror', status );
	msg = sprintf ( '%s:  ''def_dim'' failed on dim y, file %s, error message '' %s ''\n', mfilename, ncfile, ncerr_msg );
	error ( msg );
end


[varid, status] = mexnc ( 'def_var', ncid, 'z_text', 'NC_CHAR', 2, [ydimid xdimid] );
if ( status ~= 0 )
	ncerr_msg = mexnc ( 'strerror', status );
	msg = sprintf ( '%s:  ''def_var'' failed on var x_short, file %s, error message '' %s ''\n', mfilename, ncfile, ncerr_msg );
	error ( msg );
end


[status] = mexnc ( 'enddef', ncid );
if ( status ~= 0 )
	ncerr_msg = mexnc ( 'strerror', status );
	msg = sprintf ( '%s:  ''enddef'' failed, file %s, error message '' %s ''\n', mfilename, ncfile, ncerr_msg );
	error ( msg );
end

[varid, status] = mexnc('INQ_VARID', ncid, 'z_text');
if ( status ~= 0 )
	msg = sprintf ( '%s:  INQ_VARID failed\n', mfilename );
	error ( msg );
end


%
% {PUT,GET}_VAR_TEXT
[varname,xtype,ndims,dimids,natts,status] = mexnc('INQ_VAR',ncid,varid);
for j = 1:ndims
	dimlen(j) = mexnc('INQ_DIMLEN',ncid,dimids(j));
end

input_data = [1:1:prod(dimlen)] + 96;
input_data = reshape ( input_data, dimlen(1), dimlen(2) );
input_data = char ( input_data );
input_data = input_data';
status = mexnc ( 'PUT_VAR_TEXT', ncid, varid, input_data );
if ( status ~= 0 )
	ncerr_msg = mexnc ( 'strerror', status );
	msg = sprintf ( '%s:  PUT_VAR_TEXT failed, (%s)\n', mfilename, ncerr_msg );
	error ( msg );
end


[output_data, status] = mexnc ( 'GET_VAR_TEXT', ncid, varid );
if ( status ~= 0 )
	msg = sprintf ( '%s:  GET_VAR_TEXT failed, msg ''%s''\n', mfilename, mexnc ( 'strerror', status ) );
	error ( msg );
end


d = max(abs(double(output_data)-double(input_data)))';
if (any(d))
	msg = sprintf ( '%s:  values written by PUT_VAR_TEXT do not match what was retrieved by GET_VAR_TEXT\n', mfilename  );
	error ( msg );
end

fprintf ( 1, 'PUT_VAR_TEXT succeeded\n' );
fprintf ( 1, 'GET_VAR_TEXT succeeded\n' );


%%
%% Test PUT/GET_VAR1
fprintf ( 2, 'Skipping test for PUT/GET_VAR1_TEXT\n' );
%input_data = 97;
%input_data = char ( input_data );
%status = mexnc ( 'PUT_VAR1_TEXT', ncid, varid, [2 2], input_data );
%if ( status < 0 )
%	msg = sprintf ( '%s:  PUT_VAR1_TEXT failed\n', mfilename );
%	error ( msg );
%end
%
%
%status = mexnc ( 'SYNC', ncid );
%if ( status < 0 )
%	msg = sprintf ( '%s:  SYNC failed, msg ''%s''\n', mfilename, mexnc ( 'strerror', status ) );
%	error ( msg );
%end
%
%
%[output_data, status] = mexnc ( 'GET_VAR1_TEXT', ncid, varid, [2 2] );
%if ( status < 0 )
%	msg = sprintf ( '%s:  GET_VAR1_TEXT failed, msg ''%s''\n', mfilename, mexnc ( 'strerror', status ) );
%	error ( msg );
%end
%
%d = max(abs(double(output_data)-double(input_data)));
%if (d > 0)
%	msg = sprintf ( '%s:  values written by PUT_VAR1_TEXT do not match what was retrieved by GET_VAR_TEXT\n', mfilename  );
%	error ( msg );
%end
%
%fprintf ( 1, 'PUT_VAR1_TEXT succeeded\n' );
%fprintf ( 1, 'GET_VAR1_TEXT succeeded\n' );




%
% Test PUT/GET_VARA
input_data = [1:1:prod(dimlen)] + 96;
input_data = reshape ( input_data, dimlen(1), dimlen(2) );
input_data = input_data(2:4,1:3);
input_data = char ( input_data );

status = mexnc ( 'PUT_VARA_TEXT', ncid, varid, [1 0], [3 3], input_data' );
if ( status < 0 )
	msg = sprintf ( '%s:  PUT_VARA_TEXT failed\n', mfilename );
	error ( msg );
end


status = mexnc ( 'SYNC', ncid );
if ( status < 0 )
	msg = sprintf ( '%s:  SYNC failed, msg ''%s''\n', mfilename, mexnc ( 'strerror', status ) );
	error ( msg );
end


[output_data, status] = mexnc ( 'GET_VARA_TEXT', ncid, varid, [1 0], [3 3] );
if ( status < 0 )
	msg = sprintf ( '%s:  GET_VARA_TEXT failed, msg ''%s''\n', mfilename, mexnc ( 'strerror', status ) );
	error ( msg );
end

output_data = output_data';

d = max(abs(double(output_data)-double(input_data)))';
if (any(d))
	msg = sprintf ( '%s:  values written by PUT_VARA_TEXT do not match what was retrieved by GET_VARA_TEXT\n', mfilename  );
	error ( msg );
end

fprintf ( 1, 'PUT_VARA_TEXT succeeded\n' );
fprintf ( 1, 'GET_VARA_TEXT succeeded\n' );




%
% Test PUT/GET_VARS
input_data = [1:1:prod(dimlen)] + 96;
input_data = reshape ( input_data, dimlen(1), dimlen(2) );
input_data = input_data(1:2:end,1:2:end);
input_data = char ( input_data );
[r,c] = size(input_data);

status = mexnc ( 'PUT_VARS_TEXT', ncid, varid, [0 0], [r c], [2 2], input_data' );
if ( status < 0 )
	msg = sprintf ( '%s:  PUT_VARS_TEXT failed\n', mfilename );
	error ( msg );
end


status = mexnc ( 'SYNC', ncid );
if ( status < 0 )
	msg = sprintf ( '%s:  SYNC failed, msg ''%s''\n', mfilename, mexnc ( 'strerror', status ) );
	error ( msg );
end


[output_data, status] = mexnc ( 'GET_VARS_TEXT', ncid, varid, [0 0], [r c], [2 2] );
if ( status < 0 )
	msg = sprintf ( '%s:  GET_VARS_TEXT failed, msg ''%s''\n', mfilename, mexnc ( 'strerror', status ) );
	error ( msg );
end

output_data = output_data';

d = max(abs(double(output_data)-double(input_data)))';
if (any(d))
	msg = sprintf ( '%s:  values written by PUT_VARS_TEXT do not match what was retrieved by GET_VARS_TEXT\n', mfilename  );
	error ( msg );
end

fprintf ( 1, 'PUT_VARS_TEXT succeeded\n' );
fprintf ( 1, 'GET_VARS_TEXT succeeded\n' );


switch(version('-release'))
case { '13', '14', '2006a', '2006b', '2007a', '2007b', '2008a' }
	input_data = [1:1:len_y*len_x] + 96;
	input_data = reshape ( input_data, len_y, len_x );
	input_data = char ( input_data );
	
	start_coord = [0 0];
	count_coord = [6 4];
	stride_coord = [1 1];
	
	%
	% This is the tricky part.  When the matrix arrives in the mex file, it
	% arrives essentially transposed, as a 4x6 array, monotonically increasing
	% across the columns instead of rows.  But in the netcdf file, we want the
	% dataset to increase monotonically by rows.  This is an inter-element
	% distance of 1 for the rows.  So the first column should consist of 
	% [1 2 3 4 5 6], and the 2nd row starts at 7.  That inter-element distance
	% is then 6.  In the imap vector, you start with the slowest varying FILE
	% dimension and proceed out to the most rapidly varying.  This means imap
	% should be 6 and then 1.
	%
	%     NetCDF dimension         inter-element distance
	%     most rapidly varying     6
	%     most slowly varying      1
	%
	imap_coord = [1 6];
	
	%
	% Write without transposing first.
	status = mexnc ( 'put_varm_text', ncid, varid, start_coord, count_coord, stride_coord, imap_coord, input_data );
	if ( status ~= 0 )
		ncerr_msg = mexnc ( 'strerror', status );
		msg = sprintf ( '%s:  ''put_varm_text'' failed on var rh, file %s, error message '' %s ''\n', mfilename, ncfile, ncerr_msg );
		error ( msg );
	end
	
	status = mexnc ( 'sync', ncid );
	if ( status ~= 0 )
		ncerr_msg = mexnc ( 'strerror', status );
		msg = sprintf ( '%s:  ''sync'' failed on file %s, error message '' %s ''\n', mfilename, ncfile, ncerr_msg );
		error ( msg );
	end
	
	
	%
	% Now read it back.
	%
	% This is the tricky part.  When the matrix arrives from the mexfile,
	% one has to remember that it arrives transposed.  So what we want is
	% to choose imap so that it yields the transpose of what we really want.
	% Then we get the transpose of a transpose, which would be the data arranged
	% in rows and columns in matlab exactly as it is in the netcdf file.
	%
	% The NetCDF file has the data stored as
	% 
	%    |  1  7 13 19 |
	%    |  2  8 14 20 |
	%    |  3  9 15 21 |
	%    |  4 10 16 22 |
	%    |  5 11 17 23 |
	%    |  6 12 18 24 |
	%
	% The transpose of what we want would be
	%
	%    |  1  2  3  4  5  6 |
	%    |  7  8  9 10 11 12 |
	%    | 13 14 15 16 17 18 |
	%    | 19 20 21 22 23 24 |
	%
	% Start with the end of imap, which is the most rapidly varying NetCDF
	% dimension.  That's the column.  In the internal array (that's the 2nd
	% array above), how far between elements?  In the NetCDF file file, 1 and 7
	% are next to each other across the row, but in the internal array (which
	% is still inside the mex file and therefore in C, so row major order)
	% they have a distance of 6.  The first element of imap corresponds to
	% the slowest varying NetCDF dimension, the rows.  In the internal array,
	% how far between row elements?  In the NetCDF file, 1 and 2 are next to
	% each other across rows, but in the internal array, they have a distance
	% of 1 across columns.  So imap is the same as above, [1 6].
	[output_data,status] = mexnc ( 'get_varm_text', ncid, varid, start_coord, count_coord, stride_coord, imap_coord );
	if ( status ~= 0 )
		ncerr_msg = mexnc ( 'strerror', status );
		msg = sprintf ( '%s:  ''get_varm_text'' failed on var rh, file %s, error message '' %s ''\n', mfilename, ncfile, ncerr_msg );
		error ( msg );
	end
	
	d = max(abs(double(output_data)-double(input_data)))';
	if (any(d))
		msg = sprintf ( '%s:  values written by PUT_VARM_TEXT do not match what was retrieved by GET_VARM_TEXT\n', mfilename  );
		error ( msg );
	end
	
	
	
	fprintf ( 1, 'PUT_VARM_TEXT succeeded\n' );
	fprintf ( 1, 'GET_VARM_TEXT succeeded\n' );

	
otherwise	
	warning ( 'GET_VARM_text and PUT_VARM_text not supported under TMW\n' );
	return
end



%
% CLOSE
status = mexnc ( 'close', ncid );
if ( status ~= 0 )
	error ( 'CLOSE failed' );
end


return






