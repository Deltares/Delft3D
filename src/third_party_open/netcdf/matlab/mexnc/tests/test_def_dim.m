function test_def_dim ( ncfile )
% TEST_DEF_DIM
%

if nargin < 1
	ncfile = 'foo.nc';
end

test_define ( ncfile );
test_dimLengthIsUnlimitedCharCase ( ncfile );

test_neg_badNcid ( ncfile );
test_neg_emptyNcid ( ncfile );
test_neg_ncidIsNonNumeric ( ncfile );
test_neg_dimAlreadyExists ( ncfile );
test_neg_dimNameIsEmptyString ( ncfile );
test_neg_dimNameIsEmptySet ( ncfile );
test_neg_dimNameIsNonChar ( ncfile );
test_neg_dimLengthIsEmptySet ( ncfile );
test_neg_dimLengthIsNegative ( ncfile );
test_neg_dimLengthIsNonNumeric ( ncfile );

fprintf('DEF_DIM succeeded.\n');




%--------------------------------------------------------------------------
function create_ncfile ( ncfile )

[ncid, status] = mexnc ( 'create', ncfile, nc_clobber_mode );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

status = mexnc ( 'close', ncid );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

return







%--------------------------------------------------------------------------
function test_define ( ncfile )
% Define a dimension.
%

create_ncfile ( ncfile );

[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if ( status ~= 0 ), error(mexnc('strerror',status)), end

status = mexnc ( 'redef', ncid );
if ( status ~= 0 ), error(mexnc('strerror',status)), end

[xdimid, status] = mexnc ( 'def_dim', ncid, 'x', 20 );
if ( status ~= 0 ), error(mexnc('strerror',status)), end

status = mexnc ( 'close', ncid );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

%
% Reopen the file and check for it.
[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if ( status ~= 0 ), error(mexnc('strerror',status)), end

[xdimid, status] = mexnc ( 'INQ_DIMID', ncid, 'x' );
if ( status ~= 0 ), error(mexnc('strerror',status)), end

status = mexnc ( 'close', ncid );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end


return









%--------------------------------------------------------------------------
function test_dimLengthIsUnlimitedCharCase ( ncfile )
%
% Use 'NC_UNLIMITED'

v = version('-release');

create_ncfile ( ncfile );

[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if ( status ~= 0 ), error(mexnc('strerror',status)), end

status = mexnc ( 'redef', ncid );
if ( status ~= 0 ), error(mexnc('strerror',status)), end

% mexnc pre 2008b def_dim never implemented support for 'NC_UNLIMITED'
% It is implemented for 2008b+, though.
switch(v)
	case { '2008a', '2007b', '2007a', '2006b', '2006a', '14' }
		[xdimid, status] = mexnc ( 'dimdef', ncid, 'x', 'NC_UNLIMITED' );
	otherwise
		[xdimid, status] = mexnc ( 'def_dim', ncid, 'x', 'NC_UNLIMITED' );
end

if ( status ~= 0 ), error(mexnc('strerror',status)), end

status = mexnc ( 'close', ncid );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

% Reopen the file and check for it.
[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if ( status ~= 0 ), error(mexnc('strerror',status)), end

[xdimid, status] = mexnc ( 'INQ_DIMID', ncid, 'x' );
if ( status ~= 0 ), error(mexnc('strerror',status)), end

[dimlen, status] = mexnc ( 'INQ_DIMLEN', ncid, xdimid );
if ( status ~= 0 ), error(mexnc('strerror',status)), end
if ( dimlen ~= 0 ), error('failed to define unlimited dimension'), end

status = mexnc ( 'close', ncid );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end


return










%--------------------------------------------------------------------------
function test_neg_badNcid ( ncfile )
% Call DEF_DIM with a bad ncid.
%

create_ncfile ( ncfile );

[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if ( status ~= 0 ), error(mexnc('strerror',status)), end

status = mexnc ( 'redef', ncid );
if ( status ~= 0 ), error(mexnc('strerror',status)), end

[xdimid, status] = mexnc ( 'def_dim', -2000, 'a1', 25 );
if ( status == 0 ), error('succeeded when it should have failed'), end

status = mexnc ( 'close', ncid );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

return





%--------------------------------------------------------------------------
function test_neg_emptyNcid ( ncfile )
% Call DEF_DIM with a bad ncid of [].
%

create_ncfile ( ncfile );

[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if ( status ~= 0 ), error(mexnc('strerror',status)), end

status = mexnc ( 'redef', ncid );
if ( status ~= 0 ), error(mexnc('strerror',status)), end

try
	[xdimid, status] = mexnc ( 'def_dim', [], 'a1', 25 );
	error('succeeded when it should have failed');
end


status = mexnc ( 'close', ncid );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

return






%--------------------------------------------------------------------------
function test_neg_ncidIsNonNumeric ( ncfile )
% Call DEF_DIM with non-numeric ncid
%

create_ncfile ( ncfile );

[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if ( status ~= 0 ), error(mexnc('strerror',status)), end

status = mexnc ( 'redef', ncid );
if ( status ~= 0 ), error(mexnc('strerror',status)), end

try
	[xdimid, status] = mexnc ( 'def_dim', 'ncid', 'a1', 25 );
	error('succeeded when it should have failed');
end

status = mexnc ( 'close', ncid );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

return








%--------------------------------------------------------------------------
function test_neg_dimAlreadyExists ( ncfile )
% Try to define a dimension that already exists.
%

create_ncfile ( ncfile );

[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if ( status ~= 0 ), error(mexnc('strerror',status)), end

status = mexnc ( 'redef', ncid );
if ( status ~= 0 ), error(mexnc('strerror',status)), end

[xdimid, status] = mexnc ( 'def_dim', ncid, 'x', 20 );
if ( status ~= 0 ), error(mexnc('strerror',status)), end

status = mexnc ( 'close', ncid );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

%
% Reopen the file and try to define another x dimension
[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if ( status ~= 0 ), error(mexnc('strerror',status)), end

status = mexnc ( 'redef', ncid );
if ( status ~= 0 ), error(mexnc('strerror',status)), end

[xdimid2, status] = mexnc ( 'def_dim', ncid, 'x', 20 );
if ( status == 0 ), error('succeeded when it should have failed'), end

status = mexnc ( 'close', ncid );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end


return







%--------------------------------------------------------------------------
function test_neg_dimNameIsEmptyString ( ncfile )
% Negative test:  dimname is ''
%

create_ncfile ( ncfile );

[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if ( status ~= 0 ), error(mexnc('strerror',status)), end

status = mexnc ( 'redef', ncid );
if ( status ~= 0 ), error(mexnc('strerror',status)), end

try
	[xdimid, status] = mexnc ( 'def_dim', ncid, '', 20 );
	error('succeeded when it should have failed');
end

status = mexnc ( 'close', ncid );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

return







%--------------------------------------------------------------------------
function test_neg_dimNameIsEmptySet ( ncfile )
% Negative test:  dimname is []
%

create_ncfile ( ncfile );

[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if ( status ~= 0 ), error(mexnc('strerror',status)), end

status = mexnc ( 'redef', ncid );
if ( status ~= 0 ), error(mexnc('strerror',status)), end

try
	[xdimid, status] = mexnc ( 'def_dim', ncid, [], 20 );
	error('succeeded when it should have failed');
end

status = mexnc ( 'close', ncid );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

return








%--------------------------------------------------------------------------
function test_neg_dimNameIsNonChar ( ncfile )
% Negative test:  dimname is non-character
%

create_ncfile ( ncfile );

[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if ( status ~= 0 ), error(mexnc('strerror',status)), end

status = mexnc ( 'redef', ncid );
if ( status ~= 0 ), error(mexnc('strerror',status)), end

try
	[xdimid, status] = mexnc ( 'def_dim', ncid, 20, 20 );
	error('succeeded when it should have failed');
end

status = mexnc ( 'close', ncid );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

return







%--------------------------------------------------------------------------
function test_neg_dimLengthIsEmptySet ( ncfile )
% Negative test:  dimlength is []
%

create_ncfile ( ncfile );

[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if ( status ~= 0 ), error(mexnc('strerror',status)), end

status = mexnc ( 'redef', ncid );
if ( status ~= 0 ), error(mexnc('strerror',status)), end

try
	[xdimid, status] = mexnc ( 'def_dim', ncid, 'x', [] );
	error('succeeded when it should have failed');
end

status = mexnc ( 'close', ncid );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

return








%--------------------------------------------------------------------------
function test_neg_dimLengthIsNegative ( ncfile )
% Negative test:  dimlength is negative
%

create_ncfile ( ncfile );

[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if ( status ~= 0 ), error(mexnc('strerror',status)), end

status = mexnc ( 'redef', ncid );
if ( status ~= 0 ), error(mexnc('strerror',status)), end

try
	[xdimid, status] = mexnc ( 'def_dim', ncid, 'x', -1 );
	error('succeeded when it should have failed');
end

status = mexnc ( 'close', ncid );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

return








%--------------------------------------------------------------------------
function test_neg_dimLengthIsNonNumeric ( ncfile )
% Negative test:  dimlength is non-numeric
%

create_ncfile ( ncfile );

[ncid, status] = mexnc ( 'open', ncfile, nc_write_mode );
if ( status ~= 0 ), error(mexnc('strerror',status)), end

status = mexnc ( 'redef', ncid );
if ( status ~= 0 ), error(mexnc('strerror',status)), end

try
	[xdimid, status] = mexnc ( 'def_dim', ncid, 'x', '10' );
	error('succeeded when it should have failed');
end

status = mexnc ( 'close', ncid );
if ( status ~= 0 ), error ( mexnc('strerror',status) ), end

return










return















