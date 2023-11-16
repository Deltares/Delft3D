function test_redef_def_dim ( ncfile )
% TEST_REDEF_DEF_DIM
%

if nargin < 1
	ncfile = 'foo.nc';
end


test_definingDimension(ncfile);             % create simple dimensions
test_badNcid(ncfile);                       % invalid ncid
test_emptyNcid(ncfile);                     % ncid = []
test_nonNumericNcid(ncfile);                % ncid = 'ncid'
test_dimensionNameAlreadyExists(ncfile);    % 
test_dimensionNameIsEmptyString(ncfile);    % dimname = ''
test_dimensionNameIsEmptySet(ncfile);       % dimname = []
test_dimensionNameIsNonChar(ncfile);        % dimname = 0
test_dimensionLengthIsEmptySet(ncfile);     % dimlen = []
test_dimensionLengthIsNegative(ncfile);     % dimlen = -1
test_dimensionLengthIsNonNumeric(ncfile);   % dimlen = ''
test_redef_ncidIsEmptySet(ncfile);          % ncid = []
test_redef_ncidIsNonNumeric(ncfile);        % ncid = 'ncid'

% Regression tests
test_redef_alreadyInDefineMode(ncfile);     

fprintf ( 1, 'DEF_DIM succeeded.\n' );
fprintf ( 1, 'END_DEF succeeded.\n' );
fprintf ( 1, 'REDEF succeeded.\n' );


%--------------------------------------------------------------------------
function test_definingDimension(ncfile);

create_ncfile(ncfile);

[ncid, status] = mexnc('open',ncfile,'write');
if status, error(mexnc('strerror',status)), end

status = mexnc ( 'redef', ncid );
if status, error(mexnc('strerror',status)), end

[xdimid, status] = mexnc ( 'def_dim', ncid, 'x', 20 );
if status, error(mexnc('strerror',status)), end

[status] = mexnc ( 'enddef', ncid );
if status, error(mexnc('strerror',status)), end

[status] = mexnc ( 'redef', ncid );
if status, error(mexnc('strerror',status)), end

[ydimid, status] = mexnc ( 'def_dim', ncid, 'y', 24 );
if status, error(mexnc('strerror',status)), end

mexnc('close',ncid);







%--------------------------------------------------------------------------
function test_badNcid(ncfile);

[xdimid, status] = mexnc ( 'def_dim', -2000, 'a1', 25 );
if ( status == 0 )
	error ( 'Failed to catch bad ncid condition');
end


%--------------------------------------------------------------------------
function test_emptyNcid(ncfile);

error_condition = 0;
try
	[xdimid, status] = mexnc ( 'def_dim', [], 'a1', 25 );
	error_condition = 1;
end
if ( error_condition == 1 )
	error ( 'Failed to catch empty ncid condition' );
end



%--------------------------------------------------------------------------
function test_nonNumericNcid(ncfile);
error_condition = 0;
try
	[xdimid, status] = mexnc ( 'def_dim', 'ncid', 'a1', 25 );
	error_condition = 1;
end
if ( error_condition == 1 )
	error ( 'Failed to catch non numeric ncid condition' );
end






%--------------------------------------------------------------------------
function test_dimensionNameAlreadyExists(ncfile)

ncid = mexnc('open', ncfile, 'write' );

[xdimid, status] = mexnc ( 'def_dim', ncid, 'xx', 25 );
if ( status == 0 )
	error ( 'Failed to catch condition of already existing dimension' );
end


mexnc('close',ncid);




%--------------------------------------------------------------------------
function test_dimensionNameIsEmptyString(ncfile);

create_ncfile(ncfile);

ncid = mexnc('open',ncfile,'write');
mexnc('redef',ncid);

[xdimid, status] = mexnc ( 'def_dim', ncid, '', 25 );
if status == 0
	error('Failed to catch empty dim name condition');
end

mexnc('close',ncid);



%--------------------------------------------------------------------------
function test_dimensionNameIsEmptySet(ncfile)

create_ncfile(ncfile);
ncid = mexnc('open',ncfile,'write');
mexnc('redef',ncid);

error_condition = 0;
try
	[xdimid, status] = mexnc ( 'def_dim', ncid, [], 25 );
	error_condition = 1;
end
if ( error_condition == 1 )
	error ( 'Failed to catch empty dim name condition');
end
mexnc('close',ncid);



%--------------------------------------------------------------------------
function test_dimensionNameIsNonChar(ncfile)

create_ncfile(ncfile);
ncid = mexnc('open',ncfile,'write');
mexnc('redef',ncid);

error_condition = 0;
try
	[xdimid, status] = mexnc ( 'def_dim', ncid, 25, 25 );
	error_condition = 1;
end
if ( error_condition == 1 )
	error ( 'Failed to catch non char dimension name' );
end

mexnc('close',ncid);


%--------------------------------------------------------------------------
function test_dimNameIsEmptySet(ncfile)

create_ncfile(ncfile);
ncid = mexnc('open',ncfile,'write');
mexnc('redef',ncid);


try
	[xdimid, status] = mexnc ( 'def_dim', ncid, 'b1', [] );
	error_condition = 1;
end
if ( error_condition == 1 )
	error ( 'Failed to catch empty set dimension name' );
end

mexnc('close',ncid);


%--------------------------------------------------------------------------
function test_dimensionNameIsNegative(ncfile)


create_ncfile(ncfile);
ncid = mexnc('open',ncfile,'write');
mexnc('redef',ncid);


[xdimid, status] = mexnc ( 'def_dim', ncid, 'b1', -25 );
if ( status == 0 )
	error ( 'Failed to catch empty set dimension name' );
end


mexnc('close',ncid);


%--------------------------------------------------------------------------
function test_dimensionLengthIsNonNumeric(ncfile)


create_ncfile(ncfile);
ncid = mexnc('open',ncfile,'write');
mexnc('redef',ncid);

try
	[xdimid, status] = mexnc ( 'def_dim', ncid, 'b3', 'wardrobe malfunction' );
	error ( 'FAiled to catch bad dimension length argument' );
end
mexnc('close',ncid);


%--------------------------------------------------------------------------
function test_dimensionLengthIsEmptySet(ncfile)


create_ncfile(ncfile);
ncid = mexnc('open',ncfile,'write');
mexnc('redef',ncid);

try
	[xdimid, status] = mexnc ( 'def_dim', ncid, 'b3', [] );
	error ( 'FAiled to catch bad dimension length argument' );
end
mexnc('close',ncid);


%--------------------------------------------------------------------------
function test_dimensionLengthIsNegative(ncfile)


create_ncfile(ncfile);
ncid = mexnc('open',ncfile,'write');
mexnc('redef',ncid);

try
	[xdimid, status] = mexnc ( 'def_dim', ncid, 'b3', -25 );
	error ( 'FAiled to catch bad dimension length argument' );
end
mexnc('close',ncid);


%--------------------------------------------------------------------------
function test_redef_ncidIsEmptySet(ncfile)

create_ncfile(ncfile);
ncid = mexnc('open',ncfile,'write');

error_condition = 0;
try
	[status] = mexnc ( 'redef', [] );
	error_condition = 1;
end
if ( error_condition == 1 )
	error('failed to catch empty set ncid for redef');
end

mexnc('close',ncid);



%--------------------------------------------------------------------------
function test_redef_ncidIsNonNumeric(ncfile)

create_ncfile(ncfile);
ncid = mexnc('open',ncfile,'write');

error_condition = 0;
try
	[status] = mexnc ( 'redef', 'ncid' );
	error_condition = 1;
end
if ( error_condition == 1 )
	error('failed to catch non numeric ncid for redef');
end


mexnc('close',ncid);



return





%-------------------------------------------------------------------------------
function test_redef_alreadyInDefineMode(ncfile)

create_ncfile(ncfile);
ncid = mexnc('open',ncfile,'write');

mexnc('redef', ncid);
status = mexnc('redef',ncid);

if ( status == 0 )
	error('failed to catch double redef condition');
end

mexnc('close',ncid);







%-------------------------------------------------------------------------------
function create_ncfile ( ncfile )

[ncid, status] = mexnc ( 'create', ncfile, nc_clobber_mode );
if ( status < 0 )
	ncerr = mexnc ( 'strerror', status );
	err_msg = sprintf ( '%s:  ''%s''\n', mfilename, ncerr );
	error ( err_msg );
end

[xdimid, status] = mexnc ( 'def_dim', ncid, 'xx', 20 );
if status, error(mexnc('strerror',status)), end

status = mexnc ( 'close', ncid );
if ( status ~= 0 )
	ncerr = mexnc ( 'strerror', status );
	err_msg = sprintf ( '%s:  ''%s''\n', mfilename, ncerr );
	error ( err_msg );
end

