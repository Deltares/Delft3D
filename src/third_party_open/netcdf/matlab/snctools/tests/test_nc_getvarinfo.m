function test_nc_getvarinfo(mode)

if nargin < 1
	mode = 'nc-3';
end

fprintf('\t\tTesting NC_GETVARINFO...  ' );

switch(mode)
	case 'nc-3'
        testroot = fileparts(mfilename('fullpath'));
		ncfile = fullfile(testroot,'testdata/getlast.nc');
		run_nc_tests(ncfile);
		run_negative_tests;

	case 'nc-4'
        testroot = fileparts(mfilename('fullpath'));
		ncfile = fullfile(testroot,'testdata/getlast-4.nc');
		run_nc_tests(ncfile);

	case 'http'
		run_http_tests;

end
fprintf('OK\n');



%--------------------------------------------------------------------------
function run_nc_tests(ncfile)

test_limited_variable(ncfile);
test_unlimited_variable(ncfile);
test_unlimited_variable_with_one_attribute(ncfile);

test_fields(ncfile);

return





%--------------------------------------------------------------------------
function run_negative_tests()
test_noInputs;
test_tooFewInputs;
test_tooManyInput;
test_fileIsNotNetcdfFile;
test_varIsNotNetcdfVariable;
test_fileIsNumeric_varIsChar;
test_fileIsChar_varIsNumeric;



%--------------------------------------------------------------------------
function test_noInputs ()


try
	nc_getvarinfo;
catch %#ok<CTCH>
    return
end
error('failed');








%--------------------------------------------------------------------------
function test_tooFewInputs()

testroot = fileparts(mfilename('fullpath'));
ncfile = fullfile(testroot,'testdata/full.nc');

try
	nc_getvarinfo ( ncfile );
catch %#ok<CTCH>
    return
end
error('failed');







%--------------------------------------------------------------------------
function test_tooManyInput()

testroot = fileparts(mfilename('fullpath'));
ncfile = fullfile(testroot,'testdata/full.nc');
try
	nc_getvarinfo ( ncfile, 't1' );
catch %#ok<CTCH>
    return
end
error('failed');









%--------------------------------------------------------------------------
function test_fileIsNotNetcdfFile ()


try
	nc_getvarinfo ( 'iamnotarealfilenoreally', 't1' );
catch %#ok<CTCH>
    return
end
error('failed');















%--------------------------------------------------------------------------
function test_varIsNotNetcdfVariable()

testroot = fileparts(mfilename('fullpath'));
ncfile = fullfile(testroot,'testdata/full.nc');
try
	nc_getvarinfo ( ncfile, 't5' );
catch %#ok<CTCH>
    return
end
error('failed');










%--------------------------------------------------------------------------
function test_fileIsNumeric_varIsChar ()

warning('off','snctools:nc_getvarinfo:deprecatedSyntax');
try
	nc_getvarinfo ( 0, 't1' );
catch %#ok<CTCH>
    warning('on','snctools:nc_getvarinfo:deprecatedSyntax');
    return
end
error('failed');




%--------------------------------------------------------------------------
function test_fileIsChar_varIsNumeric()


testroot = fileparts(mfilename('fullpath'));
ncfile = fullfile(testroot,'testdata/full.nc');
try
	nc_getvarinfo ( ncfile, 0 );
catch %#ok<CTCH>
    return
end
error('failed');





%--------------------------------------------------------------------------
function test_fields(ncfile)

v = nc_getvarinfo(ncfile,'x','Name');
if ~strcmp(v, 'x' )
    error('failed');
end

v = nc_getvarinfo(ncfile,'x','Nctype');
if (v~=6 )
    error('failed');
end

v = nc_getvarinfo(ncfile,'x','Unlimited');
if (v~=0 )
    error('failed');
end

v = nc_getvarinfo(ncfile,'x','Dimension');
if (length(v)~=1 )
    error('failed');
end
if ( ~strcmp(v{1},'x') )
    error('failed');
end

v = nc_getvarinfo(ncfile,'x','Size');
if (v~=2 )
    error('failed');
end
if (numel(v)~=1 )
    error('failed');
end

v = nc_getvarinfo(ncfile,'x','Attribute');
if (~isempty(v) )
    error('failed');
end

return




%--------------------------------------------------------------------------
function test_limited_variable(ncfile)

v = nc_getvarinfo ( ncfile, 'x' );

if ~strcmp(v.Name, 'x' )
    error('failed');
end
if (v.Nctype~=6 )
    error('failed');
end
if (v.Unlimited~=0 )
    error('failed');
end
if (length(v.Dimension)~=1 )
    error('failed');
end
if ( ~strcmp(v.Dimension{1},'x') )
    error('failed');
end
if (v.Size~=2 )
    error('failed');
end
if (numel(v.Size)~=1 )
    error('failed');
end
if (~isempty(v.Attribute) )
    error('failed');
end

return





%--------------------------------------------------------------------------
function test_unlimited_variable(ncfile)

v = nc_getvarinfo ( ncfile, 't1' );

if ~strcmp(v.Name, 't1' )
    error('failed');
end
if (v.Nctype~=6 )
    error('failed');
end
if (v.Unlimited~=1 )
    error('failed');
end
if (length(v.Dimension)~=1 )
    error('failed');
end
if (v.Size~=10 )
    error('failed');
end
if (numel(v.Size)~=1 )
    error('failed');
end
if (~isempty(v.Attribute) )
    error('failed');
end

return







%--------------------------------------------------------------------------
function test_unlimited_variable_with_one_attribute(ncfile)

v = nc_getvarinfo ( ncfile, 't4' );

if ~strcmp(v.Name, 't4' )
	error('Name was not correct.');

end
if (v.Nctype~=6 )
	error('Nctype was not correct.');
end
if (v.Unlimited~=1 )
    error('Unlimited was not correct.');
end
if (length(v.Dimension)~=2 )
	error('Dimension was not correct.');
end
if (numel(v.Size)~=2 )
	error( 'Rank was not correct.');
end
if (length(v.Attribute)~=1 )
	error('Attribute was not correct.');
end

return




%--------------------------------------------------------------------------
function run_http_tests()
% These tests are regular URLs, not OPeNDAP URLs.

test_fileIsHttpUrl_varIsChar;
test_fileIsJavaNcid_varIsChar;
return






%--------------------------------------------------------------------------
function test_fileIsJavaNcid_varIsChar ( )

import ucar.nc2.dods.*     
import ucar.nc2.*          

url = 'http://rocky.umeoce.maine.edu/GoMPOM/cdfs/gomoos.20070723.cdf';
jncid = NetcdfFile.open(url);

try
	nc_getvarinfo ( jncid, 'w' );
catch %#ok<CTCH>
    error('failed');
end





%--------------------------------------------------------------------------
function test_fileIsHttpUrl_varIsChar ( )

import ucar.nc2.dods.*     
import ucar.nc2.*          

url = 'http://rocky.umeoce.maine.edu/GoMPOM/cdfs/gomoos.20070723.cdf';

try
	nc_getvarinfo ( url, 'w' );
catch %#ok<CTCH>
    error('failed');
end


