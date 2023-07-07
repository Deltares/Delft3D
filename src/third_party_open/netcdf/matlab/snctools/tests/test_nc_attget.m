function test_nc_attget(mode)

if nargin < 1
	% Run usual tests.
	mode = 'nc-3';
end


fprintf ('\t\tTesting NC_ATTGET...' );

switch(mode)
	case 'nc-3'
		testroot = fileparts(mfilename('fullpath'));
		ncfile = fullfile(testroot,'testdata/attget.nc');
	    run_local_nc_tests(ncfile);
	    run_negative_tests(ncfile);

	case 'netcdf4-classic'
		testroot = fileparts(mfilename('fullpath'));
		ncfile = fullfile(testroot,'testdata/attget-4.nc');
	    run_local_nc_tests(ncfile);
	    run_negative_tests(ncfile);
        
    case 'netcdf4-enhanced'
        run_nc4_enhanced_tests;

	case 'hdf'
		testroot = fileparts(mfilename('fullpath'));
		ncfile = fullfile(testroot,'testdata/attget.hdf');
	    run_local_nc_tests(ncfile);
	    run_negative_tests(ncfile);

	case 'grib'
		testroot = fileparts(mfilename('fullpath'));
		gribfile = fullfile(testroot,'testdata',...
		    'ecmf_20070122_pf_regular_ll_pt_320_pv_grid_simple.grib2');
	    run_local_grib_tests(gribfile);

	case 'http'
	    run_http_tests;
end

fprintf('OK\n');




%--------------------------------------------------------------------------
function run_nc4_enhanced_tests()

v = version('-release');
switch(v)
    case {'14','2006a'};
        fprintf('\tfiltering out all enhanced-model tests on %s.\n', v);
        return;
end

testroot = fileparts(mfilename('fullpath'));

% group char attributes
ncfile = [testroot '/testdata/tst_group_data.nc'];
test_group_attribute(ncfile);
test_group_var_attribute(ncfile);

switch(v)
    case {'2006b','2007a','2007b','2008a','2008b','2009a',...
            '2009b','2010a','2010b'};
        fprintf('\tfiltering out enhanced-model datatype tests on %s.\n', v);
        return;

		
end



% Strings
ncfile = [testroot '/testdata/moons.nc'];
test_global_string_attribute(ncfile);
test_string_variable(ncfile);
test_empty_string_attribute(ncfile);

% Enums
ncfile = [testroot '/testdata/tst_enum_data.nc'];
test_root_group_enum(ncfile);


% VLens
ncfile = [testroot '/testdata/tst_vlen_data.nc'];
test_root_group_vlen(ncfile);


% Opaques
ncfile = [testroot '/testdata/tst_opaque_data.nc'];
test_root_group_opaque(ncfile);

% Compounds
ncfile = [testroot '/testdata/tst_comp.nc'];
test_root_group_compound(ncfile);


%--------------------------------------------------------------------------
function test_group_attribute(ncfile)
act_data = nc_attget(ncfile,'/g2/g3','title');
exp_data = 'in third group';

if ~isequal(act_data,exp_data)
    error('failed');
end

%--------------------------------------------------------------------------
function test_group_var_attribute(ncfile)
act_data = nc_attget(ncfile,'/g2/g3/var','units');
exp_data = 'mm/msec';

if ~isequal(act_data,exp_data)
    error('failed');
end
%--------------------------------------------------------------------------
function test_root_group_compound(ncfile)

act_data = nc_attget(ncfile,'obs','_FillValue');
exp_data = struct(...
    'day', int8(-99), ...
    'elev', int16(-99), ...
    'count', int32(-99), ...
    'relhum', single(-99), ...
    'time', -99, ...
    'category', uint8(255), ...
    'id', uint16(2^16-1), ...
    'particularity', uint32(4294967295), ...
    'attention_span', int64(-9223372036854775806));
    

if ~isequal(act_data,exp_data)
    error('failed');
end


%--------------------------------------------------------------------------
function test_root_group_opaque(ncfile)

act_data = nc_attget(ncfile,'raw_obs','_FillValue');
exp_data = { uint8([202 254 186 190 202 254 186 190 202 254 186]') };

if ~isequal(act_data,exp_data)
    error('failed');
end


%--------------------------------------------------------------------------
function test_root_group_vlen(ncfile)

act_data = nc_attget(ncfile,'ragged_array','_FillValue');
exp_data = {single(-999)};

if ~isequal(act_data,exp_data)
    error('failed');
end

%--------------------------------------------------------------------------
function test_root_group_enum(ncfile)

act_data = nc_attget(ncfile,'primary_cloud','_FillValue');
exp_data = {'Missing'};

if ~isequal(act_data,exp_data)
    error('failed');
end

%--------------------------------------------------------------------------
function test_string_variable(ncfile)

act_data = nc_attget(ncfile,'ourano','Bianca');

exp_data = {'Puck' 'Miranda'};


if ~isequal(act_data,exp_data)
    error('failed');
end

%--------------------------------------------------------------------------
function test_global_string_attribute(ncfile)

act_data = nc_attget(ncfile,-1,'others');

exp_data = {'Francisco', 'Caliban', 'Stephano', 'Trinculo', ...
        'Sycorax', 'Margaret', 'Prospero', 'Setebos', 'Ferdinand'};
if ~isequal(act_data,exp_data)
    error('failed');
end

%--------------------------------------------------------------------------
function test_empty_string_attribute(ncfile)

act_data = nc_attget(ncfile,'/Cressida/Portia','nothing');
if ~isempty(act_data)
    error('failed');
end

%--------------------------------------------------------------------------
function run_local_nc_tests ( ncfile )

test_retrieveDoubleAttribute ( ncfile );
test_retrieveFloatAttribute ( ncfile );
test_retrieveIntAttribute ( ncfile );
test_retrieveShortAttribute ( ncfile );
test_retrieveUint8Attribute ( ncfile );
test_retrieveInt8Attribute ( ncfile );
test_retrieveTextAttribute ( ncfile );

test_retrieveGlobalAttribute_empty ( ncfile );
test_global_att_numeric_id ( ncfile );
test_global_att_nc_global ( ncfile );
test_gobal_att_using_global ( ncfile );

return;











%--------------------------------------------------------------------------
function test_retrieveIntAttribute ( ncfile )

attvalue = nc_attget ( ncfile, 'x_db', 'test_int_att' );
if ( ~strcmp(class(attvalue), 'int32' ) )
	error('class of retrieved attribute was not int32.');
end
if ( attvalue ~= int32(3) )
	error('retrieved attribute differs from what was written.');
end

return










%--------------------------------------------------------------------------
function test_retrieveShortAttribute ( ncfile )


attvalue = nc_attget ( ncfile, 'x_db', 'test_short_att' );
if ( ~strcmp(class(attvalue), 'int16' ) )
	error('class of retrieved attribute was not int16.');
end
if ( length(attvalue) ~= 2 )
	error('retrieved attribute length differs from what was written.');
end
if ( any(double(attvalue) - [5 7])  )
	error('retrieved attribute differs from what was written.');
end

return








%--------------------------------------------------------------------------
function test_retrieveUint8Attribute ( ncfile )

attvalue = nc_attget ( ncfile, 'x_db', 'test_uchar_att' );
if ( ~strcmp(class(attvalue), 'int8' ) )
	error('class of retrieved attribute was not int8.');
end
if ( uint8(attvalue) ~= uint8(100) )
	error('retrieved attribute differs from what was written.');
end

return




%--------------------------------------------------------------------------
function test_retrieveInt8Attribute ( ncfile )

attvalue = nc_attget ( ncfile, 'x_db', 'test_schar_att' );
if ( ~strcmp(class(attvalue), 'int8' ) )
	error('class of retrieved attribute was not int8.');
end
if ( attvalue ~= int8(-100) )
	error('retrieved attribute differs from what was written.');
end

return







%--------------------------------------------------------------------------
function test_retrieveTextAttribute ( ncfile )

attvalue = nc_attget ( ncfile, 'x_db', 'test_text_att' );
if ( ~ischar(attvalue ) )
	error('class of retrieved attribute was not char.');
end

if ( ~strcmp(attvalue,'abcdefghijklmnopqrstuvwxyz') )
	error('retrieved attribute differs from what was written.');
end

return







%--------------------------------------------------------------------------
function test_retrieveGlobalAttribute_empty ( ncfile )

warning ( 'off', 'SNCTOOLS:nc_attget:java:doNotUseGlobalString' );
warning ( 'off', 'SNCTOOLS:nc_attget:hdf5:doNotUseEmptyVarname' );
warning ( 'off', 'SNCTOOLS:nc_attget:hdf5:doNotUseGlobalVarname' );

attvalue = nc_attget ( ncfile, -1, 'test_double_att' );
if ( ~strcmp(class(attvalue), 'double' ) )
	error('class of retrieved attribute was not double.');
end
if ( attvalue ~= 3.14159 )
	error('retrieved attribute differs from what was written.');
end

warning ( 'on', 'SNCTOOLS:nc_attget:java:doNotUseGlobalString' );
warning ( 'off', 'SNCTOOLS:nc_attget:hdf5:doNotUseEmptyVarname' );
warning ( 'off', 'SNCTOOLS:nc_attget:hdf5:doNotUseGlobalVarname' );

return





%--------------------------------------------------------------------------
function test_global_att_numeric_id ( ncfile )

attvalue = nc_attget ( ncfile, -1, 'test_double_att' );
if ( ~strcmp(class(attvalue), 'double' ) )
	error('class of retrieved attribute was not double.');
end
if ( attvalue ~= 3.14159 )
	error('retrieved attribute differs from what was written.');
end

return





%--------------------------------------------------------------------------
function test_global_att_nc_global ( ncfile )

attvalue = nc_attget ( ncfile, nc_global, 'test_double_att' );
if ( ~strcmp(class(attvalue), 'double' ) )
	error('class of retrieved attribute was not double.');
end
if ( attvalue ~= 3.14159 )
	error('retrieved attribute differs from what was written.');
end

return 






%--------------------------------------------------------------------------
function test_gobal_att_using_global ( ncfile )

warning ( 'off', 'snctools:attget:doNotUseGlobalString' );
warning ( 'off', 'SNCTOOLS:attget:java:doNotUseGlobalString' );

attvalue = nc_attget ( ncfile, 'GLOBAL', 'test_double_att' );
if ( ~strcmp(class(attvalue), 'double' ) )
	error('class of retrieved attribute was not double.');
end
if ( attvalue ~= 3.14159 )
	error('retrieved attribute differs from what was written.');
end

warning ( 'on', 'snctools:attget:java:doNotUseGlobalString' );
warning ( 'on', 'snctools:attget:doNotUseGlobalString' );

return
















%--------------------------------------------------------------------------
function test_retrieveDoubleAttribute ( ncfile )

attvalue = nc_attget ( ncfile, 'x_db', 'test_double_att' );
if ( ~strcmp(class(attvalue), 'double' ) )
	error('class of retrieved attribute was not double.');
end
if ( attvalue ~= 3.14159 )
	error('retrieved attribute differs from what was written.');
end

return







%--------------------------------------------------------------------------
function test_retrieveFloatAttribute ( ncfile )

attvalue = nc_attget ( ncfile, 'x_db', 'test_float_att' );
if ( ~strcmp(class(attvalue), 'single' ) )
	error('class of retrieved attribute was not single.');
end
if ( abs(double(attvalue) - 3.14159) > 3e-6 )
	error('retrieved attribute differs from what was written.');
end

return




%--------------------------------------------------------------------------
function run_local_grib_tests(origfile)

grib_file = tempname;
copyfile(origfile,grib_file);
test_grib2_char(grib_file);

return;







%--------------------------------------------------------------------------
function test_grib2_char(gribfile)

act_data = nc_attget(gribfile,-1,'Conventions');
exp_data = 'CF-1.4';
if ~strcmp(act_data,exp_data)
    error('failed'); 
end
return




%--------------------------------------------------------------------------
function run_http_tests()

test_retrieveAttribute_HTTP;
test_retrieveAttribute_http_jncid;
return







%--------------------------------------------------------------------------
function test_retrieveAttribute_HTTP ()

url = 'http://rocky.umeoce.maine.edu/GoMPOM/cdfs/gomoos.20070723.cdf';

w = nc_attget ( url, 'w', 'valid_range' );
if ~strcmp(class(w),'single')
	error ( 'Class of retrieve attribute was not single' );
end
if (abs(double(w(2)) - 0.5) > eps)
	error ( 'valid max did not match' );
end
if (abs(double(w(1)) + 0.5) > eps)
	error ( 'valid max did not match' );
end
return


%--------------------------------------------------------------------------
function test_retrieveAttribute_http_jncid ()

import ucar.nc2.dods.*     
import ucar.nc2.*          

url = 'http://rocky.umeoce.maine.edu/GoMPOM/cdfs/gomoos.20070723.cdf';
jncid = NetcdfFile.open(url);
                           

w = nc_attget (jncid, 'w', 'valid_range' );
if ~strcmp(class(w),'single')
	error ( 'Class of retrieve attribute was not single' );
end
if (abs(double(w(2)) - 0.5) > eps)
	error ( 'valid max did not match' );
end
if (abs(double(w(1)) + 0.5) > eps)
	error ( 'valid max did not match' );
end
close(jncid);
return





%--------------------------------------------------------------------------
function run_negative_tests(ncfile)
v = version('-release');
switch(v)
    case { '14','2006a','2006b','2007a','2007b'}
        fprintf('No negative tests run on %s...  ',v);
    otherwise
		test_nc_attget_neg(ncfile);
end
