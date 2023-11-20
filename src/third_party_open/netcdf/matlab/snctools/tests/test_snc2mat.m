function test_snc2mat()
fprintf ('\t\tTesting SNC2MAT...  ' );
run_negative_tests;

test_generic_file;
fprintf('OK\n');
return





%--------------------------------------------------------------------------
function run_negative_tests()
v = version('-release');
switch(v)
	case{'14','2006a','2006b','2007a'}
	    fprintf('Some negative tests filtered out on version %s...  ', v);
    otherwise
		test_file_does_not_exist;
end


%--------------------------------------------------------------------------
function test_file_does_not_exist ( )

% netcdf file does not exist.
try
	snc2mat ( 'bad.nc', 'bad.mat' );
catch  %#ok<CTCH>
    %  'MATLAB:netcdf:open:noSuchFile'
    return
end

%--------------------------------------------------------------------------
function test_generic_file()

v = version('-release');
switch(v)
    case { '14', '2006a', '2006b', '2007a', '2007b', '2008a'}
        try
            mexnc('inq_libvers');
        catch %#ok<CTCH>
            fprintf('\tNo testing on java read-only configuration.\n');
            return
        end
end

testroot = fileparts(mfilename('fullpath'));
ncfile = fullfile(testroot,'testdata/tst_pres_temp_4D_netcdf.nc');

matfile_name = [ ncfile '.mat' ];
snc2mat ( ncfile, matfile_name );


%
% now check it
d = load ( matfile_name );
act_data = d.pressure.data;
exp_data = nc_varget(ncfile,'pressure');



ddiff = act_data-exp_data;
d = ddiff(:);
d = max(abs(d));
if (any(d))
	error ( 'failed' );
end
return











