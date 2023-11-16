function test_nc_getdiminfo(mode)

if nargin < 1
    mode = 'netcdf-3';
end

fprintf('\t\tTesting NC_GETDIMINFO ...  ' );

testroot = fileparts(mfilename('fullpath'));
switch(mode)
    case 'netcdf-3'  
        empty_ncfile = fullfile(testroot,'testdata/empty.nc');
        full_ncfile = fullfile(testroot,'testdata/full.nc');
        test_local(empty_ncfile,full_ncfile);
    case 'netcdf4-classic'      
        empty_ncfile = fullfile(testroot,'testdata/empty-4.nc');
        full_ncfile = fullfile(testroot,'testdata/full-4.nc');
        test_local(empty_ncfile,full_ncfile);
end

fprintf('OK\n');











%--------------------------------------------------------------------------
function test_local (empty_ncfile, full_ncfile )

test_unlimited ( full_ncfile );
test_limited ( full_ncfile );
test_fields ( full_ncfile );

test_neg_noArgs                                  ;
test_neg_onlyOneArg              ( empty_ncfile );
test_neg_tooManyInputs           ( empty_ncfile );
test_neg_1stArgNotNetcdfFile;
test_neg_2ndArgNotVarName   ;

return






%--------------------------------------------------------------------------
function test_neg_noArgs ()
try
    nb = nc_getdiminfo; %#ok<NASGU>
    error('succeeded when it should have failed.');
catch %#ok<CTCH>
    return
end




%--------------------------------------------------------------------------
function test_neg_onlyOneArg ( ncfile )
try
    nb = nc_getdiminfo ( ncfile ); %#ok<NASGU>
    error('succeeded when it should have failed.');
catch %#ok<CTCH>
    return
end




%--------------------------------------------------------------------------
function test_neg_tooManyInputs ( ncfile )
try
    diminfo = nc_getdiminfo ( ncfile, 'x', 'y' ); %#ok<NASGU>
    error('succeeded when it should have failed.');
catch %#ok<CTCH>
    return
end








%--------------------------------------------------------------------------
function test_neg_1stArgNotNetcdfFile ( )

try
    diminfo = nc_getdiminfo ( 'does_not_exist.nc', 'x' ); %#ok<NASGU>
    error('succeeded when it should have failed.');
catch %#ok<CTCH>
    return
end






%--------------------------------------------------------------------------
function test_neg_2ndArgNotVarName ( ncfile )

try
    nc_getdiminfo ( ncfile, 'var_does_not_exist' );
    error('succeeded when it should have failed.');
catch %#ok<CTCH>
    return
end














%--------------------------------------------------------------------------
function test_unlimited ( ncfile )
diminfo = nc_getdiminfo ( ncfile, 't' );
if ~strcmp ( diminfo.Name, 't' )
    error('diminfo.Name was incorrect.');
end
if ( diminfo.Length ~= 0 )
    error('diminfo.Length was incorrect.');
end
if ( diminfo.Unlimited ~= 1 )
    error('diminfo.Unlimited was incorrect.');
end
return




%--------------------------------------------------------------------------
function test_limited ( ncfile )

diminfo = nc_getdiminfo ( ncfile, 's' );
if ~strcmp ( diminfo.Name, 's' )
    error('diminfo.Name was incorrect.');
end
if ( diminfo.Length ~= 1 )
    error('diminfo.Length was incorrect.');
end
if ( diminfo.Unlimited ~= 0 )
    error('diminfo.Unlimited was incorrect.');
end
return


%--------------------------------------------------------------------------
function test_fields ( ncfile )
% Use the 3 argument case.

diminfo = nc_getdiminfo(ncfile,'s','Length');
if ~isequal(diminfo,1)
    error('failed');
end

diminfo = nc_getdiminfo(ncfile,'s','Unlimited');
if ~isequal(diminfo,0)
    error('failed');
end
