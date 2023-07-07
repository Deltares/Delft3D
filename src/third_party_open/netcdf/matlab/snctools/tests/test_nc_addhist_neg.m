function test_nc_addhist_neg()
% Negative testing for NC_ADDHIST.


ncfile = 'foo.nc';
                             
test_not_netcdf_file;           
test_2nd_input_not_char(ncfile);    

return



%--------------------------------------------------------------------------
function test_not_netcdf_file()
% Negative test.  If it's not a netcdf file, it should trigger an error.
ncfile = which('example.cdf');
try
	nc_addhist (ncfile, 'test' );
catch me
    if ~strcmp(me.identifier,'snctools:writeBackend:unknown')
        rethrow(me);
    end
end






%--------------------------------------------------------------------------
function test_2nd_input_not_char ( ncfile )
% Negative test.  The history blurb value should be character.

nc_create_empty(ncfile,nc_clobber_mode);
try
	nc_addhist(ncfile,5);
catch me
    if ~strcmp(me.identifier,'snctools:addHist:badDatatype');
        rethrow(me);
    end
end



