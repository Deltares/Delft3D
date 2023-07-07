function test_nc_adddim(mode)

fprintf('\t\tTesting NC_ADDDIM ...  ' );

if nargin < 1
    mode = nc_clobber_mode;
end
ncfile = 'foo.nc';
test_add_regular_dimension(ncfile,mode);                 
test_add_unlimited (ncfile,mode);                         
test_dimension_already_exists(ncfile,mode);  

run_negative_tests(mode);

fprintf('OK\n');

return







%--------------------------------------------------------------------------
function test_add_regular_dimension(ncfile,mode)
% Positive test:  add a fixed-length dimension.

nc_create_empty(ncfile,mode);
nc_adddim ( ncfile, 't', 5 );

%
% Now check that the new dimension are there.
d = nc_getdiminfo ( ncfile, 't' );
if ( ~strcmp(d.Name,'t') )
	error ( 'nc_adddim failed on fixed dimension add name');
end
if ( d.Length ~= 5 )
	error ( 'nc_adddim failed on fixed dimension add length');
end
if ( d.Unlimited ~= 0  )
	error ( 'nc_adddim incorrectly classified the dimension');
end

return


















%--------------------------------------------------------------------------
function test_add_unlimited(ncfile,mode)
% Positive test:  add an unlimited dimension.

nc_create_empty(ncfile,mode);
nc_adddim ( ncfile, 't', 0 );

%
% Now check that the new dimension are there.
d = nc_getdiminfo ( ncfile, 't' );
if ( ~strcmp(d.Name,'t') )
	error ( 'nc_adddim failed on fixed dimension add name');
end
if ( d.Length ~= 0 )
	error ( 'nc_adddim failed on fixed dimension add length');
end
if ( d.Unlimited ~= 1  )
	error ( 'nc_adddim incorrectly classified the dimension');
end

return


















%--------------------------------------------------------------------------
function test_dimension_already_exists(ncfile,mode)
% Negative test:  try to add a dimension that is already there.  Should 
% error out.

nc_create_empty(ncfile,mode);
nc_adddim ( ncfile, 't', 0 );
try
	nc_adddim ( ncfile, 't', 0 );
catch %#ok<CTCH>
    return
end
error('succeeded when it should have failed.');






%--------------------------------------------------------------------------
function run_negative_tests(mode)

if strcmp(mode,'hdf4')
    fprintf('no negative tests on hdf4.  ');
    return
end

v = version('-release');
switch(v)
    case {'14','2006a','2006b','2007a','2007b'}
        fprintf('\tNo negative tests on %s.  ' , v);
        return
    otherwise
        test_nc_adddim_neg(mode);
end
        
