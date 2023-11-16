function test_nc_addhist(mode)

fprintf('\t\tTesting NC_ADDHIST... ' );

ncfile = 'foo.nc';
if nargin < 1
    mode = nc_clobber_mode;
end

nc_create_empty(ncfile,mode);

test_add_global_history(ncfile,mode);          
test_add_global_history_twice(ncfile,mode); 

run_negative_tests;

fprintf('OK\n');




%--------------------------------------------------------------------------
function test_add_global_history(ncfile,mode)
% Try to add a generic string.

nc_create_empty(ncfile,mode);
histblurb = 'blah';
nc_addhist ( ncfile, histblurb );

hista = nc_attget ( ncfile, nc_global, 'history' );
s = strfind(hista, histblurb );
if isempty(s)
	error('history attribute did not contain first attribution.');
end
return




%--------------------------------------------------------------------------
function test_add_global_history_twice ( ncfile,mode )
% Try to add a generic string.  Twice.

nc_create_empty ( ncfile,mode);
histblurb = 'blah a';
nc_addhist ( ncfile, histblurb );
histblurb2 = 'blah b';
nc_addhist ( ncfile, histblurb2 );
histatt = nc_attget ( ncfile, nc_global, 'history' );
s = strfind(histatt, histblurb2 );
if isempty(s)
	error('history attribute did not contain second attribution');
end
return



%--------------------------------------------------------------------------
function run_negative_tests()

v = version('-release');
switch(v)
    case {'14','2006a','2006b','2007a','2007b'}
        fprintf('\tNo negative tests on %s.  ' , v);
        return
    otherwise
        test_nc_addhist_neg();
end

        





