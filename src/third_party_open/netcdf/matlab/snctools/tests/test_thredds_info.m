function test_thredds_info()

fprintf('\n\t\tTesting THREDDS_INFO ...  ' );

run_motherlode_test;
fprintf('OK\n');




%--------------------------------------------------------------------------
function run_motherlode_test (  )
    
% This catalog should have 3 top-level sub-catalogs.
url = 'http://thredds.ucar.edu/thredds/catalog/subsetService/testdata/catalog.xml';
info = thredds_info(url);

expstruct = struct('name', '', 'dataset', [], ...
    'URL', 'http://thredds.ucar.edu/thredds/catalog/subsetService/testdata/catalog.xml');
expstruct.dataset = struct('name', 'NCSS Test Data', ...
    'dataset', []);
expstruct.dataset.dataset = struct('name', 'NCSS Test Data/dist2coast_1deg_ocean.nc', ...
    'service', [], ...
    'time_coverage', []);
expstruct.dataset.dataset.service = struct('NetcdfSubset', ...
    'http://thredds.ucar.edu/thredds/ncss/grid/subsetService/testdata/dist2coast_1deg_ocean.nc');
expstruct.dataset.dataset.time_coverage = struct('start', [], 'stop', []);    

if ~isequal(info, expstruct)
    error('failed')
end
return

