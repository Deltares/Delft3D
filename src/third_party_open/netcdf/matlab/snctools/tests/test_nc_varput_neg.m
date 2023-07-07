function test_nc_varput_neg (  )

run_nc3_tests()



%--------------------------------------------------------------------------
function run_nc3_tests()

testroot = fileparts(mfilename('fullpath'));

ncfile = fullfile(testroot,'testdata/varput.nc');
run_generic_tests(ncfile);
run_singleton_tests(ncfile);

% This doesn't work for nc4 or hdf4
test_neg_2d_to_singleton ( ncfile );


return




%--------------------------------------------------------------------------
function run_singleton_tests(input_ncfile)

ncfile = 'foo.nc';
copyfile(input_ncfile,ncfile);

test_singleton_bad_start ( ncfile );
test_singleton_bad_count ( ncfile );
test_singleton_with_stride_which_is_bad ( ncfile );

return

%--------------------------------------------------------------------------
function run_generic_tests(input_ncfile)

ncfile = 'foo.nc';
copyfile(input_ncfile,ncfile);

test_write_1D_size_mismatch(ncfile);
test_write_1D_bad_stride(ncfile);
test_write_1D_rank_mismatch(ncfile);

test_write_2D_strided_bad_start ( ncfile );
test_write_2D_chunk_bad_count ( ncfile );
test_write_2D_bad_stride ( ncfile );


return






















%--------------------------------------------------------------------------
function test_neg_2d_to_singleton ( ncfile )
% Don't test on 2007a or earlier

global ignore_eids
try
    nc_varput ( ncfile, 'test_singleton', [2 1] );
catch me 
    if ignore_eids
        return
    end
    switch(me.identifier)
        case { 'MATLAB:netcdf:putVar:dataSizeMismatch', ...
                'SNCTOOLS:NC_VARPUT:MEXNC:varput:dataSizeMismatch', ...
                'MATLAB:netcdf:open:notANetcdfFile'}
            return
        otherwise
            rethrow(me);
    end
end
error('nc_varput succeeded when it should not have.');








%--------------------------------------------------------------------------
function test_write_1D_rank_mismatch ( ncfile )

global ignore_eids

input_data = [0 0; 1 1];
try
    nc_varput ( ncfile, 'test_1D', input_data, [0 0], [2 2] );
catch me
    if ignore_eids
        return
    end
    switch(me.identifier)
        case { 'MATLAB:netcdf:putVara:dataSizeMismatch', ...
                'snctools:varput:badIndexing', ...
                'SNCTOOLS:NC_VARPUT:MEXNC:putVara:dataSizeMismatch', ...
                'SNCTOOLS:varput:hdf4:writedataFailed'}
            return
        otherwise
            rethrow(me);
    end
end        

error('nc_varput succeeded when it should not have.');









%--------------------------------------------------------------------------
function test_write_1D_size_mismatch ( ncfile )


global ignore_eids

input_data = 3.14159;
try
    nc_varput ( ncfile, 'test_1D', input_data, 4, 2 );
catch me
    if ignore_eids
        return
    end
    switch(me.identifier)
        case { 'MATLAB:netcdf:putVara:dataSizeMismatch', ...
                'SNCTOOLS:NC_VARPUT:MEXNC:putVara:dataSizeMismatch', ...
                'SNCTOOLS:varput:hdf4:writedataFailed'}
            return
        otherwise
            rethrow(me);
    end
end        


error('failed')







%--------------------------------------------------------------------------
function test_write_1D_bad_stride ( ncfile )


global ignore_eids;

input_data = [3.14159; 2];
try
    nc_varput ( ncfile, 'test_1D', input_data, 0, 2, 8 );
catch me
    if ignore_eids
        return
    end
    switch(me.identifier)
        case { 'MATLAB:netcdf:putVars:indexExceedsDimensionBound', ...
                'MATLAB:netcdf:putVars:einvalcoords:indexExceedsDimensionBound', ...
                'SNCTOOLS:NC_VARPUT:writeOperationFailed', ...
                'SNCTOOLS:varput:hdf4:writedataFailed'}
            return
        otherwise
            rethrow(me);
    end
end
    
error('nc_varput succeeded when it should have failed.');






%--------------------------------------------------------------------------
function test_singleton_bad_start ( ncfile )


input_data = 3.14159;
try
    nc_varput ( ncfile, 'test_singleton', input_data, 4, 1 );
catch me

    switch(me.identifier)
        case { 'SNCTOOLS:nc_varput:badIndexing' }
            return
        otherwise
            rethrow(me);
    end

end

error('failed')




%--------------------------------------------------------------------------
function test_singleton_bad_count ( ncfile )


input_data = 3.14159;
try
    nc_varput ( ncfile, 'test_singleton', input_data, 0, 2 );
catch me

    switch(me.identifier)
        case { 'SNCTOOLS:nc_varput:badIndexing' }
            return
        otherwise
            rethrow(me);
    end    

end


error('failed')







%--------------------------------------------------------------------------
function test_singleton_with_stride_which_is_bad ( ncfile )

input_data = 3.14159;
try
    nc_varput ( ncfile, 'test_singleton', input_data, 0, 1, 1 );
catch me
 
    switch(me.identifier)
        case { 'SNCTOOLS:NC_VARPUT:badIndexing' }
            return
        otherwise
            rethrow(me);
    end   

end

error('failed')
















%--------------------------------------------------------------------------
function test_write_2D_strided_bad_start ( ncfile )
% write using put_vars with a bad offset


sz = nc_varsize(ncfile,'test_2D');
start = [2 1];
count = sz/2;
stride = [2 2];

input_data = (1:prod(count)) + 3.14159;
input_data = reshape(input_data,count);

try
    nc_varput ( ncfile, 'test_2D', input_data, start, count, stride);
catch me

    switch(me.identifier)
        case { 'SNCTOOLS:NC_VARPUT:writeOperationFailed', ...
                 'MATLAB:netcdf:putVars:einvalcoords:indexExceedsDimensionBound', ...
                'MATLAB:netcdf:putVars:indexExceedsDimensionBound', ...
                'SNCTOOLS:varput:hdf4:writedataFailed'}
            return
        otherwise
            rethrow(me);
    end  
end
error('failed');







%--------------------------------------------------------------------------
function test_write_2D_chunk_bad_count ( ncfile )
% vara with bad count

sz = nc_varsize(ncfile,'test_2D');
start = [0 0];
count = sz+1;

input_data = (1:prod(count)) + 3.14159;
input_data = reshape(input_data,count);
try
    nc_varput ( ncfile, 'test_2D', input_data, start, count );
catch me

    switch(me.identifier)
        case { 'SNCTOOLS:NC_VARPUT:writeOperationFailed', ...
                'MATLAB:netcdf:putVara:startPlusCountExceedsDimensionBound', ...
                'MATLAB:netcdf:putVara:eedge:startPlusCountExceedsDimensionBound', ...
                'SNCTOOLS:varput:hdf4:writedataFailed'}
            return
        otherwise
            rethrow(me);
    end 
end
error('failed');







%--------------------------------------------------------------------------
function test_write_2D_bad_stride ( ncfile )

sz = nc_varsize(ncfile,'test_2D');
start = [0 0];
count = sz/2;
stride = [3 3];

input_data = (1:prod(count)) + 3.14159;
input_data = reshape(input_data,count);
try
    nc_varput ( ncfile, 'test_2D', input_data, start, count, stride);
catch me

    switch(me.identifier)
        case { 'SNCTOOLS:NC_VARPUT:writeOperationFailed', ...
                'MATLAB:netcdf:putVars:indexExceedsDimensionBound', ...
                'MATLAB:netcdf:putVars:einvalcoords:indexExceedsDimensionBound', ...
                'SNCTOOLS:varput:hdf4:writedataFailed'}
            return
        otherwise
            rethrow(me);
    end  
end
error('failed');




