function test_nc_addnewrecs_neg(ncfile,mode)
% Negative testing for NC_ADDNEWRECS

test_no_record_variable(ncfile);



%---------------------------------------------------------------------------
function test_no_record_variable(ncfile)
% The buffer needs the record variable.

buffer.time2 = 1;
buffer.test_var = 1.1;

try
	nc_addnewrecs(ncfile,buffer);
catch me
    if ~strcmp(me.identifier,'snctools:addnewrecs:missingRecordVariable')
        rethrow(me);
    end
end
