function record_variable = snc_find_record_variable(ncfile)
%SNC_FIND_RECORD_VARIABLE  return name of record variable
not_found = true;
info1 = nc_info(ncfile);
for j = 1:numel(info1.Dataset)
    if info1.Dataset(j).Unlimited ...
            && (numel(info1.Dataset(j).Dimension) == 1) ...
            && strcmp(info1.Dataset(j).Name,info1.Dataset(j).Dimension{1})
        idx = j;
        not_found = false;
    end
end
if not_found
    error('snctools:snc_find_record_variable:noRecordVariableFound', ...
        'Could not find a record variable in %s.', ncfile);
end
record_variable = info1.Dataset(idx).Name;
