function info = thredds_dump(url)
% THREDDS_DUMP  Dump metadata from a thredds catalog.
%   THREDDS_DUMP(XML_URL) recursively dumps metadata from a thredds
%   catalog URL.  The URL must be for the catalog XML file.
%
% Example:
%   url = 'http://thredds.ucar.edu/thredds/catalog/subsetService/testdata/catalog.xml';
%   thredds_dump(url);

info = thredds_info(url);

dump_element(info,0);



%--------------------------------------------------------------------------
function dump_element(info,level)

if isfield(info,'service')
    dump_dataset(info,level);
else
    dump_catalog(info,level);
end

%--------------------------------------------------------------------------
function dump_catalog(info,level)

fprintf('%sCatalog:  %s\n', local_indent(level), info.name);
if isfield(info,'URL')
    fprintf('%sURL:  %s\n', local_indent(level+1), info.URL);
end

for j = 1:numel(info.dataset)
    dump_element(info.dataset(j),level+1);
end

%--------------------------------------------------------------------------
function dump_dataset(info,level)


fprintf('%sDataset:  %s\n', local_indent(level), info.name);

services = fieldnames(info.service);
n = numel(services);

fprintf('%sService:  \n', local_indent(level+1));
for j = 1:n
    fprintf('%s%s:  %s\n', local_indent(level+2), services{j}, ...
        info.service.(services{j}));
end

if ~isempty(info.time_coverage.start)
    fprintf('%sTime Coverage:  %s -- %s\n', local_indent(level+1), ...
        datestr(info.time_coverage.start), datestr(info.time_coverage.stop));
end
    
    
%--------------------------------------------------------------------------
function indent = local_indent(level)
indent = repmat('  ',1,level);


