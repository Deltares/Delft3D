function varargout = geojson(cmd, varargin)
%GEOJSON Read/write GeoJSON files.

%----- LGPL --------------------------------------------------------------------
%                                                                               
%   Copyright (C) 2011-2023 Stichting Deltares.                                     
%                                                                               
%   This library is free software; you can redistribute it and/or                
%   modify it under the terms of the GNU Lesser General Public                   
%   License as published by the Free Software Foundation version 2.1.                         
%                                                                               
%   This library is distributed in the hope that it will be useful,              
%   but WITHOUT ANY WARRANTY; without even the implied warranty of               
%   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU            
%   Lesser General Public License for more details.                              
%                                                                               
%   You should have received a copy of the GNU Lesser General Public             
%   License along with this library; if not, see <http://www.gnu.org/licenses/>. 
%                                                                               
%   contact: delft3d.support@deltares.nl                                         
%   Stichting Deltares                                                           
%   P.O. Box 177                                                                 
%   2600 MH Delft, The Netherlands                                               
%                                                                               
%   All indications and logos of, and references to, "Delft3D" and "Deltares"    
%   are registered trademarks of Stichting Deltares, and remain the property of  
%   Stichting Deltares. All rights reserved.                                     
%                                                                               
%-------------------------------------------------------------------------------
%   http://www.deltaressystems.com
%   $HeadURL$
%   $Id$

lcmd = lower(cmd);
switch lcmd
    case 'open'
        [varargout{1:max(nargout,1)}] = openfile(varargin{:});
    case 'write'
        if nargout == 0
            writefile(varargin{:});
        else
            [varargout{1:nargout}] = writefile(varargin{:});
        end
end

function S = openfile(filename)
fid = fopen(filename, 'r');
if fid < 0
    error('Unable to open file %s',filename)
end
fseek(fid,0,1);
nbytes = ftell(fid);
fseek(fid,0,-1);
txt = fread(fid,nbytes,'uint8=>char')';
fclose(fid);
%
json = parse_json(txt);
json.FileName = filename;
json.FileType = 'JSON';
S = parse_geojson(json);

function S = parse_geojson(json)
S = json;
nfeat = length(S.features);
%
props = cell(nfeat,1);
for f = 1:nfeat
    props{f} = fieldnames(S.features{1}.properties);
end
props = unique(cat(1,props{:}));
%
igeotype = 1;
igeocrds = 2;
propdata = cell(2+length(props), nfeat);
for f = 1:nfeat
    F = S.features{f};
    %
    for p = 1:length(props)
        if isfield(F.properties,props{p})
            propdata{2+p,f} = F.properties.(props{p});
        end
    end
    %
    G = F.geometry;
    propdata{igeotype,f} = G.type;
    C = G.coordinates;
    switch G.type
        case 'Point'
            C = cat(2,C{:});
        %case 'LineString'
        %case 'Polygon'
        %case 'MultiPoint'
        case 'MultiLineString'
            nParts = size(C,2);
            for p = 1:nParts
                P = C{1,p};
                P = cat(1,P{:});
                C{1,p} = cell2mat(P);
                if p < nParts
                    C{2,p} = repmat(NaN,[1 size(P,2)]);
                end
            end
            C = cat(1,C{:});
        %case 'MultiPolygons'
        otherwise
            error('Geometry type %s is not yet supported.',G.type)
    end
    propdata{igeocrds,f} = C;
end
S.features = [];
S.features.properties = cat(1,{'geometry_type';'geometry_coordinates'},props);
S.features.data = propdata;
S.FileType = 'GeoJSON';

function [data,i] = parse_json(txt,i)
C = char([9 10 13 32]);
len = length(txt);
%
ROOT = 0;
OBJECT = 1;
LABEL = 2;
LABEL_FINISHED = 3;
EXPECTING_VALUE = 4;
STRING = 5;
VALUE_FINISHED = 6;
LIST = 7;
LIST_ITEM_FINISHED = 8;
%
if nargin < 2
    i = 1;
end
%
reclist = 'none';
type = ROOT;
data = [];
while i < len
    if any(C == txt(i))
        % skip always
    elseif txt(i) == '"'
        if type == OBJECT
            type = LABEL;
            i0 = i;
        elseif type == LABEL
            type = LABEL_FINISHED;
            label = txt(i0+1:i-1);
        elseif type == EXPECTING_VALUE
            type = STRING;
            i0 = i;
        elseif STRING % ... and not escaped \" (note escaped backslash \\)
            nslashes = 0;
            while i - nslashes > 1
                if txt(i-nslashes-1) == '\'
                    nslashes = nslashes+1;
                else
                    break
                end
            end
            % escaped if odd number of slashes
            if nslashes ~= round(nslashes/2)*2
                % yes, escaped quote --> not the end
                i = i+1;
                continue
            end
            type = VALUE_FINISHED;
            string = txt(i0+1:i-1);
            % TODO: modify all escaped characters \b,\f, \n, \r, \t, \", \\
            data.(label) = string;
        end 
    elseif type == LABEL || type == STRING
        % skip all characters
    elseif txt(i) == '{'
        if type == ROOT
            type = OBJECT;
        elseif type == EXPECTING_VALUE
            [object,i] = parse_json(txt,i);
            data.(label) = object;
            type = VALUE_FINISHED;
        elseif type == LIST
            [object,i] = parse_json(txt,i);
            list{end+1} = object;
            type = LIST_ITEM_FINISHED;
        end
    elseif txt(i) == '}'
        if type == VALUE_FINISHED
            return
        end
    elseif txt(i) == '['
        if type == EXPECTING_VALUE
            list = {};
            type = LIST;
        elseif type == LIST
            reclist = {list reclist};
            list = {};
        end
    elseif txt(i) == ']'
        if type == LIST_ITEM_FINISHED
            if iscell(reclist)
                reclist{1}{end+1} = list;
                list = reclist{1};
                reclist = reclist{2};
            else
                data.(label) = list;
                type = VALUE_FINISHED;
            end
        end
    elseif txt(i) == ':'
        if type == LABEL_FINISHED
            type = EXPECTING_VALUE;
        end
    elseif txt(i) == ','
        if type == VALUE_FINISHED
            type = OBJECT;
        elseif type == LIST_ITEM_FINISHED
            type = LIST;
        end
    elseif txt(i) == 'f'
        if strcmp(txt(i:i+4),'false')
            if type == EXPECTING_VALUE
                data.(label) = false;
                type = VALUE_FINISHED;
            elseif type == LIST
                list{end+1} = false;
                type = LIST_ITEM_FINISHED;
            end
            i = i+4;
        end
    elseif txt(i) == 'n'
        if strcmp(txt(i:i+3),'null')
            if type == EXPECTING_VALUE
                data.(label) = [];
                type = VALUE_FINISHED;
            elseif type == LIST
                list{end+1} = [];
                type = LIST_ITEM_FINISHED;
            end
            i = i+3;
        end
    elseif txt(i) == 't'
        if strcmp(txt(i:i+3),'true')
            if type == EXPECTING_VALUE
                data.(label) = true;
                type = VALUE_FINISHED;
            elseif type == LIST
                list{end+1} = true;
                type = LIST_ITEM_FINISHED;
            end
            i = i+3;
        end
    else
        [val,~,errmsg,j] = sscanf(txt(i:min(len,i+30)),'%f',1);
        if isempty(errmsg)
            if type == EXPECTING_VALUE
                data.(label) = val;
                type = VALUE_FINISHED;
            elseif type == LIST
                list{end+1} = val;
                type = LIST_ITEM_FINISHED;
            end
            i = i+j-1;
            continue
        end
        error('Unexpected symbol %s encountered at character %i',txt(i),i)
    end
    i = i+1;
end

function DUMMY = writefile(filename, geomtype, id, geometry, properties)
DUMMY = [];

lbreak = '\n';
indent = '    ';
geofloat = '%.15g';
propfloat = '%.15g';

propnames = {};
if nargin < 5
    properties = [];
elseif ~isempty(properties)
    propnames = fieldnames(properties);
end
nprops = numel(propnames);
hasprops = nprops > 0;

indent2 = [indent, indent];
indent3 = [indent2, indent];
indent4 = [indent2, indent2];
indent5 = [indent3, indent2];

fid = fopen(filename, 'w');
fprintf(fid, ['{' lbreak indent '"type": "FeatureCollection",' lbreak indent '"features": [{' lbreak]);
for i = 1:length(geometry)
    if i > 1
        fprintf(fid, [indent '}, {' lbreak]);
    end
    fprintf(fid, [indent2 '"type": "Feature",' lbreak indent2 '"id": "%s"' lbreak indent2 '"geometry": {' lbreak], id{i});
    switch geomtype
        case 'point'
            fprintf(fid, [indent3 '"type": "Point",' lbreak indent3 '"coordinates": [' geofloat ', ' geofloat ']' lbreak], geometry{i});
        case 'polyline'
            fprintf(fid, [indent3 '"type": "LineString",' lbreak indent3 '"coordinates": [' lbreak]);
            fprintf(fid, [indent4 '[' geofloat ', ' geofloat '],' lbreak], geometry{i}(1:end-1,:)');
            fprintf(fid, [indent4 '[' geofloat ', ' geofloat ']' lbreak], geometry{i}(end,:));
            fprintf(fid, [indent3 ']' lbreak]);
        case 'polygon'
            fprintf(fid, [indent3 '"type": "Polygon",' lbreak indent3 '"coordinates": [' lbreak indent4 '[' lbreak]);
            fprintf(fid, [indent5 '[' geofloat ', ' geofloat '],' lbreak], geometry{i}(1:end-1,:)');
            fprintf(fid, [indent5 '[' geofloat ', ' geofloat ']' lbreak], geometry{i}(end,:));
            fprintf(fid, [indent4 ']' lbreak indent3 ']' lbreak]);
    end
    fprintf(fid, [indent2 '},' lbreak indent2 '"properties": ']);
    if hasprops
        fprintf(fid, ['{' lbreak]);
        end_prev_prop = '';
        for p = 1:nprops
            propValues = properties.(propnames{p});
            if iscell(propValues)
                val = propValues{i};
            else
                val = propValues(i);
            end
            if isempty(val)
                continue
            elseif ischar(val)
                propval = sprintf('"%s"', val);
            elseif isnan(val)
                propval = 'null';
            else
                propval = sprintf(propfloat, val);
            end
            fprintf(fid, [end_prev_prop indent3 '"%s": %s'], propnames{p}, propval);
            end_prev_prop = [',' lbreak];
        end
        fprintf(fid, [lbreak indent2 '}' lbreak]);
    else
        fprintf(fid, ['null' lbreak]);
    end
end
fprintf(fid, [indent '}]' lbreak '}' lbreak]);
fclose(fid);