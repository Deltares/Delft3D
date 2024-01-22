function S = qp_session(cmd,varargin)
%QP_SESSION Save QuickPlot figures to and rebuild them from an ASCII file.
%   QP_SESSION is still incomplete, not documented and subject to change.

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

switch cmd
    case 'expandables'
        S = local_identify_expandables(varargin{:});
    case 'read'
        S = local_read(varargin{:});
    case 'rebuild'
        S = local_rebuild(varargin{:});
        %
    case 'extract'
        S = local_extract(varargin{:});
    case 'serialize'
        S = local_serialize(varargin{:});
    case 'make_expandables'
        S = local_make_expandables(varargin{:});
    case 'save'
        local_save(varargin{:})
end

function v = current_version(cmd,version)
switch cmd
    case 'writes'
        v = '1.1';
    case 'supports'
        vn = sscanf(version,'%d.'); % 1.10 gives minor = 10, 1.01 accepted as alternative to 1.1
        major = vn(1);
        if length(vn) >= 2
            minor = vn(2);
        else
            minor = 0;
        end
        v = (major == 1) & (minor <= 1);
        
end

function S = local_make_expandables(S,explist)
values = repmat({{}},size(explist));
for i = 1:length(S)
    [key,rem] = strtok(S{i});
    key = lower(key);
    [chk,j] = ismember(key,explist);
    if chk
        args = parseargs(S{i});
        for p = 2:length(args)
            val = args{p};
            if strcmp(key,'filename') && strcmp(val,'geodata')
                continue
            end
            [chk,k] = ismember(val,values{j});
            if ~chk
                k = length(values{j})+1;
                values{j}{k} = val;
            end
            exp = sprintf('$%s%i$',key,k);
            S{i} = qp_strrep(S{i},val,exp);
        end
    end
end
%
nval = cellfun('length',values);
if sum(nval)>0
    S    = [S(1);cell(sum(nval)+3,1);S(2:end)];
    S{2} = '';
    S{3} = 'Expand';
    maxL = 0;
    Line = 4;
    for j = 1:length(explist)
        for k = 1:length(values{j})
            S{Line} = sprintf('%s%i',explist{j},k);
            maxL    = max(maxL,length(S{Line}));
            Line    = Line+1;
        end
    end
    Fmt = ['  %-' num2str(maxL) 's ''%s'''];
    Line = 4;
    for j = 1:length(explist)
        for k = 1:length(values{j})
            S{Line} = sprintf(Fmt,S{Line},values{j}{k});
            Line    = Line+1;
        end
    end
    S{Line} = 'EndExpand';
end

function X = local_identify_expandables(filename)
XX = cell(100,2);
X = {};
j = 0;
fid = fopen(filename,'r','n','UTF-8');
Str = getline(fid);
Expand = 0;
while ~isempty(Str)
    if strcmpi(strtrim(Str),'Expand')
        Expand = 1;
    elseif strcmpi(strtrim(Str),'EndExpand')
        Expand = 0;
    elseif Expand
        args = parseargs(Str);
    else
        ch = strfind(Str,'$');
        i = 1;
        while i<length(ch)
            substr = Str(ch(i)+1:ch(i+1)-1);
            if isvarname(substr)
                substr = lower(substr);
                if ~ismember(substr,X)
                    if j==length(XX)
                        XX{2*j} = [];
                    end
                    j = j+1;
                    XX{j} = substr;
                    X = XX(1:j);
                end
            end
            i = i+1;
        end
    end
    Str = getline(fid);
end
fclose(fid);
X = sort(X);

function S = local_read(filename,PAR)
if nargin==1
    PAR      = [];
    parnames = {};
else
    parnames = fieldnames(PAR);
end
fid = fopen(filename,'r','n','UTF-8');
Str = getline(fid);
expand = 0;
fgi = 0;
axi = 0;
itm = 0;
opt = '';
Line = 1;
S = [];
while ~isempty(Str)
    if ~isempty(PAR)
        Str = qp_strrep(Str,PAR,'$');
    end
    args = parseargs(Str);
    key  = lower(args{1});
    if Line==1
        if ~strcmp(key,'delft3d-quickplot') ...
                || length(args)<3 ...
                || ~ischar(args{2}) ...
                || ~strcmp(args{2},'session file') ...
                || ~isnumeric(args{3})
            fclose(fid);
            error('First line of session file should read:\nDelft3D-QUICKPLOT ''session file'' <versionnumber>\nHowever, first line of %s reads:\n%s',filename,Str)
        else
            args{3} = sscanf(Str,'%*[^'']''%*[^'']'' %s',1);
            if ~current_version('supports',args{3})
                fclose(fid);
                error('Version %s of session file not supported.',args{3})
            end
        end
        Line=2;
    end
    switch key
        case 'expand'
            expand = 1;
        case 'endexpand'
            expand = 0;
        case 'figure'
            fgi = length(S)+1;
            axi = 0;
            S(fgi).name = args{2};
            S(fgi).papertype = 'a4';
            S(fgi).paperorientation = 'portrait';
            S(fgi).papersize = [];
            S(fgi).paperunits = 'centimeters';
            S(fgi).windowsize = [];
            S(fgi).colour = get(0,'factoryuicontrolbackgroundcolor')*255;
            S(fgi).expandpar = [];
            S(fgi).frame.style = 'none';
            S(fgi).axes = [];
        case 'framestyle'
            S(fgi).frame.style = args{2};
        case 'axes'
            axi = length(S(fgi).axes)+1;
            S(fgi).axes(axi).name = args{2};
            S(fgi).axes(axi).position = [];
            S(fgi).axes(axi).type = [];
            S(fgi).axes(axi).colour = [255 255 255];
            S(fgi).axes(axi).box = 'off';
            S(fgi).axes(axi).linewidth = 0.5;
            S(fgi).axes(axi).title = '<automatic>';
            for x = 'xyz'
                S(fgi).axes(axi).([x 'label']) = '<automatic>';
                S(fgi).axes(axi).([x 'colour']) = [0 0 0];
                S(fgi).axes(axi).([x 'grid'])  = 'off';
                if x=='x'
                    S(fgi).axes(axi).([x 'loc']) = 'bottom';
                elseif x=='y'
                    S(fgi).axes(axi).([x 'loc']) = 'left';
                end
                S(fgi).axes(axi).([x 'scale']) = 'linear';
                S(fgi).axes(axi).([x 'lim']) = 'auto';
            end
            S(fgi).axes(axi).items = [];
        case 'item'
            itm = length(S(fgi).axes(axi).items)+1;
            S(fgi).axes(axi).items(itm).name       = args{2};
            S(fgi).axes(axi).items(itm).filename   = '';
            S(fgi).axes(axi).items(itm).domain     = '';
            S(fgi).axes(axi).items(itm).subfield   = '';
            S(fgi).axes(axi).items(itm).dimensions = [];
            S(fgi).axes(axi).items(itm).options    = [];
        case 'dimensions'
            opt = 'dimensions';
        case 'enddimensions'
            opt = '';
        case 'options'
            opt = 'item';
        case 'endoptions'
            opt = '';
        case 'parametersource'
            opt = 'parsource';
        case 'endparametersource'
            opt = '';
        case 'enditem'
            itm = 0;
        case 'endaxes'
            axi = 0;
        case 'endfigure'
            fgi = 0;
        otherwise
            if expand
                if ~ismember(key,parnames)
                    PAR.(key) = args{2};
                end
            elseif fgi>0
                if length(key)>5 && strcmp(key(end-4:end),'color')
                    key = [key(1:end-5) 'colour'];
                end
                if length(args)>2
                    val = args(2:end);
                else
                    val = args{2};
                end
                if strcmp(opt,'parsource')
                    S(fgi).expandpar.(key) = val;
                elseif strcmp(opt,'dimensions')
                    S(fgi).axes(axi).items(itm).dimensions.(key) = val;
                elseif strcmp(opt,'item')
                    S(fgi).axes(axi).items(itm).options.(key) = val;
                elseif itm>0
                    S(fgi).axes(axi).items(itm).(key) = val;
                elseif axi>0
                    S(fgi).axes(axi).(key) = val;
                elseif strncmp(key,'frametext',9)
                    S(fgi).frame.(key) = val;
                else
                    S(fgi).(key) = val;
                end
            end
    end
    Str = getline(fid);
end
fclose(fid);

function Str = getline(fid)
Str = '';
while isempty(Str) && ~feof(fid)
    Str = fgetl(fid);
end

function args = parseargs(Str)
i=0;
args = {};
while 1
    [Key,Rem] = strtok(Str);
    if isempty(Key)
        break
    elseif Key(1)=='['
        i1 = strfind(Str,'[');
        i2 = strfind(Str,']');
        Key = str2vec(Str(i1(1)+1:i2(1)-1),'%f');
        Str = Str(i2(1)+1:end);
    elseif Key(1)==''''
        i1 = strfind(Str,'''');
        j  = 2;
        while j<length(i1)-1 && i1(j)+1==i1(j+1)
            j = j+2;
        end
        Key = Str(i1(1)+1:i1(j)-1);
        Key = strrep(Key,'''''','''');
        Str = Str(i1(j)+1:end);
    elseif i>0
        Key = str2double(Key);
        Str = Rem;
    else
        Str = Rem;
    end
    i=i+1;
    args{i} = Key;
end

function local_save(S,filename)
if isstruct(S)
    S = local_serialize(S);
end
fid = fopen(filename,'w','n','UTF-8');
fprintf(fid,'%s\n',S{:});
fclose(fid);

function C = addline(C,varargin)
if isempty(C)
    C = cell(100,1);
    C{1} = 1;
end
i = C{1}+1;
if i>length(C)
    C{2*i} = [];
end
C{i} = sprintf(varargin{:});
C{1} = i;

function C = local_serialize(S)
C = addline({},'Delft3D-QUICKPLOT ''session file'' %s',current_version('writes'));
for fgi = 1:length(S)
    C = addline(C,'');
    C = addline(C,'Figure             ''%s''',quote_protect(S(fgi).name));
    C = addline(C,'  PaperType        ''%s''',S(fgi).papertype);
    C = addline(C,'  PaperOrientation ''%s''',S(fgi).paperorientation);
    if strcmp(S(fgi).papertype,'<custom>')
        C = addline(C,'  PaperSize        [%g %g]',S(fgi).papersize);
        C = addline(C,'  PaperUnits       ''%s''',S(fgi).paperunits);
    end
    if ~isequal(S(fgi).colour,get(0,'factoryuicontrolbackgroundcolor')*255)
        C = addline(C,'  Colour           [%i %i %i]',S(fgi).colour);
    end
    C = addline(C,'  WindowSize       [%i %i]',S(fgi).windowsize);
    if ~isempty(S(fgi).expandpar)
        C = addline(C,'  ParameterSource');
        if iscell(S(fgi).expandpar.filename)
            N = length(S(fgi).expandpar.filename);
            C = addline(C,['    FileName ',repmat(' ''%s''',1,N)],S(fgi).expandpar.filename{:});
        else
            C = addline(C,'    FileName  ''%s''',S(fgi).expandpar.filename);
        end
        if ~isempty(S(fgi).expandpar.domain)
            C = addline(C,'    Domain    ''%s''',quote_protect(S(fgi).expandpar.domain));
        end
        C = addline(C,'  EndParameterSource');
    end
    C = addline(C,'  FrameStyle       ''%s''',quote_protect(S(fgi).frame.style));
    ibt = 1;
    fld = 'frametext1';
    while isfield(S(fgi).frame,fld)
        C = addline(C,'  FrameText%-4i    ''%s''',ibt,quote_protect(S(fgi).frame.(fld)));
        ibt = ibt+1;
        fld = sprintf('frametext%i',ibt);
    end
    %
    for axi = 1:length(S(fgi).axes)
        C = addline(C,'');
        C = addline(C,'  Axes        ''%s''',quote_protect(S(fgi).axes(axi).name));
        C = addline(C,'    Position  [%g %g %g %g]',S(fgi).axes(axi).position);
        if ~isempty(S(fgi).axes(axi).type)
            C = addline(C,'    Type      ''%s''',quote_protect(S(fgi).axes(axi).type));
        end
        if ~strcmp(S(fgi).axes(axi).title,'<automatic>')
            C = addline(C,'    Title     ''%s''',quote_protect(S(fgi).axes(axi).title));
        end
        if ischar(S(fgi).axes(axi).colour)
            C = addline(C,'    Colour    ''none''');
        elseif ~isequal(S(fgi).axes(axi).colour,[255 255 255])
            C = addline(C,'    Colour    [%i %i %i]',S(fgi).axes(axi).colour);
        end
        C = addline(C,'    Box       ''%s''',S(fgi).axes(axi).box);
        C = addline(C,'    LineWidth %g',S(fgi).axes(axi).linewidth);
        for x = 'xy'
            X = upper(x);
            if ~strcmp(S(fgi).axes(axi).([x 'label']),'<automatic>')
                C = addline(C,'    %sLabel    ''%s''',X,quote_protect(S(fgi).axes(axi).([x 'label'])));
            end
            if ~strcmp(S(fgi).axes(axi).([x 'grid']),'off')
                C = addline(C,'    %sGrid     ''%s''',X,S(fgi).axes(axi).([x 'grid']));
            end
            if X<'Z'
                C = addline(C,'    %sLoc      ''%s''',X,S(fgi).axes(axi).([x 'loc']));
            end
            if ~strcmp(S(fgi).axes(axi).([x 'scale']),'linear')
                C = addline(C,'    %sScale    ''%s''',X,S(fgi).axes(axi).([x 'scale']));
            end
            if ~ischar(S(fgi).axes(axi).([x 'lim'])) % i.e. not "auto"
                C = addline(C,'    %sLim      [%g %g]',X,S(fgi).axes(axi).([x 'lim']));
            end
            if ~isequal(S(fgi).axes(axi).([x 'colour']),[0 0 0])
                C = addline(C,'    %sColour   [%i %i %i]',X,S(fgi).axes(axi).([x 'colour']));
            end
        end
        %
        for itm = 1:length(S(fgi).axes(axi).items)
            C = addline(C,'');
            C = addline(C,'    Item        ''%s''',quote_protect(S(fgi).axes(axi).items(itm).name));
            if iscell(S(fgi).axes(axi).items(itm).filename)
                N = length(S(fgi).axes(axi).items(itm).filename);
                C = addline(C,['      FileName ',repmat(' ''%s''',1,N)],S(fgi).axes(axi).items(itm).filename{:});
            else
                C = addline(C,'      FileName  ''%s''',S(fgi).axes(axi).items(itm).filename);
            end
            if ~isempty(S(fgi).axes(axi).items(itm).domain)
                C = addline(C,'      Domain    ''%s''',quote_protect(S(fgi).axes(axi).items(itm).domain));
            end
            if ~isempty(S(fgi).axes(axi).items(itm).subfield)
                C = addline(C,'      SubField  ''%s''',quote_protect(S(fgi).axes(axi).items(itm).subfield));
            end
            if ~isempty(S(fgi).axes(axi).items(itm).dimensions)
                C = addline(C,'      Dimensions');
                flds = fieldnames(S(fgi).axes(axi).items(itm).dimensions);
                for ifld = 1:length(flds)
                    val = S(fgi).axes(axi).items(itm).dimensions.(flds{ifld});
                    if ischar(val)
                        C = addline(C,'        %-24s ''%s''',flds{ifld},val);
                    elseif isnumeric(val) && isequal(size(val),[1 1])
                        C = addline(C,'        %-24s %g',flds{ifld},val);
                    elseif isnumeric(val) && size(val,1)==1
                        C = addline(C,'        %-24s %s',flds{ifld},vec2str(val));
                    end
                end
                C = addline(C,'      EndDimensions');
            end
            if ~isempty(S(fgi).axes(axi).items(itm).options)
                C = addline(C,'      Options');
                flds = fieldnames(S(fgi).axes(axi).items(itm).options);
                for ifld = 1:length(flds)
                    val = S(fgi).axes(axi).items(itm).options.(flds{ifld});
                    if ischar(val)
                        C = addline(C,'        %-24s ''%s''',flds{ifld},quote_protect(val));
                    elseif isnumeric(val) && isequal(size(val),[1 1])
                        C = addline(C,'        %-24s %g',flds{ifld},val);
                    elseif isnumeric(val) && size(val,1)==1
                        C = addline(C,'        %-24s %s',flds{ifld},vec2str(val));
                    end
                end
                C = addline(C,'      EndOptions');
            end
            C = addline(C,'    EndItem');
        end
        C = addline(C,'  EndAxes');
    end
    C = addline(C,'EndFigure');
end
C = C(2:C{1});

function H = local_rebuild(S,PAR)
if ischar(S)
    if nargin>1
        S = qp_session('read',S,PAR);
    else
        S = qp_session('read',S);
    end
end
opened_files = {};
for fgi = length(S):-1:1
    d3d_qp('newfigure','free format figure',S(fgi).name)
    H(fgi) = qpsf;
    d3d_qp('figurecolour',S(fgi).colour/255)
    if strcmp(S(fgi).papertype,'<custom>')
        d3d_qp('figurepapertype','<custom>',S(fgi).papersize,S(fgi).paperunits)
    else
        d3d_qp('figurepapertype',S(fgi).papertype,S(fgi).paperorientation)
    end
    d3d_qp('figureborderstyle',S(fgi).frame.style)
    %
    if ~isempty(S(fgi).expandpar)
        opened_files = file_and_domain(opened_files,S(fgi).expandpar);
        %
        d3d_qp('setasparametersource')
    end
    if ~strcmp(S(fgi).frame.style,'none')
        ibt = 1;
        fld = 'frametext1';
        BTx = {};
        while isfield(S(fgi).frame,fld)
            BTx{ibt} = S(fgi).frame.(fld);
            ibt = ibt+1;
            fld = sprintf('frametext%i',ibt);
        end
        if ~isempty(BTx)
            d3d_qp('figureborder',BTx{:})
        end
    end
    d3d_qp('deleteaxes') % delete default axes created by figure border
    if ~isempty(S(fgi).windowsize)
        qp_figaspect(H(fgi),S(fgi).windowsize)
    end
    %
    for axi = length(S(fgi).axes):-1:1
        d3d_qp('newaxes_specloc',S(fgi).axes(axi).position,'normalized')
        d3d_qp('axesname',S(fgi).axes(axi).name)
        if ~isempty(S(fgi).axes(axi).type)
            d3d_qp('setaxestype',S(fgi).axes(axi).type)
        end
        if ischar(S(fgi).axes(axi).colour)
            d3d_qp('axescolour',S(fgi).axes(axi).colour)
        else
            d3d_qp('axescolour',S(fgi).axes(axi).colour/255)
        end
        d3d_qp('axesboxed',strcmp(S(fgi).axes(axi).box,'on'))
        d3d_qp('axeslinewidth',S(fgi).axes(axi).linewidth)
        d3d_qp('axesgrid',strcmp(S(fgi).axes(axi).xgrid,'on'),strcmp(S(fgi).axes(axi).ygrid,'on'))
        d3d_qp('axesloc',S(fgi).axes(axi).xloc,S(fgi).axes(axi).yloc)
        d3d_qp('axeslimits',S(fgi).axes(axi).xlim,S(fgi).axes(axi).ylim)
        d3d_qp('axesscale',S(fgi).axes(axi).xscale,S(fgi).axes(axi).yscale)
        %
        d3d_qp('title',S(fgi).axes(axi).title)
        for x = 'xyz'
            d3d_qp([x 'label'],S(fgi).axes(axi).([x 'label']))
            d3d_qp([x 'colour'],S(fgi).axes(axi).([x 'colour'])/255)
        end
        %
        for itm = length(S(fgi).axes(axi).items):-1:1
            if isequal(S(fgi).axes(axi).items(itm).filename,'geodata')
                d3d_qp('geodata',S(fgi).axes(axi).items(itm).name)
                continue
            end
            %
            opened_files = file_and_domain(opened_files,S(fgi).axes(axi).items(itm));
            %
            if d3d_qp('selectfield',S(fgi).axes(axi).items(itm).name)
                if ~isempty(S(fgi).axes(axi).items(itm).subfield)
                    d3d_qp('selectsubfield',S(fgi).axes(axi).items(itm).subfield)
                end
                %
                Props = d3d_qp('selectedfield');
                %
                if ~isempty(S(fgi).axes(axi).items(itm).dimensions)
                    dim0 = {'time' 'station' 'm' 'n' 'k'};
                    DIM0 = {'T' 'S' 'M' 'N' 'K'};
                    if length(Props.DimFlag)>5
                        dims = [dim0 lower(Props.DimName)];
                        DIMS = [DIM0 Props.DimName];
                    else
                        dims = dim0;
                        DIMS = DIM0;
                    end
                    for dim = 1:length(dims)
                        if Props.DimFlag(dim)~=0
                            sel = S(fgi).axes(axi).items(itm).dimensions.(dims{dim});
                            if isnumeric(sel)
                                d3d_qp(['all' DIMS{dim}],0)
                                d3d_qp(['edit' DIMS{dim}],sel)
                            else
                                d3d_qp(['all' DIMS{dim}],1)
                            end
                        end
                    end
                end
                %
                if ~isempty(S(fgi).axes(axi).items(itm).options)
                    Ops = S(fgi).axes(axi).items(itm).options;
                    if isfield(Ops,'axestype')
                        d3d_qp('axestype',Ops.axestype)
                    end
                    if isfield(Ops,'vectorcomponent')
                        d3d_qp('component',Ops.vectorcomponent)
                    end
                    if isfield(Ops,'presentationtype') && ...
                            ~strcmp(Ops.presentationtype,{'polygons'}) && ...
                            (~isfield(Ops,'vectorcomponent') || ~strcmp(Ops.presentationtype,Ops.vectorcomponent))
                        d3d_qp('presenttype',Ops.presentationtype)
                    end
                    if isfield(Ops,'extend2edge')
                        d3d_qp('extend2edge',Ops.extend2edge)
                    end
                    if isfield(Ops,'vectorcolour')
                        d3d_qp('colourvectors',1)
                        if ~strcmp(Ops.vectorcolour,'component')
                            d3d_qp('vectorcolour',Ops.vectorcolour)
                        end
                    elseif isfield(Ops,'vectorcomponent') && strncmp(Ops.vectorcomponent,'vector',6)
                        d3d_qp('colourvectors',0)
                    end
                    if isfield(Ops,'units')
                        d3d_qp('dataunits',Ops.units)
                    else
                        d3d_qp('dataunits','As in file')
                    end
                    if isfield(Ops,'angleconvention')
                        d3d_qp('angleconvention',Ops.angleconvention)
                    end
                    if isfield(Ops,'plotcoordinate')
                        d3d_qp('plotcoordinate',Ops.plotcoordinate)
                    end
                    if isfield(Ops,'vectorstyle')
                        d3d_qp('vectorstyle',Ops.vectorstyle)
                    end
                    if isfield(Ops,'vectorscalingmode')
                        d3d_qp('vecscalem',Ops.vectorscalingmode)
                    end
                    if isfield(Ops,'vectorscale')
                        d3d_qp('1vecunit',Ops.vectorscale)
                    end
                    if isfield(Ops,'verticalscalingmode')
                        d3d_qp('vertscalem',Ops.verticalscalingmode)
                    end
                    if isfield(Ops,'verticalscalefactor')
                        d3d_qp('vscale',Ops.verticalscalefactor)
                    end
                    if isfield(Ops,'numformat')
                        d3d_qp('numformat',Ops.numformat)
                    end
                    if isfield(Ops,'fontsize')
                        d3d_qp('fontsize',Ops.fontsize)
                    end
                    if isfield(Ops,'horizontalalignment')
                        d3d_qp('horizontalalignment',Ops.horizontalalignment)
                    end
                    if isfield(Ops,'verticalalignment')
                        d3d_qp('verticalalignment',Ops.verticalalignment)
                    end
                    if isfield(Ops,'colourdams')
                        d3d_qp('colourdams',Ops.colourdams)
                    end
                    if isfield(Ops,'colour')
                        d3d_qp('colour',Ops.colour)
                    end
                    if isfield(Ops,'facecolour')
                        d3d_qp('fillpolygons',1)
                        d3d_qp('facecolour',Ops.facecolour)
                    elseif isfield(Ops,'presentationtype') && strcmp(Ops.presentationtype,'polygons')
                        d3d_qp('fillpolygons',0)
                    end
                    if isfield(Ops,'textboxfacecolour')
                        d3d_qp('textbox',1)
                        d3d_qp('textboxfacecolour',Ops.textboxfacecolour)
                    elseif isfield(Ops,'presentationtype') && strcmp(Ops.presentationtype,'values')
                        d3d_qp('textbox',0)
                    end
                    if isfield(Ops,'linestyle')
                        d3d_qp('linestyle',Ops.linestyle)
                    end
                    if isfield(Ops,'linewidth')
                        d3d_qp('linewidth',Ops.linewidth)
                    end
                    if isfield(Ops,'marker')
                        d3d_qp('marker',Ops.marker)
                        if strcmp(Ops.markercolour,'auto')
                            d3d_qp('usemarkercolour',0)
                        else
                            d3d_qp('usemarkercolour',1)
                            d3d_qp('markercolour',Ops.markercolour)
                        end
                        if strcmp(Ops.markerfillcolour,'none')
                            d3d_qp('usemarkerfillcolour',0)
                        else
                            d3d_qp('usemarkerfillcolour',1)
                            d3d_qp('markerfillcolour',Ops.markerfillcolour)
                        end
                    end
                    if isfield(Ops,'presentationtype') && strcmp(Ops.presentationtype,'patches')
                        if isfield(Ops,'thresholds')
                            d3d_qp('colclassify',1)
                        else
                            d3d_qp('colclassify',0)
                        end
                    end
                    if isfield(Ops,'thresholds')
                        d3d_qp('thresholds',Ops.thresholds)
                    end
                    if isfield(Ops,'thresholddistribution')
                        d3d_qp('threshdistr',Ops.thresholddistribution)
                    end
                    if isfield(Ops,'colourlimits')
                        if isempty(Ops.colourlimits)
                            d3d_qp('climmode','automatic')
                        else
                            d3d_qp('climmode','manual')
                            d3d_qp('climmin',Ops.colourlimits(1))
                            d3d_qp('climmax',Ops.colourlimits(2))
                        end
                    end
                    if isfield(Ops,'symmetriccolourlimits')
                        d3d_qp('climsymm',Ops.symmetriccolourlimits)
                    end
                    if isfield(Ops,'colourmap')
                        d3d_qp('colourmap',Ops.colourmap)
                    end
                    if isfield(Ops,'colourbar')
                        switch Ops.colourbar
                            case 'none'
                                d3d_qp('colourbar',0)
                            case 'vert'
                                d3d_qp('colourbar',1)
                                d3d_qp('colbarhorz',0)
                            case 'horiz'
                                d3d_qp('colourbar',1)
                                d3d_qp('colbarhorz',1)
                        end
                    end
                    if isfield(Ops,'thinningmode')
                        d3d_qp('thinfld',Ops.thinningmode)
                    end
                    if isfield(Ops,'thinningfactors')
                        d3d_qp('thinfact',Ops.thinningfactors(1))
                    end
                    if isfield(Ops,'thinningdistance')
                        d3d_qp('thindist',Ops.thinningdistance)
                    end
                    if isfield(Ops,'clippingvalues')
                        if isnumeric(Ops.clippingvalues)
                            Ops.clippingvalues = sprintf('%g',Ops.clippingvalues);
                        end
                        d3d_qp('clippingvals',Ops.clippingvalues)
                    end
                    if isfield(Ops,'xclipping')
                        if isnumeric(Ops.xclipping)
                            Ops.xclipping = sprintf('%g',Ops.xclipping);
                        end
                        d3d_qp('xclipping',Ops.xclipping)
                    end
                    if isfield(Ops,'yclipping')
                        if isnumeric(Ops.yclipping)
                            Ops.yclipping = sprintf('%g',Ops.yclipping);
                        end
                        d3d_qp('yclipping',Ops.yclipping)
                    end
                end
                d3d_qp('addtoplot')
            else
                ui_message('error','Skipping dataset "%s"; field not found.',S(fgi).axes(axi).items(itm).name)
            end
        end
    end
end


function opened_files = file_and_domain(opened_files,item)
fn = item.filename;
if iscell(fn)
    if any(ismember(fn,opened_files))
        d3d_qp('selectfile',fn{:})
    else
        d3d_qp('openfile',fn{:})
        opened_files{end+1} = fn;
    end
else
    if any(ismember(fn,opened_files))
        d3d_qp('selectfile',fn)
    else
        d3d_qp('openfile',fn)
        opened_files{end+1} = fn;
    end
end
%
if isfield(item,'domain') && ~isempty(item.domain)
    d3d_qp('selectdomain',item.domain)
end


function S = local_extract(H, S)
if nargin>1
    offset = length(S);
else
    offset = 0;
end

for fgi = length(H):-1:1
    HInfo = get(H(fgi));
    fgi1 = offset + fgi;
    
    S(fgi1).name        = HInfo.Name;
    S(fgi1).papertype   = HInfo.PaperType;
    S(fgi1).paperorientation = HInfo.PaperOrientation;
    S(fgi1).papersize   = HInfo.PaperSize;
    S(fgi1).paperunits  = HInfo.PaperUnits;
    S(fgi1).windowsize  = HInfo.Position(3:4); % Units = pixels
    S(fgi1).colour      = round(HInfo.Color*255);
    S(fgi1).expandpar   = [];
    ExpandP = getappdata(H(fgi),'ExpandPAR');
    if ~isempty(ExpandP)
        S(fgi1).expandpar.filename = ExpandP.FileName;
        S(fgi1).expandpar.domain   = ExpandP.Domain;
    end
    S(fgi1).frame.style = 'none';
    
    for i = 1:length(HInfo.Children)
        A = HInfo.Children(i);
        UD = get(A,'userdata');
        if strcmp(get(A,'type'),'axes') && ...
                strcmp(get(A,'tag'),'Colorbar') && ...
                isfield(UD,'origPos') && isfield(UD,'PlotHandle')
            setappdata(UD.PlotHandle,'origPos_before_Colorbar',UD.origPos)
        end
    end
    
    axi = 0;
    S(fgi1).axes = [];
    for i = 1:length(HInfo.Children)
        A = HInfo.Children(i);
        AInfo = get(A);
        if strcmp(AInfo.Type,'axes') && strcmp(AInfo.Tag,'border')
            % use A.UserData;
            AInfo = md_paper(A,'getprops');
            S(fgi1).frame.style = AInfo.Name;
            ibt = 1;
            btxt = 'BorderText1';
            ftxt = 'frametext1';
            while isfield(AInfo,btxt)
                S(fgi1).frame.(ftxt) = AInfo.(btxt);
                ibt  = ibt+1;
                btxt = sprintf('BorderText%i',ibt);
                ftxt = sprintf('frametext%i',ibt);
            end
        elseif strcmp(AInfo.Type,'axes') && strcmp(AInfo.Tag,'Colorbar')
            % skip
        elseif strcmp(AInfo.Type,'axes')
            % normal axes
            axi = axi+1;
            S(fgi1).axes(axi).name      = AInfo.Tag;
            S(fgi1).axes(axi).position  = AInfo.Position;
            if isappdata(A,'origPos_before_Colorbar')
                S(fgi1).axes(axi).position = getappdata(A,'origPos_before_Colorbar');
                rmappdata(A,'origPos_before_Colorbar')
            end
            if ischar(AInfo.Color)
                S(fgi1).axes(axi).colour = AInfo.Color;
            else
                S(fgi1).axes(axi).colour = round(AInfo.Color*255);
            end
            S(fgi1).axes(axi).type      = getappdata(A,'BasicAxesType');
            S(fgi1).axes(axi).box       = AInfo.Box;
            S(fgi1).axes(axi).linewidth = AInfo.LineWidth;
            %
            if isappdata(A,'title')
                S(fgi1).axes(axi).title = getappdata(A,'title');
            else
                S(fgi1).axes(axi).title = '<automatic>';
            end
            for x = 'xyz'
                X = upper(x);
                if isappdata(A,[x 'label'])
                    S(fgi1).axes(axi).([x 'label']) = getappdata(A,[x 'label']);
                else
                    S(fgi1).axes(axi).([x 'label']) = '<automatic>';
                end
                S(fgi1).axes(axi).([x 'colour']) = round(AInfo.([X 'Color'])*255);
                S(fgi1).axes(axi).([x 'grid']) = AInfo.([X 'Grid']);
                if X < 'Z'
                    S(fgi1).axes(axi).([x 'loc']) = AInfo.([X 'AxisLocation']);
                end
                S(fgi1).axes(axi).([x 'scale']) = AInfo.([X 'Scale']);
                if strcmp(AInfo.([X 'LimMode']),'manual')
                    xlm = AInfo.([X 'Lim']);
                    if strcmp(AInfo.([X 'Dir']),'reverse')
                        xlm = fliplr(xlm);
                    end
                    S(fgi1).axes(axi).([x 'lim']) = xlm;
                else
                    if strcmp(AInfo.([X 'Dir']),'reverse')
                        S(fgi1).axes(axi).([x 'lim']) = 'auto-reverse';
                    else
                        S(fgi1).axes(axi).([x 'lim']) = 'auto';
                    end
                end
            end
            %
            c = get(A,'children');
            t = get(c,'tag');
            ok = strncmp(t,'QPPlotTag',9);
            c = c(ok);
            u = get(c,'userdata');
            if iscell(u)
                ok = ~cellfun('isempty',u);
                %c = c(ok);
                u = u(ok);
            elseif isempty(u)
                u = {};
            else
                u = {u};
            end
            S(fgi1).axes(axi).items = [];
            for itm = length(u):-1:1
                IInfo = u{itm};
                S(fgi1).axes(axi).items(itm).name     = IInfo.PlotState.Props.Name;
                if isfield(IInfo.PlotState.FI,'Otherargs') && ~isempty(IInfo.PlotState.FI.Otherargs)
                    S(fgi1).axes(axi).items(itm).filename = [{IInfo.PlotState.FI.Name} IInfo.PlotState.FI.Otherargs];
                elseif isfield(IInfo.PlotState.FI,'Name')
                    S(fgi1).axes(axi).items(itm).filename = IInfo.PlotState.FI.Name;
                else
                    S(fgi1).axes(axi).items(itm).filename = IInfo.PlotState.FI.FileType;
                end
                %
                dom = qpread(IInfo.PlotState.FI,'domains');
                if isempty(dom)
                    dom = '';
                else
                    dom = dom{IInfo.PlotState.Domain};
                end
                S(fgi1).axes(axi).items(itm).domain   = dom;
                %
                sub = qpread(IInfo.PlotState.FI,IInfo.PlotState.Props,'subfields');
                if isempty(sub)
                    sub = '';
                else
                    sub = sub{IInfo.PlotState.SubField{1}};
                end
                S(fgi1).axes(axi).items(itm).subfield = sub;
                %
                S(fgi1).axes(axi).items(itm).dimensions = [];
                dim0 = {'time' 'station' 'm' 'n' 'k'};
                if length(IInfo.PlotState.Props.DimFlag)>5
                    dims = [dim0 lower(IInfo.PlotState.Props.DimName)];
                else
                    dims = dim0;
                end
                for dim = 1:length(IInfo.PlotState.Props.DimFlag)
                    if IInfo.PlotState.Props.DimFlag(dim)~=0
                        val = IInfo.PlotState.Selected{dim};
                        if isequal(val,0)
                            val = 'all';
                        end
                        S(fgi1).axes(axi).items(itm).dimensions.(dims{dim}) = val;
                    end
                end
                Ops = IInfo.PlotState.Ops;
                if isfield(Ops,'clippingvalues')
                    if isstruct(Ops.clippingvalues)
                        Ops.clippingvalues = realset(Ops.clippingvalues);
                    end
                end
                if isfield(Ops,'xclipping')
                    if isstruct(Ops.xclipping)
                        Ops.xclipping = realset(Ops.xclipping);
                    end
                end
                if isfield(Ops,'yclipping')
                    if isstruct(Ops.yclipping)
                        Ops.yclipping = realset(Ops.yclipping);
                    end
                end
                if isfield(Ops,'version')
                    Ops = rmfield(Ops,'version');
                end
                S(fgi1).axes(axi).items(itm).options  = Ops;
            end
        end
    end
end

function [liA,locB] = ismember(A,B)
if ischar(A)
    A = {A};
end
liA  = false(size(A));
locB = zeros(size(A));
for j = 1:numel(A)
    for i = 1:numel(B)
        if isequal(A{j},B{i})
            liA(j)  = true;
            locB(j) = i;
            break
        end
    end
end

function str = quote_protect(str)
str = strrep(str,'''','''''');

function str = unquote_protect(str)
str = strrep(str,'''''','''');
