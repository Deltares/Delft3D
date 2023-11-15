function varargout=ai_ungen(cmd,varargin)
%AI_UNGEN Read/write ArcInfo (un)generate files.
%   FILEINFO = AI_UNGEN('open',FILENAME) opens the specified file and reads
%   its contents. It returns a structure to be used in AI_UNGEN('read',...)
%   calls.
%
%   XY = AI_UNGEN('readc',FILEINFO,IDX) reads the objects listed by IDX
%   from the file given by FILEINFO. It returns a cell array XY in which
%   every entry XY{I} is an Nx2 matrix containing N pairs of X,Y
%   coordinates representing object I. If instead of the FILEINFO structure
%   -- as obtained from a AI_UNGEN('open',...) call -- a file name is
%   provided then the indicated file is first opened. If IDX isn't
%   specified or equal to ':' then the coordinates of all objects are
%   returned.
%
%   [X,Y] = AI_UNGEN('read',FILEINFO) returns the X and Y data in the file.
%   If instead of the INFO structure a file name is provided then the
%   indicated file is opened and the data is returned. If only one output
%   argument is requested then both X and Y coordinates are returned in the
%   same array: XY = AI_UNGEN('read',FILEINFO)
%
%   AI_UNGEN('write',FILENAME,X,Y) writes the line segments to file. X,Y
%   should either contain NaN separated line segments or X,Y cell arrays
%   containing the line segments. Alternatively, one may provide X and Y
%   coordinates in one array: AI_UNGEN('write',FILENAME,XY)
%
%   AI_UNGEN('write',...,'-1') doesn't write line segments of length 1.
%
%   See Also: BNA, LANDBOUNDARY, DXF.

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

if nargout>0
    varargout=cell(1,nargout);
end
if nargin==0
    return
end
switch cmd
    case 'open'
        Out=Local_open_file(varargin{:});
        varargout{1}=Out;
    case 'readc'
        varargout{1}=Local_read_file('cell',varargin{:});
    case 'read'
        Out=Local_read_file('array',varargin{:});
        if nargout==1
            varargout{1}=Out;
        elseif nargout>1
            varargout{1}=Out(:,1);
            varargout{2}=Out(:,2);
        end
    case 'write'
        Local_write_file(varargin{:});
    otherwise
        uiwait(msgbox('unknown command','modal'));
end


function T=Local_open_file(filename)
T=[];
if nargin==0
    [fn,fp]=uigetfile('*.gen');
    if ~ischar(fn)
        return
    end
    filename=[fp fn];
end

fid=fopen(filename,'r','n','US-ASCII');
T.FileName=filename;
T.FileType='ArcInfoUngenerate';
T.SubType='unknown';
T.Check='NotOK';

%Ungenerate with ANNO option writes:
%
%ID, LEVEL, SYMBOL, HEIGHT
%TEXT
%X,Y
%X,Y
%..
%END
%
% id=-999999 for islands
autoclose=0;
Line=fgetl(fid);
[id,nval,err,irem]=sscanf(Line,'%f%*[ ,]');
if nval>1 && ~isempty(err)
    fclose(fid);
    error('''%s'' while parsing first line ''%s''.',err,Line)
end
ncoords = 2;
switch nval
    case 5
        % ID, BottomLeftX, BottomLeftY, TopRightX, TopRightY
        T.SubType='rectangle';
        fseek(fid,0,-1);
    case 4
        % ID, CenterX, CenterY, Radius
        T.SubType='circle';
        fseek(fid,0,-1);
    case 3
        % point
        % ID, X, Y
        %
        % polygon with label point
        % ID, X, Y
        % X, Y
        % X, Y
        % X, Y
        % END
        Line2=fgetl(fid);
        [id2,nval2,err2,irem2]=sscanf(Line2,'%f%*[ ,]');
        switch nval2
            case 2
                T.SubType='polygon';
                fseek(fid,0,-1);
            case 3
                T.SubType='point';
                fseek(fid,0,-1);
            case 0
                if strcmpi(strtok(Line2(irem2:end)),'END')
                    T.SubType='point';
                    fseek(fid,0,-1);
                end
        end
    case 1
        % polyline
        % ID             or      ID
        % X, Y                   X, Y, Z
        % X, Y                   X, Y, Z
        % END                    END
        %
        % polygon with auto label
        % ID, AUTO
        % X, Y
        % X, Y
        % END
        switch upper(strtok(Line(irem:end)))
            case ''
                T.SubType='line';
            case 'AUTO'
                T.SubType='polygon';
            otherwise
                fclose(fid);
                error('Invalid string encountered ''%s''.',Line(irem:end))
        end
        %
        Line2=fgetl(fid);
        [id,ncoords,err,irem]=sscanf(Line2,'%f%*[ ,]');
        fseek(fid,0,-1);
    case 0
        switch upper(strtok(Line(irem:end)))
            case 'CLOSE'
                autoclose=1;
                T.SubType='line'; % set to polygon at the end
            case 'BOX'
                T.SubType='box'; % set to rectangle at the end
            case 'DONUT'
                T.SubType='donut';
            otherwise
                fclose(fid);
                error('Invalid string encountered ''%s''.',Line(irem:end))
        end
end
%
i=1;
finalEND=false;
switch ncoords
    case 2
        pFormat = '%f%*[ ,]%f\n';
    case 3
        pFormat = '%f%*[ ,]%f%*[ ,]%f\n';
    otherwise
        fclose(fid);
        error('%i coordinates not supported.',ncoords)
end
%
while ~feof(fid)
    start=ftell(fid);
    Line=fgetl(fid);
    [id,nval,err,irem]=sscanf(Line,'%f%*[ ,]');
    if nval==0 && strcmpi(Line(irem:end),'END')
        finalEND=true;
        break
    end
    switch T.SubType
        case 'point'
            switch nval
                case 3
                    T.Seg(i).ID=id(1);
                    T.Seg(i).Coord=id(2:3)';
                otherwise
                    fclose(fid);
                    error('Unable to read %s data on line: %s.',T.SubType,Line)
            end
        case 'line'
            switch nval
                case 1
                    T.Seg(i).ID=id;
                    T.Seg(i).Coord=fscanf(fid,pFormat,[ncoords inf])';
                    if autoclose && ~isequal(T.Seg(i).Coord(end,:),T.Seg(i).Coord(1,:))
                        T.Seg(i).Coord=T.Seg(i).Coord([1:end 1],:);
                    end
                case 2
                    i=i-1;
                    fseek(fid,start,-1);
                    Coord=fscanf(fid,pFormat,[ncoords inf])';
                    if autoclose && ~isequal(Coord(end,:),Coord(1,:))
                        Coord=Coord([1:end 1],:);
                    end
                    T.Seg(i).Coord=[T.Seg(i).Coord; NaN NaN; Coord];
                otherwise
                    fclose(fid);
                    error('Unable to read %s data on line: %s.',T.SubType,Line)
            end
            Line=fgetl(fid);
            if ~strcmp(deblank(Line),'END')
                fclose(fid);
                error('Unexpected string: %s.',Line)
            end
        case 'polygon'
            T.Seg(i).ID=id(1);
            switch nval
                case 1
                    T.Seg(i).LabelPoint='AUTO';
                    T.Seg(i).Coord=fscanf(fid,'%f%*[ ,]%f\n',[2 inf])';
                case 2
                    i=i-1;
                    fseek(fid,start,-1);
                    Coord=fscanf(fid,'%f%*[ ,]%f\n',[2 inf])';
                    if autoclose && ~isequal(Coord(end,:),Coord(1,:))
                        Coord=Coord([1:end 1],:);
                    end
                    T.Seg(i).Coord=[T.Seg(i).Coord; NaN NaN; Coord];
                case 3
                    T.Seg(i).LabelPoint=id(2:3)';
                    T.Seg(i).Coord=fscanf(fid,'%f%*[ ,]%f\n',[2 inf])';
                otherwise
                    fclose(fid);
                    error('Unable to read %s data on line: %s.',T.SubType,Line)
            end
            Line=fgetl(fid);
            if ~strcmp(deblank(Line),'END')
                fclose(fid);
                error('Unexpected string: %s.',Line)
            end
        case 'rectangle'
            switch nval
                case 5
                    T.Seg(i).ID=id(1);
                    T.Seg(i).Coord=id(2:5)';
                otherwise
                    fclose(fid);
                    error('Unable to read %s data on line: %s.',T.SubType,Line)
            end
        case 'box'
            switch nval
                case 4
                    T.Seg(i).ID=id(1);
                    T.Seg(i).Coord=[id(2:3);id(2:3)+id(4)]';
                case 5
                    T.Seg(i).ID=id(1);
                    T.Seg(i).Coord=[id(2:3);id(2:3)+id(4:5)]';
                otherwise
                    fclose(fid);
                    error('Unable to read %s data on line: %s.',T.SubType,Line)
            end
        case 'circle'
            switch nval
                case 4
                    T.Seg(i).ID=id(1);
                    T.Seg(i).Coord=id(2:4)';
                otherwise
                    fclose(fid);
                    error('Unable to read %s data on line: %s.',T.SubType,Line)
            end
        case 'donut'
            switch nval
                case 5
                    T.Seg(i).ID=id(1);
                    T.Seg(i).Coord=id(2:5)';
                otherwise
                    fclose(fid);
                    error('Unable to read %s data on line: %s.',T.SubType,Line)
            end
    end
    i=i+1;
end
fclose(fid);
switch T.SubType
    case 'box'
        T.SubType='rectangle';
    case 'line'
        if autoclose
            T.SubType='polygon';
        end
end
if ~finalEND
    error('Missing closing END statement in file.');
end
T.Check='OK';
%
% Compute total number of points
%
switch T.SubType
    case 'point'
        nel=length(T.Seg);
    case {'line','polygon'}
        nel=0;
        for i=1:length(T.Seg)
            nel=nel+size(T.Seg(i).Coord,1)+1;
        end
    case 'rectangle'
        nel=length(T.Seg)*(4+2);
    case 'circle'
        nel=length(T.Seg)*(NPointPerCircle+2);
    case 'donut'
        nel=length(T.Seg)*2*(NPointPerCircle+2);
end
T.TotalNPnt=nel-1;


function N=NPointPerCircle
N=36;


function Data=Local_read_file(tp,varargin)
if nargin==1
    T=Local_open_file;
else
    if isstruct(varargin{1})
        T=varargin{1};
    else
        T=Local_open_file(varargin{1});
    end
end
if nargin<=2 || isequal(varargin{2},':')
    objects = 1:length(T.Seg);
else
    objects = varargin{2};
end

if strcmp(T.SubType,'point')
    nel = length(objects);
    Data = NaN(nel,2);
    for i = 1:nel
        Data(i,:) = T.Seg(objects(i)).Coord;
    end
    return
end

cellData = strcmp(tp,'cell');
if cellData
    Data = cell(1,length(objects));
else
    switch T.SubType
        case {'line','polygon'}
            nel = 0;
            for i = objects
                t1=size(T.Seg(i).Coord,1);
                nel = nel+t1+1;
            end
        case 'rectangle'
            nel = (5+1) * length(objects);
        case 'circle'
            nel = (NPointPerCircle+1) * length(objects);
        case 'donut'
            nel = (2*NPointPerCircle+2) * length(objects);
    end
    offset = 0;
    Data = NaN(nel,2);
end

alpha=2*pi*[0:NPointPerCircle-1 0]'/NPointPerCircle;
sina=sin(alpha);
cosa=cos(alpha);
for i = 1:length(objects)
    Coord = T.Seg(objects(i)).Coord;
    switch T.SubType
        case {'line','polygon'}
        case 'rectangle'
            Coord=[Coord(1:2);Coord([1 4]);Coord(3:4);Coord([3 2]);Coord(1:2)];
        case 'circle'
            Coord=[Coord(1)+Coord(3)*sina Coord(2)+Coord(3)*cosa];
        case 'donut'
            Coord=[Coord(1)+Coord(4)*sina Coord(2)+Coord(4)*cosa
                NaN NaN
                Coord(1)+Coord(3)*sina Coord(2)+Coord(3)*cosa];
    end
    if cellData
        Data{i} = Coord;
    else
        t1 = size(Coord,1);
        Data(offset+(1:t1),:) = Coord;
        offset = offset+t1+1;
    end
end


function Local_write_file(varargin)
j=0;
RemoveLengthOne=0;
XYSep=0;
BNA=0;
filename='';
for i=1:nargin
    if ischar(varargin{i}) && strcmp(varargin{i},'-1')
        RemoveLengthOne=1;
    elseif ischar(varargin{i}) && strcmp(varargin{i},'BNA')
        BNA=1;
    elseif ischar(varargin{i}) && isempty(filename)
        filename=varargin{i};
    elseif j==0
        Data1=varargin{i};
        j=j+1;
    elseif (isnumeric(varargin{i}) || iscell(varargin{i})) && j==1
        Data2=varargin{i};
        XYSep=1; % x and y supplied separately?
    else
        error('Invalid input argument %i',i+2)
    end
end

if isempty(filename)
    [fn,fp]=uiputfile('*.*');
    if ~ischar(fn),
        return
    end
    filename=[fp fn];
end

if isnumeric(Data1) % convert to column vectors
    if XYSep
        Data1=Data1(:);
        Data2=Data2(:);
    else
        if size(Data1,2)~=2 % [x;y] supplied
            Data1=transpose(Data1);
        end
    end
end

if iscell(Data1)
    j=0;
    for i=1:length(Data1)
        if XYSep
            Length=length(Data1{i}(:));
        else
            if size(Data1{i},2)~=2
                Data1{i}=transpose(Data1{i});
            end
            Length=size(Data1{i},1);
        end
        if ~(isempty(Data1{i}) || (RemoveLengthOne && Length==1)) % remove lines of length 0 (and optionally 1)
            j=j+1;
            T.Seg(j).ID = j;
            if XYSep
                T.Seg(j).Coord = [Data1{i}(:) Data2{i}(:)];
            else
                T.Seg(j).Coord = Data1{i};
            end
        end
    end
elseif ~isstruct(Data1)
    I=[0; find(isnan(Data1(:,1))); size(Data1,1)+1];
    j=0;
    for i=1:(length(I)-1)
        if I(i+1)>(I(i)+1+RemoveLengthOne) % remove lines of length 0  (and optionally 1)
            j=j+1;
            T.Seg(j).ID = j;
            if XYSep
                T.Seg(j).Coord = [Data1((I(i)+1):(I(i+1)-1)) Data2((I(i)+1):(I(i+1)-1))];
            else
                T.Seg(j).Coord = Data1((I(i)+1):(I(i+1)-1),:);
            end
        end
    end
else
    T=Data1;
end

fid=fopen(filename,'w','n','US-ASCII');
for j=1:length(T.Seg)
    if BNA
        if isfield(T.Seg(j),'ID1')
            id1=T.Seg(j).ID1;
            id2=T.Seg(j).ID2;
        else
            id1=num2str(T.Seg(j).ID);
            id2='';
        end
        fprintf(fid,'"%s","%s",%d\n',id1,id2,size(T.Seg(j).Coord,1));
    else
        if isfield(T.Seg(j),'ID')
            id=T.Seg(j).ID;
        else
            id=j;
        end
        fprintf(fid,'%d\n',id);
    end
    fprintf(fid,'%f, %f\n',transpose(T.Seg(j).Coord));
    if ~BNA
        fprintf(fid,'END\n');
    end
end
if ~BNA
    fprintf(fid,'END\n');
end
fclose(fid);
