function varargout=landboundary(cmd,varargin)
%LANDBOUNDARY Read/write land boundary files.
%   XY = LANDBOUNDARY('read',FILENAME, ...) reads the data from the
%   specified land boundary file and returns the x and y coordinates as one
%   Nx2 array.
%
%   [X,Y] = LANDBOUNDARY('read',FILENAME, ...) returns the coordinates as
%   two separate Nx1 arrays X and Y.
%
%   The following optional arguments are supported for the read call:
%
%    * 'autocorrect'      : try to correct for some file format errors.
%    * 'nskipdatalines',N : skip the first N data lines. Comment lines
%                           (starting with *) are always skipped.
%
%   LANDBOUNDARY('write',FILENAME,XY, ...) writes a landboundary to file.
%   XY should either be a Nx2 array containing NaN separated line segments
%   or a cell array containing one line segment per cell. If XY is a 2xN
%   array with N not equal to 2, then XY is transposed.
%
%   LANDBOUNDARY('write',FILENAME,X,Y, ...) writes a landboundary to file.
%   X and Y may be supplied as separate Nx1 arrays containing NaN separated
%   line segments or cell arrays containing one line segment per cell. The
%   X and Y line segments should correspond in length.
%
%   The following optional arguments are supported for the write call:
%
%    * '-1'               : does not write line segments of length 1.
%    * 'dosplit'          : saves line segments as separate TEKAL blocks
%                           instead of saving them as one long line
%                           interrupted by missing values. This approach is
%                           well suited for spline files, but less suited
%                           for landboundaries with a large number of
%                           segments.
%    * 'format',FMT       : format for line segment names (default '%4i')
%
%   FILE = LANDBOUNDARY('write',...) returns a file info structure for the
%   file written. This structure can be used to read the file using the
%   TEKAL function.
%
%   See also TEKAL.

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
    case 'readc'
        varargout{1}=Local_read_file('cell',varargin{:});
    case 'read'
        Out=Local_read_file('plain',varargin{:});
        if nargout==1
            varargout{1}=Out;
        elseif nargout>1
            for i = 1:nargout-1
                varargout{i}=Out(:,i);
            end
            varargout{nargout}=Out(:,nargout:end);
        end
    case 'write'
        Out=Local_write_file(varargin{:});
        if nargout>0
            varargout{1}=Out;
        end
    otherwise
        uiwait(msgbox('unknown command','modal'));
end


function Data=Local_read_file(type,filename,varargin)
switch type
    case 'cell'
        Data=cell(0,1);
    case 'plain'
        Data=zeros(0,2);
end
if nargin<2
    [fn,fp]=uigetfile('*.ldb;*.pli;*.pliz');
    if ~ischar(fn)
        return
    end
    filename=[fp fn];
end

% By default switch on loaddata because we'll need all data immediately in
% the code below. Since we won't have to read the data twice, this will
% usually be faster. The only benefit of not reading the data is that this
% will create a lot of small arrays in memory for all line segments
% individually whereas we eventually need it all in one big array.
T=tekal('open',filename,'loaddata',varargin{:});

lasterr('')
try
    if isempty(T.Field)
        switch type
            case 'cell'
                Data = cell(0,1);
            case 'plain'
                Data = zeros(0,2);
        end
    else
        Sz=cat(1,T.Field.Size);
        if ~all(Sz(:,2)==Sz(1,2))
            error('The number of columns in the files is not constant.')
        end
        switch type
            case 'cell'
                if iscell(T.Field(1).Data) % e.g. fixed weir pliz file
                    Data = cat(1,T.Field.Data);
                else % most common case
                    Data = {T.Field.Data}';
                end
                for i = 1:length(Data)
                    Data{i}( (Data{i}(:,1)==999.999) & (Data{i}(:,2)==999.999) ,:)=NaN;
                end
            case 'plain'
                SzTot = sum(Sz(:,1)) + size(Sz,1) - 1;
                if iscell(T.Field(1).Data) % e.g. fixed weir pliz file
                    Field1 = T.Field(1).Data;
                    nColumns = length(Field1);
                    Data = cell(1,nColumns);
                    for c = 1:nColumns
                        cSz = size(Field1{c});
                        if iscell(Field1{c})
                            Data{c} = cell(SzTot,cSz(2));
                            offset = 0;
                            for i = 1:length(T.Field)
                                Data{c}(offset+(1:Sz(i,1)),:) = T.Field(i).Data{c};
                                offset = offset + Sz(i,1) + 1;
                            end
                        else
                            Data{c} = NaN(Sz(1),cSz(2));
                            offset = 0;
                            for i = 1:length(T.Field)
                                Data{c}(offset+(1:Sz(i,1)),:) = T.Field(i).Data{c};
                                offset = offset + Sz(i,1) + 1;
                            end
                        end
                    end
                    Data{1}( (Data{1}(:,1)==999.999) & (Data{1}(:,2)==999.999) ,:)=NaN;
                else % most common case
                    Data = NaN(SzTot, Sz(1,2));
                    offset = 0;
                    for i = 1:length(T.Field)
                        Data(offset+(1:Sz(i,1)),:) = T.Field(i).Data;
                        offset = offset + Sz(i,1) + 1;
                    end
                    Data( (Data(:,1)==999.999) & (Data(:,2)==999.999) ,:)=NaN;
                end
        end
    end
catch e
    fprintf(1,'ERROR: Error extracting landboundary from tekal file:\n%s\n',lasterr);
end


function TklFileInfo=Local_write_file(filename,varargin)

if nargin==1
    [fn,fp]=uiputfile('*.*');
    if ~ischar(fn)
        TklFileInfo=[];
        return
    end
    filename=[fp fn];
end

j=0;
RemoveLengthOne=0;
XYSep=0;
DoSplit=0;
CellData=0;
Format='%4i';
i=1;
while i<=nargin-1
    if ischar(varargin{i}) && strcmp(varargin{i},'-1')
        RemoveLengthOne=1;
    elseif ischar(varargin{i}) && strcmpi(varargin{i},'dosplit')
        DoSplit=1;
    elseif ischar(varargin{i}) && strcmpi(varargin{i},'names') 
        names=varargin{i+1};
        i=i+1;
    elseif ischar(varargin{i}) && strcmpi(varargin{i},'format') && i<nargin-1
        Format=varargin{i+1};
        i=i+1;
    elseif isnumeric(varargin{i}) && (j==0 || j==1)
        if j==0
           data1=varargin{i};
           j=1;
        else % j==1
            data2=varargin{i};
            XYSep=1; % x and y supplied separately?
        end
    elseif iscell(varargin{i}) && (j==0 || j==1) 
        CellData=1;
        if j==0
           Data1=varargin{i};
           j=1;
        else % j==1
            Data2=varargin{i};
            XYSep=1; % x and y supplied separately?
        end
    else
        error('Invalid input argument %i',i+2)
    end
    i=i+1;
end

if CellData
    Ncell = numel(Data1);
else
    Ncell = 1;
end

j=0;
for c = 1:Ncell
    if CellData
        data1 = Data1{c};
        if XYSep
            data2 = Data2{c};
        end
    end
    % convert to column vector if needed
    if XYSep
        if size(data1,1)~=numel(data1)
            data1 = data1(:);
        end
        if size(data2,1)~=numel(data2)
            data2 = data2(:);
        end
        if any(size(data1)~=size(data2))
            error('Number of x coordinates differs from number of y coordinates for array %i',c)
        end
    else
        if ndims(data1)>2
            error('Invalid size of data array %i.',c)
        elseif size(data1,2)==3
            if size(data1,1)==2
                data1 = data1';
            else % accept third column as z coordinate
            end
        elseif size(data1,2)~=2
            if size(data1,1)==2
                data1 = data1';
            else
                error('Invalid size of data array %i.',c)
            end
        end
    end
    if ~DoSplit
        I=[0;size(data1,1)+1];
        data1(isnan(data1))=999.999;
        if XYSep
            data2(isnan(data2))=999.999;
        end
    else
        % assume that data1 and data2 have NaN at the same indices
        I=[0; find(isnan(data1(:,1))); size(data1,1)+1];
    end
    for i=1:(length(I)-1)
        if I(i+1)>(I(i)+1+RemoveLengthOne)
            % remove lines of length 0 (and optionally 1)
            j=j+1;
            if j==1
                T.Field(1).Comments = { ...
                    '*column 1 = x coordinate'
                    '*column 2 = y coordinate'};
            end
            if exist('names','var')
                T.Field(j).Name=names{j};
            else
                T.Field(j).Name = sprintf(Format,j);
            end
            T.Field(j).Size = [I(i+1)-I(i)-1 2];
            if XYSep
                T.Field(j).Data = [data1((I(i)+1):(I(i+1)-1)) data2((I(i)+1):(I(i+1)-1))];
            else
                T.Field(j).Data = data1((I(i)+1):(I(i+1)-1),:);
            end
        end
    end
end

TklFileInfo=tekal('write',filename,T);
