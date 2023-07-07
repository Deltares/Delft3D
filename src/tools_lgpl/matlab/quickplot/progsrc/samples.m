function [x,y,z]=samples(cmd,varargin)
%SAMPLES Read/write sample data from file.
%   XYZ = SAMPLES('read',FILENAME) read the specified file and return the
%   data contained in it. In the classic case of a simple plain Delft3D
%   samples file contain three data columns, the function returns an Nx3
%   array with the data. Due to the algorithm used, the file may contain
%   any number of comments in MATLAB style, i.e. starting with %. As a
%   generalization beyond the classic samples file, the number of columns
%   may differ from 3.
%
%   [X,Y,Z] = SAMPLES('read',FILENAME) read the specified file and return
%   the samples in three separate Nx1 arrays.
%
%   SAMPLES('write',FILENAME,XYZ) write samples given in a Nx3 (or 3xN)
%   array to a samples file. Because of the automatic transpose option,
%   this syntax does not support any other number of data columns. Use
%   'writeraw' to write an M x N matrix as a file with M data columns
%   and N rows.
%
%   SAMPLES('write',FILENAME,X,Y,Z) 
%   SAMPLES('write',FILENAME,X1,X2,...,XM) write samples given in M arrays
%   of size Nx1 (or 1xN) to a samples file. Because of the automatic
%   transpose option, this syntax does not support any other number of
%   data columns.
%
%   SAMPLES('writeraw',FILENAME,MATRIX) write an M x N matrix as a file
%   with M data columns and N rows.
%
%   SAMPLES('writeraw',FILENAME,'format',FORMAT, MATRIX)
%   SAMPLES('write',FILENAME,'format',FORMAT, XYZ)
%   SAMPLES('write',FILENAME,'format',FORMAT, X,Y,Z)
%   SAMPLES('write',FILENAME,'format',FORMAT, X1,X2,...,XM) write the data
%   using the specified number format. The default number format used is
%   a fixed point %f. Here FORMAT should contain a single valid format
%   specification like '%15.7f', '%16.7e' or '%g' in which case the format
%   is used for all data columns, or the FORMAT should contain the same
%   format specifiers as there are data columns to be written; use this
%   option to vary the format used for the columns.

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

switch lower(cmd)
    case 'read'
        xyz=Local_read_samples(varargin{:});
        if nargout>1
            x=xyz(:,1);
            y=xyz(:,2);
            z=xyz(:,3);
        else
            x=xyz;
        end
    case {'write','writeraw'}
        if ~ischar(varargin{1}) || strcmpi(varargin{1},'format')
            filename = '?';
            i = 0;
        else
            filename = varargin{1};
            i = 1;
        end
        if nargin > i && strcmpi(varargin{i+1},'format')
            format = varargin{i+2};
            i = i+2;
        else
            format = '%f';
        end
        xyz = varargin(i+1:end);
        if length(xyz) == 1 % single matrix XYZ
            xyz = xyz{1};
            if strcmpi(cmd,'write')
                % backward compatible: consider flipping
                szd = size(xyz);
                if length(szd)>2
                    error('Invalid data argument; data must be provided as one matrix, or multiple vectors')
                end
                if szd(2) == 3
                    xyz = transpose(xyz);
                end
            end
        else % a number of vectors
            for d = 1:length(xyz)
                szd = size(xyz{d});
                if length(szd)>2 || all(szd ~= 1) || ~any(szd == 1)
                    error('Invalid data argument %i; data must be provided as a single matrix, or multiple vectors',d)
                else % row or column vector, make sure all data is in row vector form
                    if d == 1
                        N = length(xyz{d});
                    elseif length(xyz{d}) ~= N
                        error('Invalid data argument %i; all vectors should have equal length (%i)', N)
                    end
                    if szd(1) ~= 1
                        xyz{d} = transpose(xyz{d});
                    end
                end
            end
            xyz = cat(1,xyz{:});
        end
        Local_write_samples(filename,format,xyz);
    otherwise
        error('Unknown command: %s',var2str(cmd)) 
end

function xyz=Local_read_samples(filename,opt)
if (nargin==0) || strcmp(filename,'?')
    [fname,fpath]=uigetfile('*.xyz','Select sample file');
    if ~ischar(fname)
        xyz=zeros(0,3);
        return
    end
    filename=[fpath,fname];
end

readtype='default';
if nargin>1
    opt = lower(opt);
    switch opt
        case {'simple','struct'}
            readtype = opt;
        otherwise
            error('Invalid samples read option: %s',opt)
    end
end

if exist(filename)~=2
    error('Cannot open %s.',filename)
end
try
    xyz0 = load(filename);
    if isempty(xyz0)
        error('Empty samples set returned: is this a valid sample file?');
    end
    simplexyz = 1;
catch
    try
        xyz0 = asciiload(filename);
        simplexyz = 1;
    catch
        simplexyz = 0;
    end
end

if simplexyz
    %
    % relatively simple sample file without header, data alrady read by the
    % load or asciiload call above. We may have to add a bit of metadata.
    %
    switch readtype
        case {'simple','default'}
            xyz = xyz0;
        otherwise
            xyz.XYZ = xyz0;
            if iscell(xyz0)
                npar = 0;
                for c = 1:length(xyz0)
                    npar = npar+size(xyz0{c},2);
                end
            else
                npar = size(xyz0,2);
            end
            xyz.Params = cell(1,npar);
            for i=1:npar
                xyz.Params{i}=sprintf('Parameter %i',i);
            end
            xyz.FileType = 'samples';
            xyz.FileName = filename;
            %
            fid=fopen(filename,'r','n','US-ASCII');
            Line=fgetl(fid);
            xyz.Header = {};
            while ~isempty(Line) && Line(1)=='%'
                xyz.Header{end+1} = Line;
                Line=fgetl(fid);
            end
            fclose(fid);
            %
            if ~isempty(xyz.Header)
                for i = length(xyz.Header):-1:1
                    j = strfind(xyz.Header{i},'Run:');
                    if ~isempty(j)
                        xyz.Run = strtok(xyz.Header{i}(j+4:end));
                    end
                    j = strfind(xyz.Header{i},'Table:');
                    if ~isempty(j)
                        xyz.Table = strtok(xyz.Header{i}(j+6:end));
                    end
                    j = strfind(xyz.Header{i},'SWAN version:');
                    if ~isempty(j)
                        xyz.SWAN_version = strtok(xyz.Header{i}(j+13:end));
                    end
                    j1 = strfind(xyz.Header{i},'[');
                    j2 = strfind(xyz.Header{i},']');
                    if length(j1) == npar && length(j2) == npar
                        xyz.ParamUnits = repmat({''},1,npar);
                        for j = 1:npar
                            xyz.ParamUnits{j} = strtrim(xyz.Header{i}(j1(j)+1:j2(j)-1));
                        end
                        par = textscan(xyz.Header{i-1}(2:end),'%s');
                        xyz.Params = par{1};
                    end
                end
            end
    end
else
    %
    % So far unable to read the file. It probably contains some comments,
    % header or labels. Let's analyze the content in more detail; the
    % readtype is always forced to 'struct'
    %
    fid=fopen(filename,'r','n','US-ASCII');
    csv = false;
    try
        %
        % All non-empty lines starting with * or # are considered to be
        % header lines.
        %
        skiplines=0;
        Line=fgetl(fid);
        xyz.Header={};
        while ischar(Line) && ~isempty(Line) && (Line(1)=='*' || Line(1)=='#')
            skiplines=skiplines+1;
            xyz.Header{skiplines} = strtrim(Line(2:end));
            Line=fgetl(fid);
        end
        if ~ischar(Line)
            %
            % End of file read while skipping the header lines ...
            % ... fail because we didn't find any data.
            %
            fclose(fid);
            error('%s does not contain samples.',filename);
        end
        %
        % First non-header line ... check for column labels ...
        %
        Params = {};
        first_token = strtok(Line);
        if first_token(1) == '"'
            %
            % Column labels on one or more lines surrounded by double
            % quotes separated by spaces.
            %
            firstitem = strtok(Line);
            while firstitem(1)=='"'
                X = strsplit(Line,'"');
                for i = 1:2:length(X)
                    sep = strtrim(X{i});
                    if isequal(sep,',')
                        if i == 3
                            csv = true;
                        elseif csv
                            % OK, comma when expecting comma
                        else
                            error('Reading line: %s\nInconsistent separation cf column labels: initial column lables were separated by space or new line, but now comma encountered.',Line);
                        end
                    elseif isempty(sep)
                        if csv && i ~= length(X)
                            error('Reading line: %s\nInconsistent separation cf column labels:initial column labels were separated by comma, but now there is no comma.',Line);
                        else
                            % OK, no comma when expecting no comma
                        end
                    else
                        % separator not empty and not comma
                        error('Reading line: %s\nInvalid separator "%s" encountered.',Line,sep);
                    end
                end
                Params = [Params X(2:2:end)];
                skiplines=skiplines+1;
                Line=fgetl(fid);
                firstitem = strtok(Line);
            end
        else
            %
            % Column labels may occur on one line separated by commas.
            %
            try
                X = textscan(Line,' %[^,],','returnonerror',0);
                if length(X{1}) < 2
                    error('Too few columns for a sample file.')
                else
                    csv = true;
                    Params = X{1}';
                    skiplines=skiplines+1;
                    Line=fgetl(fid);
                end
            catch
            end
        end
        %
        % First data line ... may contain more data columns than the number
        % of column labels read (e.g. in case of a file without column
        % labels), or fewer (should we really support this?)
        %
        if csv
            X = textscan(Line,' %[^,],','returnonerror',0);
        else
            X = textscan(Line,' %[^ \t]','returnonerror',0);
        end
        n = length(X{1});
        if n<3
            error('Not enough values for sample data (X,Y,Value1,...)')
        end
        %
        % Second data line ... should contain the same number of data
        % columns.
        %
        Line=fgetl(fid);
        if csv
            X = textscan(Line,' %[^,],','returnonerror',0);
        else
            X  = textscan(Line,' %[^ \t]','returnonerror',0);
        end
        n2 = length(X{1});
        if n2~=n && ~feof(fid)
            error('Number of values per data line should be consistent.')
        end
        %
        % Correct any mismatch between the number of data columns and the
        % number of column labels on the label side ...
        %
        if length(Params)<n
            for i=n:-1:(length(Params)+1)
                Params{i}=sprintf('Parameter %i',i);
            end
        elseif length(Params)>n
            Params=Params(1:n);
        end
        fclose(fid);
    catch
        fclose(fid);
        rethrow(lasterror)
    end
    
    xyz.XYZ=asciiload(filename,'skiplines',skiplines,'comment','*');
    xyz.Params=Params;
    xyz.FileType='samples';
    xyz.FileName=filename;
    
    if strcmp(readtype,'simple')
        xyz = xyz.XYZ;
    end
end
%
if isstruct(xyz)
    xyz.X = [];
    xyz.Y = [];
    xyz.Time = [];
    for i = 1:length(xyz.Params)
        switch lower(xyz.Params{i})
            case {'longitude','lon','x','xp','x-coordinate','x coordinate','x_coordinate','x_gpp','distance','chainage'}
                xyz.X = i;
            case {'latitude' ,'lat','y','yp','y-coordinate','y coordinate','y_coordinate','y_gpp'}
                xyz.Y = i;
            case 'time'
                if i>1 && strcmpi(xyz.Params{i-1},'date')
                    xyz.Time = [i-1 i];
                else
                    xyz.Time = i;
                end
            case 'datetime'
                xyz.Time = i;
        end
    end
    if isempty(xyz.X) && isempty(xyz.Y)
        xyz.X = find(strcmp(xyz.Params,'Parameter 1'));
        if ~isempty(xyz.X)
           xyz.Y = find(strcmp(xyz.Params,'Parameter 2'));
        end
    end
    %
    if isempty(xyz.Time)
        if iscell(xyz.XYZ)
            xyz.nLoc  = size(xyz.XYZ{1},1);
        else
            xyz.nLoc  = size(xyz.XYZ,1);
        end
        xyz.Times = gettimestamp(xyz.Header);
    else
        crds = [xyz.X xyz.Y];
        iTime = [];
        %
        % Usually times will be sorted in the data file. If this is not the
        % case, we might want to sort them, but we haven't implemented that
        % yet.
        %
        if ~isempty(crds)
            sortloc = unique(xyz.XYZ(:,crds),'rows');
            nLoc = size(sortloc,1);
            nVal = size(xyz.XYZ,1);
            locations = xyz.XYZ(1:nLoc,crds); % alternatively use unique(...,'stable')
            if nVal/nLoc == floor(nVal/nLoc) && isequal(xyz.XYZ(:,crds),repmat(locations,[nVal/nLoc 1]))
                Times = gettimes(xyz.XYZ(1:nLoc:nVal,xyz.Time));
                iTime = cumsum(repmat((1:nLoc)'==1,[nVal/nLoc 1]));
            end
        end
        if isempty(iTime)
            [Times,idum,iTime] = unique(gettimes(xyz.XYZ(:,xyz.Time)),'stable');
            nLoc = hist(iTime,max(iTime));
        end
        %
        xyz.Times = Times;
        xyz.iTime = iTime;
        %
        if all(nLoc==nLoc(1))
            xyz.nLoc = nLoc(1);
        else
            xyz.nLoc = nLoc;
        end
    end
end


function Local_write_samples(filename,format,xyz)
nrows = size(xyz,1);
nfields = sum(format == '%');
if nfields == 1
    format = repmat([format ' '],1,nrows); % add space between values
    format = [format(1:end-1) '\n']; % remove last space, add line feed
elseif nfields == nrows
    if ~isequal(format(end-1:end),'\n')
        format = [format '\n']; % add line feed if missing
    end
else
    error('Invalid format specification; it contains %i fields, but 1 or %i expected.',nfields,nrows)
end

if strcmp(filename,'?')
    [fn,fp]=uiputfile('*.xyz');
    if ~ischar(fn)
        return
    end
    filename=[fp fn];
end
fid=fopen(filename,'wt','n','US-ASCII');
if fid<0
    error(['Could not create or open: ',filename])
end
fprintf(fid,format,xyz);
fclose(fid);


function Times = gettimes(Times)
if size(Times,2)==2 % gpp => 20140118 105120
    d = Times(:,1);
    s = Times(:,2);
else % swan => 20140118.105120
    d = floor(Times);
    s = round((Times - d)*1000000);
end
m = floor(d/100);
d = d - 100*m;
y = floor(m/100);
m = m - 100*y;
%
mn = floor(s/100);
s  = s - mn*100;
h = floor(mn/100);
mn = mn - h*100;
%
Times = datenum(y,m,d,h,mn,s);


function Time=gettimestamp(Cmnt)
Time=[];
if ~isempty(Cmnt)
    for i=1:length(Cmnt)
        [Tk,Rm]=strtok(Cmnt{i});
        if (length(Cmnt{i})>10) && strcmpi(Tk,'time')
            [a,c,err,idx]=sscanf(Rm,'%*[ :=]%i %i',2);
            if c==2
                yr = floor(a(1)/10000);
                mo = floor((a(1)-yr*10000)/100);
                dy = a(1)-yr*10000-mo*100;
                hr = floor(a(2)/10000);
                mn = floor((a(2)-hr*10000)/100);
                sc = a(2)-hr*10000-mn*100;
                Time=datenum(yr,mo,dy,hr,mn,sc);
            else
                try
                    [a,c,err,idx]=sscanf(Rm,'%*[ :=] %1c',1);
                    Time=datenum(Rm(idx-1:end));
                catch
                end
            end
        end
    end
end