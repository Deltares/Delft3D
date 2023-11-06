function varargout = tekal(cmd,varargin)
%TEKAL Read/write for Tekal files.
%   INFO = TEKAL('open',FILENAME, ...) reads the specified Tekal file. If
%   the FILENAME is not specified you will be asked to select a file
%   interactively. The returned structure contains the metadata of the
%   Tekal file.
%
%   The following optional arguments are supported for the read call:
%
%    * 'autocorrect'      : Try to correct for some file format errors.
%    * 'loaddata'         : Load data into structure while reading file.
%                           This creates a bigger INFO data structure, but
%                           prevents further file access during 'read'
%                           calls. Switched off by default.
%    * 'nskipdatalines',N : Skip the first N data lines. Comment lines
%                           (starting with *) are always skipped.
%    * 'sorted'           : Return data blocks not in read order, but in
%                           ASCII dictionary order of the data block names.
%
%   DATA = TEKAL('read',INFO,RECORD) reads the selected data record from
%   the specified file. The RECORD may be specified as integer (data block
%   number) or as block name (if that name is unique). In case RECORD is an
%   integer, you may select to read multiple records. In this case DATA
%   will be a cell array grouping all data blocks read.
%
%   INFO = TEKAL('write',FILENAME,DATA) writes the matrix DATA to a simple
%   plain Tekal file containing one data block called 'DATA'. The function
%   returns a structure INFO containing the metadata of the newly created
%   file.
%
%   NEWINFO = TEKAL('write',FILENAME,INFO, ...) writes a more complete new
%   Tekal file based on the information in the INFO structure. INFO should
%   be a structure (array) containing at least a field called Data, and
%   optional fields Name, Comments, and Format. This structure may be
%   nested inside another structure as a field called Field (see example,
%   and return argument of TEKAL('read',...) command. The default number
%   format used is '%.15g'; this can be overruled by means of the Format
%   field which may specify either a single valid format specification such
%   as '%15.7f' or '%16.7e', or string with as many format specifications
%   as values per data line.
%
%   Alternatively, INFO may be a structure containing fields X, Y, and
%   Values where X and Y are M x N matrices and Values is an M x N x NVal
%   array. The block name may be specified as Blckname instead of Name.
%
%   The following optional argument is supported for the write call:
%
%    * 'sorted'           : Write data blocks not in specified order, but
%                           in ASCII dictionary order of the data block
%                           names.
%
%   Example
%      % Data for block 1
%      INFO.Field(1).Name = 'Magic';
%      INFO.Field(1).Data = magic(5);
%      % Data for block 2
%      INFO.Field(2).Name = 'Random';
%      INFO.Field(2).Data = rand(8,5,3,2);
%      INFO.Field(2).Comments = {'Note we can handle nD arrays too.'};
%      % write file
%      OUT = tekal('write','test.tek',INFO);
%
%      Random = tekal('read',OUT,'Random');
%      size(Random) % returns [8 5 3 2]
%
%   See also LANDBOUNDARY, TEKAL2TBA, QPFOPEN, QPREAD.

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

if nargin==0
    if nargout>0
        varargout{1} = [];
    end
    return
end
switch lower(cmd)
    case 'open'
        varargout{1} = Local_open_file(varargin{:});
    case 'read'
        if ischar(varargin{1}) % tekal('read','filename', ... is interpreted as tekal('open' for backward compatibility
            varargout{1} = Local_open_file(varargin{:});
        else
            [varargout{1:nargout}] = Local_read_file(varargin{:});
        end
    case 'resample'
        varargout{1} = Local_resample(varargin{:});
    case 'write'
        varargout{1} = Local_write_file(varargin{:});
    otherwise
        error('Unknown command: %s',var2str(cmd))
end

function FileInfo=Local_resample(FileInfo,varargin)
distance = varargin{1};
Data = tekal('read',FileInfo);
if ~iscell(Data)
    Data = {Data};
end
for i = 1:length(Data)
    d = pathdistance(Data{i}(:,1),Data{i}(:,2));
    keep = true(size(d));
    keep(diff(d)==0) = false;
    n = max(1,round(d(end)/distance));
    di = d(end)*(0:n)/n;
    FileInfo.Field(i).Data = interp1(d(keep),Data{i}(keep,:),di);
    FileInfo.Field(i).Size = size(FileInfo.Field(i).Data);
end


function FileInfo=Local_open_file(varargin)
FileInfo.Check='NotOK';
TryToCorrect=0;
Sorted=0;
LoadData=0;
nSkipDataLines=0;
if nargin==0
    [fn,fp]=uigetfile('*.*');
    if ~ischar(fn)
        return
    end
    filename=[fp fn];
else
    filename=varargin{1};
end
INP=varargin(2:end);
i=1;
while i<=length(INP)
    if ~ischar(INP{i})
        error('Invalid argument - Expecting a string, but reading: %s',var2str(INP{i}))
    else
        switch lower(INP{i})
            case 'autocorrect'
                TryToCorrect=1;
            case 'loaddata'
                LoadData=1;
            case 'sorted'
                Sorted=1;
            case 'nskipdatalines'
                nSkipDataLines=INP{i+1};
                i=i+1;
            otherwise
                error('Unknown option: %s',INP{i})
        end
    end
    i=i+1;
end

variable=0;
fid=fopen(filename,'r','n','US-ASCII');
if fid<0
    error('Cannot open file ...')
end
%
FileInfo.FileName=filename;
FileInfo.FileType='tekal';
ErrorFound=0;
Cmnt={};

while 1
    line=fgetl(fid);
    while isempty(line)
        line=fgetl(fid);
    end
    % end of file check (or some other error such that no line was read)
    % fprintf(1,'>''%s''<\n',line);
    if ~ischar(line)
        break
    end

    % check if we are dealing with a non-Tekal binary file ...
    % Tabs (char 9) are acceptable!
    if feof(fid) && length(line)==1 && line==26 % EOF signal
        break
    elseif any(line<32 & line~=9) && ~TryToCorrect
        fclose(fid);
        error('Invalid line: %s',line)
    end

    % remove empty spaces
    line=deblank(line);
    % is comment or empty line
    if isempty(line)
    elseif line(1)=='*' % comment
        Cmnt{end+1,1}=line;
    elseif nSkipDataLines>0
        nSkipDataLines=nSkipDataLines-1;
    else
        % if not, it must be a variable name
        variable=variable+1;
        FileInfo.Field(variable).Name=deblank(line);
        FileInfo.Field(variable).Comments=Cmnt;
        % read data dimensions
        here=ftell(fid);
        line=fgetl(fid);
        if ~ischar(line)
            dim=[];
        else
            [dim,~,err]=sscanf(line,['%f' space],[1 inf]);
            if ~isequal(dim,round(dim)) || ~isempty(err)
                if TryToCorrect
                    dim = inf;
                    fprintf(1,'Expecting block size but reading: %s\nInterpreting as missing block size; trying to automatically detect block size.\n',line);
                    fseek(fid,here,-1);
                else
                    fclose(fid);
                    error('Error reading data dimensions from line: %s',line)
                end
            end
        end
        if ~isempty(dim)
            dim(dim==-999)=inf;
            FileInfo.Field(variable).Size=dim;
            if (length(dim)>2) && prod(FileInfo.Field(variable).Size(3:end))~=FileInfo.Field(variable).Size(1)
                % MN   n   M    should be interpreted as    MN   n   M   N (=MN/M)
                if prod(FileInfo.Field(variable).Size(3:end))>0 && rem(FileInfo.Field(variable).Size(1),prod(FileInfo.Field(variable).Size(3:end)))==0
                    FileInfo.Field(variable).Size(end+1)=FileInfo.Field(variable).Size(1)/prod(FileInfo.Field(variable).Size(3:end));
                else
                    Sz = FileInfo.Field(variable).Size;
                    error(['Field %i labelled ''%s''\n' ...
                        'Specified dimension: %s\n' ...
                        'Number of rows (%i) is not integer multiple of reshape dimensions (%s).'], ...
                    variable, ...
                    FileInfo.Field(variable).Name, ...
                    sprintf('%i ',Sz), ...
                    Sz(1), ...
                    deblank(sprintf('%i ',Sz(3:end))))
                end
            end
            if length(dim)>1
                dim=dim([2 1]);
            else
                % auto detect number of values per line
                here = ftell(fid);
                line = fgetl(fid);
                fseek(fid,here,-1);
                values = sscanf(line,'%f');
                nvalues = max(1,length(values));
                FileInfo.Field(variable).Size=[dim nvalues];
                dim=[nvalues dim];
            end
            FileInfo.Field(variable).ColLabels=getcollabels(dim(1),Cmnt);
            FileInfo.Field(variable).Time=gettimestamp(Cmnt);
            FileInfo.Field(variable).Offset=ftell(fid);

            % create Data field but don't use it
            % This field is created to be compatible with the write statement
            FileInfo.Field(variable).Data=[];
            FileInfo.Field(variable).ExtraData=[];
            % skip data values
            if prod(FileInfo.Field(variable).Size)==0
                FileInfo.Field(variable).DataTp='numeric';
            else
                line=fgetl(fid);
                fseek(fid,FileInfo.Field(variable).Offset,-1);
                [X,N,Err]=sscanf(line,['%f' space]);
                FileInfo.Field(variable).DataTp='numeric';
                if ~isempty(Err) && dim(1)==N+1 % annotation mode
                    FileInfo.Field(variable).DataTp='annotation';
                    if LoadData
                        Data = read_annotation(fid,dim);
                        FileInfo.Field(variable).Data=Data;
                    else
                        for i=1:dim(2)
                            fgetl(fid);
                        end
                    end
                else % all numerics
                    % check number of values per line,
                    if (length(FileInfo.Field(variable).Size)>1) && (N<dim(1))
                        Msg=sprintf('Actual number of values per line %i does not match indicated number\nof values per line %i.',N,dim(1));
                        if ~TryToCorrect
                            fclose(fid);
                            error(Msg)
                        end
                        fprintf(1,'ERROR: %s\nUsing actual number and trying to continue ...\n',Msg);
                        dim(1)=N;
                        FileInfo.Field(variable).Size(2)=N;
                    end
                    [Data,ErrorFound,ExtraData] = read_numeric(fid,FileInfo.Field(variable).Name,dim,TryToCorrect,variable);
                    if ErrorFound
                        break
                    end
                    if ~isfinite(dim(2));
                        dim(2)=floor(numel(Data)/dim(1));
                        FileInfo.Field(variable).Size(1)=dim(2);
                        if length(FileInfo.Field(variable).Size)>2
                            i = find(~isfinite(FileInfo.Field(variable).Size));
                            FileInfo.Field(variable).Size(i) = 1;
                            FileInfo.Field(variable).Size(i) = dim(2)/prod(FileInfo.Field(variable).Size(3:end));
                        end
                    end

                    if LoadData
                        % replace 999999 by Not-a-Numbers
                        Data(Data(:)==999999)=NaN;
                        
                        % reshape to match Size
                        if length(FileInfo.Field(variable).Size)>2
                            Data=reshape(Data,[FileInfo.Field(variable).Size(3:end) FileInfo.Field(variable).Size(2)]);
                        end
                        FileInfo.Field(variable).Data = Data;
                        if ~all(cellfun('isempty',ExtraData))
                            FileInfo.Field(variable).ExtraData = ExtraData;
                        end
                    end

                    % read closing end of line
                    fgetl(fid);
                end
            end
        else
            if ~TryToCorrect
                if ~ischar(line) % EOF
                    line = '';
                end
                fclose(fid);
                error('Cannot determine field size from "%s"',line)
            end
            % remove field
            FileInfo.Field(variable)=[];
            variable=variable-1;
        end
        Cmnt={};
    end
end
fclose(fid);
if ~isfield(FileInfo,'Field')
    if TryToCorrect
        FileInfo.Field=[];
    else
        error('File is empty: %s',FileInfo.FileName)
    end
end
if Sorted
    [dumNames,Idx] = sort({FileInfo.Field.Name});
    FileInfo.Field = FileInfo.Field(Idx);
end
if ~ErrorFound
    FileInfo.Check='OK';
end

function [Data,ExtraData]=Local_read_file(FileInfo,var)
% check whether the data has been read
if nargin<2 || strcmp(var,':') || isequal(var,0)
    % no var argument, 0 and ':' --> in all these cases select all fields
    % to read
    var=1:length(FileInfo.Field);
elseif ischar(var)
    Fields={FileInfo.Field.Name};
    varnr=ustrcmpi(var,Fields);
    if varnr<0
        fprintf('Cannot determine which field to read.\n');
        Data=[];
        ExtraData=[];
        return
    else
        var=varnr;
    end
end
Data = cell(1,length(var));
ExtraData = Data;
fid = -1;
try
    for i = 1:length(var)
        v = var(i);
        if isfield(FileInfo.Field(v),'Data') && ~isempty(FileInfo.Field(v).Data)
            Data{i} = FileInfo.Field(v).Data;
            if isfield(FileInfo.Field(v),'ExtraData') && ~isempty(FileInfo.Field(v).ExtraData)
                ExtraData{i} = FileInfo.Field(v).ExtraData;
            end
        else
            if fid<0
                fid=fopen(FileInfo.FileName,'r','n','US-ASCII');
            end
            fseek(fid,FileInfo.Field(v).Offset,-1);
            if length(FileInfo.Field(v).Size)>1
                dim=FileInfo.Field(v).Size([2 1]);
            else
                dim=FileInfo.Field(v).Size;
            end
            switch FileInfo.Field(v).DataTp
                case 'annotation'
                    data = read_annotation(fid,dim);
                otherwise %'numeric' % all numerics; use fscanf
                    %Data=fscanf(fid,'%f',dim);
                    [data,~,extradata] = read_numeric(fid,FileInfo.Field(v).Name,dim,0,v);
                    
                    % replace 999999 by Not-a-Numbers
                    data(data(:)==999999)=NaN;
                    
                    % reshape to match Size
                    if length(FileInfo.Field(v).Size)>2
                        data = reshape(data,[FileInfo.Field(v).Size(3:end) FileInfo.Field(v).Size(2)]);
                    end
            end
            Data{i} = data;
            ExtraData{i} = extradata;
        end
    end
catch
end
if fid>0
    fclose(fid);
end
if length(var)==1
    Data = Data{1};
    ExtraData = ExtraData{1};
end


function S = space
S = ['%*[ ,' char(9),']'];


function Data = read_annotation(fid,dim)
Data={};
for i=1:dim(2)
    line=fgetl(fid);
    Tkn=find(diff([0 ~ismember(line,[' ,' char(9)])])==1);
    Data{1}(:,i)=sscanf(line,['%f' space],dim(1)-1);
    Str=deblank(line(Tkn(dim(1)):end));
    if Str(1)=='''' && Str(end)==''''
        Str=Str(2:end-1);
        Str=strrep(Str,'''''','''');
    elseif Str(1)=='"' && Str(end)=='"'
        Str=Str(2:end-1);
        Str=strrep(Str,'""','"');
    elseif Str(1)=='/' && Str(end)=='/'
        Str=Str(2:end-1);
        Str=strrep(Str,'//','/');
    end
    Data{2}{i}=Str;
end
Data{1}=Data{1}';
Data{2}=Data{2}';


function [Data,ErrorFound,ExtraData] = read_numeric(fid,BlockName,dim,TryToCorrect,variable)
offset     = ftell(fid);
ErrorFound = 0;
try
    % read until problem occurs (hopefully the name of the next block or eof)
    Data = [];
    ExtraData = [];
    found = true;
    while found
        found = false;
        % unfortunately lines starting with tab and without any blanks
        % cannot be read by one textscan statement.
        for pre = 1:2
            if pre == 1
                prefix = '';
            else
                prefix = '%*[ \t]';
            end
            if ~isfinite(dim(2))
                d2 = textscan(fid,[prefix repmat('%f%*[ \t]',1,dim(1)-1) '%f%s'],'delimiter','','whitespace','');
            else
                d2 = textscan(fid,[prefix repmat('%f%*[ \t]',1,dim(1)-1) '%f%s'],dim(2)-size(Data,1),'delimiter','','whitespace','');
            end
            Data2 = cat(2,d2{1:dim(1)});
            if ~isempty(Data2)
                found = true;
                Data = cat(1,Data,Data2);
                ExtraData2 = strtrim(d2{end});
                ExtraData = cat(1,ExtraData,ExtraData2);
                if size(Data,1) == dim(2)
                    break
                end
            end
        end
        if size(Data,1) == dim(2)
            break
        end
    end
    if isfinite(dim(2)) && size(Data,1) ~= dim(2)
        error('Incomplete data block')
    end
    textscan_failed = 0;
catch
    textscan_failed = 1;
end
if textscan_failed
    fseek(fid,offset,-1);
    [Data,Nr]=fscanf(fid,['%f%*[ ,' char(9) char(13) char(10) ']'],dim);
    if Nr<prod(dim) && isfinite(dim(2)) % number read less than number expected (no auto-detect)
        %
        fseek(fid,offset,-1);
        [Data,Nr]=fscanf(fid,['%*[^',char(10),']%c'],dim(2));
        %
        if Nr<dim(2)-1
            Msg=sprintf('End of file reached while processing data field %i.\nData is probably damaged.',variable);
            if ~TryToCorrect
                fclose(fid);
                error(Msg)
            end
            fprintf(1,'ERROR: %s\n',Msg);
            ErrorFound=1;
        else
            EndData = ftell(fid);
            NBytes = EndData-offset;
            fseek(fid,offset,-1);
            Chars = fread(fid,[1 NBytes],'char=>char');
            Chars(Chars==char(13))=[];
            Chars = [' ' strrep(Chars,char(10),[' ',char(10)])];
            n = ['%*[^' char(13) char(10) ']'];
            p = ['%*[ ,' char(9) char(13) char(10) ']'];
            [Data,Nr,Er,Nxt]=sscanf(Chars,[repmat([p '%f'],1,dim(1)) n],dim);
            if Nr<prod(dim)
                irow = floor(Nr/dim(1))+1;
                if irow==1
                    rowFirst = Chars(1:Nxt-1);
                    rowStr = sscanf(Chars(2:end),n([1 3:end]),1);
                else
                    rowFirst=fliplr(sscanf(Chars(Nxt-1:-1:1),n([1 3:end]),1));
                    rowStr = [rowFirst sscanf(Chars(Nxt:end),n([1 3:end]),1)];
                end
                Msg=sprintf(['Field %i labelled ''%s''\n' ...
                    'Unable to interpret "%s" at character %i in data row %i:\n%s\n' ...
                    'Data field is probably damaged.'], ...
                    variable, ...
                    BlockName, ...
                    Chars(Nxt), ...
                    length(rowFirst)+1, ...
                    floor(Nr/dim(1))+1, ...
                    rowStr);
                if ~TryToCorrect
                    fclose(fid);
                    error(Msg)
                end
                fprintf(1,'ERROR: %s\nTrying to continue ...\n',Msg);
                ErrorFound=1;
            end
        end
    end
    
    % transpose data if it has two dimensions
    if length(dim)>1
        Data=Data';
    end
    ExtraData=repmat({''},size(Data,1),1);
end


function NewFileInfo=Local_write_file(filename,FileInfo,varargin)
fid=fopen(filename,'w','n','US-ASCII');
NewFileInfo.Check='NotOK';
if fid<0
    error('invalid filename')
end

Sorted=0;
i=1;
while i<=length(varargin)
    switch lower(varargin{i})
        case 'sorted'
            Sorted=1;
    end
    i=i+1;
end

if isstruct(FileInfo)
    NewFileInfo.FileName=filename;
    %
    if ~isfield(FileInfo,'Field') && (isfield(FileInfo,'Data') || isfield(FileInfo,'Values'))
        F2.Field = FileInfo;
        FileInfo = F2;
    end
    %
    for i=1:length(FileInfo.Field)
        if ~isfield(FileInfo.Field(i),'Name') || isempty(FileInfo.Field(i).Name)
            if isfield(FileInfo.Field(i),'Blckname') && ~isempty(FileInfo.Field(i).Blckname)
                FileInfo.Field(i).Name = FileInfo.Field(i).Blckname;
            else
                FileInfo.Field(i).Name = sprintf('BL%2.2i',i);
            end
        end
    end
    %
    if Sorted
        [dumNames,Idx] = sort({FileInfo.Field.Name});
        FileInfo.Field = FileInfo.Field(Idx);
    end
    %
    for i=1:length(FileInfo.Field)
        if isfield(FileInfo.Field(i),'Data') && ~isempty(FileInfo.Field(i).Data)
            Data = FileInfo.Field(i).Data;
        elseif isfield(FileInfo.Field(i),'Offset')
            Data = tekal('read',FileInfo,i);
        elseif isfield(FileInfo.Field(i),'X')
            Data = cat(3,FileInfo.Field(i).X,FileInfo.Field(i).Y,FileInfo.Field(i).Values);
        else
            Data = [];
        end
        %
        NewFileInfo.Field(i).Size=size(Data);
        if ndims(Data)>2
            NewFileInfo.Field(i).Size=[prod(NewFileInfo.Field(i).Size(1:end-1)) NewFileInfo.Field(i).Size(end) NewFileInfo.Field(i).Size(1:end-1)];
            Data=reshape(Data,NewFileInfo.Field(i).Size(1:2));
        end

        ncol=NewFileInfo.Field(i).Size(2);
        NewFileInfo.Field(i).ColLabels=getcollabels(ncol,{});
        if isfield(FileInfo.Field(i),'Comments')
            if isempty(FileInfo.Field(i).Comments)
                NewFileInfo.Field(i).Comments={};
            elseif ischar(FileInfo.Field(i).Comments)
                NewFileInfo.Field(i).Comments{1}=FileInfo.Field(i).Comments;
            else
                NewFileInfo.Field(i).Comments=FileInfo.Field(i).Comments;
            end
            for c=1:length(NewFileInfo.Field(i).Comments)
                if isempty(NewFileInfo.Field(i).Comments{c}) || NewFileInfo.Field(i).Comments{c}(1)~='*'
                    NewFileInfo.Field(i).Comments{c}=['*' NewFileInfo.Field(i).Comments{c}];
                end
                fprintf(fid,'%s\n',NewFileInfo.Field(i).Comments{c});
            end
            NewFileInfo.Field(i).ColLabels=getcollabels(ncol,NewFileInfo.Field(i).Comments);
        end

        NewFileInfo.Field(i).Name=FileInfo.Field(i).Name;
        fprintf(fid,'%s\n',FileInfo.Field(i).Name);

        fprintf(fid,' %i',NewFileInfo.Field(i).Size); fprintf(fid,'\n');
        NewFileInfo.Field(i).Offset=ftell(fid);

        if isfield(FileInfo.Field(i),'Format') && ~isempty(FileInfo.Field(i).Format)
            Format = FileInfo.Field(i).Format;
        else
            Format = '%.15g';
        end
        nperc  = sum(Format=='%');
        eol    = strfind(Format,'\n');
        if nperc==size(Data,2)
            if isempty(eol)
                Format=[Format '\n']; %#ok<AGROW>
            elseif length(eol)>1 || eol<max(nperc)
                fclose(fid);
                error('Invalid format "%s" specified for field %i.',Format,i)
            end
        elseif nperc==1
            if ~isempty(eol)
                fclose(fid);
                error('Invalid format "%s" specified for field %i.',Format,i)
            elseif length(NewFileInfo.Field(i).ColLabels)>1 && ...
                    strcmp(NewFileInfo.Field(i).ColLabels(1),'Date') && ...
                    strcmp(NewFileInfo.Field(i).ColLabels(2),'Time')
                Format=['%08i %06i' repmat([' ' Format],1,size(Data,2)-2) '\n'];
            else
                Format=[repmat([' ' Format],1,size(Data,2)) '\n'];
            end
        else
            fclose(fid);
            error('Invalid format "%s" specified for field %i.',Format,i)
        end
        
        if any(isnan(Data(:)))
            Data(isnan(Data))=-999;
        end
        fprintf(fid,Format,Data');

        NewFileInfo.Field(i).Data=[];
        NewFileInfo.Field(i).DataTp='numeric';
    end
    NewFileInfo.Check='OK';
else
    NewFileInfo.FileName=filename;
    fprintf(fid,'* This was created by Matlab at %s.\n',datestr(now));
    fprintf(fid,'DATA\n');
    NewFileInfo.Field.Name='DATA';
    NewFileInfo.Field.Size=size(FileInfo);
    if ndims(FileInfo)>2
        NewFileInfo.Field.Size=[prod(NewFileInfo.Field.Size(1:end-1)) NewFileInfo.Field.Size(end) NewFileInfo.Field.Size(1:end-1)];
        FileInfo=reshape(FileInfo,NewFileInfo.Field.Size(1:2));
    end
    fprintf(fid,' %i',NewFileInfo.Field.Size); fprintf(fid,'\n');
    NewFileInfo.Field.Offset=ftell(fid);
    NewFileInfo.Field.Data=[];
    NewFileInfo.Field.DataTp='numeric';
    fprintf(fid,[repmat(' %.15g',1,size(FileInfo,2)) '\n'],FileInfo');
    NewFileInfo.Check='OK';
end

fclose(fid);


function ColLabels=getcollabels(ncol,Cmnt)
ColLabels={}; % necessary for standalone version
ColLabels(1:ncol,1)={''};
if ~isempty(Cmnt)
    for i=1:length(Cmnt)
        [Tk,Rm]=strtok(Cmnt{i}(2:end));
        if (length(Cmnt{i})>10) && strcmpi(Tk,'column')
            [a,c,err,idx]=sscanf(Rm,'%i%*[ :=]%c',2);
            if (c==2) && a(1)<=ncol && a(1)>0
                ColLabels{a(1)}=deblank(Rm(idx-1:end));
            end
        end
    end
end


function Time=gettimestamp(Cmnt)
Time=[];
if ~isempty(Cmnt)
    for i=1:length(Cmnt)
        [Tk,Rm]=strtok(Cmnt{i}(2:end));
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