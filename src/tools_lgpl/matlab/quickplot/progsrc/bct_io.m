function Info=bct_io(cmd,varargin)
%BCT_IO Read/write boundary condition tables.
%   INFO = BCT_IO('READ',FILENAME) opens the specified file and read all
%   its time series into memory. The returning structure INFO contains a
%   Table structure that list the data of each time series stored in that
%   particular file.
%
%   BCT_IO('WRITE',FILENAME,INFO) writes the time-series data to the
%   specified file. The INFO structure should be similar to the one
%   obtained from a BCT_IO('OPEN',...) call. Each Table may contain in
%   addition a Format field specifying the number format of the data values
%   to be written to the file.
%
%   DATE = BCT_IO('TIMES',INFO,I) returns a vector DATE containing the
%   dates associated with table I of file INFO.
%
%   DATE = BCT_IO('TIMES',INFO,I,IDX) returns DATE(IDX) rather than the
%   full date vector.
%
%   INFO2 = BCT_IO('CLIP',INFO,T1,T2) clips all time series such that the
%   period of T1 until T2 is still covered. If T2 is missing, then only
%   time series segment that includes T1 is preserved.

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
    case 'clip'
        Info=Local_clip_bct(varargin{:});
    case 'times'
        Info=getTableTimes(varargin{:});
    case 'read'
        Info=Local_read_bct(varargin{:});
    case 'write'
        OK=Local_write_bct(varargin{:});
        if nargout>0
            Info=OK;
        elseif OK<0
            error('Error writing file')
        end
    otherwise
        error('Unknown bct_io command: ''%s''.',cmd)
end


function Info=Local_clip_bct(Info,t1,t2)
if nargin<3
    t2=t1;
end
for i=1:length(Info.Table)
    T=getTableTimes(Info,i);
    iKeep = T>=t1 & T<=t2;
    Keep = find(iKeep);
    if isempty(Keep)
        first = min(find(T>=t2));
        iKeep(first)=1;
        if first>1
            iKeep(first-1)=1;
        end
    else
        first = min(Keep);
        if T(first)>t1 && first>1
            iKeep(first-1)=1;
        end
        last = max(Keep);
        if T(last)<t2 && last<length(iKeep)
            iKeep(last+1)=1;
        end
    end
    Info.Table(i).Data = Info.Table(i).Data(iKeep,:);
end

function T=getTableTimes(Info,i,t)
if nargin<3
    t=':';
end
Tab=Info.Table(i);
switch length(Tab.ReferenceTime)
    case 0
        T0=0; % from model
    case 1
        T0=tdelft3d(Tab.ReferenceTime,0);
    case 2
        T0=tdelft3d(Tab.ReferenceTime(1),Tab.ReferenceTime(2));
end
switch Tab.TimeUnit
    case 'decades'
        f=10*365;
    case 'years'
        f=365;
    case 'days'
        f=1;
    case 'hours'
        f=24;
    case 'minutes'
        f=24*60;
    case 'seconds'
        f=24*60*60;
    case 'ddhhmmss'
    case {'date','absolute'}
end
T=T0+Tab.Data(t,1)/f;

function Info = Local_read_bct(filename)

if exist(filename,'file')~=2
    error('File does not exist: %s',filename)
end
fid=fopen(filename,'r','n','US-ASCII');
Info.Check='NotOK';
Info.FileName=filename;
Info.NTables=0;

floc=ftell(fid);
Line=fgetl(fid);
line=lower(Line);
switch deblank(line)
    case '[forcing]'
        Info = Local_read_bc(Info,fid);
        return
    case '[general]' %SOBEK-3 header, we cycle until we get to [boundary]; In FM-1D input there is [forcing]
        while ~strcmp(line,'[boundary]') && ~strcmp(line,'[forcing]')
            line=lower(fgetl(fid));
            if feof(fid)
                error('The file seems to be from SOBEK-3 or Delft3D FM 1D. I could not find the [boundary] or [forcing] block')
            end
        end
        Info = Local_read_bc(Info,fid);
        return
end
%
while ischar(Line) && ~isempty(Line) && Line(1)=='#'
    floc=ftell(fid);
    Line=fgetl(fid);
end

i=1;
NPar=0;
try
    while ~feof(fid)
        [keyw,remainder] = strtok(Line);
        switch keyw
            case 'table-name'
                Info.Table(i).Name = deblank(strextract(remainder));
            case 'contents'
                Info.Table(i).Contents = deblank(strextract(remainder));
            case 'location'
                Info.Table(i).Location = deblank(strextract(remainder));
            case 'time-function'
                Info.Table(i).TimeFunction=deblank(strextract(remainder));
            case 'reference-time'
                Info.Table(i).ReferenceTime=sscanf(remainder,'%i',[1 2]);
            case 'time-unit'
                Info.Table(i).TimeUnit=deblank(strextract(remainder));
            case 'interpolation'
                Info.Table(i).Interpolation=deblank(strextract(remainder));
            case 'parameter'
                NPar=NPar+1;
                [Info.Table(i).Parameter(NPar).Name,Part2]=strextract(remainder);
                Info.Table(i).Parameter(NPar).Name=deblank(Info.Table(i).Parameter(NPar).Name);
                Unit=strfind(Part2,'''');
                if isempty(Unit) % no quotes
                    Unit=strfind(lower(Part2),'unit');
                    if isempty(Unit) % no string 'unit'
                        % nothing to do, Part2 most likely already contains the unit string
                    else % remove 'unit'
                        Part2=Part2((Unit(1)+4):end);
                    end
                    Info.Table(i).Parameter(NPar).Unit=deblank(Part2);
                else
                    Part2=Part2(Unit(1):end);
                    Info.Table(i).Parameter(NPar).Unit=deblank(strextract(Part2));
                end
            case 'records-in-table'
                NRec=sscanf(remainder,'%i',1);
            otherwise
                if NPar==0
                    error('No parameter keywords found before data.')
                end
                fseek(fid,floc,-1);
                Info.Table(i).Data=transpose(fscanf(fid,'%f',[NPar inf]));
                i=i+1;
                NPar=0;
                Info.NTables=Info.NTables+1;
        end
        floc=ftell(fid);
        Line=fgetl(fid);
    end
catch
    fclose(fid);
    error('Error interpreting line:\n%s',Line)
end
fclose(fid);
if Info.NTables==0
    error('No tables in bct file?')
end
Info.Check='OK';


function Info = Local_read_bc(Info,fid)
i    = 1;
NPar = 0;
while ~feof(fid)
    floc = ftell(fid);
    %
    Line = fgetl(fid);
    ieq = strfind(Line,'=');
    if isempty(ieq)
        key = deblank(Line);
        if any(strcmpi(key,{'[boundary]','[lateraldischarge]','[forcing]'}))
            continue
        end
        Data = sscanf(Line,'%f',inf);
        if length(Data)==NPar
            % data
            fseek(fid,floc,-1);
            Data = fscanf(fid,'%f',[NPar inf]);
            Info.Table(i).Data = Data';
            Info.NTables=Info.NTables+1;
            
            %display
            %fprintf('Table for bc read: %s \n',Info.Table(i).Location)
            
            %update
            if isempty(Info.Table(i).Data)
                warning('Empty data. Probably an error in reading.')
            end
            i=i+1;
            NPar=0;
        else
            error('Unable to identify keyword-value pair on line: %s',Line)
        end
    else
        ieq = ieq(1);
        key = strtrim(Line(1:ieq-1));
        remainder = strtrim(Line(ieq+1:end));
    end
    switch lower(key)
        case 'name'
            % Name                            = Downstream
            Info.Table(i).Name = '';
            Info.Table(i).Contents = '';
            Info.Table(i).Location = remainder;
            Info.Table(i).TimeFunction = '';
            Info.Table(i).ReferenceTime = '';
            Info.Table(i).TimeUnit = '';
            Info.Table(i).Interpolation = '';
            Info.Table(i).Parameter = [];
            Info.Table(i).Data = [];
        case 'function'
            % Function                        = qhtable
            Info.Table(i).Contents = remainder;
        case 'time-interpolation'
            % Time-interpolation              = linear
            Info.Table(i).Interpolation = remainder;
        case 'quantity'
            % Quantity                        = qhbnd discharge
            % Quantity                        = qhbnd waterlevel
            % Quantity                        = time
            NPar=NPar+1;
            Info.Table(i).Parameter(NPar).Name=remainder;
        case 'unit'
            % Unit                            = m³/s
            % Unit                            = m
            % Unit                            = seconds since 2017-02-15 00:00:00
            Info.Table(i).Parameter(NPar).Unit=remainder;
            %
            if strcmp(Info.Table(i).Parameter(NPar).Name,'time')
                [Info.Table(i).TimeUnit, remainder] = strtok(remainder);
                Time = sscanf(remainder,' since %d-%d-%d %d:%d:%d');
                if length(Time) >= 3
                    Time(7) = 0;
                    Info.Table(i).ReferenceTime = [[10000 100 1]*Time(1:3) [10000 100 1]*Time(4:6)];
                else
                    error(['Unable to determine reference time from ' Info.Table(i).Parameter(NPar).Unit])
                end
            end
    end
end
%
fclose(fid);
Info.Check='OK';


function [Str1,remainder]=strextract(Str,Quote)
if nargin==1
    Quote='''';
end
Q=strfind(Str,Quote);
if isempty(Q)
    error('No quote found in ''%s''',Str)
end
if length(Q)<1
    error('Too few quotes found in ''%s''',Str)
end
Str = Str(Q(1)+1:end);
Q = Q(2:end)-Q(1);
%
i=1;
done=0;
while ~done
    if i<length(Q)
        if Q(i+1)==Q(i) % two quotes representing just one quote
            i=i+2;
        else
            done=1;
        end
    elseif i==length(Q)
        done=1;
    else
        warning('No ending quote found in string ''%s''.',Str)
        Str1=strrep(Str,[Quote Quote],Quote);
        remainder='';
        return
    end
end

Str1=strrep(Str(1:(Q(i)-1)),[Quote Quote],Quote);
remainder=Str((Q(i)+1):end);


function OK=Local_write_bct(filename,Info)
if ~ischar(filename) && nargin==1
    Info = filename;
    filename = Info.FileName;
end
fid=fopen(filename,'w','n','US-ASCII');
% When the file is written using a fixed line/record length this is
% shown in the first line
%fprintf(fid,'# %i\n',linelength);
parameter_mismatch = 0;
for i = 1:length(Info.Table)
    if isfield(Info.Table,'Name') && ~isempty(Info.Table(i).Name)
        fprintf(fid,'table-name          ''%s''\n',Info.Table(i).Name);
    end
    if isfield(Info.Table,'Contents') && ~isempty(Info.Table(i).Contents)
        fprintf(fid,'contents            ''%s''\n',Info.Table(i).Contents);
    end
    if isfield(Info.Table,'Location') && ~isempty(Info.Table(i).Location)
        fprintf(fid,'location            ''%s''\n',Info.Table(i).Location);
    end
    fprintf(fid,'time-function       ''%s''\n',Info.Table(i).TimeFunction);
    fprintf(fid,'reference-time       %i\n',Info.Table(i).ReferenceTime);
    fprintf(fid,'time-unit           ''%s''\n',Info.Table(i).TimeUnit);
    fprintf(fid,'interpolation       ''%s''\n',Info.Table(i).Interpolation);

    nval = size(Info.Table(i).Data,2);
    if ~parameter_mismatch && length(Info.Table(i).Parameter)>nval
        warning('bct_io:too_many_parameters', ...
            'More parameters defined than columns in the data set. Verify the correctness of the file written!')
        parameter_mismatch = 1;
    elseif ~parameter_mismatch && length(Info.Table(i).Parameter)<nval
        warning('bct_io:too_few_parameters', ...
            'More columns in the data set than parameters defined. Verify the correctness of the file written!')
        parameter_mismatch = 1;
    end
    for j=1:nval
        if j <= length(Info.Table(i).Parameter)
            fprintf(fid,'parameter           ''%s'' unit ''%s''\n', ...
                Info.Table(i).Parameter(j).Name, ...
                Info.Table(i).Parameter(j).Unit);
        else
            fprintf(fid,'parameter           ''unknown'' unit ''unknown''\n');
        end
    end

    fprintf(fid,'records-in-table     %i\n',size(Info.Table(i).Data,1));
    if isfield(Info.Table(i),'Format') && ~isempty(Info.Table(i).Format)
       fmt = Info.Table(i).Format;
    else
       fmt = '';
    end

    nperc = length(strfind(fmt,'%'));
    if nperc==0
       fmt   = '%15.7e';
       nperc = 1;
    end
    if nperc==1
       fmt = [repmat([' ',strtrim(fmt)],1,nval),'\n'];
       fmt = fmt(2:end);
    elseif nperc==nval
       fmt = [Info.Table(i).Format,'\n'];
    elseif nperc>nval
       perc = strfind(fmt,'%');
       fmt = [deblank(fmt(1:perc(nval+1)-1)),'\n'];
    else % nperc<nval
       perc = strfind(fmt,'%');
       lastfmt = deblank(fmt(perc(end):end));
       fmt = [deblank(fmt),repmat([' ',lastfmt],1,nval-nperc),'\n'];
    end
    fprintf(fid,fmt, transpose(Info.Table(i).Data));
end
fclose(fid);
OK=1;
