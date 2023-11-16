function [Out1,Out2] = noosfile(cmd,varargin)
%NOOSFILE Read MATROOS/NOOS time-series files.
%   FI = NOOSFILE('open',FILENAME) opens the specified and reads its
%   contents.
%
%   {Time,Val] = NOOSFILE('read',FI,N) returns the times and values from
%   series N.


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
    case 'open'
        if nargout>1
            error('Too many output arguments.')
        end
        Out1=File_open(varargin{:});
    case 'read'
        [Out1,Out2]=File_read(varargin{:});
    case 'write'
        error('write command not yet supported.')
        %Out1=File_write(varargin{:});
    otherwise
        error('Unknown command: %s',var2str(cmd))
end

function [Time,Val] = File_read(FI,i)
Time = FI.Series(i).times;
Val  = FI.Series(i).val;

function FI = File_open(filename)
fid = fopen(filename,'r','n','US-ASCII');
if fid<0
    error('Could not open file %s',filename)
end
try
    i = 1;
    anyheader = 0;
    while ~feof(fid)
        %
        % parse header
        %
        % #------------------------------------------------------
        % # Timeseries retrieved from the MATROOS series database
        % # Created at Mon Nov 19 12:07:20 CET 2007
        % #------------------------------------------------------
        % # Location    : hoekvanholland
        % # Position    : (4.120118,51.979135)
        % # Source      : observed
        % # Unit        : waterlevel
        % # Analyse time: most recent
        % # Timezone    : GMT
        % #------------------------------------------------------
        %
        Series(i).location = 'unspecified';
        Series(i).x = NaN;
        Series(i).y = NaN;
        Series(i).source = 'unspecified';
        Series(i).unit = 'unspecified';
        Series(i).analysis_time = 'unspecified';
        Series(i).timezone = 'MET';
        missval = [];
        %Series(i).header_offset = ftell(fid);
        %
        header = 1;
        while header && ~feof(fid)
            loc = ftell(fid);
            line = strtrim(fgetl(fid));
            if isempty(line)
                % skip empty lines?
            elseif line(1)=='#'
                anyheader = 1;
                [keyw,n,err,j]=sscanf(line(2:end),'%[^:=]%1[:=]',2);
                if n==2 % keyword line
                    val = strtrim(line(j+1:end));
                    switch lower(strtrim(keyw(1:end-1)))
                        case 'location'
                            Series(i).location = val;
                        case 'position'
                            loc = sscanf(val,'(%f,%f)',2);
                            if length(loc)==2
                                Series(i).x = loc(1);
                                Series(i).y = loc(2);
                            end
                        case 'source'
                            Series(i).source = val;
                        case 'unit'
                            Series(i).unit = val;
                        case 'analyse time'
                            tval = sscanf(val,'%f',1);
                            if isempty(tval)
                                Series(i).analysis_time = val;
                            else
                                Series(i).analysis_time = ymdhm(tval);
                            end
                        case 'timezone'
                            Series(i).timezone = val;
                        case 'missing val'
                            missval = sscanf(val,'%f',1);
                    end
                end
            else
                [values,n,err]=sscanf(line,'%f');
                if ~isempty(err)
                    fclose(fid);
                    error('Missing comment character or invalid data in line "%s".',line)
                elseif n~=2
                    error('Two numbers expected per data line, but reading "%s".',line)
                elseif i==1 && ~anyheader
                    fclose(fid);
                    error('NOOS file without header not supported.')
                end
                header = 0;
                fseek(fid,loc,-1);
                %Series(i).data_offset = loc;
                data = fscanf(fid,'%f',[2 inf]);
                data(1,:) = ymdhm(data(1,:));
                if isempty(missval) % default clip all values >= 1000
                    valid = abs(data(2,:))<1000;
                else
                    valid = data(2,:)~=missval;
                end
                Series(i).times = data(1,valid);
                Series(i).val = data(2,valid);
                i = i+1;
            end
        end
        if header
            % file ends with comment/header lines. Remove the incomplete data
            % block since it doesn't contain any data.
            Series(i)=[];
        end
    end
    fclose(fid);
    FI.FileName = filename;
    FI.FileType = 'NOOS';
    FI.Series = Series;
catch
    fclose(fid);
end

function date = ymdhm(data)
yr = floor(data/100000000);
mo = floor(data/1000000)-100*yr;
dy = floor(data/10000)-10000*yr-100*mo;
hr = floor(data/100)-1000000*yr-10000*mo-100*dy;
mn = floor(data)-100000000*yr-1000000*mo-10000*dy-100*hr;
date = datenum(yr,mo,dy, hr,mn,0);
