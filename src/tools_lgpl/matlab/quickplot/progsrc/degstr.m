function str = degstr(degfloat,format,varargin)
%DEGSTR Convert degree values to string representation.
%   STR = DEGSTR(DEG,'degree') converts a degree value to a string
%   representation of degrees and as appropriate minutes and seconds. The
%   second argument is optional: 'degree' is thefault representation type.
%   If DEG is an array then STR is a char matrix where each line is the
%   string represenation of one element of DEG. If DEG contains only whole
%   number the format will contain only the indication of the degrees; if
%   DEG contains only values that can be represented as whole degrees and
%   minutes then the seconds will be dropped; in all other cases, the
%   result will contain degrees, minutes and seconds.
%
%   STR = DEGSTR(DEG,'latitude') converts a degree value to a string
%   representation of a latitude.
%   STR = DEGSTR(DEG,'longitude') converts a degree value to a string
%   representation of a longitude.
%   STR = DEGSTR(DEG,'lonlat') converts two degree values to a string
%   representation of a longitude-latitude pair. If DEG is a Nx2 matrix
%   then STR is a char array with N rows where each line is the string
%   reprensentation of one pair of longitude and latitude values.
%
%   CELLSTR = DEGSTR(DEG,FORMAT,'cell') returns a cell string instead of a
%   char array. Each string corresponds to one value (pair).
%
%   Example:
%     degstr([35 -37.5])
%     %returns a char array
%         35°00'
%         -37°30'
%     degstr([3 -4],'lat')
%     %returns a char array
%         3° N
%         4° S
%     degstr([3.5 -3.75],'lon','cell')
%     %returns a cell array
%         '3°30' E'
%         '3°45' W'
%     degstr([3.5 -3.75; -6 3.81],'lonlat')
%     %returns a char array
%         (3°30'00'' E,3°45'00'' S)
%         (6°00'00'' W,3°48'36'' N)
%
%   See also: SPRINTF, DATESTR, TICK.

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

degminsec = true;
tocell    = {};
DecSep    = '.';
Language  = 'English';
if nargin<2
    shortformat = 'deg';
else
    if strcmp(format,'lonlat')
        shortformat = format;
    else
        shortformat = format(1:3);
    end
end
shortformat = lower(shortformat);
%
i = 1;
while i<= length(varargin)
    if ischar(varargin{i})
        switch lower(varargin{i})
            case 'cell'
                tocell = {'cell'};
            case {'dm','dms','degmin','degminsec'}
                degminsec = true;
            case {'dd','decdeg'}
                degminsec = false;
            case 'decsep'
                DecSep = varargin{i+1};
                i = i+1;
            case 'language'
                Language = varargin{i+1};
                i = i+1;
        end
    end
    i = i+1;
end
%
anyzero = any(abs(degfloat(:))<0.5/3600);
%
switch lower(Language)
    case 'english'
        WESN = {'West'  'East'  'South' 'North'};
    case 'dutch'
        WESN = {'West'  'Oost'  'Zuid'  'Noord'};
    case 'german'
        WESN = {'West'  'Ost'   'Süd'   'Nord'};
    case 'french'
        WESN = {'Ouest' 'Est'   'Sud'   'Nord'};
    case 'italian'
        WESN = {'Ovest' 'Est'   'Sud'   'Nord'};
    case 'spanish'
        WESN = {'Oeste' 'Este'  'Sur'   'Norte'};
    otherwise
        error('Unsupported language: %s',Language)
end
west  = WESN{1}(1);
east  = WESN{2}(1);
south = WESN{3}(1);
north = WESN{4}(1);
%
switch shortformat
    case 'lonlat'
        if size(degfloat,2)~=2
            error('In case of ''lonlat'' the first argument DEG should be a Nx2 array')
        end
        degfloat = reshape(degfloat',[1 size(degfloat')]);
        while 1
            I = abs(degfloat(1,2,:))>90;
            if ~any(I)
                break
            end
            degfloat(1,1,I) = degfloat(1,1,I)+180;
            degfloat(1,2,I) = (180-abs(degfloat(1,2,I))).*sign(degfloat(1,2,I));
        end
        degfloat(:,1,:) = clipperiodic(degfloat(:,1,:),360);
        s = sign(degfloat);
        s(:,2,:)=s(:,2,:)+3;
        dir = [west ' ' east south ' ' north];
        dir = dir(s+2);
        degfloat = abs(degfloat);
        if degminsec
            deg = floor(degfloat);
            degfloat = (degfloat-deg)*60;
        end
    case {'lon','lat'}
        degfloat = degfloat(:)';
        switch shortformat
            case 'lon'
                degfloat = clipperiodic(degfloat,360);
                dir = [west ' ' east ' '];
            case 'lat'
                iter=1;
                while 1
                    I = abs(degfloat)>90;
                    if ~any(I)
                        break
                    end
                    degfloat(I) = (180-abs(degfloat(I))).*sign(degfloat(I));
                end
                dir = [south ' ' north ' '];
        end
        s = sign(degfloat);
        dir = dir(s+2);
        degfloat = abs(degfloat);
        if degminsec
            deg = floor(degfloat);
            degfloat = (degfloat-deg)*60;
        end
    case 'deg'
        %dummy dir
        degfloat = clipperiodic(degfloat,360);
        sz = size(degfloat);
        sz(1) = 0;
        dir = zeros(sz);
        %
        degfloat = degfloat(:)';
        if degminsec
            deg = fix(degfloat);
            degfloat = abs((degfloat-deg)*60);
        end
    otherwise
        error('Unknown format: %s',format)
end
%
if degminsec
    min = floor(degfloat);
    degfloat = (degfloat-min)*60;
    sec = round(degfloat);
    i = sec==60;
    if any(i(:))
        sec(i) = 0;
        min(i) = min(i)+1;
        i = min==60;
        min(i) = 0;
        deg(i) = deg(i)+sign(deg(i)+0.01);
    end
    %
    if all(sec(:)==0)
        if all(min(:)==0)
            xformat = ['%i' char(176) ' %c'];
            data = cat(1,deg,dir);
        else
            xformat = ['%i' char(176) '%2.2i'' %c'];
            data = cat(1,deg,min,dir);
        end
    else
        xformat = ['%i' char(176) '%2.2i''%2.2i'''' %c'];
        data = cat(1,deg,min,sec,dir);
    end
else
    xformat = ['%g' char(176) ' %c'];
    data = cat(1,degfloat,double(dir));
end
%
switch shortformat
    case 'lonlat'
        str = sprintf(['(' xformat ',' xformat ')x'],data);
        if anyzero
            str = strrep(str,'  ',''); % remove spaces
        end
    case {'lon','lat'}
        str = sprintf([xformat 'x'],data);
        if anyzero
            str = strrep(str,'  ','');
        end
    case 'deg'
        str = sprintf([xformat(1:end-3) 'x'],data);
end
if ~degminsec && ~strcmp(DecSep,'.')
    str = strrep(str,'.',DecSep);
end
str = multiline(str(1:end-1),'x',tocell{:});

%--------------------------------------------------------------------------
function value = clipperiodic(value,period)
value = rem(value,period);
i = value<-period/2;
value(i) = value(i)+period;
i = value>period/2;
value(i) = value(i)-period;
