function [refdate,dt,TZshift] = nc_interpret_time_unit(unitStr)
%NC_INTERPRET_TIME_UNIT Interpret netCDF time unit string.
%   [REFDATE, DT, TZONE] = NC_INTERPRET_TIME_UNIT(UNITSTR) splits the
%   provided time unit string into a reference date, time step, and time
%   zone shift.

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

[unit1,unit2] = strtok(unitStr);
switch unit1
    case {'millisecond','milliseconds','millisec','millisecs','msec','ms'}
        dt = 0.001/86400;
    case {'second','seconds','sec','secs','s'}
        dt = 1/86400;
    case {'minute','minutes','min','mins'}
        dt = 1/1440;
    case {'hour','hours','hr','hrs','h'}
        dt = 1/24;
    case {'day','days','d'}
        dt = 1;
    case {'week'}
        dt = 7;
    case {'month','months','mon'}
        dt = 365.242198781/12;
    case {'year','years','yr','yrs'}
        dt = 365.242198781;
    case {'common_year','common_years'}
        dt = 365;
    case {'leap_year','leap_years'}
        dt = 366;
    case {'Julian_year','Julian_years'}
        dt = 365.25;
    case {'Gregorian_year','Gregorian_years'}
        dt = 365.2425;
    otherwise
        % this actually doesn't look like a time unit
        error('Unknown time unit: "%s".', unitStr)
end
%
% even though there is a space between the time and the time
% zone, this line supports the case in which a + or - of the
% time zone is directly attached to the time.
refdate = sscanf(unit2,' since %d-%d-%d%*1[ T]%d:%d:%f %d:%d',[1 8]);
if length(refdate)==1
    % possibly basic (condensed) format
    refdate = sscanf(unit2,' since %4d%2d%2d%*1[ T]%2d%2d%f %d:%d',[1 8]);
end
if length(refdate)>=6
    if length(refdate)==8
        % offset HH:MM
        TZshift = refdate(7) + sign(refdate(7))*refdate(8)/60;
    elseif length(refdate)==7
        % offset HH or HHMM
        TZshift = refdate(7);
        if abs(TZshift)>24
            TZshift = fix(TZshift/100)+unit2(TZshift,100)/60;
        end
    else
        TZshift = 0;
    end
    refdate = datenum(refdate(1:6));
elseif length(refdate)>=3
    refdate(6) = 0;
    refdate = datenum(refdate);
    TZshift = NaN;
else
    refdate = [];
    TZshift = NaN;
end