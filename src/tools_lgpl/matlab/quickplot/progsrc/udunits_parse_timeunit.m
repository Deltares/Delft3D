function [dt,refdate,TZshift] = udunits_parse_timeunit(unit_str)
%UDUNITS_PARSE_TIMEUNIT
%   [DT,REFDATE,TZSHIFT] = UDUNITS_PARSE_TIMEUNIT(str) returns time unit DT
%   expressed in days, the reference date and time REFDATE, and time zone
%   shift TZSHIFT.
%
%   Example:
%       str = 'minutes since 1970-01-01 00:00:00.0 -05:00';
%       [dt,refdate,tzshift] = udunits_parse_timeunit(str);
%
%       returns
%
%       dt = 6.9444e-04
%       refdate = 719529
%       tzshift = -5

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

[unit1,unit2] = strtok(lower(unit_str));
if ~ischar(unit1)
    unit1='';
end
switch unit1
    case {'millisecond','milliseconds','millisec','millisecs','ms'}
        dt = 0.001;
    case {'second','seconds','sec','secs','s'}
        dt = 1;
    case {'minute','minutes','min','mins'}
        dt = 60;
    case {'hour','hours','hr','hrs','h'}
        dt = 3600;
    case {'day','days','d'}
        dt = 86400; % 24*3600
    case {'month','months'}
        dt = 365.242198781*24*3600/12;
    case {'year','years','yr','yrs'}
        dt = 365.242198781*24*3600;
    case {'common_year','common_years'}
        dt = 365          *24*3600;
    case {'leap_year','leap_years'}
        dt = 366          *24*3600;
    case {'Julian_year','Julian_years'}
        dt = 365.25       *24*3600;
    case {'Gregorian_year','Gregorian_years'}
        dt = 365.2425     *24*3600;
    otherwise
        error('Unable to interpret UDUNITs time unit "%s"',unit1)
end
dt = dt/86400;
%
refdate = sscanf(unit2,' since %d-%d-%d %d:%d:%f %d:%d',[1 8]);
if length(refdate)>=6
    if length(refdate)==8
        TZshift = refdate(7) + sign(refdate(7))*refdate(8)/60;
    elseif length(refdate)==7
        % this is actually not correct: report this and continue
        TZshift = refdate(7);
        TZformat = 'HH';
        if abs(TZshift)>24
            TZshift = fix(TZshift/100)+rem(TZshift,100)/60;
            TZformat = 'HHMM';
        end
        ui_message('error','Time zone format invalid in "%s", expecting HH:MM instead of %s',unit_str,TZformat)
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