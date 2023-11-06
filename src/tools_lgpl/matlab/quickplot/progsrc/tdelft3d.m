function varargout = tdelft3d(varargin)
%TDELFT3D Conversion procedure for Delft3D date & time.
%
%   D = TDELFT3D(YYYYMMDD, HHMMSS)
%   D = TDELFT3D(ITDATE)
%   converts Delft3D date representation (integers YYYYMMDD for date and
%   HHMMSS for time) to serial date D. ITDATE is a vector of length 2 with
%   YYYYMMDD as first value and HHMMSS as second value.
%
%   [YR, MO, DY, HR, MN, SC] = TDELFT3D(YYYYMMDD, HHMMSS)
%   [YR, MO, DY, HR, MN, SC] = TDELFT3D(ITDATE)
%   converts Delft3D date representation (integers YYYYMMDD for date and
%   HHMMSS for time) to separate values for year, month, day, hour, minute,
%   second. ITDATE is a vector of length 2 with YYYYMMDD as first value and
%   HHMMSS as second value.
%
%   [YYYYMMDD, HHMMSS] = TDELFT3D(D)
%   ITDATE = TDELFT3D(D)
%   converts serial date or date vector D to Delft3D date representation
%   (integers YYYYMMDD for date and  HHMMSS for time). ITDATE is a vector
%   of length 2 with YYYYMMDD as first value and HHMMSS as second value.
%
%   [YYYYMMDD, HHMMSS] = TDELFT3D(YR, MO, DY, HR, MN, SC)
%   ITDATE = TDELFT3D(YR, MO, DY, HR, MN, SC)
%   converts separate values for year, month, day, hour, minute, second to
%   Delft3D date representation (integers YYYYMMDD for date and  HHMMSS for
%   time). ITDATE is a vector of length 2 with YYYYMMDD as first value and
%   HHMMSS as second value. The arguments HR, MN, SC are optional, they are
%   assumed equal to 0 if not specified.
%
%   See also DATE, DATENUM, DATEVEC.

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

IN = varargin;

if (nargin==1 && (numel(IN{1}) ~= 2) || any(IN{1}(:) ~= round(IN{1}(:)))) || ... % serial date, vectorized -- except for two integers which could be ITDATE ...
        (nargin==1 && numel(IN{1}) == 2 && (~isYYMMDD(IN{1}(1)) || ~ishhmmss(IN{1}(2)))) || ... % two integers
        (nargin==1 && ismatrix(IN{1}) && size(IN{1},2) >= 3) || ... % date vector, vectorized
        (nargin >= 3 && nargin <= 6) % separate values, vectorized
    % ... = TDELFT3D(D)
    % ... = TDELFT3D(YR, MO, DY, HR, MN, SC)

    if nargin >= 3
        dvec = [IN{:}];
    elseif numel(IN{1}) >= 3
        dvec = IN{1};
    else
        dvec = datevec(IN{1});
    end
    if length(dvec)<6
        dvec(6) = 0;
    end

    YYMMDD = dvec(1)*10000 + dvec(2)*100 + dvec(3);
    hhmmss = dvec(4)*10000 + dvec(5)*100 + floor(dvec(6));

    if nargout == 2
        % [YYYYMMDD, HHMMSS] = ...
        varargout = {YYMMDD, hhmmss};
    else
        % ITDATE = ...
        varargout = {[YYMMDD; hhmmss]};
    end

elseif (nargin == 1 && numel(IN{1}) == 2) || ... % ITDATE
        (nargin == 2 && numel(IN{1}) == numel(IN{2})) % YYYYMMDD, HHMMSS vectorized
    % ... = TDELFT3D(YYYYMMDD, HHMMSS)
    % ... = TDELFT3D(ITDATE)

    if nargin == 1
        YYMMDD = IN{1}(1);
        hhmmss = IN{1}(2);
    else
        YYMMDD = IN{1};
        hhmmss = IN{2};
    end

    Y = fix(YYMMDD / 10000);
    MMDD = YYMMDD - 10000*Y;
    M = fix(MMDD/100);
    D = MMDD - 100*M;
    
    h = fix(hhmmss/10000);
    mmss = hhmmss - 10000*h;
    m = fix(mmss/100);
    s = mmss - 100*m;

    if nargout <= 1
        % D = ...
        varargout = {datenum(Y, M, D, h, m, s)};
    else
        % [YR, MO, DY, HR, MN, SC] = ...
        varargout = {Y, M, D, h, m, s};
    end

else

    error('Invalid number of arguments, or arguments of invalid size. Check syntax.')

end

function ok = isYYMMDD(YYMMDD)
ok = false;
Y = fix(YYMMDD / 10000);
MMDD = YYMMDD - 10000*Y;
M = fix(MMDD/100);
D = MMDD - 100*M;
if M<12 && D<numDays(M) && YYMMDD == round(YYMMDD)
    ok = true;
end

function maxD = numDays(M)
maxDays = [31 29 31 30 31 30 31 31 30 31 30 31];
maxD = maxDays(M);

function ok = ishhmmss(hhmmss)
ok = false;
if hhmmss < 0 || hhmmss ~= round(hhmmss)
    return
end
h = fix(hhmmss/10000);
mmss = hhmmss - 10000*h;
m = fix(mmss/100);
s = mmss - 100*m;
if h < 24 && m < 60 && s <60
    ok = true;
end