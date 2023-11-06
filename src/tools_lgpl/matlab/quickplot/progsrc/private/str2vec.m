function vec=str2vec(str,varargin)
%STR2VEC Convert string into a vector.
%   V=STR2VEC(S) converts the string S containing a space separated list of
%   integers into a (numeric) vector V. The string may also contain one or
%   more colon operators to represent series of equidistant numbers.
%
%   ...,'%f') retrieves a list of floating point values.
%   ...,'%d') retrieves a list of integers (default).
%
%   ...,'range',[Min Max]) checks also whether all integers of V are within
%   the specified range. If the string starts with a colon, the Min value
%   is assumed to preceed, if the string ends with a colon, the Max value
%   is assumed to follow. That is, :2: equals Min:2:Max.
%
%   ...,'applylimit') replaces out of bounds values to the limit values
%   instead of producing an error.
%
%   See also VEC2STR, EVAL, NUM2STR.

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

INP=varargin;
valtyp='%d';
maxint=[];
ApplyLimit=0;
i=1;
while i<=length(INP)
    switch lower(INP{i})
        case {'%f','%d'}
            valtyp=lower(INP{i});
            i=i+1;
        case 'applylimit'
            ApplyLimit=1;
            i=i+1;
        case 'range'
            maxint=INP{i+1};
            i=i+2;
        otherwise
            error('Invalid input argument %i.',i+2)
    end
end

s=str;
n=0;
vec=[];
colonafter=[];
while 1
    [v,c,e,n]=sscanf(s,valtyp);
    if ~isempty(v)
        vec=[vec;v];
    end
    if n<=length(s)
        if isequal(s(n),':')
            if isempty(vec)
                if isempty(maxint)
                    error('String starts with colon.')
                else
                    vec=maxint(1);
                end
            elseif ~isempty(colonafter) && colonafter(end)==length(vec)
                error('Double colon encountered.')
            end
            colonafter(end+1)=length(vec);
            n=n+1;
        else
            error('Unexpected character: %s.',s(n))
        end
    else
        break
    end
    s=s(n:end);
end
if ~isempty(colonafter) && colonafter(end)==length(vec)
    if isempty(maxint)
        error('String ends with colon.')
    end
    vec(end+1)=maxint(2);
end
while ~isempty(colonafter)
    f1=colonafter(1);
    i=2;
    while (i<=length(colonafter)) && (colonafter(i)==f1+i-1)
        i=i+1;
    end
    i=i-1;
    if i==1
        % ... v1:v2 ...
        % f1--^  ^-- f1+1
        if ApplyLimit
            vec(f1+1)=min(vec(f1+1),maxint(2));
            vec(f1)=max(vec(f1),maxint(1));
        end
        vecin=(vec(f1):vec(f1+1))';
        vec=cat(1,vec(1:f1-1,1),vecin,vec(f1+2:end,1));
        colonafter=colonafter(2:end)+length(vecin)-2;
    elseif i==2
        % ... v1:v2:v3 ...
        % f1--^     ^-- f1+2
        if ApplyLimit
            if vec(f1+1)>0, % increasing
                vec(f1+2)=min(vec(f1+2),maxint(2));
                vec(f1)=max(vec(f1),maxint(1));
            else % decreasing
                vec(f1+2)=max(vec(f1+2),maxint(1));
                vec(f1)=min(vec(f1),maxint(2));
            end
        end
        vecin=(vec(f1):vec(f1+1):vec(f1+2))';
        vec=cat(1,vec(1:f1-1,1),vecin,vec(f1+3:end,1));
        colonafter=colonafter(3:end)+length(vecin)-3;
    else % i>2
        error('Series of multiple colons encountered in ''%s''.',str)
    end
end
if ApplyLimit
    vec=min(vec,maxint(2));
    vec=max(vec,maxint(1));
end
if ~isempty(maxint)
    if any(vec<maxint(1)) || any(vec>maxint(end))
        error('Out of range value encountered.')
    end
end
vec=vec';
