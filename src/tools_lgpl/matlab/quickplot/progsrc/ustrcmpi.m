function [I,IAll]=ustrcmpi(Str,StrSet,varargin)
%USTRCMPI Find a unique string.
%   INDEX = USTRCMPI(STR,STRSET) compares the string STR with the strings
%   in the string set STRSET and returns the INDEX of the string in the set
%   that best matches the string STR. The best match is determined by
%   checks in the following order:
%        1. exact match
%        2. exact match case insensitive
%        3. starts with Str
%        4. starts with Str case insensitive
%        5. matches start of Str (unique longest match)
%        6. matches start of Str case insensitive (unique longest match)
%   If no string is found or if there is no unique match, the function
%   returns -1.
%
%   INDEX = USTRCMPI(STR,STRSET,N) limits the checks to the first N cases.
%
%   [INDEX,ALLINDEX] = USTRCMPI(STR,STRSET) returns also the indices of all
%   matches when there are multiple.
%
%   See also STRCMP, STRCMPI, STRNCMP, STRNCMPI, STRMATCH.

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

if nargin<2
   error('Not enough input arguments.')
end
opt=1:6;
minlen=0;
i=1;
while i<=length(varargin)
    if strcmpi(varargin{i},'casematch')
        opt(opt==2*round(opt/2))=[];
    elseif strcmpi(varargin{i},'minmatchlen')
        i=i+1;
        minlen = varargin{i};
    elseif isnumeric(varargin{i})
        if numel(varargin{i})==1
            opt(opt>varargin{i})=[];
        else
            opt(~ismember(opt,varargin{i}))=[];
        end
    end
    i=i+1;
end

if (ischar(Str) && size(Str,1)>1) || (iscellstr(Str) && length(Str)>1) % multiple Str values
   if (ischar(StrSet) && size(StrSet,1)>1) || (iscellstr(StrSet) && length(StrSet)>1) % multiple StrSet values
      error('Either one of the two USTRCMPI arguments must be a single string.')
   else
      [I,IAll]=ustrcmpi(StrSet,Str);
      return
   end
else
   if ischar(StrSet) && ~iscellstr(StrSet)
      StrSet=cellstr(StrSet);
   end
end

nany=1;
if nany && ismember(1,opt)
    I=strcmp(Str,StrSet);
    nany = ~any(I);
end
if nany && ismember(2,opt)
    I=strcmpi(Str,StrSet);
    nany = ~any(I);
end
if length(Str)<minlen
    Str(minlen) = ' ';
end
if nany && ismember(3,opt)
    I=strncmp(Str,StrSet,length(Str));
    nany = ~any(I);
end
if nany && ismember(4,opt)
    I=strncmpi(Str,StrSet,length(Str));
    nany = ~any(I);
end
if nany && ismember(5,opt)
    I=double(I);
    LS = length(Str);
    for i=1:length(StrSet)
        L = length(StrSet{i});
        if L<LS && L>=minlen
            I(i)=strncmp(Str,StrSet{i},L)*L;
        end
    end
    nany = ~any(I);
    if ~nany
        m=max(I);
        I=I==m;
    end
end
if nany && ismember(6,opt)
    I=double(I);
    LS = length(Str);
    for i=1:length(StrSet)
        L = length(StrSet{i});
        if L<LS && L>=minlen
            I(i)=strncmpi(Str,StrSet{i},L)*L;
        end
    end
    nany = ~any(I);
    if ~nany
        m=max(I);
        I=I==m;
    end
end
if nany
    I=-1;
    IAll=[];
else
    IAll=find(I);
    if numel(IAll)==1
        I=IAll;
    else
        I=-1;
    end
end
