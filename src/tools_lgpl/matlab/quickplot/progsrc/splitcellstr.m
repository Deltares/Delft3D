function spliced = splitcellstr(cs,delim)
%SPLITCELLSTR Split cell string at delimiters.
%   CSTRMAT = SPLITCELLSTR(CSTR,DELIM) splits the lines of the cell string
%   CSTR at one of the characters in DELIM (the default delimiter is ',')
%   and returns a cell string matrix CSTRMAT where each row consists of one
%   string split of the original cell string split at a delimiter
%   character. The function works also if CSTR is a 2D char matrix instead
%   of a cell string.
%
%   Example:
%
%      s = {'This is','a simple example.'};
%      cs = splitcellstr(s,' ')
%
%   returns
%
%      cs = 
%          'This'    'is'        ''        
%          'a'       'simple'    'example.'
%
%   See also STRFIND, STRTOK.

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
   delim = ',';
end
ndelim = length(delim);

if iscellstr(cs)
   nlines = length(cs);
elseif ischar(cs) && ndims(cs)==2
   nlines = size(cs,1);
else
   error('First argument should be cell string or 2D char matrix.')
end

% Determine separator locations
delims = cell(nlines,1);
for i = 1:nlines
   if iscell(cs)
      Line = cs{i};
   else
      Line = cs(i,:);
   end
   if ndelim==1
      delims{i} = strfind(Line,delim);
   else
       delims{i} = find(ismember(Line,delim));
   end
end

% Count separators
nparts = cellfun('length',delims)+1;
if isempty(nparts)
    maxnparts = 0;
else
    maxnparts = max(nparts);
end

% Split into cell string matrix
if maxnparts==1 && iscellstr(cs)
   % Trivial case
   spliced = cs(:);
else
   spliced = repmat({''},nlines,maxnparts);
   for i = 1:nlines
      if iscell(cs)
         Line = cs{i};
      else
         Line = cs(i,:);
      end
      Delim  = delims{i};
      nDelim = length(Delim);
      if nDelim==0
         spliced{i,1} = Line;
      elseif nDelim==1
         spliced{i,1} = Line(1:Delim-1);
         spliced{i,2} = Line(Delim+1:end);
      else
         spliced{i,1} = Line(1:Delim(1)-1);
         for j = 2:nDelim
            spliced{i,j} = Line(Delim(j-1)+1:Delim(j)-1);
         end
         spliced{i,nDelim+1} = Line(Delim(nDelim)+1:end);
      end
   end
end
