function aList2aStr = inlist(aStr,aList)
%INLIST Match cell arrays of strings.
%   I = INLIST(C1,C2) returns for every string in the cell array of strings
%   C1 the index of the matching string in the cell array of strings C2. If
%   the string does not occur in C2 then NaN is returned. If the string
%   occurs multiple times in C2 then the last index is returned.
%
%   See also STRMATCH.

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

if ~iscellstr(aStr) | ~iscellstr(aList)
   error('Cell array of strings required as input arguments.')
end

[bStr,aStr2bStr,bStr2aStr] = unique(aStr);
[bList,aList2bList] = unique(aList);

[c,ia,ib] = intersect(bStr,bList);
bList2bStr = zeros(1,length(bStr));
bList2bStr(ia) = ib;
iszero = bList2bStr==0;
bList2bStr(iszero) = 1;

aList2aStr = aList2bList(bList2bStr(bStr2aStr));
aList2aStr(iszero(bStr2aStr)) = NaN;
