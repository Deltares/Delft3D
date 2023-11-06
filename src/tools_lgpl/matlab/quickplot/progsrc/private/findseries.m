function bs=findseries(bool)
%FINDSERIES  Find series of nonzero elements.
%   I = FINDSERIES(X) returns an Nx2 array of pairs of indices referring to
%   the start and end of series of nonzero elements. The first column of I
%   contains the start index. The second column of I contains the end
%   index.
%
%   Example
%      X = [0 0 0 1 2 3 0 0 0 1 2 3 4 5 0];
%      I = findseries(X)
%
%   returns
%
%      I =
%           4     6
%          10    14
%
%   since X contains non-zero values at indices 4 thru 6 and 10 thru 14.
%
%   See also FIND.

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

ibool = find(bool(:));
if isempty(ibool)
    bs = zeros(0,2);
    return
end
istart = ibool(cat(2,1,find(diff(ibool')>1)+1));
iend = ibool(cat(2,find(diff(ibool')>1),length(ibool)));
bs = [istart iend];
