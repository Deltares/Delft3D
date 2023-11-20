function Props=separators(Props)
%SEPARATORS Remove double sepators and separators at end of list.
%   PropOut = SEPARATORS(PropIn)
%   PropOut equals PropIn except for the fact that double separators
%   have been removed

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

if size(Props,2)>1
    Props=Props(:);
end
for i=length(Props):-1:1
    if ~isempty(strmatch('---',Props(i).Name))
        if i==size(Props,1) || ~isempty(strmatch('---',Props(i+1).Name)) || i==1
            Props(i,:)=[];
        end
    end
end
if ~isempty(Props) && ~isempty(strmatch('---',Props(1).Name))
    Props(1)=[];
end
