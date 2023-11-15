function Out=insstruct(Base,i,Ins)
%INSSTRUCT Insert array.
%   In compiled mode the following line
%     Out=[Out(1:i-1);Ins;Out(i+1:end)];
%   gives the error
%     ERROR: CAT arguments are not consistent in structure field number.
%   if i==length(Out).
%   This routine is a workaround.

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

if length(Base)==1,
    Out=Ins;
elseif i==length(Base),
    Out=[Base(1:i-1);Ins];
elseif i==1,
    Out=[Ins;Base(i+1:end)];
else
    Out=[Base(1:i-1);Ins;Base(i+1:end)];
end
