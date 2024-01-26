function B = resourcemanager(cmd,key,A)
%RESOURCEMANAGER

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

persistent resourcelist
if isempty(resourcelist)
   resourcelist=cell(0,2);
end
%
% Determine whether key already exists in resourcelist
%
i = 1;
while i<=size(resourcelist,1)
   if isequal(key,resourcelist{i,1})
      break
   end
   i = i+1;
end
%
% Process command
%
switch cmd
   case 'store'
      %
      % Add or overwrite resource in list
      %
      if i>size(resourcelist,1)
         resourcelist{i,1} = key;
      end
      resourcelist{i,2} = A;
   case 'retrieve'
      %
      % Get resource from list
      %
      if i>size(resourcelist,1)
         B = expand(key);
         resourcelist{i,1} = key;
         resourcelist{i,2} = B;
      else
         B = resourcelist{i,2};
      end
   case 'remove'
      %
      % Remove resource from list
      %
      if i>size(resourcelist,1)
         resourcelist(i,:)=[];
      end
end
