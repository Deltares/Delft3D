function Data=classic(A)
%CLASSIC Convert qp_data object to classic QUICKPLOT data structure.

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

switch length(A.Value)
   case 1
      Data.Val = A.Value.Data;
   case 2
      Data.XComp = A.Value(1).Data;
      Data.YComp = A.Value(2).Data;
   case 3
      Data.XComp = A.Value(1).Data;
      Data.YComp = A.Value(2).Data;
      Data.ZComp = A.Value(3).Data;
end
Dims = {A.Dimensions.Name};
i = strmatch('Time',Dims,'exact');
Data.Time = A.Dimensions(i).Values;
Data.Name  = A.Name;
Data.Units = A.Unit;
