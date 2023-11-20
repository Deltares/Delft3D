function Dimensions = adddimension(Dimensions,Name,Description,Type,Unit,Values)
%ADDDIMENSION Add a dimension to a dimension list.

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

%% Define fields of Dimensions structure
% Possible values for Type are
%
% * 'discrete'
% * 'continuous'
% * 'discrete-time'
% * 'continuous-time'
%
if isstruct(Dimensions)
    previousDimensions = {Dimensions.Name};
    if ismember(Name,previousDimensions)
        error('Dimension %s defined twice.',Name)
    end
    i = length(Dimensions)+1;
else
    i = 1;
end
Dimensions(i).Name        = Name;
Dimensions(i).Description = Description;
Dimensions(i).Type        = Type;
Dimensions(i).Unit        = Unit;
Dimensions(i).Values      = Values;
