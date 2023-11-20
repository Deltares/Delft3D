function setzcoord(h,z)
%SETZCOORD Sets the z coordinate.
%   SETZCOORD(H,Z) sets the z coordinate of the objects referred to by handles
%   H to the specified value Z.
%
%   See also SET.

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

for i = 1:length(h)
    if ~ishandle(h)
        continue
    end
    switch get(h(i),'type')
        case 'line'
            zdata = get(h(i),'xdata');
            set(h(i),'zdata',repmat(z,size(zdata)))
        case 'surface'
            zdata = get(h(i),'zdata');
            set(h(i),'zdata',repmat(z,size(zdata)))
        case {'text','light'}
            coord = get(h(i),'position');
            coord(3) = z;
            set(h(i),'position',coord)
        case 'patch'
            coord = get(h(i),'vertices');
            if ~isempty(coord)
                coord(:,3) = z;
                set(h(i),'vertices',coord)
            end
        otherwise
            % don't do anything for root, figure, axes, image, uicontrol, etc.
    end
end
