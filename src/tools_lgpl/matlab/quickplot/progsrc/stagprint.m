function Str = stagprint(Stagger,LocTopology)
%STAGPRINT Convert stagger name into string for printing.

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
    LocTopology = 'Struct2D+';
end
if ~isempty(strfind(LocTopology,'1D'))
    mesh = 'network';
elseif strcmp(LocTopology(1:2),'Un')
    mesh = 'mesh';
else
    mesh = 'grid';
end
switch Stagger
    case 'Nodes2D'
        Str = [mesh ' points (2D)'];
    case 'Edges2D'
        if isequal(mesh,'network')
            Str = [mesh ' edges (2D)'];
        else
            Str = [mesh ' cell interfaces (2D)'];
        end
    case 'Faces2D'
        Str = [mesh ' cells (2D)'];
    case 'Nodes3D'
        Str = [mesh ' points (3D)'];
    case 'VEdges3D'
        Str = ['vertical ' mesh ' cell edges (3D)'];
    case 'HEdges3D'
        Str = ['horizontal ' mesh ' cell edges (3D)'];
    case 'Edges3D'
        Str = [mesh ' cell edges (3D)'];
    case 'VFaces3D'
        if isequal(mesh,'network')
            Str = ['vertical ' mesh ' faces (3D)'];
        else
            Str = ['vertical ' mesh ' cell interfaces (3D)'];
        end
    case 'HFaces3D'
        Str = ['horizontal ' mesh ' cell interfaces (3D)'];
    case 'Faces3D'
        Str = [mesh ' cell interfaces (3D)'];
    case 'Voxels3D'
        Str = [mesh ' cells (3D)'];
    otherwise
        Str = 'unknown locations';
end
