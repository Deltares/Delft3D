function PS = plotstyles_logical(PS,A)
%PLOTSTYLES_LOGICAL Obtain plot styles for selected logical object.

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

if isempty(A.Grid)
    error('TODO: no grid')
else
    vDims = A.Value(1).Dimensions;
    gDims = A.Grid.SpatialDimensions;
    %
    [timdep,nval,stagger,PlotDim] = extract_dataprops(A,A.Value(1),vDims,gDims);
    %
    if nval>1
        error('Multi-valued logical not yet supported.')
    end
    switch PlotDim
        case '2DH'
            switch stagger
                case 'Edges2D'
                    PS = addPlotStyle(PS,'edges',timdep,@plot_edges);
                case 'Faces2D'
                    PS = addPlotStyle(PS,'blankout patches',timdep,@plot_unipatch);
            end
    end
end
