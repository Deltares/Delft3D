function PS = plotstyles_none(PS,A)
%PLOTSTYLES_NONE Obtain plot styles for selected object without data.

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
    % no values, no grid
else
    vDims = A.Grid.X.Dimensions;
    gDims = A.Grid.SpatialDimensions;
    %
    [timdep,nval,stagger,PlotDim] = extract_dataprops(A,A.Grid.X,vDims,gDims);
    %
    switch PlotDim
        case '3D'
            PS = addPlotStyle(PS,'grid3D',timdep,@dummyPlotter);
        case '2DH'
            PS = addPlotStyle(PS,'grid',timdep,@plot_grid);
            % smoothness, orthogonality -> create float?
        case '2DV'
            PS = addPlotStyle(PS,'grid',timdep,@dummyPlotter);
        case '1DV'
            PS = addPlotStyle(PS,'gridline',timdep,@dummyPlotter);
        case '1DH'
            PS = addPlotStyle(PS,'gridline',timdep,@dummyPlotter);
        case '0D'
            PS = addPlotStyle(PS,'gridpoint',timdep,@dummyPlotter);
    end
end
