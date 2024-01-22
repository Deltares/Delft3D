function PS = plotstyles_vectorxy(PS,A)
%PLOTSTYLES_VECTORXY Obtain plot styles for selected vector(xy) object.

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
    if nval==1
        switch PlotDim
            case '2DH'
                switch stagger
                    case 'Faces2D'
                        % TODO: velocity profiles in 2DH (hull of vectors)
                        PS = addPlotStyle(PS,'vectors in cell centre',timdep,@plot_vectors);
                        F = qp_unitconversion(A.Unit,'m/s');
                        if ~ischar(F)
                            PS = addPlotStyle(PS,'moving vectors',true,@dummyPlotter);
                        end
                    case 'Edges2D'
                        PS = addPlotStyle(PS,'staggered normal vectors',timdep,@dummyPlotter);
                        PS = addPlotStyle(PS,'vectors in cell centre',timdep,@plot_vectors);
                        F = qp_unitconversion(A.Unit,'m/s');
                        if ~ischar(F)
                            PS = addPlotStyle(PS,'moving vectors',true,@dummyPlotter);
                        end
                end
        end
    end
end
