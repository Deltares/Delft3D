function PS = plotstyles_float(PS,A)
%PLOTSTYLES_FLOAT Obtain plot styles for selected float object.

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

level = strcmp(A.ValType,'level');
if isempty(A.Grid)
    vDims = A.Value(1).Dimensions;
    gDims = {};
    %
    [timdep,nval] = extract_dataprops(A,A.Value(1),vDims,gDims);
    %
    if nval==1
        PS = addPlotStyle(PS,'time-series',false,@dummyPlotter);
        PS = addPlotStyle(PS,'value',timdep,@dummyPlotter);
    end
else
    vDims = A.Value(1).Dimensions;
    gDims = A.Grid.SpatialDimensions;
    %
    [timdep,nval,stagger,PlotDim] = extract_dataprops(A,A.Value(1),vDims,gDims);
    %
    if nval==1 %singlevalue
        switch PlotDim
            case {'2DH','2DV'}
                switch stagger
                    case {'Faces2D','Nodes2D','HFaces3D','Voxels3D','VEdges3D'}
                        % space-space plot
                        % TODO: contour lines: alternating line styles, different line styles pos. & neg.
                        PS = addPlotStyle(PS,'patches',timdep,@plot_patches);
                        PS = addPlotStyle(PS,'contour lines',timdep,@dummyPlotter);
                        PS = addPlotStyle(PS,'contour patches',timdep,@dummyPlotter);
                        PS = addPlotStyle(PS,'continuous shades',timdep,@dummyPlotter);
                        PS = addPlotStyle(PS,'markers',timdep,@plot_markers);
                        if level
                            % space-space-level plot
                            PS = addPlotStyle(PS,'surface - mono colour',timdep,@dummyPlotter);
                            PS = addPlotStyle(PS,'surface - contour lines',timdep,@dummyPlotter);
                            PS = addPlotStyle(PS,'surface - contour patches',timdep,@dummyPlotter);
                            PS = addPlotStyle(PS,'surface - continuous shades',timdep,@dummyPlotter);
                            PS = addPlotStyle(PS,'markers3D',timdep,@dummyPlotter);
                        end
                end
            case '1DV'
                if timdep>2
                    % time-space plot (TIME x 1DV plane)
                    PS = addPlotStyle(PS,'time-space - patches',false,@dummyPlotter);
                    PS = addPlotStyle(PS,'time-space - contour lines',false,@dummyPlotter);
                    PS = addPlotStyle(PS,'time-space - contour patches',false,@dummyPlotter);
                    PS = addPlotStyle(PS,'time-space - continuous shades',false,@dummyPlotter);
                    PS = addPlotStyle(PS,'time-space - markers',false,@dummyPlotter);
                end
                % space-space-level plot (vertical line in 3D)
                PS = addPlotStyle(PS,'markers3D',timdep,@dummyPlotter);
                PS = addPlotStyle(PS,'1DV stick - continuous shades',timdep,@dummyPlotter);
                % value-level plot
                PS = addPlotStyle(PS,'1DV line',timdep,@dummyPlotter);
            case '1DH'
                if timdep>2
                    % time-space plot (TIME x 1DH plane)
                    PS = addPlotStyle(PS,'time-space - patches',false,@dummyPlotter);
                    PS = addPlotStyle(PS,'time-space - contour lines',false,@dummyPlotter);
                    PS = addPlotStyle(PS,'time-space - contour patches',false,@dummyPlotter);
                    PS = addPlotStyle(PS,'time-space - continuous shades',false,@dummyPlotter);
                    PS = addPlotStyle(PS,'time-space - markers',false,@dummyPlotter);
                    if level
                        % time-space-level plot (surf in TIME x 2DV)
                        PS = addPlotStyle(PS,'time-surf - mono colour',timdep,@dummyPlotter);
                        PS = addPlotStyle(PS,'time-surf - contour lines',timdep,@dummyPlotter);
                        PS = addPlotStyle(PS,'time-surf - contour patches',timdep,@dummyPlotter);
                        PS = addPlotStyle(PS,'time-surf - continuous shades',timdep,@dummyPlotter);
                        PS = addPlotStyle(PS,'time-surf - markers',timdep,@dummyPlotter);
                    end
                end
                % space-space plot (line in 2DH)
                PS = addPlotStyle(PS,'markers',timdep,@dummyPlotter);
                PS = addPlotStyle(PS,'values',timdep,@dummyPlotter);
                PS = addPlotStyle(PS,'1DH line - continuous shades',timdep,@dummyPlotter);
                if level
                    % space-space-level plot (3D line in 3D)
                    PS = addPlotStyle(PS,'markers3D',timdep,@dummyPlotter);
                    PS = addPlotStyle(PS,'values3D',timdep,@dummyPlotter);
                    PS = addPlotStyle(PS,'1D profile - continuous shades',timdep,@dummyPlotter);
                    % time-space-level plot (surf in TIME x 2DV)
                    PS = addPlotStyle(PS,'time-surf',false,@dummyPlotter);
                end
                % space-value plot
                PS = addPlotStyle(PS,'1DH line',timdep,@dummyPlotter);
            case '0D'
                % space-space plot (value in 2DH)
                PS = addPlotStyle(PS,'markers',timdep,@dummyPlotter);
                % tabular
                PS = addPlotStyle(PS,'value',timdep,@dummyPlotter);
                if level
                    % z-level in any value x LEVEL plot
                    PS = addPlotStyle(PS,'level-marker',timdep,@dummyPlotter);
                    if timdep
                        % time-space plot (line in TIME x 1DV)
                        PS = addPlotStyle(PS,'level-marker',timdep,@dummyPlotter);
                    end
                end
        end
    else % multivalue
        switch PlotDim
            case {'1DH','2DH'}
                PS = addPlotStyle(PS,'minihist',timdep,@dummyPlotter);
            case '0D'
                PS = addPlotStyle(PS,'hist',timdep,@dummyPlotter);
        end
    end
end
if timdep==3
    PS = addPlotStyle(PS,'timeline',false,@dummyPlotter);
else
    PS = addPlotStyle(PS,'timeline',timdep,@dummyPlotter);
end
