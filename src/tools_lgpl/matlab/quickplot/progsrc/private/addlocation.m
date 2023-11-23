function Locations = addlocation(Locations,Name,Topology,Dimensions, ...
    TimeDimensions,Group,XCoord,YCoord,ZCoords)
%ADDLOCATION Add a location to a location list.

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

%% Define name
if isempty(Locations)
    l = 1;
else
    l = length(Locations)+1;
end
if ~ischar(Name)
    error('Non-character topology name encountered.')
end
Locations(l).Name            = Name;

%% Define topology and spatial dimensions
% Possible values for Topology are
%
% * *Struct3D* The spatial dimension field should contain a cell string
% with exactly 9 strings indicating the number of cells and grid points in
% the three grid directions and the relation of the cell and grid point
% numbering: M cells, M points, M numbering type, N cells, N points, N
% numbering type, K cells, K points and K numbering type. See below for a
% description of the numbering types.
% * *Struct2D+* The spatial dimension field should contain a cell string
% with 6 plus k times 3 strings indicating: M cells, M points, M numbering
% type, N cells, N points, N numbering type and for each of the k vertical
% dimensions a triple indicating K cells, K points and K numbering type.
%
% * *Unstruct3D* The spatial dimension field should contain a cell string
% with exactly 4 strings indicating the names of the following dimensions:
% number of voxels, number of faces, number of edges, number of nodes.
% * *Unstruct2D+* The spatial dimension field should contain a cell string
% with 3 plus k times 3 strings indicating the names of the following
% dimensions: number of faces (2D plane), number of edges (2D plane),
% number of nodes (2D plane) and for each of the k vertical dimensions a
% triple indicating K cells, K points and K numbering type.
% * *Unstruct1D+* The spatial dimension field should contain a cell string
% with 2 plus k times 2 strings indicating the names of the following
% dimensions: number of edges (2D plane), number of nodes (2D plane) and
% for each of the k vertical dimensions a triple indicating K cells, K
% points and K numbering type.
% * *Unstruct0D+* The spatial dimension field should contain a cell string
% with 1 plus k times 2 strings indicating the names of the following
% dimensions: number of nodes (2D plane) and  for each of the k vertical
% dimensions a triple indicating K cells, K points and K numbering type.
%
% *Structured dimension numbering type*
% For structured dimensions (vertical dimensions of Unstruct2D+,
% Unstruct1D+ and Unstruct0D+ topologies and all dimensions of the
% structured Struct2D+ and Struct3D topologies) you need to specify the
% relationship between the dimension of the cell and grid point dimensions.
% Four relationships are currently supported:
%
% * *WholeCells* Only whole cell are defined: there is one cell less than
% there are grid points.
% * *DanglingLowCell* The number of cells is equal to the number of grid
% points with one 'dangling' halve grid cell on the lower end.
% * *DanglingHighCell* The number of cells is equal to the number of grid
% points with one 'dangling' halve grid cell on the high end.
% * *ExtendedCells* The number of cells is one larger than the number of
% grid points: the cells extend at both ends beyond the grid.
%
Topologies = ...
    {'Struct3D'     'is'    9
    'Struct2D+'     'vdims' 6
    'Unstruct3D'    'is'    4
    'Unstruct2D+'   'vdims' 3
    'Unstruct1D+'   'vdims' 2
    'Unstruct0D+'   'vdims' 1};
iTopo = strmatch(Topology,Topologies(:,1),'exact');
if isempty(iTopo)
    error('Invalid topology for %s: %s.',Name,Topology)
end
Locations(l).Topology          = Topology;
%
nDims = length(Dimensions);
if ~iscellstr(Dimensions)
    error('Invalid spatial dimension list for %s.',Name)
end
%
nMinDims = Topologies{iTopo,3};
switch Topologies{iTopo,2}
    case 'is'
        dimsOkay = nDims==nMinDims;
    case 'vdims'
        dimsOkay = nDims>=nMinDims && ...
            round((nDims-nMinDims)/3)==(nDims-nMinDims)/3;
end
if ~dimsOkay
    error('Invalid number of dimensions specified for %s topology.',Topology)
end
%
if isequal(Topology(1),'S')
    StructDimTypes = 3:3:nDims;
else
    StructDimTypes = nMinDims+3:3:nDims;
end
for i = StructDimTypes
    if ~ismember(Dimensions{i}, ...
            {'WholeCells','DanglingLowCell','DanglingHighcell','ExtendedCells'})
        error('Invalid structured dimension numbering type ''%s''.',Dimensions{i})
    end
end
%
Locations(l).SpatialDimensions = Dimensions;

%% Define time dimension(s)
% The time dimension should be either empty (time independent) location
% definition, the name of a time dimension or optionally a cell array of
% time dimensions.
%
if isempty(TimeDimensions)
    TimeDimensions = {};
elseif ischar(TimeDimensions)
    TimeDimensions = {TimeDimensions};
elseif ~iscellstr(TimeDimensions)
    error('Invalid time dimension for %s',Name)
end
Locations(l).TimeDimensions    = TimeDimensions;

%% Define the location group
% Locations with identical group numbers represent the individual domains
% of a multi-domain simulation.
%
if ~isnumeric(Group) || ~isequal(size(Group),[1 1])
    error('Invalid location group for %s.',Name)
end
Locations(l).Group             = Group;

%% Define the coordinates
% Store the quantities associated with the 3 coordinate directions. For
% most applications both X and Y coordinates will be defined; only for 1DV
% and 2DV data sets there may be no X and Y coordinates or only an X
% coordinate defined. There may be no Z coordinate (in case of a 2DH data
% set) or multiple Z coordinates (in case of multiple distinct vertical
% grids). The fact that there may be multiple Z coordinates is reflected by
% the name of the local variable but it is not reflected in the field of
% the locations structure.
%
% There may be cases where no coordinates are available at all. This may
% only occur in case of Unstruct0D+ topologies without vertical "+"
% dimension.
%
Locations(l).XCoord = XCoord;
Locations(l).YCoord = YCoord;
Locations(l).ZCoord = ZCoords;

%% Define the default stagger location
% The default stagger location is used to determine the index dimensions to
% be used when selecting a subsection of the overall domain. Possible
% stagger locations are
%
% *Struct3D*, *Unstruct3D*
%
% * 3D locations: _Nodes3D_, Edges3D(3), Faces3D(3), _Voxels3D_
%
% *Struct2D+*, *Unstruct2D+*
%
% * 2D locations: _Nodes2D_, Edges2D(2), _Faces2D_
% * 3D locations: _Nodes3D_, _VEdges3D_, HEdges3D(2), Edges3D(3),
% VFaces3D(2), _HFaces3D_, Faces3D(3), _Voxels3D_
%
% *Unstruct1D+*
%
% * 2D locations: _Nodes2D_, _Edges2D_
% * 3D locations: _Nodes3D_, _VEdges3D_, _HEdges3D_, Edges3D, _VFaces3D_
%
% *Unstruct0D+*
%
% * 2D locations: _Nodes2D_
% * 3D locations: _Nodes3D_, _VEdges3D_
%
% For the default stagger location only the italic stagger locations may be
% used because only these uniquely define indexing for horizontal and (if
% appropriate) vertical dimensions. The 2D versions may only be used for 2D
% topologies; the 3D versions may only be used for 3D topologies. Some
% stagger locations of Struct3D and Struct2D+ topologies have have
% subtypes; the number of subtypes ("directions") has been indicated in the
% table in parentheses.
%
% Quantities defined at a certain stagger location may be indexed using the
% indices of a different stagger location. The hierarchy point < edge <
% face < voxel can be used to determine which stagger locations are
% acceptable: point data may be indexed using any of the higher aggregation
% levels (as appropriate for the topology considered) whereas voxel data
% may only be indexed using voxels. The reason for this is that a voxel
% index may be used to determine which faces, edges and points are
% associated with it but a point index does not uniquely identify which
% edges, faces and voxels to associate with it.
%
% By default use the highest aggregation level (i.e. preferably voxels) for
% indexing. For quantities not defined at the default stagger location, its
% own spatial dimensions should be replaced by the spatial dimensions of
% the default stagger location. The conversion table indicates for each
% possible dimension what other dimension should be used.
%
switch Topology
    case 'Struct3D'
        DefStagger = 'Voxels3D';
        ConversionTable = Dimensions([1 1 1 4 4 4 7 7]);
    case 'Struct2D+'
        if length(Dimensions)==4
            DefStagger = 'Faces2D';
        else
            DefStagger = 'Voxels3D';
        end
        ConversionTable = Dimensions([1 1 1 4 4 4 kdims(Dimensions,6)]);
    case 'Unstruct3D'
        DefStagger = 'Voxels3D';
        ConversionTable = Dimensions([1 1 1 1]);
    case 'Unstruct2D+'
        if length(Dimensions)==3
            DefStagger = 'Faces2D';
        else
            DefStagger = 'Voxels3D';
        end
        ConversionTable = Dimensions([1 1 1 kdims(Dimensions,3)]);
    case 'Unstruct1D+'
        if length(Dimensions)==2
            DefStagger = 'Edges2D';
        else
            DefStagger = 'VFaces3D';
        end
        ConversionTable = Dimensions([1 1 kdims(Dimensions,2)]);
    case 'Unstruct0D+'
        if length(Dimensions)==1
            DefStagger = 'Nodes2D';
        else
            DefStagger = 'VEdges3D';
        end
        ConversionTable = Dimensions([1 kdims(Dimensions,1)]);
end
Locations(l).DefaultStagger     = DefStagger;
Locations(l).DimConversionTable = ConversionTable;

function vdims = kdims(Dimensions,hdims)
nvdims = length(Dimensions)-hdims;
vdims = hdims+1+floor(((1:nvdims)-1)/3)*3;
