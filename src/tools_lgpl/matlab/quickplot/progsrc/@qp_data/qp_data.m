function X = qp_data(varargin)
% QUICKPLOT Data Object.
%
% Object construction
%   qp_data    - Construct qp_data object.
%
% General
%   disp       - Display qp_data object.
%   display    - Display qp_data object.
%   subsref    - Subscripted reference qp_data object.
%   fieldnames - Get object property names.
%
% Graphics
%   plot       - Plot qp_data object.
%
% Helper routines
%   classic    - Convert qp_data object to classic QUICKPLOT data structure.

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

%% Create basic structure
S.Name       = '';
S.Unit       = '';
S.ValType    = '';
S.Grid       = [];
S.Value      = [];
S.Dimensions = [];
S.Original   = [];
S.Dummy      = false;
if nargin>1 && ischar(varargin{1})
    switch varargin{1}
        case 'upgrade'
            S = classic_qp(S,varargin{2:end});
        case 'wrap'
            S = wrap_data(S,varargin{2:end});
        case 'wrapdummy'
            S = wrap_data(S,varargin{2:end});
            S.Dummy = true;
        otherwise
            S.Original = varargin{1};
    end
else
    S.Original = varargin{1};
end

%% Convert structure to object
X = class(S,'qp_data');

%% Upgrade classic data structure to new structure
function S = classic_qp(S,Original,Selection)
if nargin>2
    selected = struct2cell(Selection);
    dimnames = fieldnames(Selection);
    Time = strmatch('Time',dimnames,'exact');
    dimnames(Time)=[];
    selected(Time)=[];
else
    dimnames = {};
    selected = {};
end
S.Name    = Original.Name;
S.Unit    = Original.Units;
S.ValType = 'none';
closebracket = '';
if isfield(Original,'Val')
    S.Value(1).Dimensions = {};
    S.Value(1).Data = Original.Val;
    S.Value(1).Stagger = '?';
    S.ValType = 'double';
end
if isfield(Original,'XComp')
    S.Value(1).Dimensions = {};
    S.Value(1).Data = Original.XComp;
    S.Value(1).Stagger = '?';
    S.ValType = 'vector(x';
    closebracket = ')';
end
if isfield(Original,'YComp')
    S.Value(end+1).Dimensions = {};
    S.Value(end).Data = Original.YComp;
    S.Value(end).Stagger = '?';
    S.ValType = [S.ValType 'y'];
end
if isfield(Original,'ZComp')
    S.Value(end+1).Dimensions = {};
    S.Value(end).Data = Original.ZComp;
    S.Value(end).Stagger = '?';
    S.ValType = [S.ValType 'z'];
end
S.ValType = [S.ValType closebracket];
S.Dimensions(1).Name        = 'Time';
S.Dimensions(1).Description = 'Time';
S.Dimensions(1).Type        = 'discrete-time';
S.Dimensions(1).Unit        = 'matlabdate';
S.Dimensions(1).Values      = Original.Time;
%
szV = size(S.Value(1).Data);
Dims = {'Time'};
if isfield(Original,'TRI')
    if isempty(dimnames)
        dimnames = {'M' 'K'};
        dimnames(length(szV)+1:end)=[];
        dimnames(szV==1)=[];
    end
else
    if length(Original.Time)>1
        szV=szV(2:end);
    else
        Dims = {};
    end
    if isempty(dimnames)
        dimnames = {'M' 'N' 'K'};
        dimnames(length(szV)+1:end)=[];
        dimnames(szV==1)=[];
    end
end
Dims = cat(2,Dims,dimnames);
for i = 1:length(dimnames)
    S.Dimensions(end+1).Name      = dimnames{i};
    S.Dimensions(end).Description = dimnames{i};
    S.Dimensions(end).Type        = 'discrete';
    S.Dimensions(end).Unit        = '';
    if isempty(selected)
        if i > length(szV)
            v = NaN;
        else
            v = repmat(NaN,1,szV(i));
        end
        S.Dimensions(end).Values = v;
    else
        S.Dimensions(end).Values = selected{i};
    end
end
for i = 1:length(S.Value)
    S.Value(i).Dimensions = Dims;
end
%For debug purposes you may choose to store the original data structure too
S.Original = []; %Original;

%% Wrap minimal data structure to new structure
function S = wrap_data(S,Quantity,Loc,Dimensions,Selection,Original)
if isstruct(Original)
    S = classic_qp(S,Original,Selection);
else
    if ~isempty(Loc)
        S.Grid.Name = 'QuickPlot.NoCoordinates';
        S.Grid.Topology = Loc.Topology;
        switch Loc.Topology
            case 'Struct3D'
                mn = 9;
            case 'Struct2D+'
                mn = 6;
            case 'Unstruct3D'
                mn = 4;
            case 'Unstruct2D+'
                mn = 3;
            case 'Unstruct1D+'
                mn = 2;
            case 'Unstruct0D+'
                mn = 1;
        end
        k = unique(floor(find(ismember(Loc.SpatialDimensions((mn+1):end),fieldnames(Selection)))/3));
        if isempty(k)
            S.Grid.SpatialDimensions = Loc.SpatialDimensions(1:mn);
        else
            S.Grid.SpatialDimensions = Loc.SpatialDimensions([1:mn mn+3*k+(1:3)]);
        end
    end
    %
    i0 = 0;
    for c = 1:length(Quantity)
        Q = Quantity(c);
        D = Original{c};
        if strmatch('QuickPlot.',Q.Name)
            S.Grid.Name = Loc.Name;
            %
            % Transfer information about the grid
            %
            switch Q.Name(11:end)
                case 'x_coordinate'
                    S.Grid.X.Data = D{1};
                    S.Grid.X.Unit = Q.Unit;
                    S.Grid.X.Dimensions = Q.Dimensions;
                    S.Grid.X.Stagger = Q.Stagger;
                case 'y_coordinate'
                    S.Grid.Y.Data = D{1};
                    S.Grid.Y.Unit = Q.Unit;
                    S.Grid.Y.Dimensions = Q.Dimensions;
                    S.Grid.Y.Stagger = Q.Stagger;
                case 'z_coordinate'
                    S.Grid.Z.Data = D{1};
                    S.Grid.Z.Unit = Q.Unit;
                    S.Grid.Z.Dimensions = Q.Dimensions;
                    S.Grid.Z.Stagger = Q.Stagger;
            end
        else
            S.Name    = Q.Name;
            S.Unit    = Q.Unit;
            S.ValType = Q.ValType;
            for i = 1:numel(D)
                S.Value(i0+i).Data = D{i};
                S.Value(i0+i).Unit = Q.Unit;
                id = min(i,size(Q.Dimensions,1));
                S.Value(i0+i).Dimensions = Q.Dimensions(id,:);
                S.Value(i0+i).Stagger = sprintf('%s-d%i',Q.Stagger,i);
            end
            if numel(D)==1
                S.Value(i0+1).Stagger = Q.Stagger;
            end
            i0 = i0+numel(D);
        end
    end
end
%
if isempty(S.Value)
    S.Name = 'QuickPlot.NoValues';
    S.ValType = 'none';
end
%
vardims = fieldnames(Selection)';
alldims = {Dimensions.Name};
for i = 1:length(vardims)
    dim = vardims{i};
    j = strmatch(dim,alldims,'exact');
    S.Dimensions(i).Name        = dim;
    S.Dimensions(i).Description = Dimensions(j).Description;
    S.Dimensions(i).Type        = Dimensions(j).Type;
    S.Dimensions(i).Unit        = Dimensions(j).Unit;
    S.Dimensions(i).Values      = Dimensions(j).Values(Selection.(dim));
end
%For debug purposes you may choose to store the original data structure too
S.Original = []; %Original;

