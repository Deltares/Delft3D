function disp(A)
%DISP Display qp_data_resource object.

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

%% Show basic information: name and type
%fprintf('    QUICKPLOT Data Resource Object\n')
fprintf('    Source     : %s\n',A.Key.Name)
cKey = struct2cell(A.Key);
fKey = fieldnames(A.Key);
for i=1:length(fKey)
    if ~isequal(fKey{i},'Name')
        fprintf('    %-11s: %s\n',fKey{i},cKey{i})
    end
end
fprintf('\n')

%% Get resource structure from resourcemanager
Resource = resourcemanager('retrieve',A.Key);

%% Show quantity/quantities
% If there only one quantity then show
%
% * name
% * type
% * unit
% * non-spatial dimensions
%
% If there are multiple quantities show
%
% * number of quantities
% * show unit if restricted
%
NQuant = length(A.QuantityNrs);
if NQuant==1
    Q = Resource.Quantities(A.QuantityNrs);
    fprintf('    Quantity   : %s\n',Q.Name);
    if strcmp(Q.ValType,'level')
        fprintf('    Type       : float (%s)\n',Q.ValType)
    else
        fprintf('    Type       : %s\n',Q.ValType);
    end
    if ~isempty(Q.Unit)
        if ~isempty(A.Unit)
            Unit = A.Unit;
        else
            Unit = Q.Unit;
        end
        if ~isempty(Unit)
            fprintf('    Unit       : %s',Unit);
            [a,b]=qp_unitconversion(Unit,'SI');
            if ischar(a)
                fprintf(' (SI equivalent unknown)')
            elseif ~isequal(b,Unit)
                fprintf(' (equivalent to %g %s)',a,b);
            end
            fprintf('\n')
        end
    end
    %
    DimList = unique(Q.Dimensions(:));
    DimList(ismember(DimList,Q.SpatialDimensions))=[];
    printdims(DimList,Resource.Dimensions,A.Selection)
else
    fprintf('    Quantities : %i\n',length(A.QuantityNrs));
    if ~isempty(A.Unit)
        Unit = A.Unit;
        fprintf('    Unit       : %s',Unit);
        [a,b]=qp_unitconversion(Unit,'SI');
        if ischar(a)
            fprintf(' (SI equivalent unknown)');
        elseif ~isequal(b,Unit)
            fprintf(' (equivalent to %g %s)',a,b);
        end
        fprintf('\n');
    end
end

%% Show locations
L = max(cellfun('length',{Resource.Locations(A.LocationNrs).Name}));
if ~isempty(L)
    Lstr = sprintf('%%-%is (%%s)\\n',L);
else
    fprintf('    Locations  : <none>\n');
end
NLoc = length(A.LocationNrs);
for i = 1:NLoc
    Loc = Resource.Locations(A.LocationNrs(i));
    if i == 1
        if NLoc>1
            fprintf('    Locations  : ');
        else
            fprintf('    Location   : ');
        end
    else
        fprintf('                 ');
    end
    if isequal(Loc.Name,'<unnamed location>')
        fprintf('unnamed %s\n',topodescription(Loc.Topology))
    else
        fprintf(Lstr,Loc.Name,topodescription(Loc.Topology))
    end
end
if NLoc==1 && ~A.Raw
    if NQuant>1
        fprintf('\n    Stagger    : indexed using cell indices\n');
    else
        stagger = Resource.Quantities(A.QuantityNrs).Stagger;
        fprintf('\n    Stagger    : %s indexed using cell indices\n',stagprint(stagger));
    end
    Loc = Resource.Locations(A.LocationNrs);
    switch Loc.Topology
        case 'Struct3D'
            DimList = Loc.SpatialDimensions([1 4 7]);
            VDims = {};
        case 'Struct2D+'
            DimList = Loc.SpatialDimensions([1 4]);
            VDims = Loc.SpatialDimensions(7:end);
        case 'Unstruct3D'
            DimList = Loc.SpatialDimensions(1);
            VDims = {};
        case 'Unstruct2D+'
            DimList = Loc.SpatialDimensions(1);
            VDims = Loc.SpatialDimensions(4:end);
        case 'Unstruct1D+'
            DimList = Loc.SpatialDimensions(1);
            VDims = Loc.SpatialDimensions(2:end);
        case 'Unstruct0D+'
            DimList = Loc.SpatialDimensions(1);
            VDims = Loc.SpatialDimensions(1:end);
    end
    %
    % Add cell index of vertical dimension if only one quantity has been
    % selected.
    %
    if length(A.QuantityNrs)==1
        V = find(ismember(VDims,Resource.Quantities(A.QuantityNrs).SpatialDimensions));
        V = floor((V-1)/3)*3+1;
        DimList = [DimList VDims(V)];
    end
    %
    printdims(DimList,Resource.Dimensions,A.Selection)
elseif NQuant==1
    Loc = Resource.Locations(A.LocationNrs);
    Quant = Resource.Quantities(A.QuantityNrs);
    stagger = Quant.Stagger;
    nstagger = size(Quant.Dimensions,1);
    switch stagger
        case {'Edges3D','HEdges3D'}
            dirs = Loc.DimConversionTable(ismember(Loc.SpatialDimensions,Quant.Dimensions(1,:)));
            stagDir = cell(1,length(dirs));
            for i = 1:length(dirs)
                stagDir{i} = ['aligned with ' dirs{i}];
            end
        case {'Faces3D','VFaces3D','Edges2D'}
            dirs = Loc.DimConversionTable(ismember(Loc.SpatialDimensions,Quant.Dimensions(1,:)));
            stagDir = cell(1,length(dirs));
            for i = 1:length(dirs)
                stagDir{i} = ['normal to ' dirs{i}];
            end
        otherwise
            stagDir = {};
    end
    for st = 1:nstagger
        fprintf('\n    Stagger    : %s',stagprint(stagger));
        if nstagger>1
            if nstagger<=length(stagDir)
                fprintf(' - %s direction\n',stagDir{st});
            else
                fprintf(' - direction %i\n',st);
            end
        else
            fprintf('\n');
        end
        Dims = Quant.Dimensions(st,:);
        Dims = Dims(ismember(Dims,Quant.SpatialDimensions));
        printdims(Dims,Resource.Dimensions,A.Selection)
    end
end
%% Add empty line if FormatSpacing is loose
if ~isequal(get(0,'FormatSpacing'),'compact')
    fprintf('\n');
end
