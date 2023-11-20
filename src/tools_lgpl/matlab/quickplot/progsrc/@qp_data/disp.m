function disp(A)
%DISP Display qp_data object.

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

%% Handle exceptional cases
% These cases are useful during debugging but are not really practical for
% any other purpose.

if isempty(A.Name)
    if ~isempty(A.Original)
        fprintf('    raw QUICKPLOT Data Object\n')
        disp(A.Original)
    else
        fprintf('    empty QUICKPLOT Data Object\n')
    end
else

    %% Handle standard case
    % Show the name, type, unit (including SI version) and dimensions as
    % appropriate.

    %fprintf('    QUICKPLOT Data Object\n')
    if A.Dummy
        fprintf('    [empty dummy object]\n')
    end
    if isequal(A.Name,'QuickPlot.NoValues')
        %fprintf('    NO VALUES\n')
    else
        %
        % Show Name
        %
        fprintf('    Quantity   : %s\n',A.Name)
        %
        % Show Type
        %
        if strcmp(A.ValType,'level')
            fprintf('    Type       : float (%s)\n',A.ValType)
        else
            fprintf('    Type       : %s\n',A.ValType)
        end
        %
        % Show Unit and convert to SI
        %
        if ~isempty(A.Unit)
            fprintf('    Unit       : %s',A.Unit);
            [a,b]=qp_unitconversion(A.Unit,'SI');
            if ischar(a)
                fprintf(' (SI equivalent unknown)');
            elseif ~isequal(b,A.Unit)
                fprintf(' (equivalent to %g %s)',a,b);
            end
            fprintf('\n');
        end
    end
    %
    if isempty(A.Grid) || ~isfield(A.Grid,'SpatialDimensions')
        SpatialDims = {};
    else
        SpatialDims = A.Grid.SpatialDimensions;
    end
    if ~isempty(A.Value)
        Dims = A.Value(1).Dimensions;
        Dims = Dims(~ismember(Dims,SpatialDims));
        printdims(Dims,A.Dimensions,[])
    end
    %
    if ~isfield(A.Grid,'Name')
    elseif isequal(A.Grid.Name,'QuickPlot.NoCoordinates')
        fprintf('    Location   : not available\n');
    else
        if isequal(A.Grid.Name,'<unnamed location>')
            fprintf('    Location   : unnamed %s\n',topodescription(A.Grid.Topology))
        else
            fprintf('    Location   : %s (%s)',A.Grid.Name,topodescription(A.Grid.Topology))
        end
        for x = 'XYZ'
            if isfield(A.Grid,x)
                X = A.Grid.(x);
                minX = min(X.Data(:));
                maxX = max(X.Data(:));
                if isempty(minX)
                    % no coordinates available
                elseif isequal(minX,maxX)
                    fprintf('                 %s = %g %s\n',x,minX,X.Unit);
                else
                    fprintf('                 %s = %g to %g %s\n',x,minX,maxX,X.Unit);
                end
            end
        end
    end
    %
    % Show Dimension(s) with units as appropriate
    %
    if ~isempty(A.Value) && isfield(A.Grid,'Name')
        nstagger = length(A.Value);
        if length(A.Value(1).Stagger)<3
            stagger = A.Value(1).Stagger;
        elseif strcmp(A.Value(1).Stagger(end-2:end-1),'-d')
            stagger = A.Value(1).Stagger(1:end-3);
        else
            stagger = A.Value(1).Stagger;
        end
        %
        if isempty(A.Grid)
            stagDir = {};
        else
            dirs = SpatialDims(1:3:end);
            switch stagger
                case {'Edges3D','HEdges3D'}
                    stagDir = cell(1,length(dirs));
                    for i = 1:length(dirs)
                        stagDir{i} = sprintf('aligned with %s direction',dirs{i});
                    end
                case {'Faces3D','VFaces3D','Edges2D'}
                    stagDir = cell(1,length(dirs));
                    for i = 1:length(dirs)
                        stagDir{i} = sprintf('normal to %s direction',dirs{i});
                    end
                otherwise
                    switch A.ValType
                        case 'vector(xy)'
                            stagDir = {'x component','y component'};
                        case {'vector(ij)','vector(ijk)'}
                            stagDir = cell(1,length(dirs));
                            for i = 1:length(dirs)
                                stagDir{i} = sprintf('%s component',dirs{i});
                            end
                        otherwise
                            stagDir = {};
                    end
            end
        end
        %
        for st = 1:nstagger
            fprintf('\n    Stagger    : %s',stagprint(stagger));
            if nstagger>1
                if nstagger<=length(stagDir)
                    fprintf(' - %s\n',stagDir{st});
                else
                    fprintf(' - direction %i\n',st);
                end
            else
                fprintf('\n');
            end
            Dims = A.Value(st).Dimensions;
            Dims = Dims(ismember(Dims,SpatialDims));
            printdims(Dims,A.Dimensions,[])
        end
    end
end

pops = plotstyles(A);
Str = '\n    Plot Styles: %s';
for i = 1:length(pops)
    if pops(i).Animated
        fprintf([Str ' (animated)\n'],pops(i).Name);
    else
        fprintf([Str '\n'],pops(i).Name);
    end
    Str = '                 %s';
end
if isempty(pops)
    fprintf(Str,'None Available');
end

%% Add empty line if FormatSpacing is loose
if ~isequal(get(0,'FormatSpacing'),'compact')
    fprintf('\n');
end
