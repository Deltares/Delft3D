function Selection = adjustselect(Selection,Location,Dimensions,M)
%ADJUSTSELECT Adjust the selection of spatial dimensions.
%   SEL = ADJUSTSELECT(SEL,LOC,DIM,M) adjusts the selection structure SEL
%   after the user has changed the selection of dimension M. The structures
%   LOC and DIM contain necessary information on the grid and dimensions
%   respectively.

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

iM = strmatch(M,Location.SpatialDimensions,'exact');
if isempty(iM)
    % this routine has to do only something for spatial dimensions
    return
end
%
% Find all dimensions associated with the changed dimension M. In case of a
% structured dimension this will lead to a fence, pole, type (FPT) tripple.
%
Mgroup = Location.DimConversionTable{iM};
iFPT = strmatch(Mgroup,Location.DimConversionTable,'exact');
FPT = Location.SpatialDimensions(iFPT);
%
if Mgroup>1 || ...
        strcmp(Location.Topology,'Struct3D') || ...
        strcmp(Location.Topology,'Struct2D+')
    %
    % Handle the structured dimension case.
    %
    Pole = FPT{2};
    pDim = strmatch(Pole,{Dimensions.Name},'exact');
    nPoles = length(Dimensions(pDim).Values);
    if strcmp(M,Pole)
        %
        % From poles to fences
        %
        Selection = pole2fence(Selection,FPT{:},nPoles);
    else
        %
        % From fences to poles
        %
        Selection = fence2pole(Selection,FPT{:},nPoles);
    end
else
    %
    % Unstructured dimension case.
    %
    switch Location.Topology
        case 'Unstruct3D'
            %
            % Map among points, edges, faces and voxels.
            %
            error('Not yet implemented.')
        case 'Unstruct2D+'
            %
            % Map among points, edges and faces.
            %
            error('Not yet implemented.')
        case 'Unstruct1D+'
            %
            % Map from points to segments or reverse.
            %
            error('Not yet implemented.')
        case 'Unstruct0D+'
            %
            % No mapping needed in horizontal direction.
            %
    end
end

function Selection = fence2pole(Selection,Fence,Pole,Type,nPoles)
MF = Selection.(Fence);
if isempty(MF)
    Selection.(Pole) = MF;
    return
end
switch Type
    case 'DanglingLowCell'
        %
        % 1   2   3   4   5
        %   |   |   |   |   |
        %   1   2   3   4   5
        %
        if MF(1)>1
            MP = [MF(1)-1 MF];
        else
            MP = MF;
        end
    case 'DanglingHighCell'
        %
        %     1   2   3   4   5
        %   |   |   |   |   |
        %   1   2   3   4   5
        %
        if MF(end)<nPoles
            MP = [MF MF(end)+1];
        else
            MP = MF;
        end
    case 'WholeCells'
        %
        %     1   2   3   4
        %   |   |   |   |   |
        %   1   2   3   4   5
        %
        MP = [MF MF(end)+1];
    case 'ExtendedCells'
        %
        % 1   2   3   4   5   6
        %   |   |   |   |   |
        %   1   2   3   4   5
        %
        if MF(1)>1
            MP = [MF(1)-1 MF];
        else
            MP = MF;
        end
        if MP(end)>nPoles
            MP(end) = [];
        end
end
Selection.(Pole) = MP;

function Selection = pole2fence(Selection,Fence,Pole,Type,nPoles)
MP = Selection.(Pole);
%
% Check for valid pole selection.
%
if ~isempty(MP)
    if ~isequal(MP,min(MP):max(MP)) || MP(1)<1 || MP(end)>nPoles
        MP = max(1,min(MP)):min(nPoles,max(MP));
        if length(MP)>1
            warning('Deltares:QPDataResource:DimUpd', ...
                'Invalid selection for dimension %s adjusted to %i:%i.', ...
                Pole,MP(1),MP(end))
        elseif ~isempty(MP)
            warning('Deltares:QPDataResource:DimUpd', ...
                'Invalid selection for dimension %s adjusted to %i.', ...
                Pole,MP)
        else
            warning('Deltares:QPDataResource:DimUpd', ...
                'Selection for dimension %s out of range; adjusted to [].', ...
                Pole)
        end
    end
end
%
if length(MP)<=1
    %
    % One or no poles selected, so also no fences selected.
    %
    Selection.(Fence) = [];
    return
end
%
% Determine fences.
%
switch Type
    case 'DanglingLowCell'
        %
        % 1   2   3   4   5
        %   |   |   |   |   |
        %   1   2   3   4   5
        %
        MF = MP(2:end);
    case 'DanglingHighCell'
        %
        %     1   2   3   4   5
        %   |   |   |   |   |
        %   1   2   3   4   5
        %
        MF = MP(1:end-1);
    case 'WholeCells'
        %     1   2   3   4
        %   |   |   |   |   |
        %   1   2   3   4   5
        %
        MF = MP(1:end-1);
    case 'ExtendedCells'
        %
        % 1   2   3   4   5   6
        %   |   |   |   |   |
        %   1   2   3   4   5
        %
        MF = MP(2:end);
end
Selection.(Fence) = MF;

