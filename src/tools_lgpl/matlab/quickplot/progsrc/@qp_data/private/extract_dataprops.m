function [timdep,nval,stagger,PlotDim] = extract_dataprops(A,V,vDims,gDims)
%EXTRACT_DATAPROPS Determine characteristics of object.

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

%TODO: hasZlevel???

aDims = {A.Dimensions.Name};
vDspat = ismember(vDims,gDims);
vDindx = zeros(size(vDims));
vDsize = zeros(size(vDims));
vDtime = false(size(vDims));
for d = 1:length(vDims)
    vDindx(d) = strmatch(vDims{d},aDims,'exact');
    vDsize(d) = length(A.Dimensions(vDindx(d)).Values);
    vDtime(d) = ismember(A.Dimensions(vDindx(d)).Type,{'discrete-time','continous-time'});
end
%
isDim = find(vDspat);
sDims = vDims(vDspat);
%
% Definition intended:
% TimeDependent = 0 : no time associated with quantity
%                 1 : one time available (one selected)
%                 2 : one time selected, multiple available
%                 3 : multiple times selected
%
% Current definition:
% TimeDependent = 0 : no time associated with quantity
%                 1 : one time selected
%                 2 : NA
%                 3 : multiple times selected
%
timdep = double(any(vDtime)) + 2*double(vDsize(vDtime)>1);
nval = prod(vDsize(~vDtime & ~vDspat));
if nargout<3
    return
end
%
if length(V.Stagger)<3
    stagger = V.Stagger;
elseif strcmp(V.Stagger(end-2:end-1),'-d')
    stagger = V.Stagger(1:end-3);
else
    stagger = V.Stagger;
end
if nargout<4
    return
end
%
switch A.Grid.Topology
    case 'Struct3D'
        D1 = isDim(ismember(sDims,gDims(1:2)));
        D2 = isDim(ismember(sDims,gDims(4:5)));
        D3 = isDim(ismember(sDims,gDims(7:8)));
        if vDsize(D1)==1
            if vDsize(D2)==1
                if vDsize(D3)==1
                    PlotDim = '0D';
                else
                    PlotDim = '1D in 3D';
                end
            elseif vDsize(D3)==1
                PlotDim = '1D in 3D';
            else
                PlotDim = '2D in 3D';
            end
        elseif vDsize(D2)==1
            if vDsize(D3)==1
                PlotDim = '1D in 3D';
            else
                PlotDim = '2D in 3D';
            end
        elseif vDsize(D3)==1
            PlotDim = '2D in 3D';
        else
            PlotDim = '3D';
        end
    case 'Struct2D+'
        H1 = isDim(ismember(sDims,gDims(1:2)));
        H2 = isDim(ismember(sDims,gDims(4:5)));
        V  = isDim(ismember(sDims,gDims(7:end)));
        if isempty(V) || vDsize(V)==1
            if isempty(H1) || vDsize(H1)==1
                if isempty(H2) || vDsize(H2)==1
                    PlotDim = '0D';
                else
                    PlotDim = '1DH';
                end
            elseif isempty(H2) || vDsize(H2)==1
                PlotDim = '1DH';
            else
                PlotDim = '2DH';
            end
        elseif isempty(H1) || vDsize(H1)==1
            if isempty(H2) || vDsize(H2)==1
                PlotDim = '1DV';
            else
                PlotDim = '2DV';
            end
        elseif isempty(H2) || vDsize(H2)==1
            PlotDim = '2DV';
        else
            PlotDim = '3D';
        end
    case 'Unstruct3D'
        S = isDim(ismember(sDims,gDims(1:4)));
        if isempty(S) || vDsize(S)==1
            PlotDim = '0D';
        else
            PlotDim = '3D';
        end
    case 'Unstruct2D+'
        H  = isDim(ismember(sDims,gDims(1:3)));
        V  = isDim(ismember(sDims,gDims(4:end)));
        if isempty(V) || vDsize(V)==1
            if isempty(H) || vDsize(H)==1
                PlotDim = '0D';
            else
                PlotDim = '2DH';
            end
        elseif isempty(H) || vDsize(H)==1
            PlotDim = '1DV';
        else
            PlotDim = '3D';
        end
    case 'Unstruct1D+'
        H  = isDim(ismember(sDims,gDims(1:2)));
        V  = isDim(ismember(sDims,gDims(3:end)));
        if isempty(V) || vDsize(V)==1
            if isempty(H) || vDsize(H)==1
                PlotDim = '0D';
            else
                PlotDim = '1DHN';
            end
        elseif isempty(H) || vDsize(H)==1
            PlotDim = '1DV';
        else
            PlotDim = '2DVN';
        end
    case 'Unstruct0D+'
        H  = isDim(ismember(sDims,gDims(1)));
        V  = isDim(ismember(sDims,gDims(2:end)));
        if isempty(V) || vDsize(V)==1
            if isempty(H) || vDsize(H)==1
                PlotDim = '0D';
            else
                PlotDim = '0DM';
            end
        elseif isempty(H) || vDsize(H)==1
            PlotDim = '1DVM';
        else
            PlotDim = '1DV';
        end
end
