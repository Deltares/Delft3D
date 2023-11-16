function Quantities = default_quantities(FI)
%DEFAULT_QUANTITIES Default implementation for quantities.

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

%% Locations and domains are considered to be identical
[Success, Locations] = qp_getdata(FI,'locations');
ndoms = length(Locations);
Props = cell(1,ndoms);
for Domain = 1:ndoms
    %   [Success,DataProps ]            = QP_GETDATA(FI,Domain)
    [Success, DataProps] = qp_getdata(FI,Domain);
    if isempty(DataProps)
        continue
    end
    % remove separators
    DataProps(strmatch('-------',{DataProps.Name},'exact'))=[];
    %
    c = struct2cell(DataProps);
    f = fieldnames(DataProps);
    %
    n = strmatch('Unit',f,'exact');
    if isempty(n)
        i = strmatch('Units',f,'exact');
        n = length(f)+1;
        f{n} = 'Unit';
        c(n,:) = c(i,:);
    end
    %
    n = strmatch('ValType',f,'exact');
    if isempty(n)
        i = strmatch('NVal',f,'exact');
        n = length(f)+1;
        f{n} = 'ValType';
        for q = 1:size(c,2)
            switch convertnval(c{i,q})
                case -1
                    ValType = 'selfplot';
                case 0
                    ValType = 'none';
                case 1
                    ValType = 'float';
                case 2
                    ValType = 'vector(xy)';
                case 3
                    ValType = 'vector(xyz)';
                case 4
                    ValType = 'string';
                case 5
                    ValType = 'logical';
                case 6
                    ValType = 'discrete';
            end
            c{n,q} = ValType;
        end
    end
    %
    n = strmatch('Dimensions',f,'exact');
    if isempty(n)
        i = strmatch('DimFlag',f,'exact');
        n = length(f)+1;
        f{n} = 'Dimensions';
        for q = 1:size(c,2)
            DimFlag = c{i,q};
            Dims = {'Time' 'Station' 'M' 'N' 'K'};
            Dims(:,DimFlag==0)=[];
            c{n,q} = Dims;
        end
    end
    %
    n = strmatch('Stagger',f,'exact');
    if isempty(n)
        i = strmatch('DimFlag',f,'exact');
        n = length(f)+1;
        f{n} = 'Stagger';
        for q = 1:size(c,2)
            DimFlag = c{i,q};
            DimFlag = min(DimFlag(2:5),1);
            if isequal(DimFlag,[0 1 1 0])
               c{n,q} = 'Faces2D';
            elseif isequal(DimFlag,[0 1 1 1])
               c{n,q} = 'Voxels3D';
            elseif isequal(DimFlag,[1 0 0 0])
               c{n,q} = 'Points2D';
            elseif isequal(DimFlag,[1 0 0 1])
               c{n,q} = 'VEdges3D';
            else
               c{n,q} = 'none';
            end
        end
    end
    %
    n = strmatch('Location',f,'exact');
    if isempty(n)
        n = length(f)+1;
        f{n} = 'Location';
        c(n,:) = {Locations(Domain).Name};
    end
    %
    Props{Domain} = cell2struct(c,f,1);
end
Quantities = cat(1,Props{:});
