function [ Ans, OrigFI ] = merge_trim_com( OrigFI,domain,Props,XYRead,DataRead,DataInCell,get_single_partition,nPartitions,mergeParts,mergeDim,hasSubfields,varargin)
%MERGE_TRIM_COM Routine for merging partitioned data from trim/com files
%
%   See also: d3d_trimfil, d3d_comfil

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
T_=1; ST_=2; M_=3; N_=4; K_=5;

if isfield(OrigFI,'NEFIS')
    if nPartitions == 1 || Props.DimFlag(M_) == 0
        domain = min(domain,length(OrigFI.NEFIS));
        OrigFI.NEFIS(domain).QP_Options = OrigFI.QP_Options;
        [Ans,OrigFI.NEFIS(domain)] = get_single_partition(OrigFI.NEFIS(domain),1,Props,XYRead,DataRead,DataInCell,varargin{:});
    else
        if mergeParts
            indexVarArg = sum(Props.DimFlag(1:mergeDim) ~= 0) + hasSubfields;
            if indexVarArg > length(varargin)
                returnSelected = 0;
            else
                returnSelected = varargin{indexVarArg};
            end
            if isequal(returnSelected,0)
                returnLengthMesh = OrigFI.Merge.Length;
                if DataInCell
                    returnLengthData = returnLengthMesh - 1;
                else
                    returnLengthData = returnLengthMesh;
                end
            else
                returnLengthData = length(returnSelected);
                if DataInCell
                    returnLengthMesh = returnLengthData + 1;
                else
                    returnLengthMesh = returnLengthData;
                end
            end
            
            returnIndicesData = cell(1,nPartitions);
            returnIndicesMesh = cell(1,nPartitions);
            queryIndices = cell(1,nPartitions);
            for d = length(OrigFI.NEFIS):-1:1
                partIndices = OrigFI.Merge.Indices{d};
                if d > 1
                    partOffset = partIndices(1) - 4;
                else
                    partOffset = partIndices(1) - 1;
                end
                
                if isequal(returnSelected,0)
                    if DataInCell
                        if d == 1
                            returnIndicesData{d} = partIndices(1:end-1);
                            returnIndicesMesh{d} = partIndices;
                        else
                            returnIndicesData{d} = partIndices-1;
                            returnIndicesMesh{d} = [partIndices(1)-1 partIndices];
                        end
                    else
                        returnIndicesData{d} = partIndices;
                        returnIndicesMesh{d} = partIndices;
                    end
                else
                    returnIndicesData{d} = find(ismember(returnSelected,partIndices));
                    partIndices(~ismember(partIndices,returnSelected)) = [];
                    if isempty(partIndices) % this partition does not contribute to the data request
                        returnIndicesData{d} = partIndices;
                        returnIndicesMesh{d} = partIndices;
                        queryIndices{d} = partIndices;
                        continue
                    end
                    returnIndicesMesh{d} = returnIndicesData{d};
                end
                
                partIndices = partIndices - partOffset;
                queryIndices{d} = partIndices;
            end
        end
        for d = length(OrigFI.NEFIS):-1:1
            if mergeParts
                if isempty(queryIndices{d})
                    continue
                end
                varargin{indexVarArg} = queryIndices{d};
            end
            if isfield(OrigFI,'QP_Options')
                OrigFI.NEFIS(d).QP_Options = OrigFI.QP_Options;
            end
            [Ans(d),OrigFI.NEFIS(d)] = get_single_partition(OrigFI.NEFIS(d),1,Props,XYRead,DataRead,DataInCell,varargin{:});
        end
        if mergeParts
            hasMultiTime = 0;
            if isfield(Ans,'Time') && length(Ans(1).Time) > 1
                hasMultiTime = 1;
            end
            for cfld = {'X','Y','Z','Val','XComp','YComp','ZComp','XDam','YDam'}
                fld = cfld{1};
                if ~isfield(Ans,fld)
                    continue
                end
                Merged = [];
                for d = 1:length(OrigFI.NEFIS)
                    if isempty(queryIndices{d})
                       continue 
                    end
                    if ismember(fld,{'X','Y'})
                        indices = returnIndicesMesh{d};
                        returnLength = returnLengthMesh;
                        if isfield(Ans,'Z')
                            multiTime = hasMultiTime;
                        else
                            multiTime = 0;
                        end
                    else
                        indices = returnIndicesData{d};
                        returnLength = returnLengthData;
                        multiTime = hasMultiTime;
                    end
                    if multiTime
                        timeDim = {':'};
                    else
                        timeDim = {};
                    end
                    if isempty(Merged)
                        MergeSize = size(Ans(d).(fld));
                        switch mergeDim
                            case M_
                                MergeSize(multiTime+1) = returnLength;
                            case N_
                                MergeSize(multiTime+2) = returnLength;
                        end
                        Merged = zeros(MergeSize);
                    end
                    switch mergeDim
                        case M_
                            Merged(timeDim{:},indices,:,:) = Ans(d).(fld);
                        case N_
                            Merged(timeDim{:},:,indices,:) = Ans(d).(fld);
                    end
                end
                Ans(1).(fld) = Merged;
            end
            Ans = Ans(1);
        end
    end
else
    [Ans,OrigFI] = get_single_partition(OrigFI,domain,Props,XYRead,DataRead,DataInCell,varargin{:});
end
