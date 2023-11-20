function varargout=hdffil(FI,domain,field,cmd,varargin)
%HDFFIL QP support for HDF files.
%   Domains                 = XXXFIL(FI,[],'domains')
%   DataProps               = XXXFIL(FI,Domain)
%   Size                    = XXXFIL(FI,Domain,DataFld,'size')
%   Times                   = XXXFIL(FI,Domain,DataFld,'times',T)
%   StNames                 = XXXFIL(FI,Domain,DataFld,'stations')
%   SubFields               = XXXFIL(FI,Domain,DataFld,'subfields')
%   [TZshift   ,TZstr  ]    = XXXFIL(FI,Domain,DataFld,'timezone')
%   [Data      ,NewFI]      = XXXFIL(FI,Domain,DataFld,'data',subf,t,station,m,n,k)
%   [Data      ,NewFI]      = XXXFIL(FI,Domain,DataFld,'celldata',subf,t,station,m,n,k)
%   [Data      ,NewFI]      = XXXFIL(FI,Domain,DataFld,'griddata',subf,t,station,m,n,k)
%   [Data      ,NewFI]      = XXXFIL(FI,Domain,DataFld,'gridcelldata',subf,t,station,m,n,k)
%                             XXXFIL(FI,[],'options',OptionsFigure,'initialize')
%   [NewFI     ,cmdargs]    = XXXFIL(FI,[],'options',OptionsFigure,OptionsCommand, ...)
%
%   The DataFld can only be either an element of the DataProps structure.

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

%========================= GENERAL CODE =======================================
T_=1; ST_=2; M_=3; N_=4; K_=5;

if nargin<2
    error('Not enough input arguments')
end

OrigFI = FI;

if nargin==2
    varargout={infile(FI,domain)};
    return
elseif ischar(field)
    switch field
        case 'options'
            [varargout{1:2}]=options(FI,cmd,varargin{:});
        case 'domains'
            varargout={domains(FI)};
        case 'dimensions'
            varargout={dimensions(FI)};
        case 'locations'
            varargout={locations(FI)};
        case 'quantities'
            varargout={quantities(FI)};
        case 'getparams'
            varargout={[]};
        case 'data'
            [varargout{1:2}]=getdata(FI,cmd,varargin{:});
    end
    return
else
    Props=field;
end

cmd=lower(cmd);
switch cmd
    case 'size'
        varargout={getsize(FI,Props)};
        return
    case 'times'
        varargout={readtim(FI,Props,varargin{:})};
        return
    case 'timezone'
        [varargout{1:2}]=gettimezone(FI,domain,Props);
        return
    case 'stations'
        varargout={readsts(FI,Props,0)};
        return
    case 'subfields'
        varargout={getsubfields(FI,Props,varargin{:})};
        return
    otherwise
        [XYRead,DataRead,DataInCell]=gridcelldata(cmd);
end

sz = getsize(FI,Props);
sz = sz(Props.DimFlag~=0);
idx = varargin;
for i = 1:length(varargin)
    if isequal(varargin{i},0)
        idx{i} = 1:sz(i);
    end
end
for i = length(sz):-1:length(varargin)+1
    idx{i} = 1:sz(i);
end

if 0%strcmp(FI.FileType,'XMDF')
else
    start = ones(size(idx));
    count = sz;
    ridx = idx;
    for i = 1:length(idx)
        if idx{i}(1)>1
            start(i) = idx{i}(1);
            ridx{i} = idx{i}-idx{i}(1)+1;
        end            
        count(i) = idx{i}(end)-idx{i}(1)+1;
    end
    Info = subsref(FI.GroupHierarchy,Props.Dataset);
    data = h5read(Info.Filename,Info.Name,start,count);
    Ans.Data = data(ridx{:});
end

varargout={Ans OrigFI};
% -----------------------------------------------------------------------------

% -----------------------------------------------------------------------------
function Out=infile(FI,domain)
T_=1; ST_=2; M_=3; N_=4; K_=5;
%======================== SPECIFIC CODE =======================================
PropNames={'Name'                   'Units' 'Geom' 'Coords' 'DimFlag' 'DataInCell' 'NVal' 'SubFld' 'MNK' 'Dataset' };
DataProps={'dummy field'            ''      ''     ''      [0 0 0 0 0]  0           0      []       0    []        };
Out=cell2struct(DataProps,PropNames,2);
%
if 0%strcmp(FI.FileType,'XMDF')
    for g = 1:length(FI.GroupHierarchy.Groups)
        FI.GroupHierarchy.Groups
    end
else
    [~,sr] = domains(FI);
    sr = sr{domain};
    srlen = length(sr);
    if srlen == 0
        Group = FI.GroupHierarchy;
    else
        Group = subsref(FI.GroupHierarchy,sr);
    end
    sr(srlen+1).type = '.';
    sr(srlen+1).subs = 'Datasets';
    sr(srlen+2).type = '()';
    %
    if isstruct(Group.Attributes)
        Attribs = {Group.Attributes.Shortname}';
    else
        Attribs = {};
    end
    if ismember('Grouptype',Attribs)
        att = hdf5read(Group.Filename,Group.Name,'Grouptype');
        dataset = att.Data; % General, DATASET SCALAR, DATASET VECTOR
    else
        dataset = 'unknown';
    end
    if ismember('DatasetUnits',Attribs)
        att = hdf5read(Group.Filename,Group.Name,'DatasetUnits');
        units = att.Data;
    else
        units = '';
    end
    %
    ndata = length(Group.Datasets);
    Out = repmat(Out,ndata,1);
    for i = 1:ndata
        Info = Group.Datasets(i);
        %
        Out(i).Name = Info.Name;
        Out(i).Units = units;
        Out(i).DimFlag(M_+(0:Info.Rank-1)) = 1;
        if strcmp(Info.Datatype.Class,'H5T_STRING')
            Out(i).NVal = 4;
        else
            Info.Datatype.Class
            switch dataset
                case 'DATASET SCALAR'
                    Out(i).NVal = 1;
                case 'DATASET VECTOR'
                    Out(i).NVal = 2;
            end
        end
        %
        sr(srlen+2).subs = {i};
        Out(i).Dataset = sr;
    end
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function sz=getsize(FI,Props)
ndims = length(Props.DimFlag);
sz = zeros(1,ndims);
Info = subsref(FI.GroupHierarchy,Props.Dataset);
sz(Props.DimFlag~=0) = Info.Dims;
% -----------------------------------------------------------------------------

function [d,sr] = domains(FI,sr0)
if nargin==1
    if 0%strcmp(FI.FileType,'XMDF')
        d = {};
        sr = {};
        return
    end
    group = FI.GroupHierarchy;
    sr0 = [];
else
    group = FI;
end
srnext = length(sr0)+1;
nsubgroups = length(group.Groups);
d = cell(1+nsubgroups,1);
sr = cell(1+nsubgroups,1);
d{1} = group.Name;
if nsubgroups>0
    sr{1} = {sr0};
    for i = 1:nsubgroups
        sr0(srnext).type = '.';
        sr0(srnext).subs = 'Groups';
        sr0(srnext+1).type = '()';
        sr0(srnext+1).subs = {i};
        [d{i+1},sr{i+1}] = domains(group.Groups(i),sr0);
    end
    d = cat(1,d{:});
    sr = cat(1,sr{:});
else
    sr{1} = sr0;
end