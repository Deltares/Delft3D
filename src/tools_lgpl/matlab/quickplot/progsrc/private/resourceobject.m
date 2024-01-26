function varargout=resourceobject(DataRes,domain,field,cmd,varargin)
%RESOURCEOBJECT Implements old interface for new QUICKPLOT Data Resource Object.
%   Domains                 = XXXFIL(FI,[],'domains')
%   DataProps               = XXXFIL(FI,Domain)
%   Size                    = XXXFIL(FI,Domain,DataFld,'size')
%   Times                   = XXXFIL(FI,Domain,DataFld,'times',T)
%   StNames                 = XXXFIL(FI,Domain,DataFld,'stations')
%   SubFields               = XXXFIL(FI,Domain,DataFld,'subfields')
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

if DataRes.NLocations == 1 && isequal(domain,0)
    domain = 1;
end
if nargin<2
    error('Not enough input arguments');
elseif nargin==2
    varargout={infile(DataRes,domain)};
    return
elseif ischar(field)
    switch field
        case 'options'
            [varargout{1:2}]=options(DataRes,cmd,varargin{:});
        case 'domains'
            varargout={domains(DataRes)};
        case 'dimensions'
            varargout={dimensions(DataRes)};
        case 'locations'
            varargout={locations(DataRes)};
        case 'quantities'
            varargout={quantities(DataRes)};
        case 'data'
            [varargout{1:2}]=getdata(DataRes,cmd,varargin{:});
    end
    return
end

field = rmfield(field,'DimFlag');
cmd=lower(cmd);
switch cmd
    case 'size'
        varargout={getsize(DataRes,domain,field)};
        return;
    case 'times'
        varargout={readtim(DataRes,domain,field,varargin{:})};
        return
    case 'stations'
        varargout={readsts(DataRes,domain,field,varargin{:})};
        return
    case 'subfields'
        varargout={getsubfields(DataRes,domain,field,varargin{:})};
        return
    otherwise
        [XYRead,DataRead,DataInCell]=gridcelldata(cmd);
end

NewDataRes = DataRes;
DataRes = DataRes.Location(domain).Quantity(field);
Selection = cell(2,DataRes.NDimensions);
Selection(1,:) = DataRes.DimensionNames;
Selection(2,:) = {1};

arg = 1;
if ~isempty(field.SubDimensions);
    idim = strmatch(field.SubDimensions{1},Selection(1,:),'exact');
    Selection{2,idim} = varargin{1};
    arg = 2;
end
for i = 1:5
    dim = field.DimMapper{i};
    if ~isempty(dim)
        idim = strmatch(dim,Selection(1,:),'exact');
        if arg<=length(varargin)
            Selection{2,idim} = varargin{arg};
            arg = arg+1;
            if isequal(Selection{2,idim},0)
                Selection{2,idim} = 1:DataRes.Size(dim);
            end
        elseif i == 1
            Selection{2,idim} = DataRes.NTimes;
        else
            Selection{2,idim} = 1:DataRes.Size(dim);
        end
    end
end
%
Data = DataRes.Get(Selection{:});
Ans = classic(Data);
varargout={Ans NewDataRes};
% -----------------------------------------------------------------------------

% -----------------------------------------------------------------------------
function Domains = domains(DataRes)
Domains = DataRes.LocationNames;
% -----------------------------------------------------------------------------

% -----------------------------------------------------------------------------
function Dimensions = dimensions(DataRes)
Dimensions = DataRes.Resource.Dimensions;
% -----------------------------------------------------------------------------

% -----------------------------------------------------------------------------
function Locations = locations(DataRes)
Locations = DataRes.Locations;
% -----------------------------------------------------------------------------

% -----------------------------------------------------------------------------
function Quantities = quantities(DataRes)
Quantities = DataRes.Quantities;
% -----------------------------------------------------------------------------

% -----------------------------------------------------------------------------
function [Data,NewDataRes] = getdata(DataRes,field,DimSelection)
Data = DataRes.Quantity(field).get(DimSelection);
NewDataRes = DataRes;
% -----------------------------------------------------------------------------

% -----------------------------------------------------------------------------
function Out = infile(DataRes,domain)
DataRes = DataRes.Location(domain);
Quants = DataRes.Quantities;
Loc = DataRes.Locations;
c = struct2cell(Quants);
f = fieldnames(Quants);
%
n = strmatch('Units',f,'exact');
if isempty(n)
    i = strmatch('Unit',f,'exact');
    n = length(f)+1;
    f{n} = 'Units';
    c(n,:) = c(i,:);
end
%
n = strmatch('NVal',f,'exact');
if isempty(n)
    i = strmatch('ValType',f,'exact');
    n = length(f)+1;
    f{n} = 'NVal';
    for q = 1:size(c,2)
        switch c{i,q}
            case 'selfplot'
                NVal = -1;
            case 'none';
                NVal = 0;
            case 'float';
                NVal = 1;
            case 'vector(xy)';
                NVal = 2;
            case 'vector(xyz)';
                NVal = 3;
            case 'string';
                NVal = 4;
            case 'logical';
                NVal = 5;
            case 'discrete';
                NVal = 6;
        end
        c{n,q} = NVal;
    end
end
%
n = strmatch('DimFlag',f,'exact');
m = strmatch('DimMapper',f,'exact');
T_=1; ST_=2; M_=3; N_=4; K_=5;
if isempty(n) || isempty(m)
    ai = strmatch('Dimensions',f,'exact');
    gi = strmatch('SpatialDimensions',f,'exact');
    ti = strmatch('TimeDimensions',f,'exact');
    if isempty(n)
        n = length(f)+1;
        f{n} = 'DimFlag';
    end
    if isempty(m)
        m = length(f)+1;
        f{m} = 'DimMapper';
    end
    for q = 1:size(c,2)
        DimFlag = [0 0 0 0 0];
        DimMapper = {'' '' '' '' ''};
        %
        % time
        if ~isempty(c{ti,q})
            DimFlag(T_) = 1;
            DimMapper(T_) = c{ti,q};
        end
        %
        dims = c{ai,q}(1,:);
        sdims = dims(ismember(dims,c{gi,q}));
        [dummy,sindex_cdims] = ismember(sdims,Loc.SpatialDimensions);
        sdims = Loc.DimConversionTable(sindex_cdims);
        if ~isempty(sdims)
            switch Loc.Topology
                case 'Struct3D'
                    DimFlag([M_ N_ K_]) = 1;
                    DimMapper([M_ N_ K_]) = sdims;
                case 'Struct2D+'
                    DimFlag([M_ N_]) = 1;
                    DimMapper([M_ N_]) = sdims(1:2);
                    if length(sdims)>2
                        DimFlag(K_) = 1;
                        DimMapper(K_) = sdims(3);
                    end
                case 'Unstruct3D'
                    DimFlag(M_) = 1;
                case 'Unstruct2D+'
                    DimFlag(M_) = 1;
                    DimMapper(M_) = sdims(1);
                    if length(sdims)>1
                        DimFlag(K_) = 1;
                        DimMapper(K_) = sdims(2);
                    end
                case 'Unstruct1D+'
                    DimFlag(M_) = 1;
                    DimMapper(M_) = sdims(1);
                    if length(sdims)>1
                        DimFlag(K_) = 1;
                        DimMapper(K_) = sdims(2);
                    end
                case 'Unstruct0D+'
                    DimFlag(ST_) = 1;
                    DimMapper(ST_) = sdims(1);
                    if length(sdims)>1
                        DimFlag(K_) = 1;
                        DimMapper(K_) = sdims(2);
                    end
            end
        end
        %
        c{m,q} = DimMapper;
        c{n,q} = DimFlag;
    end
end
%
i = strmatch('DataInCell',f,'exact');
if isempty(i)
    i = length(f)+1;
    f{i} = 'DataInCell';
    c(i,:) = {1};
end
%
%add(f,'SubFld',{});
%add(f,'UseGrid',1);
Out = cell2struct(c,f,1);
% -----------------------------------------------------------------------------

% -----------------------------------------------------------------------------
function subf = getsubfields(DataRes,domain,field,f)
DataRes = DataRes.Location(domain).Quantity(field);
switch DataRes.NSubfields
    case 0
        subf={};
    case 1
        subf = DataRes.SubfieldValues(1);
    otherwise
        error('Multiple subfields encountered but only one supported!')
end
if nargin>3 && f~=0
    subf = subf(f);
end
% -----------------------------------------------------------------------------

% -----------------------------------------------------------------------------
function sz = getsize(DataRes,domain,field)
sz = [0 0 0 0 0];
DataRes = DataRes.Location(domain).Quantity(field);
i = ~cellfun('isempty',field.DimMapper);
sz(i) = DataRes.Size(field.DimMapper{i});
% -----------------------------------------------------------------------------

% -----------------------------------------------------------------------------
function T = readtim(DataRes,domain,field,t)
T = DataRes.Location(domain).Quantity(field).Times;
if nargin>3 && t~=0
    T = T(t);
end
% -----------------------------------------------------------------------------

% -----------------------------------------------------------------------------
function S = readsts(DataRes,domain,field,s)
S = DataRes.Location(domain).Quantity(field).StationNames;
if nargin>3 && s~=0
    S = S(s);
end
% -----------------------------------------------------------------------------

% -----------------------------------------------------------------------------
%function [NewDataRes,cmdargs]=options(DataRes,mfig,cmd,varargin)
% -----------------------------------------------------------------------------

% -----------------------------------------------------------------------------
%function OK=optfig(h0)
% -----------------------------------------------------------------------------


