function varargout=gribfil(FI,domain,field,cmd,varargin)
%GRIBFIL QP support for GRIB files.
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
elseif nargin==2
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
        varargout={getsize(FI,domain,Props)};
        return
    case 'times'
        varargout={readtim(FI,domain,Props,varargin{:})};
        return
    case 'timezone'
        [varargout{1:2}]=gettimezone(FI,domain,Props);
        return
    case 'stations'
        varargout={readsts(FI,domain,Props,0)};
        return
    case 'subfields'
        varargout={{}};
        return
    otherwise
        [XYRead,DataRead,DataInCell]=gridcelldata(cmd);
end

DimFlag=Props.DimFlag;

% initialize and read indices ...
idx={[] [] 0 0 0};
fidx=find(DimFlag);
idx(fidx(1:length(varargin)))=varargin;

% select appropriate timestep ...
sz=getsize(FI,domain,Props);
if DimFlag(T_)
    if isempty(idx{T_})
        idx{T_}=sz(T_);
    end
    if isequal(idx{T_},0)
        idx{T_}=1:sz(T_);
    end
end

%========================= GENERAL CODE =======================================
allidx=zeros(size(sz));
for i=[M_ N_ K_]
    if DimFlag(i)
        if isequal(idx{i},0) || isequal(idx{i},1:sz(i))
            idx{i}=1:sz(i);
            allidx(i)=1;
        end
    end
end

if max(idx{T_})>sz(T_)
    error('Selected timestep (%i) larger than number of timesteps (%i) in file.',max(idx{T_}),sz(T_))
end

nT = length(idx{T_});
nM = length(idx{M_});
nN = length(idx{N_});
firstblock = 1;
for ib = 1:nT
    b1 = Props.SubFld1(idx{T_}(ib));
    if firstblock && XYRead
        [val1,y,x,t,TRI] = grib('read',FI,b1);
    else
        [val1,dumy,dumx,t,TRI] = grib('readdata',FI,b1);
    end
    val2 = [];
    %
    if isempty(TRI)
        if firstblock
            if XYRead
                Ans.X=x(idx{M_},idx{N_});
                Ans.Y=y(idx{M_},idx{N_});
            end
            if isempty(val2)
                Ans.Val = zeros(nT,nM,nN);
            else
                Ans.XComp = zeros(nT,nM,nN);
                Ans.YComp = zeros(nT,nM,nN);
            end
        end
        Ans.Time(ib) = t;
        if isempty(val2)
            Ans.Val(ib,:,:) = reshape(val1(idx{M_},idx{N_}),[1 nM nN]);
        else
            Ans.XComp(ib,:,:)=reshape(val1(idx{M_},idx{N_}),[1 nM nN]);
            Ans.YComp(ib,:,:)=reshape(val2(idx{M_},idx{N_}),[1 nM nN]);
        end
    else
        if firstblock
            if XYRead
                if ~allidx(M_)
                    Translate=zeros(sz(M_),1);
                    Translate(idx{M_})=1:length(idx{M_});
                    TRI = Translate(TRI);
                    TRI = TRI(all(TRI,2),:);
                end
                Ans.TRI=TRI;
                %
                Ans.XYZ = cat(4,x(idx{M_}),y(idx{M_}));
            end
            Ans.Val = zeros(nT,nM);
        end
        %
        Ans.Val(ib,:) = reshape(val1(idx{M_}),[1 nM]);
    end
    %
    % reshape if a single timestep is selected ...
    if DimFlag(T_) && isequal(size(idx{T_}),[1 1])
        Flds = {'Val','XComp','YComp'};
        for f = 1:length(Flds)
            fld = Flds{f};
            if isfield(Ans,fld)
                sz=size(Ans.(fld));
                sz=[sz(2:end) 1];
                Ans.(fld)=reshape(Ans.(fld),sz);
            end
        end
    end
%
    firstblock = 0;
end
Ans.XUnits = 'deg';
Ans.YUnits = 'deg';

varargout={Ans FI};
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function Out=infile(FI,domain)
T_=1; ST_=2; M_=3; N_=4; K_=5;
%======================== SPECIFIC CODE =======================================
PropNames={'Name'              'Units'  'DimFlag' 'DataInCell' 'NVal' 'VecType' 'Tri' 'SubFld1' 'SubFld2'};
DataProps={''                  ''      [0 0 0 0 0]  0           1     ''         0     []        []};
for b = 1:length(FI.Block)
    if FI.Block(b).Edition<=1
        Info = FI.Block(b).Info;
        DataProps(end+1,:) = DataProps(1,:);
        DataProps{end,1} = Info.ParamName;
        DataProps{end,2} = Info.ParamUnit;
        if isfield(Info.Grid,'Ni')
            if length(Info.Grid.Ni)>1
                DataProps{end,3} = [1 0 6 0 0];
                DataProps{end,7} = 1;
            else
                DataProps{end,3} = [1 0 1 1 0];
            end
        end
        DataProps{end,8} = b;
    end
end
%
DataProps(1,:)=[];
[b,i,j]=unique(DataProps(:,1));
compactDataProps = DataProps(i,:);
for i=1:length(b)
    compactDataProps{i,8} = cat(2,DataProps{j==i,8});
end
%
Out=cell2struct(compactDataProps,PropNames,2);
[Out.UseGrid]=deal(1);
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function sz=getsize(FI,domain,Props)
T_=1; ST_=2; M_=3; N_=4; K_=5;
sz=[0 0 0 0 0];

%======================== SPECIFIC CODE =======================================
sz(T_) = length(Props.SubFld1);
Info = FI.Block(Props.SubFld1(1)).Info;
if ~isfield(Info.Grid,'Ni')
    error('Grid type ''%s'' not supported.',Info.Grid.TypeName)
elseif length(Info.Grid.Ni)>1
    sz(M_) = sum(Info.Grid.Ni);
else
    sz(M_) = Info.Grid.Ni;
    sz(N_) = Info.Grid.Nj;
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function T=readtim(FI,domain,Props,t)
T_=1; ST_=2; M_=3; N_=4; K_=5;

%======================== SPECIFIC CODE =======================================
T = zeros(1,length(Props.SubFld1));
for i = 1:length(Props.SubFld1)
    Info = FI.Block(Props.SubFld1(i)).Info;
    T(i) = grib('time',Info);
end
if t~=0
    T = T(t);
end
% -----------------------------------------------------------------------------
