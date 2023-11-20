function varargout=pcrasterfil(FI,domain,field,cmd,varargin)
%PCRASTERFIL QP support for PC Raster files.
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
        varargout={getsize(FI,Props)};
        return
    case 'times'
        varargout={readtim(FI,Props,varargin{:})};
        return
    case 'timezone'
        [varargout{1:2}]=gettimezone(FI,domain,Props);
        return
    case 'stations'
        varargout={{}};
        return
    case 'subfields'
        varargout={{}};
        return
    case 'plotoptions'
        varargout = {[]};
        return
    case 'plot'
        PlotIt=1;
        Parent=varargin{1};
        Ops=varargin{2};
        hOld=varargin{3};
        hNew=lddplot(FI,Parent);
        set(hNew,'color',Ops.colour);
        setaxesprops(Parent,'X-Y',{},{'',''})
        if nargout>1
            varargout={hNew FI};
        else
            varargout={hNew};
        end
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
sz=getsize(FI,Props);
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
ind=cell(1,5);
ind{2}=1;
for i=[M_ N_ K_]
    if DimFlag(i)
        if isequal(idx{i},0) | isequal(idx{i},1:sz(i))
            idx{i}=1:sz(i);
            allidx(i)=1;
        elseif ~isequal(idx{i},idx{i}(1):idx{i}(end))
            error('Only scalars or ranges allowed for index %i',i)
        end
    end
end

if max(idx{T_})>sz(T_)
    error('Selected timestep (%i) larger than number of timesteps (%i) in file.',max(idx{T_}),sz(T_))
end;

% read grid ...
x=[];
y=[];
z=[];
if XYRead

    %======================== SPECIFIC CODE =======================================
    if isequal(FI.YDir,'from bottom to top')
        sgny=-1;
    else
        sgny=1;
    end
    if XYRead & DataInCell
        eidx=idx;
        eidx{M_}(end+1)=eidx{M_}(end)+1;
        eidx{N_}(end+1)=eidx{N_}(end)+1;
        x=repmat(FI.Offset(1)+(0:FI.Size(2))*FI.CellSize(1),FI.Size(1)+1,1);
        y=repmat((FI.Offset(2)+sgny*(0:FI.Size(1))*FI.CellSize(2))',1,FI.Size(2)+1);
        x=x(eidx{[M_ N_]});
        y=y(eidx{[M_ N_]});
    else
        x=repmat(FI.Offset(1)+((1:FI.Size(2))-0.5)*FI.CellSize(1),FI.Size(1),1);
        y=repmat((FI.Offset(2)+sgny*((1:FI.Size(1))-0.5)*FI.CellSize(2))',1,FI.Size(2));
        x=x(idx{[M_ N_]});
        y=y(idx{[M_ N_]});
    end
    if DimFlag(K_)
        z=ones(size(x));
    end
    %========================= GENERAL CODE =======================================
end

if sz(T_)>1
    newf=pcraster('field',FI,idx{T_});
    val1=newf.Data;
    T=idx{T_};
else
    val1=FI.Data;
    T=[];
end
val1=val1(idx{[M_ N_]});
val2=[];

if isnumeric(Props.Val1)
    if Props.Val1==-1
        if strcmp(FI.PCRType,'boolean')
            val1=~val1;
        else
            val1=-val1;
        end
    else
        mask = val1==Props.Val1;
        val1(mask) = 1;
        val1(~mask) = NaN;
    end
end

% read time ...
%T=readtim(FI,Props,idx{T_});

% generate output ...
if Props.NVal==6
    Ans.Classes = FI.Table(:,2);
end
if XYRead
    Ans.X=x;
    Ans.Y=y;
    if DimFlag(K_)
        Ans.Z=z;
    end
end
if Props.NVal==0
elseif isempty(val2)
    Ans.Val=val1;
else
    Ans.XComp=val1;
    Ans.YComp=val2;
end

Ans.Time=T;

varargout={Ans FI};
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function Out=infile(FI,domain)
%
%======================== SPECIFIC CODE =======================================
PropNames={'Name'                         'DimFlag' 'DataInCell' 'NVal' 'VecType' 'Loc' 'ReqLoc' 'Loc3D' 'Group'          'Val1'          'Val2'};
if isequal(FI.PCRType,'ldd')
    DataProps={'drainage plot'               [5 0 4 4 0]  0        -1     ''        'z'   'z'      'c'     ''               ''              ''    };
else
    datatitle=FI.Title;
    if isempty(datatitle)
        datatitle=FI.FileName;
        dt=max(strfind(datatitle,filesep));
        datatitle=datatitle(dt+1:end);
    end
    DataProps={datatitle                     [5 0 1 1 0]  1         1     ''        'z'   'z'      'c'     ''               ''              ''
        ['negative ',datatitle]              [5 0 1 1 0]  1         1     ''        'z'   'z'      'c'     ''               -1              ''    };
    switch FI.PCRType
        case 'boolean'
            DataProps{2,1} = ['negated ',datatitle];
        case 'nominal'
            for i = size(FI.Table,1):-1:1
                DataProps(2+i,:) = DataProps(1,:);
                DataProps{2+i,1} = FI.Table{i,2};
                DataProps{2+i,end-1} = FI.Table{i,1};
                DataProps{2+i,4} = 5;
            end
            DataProps(2,1:4) = {'-------' [0 0 0 0 0] 0 0};
            DataProps(1,4) = {6};
    end
end
Out=cell2struct(DataProps,PropNames,2);
%======================== SPECIFIC CODE REMOVE ================================

%======================== SPECIFIC CODE ADD ===================================

% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function sz=getsize(FI,Props)
T_=1; ST_=2; M_=3; N_=4; K_=5;
sz=[0 0 0 0 0];

%======================== SPECIFIC CODE =======================================
if Props.DimFlag(M_) & Props.DimFlag(N_)
    sz([M_ N_])=FI.Size;
end;
if Props.DimFlag(K_)
    sz(K_)=1;
end
if Props.DimFlag(T_)
    if isfield(FI,'Series')
        sz(T_)=length(FI.Series);
    else
        sz(T_)=1;
    end
end
if Props.DimFlag(ST_)
    sz(ST_)=0;
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function T=readtim(FI,Props,t)
%======================== SPECIFIC CODE =======================================
T=now;
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function S=readsts(FI,Props,t)

%======================== SPECIFIC CODE =======================================
S={};
% -----------------------------------------------------------------------------
