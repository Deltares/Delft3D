function varargout=cfxfil(FI,domain,field,cmd,varargin)
%CFXFIL QP support for CFX4 files.
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
            varargout={{}};
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
            if DataInCell
                gidx{i}=1:sz(i)+1;
            else
                gidx{i}=idx{i};
            end
            allidx(i)=1;
        elseif ~isequal(idx{i},idx{i}(1):idx{i}(end))
            error('Only scalars or ranges allowed for index %i',i)
        else
            if DataInCell
                gidx{i}=[idx{i} idx{i}(end)+1];
            else
                gidx{i}=idx{i};
            end
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
dmp=FI.Encaps;
if XYRead

    %======================== SPECIFIC CODE =======================================
    if Props.Block==0
        x=cfx1block(dmp,FI.B1,'X COORDINATES',idx{T_});
        y=cfx1block(dmp,FI.B1,'Y COORDINATES',idx{T_});
        z=cfx1block(dmp,FI.B1,'Z COORDINATES',idx{T_});
        x=x(gidx{[M_ N_ K_]});
        y=y(gidx{[M_ N_ K_]});
        z=z(gidx{[M_ N_ K_]});
    else
        x=cfx('read',dmp,'X COORDINATES','block',Props.Block,Props.Phase,idx{T_});
        y=cfx('read',dmp,'Y COORDINATES','block',Props.Block,Props.Phase,idx{T_});
        z=cfx('read',dmp,'Z COORDINATES','block',Props.Block,Props.Phase,idx{T_});
        x=x{Props.Block};
        y=y{Props.Block};
        z=z{Props.Block};
        x=x(gidx{[M_ N_ K_]});
        y=y(gidx{[M_ N_ K_]});
        z=z(gidx{[M_ N_ K_]});
    end
    %========================= GENERAL CODE =======================================
end

val1=[];
val2=[];
val3=[];

vars={dmp.Variable.Name};
switch length(Props.Variable)
    case 1
        if Props.Block==0
            val1=cfx1block(dmp,FI.B1,vars{Props.Variable},idx{T_});
        else
            val1=cfx('read',dmp,vars{Props.Variable},'block',Props.Block,Props.Phase,idx{T_});
            val1=val1{Props.Block};
        end
    case 3
        if Props.Block==0
            val1=cfx1block(dmp,FI.B1,vars{Props.Variable(1)},idx{T_});
            val2=cfx1block(dmp,FI.B1,vars{Props.Variable(2)},idx{T_});
            val3=cfx1block(dmp,FI.B1,vars{Props.Variable(3)},idx{T_});
        else
            val1=cfx('read',dmp,vars{Props.Variable(1)},'block',Props.Block,Props.Phase,idx{T_});
            val2=cfx('read',dmp,vars{Props.Variable(2)},'block',Props.Block,Props.Phase,idx{T_});
            val3=cfx('read',dmp,vars{Props.Variable(3)},'block',Props.Block,Props.Phase,idx{T_});
            val1=val1{Props.Block};
            val2=val2{Props.Block};
            val3=val3{Props.Block};
        end
end
val1=val1(idx{[M_ N_ K_]});
if ~isempty(val2)
    val2=val2(idx{[M_ N_ K_]});
end
if ~isempty(val3)
    val3=val3(idx{[M_ N_ K_]});
end


% read time ...
T=readtim(FI,Props,idx{T_});

% generate output ...
if XYRead
    Ans.X=x;
    Ans.Y=y;
    if DimFlag(K_)
        Ans.Z=z;
    end
end
switch Props.NVal
    case 1
        Ans.Val=val1;
    case 2
        Ans.XComp=val1;
        Ans.YComp=val2;
    case 3
        Ans.XComp=val1;
        Ans.YComp=val2;
        Ans.ZComp=val3;
end

Ans.Time=T;

varargout={Ans FI};
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function Out=infile(FI,domain);
%
%======================== SPECIFIC CODE =======================================
PropNames={'Name'                         'DimFlag' 'DataInCell' 'NVal' 'Block' 'Phase' 'Variable' 'ThreeD'};
predefvars={'PRESSURE'          'pressure'
    'VOLUME FRACTION'   'volume fraction'
    'DENSITY'           'density'
    'VISCOSITY'         'viscosity'
    'K'                 'turbulent kinetic energy'
    'EPSILON'           'energy dissipation'};
DataProps=cell(0,length(PropNames));
dmp=FI.Encaps;
vars={dmp.Variable.Name};
for i=1:length(vars)
    i0=strmatch(vars{i},predefvars(:,1),'exact');
    if ~isempty(i0)
        vars{i}=predefvars{i0,2};
    end
end
B1=FI.B1;
if ~isempty(B1)
    u=strmatch('U VELOCITY',vars);
    v=strmatch('V VELOCITY',vars);
    w=strmatch('W VELOCITY',vars);
    i0=0;
    if ~isempty(u) & ~isempty(v) & ~isempty(w)
        DataProps(end+1,:)={'velocity'                    [5 0 1 1 1]  1         3    0  1  [1 2 3] 1};
        i0=3;
    end
    for i=i0+1:length(vars)
        DataProps(end+1,:)={vars{i}                    [5 0 1 1 1]  1         1    0  1  i 1};
    end
    if length(dmp.Block)>1
        DataProps(end+1,:)={'-------'                    [0 0 0 0 0]  0          0   0  0  0 1};
    end
end
if length(dmp.Block)>1
    for b=1:length(dmp.Block)
        u=strmatch('U VELOCITY',vars);
        v=strmatch('V VELOCITY',vars);
        w=strmatch('W VELOCITY',vars);
        i0=0;
        blk=[dmp.Block(b).Name ':'];
        if ~isempty(u) & ~isempty(v) & ~isempty(w)
            DataProps(end+1,:)={[blk 'velocity']                    [5 0 1 1 1]  1         3    b  1  [1 2 3] 1};
            i0=3;
        end
        for i=i0+1:length(vars)
            DataProps(end+1,:)={[blk vars{i}]                    [5 0 1 1 1]  1         1    b  1  i 1};
        end
        if b<length(dmp.Block)
            DataProps(end+1,:)={'-------'                    [0 0 0 0 0]  0          0   0  0  0 1};
        end
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
if Props.Block==0
    sz([M_ N_ K_])=FI.B1.IJK;
else
    sz(M_)=FI.Encaps.Block(Props.Block).I;
    sz(N_)=FI.Encaps.Block(Props.Block).J;
    sz(K_)=FI.Encaps.Block(Props.Block).K;
end
if Props.DimFlag(T_)
    sz(T_)=length(FI.Encaps.TimeStep);
end
if Props.DimFlag(ST_)
    sz(ST_)=0;
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function T=readtim(FI,Props,t)
%======================== SPECIFIC CODE =======================================
T=FI.Encaps.Time(:);
if ~isequal(t,0)
    T=T(t);
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function S=readsts(FI,Props,t)

%======================== SPECIFIC CODE =======================================
S={};
% -----------------------------------------------------------------------------
