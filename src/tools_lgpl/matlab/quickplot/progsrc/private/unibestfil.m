function varargout=unibestfil(FI,domain,field,cmd,varargin)
%UNIBESTFIL QP support for UNIBEST files.
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
switch cmd,
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
        varargout={getsubfields(FI,Props,varargin{:})};
        return
    otherwise
        [XYRead,DataRead,DataInCell]=gridcelldata(cmd);
end

DimFlag=Props.DimFlag;
idx={[] [] 0 [] []};
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

% select appropriate spatial indices ...

if DimFlag(M_)
    if isequal(idx{M_},0)
        idx{M_}=1:sz(M_);
    end
end

% read grid ...
x=[];
y=[];
z=[];
if XYRead
    %======================== SPECIFIC CODE =======================================
    x=FI.X(idx{M_})';
    if FI.DafVersion>0
        s=x;
        x=FI.XYRef(1)+sin(FI.Orientation)*s;
        y=FI.XYRef(2)+cos(FI.Orientation)*s;
    end
    %========================= GENERAL CODE =======================================
end

T=[];
if Props.NVal==1
    [T,val1]=unibest('read',FI,idx{M_},Props.Quant1,idx{T_});
    val2=[];
elseif Props.NVal==2
    [T,valx]=unibest('read',FI,idx{M_},Props.Quant1,idx{T_});
    [T,valy]=unibest('read',FI,idx{M_},Props.Quant2,idx{T_});
    alf=FI.Orientation;
    val1=valx*sin(alf)+valy*cos(alf);
    val2=valx*cos(alf)-valy*sin(alf);
end

% reshape
if length(idx{T_})>1
    val1=permute(val1,[3 1 2]);
    if ~isempty(val2)
        val2=permute(val2,[3 1 2]);
    end
end

% generate output ...
if XYRead
    Ans.X=x;
    if ~isempty(y)
        Ans.Y=y;
    end
end
if Props.NVal==0
elseif isempty(val2)
    Ans.Val=val1;
else
    Ans.XComp=val1;
    Ans.YComp=val2;
end

% return time ...
if isfield(FI,'RefDate')
    T=FI.RefDate+T;
end
Ans.Time=T;

varargout={Ans FI};
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function Out=infile(FI,domain);

%======================== SPECIFIC CODE =======================================
PropNames={'Name'                   'Units'   'DimFlag' 'DataInCell' 'NVal' 'Quant1' 'Quant2' 'MNK'};
DataProps={'grid'                      ''       [0 0 1 0 0]  0         0       0      0        0
    '-------'                   ''       [3 0 1 0 0]  0         1      -1      0        1};
if isfield(FI,'RefDate')
    DataProps{2,3}=[1 0 1 0 0];
end
DataProps(3:3+FI.NQuant,:)=repmat(DataProps(2,:),FI.NQuant+1,1);
DataProps(3:2+FI.NQuant,1)=FI.Quant.LongName';
DataProps(3:2+FI.NQuant,2)=FI.Quant.Units';
for i=1:FI.NQuant
    DataProps{2+i,6}=i;
end
%======================== CONVERT CELL ARRAY TO STRUCT ========================
Out=cell2struct(DataProps,PropNames,2);

if FI.DafVersion>0
    Tbl={'ux10'   'uy10'   'velocity at 10cm'
        'uxmean' 'uymean' 'mean velocity'
        'sbotx'  'sboty'  'bottom transport'
        'ssusx'  'ssusy'  'susp. transport'
        'stotx'  'stoty'  'total transport'};
    for i=1:size(Tbl,1)
        xcomp=ustrcmpi(Tbl{i,1},FI.Quant.ShortName);
        ycomp=ustrcmpi(Tbl{i,2},FI.Quant.ShortName);
        if xcomp>0 & ycomp>0
            Out(end+1)=Out(1+xcomp);
            Out(end).Name=Tbl{i,3};
            Out(end).NVal=2;
            Out(end).Quant2=ycomp;
        end
    end
end
% -----------------------------------------------------------------------------

% -----------------------------------------------------------------------------
function subf=getsubfields(FI,Props,f)
subf={};
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function sz=getsize(FI,Props)
T_=1; ST_=2; M_=3; N_=4; K_=5;
sz=[0 0 0 0 0];

%======================== SPECIFIC CODE =======================================
if Props.DimFlag(M_)
    sz(M_)=FI.RecSize-1;
end;
if Props.DimFlag(T_)
    sz(T_)=FI.NTimes;
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function T=readtim(FI,Props,t)
%======================== SPECIFIC CODE =======================================
if t==0
    t=1:FI.NTimes;
end
T=unibest('read',FI,[],[],t);
if isfield(FI,'RefDate')
    T=FI.RefDate+T;
end
% -----------------------------------------------------------------------------
