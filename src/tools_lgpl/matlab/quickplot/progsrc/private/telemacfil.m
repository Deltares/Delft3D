function varargout=telemacfil(FI,domain,field,cmd,varargin)
%TELEMACFIL QP support for Telemac files.
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
    otherwise
        [XYRead,DataRead,DataInCell]=gridcelldata(cmd);
end

DimFlag=Props.DimFlag;

% initialize and read indices ...
idx={[] [] 0 0 1}; % <------------------------------ TEMPORARILY AS LONG AS DimFlag(K_)==5
fidx=find(DimFlag);
subf=getsubfields(FI,Props);
if isempty(subf)
    % initialize and read indices ...
    idx(fidx(1:length(varargin)))=varargin;
else
    % initialize and read indices ...
    Props.SubFld=varargin{1};
    idx(fidx(1:(length(varargin)-1)))=varargin(2:end);
end

% select appropriate timestep ...
sz=getsize(FI,Props);
for m_ = 1:5
    if DimFlag(m_)
        if m_==T_ && isempty(idx{m_})
            idx{m_}=sz(m_);
        end
        if isequal(idx{m_},0)
            idx{m_}=1:sz(m_);
        end
    end
end

% generate output ...
nTimes = length(idx{T_});
nPntLayer = length(idx{M_});
nLayer = length(idx{K_});
if XYRead
    X=FI.Discr.X;
    Y=FI.Discr.Y;
    if FI.Discr.NPntsPerElem==6
        X=reshape(X,[1 sz([M_ K_])]);
        Y=reshape(Y,[1 sz([M_ K_])]);
        %
        X=X(:,:,idx{K_});
        Y=Y(:,:,idx{K_});
        ntri = FI.Discr.NElem/(sz(K_)-1);
        TRI=FI.Discr.Elem(1:ntri,1:3);
        if isempty(idx{T_})
            idx{T_} = 1;
            nTimes = 1;
        end
        Z=telemac('read',FI,idx{T_},1);
        Z=reshape(Z,[nTimes sz([M_ K_])]);
        Z=Z(:,:,idx{K_});
    else
        X=reshape(X,[1 sz(M_)]);
        Y=reshape(Y,[1 sz(M_)]);
        TRI=FI.Discr.Elem;
        Z=[];
    end
    if nTimes>1
        X=repmat(X,[nTimes 1]);
        Y=repmat(Y,[nTimes 1]);
    end
    Ans.XYZ=cat(4,X,Y,Z);
    if idx{M_}~=0
        Translate=zeros(sz(M_),1);
        Translate(idx{M_})=1:nPntLayer;
        TRI = Translate(TRI);
        TRI = TRI(all(TRI,2),:);
        Ans.XYZ=Ans.XYZ(:,idx{M_},:,:);
    end
    Ans.TRI=TRI;
    
    Ans.XUnits='m';
    Ans.YUnits='m';
    Ans.ZUnits='m';
end

if DimFlag(K_)
    idxM = idx{M_}(:);
    idx{M_} = [];
    for ik = 1:length(idx{K_})
        k = idx{K_}(ik);
        idx{M_} = cat(1,idx{M_},idxM+sz(M_)*(k-1));
    end
end
switch Props.NVal
    case 0
    case 1
        Ans.Val=telemac('read',FI,idx{T_},Props.Val1,idx{M_});
        Ans.Val=reshape(Ans.Val,[nTimes nPntLayer nLayer]);
    otherwise
        Ans.XComp=telemac('read',FI,idx{T_},Props.Val1,idx{M_});
        Ans.XComp=reshape(Ans.XComp,[nTimes nPntLayer nLayer]);
        Ans.YComp=telemac('read',FI,idx{T_},Props.Val2,idx{M_});
        Ans.YComp=reshape(Ans.YComp,[nTimes nPntLayer nLayer]);
        if ~isempty(Props.Val3)
            Ans.ZComp=telemac('read',FI,idx{T_},Props.Val3,idx{M_});
            Ans.ZComp=reshape(Ans.ZComp,[nTimes nPntLayer nLayer]);
        end
end

% read time ...
T=readtim(FI,Props,idx{T_});
Ans.Time=T;

varargout={Ans FI};
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function Out=infile(FI,domain)
T_=1; ST_=2; M_=3; N_=4; K_=5;

%======================== SPECIFIC CODE =======================================
PropNames={'Name'                       'Units' 'DimFlag' 'DataInCell' 'NVal' 'VecType' 'Loc' 'ReqLoc' 'Tri' 'UseGrid' 'Val1'    'Val2' 'Val3'};
DataProps={'grid'                       ''       [0 0 6 0 0]  0          0     ''        ''    ''       1     1         0         []     []
    '-------'                    ''       [0 0 0 0 0]  0          0     ''        ''    ''       0     0         []        []     []};

%======================== SPECIFIC CODE ADD ===================================
T=3;
if isfield(FI,'RefTime')
    T=1;
end
if FI.Discr.NPntsPerElem==6
    DataProps{1,3}(T_)=T;
    DataProps{1,3}(K_)=1;
end
for i=1:length(FI.Var)
    DataProps(2+i,:)={FI.Var(i).Name ...
        lower(FI.Var(i).Unit)   [T 0 6 0 0]  0          1     ''        ''    ''       1     1         i         []     []};
    if FI.Discr.NPntsPerElem==6
        DataProps{2+i,3}(K_)=1;
    end
end
VNames={FI.Var.Name};
iu = strmatch('VELOCITY U',VNames);
iv = strmatch('VELOCITY V',VNames);
iw = strmatch('VELOCITY W',VNames);
nm='VELOCITY';
if isempty(iu) && isempty(iv)
    iu = strmatch('VITESSE U',VNames);
    iv = strmatch('VITESSE V',VNames);
    iw = strmatch('VITESSE W',VNames);
    nm='VITESSE';
end
if ~isempty(iu) && ~isempty(iv)
    DataProps(end+1,:)={nm     ...
        lower(FI.Var(iu).Unit)   [T 0 6 0 0]  0          2     ''        ''    ''       1     1         iu        iv     []};
    if ~isempty(iw)
        DataProps{end,5}=3;
        DataProps{end,end}=iw;
    end
    if FI.Discr.NPntsPerElem==6
        DataProps{end,3}(K_)=1;
    end
end

%======================== SPECIFIC CODE DIMENSIONS ============================
Out=cell2struct(DataProps,PropNames,2);

%======================== SPECIFIC CODE REMOVE ================================
%for i=size(Out,1):-1:1
%  Info=vs_disp(FI,Out(i).Group,Out(i).Val1);
%  if ~isempty(strmatch('---',Out(i).Name))
%  elseif ~isstruct(Info)
%    % remove references to non-stored data fields
%    Out(i)=[];
%  end
%end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function sz=getsize(FI,Props)
T_=1; ST_=2; M_=3; N_=4; K_=5;
sz=[0 0 0 0 0];

%======================== SPECIFIC CODE =======================================
if Props.DimFlag(M_)
    sz(M_)=FI.Discr.NPnts;
end
if Props.DimFlag(T_)
    sz(T_)=FI.NTimes;
end
if Props.DimFlag(K_)
    sz(K_)=FI.IParam(7);
    sz(M_)=sz(M_)/sz(K_);
end

% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function T=readtim(FI,Props,t)
T_=1; ST_=2; M_=3; N_=4; K_=5;

%======================== SPECIFIC CODE =======================================
if isequal(t,0),
    T=FI.Times;
else
    T=FI.Times(t);
end
if isfield(FI,'RefTime')
    T=T+FI.RefTime;
else
    T=T+1;
end
% -----------------------------------------------------------------------------
