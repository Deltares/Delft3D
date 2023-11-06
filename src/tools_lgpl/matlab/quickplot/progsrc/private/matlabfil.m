function varargout=matlabfil(FI,domain,field,cmd,varargin)
%MATLABFIL QP support for Matlab files saved by QuickPlot.
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
        varargout={getsubfields(FI,Props,varargin{:})};
        return
    otherwise
        [XYRead,DataRead,DataInCell]=gridcelldata(cmd);
end

DimFlag=Props.DimFlag;

% initialize and read indices ...
idx={[] [] [] [] []};
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

szV=getsize(FI,Props);
xidx=idx;
for i=1:5
    if isequal(idx{i},0)
        idx{i}=1:szV(i);
    end
    xidx{i}=idx{i};
    if ~isempty(idx{i}) & i>=M_
        xidx{i}(end+1)=idx{i}(end)+1;
    end
end

% grid ... (initialize Ans first)
if XYRead
    if isfield(FI,'TRI')
        Ans.XYZ=FI.XYZ(idx{T_},idx{M_},idx{K_},:);
        TRI=FI.TRI;
        if ~isequal(idx{M_},1:szV(M_))
            Translate=zeros(szV(M_),1);
            Translate(idx{M_})=1:length(idx{M_});
            TRI = Translate(TRI);
            TRI = TRI(all(TRI,2),:);
        end
        Ans.TRI=TRI;
    else
        Ans.X=[];
        for x='XYZ'
            if isfield(FI,x)
                XCrd=getfield(FI,x);
                szX=size(XCrd);
                all_idx=repmat({':'},1,ndims(XCrd));
                shift=0;
                if length(szX)==length(szV) % szV(T_)>1
                    shift=1;
                    if size(XCrd,1)>1
                        all_idx(1)=idx(T_);
                    end
                end
                for m_=[M_ N_ K_]
                    lm_=shift+m_-M_+1;
                    if lm_>length(szX)
                        % dimension does not exist
                    elseif szX(lm_)>szV(m_)
                        all_idx(lm_)=xidx(m_);
                    else
                        all_idx(lm_)=idx(m_);
                    end
                end
                Ans=setfield(Ans,x,XCrd(all_idx{:}));
                xUnits=[x 'Units'];
                if isfield(FI,xUnits)
                    Ans=setfield(Ans,xUnits,getfield(FI,xUnits));
                end
            end
        end
    end
end

DataRead = DataRead & (isfield(FI,'Val') | isfield(FI,'XComp'));
if DataRead
    if isfield(FI,'TRI')
        all_idx=idx([T_ M_ K_]);
        if isempty(idx{K_})
            all_idx{3}=1;
        end
    else
        if isfield(FI,'Val')
            Val=FI.Val;
            Ans.Val=[];
        elseif isfield(FI,'XComp')
            Val=FI.XComp;
            Ans.XComp=[];
        else
            Val=[];
            Ans.Time=[];
        end
        szVal=size(Val);
        all_idx=repmat({':'},1,ndims(Val));
        shift=0;
        if szV(T_)>1
            shift=1;
            all_idx(1)=idx(T_);
        end
        for m_=[M_ N_ K_]
            lm_=shift+m_-M_+1;
            if lm_>length(szVal)
                % dimension does not exist
            else
                all_idx(lm_)=idx(m_);
            end
        end
    end
    if isfield(FI,'Val')
        Ans.Val=FI.Val(all_idx{:});
    elseif isfield(FI,'XComp')
        Ans.XComp=FI.XComp(all_idx{:});
        if isfield(FI,'YComp')
            Ans.YComp=FI.YComp(all_idx{:});
        end
        if isfield(FI,'ZComp')
            Ans.ZComp=FI.ZComp(all_idx{:});
        end
    end
end

if szV(T_)>1 & length(idx{T_})==1 & ~isfield(FI,'TRI')
    if length(szX) == length(szV)
        for fldc={'X','Y','Z'};
            fld=fldc{1};
            if isfield(Ans,fld)
                Fld=getfield(Ans,fld);
                newsize=size(Fld);
                newsize(1)=[];
                newsize(end+1)=1;
                Ans=setfield(Ans,fld,reshape(Fld,newsize));
            end
        end
    end
    for fldc={'XComp','YComp','ZComp','Val'};
        fld=fldc{1};
        if isfield(Ans,fld)
            Fld=getfield(Ans,fld);
            newsize=size(Fld);
            newsize(1)=[];
            newsize(end+1)=1;
            Ans=setfield(Ans,fld,reshape(Fld,newsize));
        end
    end
end
if isempty(FI.Time)
    Ans.Time=[];
else
    Ans.Time=FI.Time(idx{T_});
end

varargout={Ans FI};
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function Out=infile(FI,domain);
T_=1; ST_=2; M_=3; N_=4; K_=5;
%======================== SPECIFIC CODE =======================================
PropNames={'Name'              'Units'  'DimFlag' 'DataInCell' 'NVal' 'SubFld' 'MNK' 'Tri'};
DataProps={abbrevfn(FI.FileName) ''     [1 0 1 1 1]    0         0    []       0      0 };
Out=cell2struct(DataProps,PropNames,2);
if isfield(FI,'Name')
    Out.Name=FI.Name;
end
if isfield(FI,'Units')
    Out.Units=FI.Units;
end
if isfield(FI,'Val')
    Out.NVal=1;
    szV=size(FI.Val);
elseif isfield(FI,'XComp')
    Out.NVal=1;
    szV=size(FI.XComp);
    if isfield(FI,'YComp')
        Out.NVal=Out.NVal+1;
    end
    if isfield(FI,'ZComp')
        Out.NVal=Out.NVal+1;
    end
else
    szV=[];
end
if isfield(FI,'TRI')
    Out.Tri=1;
    Out.DimFlag(M_)=6;
    Out.DimFlag(N_)=0;
    if size(FI.XYZ,3)==1
        Out.DimFlag(K_)=1;
    end
else
    szX=size(FI.X);
    if length(FI.Time)>1
        szX=szX(2:end);
        szV=szV(2:end);
    end
    if ~isequal(szV,szX)
        Out.DataInCell=2;
    end
    if Out.NVal>0
        if length(szV)<=1
            Out.DimFlag(N_)=0;
        end
        if length(szV)<=2
            Out.DimFlag(K_)=0;
        end
    else
        if length(szX)<=1
            Out.DimFlag(N_)=0;
        end
        if length(szX)<=2
            Out.DimFlag(K_)=0;
        end
    end
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function subf=getsubfields(FI,Props,f)
%======================== SPECIFIC CODE =======================================
subf={};
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function sz=getsize(FI,Props)
T_=1; ST_=2; M_=3; N_=4; K_=5;
sz=[0 0 0 0 0];
%======================== SPECIFIC CODE =======================================
sz(T_)=length(FI.Time);
if Props.NVal>0
    if isfield(FI,'Val')
        szV=size(FI.Val);
    elseif isfield(FI,'XComp')
        szV=size(FI.XComp);
    else
        szV=[];
    end
else
    szV=size(FI.X);
end
if isfield(FI,'TRI') & ~isempty(szV)
    sz(M_)=szV(2);
    sz(K_)=szV(3);
else
    if sz(T_)>1
        szV=szV(2:end);
    end
    switch length(szV)
        case 1
            sz(M_)=szV;
        case 2
            sz([M_ N_])=szV;
        case 3
            sz([M_ N_ K_])=szV;
    end
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function T=readtim(FI,Props,t)
%======================== SPECIFIC CODE =======================================
if t==0
    T=FI.Time;
else
    T=FI.Time(t);
end
% -----------------------------------------------------------------------------
