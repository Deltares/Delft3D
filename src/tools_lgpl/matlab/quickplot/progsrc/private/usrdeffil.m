function varargout=usrdeffil(FI,domain,field,cmd,varargin)
%USRDEFFIL QP support for user defined variables.
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
%   Copyright (C) 2011-2025 Stichting Deltares.                                     
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
        varargout={readsts(FI,Props,varargin{:})};
        return
    case 'subfields'
        varargout={{}};
        return
    case 'plotoptions'
        varargout = {[]};
        return
    case 'plot'
        Parent = varargin{1};
        Ops = varargin{2};
        hOld=varargin{3};
        SubSelected = varargin(4:end);
        Selected = FI.Selected;
        j=0;
        for i = 1:5
            if FI.DimFlag(i)~=0
                j=j+1;
                Selected{i}=Selected{i}(SubSelected{j});
            end
        end
        Selected(FI.DimFlag==0)=[];
        [Chk,hNew,FI.FileInfo]=qp_getdata(FI.FileInfo,FI.Domain,FI.Props,'plot',Parent,Ops,hOld,Selected{:});
        varargout={hNew FI};
        return
    otherwise
        [XYRead,DataRead,DataInCell]=gridcelldata(cmd);
end

DimFlag=Props.DimFlag;

% initialize and read indices ...
idx={[] [] [] [] []};
idx(DimFlag~=0)={0};
fidx=find(DimFlag);
idx(fidx(1:length(varargin)))=varargin;

i=Props.Fld;
[Ans,FIi]=getdata(FI(i),cmd,idx);
if Props.DimFlag(T_)
    Ans.Time=readtim(FI,Props,idx{1});
end
FI(i)=FIi;

varargout={Ans FI};
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function Out=infile(FI,domain)
T_=1; ST_=2; M_=3; N_=4; K_=5;
%======================== SPECIFIC CODE =======================================
PropNames={'Name' 'Units' 'Geom' 'Coords' 'DimFlag' 'DataInCell' 'NVal' 'Fld' 'Tri' 'ClosedPoly'};
DataProps={''     ''      ''     ''       [0 0 1 1 0]  0          0      0     0     0};
DataProps(1,:)=[];

for i=1:length(FI)
    DataProps{i,1}=FI(i).Name;
    DataProps{i,2}=FI(i).Units;
    DataProps{i,3}=FI(i).Geom;
    DataProps{i,4}=FI(i).Coords;
    DataProps{i,5}=FI(i).DimFlag;
    if isfield(FI(i).Props,'DataInCell')
        DataProps{i,6}=FI(i).Props.DataInCell;
        DataProps{i,3}=FI(i).Geom;
    else
        DataProps{i,6}=0;
    end
    DataProps{i,7}=FI(i).Props.NVal;
    DataProps{i,8}=i;
    if isfield(FI(i),'Geom') && ~isempty(FI(i).Geom)
        DataProps{i,3}=FI(i).Geom;
        DataProps{i,9}=strcmp(FI(i).Geom, 'TRI');
    elseif isfield(FI(i),'Tri') && ~isempty(FI(i).Tri)
        DataProps{i,9}=FI(i).Tri;
        DataProps{i,3}='TRI';
    else
        DataProps{i,9}=0;
    end
    DataProps{i,10}=FI(i).ClosedPoly;
end
Out=cell2struct(DataProps,PropNames,2);


% -----------------------------------------------------------------------------
function [Ans,NProps]=getdata(Props,cmd,sel)
T_=1; ST_=2; M_=3; N_=4; K_=5;
%======================== SPECIFIC CODE =======================================
TDep=0;
kIndex=3;
if isfield(Props,'Tri') && Props.Tri
    TDep=1;
    kIndex=2;
elseif Props.DimFlag(T_)
    if length(sel{T_})>1
        TDep=1;
    elseif sel{T_}==0
        szi=getsize([],Props);
        if szi(T_)>1
            TDep=1;
        end
    end
end
FieldList = {'XComp','YComp','ZComp','Val','XDamVal','YDamVal'};
if isequal(Props.FileInfo,'operator')
    P=Props.Props.Data{1};
    Oper=Props.Props.Oper;
    switch Oper
        case {'series: A,B'}
            t=sel{T_};
            first=1;
            off=0;
            for i=1:2
                P=Props.Props.Data{i};
                szi=getsize([],P);
                szit=szi(T_);
                if t==0
                    [Ansi,FI]=getdata(P,cmd,sel);
                    Props.Props.Data{i}=FI;
                else
                    ti=t(t>off)-off;
                    ti(ti>szit)=[];
                    sel{T_}=ti;
                    if ~isempty(sel{T_})
                        [Ansi,FI]=getdata(P,cmd,sel);
                        Props.Props.Data{i}=FI;
                    else
                        Ansi=[];
                    end
                end
                if isempty(Ansi)
                elseif first
                    Ans=Ansi;
                    first=0;
                else
                    if isfield(Ans,'XComp')
                        Ans.XComp=cat(1,Ans.XComp,Ansi.XComp);
                    end
                    if isfield(Ans,'YComp')
                        Ans.YComp=cat(1,Ans.YComp,Ansi.YComp);
                    end
                    if isfield(Ans,'ZComp')
                        Ans.ZComp=cat(1,Ans.ZComp,Ansi.ZComp);
                    end
                    if isfield(Ans,'Val')
                        Ans.Val=cat(1,Ans.Val,Ansi.Val);
                    end
                end
                off=off+szit;
            end
        case {'A+B','A-B','A*B','A/B','max(A,B)','min(A,B)','vector(A,B)','vec_M&D(A,B)','A under condition B','f(A,B) = user defined'}
            sz1=getsize([],P);
            P2=Props.Props.Data{2};
            sz2=getsize([],P2);
            cmd2=cmd;
            if any(sz1([M_ N_ K_])<sz2([M_ N_ K_])) % somewhere a 1 to N mapping, so use 2nd grid
                if strcmp(cmd(1:4),'grid')
                    cmd(1:4)=[];
                end
                griduse=2;
            else % default (N to N mappings or N to 1 mapping), use 1st grid
                if strcmp(cmd(1:4),'grid')
                    cmd2(1:4)=[];
                end
                griduse=1;
            end
            [Ans,FI]=getdata(P,cmd,sel);
            Props.Props.Data{1}=FI;
            [Ans2,FI]=getdata(P2,cmd2,sel);
            Props.Props.Data{2}=FI;
            if griduse~=1
                for c = {'FaceNodeConnect','ValueLocation','TRI','XYZ','X','Y','Z'}
                    f = c{1};
                    if isfield(Ans2,f)
                        Ans.(f) = Ans2.(f);
                    end
                end
            end
            if isfield(Ans,'Val')
                sz1=size(Ans.Val);
                vec1=0;
            elseif isfield(Ans,'XComp')
                sz1=size(Ans.XComp);
                vec1=1;
            elseif isfield(Ans,'XDamVal')
                sz1=size(Ans.XDamVal);
                vec1=1;
            end
            if isfield(Ans2,'Val')
                sz2=size(Ans2.Val);
                vec2=0;
            elseif isfield(Ans2,'XComp')
                sz2=size(Ans2.XComp);
                vec2=1;
            elseif isfield(Ans2,'XDamVal')
                sz2=size(Ans2.XDamVal);
                vec2=1;
            end
            if length(sz1)~=length(sz2)
                jj=max(length(sz1),length(sz2));
                if jj>length(sz1)
                    sz1=[sz1 ones(1,jj-length(sz1))];
                else
                    sz2=[sz2 ones(1,jj-length(sz2))];
                end
            end
            sz=max(sz1,sz2);
            res1=sz./max(sz1,1);
            if ~all(res1==1)
                if vec1
                    Ans.XComp=repmat(Ans.XComp,res1);
                    if isfield(Ans,'YComp');
                        Ans.YComp=repmat(Ans.YComp,res1);
                    elseif isfield(Ans,'YDamVal');
                        Ans.YComp=repmat(Ans.YDamVal,res1);
                    end
                    if isfield(Ans,'ZComp');
                        Ans.ZComp=repmat(Ans.ZComp,res1);
                    end
                else
                    Ans.Val=repmat(Ans.Val,res1);
                end
            end
            res2=sz./max(sz2,1);
            if ~all(res2==1)
                if vec2
                    Ans2.XComp=repmat(Ans2.XComp,res2);
                    if isfield(Ans2,'YComp');
                        Ans2.YComp=repmat(Ans2.YComp,res2);
                    elseif isfield(Ans2,'YDamVal');
                        Ans2.YComp=repmat(Ans2.YDamVal,res2);
                    end
                    if isfield(Ans2,'ZComp');
                        Ans2.ZComp=repmat(Ans2.ZComp,res2);
                    end
                else
                    Ans2.Val=repmat(Ans2.Val,res2);
                end
            end
            special=0;
            switch Oper
                case 'A+B'
                    OpFun='plus';
                case 'A-B'
                    OpFun='minus';
                case 'A*B'
                    OpFun='times';
                case 'A/B'
                    OpFun='rdivide';
                    if isfield(Ans2,'XComp');
                        Ans2.XComp(Ans2.XComp==0)=NaN;
                    end
                    if isfield(Ans2,'YComp');
                        Ans2.YComp(Ans2.YComp==0)=NaN;
                    end
                    if isfield(Ans2,'ZComp');
                        Ans2.ZComp(Ans2.ZComp==0)=NaN;
                    end
                    if isfield(Ans2,'Val');
                        Ans2.Val(Ans2.Val==0)=NaN;
                    end
                case 'max(A,B)'
                    OpFun='max';
                case 'min(A,B)'
                    OpFun='min';
                case {'vector(A,B)','vec_M&D(A,B)','A under condition B','f(A,B) = user defined'}
                    special=1;
            end
            if special
                switch Oper
                    case 'vector(A,B)'
                        Ans.XComp=Ans.Val;
                        Ans.YComp=Ans2.Val;
                        Ans=rmfield(Ans,'Val');
                    case 'vec_M&D(A,B)'
                        Ans.XComp=Ans.Val.*cos(Ans2.Val);
                        Ans.YComp=Ans.Val.*sin(Ans2.Val);
                        Ans=rmfield(Ans,'Val');
                    case 'A under condition B'
                        cond=Props.Props.Data{3};
                        val=Ans2.Val;
                        val=realset('keep',cond,val);
                        if vec1
                            Ans.XComp(isnan(val))=NaN;
                            Ans.YComp(isnan(val))=NaN;
                        else
                            Ans.Val(isnan(val))=NaN;
                        end
                    case 'f(A,B) = user defined'
                        fun = Props.Props.Data{3};
                        for i = 1:length(FieldList)
                            f = FieldList{i};
                            if isfield(Ans,f) && isfield(Ans2,f)
                                Ans.(f)=usereval(fun,Ans.(f),Ans2.(f));
                            elseif isfield(Ans,f) && isfield(Ans2,'Val')
                                Ans.(f)=usereval(fun,Ans.(f),Ans2.Val);
                            elseif isfield(Ans,'Val') && isfield(Ans2,f)
                                Ans.(f)=usereval(fun,Ans.Val,Ans2.(f));
                            end
                        end
                end
            else
                for i = 1:length(FieldList)
                    f = FieldList{i};
                    if isfield(Ans,f) && isfield(Ans2,f)
                        Ans.(f)=feval(OpFun,Ans.(f),Ans2.(f));
                    elseif isfield(Ans,f) && isfield(Ans2,'Val')
                        Ans.(f)=feval(OpFun,Ans.(f),Ans2.Val);
                    elseif isfield(Ans,'Val') && isfield(Ans2,f)
                        Ans.(f)=feval(OpFun,Ans.Val,Ans2.(f));
                    end
                end
                if isfield(Ans,'Val') && isfield(Ans,'XComp') % var1 is scalar, var2 is vector
                    Ans=rmfield(Ans,'Val');
                end
            end
        case {'+ constant','* constant','^ constant','max(A,constant)','min(A,constant)'}
            [Ans,FI]=getdata(P,cmd,sel);
            Props.Props.Data{1}=FI;
            c=Props.Props.Data{2};
            vec1=~isfield(Ans,'Val');
            switch Oper
                case 'max(A,constant)'
                    OpFun='max';
                case 'min(A,constant)'
                    OpFun='min';
                case '+ constant'
                    OpFun='plus';
                case '* constant'
                    OpFun='times';
                case '^ constant'
                    OpFun='power';
                    if abs(c)<1
                        if isfield(Ans,'XComp');
                            Ans.XComp(Ans.XComp<0)=NaN;
                        end
                        if isfield(Ans,'YComp');
                            Ans.YComp(Ans.YComp<0)=NaN;
                        end
                        if isfield(Ans,'ZComp');
                            Ans.ZComp(Ans.ZComp<0)=NaN;
                        end
                        if isfield(Ans,'Val');
                            Ans.Val(Ans.Val<0)=NaN;
                        end
                    end
                    if c<0
                        if isfield(Ans,'XComp');
                            Ans.XComp(Ans.XComp==0)=NaN;
                        end
                        if isfield(Ans,'YComp');
                            Ans.YComp(Ans.YComp==0)=NaN;
                        end
                        if isfield(Ans,'ZComp');
                            Ans.ZComp(Ans.ZComp==0)=NaN;
                        end
                        if isfield(Ans,'Val');
                            Ans.Val(Ans.Val==0)=NaN;
                        end
                    end
            end
            if vec1
                Ans.XComp=feval(OpFun,Ans.XComp,c);
                if isfield(Ans,'YComp');
                    Ans.YComp=feval(OpFun,Ans.YComp,c);
                end
                if isfield(Ans,'ZComp');
                    Ans.ZComp=feval(OpFun,Ans.ZComp,c);
                end
            else
                Ans.Val=feval(OpFun,Ans.Val,c);
            end
            switch Oper
                case '^ constant'
                    Ans.Val=real(Ans.Val);
            end
        case {'10log','abs'}
            switch Oper
                case '10log'
                    fun='log10';
                    filter_negative=1;
                case 'abs'
                    fun='abs';
                    filter_negative=0;
            end
            [Ans,FI]=getdata(P,cmd,sel);
            Props.Props.Data{1}=FI;
            for i = 1:length(FieldList)
                f = FieldList{i};
                if isfield(Ans,f);
                    if filter_negative
                        Ans.(f)(Ans.(f)<=0)=NaN;
                    end
                    Ans.(f)=feval(fun,Ans.(f));
                end
            end
        case {'f(A) = user defined'}
            fun=Props.Props.Data{2};
            [Ans,FI]=getdata(P,cmd,sel);
            Props.Props.Data{1}=FI;
            for i = 1:length(FieldList)
                f = FieldList{i};
                if isfield(Ans,f)
                    Ans.(f)=usereval(fun,Ans.(f));
                end
            end
        case {'max m','alg.mean m','min m','sum m', ...
                'max n','alg.mean n','min n','sum n', ...
                'max k','alg.mean k','min k','sum k'}
            switch Oper(end)
                case 'm'
                    m_ = M_;
                    dIndex = 1;
                case 'n'
                    m_ = N_;
                    dIndex = 2;
                case 'k'
                    m_ = K_;
                    dIndex = kIndex;
            end
            sel{m_}=0;
            [Ans,FI]=getdata(P,cmd,sel);
            Props.Props.Data{1}=FI;
            switch Oper(1:end-2)
                case 'max'
                    Ans.Val=max(Ans.Val,[],TDep+dIndex);
                    Ans=avgcoord(Ans,dIndex);
                case 'alg.mean'
                    Ix=isnan(Ans.Val);
                    Ans.Val(Ix)=0;
                    Ans.Val=sum(Ans.Val,TDep+dIndex)./max(sum(~Ix,TDep+dIndex),1);
                case 'sum'
                    Ix=isnan(Ans.Val);
                    Ans.Val(Ix)=0;
                    Ans.Val=sum(Ans.Val,TDep+dIndex);
                case 'min'
                    Ans.Val=min(Ans.Val,[],TDep+dIndex);
            end
            Ans=avgcoord(Ans,dIndex);
        case {'flip m','flip n','flip k'}
            switch Oper(end)
                case 'm'
                    m_ = M_;
                    dIndex = 1;
                case 'n'
                    m_ = N_;
                    dIndex = 2;
                case 'k'
                    m_ = K_;
                    dIndex = kIndex;
            end
            sel{m_}=0;
            [Ans,FI]=getdata(P,cmd,sel);
            Props.Props.Data{1}=FI;
            for i=1:length(FieldList)
                f = FieldList{i};
                if isfield(Ans,f)
                    data = Ans.(f);
                    dims = repmat({':'},1,ndims(data));
                    dims{dIndex} = size(data,dIndex):-1:1;
                    data = data(dims{:});
                    Ans.(f) = data;
                    data = [];
                end
            end
        case {'m index','n index','k index'}
            switch Oper(1)
                case 'm'
                    m_ = M_;
                    dIndex = 1;
                case 'n'
                    m_ = N_;
                    dIndex = 2;
                case 'k'
                    m_ = K_;
                    dIndex = kIndex;
            end
            [Ans,FI]=getdata(P,cmd,sel);
            Props.Props.Data{1}=FI;
            szP = getsize(P.FileInfo,P);
            %
            szData = [];
            for i=1:length(FieldList)
                f = FieldList{i};
                if isfield(Ans,f)
                    szData = size(Ans.(f));
                    Ans = rmfield(Ans,f);
                end
            end
            if isempty(szData) % NVal=0
                szData = [1 1];
                mm_ = [M_ 1; N_ 2; K_ kIndex];
                for i = 1:3
                    D_ = mm_(i,1);
                    DI = mm_(i,2);
                    if szP(D_)>0
                        if isequal(sel{D_},0)
                            szData(DI) = szP(D_);
                        else
                            szData(DI) = length(sel{D_});
                        end
                    end
                end
            end
            szIdx = ones(size(szData));
            if isequal(sel{m_},0)
                szM = szP(m_);
                idx = 1:szM;
            else
                idx = sel{m_};
                szM = length(idx);
            end
            szIdx(dIndex) = szM;
            idx = reshape(idx,szIdx);
            Ans.Val = repmat(idx,szData./szIdx);
        case 'magnitude'
            [Ans,FI]=getdata(P,cmd,sel);
            Props.Props.Data{1}=FI;
            Ans.Val=Ans.XComp.^2;
            Ans=rmfield(Ans,'XComp');
            if isfield(Ans,'YComp');
                Ans.Val=Ans.Val+Ans.YComp.^2;
                Ans=rmfield(Ans,'YComp');
            end
            if isfield(Ans,'ZComp');
                Ans.Val=Ans.Val+Ans.ZComp.^2;
                Ans=rmfield(Ans,'ZComp');
            end
            Ans.Val=sqrt(Ans.Val);
        otherwise
            error('Operator ''%s'' not implemented',Oper)
    end
    %
    % As long as Units treatment is not yet valid, remove Units ...
    %
    if isfield(Ans,'Units')
        Ans=rmfield(Ans,'Units');
    end
    if isfield(Ans,'AbsoluteUnits')
        Ans=rmfield(Ans,'AbsoluteUnits');
    end
else
    for m_=1:5
        t=sel{m_};
        if ~isempty(t)
            selt=Props.Selected{m_};
            if ~isempty(selt) & ~isequal(selt,0)
                if length(selt)==1
                    t=selt;
                elseif isequal(t,0)
                    t=selt;
                else
                    t=selt(t);
                end
            end
            sel{m_}=t;
        end
    end
    S=sel(Props.DimFlag~=0);
    %
    % Backward compatible addition of Domain argument ...
    %
    DomainNr=[];
    if isfield(Props,'Domain')
        DomainNr=Props.Domain;
    end
    [Chk,Ans,FI]=qp_getdata(Props.FileInfo,DomainNr,Props.Props,cmd,Props.SubField{:},S{:});
    Props.FileInfo=FI;
end
NProps=Props;
% -----------------------------------------------------------------------------

function Ans=avgcoord(AnsIn,i)
Ans=AnsIn; AnsIn=[];
if isfield(Ans,'X')
    Ix=~isnan(Ans.X);
    Ans.X(~Ix)=0;
    Ans.X=sum(Ans.X,i)./max(1,sum(Ix,i));
    Ix=any(Ix,i);
    Ans.X(~Ix)=NaN;
end
if isfield(Ans,'Y')
    Ix=~isnan(Ans.Y);
    Ans.Y(~Ix)=0;
    Ans.Y=sum(Ans.Y,i)./max(1,sum(Ix,i));
    Ix=any(Ix,i);
    Ans.Y(~Ix)=NaN;
end
if isfield(Ans,'Z')
    Ix=~isnan(Ans.Z);
    Ans.Z(~Ix)=0;
    Ans.Z=sum(Ans.Z,i)./max(1,sum(Ix,i));
    Ix=any(Ix,i);
    Ans.z(~Ix)=NaN;
end
if isfield(Ans,'XYZ')
    if i==1
        Ans.XYZ=mean(Ans.XYZ,2);
        Ans.TRI=[];
    else
        Ans.XYZ=mean(Ans.XYZ,3);
    end
end


% -----------------------------------------------------------------------------
function sz=getsize(FI,Props)
T_=1; ST_=2; M_=3; N_=4; K_=5;
%======================== SPECIFIC CODE =======================================
if isfield(Props,'Fld')
    i=Props.Fld;
    Props=FI(i);
end
if isequal(Props.FileInfo,'operator')
    switch Props.Props.Oper
        case 'series: A,B'
            P=Props.Props.Data{1};
            sz=getsize([],P);
            P=Props.Props.Data{2};
            szi=getsize([],P);
            sz(T_)=sz(T_)+szi(T_);
        case {'max m','alg.mean m','min m','sum m'}
            P=Props.Props.Data{1};
            sz=getsize([],P);
            sz(M_)=1;
        case {'max n','alg.mean n','min n','sum n'}
            P=Props.Props.Data{1};
            sz=getsize([],P);
            sz(N_)=1;
        case {'max k','alg.mean k','min k','sum k'}
            P=Props.Props.Data{1};
            sz=getsize([],P);
            sz(K_)=1;
        otherwise
            sz=zeros(1,5);
            for i=1:length(Props.Props.Data)
                P=Props.Props.Data{i};
                if isstruct(P) && isfield(P,'FileInfo')
                    sz=max(sz,getsize([],P));
                end
            end
    end
else
    %
    % Backward compatible addition of Domain argument ...
    %
    DomainNr=[];
    if isfield(Props,'Domain')
        DomainNr=Props.Domain;
    end
    [Chk,sz]=qp_getdata(Props.FileInfo,DomainNr,Props.Props,'size');
    for i=1:5
        if iscell(Props.Selected{i})
            if i == 3
                sz(i) = 111;
                sz(i+1) = 1;
            elseif i == 5
                sz(i) = 1;
            end
        elseif ~isempty(Props.Selected{i}) && ~isequal(Props.Selected{i},0)
            sz(i)=length(Props.Selected{i});
        end
    end
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function [T,TZshift]=readtim(FI,Props,t)
T_=1; ST_=2; M_=3; N_=4; K_=5;
%======================== SPECIFIC CODE =======================================
if isfield(Props,'Fld')
    i=Props.Fld;
    Props=FI(i);
end
if isequal(Props.FileInfo,'operator')
    switch Props.Props.Oper
        case 'series: A,B'
            P=Props.Props.Data{1};
            sz=getsize([],P);
            num_times1=sz(T_);
            if t==0
                [T,TZshift] = readtim([],P,0);
            else
                t_index = t;
                t_index(t_index>num_times1) = [];
                [T,TZshift] = readtim([],P,t_index);
            end

            P=Props.Props.Data{2};
            if t==0
                [T2,TZshift2] = readtim([],P,0);
            else
                t_index = t(t>num_times1) - num_times1;
                [T2,TZshift2] = readtim([],P,t_index);
            end
            if isnan(TZshift) || isnan(TZshift2)
                T = [T;T2];
                TZshift = NaN;
            else
                % make time zone consistent to TZshift1
                T = [T;T2-TZshift2+TZshift];
            end
        otherwise
            nt=0;
            j=0;
            for i=1:length(Props.Props.Data)
                P=Props.Props.Data{i};
                if isstruct(P) && isfield(P,'FileInfo')
                    szi=getsize([],P);
                    if szi(T_)>nt
                        nt=szi(T_);
                        j=i;
                    end
                end
            end
            if j==0
                T=[];
                TZshift=NaN;
            else
                P=Props.Props.Data{j};
                [T,TZshift]=readtim([],P,t);
            end
    end
else
    selt=Props.Selected{T_};
    if ~isempty(selt) && ~isequal(selt,0)
        if length(selt)==1
            t=selt;
        elseif isequal(t,0)
            t=selt;
        else
            t=selt(t);
        end
    end
    %
    % Backward compatible addition of Domain argument ...
    %
    DomainNr=[];
    if isfield(Props,'Domain')
        DomainNr=Props.Domain;
    end
    [Chk,TZshift]=qp_getdata(Props.FileInfo,DomainNr,Props.Props,'timezone');
    [Chk,T]=qp_getdata(Props.FileInfo,DomainNr,Props.Props,'times',t);
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function [TZshift,TZstr]=gettimezone(FI,domain,Props)
if isfield(Props,'Fld')
    i=Props.Fld;
    Props=FI(i);
end
[success,TZshift,TZstr]=nested_gettimezone(Props);
if ~success
    error(lasterr)
end
% -----------------------------------------------------------------------------


function [success,TZshift,TZstr]=nested_gettimezone(Props)
if isequal(Props.FileInfo,'operator')
    [success,TZshift,TZstr]=nested_gettimezone(Props.Props.Data{1});
    if strcmp(Props.Props.Oper,'series: A,B')
            % time zone of A and B need to be consistent ...
            if success && ~isnan(TZshift)
                [success2,TZshift2]=nested_gettimezone(Props.Props.Data{2});
                if success2
                    if isnan(TZshift2)
                        % concatenation with unknown time zone ... forget time zone
                        TZshift = NaN;
                        TZstr = '';
                    end
                else
                    success = success2; % failure
                end
            end
    else
        % use time zone of first data set ...
    end
else
    DomainNr=[];
    if isfield(Props,'Domain')
        DomainNr=Props.Domain;
    end
    [success,TZshift,TZstr] = qp_getdata(Props.FileInfo,DomainNr,Props.Props,'timezone');
end


% -----------------------------------------------------------------------------
function S=readsts(FI,Props,s)
T_=1; ST_=2; M_=3; N_=4; K_=5;
%======================== SPECIFIC CODE =======================================
if nargin==2
    s=0;
end
if isfield(Props,'Fld')
    i=Props.Fld;
    Props=FI(i);
end
if isequal(Props.FileInfo,'operator')
    nstat=0;
    j=0;
    for i=1:length(Props.Props.Data)
        P=Props.Props.Data{i};
        if isstruct(P)
            szi=getsize([],P);
            if szi(ST_)>nstat
                nstat=szi(ST_);
                j=i;
            end
        end
    end
    if j==0
        S={};
    else
        P=Props.Props.Data{j};
        S=readsts([],P,s);
    end
else
    sels=Props.Selected{ST_};
    if ~isempty(sels) && ~isequal(sels,0)
        if length(sels)==1
            s=sels;
        elseif isequal(s,0)
            s=sels;
        else
            s=sels(s);
        end
    end
    %
    % Backward compatible addition of Domain argument ...
    %
    DomainNr=[];
    if isfield(Props,'Domain')
        DomainNr=Props.Domain;
    end
    [Chk,S]=qp_getdata(Props.FileInfo,DomainNr,Props.Props,'stations',s);
    if ~isequal(s,0) && length(S)>length(s)
        S=S(s);
    end
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function [NewFI,cmdargs]=options(FI,mfig,cmd,varargin)
T_=1; ST_=2; M_=3; N_=4; K_=5;
%======================== SPECIFIC CODE =======================================
Inactive=get(0,'defaultuicontrolbackground');
Active=[1 1 1];
NewFI=FI;
cmd=lower(cmd);
cmdargs={};

switch cmd
    case 'initialize'
        OK=optfig(mfig);
        Handle_VarList=findobj(mfig,'tag','varlist');
        Str={};
        for i=1:length(FI)
            Str{i}=FI(i).Name;
        end
        if length(Str)>0
            set(Handle_VarList,'userdata',FI,'string',Str,'value',1,'enable','on','backgroundcolor',Active);
            Handle_DelVar=findobj(mfig,'tag','delvar');
            set(Handle_DelVar,'enable','on');
        end
        options(FI,mfig,'selectvar');

    case {'delvar','deletevar','deleteallvars'}
        Handle_VarList = findobj(mfig,'tag','varlist');
        Vars = get(Handle_VarList,'userdata');
        if strcmp(cmd,'deleteallvars')
            NrInList = ':';
            Vars(1:end) = []; % Vars will be empty, but not equal to []
        else
            Str = get(Handle_VarList,'string');
            NrInList = get(Handle_VarList,'value');
            Vars(NrInList) = [];
            Str(NrInList) = [];
            cmd = 'deletevar';
        end
        if isempty(Vars)
            NrInList = 1;
            set(Handle_VarList,'enable','off','backgroundcolor',Inactive);
            Handle_DelVar = findobj(mfig,'tag','delvar');
            set(Handle_DelVar,'enable','off');
            Str = ' ';
        else
            NrInList = min(NrInList,length(Vars));
        end
        set(Handle_VarList,'userdata',Vars,'string',Str,'value',NrInList);
        options(FI,mfig,'selectvar');
        NewFI = Vars;
        cmdargs={cmd};

    case 'selectvar'
        Handle_OpList=findobj(mfig,'tag','operatorlist');
        Handle_VarList=findobj(mfig,'tag','varlist');
        Vars=get(Handle_VarList,'userdata');
        Str=get(Handle_VarList,'string');
        i=get(Handle_VarList,'value');
        if isequal(Str,' ')
            Ops={};
        else
            if nargin>3
                iArg=varargin{1};
                if ischar(iArg),
                    iArg=strmatch(iArg,Str,'exact');
                    if ~isequal(size(iArg),[1 1])
                        return;
                    end
                end
                i=iArg;
                set(Handle_VarList,'value',i)
            end

            NVal=Vars(i).Props.NVal;
            switch NVal
                case -1
                case 0
                    Ops={};
                    if Vars(i).DimFlag(M_)
                        Ops(end+1) = {'m index'};
                    end
                    if Vars(i).DimFlag(N_)
                        Ops(end+1) = {'n index'};
                    end
                    if Vars(i).DimFlag(K_)
                        Ops(end+1) = {'k index'};
                    end
                case {1,1.9,2,3}
                    Ops={'A+B','A-B','A*B','A/B','max(A,B)','min(A,B)', ...
                        '+ constant','* constant','^ constant','max(A,constant)','min(A,constant)', ...
                        '10log','abs','series: A,B','A under condition B', ...
                        'f(A) = user defined','f(A,B) = user defined'};
                    if Vars(i).DimFlag(M_)
                        if NVal==1
                            Ops(end+(1:2))={'min m' 'max m'};
                        end
                        Ops(end+(1:4)) = {'alg.mean m' 'sum m' 'flip m' 'm index'};
                    end
                    if Vars(i).DimFlag(N_)
                        if NVal==1
                            Ops(end+(1:2))={'min n' 'max n'};
                        end
                        Ops(end+(1:4)) = {'alg.mean n' 'sum n' 'flip n' 'n index'};
                    end
                    if Vars(i).DimFlag(K_)
                        if NVal==1
                            Ops(end+(1:2))={'min k' 'max k'};
                        end
                        Ops(end+(1:4)) = {'alg.mean k' 'sum k' 'flip k' 'k index'};
                    end
                    if NVal>1
                        Ops(end+1)={'magnitude'};
                    else
                        Ops(end+1:end+2)={'vector(A,B)','vec_M&D(A,B)'};
                    end
            end
            cmdargs={cmd Str{i}};
        end
        iop=get(Handle_OpList,'Value');
        strop=get(Handle_OpList,'String');
        strop=strop{iop};
        iop=strmatch(strop,Ops,'exact');
        if isempty(iop)
            iop=1;
        end
        if isempty(Ops)
            set(Handle_OpList,'Enable','off','Value',iop,'String',{' '},'backgroundcolor',Inactive);
        else
            set(Handle_OpList,'Enable','on','Value',iop,'String',Ops,'backgroundcolor',Active);
        end
        options(FI,mfig,'selectoperator');

    case 'selectoperator'
        Handle_DefVar=findobj(mfig,'tag','defvariable');
        set(Handle_DefVar,'enable','on')

        Handle_VarList=findobj(mfig,'tag','varlist');
        Vars=get(Handle_VarList,'userdata');
        Str=get(Handle_VarList,'string');
        i=get(Handle_VarList,'value');

        Handle_OpList=findobj(mfig,'tag','operatorlist');
        Ops=get(Handle_OpList,'string');
        k=get(Handle_OpList,'value');
        if nargin>3
            kArg=varargin{1};
            if ischar(kArg)
                kArg=strmatch(kArg,Ops,'exact');
                if ~isequal(size(kArg),[1 1])
                    return;
                end
            end
            k=kArg;
            set(Handle_OpList,'value',k)
        end

        Handle_UserOp=findobj(mfig,'tag','useroperator');
        Handle_VarList2=findobj(mfig,'tag','varlist2');
        Str2=get(Handle_VarList2,'string');
        j=get(Handle_VarList2,'value');

        Handle_ConstTxt=findobj(mfig,'tag','constant');
        set(Handle_ConstTxt,'enable','off')
        Handle_Const=findobj(mfig,'tag','constant=?');
        set(Handle_Const,'enable','off','backgroundcolor',Inactive)

        Handle_CondTxt=findobj(mfig,'tag','condition');
        set(Handle_CondTxt,'enable','off')
        Handle_Cond=findobj(mfig,'tag','condition=?');
        set(Handle_Cond,'enable','off','backgroundcolor',Inactive)

        if isempty(Str) || isequal(Ops{k},' ')
            set(Handle_VarList2,'enable','off','value',1,'string',{' '},'backgroundcolor',Inactive,'userdata',[]);
            set(Handle_DefVar,'enable','off')
        else
            Str2Sel=Str2{j};
            NVali=Vars(i).Props.NVal;
            for ii=1:length(Vars)
                SZ{ii}=getsize(FI,FI(ii));
            end
            SZi=SZ{i};
            jj=logical(ones(length(Vars),1));
            switch Ops{k}
                case {'A+B','A-B','A*B','A/B','max(A,B)','min(A,B)','vector(A,B)','vec_M&D(A,B)','A under condition B','f(A,B) = user defined'}
                    NValMismatchNotAllowed=ismember(Ops{k},{'vector(A,B)','vec_M&D(A,B)'});
                    NValBMustEqual1=ismember(Ops{k},{'A under condition B'});
                    for ii=1:length(Vars)
                        if (NValBMustEqual1 && Vars(ii).Props.NVal~=1)
                            jj(ii)=0;
                        elseif (Vars(ii).Props.NVal~=NVali) && (NValMismatchNotAllowed || ...
                                ~((Vars(ii).Props.NVal==1) || (NVali==1 && Vars(ii).Props.NVal>0)))
                            jj(ii)=0;
                        elseif ~isequal(SZ{ii}(T_),SZi(T_)) && ~isequal(SZ{ii}(T_),1) && FI(ii).DimFlag(T_) && ~isequal(1,SZi(T_)) && FI(i).DimFlag(T_)
                            jj(ii)=0;
                        elseif any(SZ{ii}([M_ N_ K_])~=SZi([M_ N_ K_]) & SZ{ii}([M_ N_ K_])~=0 & SZ{ii}([M_ N_ K_])~=1 & SZi([M_ N_ K_])~=0 & SZi([M_ N_ K_])~=1)
                            jj(ii)=0;
                        elseif any(SZ{ii}([M_ N_ K_])~=SZi([M_ N_ K_]) & SZ{ii}([M_ N_ K_])~=1 & SZi([M_ N_ K_])==1) && ...
                                any(SZ{ii}([M_ N_ K_])~=SZi([M_ N_ K_]) & SZ{ii}([M_ N_ K_])==1 & SZi([M_ N_ K_])~=1)
                            jj(ii)=0;
                        end
                    end
                    Str2=Str(jj);
                    if isempty(Str2)
                        set(Handle_VarList2,'enable','off','value',1,'string',{' '},'backgroundcolor',Inactive,'userdata',[]);
                        set(Handle_DefVar,'enable','off')
                        if strcmp(Ops{k},'A under condition B')
                            set(Handle_CondTxt,'enable','off');
                            set(Handle_Cond,'enable','off','backgroundcolor',Inactive);
                        end
                    else
                        j=strmatch(Str2Sel,Str2,'exact');
                        if isempty(j)
                            j=1;
                        elseif ~isequal(size(j),[1 1])
                            j=j(1);
                        end
                        jj=find(jj);
                        set(Handle_VarList2,'enable','on','value',j,'string',Str2,'backgroundcolor',Active,'userdata',jj);
                        if strcmp(Ops{k},'A under condition B')
                            set(Handle_CondTxt,'enable','on');
                            set(Handle_Cond,'enable','on','backgroundcolor',Active);
                            if isempty(get(Handle_Cond,'string'))
                                set(Handle_DefVar,'enable','off')
                            end
                        end
                    end
                    if strcmp(Ops{k},'f(A,B) = user defined')
                        set(Handle_UserOp,'enable','on','backgroundcolor',Active);
                        if ~isempty(analyze_operator(get(Handle_UserOp,'string'),{'A','B'}))
                            set(Handle_DefVar,'enable','off')
                        else
                            set(Handle_DefVar,'enable','on')
                        end
                    else
                        set(Handle_UserOp,'enable','off','backgroundcolor',Inactive);
                    end
                case {'series: A,B'}
                    for ii=1:length(Vars)
                        if Vars(ii).Props.NVal~=NVali
                            jj(ii)=0;
                        elseif ~isequal(SZ{ii}([M_ N_ K_]),SZi([M_ N_ K_]))
                            jj(ii)=0;
                        end
                    end
                    Str2=Str(jj);
                    if isempty(Str2)
                        set(Handle_VarList2,'enable','off','value',1,'string',{' '},'backgroundcolor',Inactive,'userdata',[]);
                        set(Handle_DefVar,'enable','off')
                    else
                        j=strmatch(Str2Sel,Str2,'exact');
                        if isempty(j)
                            j=1;
                        elseif ~isequal(size(j),[1 1])
                            j=j(1);
                        end
                        jj=find(jj);
                        set(Handle_VarList2,'enable','on','value',j,'string',Str2,'backgroundcolor',Active,'userdata',jj);
                    end
                    set(Handle_UserOp,'enable','off','backgroundcolor',Inactive);
                case {'+ constant','* constant','^ constant','max(A,constant)','min(A,constant)'}
                    set(Handle_VarList2,'enable','off','value',1,'string',{' '},'backgroundcolor',Inactive,'userdata',[]);
                    set(Handle_ConstTxt,'enable','on');
                    set(Handle_Const,'enable','on','backgroundcolor',Active);
                    set(Handle_DefVar,'enable','on')
                    set(Handle_UserOp,'enable','off','backgroundcolor',Inactive);
                case {'magnitude','abs','10log','max m','alg.mean m','min m','sum m','max n','alg.mean n','min n','sum n','max k','alg.mean k','min k','sum k','flip m','flip n','flip k','m index','n index','k index'}
                    set(Handle_VarList2,'enable','off','value',1,'string',{' '},'backgroundcolor',Inactive,'userdata',[]);
                    set(Handle_UserOp,'enable','off','backgroundcolor',Inactive);
                case {'f(A) = user defined'}
                    set(Handle_VarList2,'enable','off','value',1,'string',{' '},'backgroundcolor',Inactive,'userdata',[]);
                    set(Handle_UserOp,'enable','on','backgroundcolor',Active);
                    if ~isempty(analyze_operator(get(Handle_UserOp,'string'),{'A'}))
                        set(Handle_DefVar,'enable','off')
                    else
                        set(Handle_DefVar,'enable','on')
                    end
                otherwise
                    set(Handle_VarList2,'enable','off','value',1,'string',{' '},'backgroundcolor',Inactive,'userdata',[]);
                    set(Handle_DefVar,'enable','off')
                    set(Handle_UserOp,'enable','off','backgroundcolor',Inactive);
            end
            cmdargs={cmd Ops{k}};
        end

    case 'useroperator'
        Handle_OpList=findobj(mfig,'tag','operatorlist');
        Ops=get(Handle_OpList,'string');
        k=get(Handle_OpList,'value');

        Handle_DefVar=findobj(mfig,'tag','defvariable');
        Handle_UserOp=findobj(mfig,'tag','useroperator');
        oper = get(Handle_UserOp,'string');
        switch Ops{k}
            case 'f(A) = user defined'
                vars = {'A'};
            case 'f(A,B) = user defined'
                vars = {'A','B'};
        end
        err = analyze_operator(oper,vars);
        if ~isempty(err)
            ui_message('error',err)
            set(Handle_DefVar,'enable','off')
        else
            set(Handle_DefVar,'enable','on')
        end
        
    case 'selectvar2'
        Handle_VarList2=findobj(mfig,'tag','varlist2');
        Str2=get(Handle_VarList2,'string');
        j=get(Handle_VarList2,'value');
        if nargin>3
            jStr=varargin{1};
            if ischar(jStr)
                jStr=strmatch(jStr,Str2,'exact');
                if ~isequal(size(jStr),[1 1])
                    return
                end
            end
            j=jStr;
            set(Handle_VarList2,'value',j)
        end
        cmdargs={cmd Str2{j}};

    case 'const'
        Handle_ConstTxt=findobj(mfig,'tag','constant');
        Handle_Const=findobj(mfig,'tag','constant=?');
        c=get(Handle_Const,'userdata');
        cstr=get(Handle_Const,'string');
        cnew=str2num(cstr);
        if nargin>3
            cnewArg=varargin{1};
            if ischar(cnewArg)
                cnewArg=str2num(cnewArg);
            end
            if ~isequal(size(cnewArg),[1 1])
                return
            end
            cnew=cnewArg;
        end
        if isempty(cnew)
            set(Handle_Const,'string',num2str(cnew));
        else
            if ~isequal(size(cnew),[1 1])
                cnew=cnew(1);
            end
            set(Handle_Const,'string',num2str(cnew),'userdata',cnew);
        end
        cmdargs={cmd cnew};

    case 'condition'
        Handle_CondTxt=findobj(mfig,'tag','condition');
        Handle_Cond=findobj(mfig,'tag','condition=?');
        c=get(Handle_Cond,'userdata');
        Str=get(Handle_Cond,'string');
        Str0=Str;
        if nargin>3
            Str=varargin{1};
        end
        %
        lasterr('');
        try
            if ischar(Str)
                [c,Str]=realset(Str);
            else
                c=Str;
                Str=realset(c);
            end
        catch
            ui_message('error',{'Catch in d3d_qp\clippingvals',lasterr})
            c=get(Handle_Cond,'userdata');
            if isstruct(c)
                Str=realset(c);
            else
                Str=vec2str(c,'noones','nobrackets');
            end
        end
        %
        set(Handle_Cond,'string',Str,'userdata',c)
        Handle_DefVar=findobj(mfig,'tag','defvariable');
        if isempty(Str)
            set(Handle_DefVar,'enable','off')
        else
            set(Handle_DefVar,'enable','on')
        end
        cmdargs={cmd Str};

    case 'defvariable'
        Handle_VarList=findobj(mfig,'tag','varlist');
        Vars=get(Handle_VarList,'userdata');
        Strs=get(Handle_VarList,'string');
        i=get(Handle_VarList,'value');

        Handle_UserOp=findobj(mfig,'tag','useroperator');
        Handle_OpList=findobj(mfig,'tag','operatorlist');
        Ops=get(Handle_OpList,'string');
        k=get(Handle_OpList,'value');

        Handle_VarList2=findobj(mfig,'tag','varlist2');
        jj=get(Handle_VarList2,'userdata');
        j=get(Handle_VarList2,'value');

        Handle_ConstTxt=findobj(mfig,'tag','constant');
        Handle_Const=findobj(mfig,'tag','constant=?');
        c=get(Handle_Const,'userdata');

        Handle_CondTxt=findobj(mfig,'tag','condition');
        Handle_Cond=findobj(mfig,'tag','condition=?');
        cond=get(Handle_Cond,'userdata');

        Props.Units      = '';
        Props.Geom       = Vars(i).Geom;
        Props.Coords     = Vars(i).Coords;
        Props.Tri        = Vars(i).Tri;
        Props.ClosedPoly = Vars(i).ClosedPoly;
        switch Ops{k}
            case {'series: A,B'}
                ii=jj(j);
                VarName=sprintf('%s,%s',Vars(i).Name,Vars(ii).Name);
                if isequal(Vars(i).Units,Vars(ii).Units)
                    Props.Units = Vars(i).Units;
                end
                Props.NVal=Vars(i).Props.NVal;
                Props.Oper=Ops{k};
                Props.Data={Vars(i) Vars(ii)};
                Props.DataInCell = Vars(i).DataInCell & Vars(ii).DataInCell;
            case {'vector(A,B)'}
                ii=jj(j);
                VarName=sprintf('vector(%s,%s)',Vars(i).Name,Vars(ii).Name);
                if isequal(Vars(i).Units,Vars(ii).Units)
                    Props.Units = Vars(i).Units;
                end
                Props.NVal=2;
                Props.Oper=Ops{k};
                Props.Data={Vars(i) Vars(ii)};
                Props.DataInCell = Vars(i).DataInCell & Vars(ii).DataInCell;
            case {'vec_M&D(A,B)'}
                ii=jj(j);
                VarName=sprintf('vector(mag=%s,dir=%s)',Vars(i).Name,Vars(ii).Name);
                Props.NVal=2;
                Props.Oper=Ops{k};
                Props.Data={Vars(i) Vars(ii)};
                Props.DataInCell = Vars(i).DataInCell & Vars(ii).DataInCell;
            case {'A+B','A-B','A*B','A/B'}
                ii=jj(j);
                switch Ops{k}
                    case {'A+B','A-B'}
                        if isequal(Vars(i).Units,Vars(ii).Units)
                            Props.Units = Vars(i).Units;
                        end
                end
                VarName=sprintf('(%s) %s (%s)',Vars(i).Name,Ops{k}(2),Vars(ii).Name);
                Props.NVal=max(Vars(i).Props.NVal,Vars(ii).Props.NVal);
                Props.Oper=Ops{k};
                Props.Data={Vars(i) Vars(ii)};
                Props.DataInCell = Vars(i).DataInCell & Vars(ii).DataInCell;
            case {'max(A,B)','min(A,B)'}
                ii=jj(j);
                if isequal(Vars(i).Units,Vars(ii).Units)
                    Props.Units = Vars(i).Units;
                end
                VarName=sprintf('%s(%s,%s)',Ops{k}(1:3),Vars(i).Name,Vars(ii).Name);
                Props.NVal=max(Vars(i).Props.NVal,Vars(ii).Props.NVal);
                Props.Oper=Ops{k};
                Props.Data={Vars(i) Vars(ii)};
                Props.DataInCell = Vars(i).DataInCell & Vars(ii).DataInCell;
            case {'f(A,B) = user defined'}
                ii=jj(j);
                fcn = get(Handle_UserOp,'string');
                VarName=sprintf('f(A,B) = %s where A = %s and B = %s',fcn,Vars(i).Name,Vars(ii).Name);
                Props.NVal=max(Vars(i).Props.NVal,Vars(ii).Props.NVal);
                Props.Oper=Ops{k};
                Props.Data={Vars(i) Vars(ii) fcn};
                Props.DataInCell = Vars(i).DataInCell & Vars(ii).DataInCell;
            case {'+ constant','* constant','^ constant'}
                VarName=sprintf('(%s) %s %g',Vars(i).Name,Ops{k}(1),c);
                switch Ops{k}
                    case '+ constant'
                        Props.Units = Vars(i).Units;
                end
                Props.NVal=Vars(i).Props.NVal;
                Props.Oper=Ops{k};
                Props.Data={Vars(i) c};
                Props.DataInCell = Vars(i).DataInCell;
            case {'max(A,constant)','min(A,constant)'}
                VarName=sprintf('%s(%s,%g)',Ops{k}(1:3),Vars(i).Name,c);
                Props.Units = Vars(i).Units;
                Props.NVal=Vars(i).Props.NVal;
                Props.Oper=Ops{k};
                Props.Data={Vars(i) c};
                Props.DataInCell = Vars(i).DataInCell;
            case {'10log','abs','flip m','flip n','flip k'}
                VarName=sprintf('%s(%s)',Ops{k},Vars(i).Name);
                switch Ops{k}
                    case '10log'
                    otherwise
                        Props.Units = Vars(i).Units;
                end
                Props.NVal=Vars(i).Props.NVal;
                Props.Oper=Ops{k};
                Props.Data={Vars(i)};
                Props.DataInCell = Vars(i).DataInCell;
            case {'max m','alg.mean m','min m','max n','alg.mean n','min n','max k','alg.mean k','min k','sum k','sum m','sum n'}
                VarName=sprintf('%s(%s)',Ops{k},Vars(i).Name);
                Props.Units = Vars(i).Units;
                Props.NVal=Vars(i).Props.NVal;
                Props.Oper=Ops{k};
                Props.Data={Vars(i)};
                switch Ops{k}
                    case {'max m','alg.mean m','min m','sum m'}
                        switch Props.Geom
                            case {'UGRID2D-FACE', 'UGRID2D-EDGE', 'UGRID2D-NODE', 'UGRID1D-EDGE', 'UGRID1D-NODE'}
                                Props.Geom = 'PNT';
                            otherwise
                                % should maybe also change
                        end
                end
                Props.DataInCell = Vars(i).DataInCell;
            case {'m index','n index','k index'}
                VarName=sprintf('%s(%s)',Ops{k},Vars(i).Name);
                Props.NVal=1;
                Props.Oper=Ops{k};
                Props.Data={Vars(i)};
                Props.DataInCell = Vars(i).DataInCell;
            case {'f(A) = user defined'}
                fcn = get(Handle_UserOp,'string');
                VarName=sprintf('f(A) = %s where A = %s',fcn,Vars(i).Name);
                Props.NVal=Vars(i).Props.NVal;
                Props.Oper=Ops{k};
                Props.Data={Vars(i) fcn};
                Props.DataInCell = Vars(i).DataInCell;
            case {'magnitude'}
                VarName=sprintf('%s(%s)',Ops{k},Vars(i).Name);
                Props.Units = Vars(i).Units;
                Props.NVal=1;
                Props.Oper=Ops{k};
                Props.Data={Vars(i)};
                Props.DataInCell = Vars(i).DataInCell;
            case {'A under condition B'}
                ii=jj(j);
                VarName=sprintf('%s if %s in {%s}',Vars(i).Name,Vars(ii).Name,realset(cond));
                Props.Units = Vars(i).Units;
                Props.NVal=Vars(i).Props.NVal;
                Props.Oper=Ops{k};
                Props.Data={Vars(i) Vars(ii) cond};
                Props.DataInCell = Vars(i).DataInCell & Vars(ii).DataInCell;
            otherwise
                return
        end
        if nargin>3
            VarName=varargin{1};
            accept=isempty(strmatch(VarName,Strs,'exact'));
        else % interactive
            accept=0;
        end
        prompt={'Name of variable:'};
        def={VarName};
        dlgTitle='Specify unique name of variable';
        lineNo=1;
        if ~isempty(strmatch(VarName,Strs,'exact'))
            prompt={'Name of variable (current name not unique):'};
        end
        while ~accept
            answer=stdinputdlg(prompt,dlgTitle,lineNo,def);
            if isempty(answer)
                break
            end
            VarName=answer{1};
            accept=isempty(strmatch(VarName,Strs,'exact'));
        end
        if accept
            ii=length(Vars)+1;
            Vars(ii).Name       = VarName;
            Vars(ii).Units      = Props.Units;
            Vars(ii).Geom       = Props.Geom;
            Vars(ii).Coords     = Props.Coords;
            Vars(ii).FileInfo   = 'operator';
            Props.Name = VarName;
            Vars(ii).Props      = Props;
            Vars(ii).Selected   = [];
            Vars(ii).DataInCell = Props.DataInCell;
            Vars(ii).DimFlag    = [0 0 0 0 0];
            for d = 1:5
                dflag = 0;
                for k = 1:length(Props.Data)
                    if dflag == 0 && ~isnumeric(Props.Data{k}) && isfield(Props.Data{k},'DimFlag')
                        dflag = Props.Data{k}.DimFlag(d);
                    end
                end
                Vars(ii).DimFlag(d) = dflag;
            end
            Vars(ii).ClosedPoly = Props.ClosedPoly;
            Vars(ii).Tri        = Props.Tri;
            set(Handle_VarList,'userdata',Vars);
            Str={};
            for i=1:length(Vars)
                Str{i}=Vars(i).Name;
            end
            set(Handle_VarList,'userdata',Vars,'string',Str,'value',length(Vars));
            options(Vars,mfig,'selectvar');
            NewFI=Vars;
            cmdargs={cmd VarName};
        end

    otherwise
        fprintf('Unknown option command: %s\n',cmd)
end
% -----------------------------------------------------------------------------

% -----------------------------------------------------------------------------
function OK=optfig(h0)
Inactive=get(0,'defaultuicontrolbackground');
FigPos=get(h0,'position');
FigPos(3:4) = getappdata(h0,'DefaultFileOptionsSize');
set(h0,'position',FigPos)

voffset=FigPos(4)-30;
%
h1 = uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Callback','d3d_qp fileoptions delvar', ...
    'Enable','off', ...
    'Position',[181 voffset 150 20], ...
    'String','Delete Variable', ...
    'Tag','delvar');
%
voffset=voffset-25;
h1 = uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Callback','d3d_qp fileoptions selectvar', ...
    'Enable','off', ...
    'Position',[11 voffset 320 20], ...
    'String',' ', ...
    'Style','popupmenu', ...
    'Tag','varlist', ...
    'Value',1);
%
voffset=voffset-25;
h1 = uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Callback','d3d_qp fileoptions selectoperator', ...
    'Enable','off', ...
    'Position',[11 voffset 320 20], ...
    'String',{' '}, ...
    'Style','popupmenu', ...
    'Tag','operatorlist', ...
    'Value',1);
%
voffset=voffset-25;
h1 = uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Callback','d3d_qp fileoptions useroperator', ...
    'Enable','off', ...
    'Position',[11 voffset 320 20], ...
    'String',' ', ...
    'Horizontalalignment','left', ...
    'Style','edit', ...
    'Tag','useroperator');
%
voffset=voffset-25;
h1 = uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Callback','d3d_qp fileoptions selectvar2', ...
    'Enable','off', ...
    'Position',[11 voffset 320 20], ...
    'String',{' '}, ...
    'Style','popupmenu', ...
    'Tag','varlist2', ...
    'Value',1);
%
voffset=voffset-25;
h1 = uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Enable','off', ...
    'Position',[11 voffset 150 20], ...
    'HorizontalAlignment','left', ...
    'String','Constant', ...
    'Style','text', ...
    'Tag','constant');
h1 = uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Callback','d3d_qp fileoptions const', ...
    'Enable','off', ...
    'Position',[171 voffset 160 20], ...
    'HorizontalAlignment','right', ...
    'String','1', ...
    'Style','edit', ...
    'Tag','constant=?', ...
    'UserData',1);
%
voffset=voffset-25;
h1 = uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Enable','off', ...
    'Position',[11 voffset 150 20], ...
    'HorizontalAlignment','left', ...
    'String','Condition', ...
    'Style','text', ...
    'Tag','condition');
h1 = uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Callback','d3d_qp fileoptions condition', ...
    'Enable','off', ...
    'Position',[171 voffset 160 20], ...
    'HorizontalAlignment','left', ...
    'String',' ', ...
    'Style','edit', ...
    'Tag','condition=?', ...
    'UserData',[]);
%
voffset=voffset-25;
h1 = uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Callback','d3d_qp fileoptions defvariable', ...
    'Enable','off', ...
    'Position',[171 voffset 160 20], ...
    'String','Define Variable', ...
    'Tag','defvariable');
OK=1;
% -----------------------------------------------------------------------------

function [err,Op] = analyze_operator(oper,var)
err = [];
ikeystart = -1;
i = 0;
j = 0;
Op = cell(1,length(oper));
while i<length(oper)
    i = i+1;
    if any('()?:+-*/^,<>~='==oper(i)) || ...
            (oper(i)=='.' && i<length(oper) && any('+-*/^'==oper(i+1)))
        if ikeystart>0
            j = j+1;
            Op{j} = oper(ikeystart:i-1);
        end
        j = j+1;
        if (oper(i)=='.') || ...
            (oper(i)=='<' && i<length(oper) && oper(i+1)=='=') || ...
            (oper(i)=='>' && i<length(oper) && oper(i+1)=='=') || ...
            (oper(i)=='=' && i<length(oper) && oper(i+1)=='=') || ...
            (oper(i)=='~' && i<length(oper) && oper(i+1)=='=')
            Op{j} = oper(i:i+1);
            i = i+1;
        elseif oper(i)=='='
            Op{j} = '==';
            i = i+1;
        else
            Op{j} = oper(i);
        end
        ikeystart = -1;
    elseif oper(i)==' '
        if ikeystart>0
            j = j+1;
            Op{j} = oper(ikeystart:i-1);
        end
        ikeystart = -1;
    elseif ikeystart<0
        ikeystart = i;
    end
end
if ikeystart>0
    j = j+1;
    Op{j} = oper(ikeystart:end);
end
Op(j+1:end)=[];
%
fcn = zeros(4,length(Op));
i = 0;
ifcn = 0;
numbers = {'pi','NaN'};
unaryop = {'+','-','~'};
powerop = {'^','.^'};
cpowerop = [{'('} powerop];
operators = [unaryop {'*','/','.+','.-','.*','./','<','>','<=','>=','==','~='}];
alloperators = [powerop {':','?','('} operators];
[f0,f1,f2,f3] = supportedfunctions;
while i<length(Op)
    i = i+1;
    if ~isnan(str2double(Op{i}))
        if i>1
            if ischar(Op{i-1}) && ismember(Op{i-1},alloperators)
                % OK
            elseif ifcn==0 || i>fcn(1,ifcn)+1
                err = sprintf('Missing operator before: %s',Op{i});
                return
            end
        end
        Op{i} = str2double(Op{i});
    else
        switch Op{i}
            case operators
                if ~ismember(Op{i},unaryop)
                    if i==1 || (ifcn>0 && i==max(fcn([1 3 4],ifcn))+1)
                        err = sprintf('Missing expression before binary operator: %s',Op{i});
                        return
                    elseif i>1 && ischar(Op{i-1}) && ismember(Op{i-1},operators)
                        err = sprintf('Invalid succession of binary operators: %s%s',Op{i-1},Op{i});
                        return
                    end
                end
            case powerop
                i0 = i;
                while i0<length(Op) && ismember(Op{i0+1},unaryop)
                    i0 = i0+1;
                end
                if i0==length(Op)
                    err = sprintf('Unexpected end of formula while processing: %s',Op{i});
                    return
                elseif strcmp(Op{i0+1},')')
                    err = sprintf('Unexpected closing bracket found following: %s',Op{i});
                    return
                elseif strcmp(Op{i0+1},'(')
                    ifcn = ifcn+1;
                    fcn(1,ifcn) = i;
                    fcn(2,ifcn) = 1;
                    ifcn = ifcn+1;
                    fcn(1,ifcn) = i0+1;
                    fcn(2,ifcn) = 1;
                    i = i0+1;
                elseif ismember(Op{i0+1},[var numbers])
                    Op{i-1}    = {Op{i} Op{i-1} Op(i+1:i0+1)};
                    Op(i:i0+1) = [];
                    i          = i-1;
                elseif ~isnan(str2double(Op{i0+1}))
                    Op{i0+1}   = str2double(Op{i0+1});
                    Op{i-1}    = {Op{i} Op{i-1} Op(i+1:i0+1)};
                    Op(i:i0+1) = [];
                    i = i-1;
                else
                    % should be a function
                    ifcn = ifcn+1;
                    fcn(1,ifcn) = i;
                    fcn(2,ifcn) = 1;
                    i = i0;
                end
            case [var numbers]
                % skip variables
                if i>1
                    if ischar(Op{i-1}) && ismember(Op{i-1},alloperators)
                        % OK
                    elseif ifcn==0 || i>fcn(1,ifcn)+1
                        err = sprintf('Missing operator before: %s',Op{i});
                        return
                    end
                end
            case '?'
                if i==1 || (ifcn>0 && i==max(fcn([1 3 4],ifcn))+1)
                    err = 'Missing conditional expression for ternary operator: ?';
                    return
                elseif i<length(Op) && strcmp(Op{i+1},'(')
                    ifcn = ifcn+1;
                    fcn(1,ifcn) = i;
                    fcn(2,ifcn) = 2;
                    Op(i+1)=[];
                else
                    err = 'Missing opening bracket after ternary operator: ?';
                    return
                end
            case '('
                if i>1 && (~ischar(Op{i-1}) || ~ismember(Op{i-1},alloperators))
                    err = 'Unexpected opening bracket found';
                    return
                end
                ifcn = ifcn+1;
                fcn(1,ifcn) = i;
                fcn(2,ifcn) = 1;
            case ')'
                if ifcn==0
                    err = 'Unexpected closing bracket found';
                    return
                else
                    ifn = fcn(1,ifcn);
                    iarg2 = fcn(3,ifcn);
                    iarg3 = fcn(4,ifcn);
                    switch fcn(2,ifcn)
                        case 1
                            if ifn+1>i-1
                                if ismember(Op{ifn},cpowerop)
                                    err = 'Empty expression between brackets';
                                else
                                    err = sprintf('Too few arguments specified for function: %s',Op{fcn(1,ifcn)});
                                end
                                return
                            elseif ismember(Op{ifn},powerop)
                                Op{ifn-1} = {Op{ifn} Op{ifn-1} Op(ifn+1:i-1)};
                                Op(ifn:i) = [];
                            else
                                Op{ifn} = {Op{ifn} Op(ifn+1:i-1)};
                                Op(ifn+1:i) = [];
                            end
                        case 2
                            if strcmp(Op{ifn},'?')
                                if iarg2==0
                                    err = 'Missing colon associated to ternary operator: ?';
                                    return
                                elseif i==iarg2+1
                                    err = 'Empty expression for false case of ternary operator: ?';
                                    return
                                end
                                Op{ifn} = {Op{ifn} {} Op(ifn+1:iarg2-1) Op(iarg2+1:i-1)};
                                Op(ifn+1:i) = [];
                                if ifcn==1
                                    Op{ifn}{2} = Op(1:ifn-1);
                                    Op(1:ifn-1)= [];
                                    ifn = 1;
                                else
                                    jj = max(fcn([1 3 4],ifcn-1))+1;
                                    Op{ifn}{2} = Op(jj:ifn-1);
                                    Op(jj:ifn-1)= [];
                                    ifn = jj;
                                end
                            else
                                if iarg2==0
                                    err = sprintf('Too few arguments specified for function: %s',Op{fcn(1,ifcn)});
                                    return
                                elseif i==iarg2+1
                                    err = sprintf('Empty second argument specified for function: %s',Op{fcn(1,ifcn)});
                                    return
                                end
                                Op{ifn} = {Op{ifn} Op(ifn+1:iarg2-1) Op(iarg2+1:i-1)};
                                Op(ifn+1:i) = [];
                            end
                        case 3
                            if iarg3==0
                                err = sprintf('Too few arguments specified for function: %s',Op{fcn(1,ifcn)});
                                return
                            elseif i==iarg3+1
                                err = sprintf('Empty third argument specified for function: %s',Op{fcn(1,ifcn)});
                                return
                            end
                            Op{ifn} = {Op{ifn} Op(ifn+1:iarg2-1) Op(iarg2+1:iarg3-1) Op(iarg3+1:i-1)};
                            Op(ifn+1:i) = [];
                    end
                    i = ifn;
                    fcn(:,ifcn) = 0;
                    ifcn = ifcn - 1;
                end
                if ifcn>0 && ismember(Op{fcn(1,ifcn)},powerop)
                    ifn = fcn(1,ifcn);
                    Op{ifn-1} = {Op{ifn} Op{ifn-1} Op(ifn+1:i)};
                    Op(ifn:i) = [];
                    i = ifn;
                    ifcn = ifcn - 1;
                end
            case ':'
                if ifcn==0 || ~isequal(Op{fcn(1,ifcn)},'?')
                    err = 'Colon only allowed in ternary operator: ?';
                    return
                elseif i==fcn(1,ifcn)+1
                    err = 'Empty expression for true case of ternary operator: ?';
                    return
                elseif fcn(3,ifcn)~=0
                    err = 'Only one colon allowed per ternary operator: ?';
                    return
                end
                fcn(3,ifcn) = i;
            case ','
                if ifcn==0 || ismember(Op{fcn(1,ifcn)},cpowerop)
                    err = 'Unexpected comma found';
                    return
                elseif strcmp(Op{fcn(1,ifcn)},'?')
                    err = 'Use colon instead of comma to separate true and false cases of ternary operator: ?';
                    return
                elseif fcn(2,ifcn)>1 && fcn(3,ifcn)==0
                    if i==ifcn+1
                        err = sprintf('Empty first argument for function: %s',Op{fcn(1,ifcn)});
                        return
                    end
                    fcn(3,ifcn) = i;
                elseif fcn(2,ifcn)>2 && fcn(4,ifcn)==0
                    if i==fcn(3,ifcn)+1
                        err = sprintf('Empty second argument for function: %s',Op{fcn(1,ifcn)});
                        return
                    end
                    fcn(4,ifcn) = i;
                else
                    err = sprintf('Too many arguments specified for function: %s',Op{fcn(1,ifcn)});
                    return
                end
            otherwise
                if i>1
                    if ischar(Op{i-1}) && ismember(Op{i-1},alloperators)
                        % OK
                    elseif ifcn==0 || i>fcn(1,ifcn)+1
                        err = sprintf('Missing operator before: %s',Op{i});
                        return
                    end
                end
                if i<length(Op) && strcmp(Op{i+1},'(')
                    ifcn = ifcn+1;
                    fcn(1,ifcn) = i;
                    switch Op{i}
                        case f0
                            % no arguments
                            % rand() = rand(max(size(A),size(B)))
                            fcn(2,ifcn) = 0;
                            if i+1<length(Op)
                                if strcmp(Op{i+2},')')
                                    Op(i+1:i+2)=[];
                                    fcn(:,ifcn) = 0;
                                    ifcn = ifcn-1;
                                else
                                    err = ['Too many arguments specified for function: ' Op{i}];
                                    return
                                end
                            else
                                err = ['Unexpected end of formula while processing function: ' Op{i}];
                                return
                            end      
                        case f1
                            % single argument
                            fcn(2,ifcn) = 1;
                            Op(i+1)=[];
                        case f2
                            % two arguments
                            % pow(X,Y) = power(X,Y)
                            % fmod(X,Y) = X – fix(X/Y) * Y
                            % hypot(X,Y) = sqrt(X.^2 + Y.^2)
                            fcn(2,ifcn) = 2;
                            Op(i+1)=[];
                        case f3
                            % conditional(C,X,Y) = C?(X:Y)
                            %                    = if C, X, else, Y, end
                            fcn(2,ifcn) = 3;
                            Op(i+1)=[];
                        otherwise
                            err = sprintf('Unknown function: %s\n',Op{i});
                            return
                    end
                elseif any('A':'Z'==Op{i}(1)) || any('a':'z'==Op{i}(1))
                    err = sprintf('Missing function arguments or unknown variable: %s\n',Op{i});
                    return
                else
                    err = sprintf('Unknown symbol or expression found: %s\n',Op{i});
                    return
                end
        end
    end
end
if isempty(Op)
    err = 'No formula specified';
    return
elseif ischar(Op{end}) && ismember(Op{end},operators)
    err = sprintf('Unexpected end of formula after operator: %s',Op{end});
    return
elseif ifcn>0
    if ismember(Op{fcn(1,ifcn)},cpowerop)
        err = 'Missing closing bracket';
    elseif strcmp(Op{fcn(1,ifcn)},'?')
        err = 'Missing closing bracket of ternary operator: ?';
    else
        err = sprintf('Unexpected end of formula while processing function: %s',Op{fcn(1,ifcn)});
    end
    return
end
%
Op = parse(Op,[f0 f1 f2 f3],alloperators);

function Op = parse(Op,ff,allops)
if ischar(Op) || isnumeric(Op)
    % Op = 'A','B'
elseif length(Op)==1 && iscell(Op{1})
    Op = parse(Op{1},ff,allops);
elseif ischar(Op{1}) && (ismember(Op{1},ff) || ismember(Op{1},allops))
    for i = 2:length(Op)
        Op{i} = parse(Op{i},ff,allops);
    end
elseif length(Op)==1 && ischar(Op{1}) && (strcmp(Op{1},'A') || strcmp(Op{1},'B'))
    Op = Op{1};
else
    i = 1;
    while i < length(Op)-1
        i = i+1;
        if ischar(Op{i}) && ismember(Op{i},{'*','/','.*','./'})
            Op{i-1}   = Op([i i-1 i+1]);
            Op(i:i+1) = [];
            i = i-1;
        end
    end
    %
    i = 1;
    while i < length(Op)-1
        i = i+1;
        if ischar(Op{i}) && ismember(Op{i},{'+','-','.+','.-'})
            Op{i-1}   = Op([i i-1 i+1]);
            Op(i:i+1) = [];
            i = i-1;
        end
    end
    %
    i = 1;
    while i < length(Op)-1
        i = i+1;
        if ischar(Op{i}) && ismember(Op{i},{'>','<','>=','<=','==','=','~='})
            Op{i-1}   = Op([i i-1 i+1]);
            Op(i:i+1) = [];
            i = i-1;
        end
    end
    %
    Op = parse(Op{1},ff,allops);
end

function [f0,f1,f2,f3] = supportedfunctions
f0 = {'rand','index'};
f1 = {'abs','acos','asin','atan','ceil','cos', ...
    'cosh','exp','floor','log','log10', ...
    'round','sin','sinh','sqrt','tan','tanh'};
f2 = {'atan2','fmod','hypot','max','min','power'};
f3 = {'conditional'};


function val = usereval(fun,A,B)
if nargin==2
    args = {A};
elseif nargin==3
    args = {A,B};
end
if ischar(fun)
    if isequal(fun,'A')
        val = A;
        return
    elseif isequal(fun,'B')
        val = B;
        return
    end
    if nargin==2
        [err,fun] = analyze_operator(fun,{'A'});
    elseif nargin==3
        [err,fun] = analyze_operator(fun,{'A','B'});
    end
    if ~isempty(err)
        error(err)
    end
    if ischar(fun)
        val = eval(fun);
        return
    end
elseif isnumeric(fun)
    val = fun;
    return
end
if iscell(fun{1})
    val = usereval(fun{1},args{:});
else
    argin = {};
    for i = 2:length(fun)
        argin{i-1} = usereval(fun{i},args{:});
    end
    switch fun{1}
        case {'rand','index'}
            sz = size(args{1});
            if length(args)>1
                sz = max(sz,size(args{2}));
            end
            argin = {sz};
        case {'^','.^','pow'}
            fun{1} = 'power';
        case {'+','.+'}
            fun{1} = 'plus';
        case {'-','.-'}
            fun{1} = 'minus';
        case {'*','.*'}
            fun{1} = 'times';
        case {'/','./'}
            fun{1} = 'rdivide';
        case '?'
            fun{1} = 'conditional';
        case '('
            val = argin{1};
            return
    end
    val = feval(fun{1},argin{:});
end

function val = index(szA)
val = reshape(1:prod(szA),szA);

function val = fmod(A,B)
val = A - fix(A/B)*B;

function val = hypot(A,B)
val = sqrt(A.^2+B.^2);

function val = conditional(C,A,B)
val       = A;
val(C==0) = B(C==0);