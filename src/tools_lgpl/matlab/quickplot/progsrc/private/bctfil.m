function varargout=bctfil(FI,domain,field,cmd,varargin)
%BCTFIL QP support for Delft3D boundary files.
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
        Parent=varargin{1};
        Ops=varargin{2};
        hOld=varargin{3};
        M=varargin{4};
        %
        QH = FI.Table(Props.Fld).Data;
        if M~=0
            QH = QH(M,:);
        end
        hNew=line(QH(:,1),QH(:,2),'color',Ops.colour, ...
            'linestyle',Ops.linestyle, ...
            'linewidth',Ops.linewidth, ...
            'marker',Ops.marker, ...
            'markersize',Ops.markersize, ...
            'markeredgecolor',Ops.markercolour, ...
            'markerfacecolor',Ops.markerfillcolour);
        setappdata(Parent,'AxesType','<blocking>')
        set(get(Parent,'title'),'string',FI.Table(Props.Fld).Location,'interpreter','none')
        set(get(Parent,'xlabel'),'string','discharge (m^3) \rightarrow')
        set(get(Parent,'ylabel'),'string','elevation (m) \rightarrow')
        varargout={hNew FI};
        return
    otherwise
        [XYRead,DataRead,DataInCell]=gridcelldata(cmd);
end

DimFlag=Props.DimFlag;

% initialize and read indices ...
idx={[] [] 0 0 0};
fidx=find(DimFlag);
idx(fidx(1:length(varargin)))=varargin;


x=[];
y=[];

% read data ...

if isequal(idx{T_},0)
    val1=FI.Table(Props.Fld).Data(:,Props.Prm);
else
    val1=FI.Table(Props.Fld).Data(idx{T_},Props.Prm);
end
val2=[];
T=readtim(FI,Props,idx{T_});

% generate output ...
if XYRead
    Ans.X=x;
    Ans.Y=y;
end
if Props.NVal==0
elseif Props.NVal==1
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
%======================== SPECIFIC CODE =======================================
PropNames={'Name'                       'DimFlag' 'DataInCell' 'NVal' 'VecType' 'Loc' 'ReqLoc' 'Loc3D' 'Fld' 'Prm'};
DataProps={};

%======================== SPECIFIC CODE DIMENSIONS ============================
l=0;
for i=1:length(FI.Table)
    for j=2:size(FI.Table(i).Parameter,2)
        l=l+1;
        if strcmp(FI.Table(i).Parameter(1).Name,'total discharge (t)')
            DataProps(l,:)={[FI.Table(i).Location ' - QH Table'] ...
                [0 0 1 0 0]  0         -1     ''        ''   ''      ''      i    j };
        else
            DataProps(l,:)={[FI.Table(i).Location ' - ' FI.Table(i).Parameter(j).Name] ...
                [1 0 0 0 0]  0          1     ''        ''   ''      ''      i    j };
        end
    end
end
Out=cell2struct(DataProps,PropNames,2);
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function sz=getsize(FI,Props)
T_=1; ST_=2; M_=3; N_=4; K_=5;
sz=[0 0 0 0 0];

%======================== SPECIFIC CODE =======================================
if Props.DimFlag(T_)
    sz(T_)=size(FI.Table(Props.Fld).Data,1);
else
    sz(M_)=size(FI.Table(Props.Fld).Data,1);
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function T=readtim(FI,Props,t)
%======================== SPECIFIC CODE =======================================
if t==0
    t=':';
end
T=bct_io('times',FI,Props.Fld,t);
% -----------------------------------------------------------------------------
