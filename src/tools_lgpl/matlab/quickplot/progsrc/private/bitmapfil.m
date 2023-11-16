function varargout=bitmapfil(FI,domain,field,cmd,varargin)
%BITMAPFIL QP support for bitmap files.
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
%   [hNew      ,NewFI]      = XXXFIL(FI,Domain,DataFld,'plot',Parent,Ops,hOld,subf,t,station,m,n,k)
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
    case 'subfields'
        varargout={{}};
        return
    case 'plotoptions'
        if isempty(varargin)
            t = 1;
        else
            t = varargin{1};
        end
        PlotOps.animate = isequal(t,0) || length(t)>1;
        varargout = {PlotOps};
        return
    case 'plot'
    otherwise
        error('''%s'' option not defined for bitmap',cmd)
end

Parent=varargin{1};
Ops=varargin{2};
hOld=varargin{3};

t=1;
if length(varargin)>3
    t=varargin{4};
end

switch Props.Name
    case 'bitmap'
        [hNew,xlim,ylim]=showimage(FI,Parent,t);
    case 'video frame'
        [hNew,xlim,ylim]=showimage(FI,Parent,t);
end

varargout={hNew FI};
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function Out=infile(FI,domain)
T_=1; ST_=2; M_=3; N_=4; K_=5;
%======================== SPECIFIC CODE =======================================
PropNames={'Name'                           'DimFlag' 'DataInCell' 'NVal'};
DataProps={'bitmap'                         [0 0 0 0 0]  0        -1     };
Out=cell2struct(DataProps,PropNames,2);
if isfield(FI,'vidObj')
    Out.Name = 'video frame';
    Out.DimFlag(T_)=7; % frames in seconds
elseif isfield(FI.FileInfo,'times')
    Out.DimFlag(T_)=5;
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function sz=getsize(FI,Props)
T_=1; ST_=2; M_=3; N_=4; K_=5;
sz=[0 0 0 0 0];
if Props.DimFlag(T_)
    if isfield(FI,'vidObj')
        sz(T_) = FI.vidObj.NumberOfFrames;
    else
        sz(T_) = length(FI.FileInfo.times);
    end
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function T=readtim(FI,Props,t)
%======================== SPECIFIC CODE =======================================
if isfield(FI,'vidObj')
    if t == 0
        t = 1:FI.vidObj.NumberOfFrames;
    end
    T = t/FI.vidObj.FrameRate;
    T = T/86400; % convert to days
else
    T = FI.FileInfo.times';
    if ~isequal(t,0)
        T = T(t);
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
        xminh=findobj(mfig,'tag','xmin');
        set(xminh,'string',sprintf('%g',FI.Loc(1)),'backgroundcolor',Active,'enable','on')
        yminh=findobj(mfig,'tag','ymin');
        set(yminh,'string',sprintf('%g',FI.Loc(2)),'backgroundcolor',Active,'enable','on')
        width=findobj(mfig,'tag','width');
        set(width,'string',sprintf('%g',FI.Loc(3)),'backgroundcolor',Active,'enable','on')
        height=findobj(mfig,'tag','height');
        set(height,'string',sprintf('%g',FI.Loc(4)),'backgroundcolor',Active,'enable','on')
    case {'xmin','ymin','width','height'}
        xminh=findobj(mfig,'tag',cmd);
        if nargin>3
            x=varargin{1};
            if ischar(x)
                x=str2num(x);
            end
        else
            x=str2num(get(xminh,'string'));
        end
        switch cmd
            case 'xmin'
                FI.Loc(1)=x;
            case 'ymin'
                FI.Loc(2)=x;
            case 'width'
                FI.Loc(3)=x;
            case 'height'
                FI.Loc(4)=x;
        end
        NewFI=FI;
        set(xminh,'string',sprintf('%g',x))
        cmdargs={cmd x};
    case 'bitmapfig'
        % get size in pixels
        [xlim,ylim,Width,Height] = getlims(FI);
        pxsz = [Width Height];
        % get size in map units
        sz = FI.Loc(3:4);
        % requested size is such that lowest resolution just matches pixels
        sz = min(pxsz./sz) * sz;
        ssz=qp_getscreen;
        fac=max(sz./ssz(3:4));
        if fac>1
            sz=floor(sz/fac);
        end
        pos=[ssz(1:2)+ssz(3:4)/2-sz/2 sz];
        Fg=qp_createfig('free format figure',FI.FileName);
        set(Fg,'units','pixels','position',pos,'resize','off');
        %
        Ax=axes('parent',Fg,'units','normalized','position',[0 0 1 1],'visible','off');
        set(Ax,'xlim',xlim,'ylim',ylim)
        d3d_qp('refreshfigs',Fg)
        d3d_qp('addtoplot')
        %
        cmdargs={cmd};
    otherwise
        error(['Unknown option command: ',cmd])
end
% -----------------------------------------------------------------------------

function [xlim,ylim,Width,Height] = getlims(FI)
if isfield(FI,'vidObj')
    Width = FI.vidObj.Width;
    Height = FI.vidObj.Height;
else
    Width = FI.FileInfo.Width;
    Height = FI.FileInfo.Height;
end
dx=abs(FI.Loc(3))/Width/2;
dy=abs(FI.Loc(4))/Height/2;
xlim=sort(FI.Loc(1)+[0 FI.Loc(3)])+[dx -dx];
ylim=sort(FI.Loc(2)+[0 FI.Loc(4)])+[dy -dy];

% -----------------------------------------------------------------------------
function [hNew,xlim,ylim]=showimage(FI,Parent,t)
[xlim,ylim] = getlims(FI);

if isfield(FI,'vidObj')
    Data = read(FI.vidObj, t);
    Alpha = [];
else
    if t == 0
        t = length(FI.FileInfo.times);
    end
    if isfield(FI.FileInfo,'times')
        tstr = sprintf(FI.FileInfo.format,FI.FileInfo.times(t));
        FileName=[FI.FileInfo.prefix tstr FI.FileInfo.postfix];
    else
        FileName=FI.FileName;
    end
    try
        [Data,cmap,Alpha]=imread(FileName);
    catch
        Data=imread(FileName);
        Alpha=[];
    end
    if size(Data,3)==1
        Data=idx2rgb(Data,FI.FileInfo.Colormap);
    end
end
Data=Data(end:-1:1,:,:);
ydir=get(Parent,'ydir');
set(Parent,'NextPlot','add')
fig = get(Parent,'parent');
if size(Data,3)==4
    hNew=image(xlim,ylim,Data(:,:,1:3), ...
        'parent',Parent, ...
        'AlphaData',Data(:,:,4));
    set(fig,'renderer','OpenGL') % need this for transparency
elseif ~isempty(Alpha)
    Alpha=Alpha(end:-1:1,:);
    hNew=image(xlim,ylim,Data, ...
        'parent',Parent, ...
        'AlphaData',Alpha);
    set(fig,'renderer','OpenGL') % need this for transparency
else
    hNew=image(xlim,ylim,Data, ...
        'parent',Parent);
end
set(Parent,'ydir',ydir)

% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function OK=optfig(h0)
Inactive=get(0,'defaultuicontrolbackground');
FigPos=get(h0,'position');
FigPos(3:4) = getappdata(h0,'DefaultFileOptionsSize');
set(h0,'position',FigPos)

voffset=FigPos(4)-30;
h1 = uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Position',[11 voffset 60 20], ...
    'String','minimum x', ...
    'HorizontalAlignment','left', ...
    'Style','text');
h1 = uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Callback','d3d_qp fileoptions xmin', ...
    'Position',[71 voffset 80 20], ...
    'String','', ...
    'HorizontalAlignment','right', ...
    'Enable','off', ...
    'Style','edit', ...
    'Tag','xmin');

h1 = uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Position',[191 voffset 60 20], ...
    'String','width', ...
    'HorizontalAlignment','left', ...
    'Style','text');
h1 = uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Callback','d3d_qp fileoptions width', ...
    'Position',[251 voffset 80 20], ...
    'String','', ...
    'HorizontalAlignment','right', ...
    'Enable','off', ...
    'Style','edit', ...
    'Tag','width');

voffset=voffset-25;
h1 = uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Position',[11 voffset 60 20], ...
    'String','minimum y', ...
    'HorizontalAlignment','left', ...
    'Style','text');
h1 = uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Callback','d3d_qp fileoptions ymin', ...
    'Position',[71 voffset 80 20], ...
    'String','', ...
    'HorizontalAlignment','right', ...
    'Enable','off', ...
    'Style','edit', ...
    'Tag','ymin');

h1 = uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Position',[191 voffset 60 20], ...
    'String','height', ...
    'HorizontalAlignment','left', ...
    'Style','text');
h1 = uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Callback','d3d_qp fileoptions height', ...
    'Position',[251 voffset 80 20], ...
    'String','', ...
    'HorizontalAlignment','right', ...
    'Enable','off', ...
    'Style','edit', ...
    'Tag','height');

voffset=voffset-25;
h1 = uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Callback','d3d_qp fileoptions bitmapfig', ...
    'Position',[11 voffset 320 20], ...
    'String','create figure with bitmap as background', ...
    'HorizontalAlignment','right');

OK=1;
