function cmap=md_colormap(varargin)
%MD_COLORMAP Colour map editor.
%    MD_COLORMAP(CMAPOBJ) starts an interactive colormap editor to edit the
%    specified colormap. By default the editor starts with the colormap
%    JET.
%
%    CMAPOBJ = MD_COLORMAP(...) returns a structure containing the edited
%    colormap. Use the CLRMAP command to convert the structure to a
%    standard MATLAB colormap array.
%
%    See also CLRMAP.

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

if nargin==0 || ~ischar(varargin{1})
    if nargout==1
        cmap=md_colormap_interface(varargin{:});
    else
        md_colormap_interface(varargin{:});
    end
    return
end

colorfiles = {'*.hls' 'Classic Delft3D HLS Files'
    '*.c3g' 'CSS3 Gradients'
    '*.ggr' 'GNU Image Manipulation Program Gradients'
    '*.gpf' 'GNUplot Palettes'
    '*.cpt' 'GMT Palettes'
    '*.inc' 'POV-Ray Colour Map Headers'
    '*.pg'  'PostGIS (ST_ColorMap) Colour Maps'
    '*.qgs' 'QGIS Colour Ramps'
    '*.sao' 'SAOImage DS9 Colour Maps'
    '*.svg' 'Scalar Vector Graphics Gradients'
    '*.*'   'All Files'};

F=gcbf;
cmd=lower(varargin{1});
if isequal(cmd,'close')
    tg=get(F,'tag');
    switch tg
        case 'STANDALONE MODE'
            delete(F)
        case 'FUNCTION MODE'
            set(F,'visible','off')
    end
    return
end

updateuicontrols
UD=get(F,'userdata');
S=UD{1};
uih=UD{2};
currentcolor=UD{3};
a1=findobj(F,'tag','Colorbar');
m = get(a1,'userdata');

switch cmd
    case 'down'
        a1=gcbo;
        if ~isequal(get(a1,'type'),'axes')
            a1=get(a1,'parent');
        end
        cp=get(a1,'currentpoint'); cp=cp(1);
        N=size(S.Colors,1);
        if S.AlternatingColors
            uniform=1;
            index=(0.5+(0:N-1))/N*(1+1/m)-1/(2*m);
        elseif isempty(S.Index)
            uniform=1;
            index=(0:N-1)/(N-1);
        else
            uniform=0;
            index=S.Index;
        end
        button=get(F,'selectiontype');
        switch button
            case 'normal'
                [dummy,currentcolor]=min(abs(index-cp));
                set(F,'windowbuttonmotionfcn','md_colormap motion')
                set(F,'windowbuttonupfcn','md_colormap up')
            case 'alt'
                j=sum(index<cp);
                clr=clrmap(S,cp);
                switch S.Space
                    case 'RGB'
                    case 'CMY'
                        clr=1-clr;
                    case 'HSV'
                        clr=rgb2hsv(clr);
                    case 'HLS'
                        clr=rgb2hls(clr);
                end
                if ~uniform
                    S.Index(j+2:end+1)=S.Index(j+1:end);
                    S.Index(j+1)=cp;
                else
                    S.Index=[];
                end
                S.Colors(j+2:end+1,:)=S.Colors(j+1:end,:);
                S.Colors(j+1,:)=clr;
                currentcolor=j+1;
        end
    case 'motion'
        cp=get(findobj(F,'tag','Colorbar'),'currentpoint');
        S0=S;
        currentcolor0=currentcolor;
        [S,currentcolor]=moveit(S,currentcolor,cp,F);
        set(F,'userdata',{S0 uih currentcolor0 S currentcolor});
        updateinterface(F,S,currentcolor,uih)
        return
    case 'up'
        set(F,'windowbuttonmotionfcn','')
        set(F,'windowbuttonupfcn','')
        if length(UD)>3
            S=UD{4};
            currentcolor=UD{5};
        end
    case 'index'
        Idx=findobj(F,'tag','index');
        cp=str2double(get(Idx,'string'));
        if length(cp)==1
            cp(1,2)=0.5;
            [S,currentcolor]=moveit(S,currentcolor,cp,F);
        else
            set(Idx,'string',sprintf('%.4f',S.Index(currentcolor)))
        end
    case 'altcolors'
        S.AlternatingColors=get(gcbo,'value');
    case 'options'
        a1=findobj(F,'tag','Colorbar');
        m = get(a1,'userdata');
        M=md_dialog('Select Option',{'Length of Colour Map'},{'editint'},{[1 max(m,256)]},{m});
        if ~isempty(M)
            m = M{1};
            set(a1,'userdata',m)
            cbI=findobj(a1,'tag','ColorbarImage2');
            set(cbI,'cdata',1:m);
            axI=get(cbI,'parent');
            set(axI,'xlim',[0 1]+[-1 1]/m/2);
        end
    case 'uniform'
        uniform=get(gcbo,'value');
        if uniform
            S.Index=[];
        else
            N=size(S.Colors,1);
            S.Index=(0:N-1)/(N-1);
        end
    case 'grayscale'
        switch S.Space
            case {'RGB','CMY'}
                S.Colors=repmat(mean(S.Colors,2),1,3);
            case 'HSV'
                RGB=hsv2rgb(S.Colors);
                RGB=repmat(mean(RGB,2),1,3);
                S.Colors=rgb2hsv(RGB);
            case 'HLS'
                RGB=hls2rgb(S.Colors);
                RGB=repmat(mean(RGB,2),1,3);
                S.Colors=rgb2hls(RGB);
        end
    case 'reverse'
        S.Colors=flipud(S.Colors);
        S.Index=1-flipud(S.Index(:));
    case 'invert'
        switch S.Space
            case {'RGB','CMY'}
                S.Colors=1-S.Colors;
            case 'HSV'
                RGB=hsv2rgb(S.Colors);
                RGB=1-RGB;
                S.Colors=rgb2hsv(RGB);
            case 'HLS'
                RGB=hls2rgb(S.Colors);
                RGB=1-RGB;
                S.Colors=rgb2hls(RGB);
        end
    case {'red','green','blue','select','cyan','magenta','yellow'}
        switch cmd
            case 'select'
                switch S.Space
                    case 'RGB'
                        RGB=S.Colors(currentcolor,:);
                    case 'CMY'
                        RGB=1-S.Colors(currentcolor,:);
                    case 'HSV'
                        RGB=hsv2rgb(S.Colors(currentcolor,:));
                    case 'HLS'
                        RGB=hls2rgb(S.Colors(currentcolor,:));
                end
                RGB=uisetcolor(RGB);
                if isequal(RGB,0)
                    return
                end
            case {'red','green','blue'}
                RGB(1,3)=str2double(get(uih(3,1),'string'));
                RGB(1,2)=str2double(get(uih(2,1),'string'));
                RGB(1,1)=str2double(get(uih(1,1),'string'));
                if any(isnan(RGB))
                    RGB=0;
                else
                    RGB=RGB/255;
                end
            case {'cyan','magenta','yellow'}
                CMY(1,3)=str2double(get(uih(3,4),'string'));
                CMY(1,2)=str2double(get(uih(2,4),'string'));
                CMY(1,1)=str2double(get(uih(1,4),'string'));
                if any(isnan(CMY))
                    RGB=0;
                else
                    CMY=CMY/255;
                    RGB=1-CMY;
                end
        end
        if ~isequal(RGB,0)
            switch S.Space
                case 'RGB'
                    S.Colors(currentcolor,:)=RGB;
                case 'CMY'
                    S.Colors(currentcolor,:)=1-RGB;
                case 'HSV'
                    S.Colors(currentcolor,:)=rgb2hsv(RGB);
                case 'HLS'
                    S.Colors(currentcolor,:)=rgb2hls(RGB);
            end
        end
    case {'hsv-hue','hsv-saturation','value'}
        HSV(3)=str2double(get(uih(3,2),'string'));
        HSV(2)=str2double(get(uih(2,2),'string'));
        HSV(1)=str2double(get(uih(1,2),'string'));
        if any(isnan(HSV))
            HSV=0;
        else
            HSV=[HSV{:}]/255;
        end
        if ~isequal(HSV,0)
            switch S.Space
                case 'RGB'
                    S.Colors(currentcolor,:)=hsv2rgb(HSV);
                case 'HSV'
                    S.Colors(currentcolor,:)=HSV;
                case 'HLS'
                    RGB=hsv2rgb(HSV);
                    S.Colors(currentcolor,:)=rgb2hls(RGB);
                case 'CMY'
                    S.Colors(currentcolor,:)=1-hsv2rgb(HSV);
            end
        end
    case {'hls-hue','lightness','hls-saturation'}
        HLS(1,3)=str2double(get(uih(3,3),'string'));
        HLS(1,2)=str2double(get(uih(2,3),'string'));
        HLS(1,1)=str2double(get(uih(1,3),'string'));
        if any(isnan(HLS))
            HLS=0;
        else
            HLS=HLS/255;
        end
        if ~isequal(HLS,0)
            switch S.Space
                case 'RGB'
                    S.Colors(currentcolor,:)=hls2rgb(HLS);
                case 'HSV'
                    RGB=hls2rgb(HLS);
                    S.Colors(currentcolor,:)=rgb2hsv(RGB);
                case 'HLS'
                    S.Colors(currentcolor,:)=HLS;
                case 'CMY'
                    S.Colors(currentcolor,:)=1-hls2rgb(HLS);
            end
        end
    case 'name'
        S.Name=get(gcbo,'string');
        setappdata(gcbo,'NameChanged',1)
    case 'colorspace'
        spaces=get(gcbo,'string');
        value=get(gcbo,'value');
        newspace=spaces{value};
        S = convertcolors(S,newspace);
    case 'save'
        try
            qp_path=qp_basedir;
            clrmap_path=[qp_path filesep 'colormaps'];
        catch
            clrmap_path=pwd;
        end
        %
        [f,p]=uiputfile(fullfile(clrmap_path,[S.Name '.clrmap']));
        if ~ischar(f)
            return;
        end
        [pp,ff,ee]=fileparts(f);
        if isempty(ee)
            f=[f '.clrmap'];
        end
        filename=[p f];
        %
        Nm=findobj(F,'tag','name');
        if ~getappdata(Nm,'NameChanged')
            [pp,ff,ee]=fileparts(f);
            S.Name = ff;
        end
        %
        try
            clrmap('write',filename,S);
        catch
            ui_message('error',lasterr);
        end
        set(Nm,'string',S.Name)
        setappdata(Nm,'NameChanged',0)
        return
    case 'export'
        [f,p]=uiputfile(colorfiles(1:end-1,:),'Save as',S.Name);
        if ~ischar(f)
            return
        end
        filename=[p f];
        a1=findobj(F,'tag','Colorbar');
        m = get(a1,'userdata');
        map=clrmap(S,m);
        try
            switch filename(end-2:end)
                case 'c3g'
                    CSS3clr('write',filename,S,S.Name);
                case 'cpt'
                    GMTclr('write',filename,S,S.Name);
                case 'ggr'
                    GIMPclr('write',filename,S,S.Name);
                case 'gpf'
                    gnuplot_clr('write',filename,S,S.Name);
                case 'hls'
                    qnhls('write',filename,map,S.Name);
                case 'inc'
                    POVRAYclr('write',filename,S,S.Name);
                case 'pg'
                    PGclr('write',filename,S,S.Name);
                case 'qgs'
                    QGISclr('write',filename,S,S.Name);
                case 'sao'
                    SAO_DS9clr('write',filename,S,S.Name);
                case 'svg'
                    SVGclr('write',filename,S,S.Name);
            end
        catch Ex
            qp_error('Catch in md_colormap',Ex);
        end
        return
    case 'apply'
        appfig=get(0,'currentfigure');
        if isempty(appfig)
            return
        end
        currentmap = get(appfig,'colormap');
        m = size(currentmap,1);
        map=clrmap(S,m);
        set(appfig,'colormap',map);
        return;
    case {'load','import'}
        switch cmd
            case 'load'
                try
                    qp_path=qp_basedir;
                    clrmap_path=[qp_path filesep 'colormaps'];
                catch
                    clrmap_path=pwd;
                end
                %
                [f,p]=uigetfile(fullfile(clrmap_path,'*.clrmap'));
                if ~ischar(f)
                    return
                end
                filename=[p f];
                try
                    SS=clrmap('read',filename);
                catch
                    ui_message('error',lasterr);
                    return
                end
            case 'import'
                [f,p]=uigetfile(colorfiles);
                if ~ischar(f)
                    return
                end
                filename=[p f];
                SS = [];
                for FT = colorfiles(:,1)'
                    ft = FT{1}(3:end);
                    try
                        if strcmp(ft,'*')
                            [pp,ff,ee] = fileparts(filename);
                            ft = lower(ee(2:end));
                        end
                        switch ft
                            case 'c3g'
                                SS = CSS3clr('read',filename);
                            case 'cpt'
                                SS = GMTclr('read',filename);
                            case 'ggr'
                                SS = GIMPclr('read',filename);
                            case 'gpf'
                                SS = gnuplot_clr('read',filename);
                            case 'inc'
                                SS = POVRAYclr('read',filename);
                            case 'pg'
                                SS = PGclr('read',filename);
                            case 'qgs'
                                SS = QGISclr('read',filename);
                            case 'sao'
                                SS = SAO_DS9clr('read',filename);
                            case 'svg'
                                SS = SVGclr('read',filename);
                            otherwise
                                [map,label]=qnhls('read',filename);
                                map=rgb2hls(map);
                                SS.Name=label;
                                SS.Space='HLS';
                                SS.Colors=map;
                                SS.AlternatingColors=0;
                                SS.Index=[];
                        end
                        break
                    catch Ex
                    end
                end
                if isempty(SS)
                    qp_error('Catch in md_colormap:',Ex,'md_colormap')
                    return
                end
        end
        S=SS;
        if ~isfield(S,'AlternatingColors')
            S.AlternatingColors=0;
        end
        AC=findobj(F,'tag','altcolors');
        set(AC,'value',S.AlternatingColors);
        if ~isfield(S,'Index')
            S.Index=[];
        end
        uniform=isempty(S.Index);
        Un=findobj(F,'tag','uniform');
        set(Un,'value',uniform)
        Nm=findobj(F,'tag','name');
        setappdata(Nm,'NameChanged',0)
        if isfield(S,'Name')
            set(Nm,'string',S.Name);
        else
            set(Nm,'string','<no name>');
        end
        CSp=findobj(F,'tag','colorspace');
        spaces=get(CSp,'string');
        S.Space=upper(S.Space);
        space=strmatch(S.Space,spaces,'exact');
        if isempty(space)
            error('Unknown colour space')
        end
        set(CSp,'value',space)
        currentcolor=1;

    otherwise
        fprintf('Unknown command: %s\n',cmd);
end

set(F,'userdata',{S uih currentcolor});
updateinterface(F,S,currentcolor,uih)


function S = convertcolors(S,newspace)
currentspace = S.Space;
if iscell(newspace)
    if ismember(currentspace,newspace)
        return
    else
        newspace = newspace{1};
    end
elseif isequal(currentspace,newspace)
    return
end
clrs=S.Colors;
switch [currentspace '->' newspace]
    case 'RGB->HSV'
        clrs=rgb2hsv(clrs);
    case 'RGB->HLS'
        clrs=rgb2hls(clrs);
    case 'RGB->CMY'
        clrs=1-clrs;
    case 'HSV->RGB'
        clrs=hsv2rgb(clrs);
    case 'HSV->HLS'
        clrs=hsv2rgb(clrs);
        clrs=rgb2hls(clrs);
    case 'HSV->CMY'
        clrs=1-hsv2rgb(clrs);
    case 'HLS->RGB'
        clrs=hls2rgb(clrs);
    case 'HLS->HSV'
        clrs=hls2rgb(clrs);
        clrs=rgb2hsv(clrs);
    case 'HLS->CMY'
        clrs=1-hls2rgb(clrs);
    case 'CMY->RGB'
        clrs=1-clrs;
    case 'CMY->HSV'
        clrs=rgb2hsv(1-clrs);
    case 'CMY->HLS'
        clrs=rgb2hls(1-clrs);
end
S.Colors=clrs;
S.Space=newspace;


function S1=md_colormap_interface(S,uicontrolfont)
if nargin==0
    S.Name='jet';
    S.Space='RGB';
    S.Index=[0 1/8 3/8 5/8 7/8 1];
    S.Colors=[0 0 0.5; 0 0 1; 0 1 1; 1 1 0; 1 0 0; 0.5 0 0];
end
if ishandle(S)
    GCF=S;
    S.Name='current colormap';
    cmap=get(GCF,'colormap');
    N=size(cmap,1);
    % Alternating colors
    period=1;
    ALTikeep=[];
    while isempty(ALTikeep)
        ALTcmap=repmat(cmap(period,:),ceil(N/length(period)),1);
        ALTcmap=ALTcmap(1:N,:);
        FirstDiff=min(find(~all((cmap-ALTcmap)==0,2)));
        if isempty(FirstDiff)
            ALTikeep=period;
            break;
        end
        period=1:FirstDiff;
    end
    % RGB
    RGBcmap=cmap;
    RGBirem=1+find(all(abs(diff(diff(RGBcmap)))<1e-4,2));
    RGBikeep=setdiff(1:N,RGBirem);
    % HSV
    HSVcmap=rgb2hsv(cmap);
    HSVirem=1+find(all(abs(diff(diff(HSVcmap)))<1e-4,2));
    HSVikeep=setdiff(1:N,HSVirem);
    %
    if length(ALTikeep)<min(length(RGBikeep),length(HSVikeep))
        S.Space='RGB';
        S.AlternatingColors=1;
        ikeep=ALTikeep;
    elseif length(HSVikeep)>length(RGBikeep)
        S.Space='RGB';
        ikeep=RGBikeep;
    else
        S.Space='HSV';
        ikeep=HSVikeep;
        cmap=HSVcmap;
    end
    S.Index=(ikeep-1)/max(ikeep-1);
    S.Colors=cmap(ikeep,:);
elseif ~isstruct(S)
    error('Invalid input argument.')
end
if nargin<2
    uicontrolfont.DefaultUicontrolFontWeight = get(0,'DefaultUicontrolFontWeight');
end
sz=qp_getscreen;

try
    Inactive = qp_settings('UIInActiveColor');
    Active   = qp_settings('UIActiveColor'); 
catch
    Inactive = get(0,'defaultuicontrolbackgroundcolor');
    Active   = [1 1 1];
end

dims=[390 225];
pos(1:2)=sz(1:2)+(sz(3:4)-dims)/2;
pos(3:4)=dims;

if ~isfield(S,'Name')
    Name='<no name>';
else
    Name=S.Name;
end
uniform=1;
if isfield(S,'Index') && ~isempty(S.Index)
    uniform=0;
else
    S.Index=[];
end

S.Space=upper(S.Space);
Spaces={'RGB','HSV','HLS','CMY'};
spaceval=strmatch(S.Space,Spaces,'exact');
if isempty(spaceval)
    error('Unknown colour space.')
end

if isfield(S,'AlternatingColors') && isequal(S.AlternatingColors,1)
    AlternatingColors=1;
else
    AlternatingColors=0;
end
S.AlternatingColors=AlternatingColors;

h0 = figure('Visible','on', ...
    'Units','pixels', ...
    'Color',Inactive, ...
    'IntegerHandle','off', ...
    'MenuBar','none', ...
    'Name','Colour Map Editor', ...
    'Doublebuffer','on', ...
    'CloseRequestFcn','', ...
    'NumberTitle','off', ...
    'Resize','off', ...
    'Position',pos, ...
    'Handlevisibility','callback', ...
    'Tag','STANDALONE MODE', ...
    uicontrolfont);
setappdata(h0,'WL_UserInterface',1)

m0=uimenu('parent',h0, ...
    'label','&File');
uimenu('parent',m0, ...
    'label','&Open...', ...
    'accelerator','O', ...
    'callback','md_colormap load');
uimenu('parent',m0, ...
    'label','&Save...', ...
    'accelerator','S', ...
    'callback','md_colormap save');
uimenu('parent',m0, ...
    'label','&Import...', ...
    'accelerator','I', ...
    'separator','on', ...
    'callback','md_colormap import');
uimenu('parent',m0, ...
    'label','&Export...', ...
    'accelerator','E', ...
    'callback','md_colormap export');
uimenu('parent',m0, ...
    'label','&Apply...', ...
    'accelerator','A', ...
    'callback','md_colormap apply');
uimenu('parent',m0, ...
    'label','&Close', ...
    'separator','on', ...
    'callback','md_colormap close');

m0=uimenu('parent',h0, ...
    'label','&Options');
uimenu('parent',m0, ...
    'label','&Settings', ...
    'callback','md_colormap options');
uimenu('parent',m0, ...
    'label','Convert to &Gray Scale', ...
    'callback','md_colormap grayscale');
uimenu('parent',m0, ...
    'label','&Reverse Colour Map', ...
    'callback','md_colormap reverse');
uimenu('parent',m0, ...
    'label','&Invert Colour Map', ...
    'callback','md_colormap invert');

%======

voffset=196;
uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Position',[11 voffset 40 18], ...
    'String','Name', ...
    'Horizontalalignment','left', ...
    'Style','text', ...
    'Enable','on');
Nm = uicontrol('Parent',h0, ...
    'BackgroundColor',Active, ...
    'Callback','md_colormap name', ...
    'Position',[51 voffset 330 20], ...
    'String',Name, ...
    'Horizontalalignment','left', ...
    'Style','edit', ...
    'Enable','on', ...
    'tag','name');
setappdata(Nm,'NameChanged',0)

%======

voffset=171;
uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Position',[11 voffset 40 18], ...
    'String','Space', ...
    'Horizontalalignment','left', ...
    'Style','text', ...
    'Enable','on');
uicontrol('Parent',h0, ...
    'BackgroundColor',Active, ...
    'Callback','md_colormap colorspace', ...
    'Position',[51 voffset 60 20], ...
    'String',Spaces, ...
    'value',spaceval, ...
    'Horizontalalignment','right', ...
    'Style','popupmenu', ...
    'Enable','on', ...
    'tag','colorspace');
%---
uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Callback','md_colormap uniform', ...
    'Position',[131 voffset 250 18], ...
    'String','Reference Colours Uniformly Distributed', ...
    'Horizontalalignment','left', ...
    'Style','checkbox', ...
    'Value',uniform, ...
    'Enable','on', ...
    'tag','uniform');

%======

voffset=146;
uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Position',[11 voffset 40 18], ...
    'String','Index', ...
    'Horizontalalignment','left', ...
    'Style','text', ...
    'Enable','on', ...
    'tag','indextext');
uicontrol('Parent',h0, ...
    'BackgroundColor',Active, ...
    'Callback','md_colormap index', ...
    'Position',[51 voffset 60 20], ...
    'String','', ...
    'Horizontalalignment','right', ...
    'Style','edit', ...
    'Enable','on', ...
    'tag','index');
%---
uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Position',[131 voffset 120 18], ...
    'String','Colour Order Fixed', ...
    'Horizontalalignment','left', ...
    'Style','checkbox', ...
    'Value',1, ...
    'Enable','on', ...
    'tag','dragorder');
%---
uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Callback','md_colormap altcolors', ...
    'Position',[261 voffset 120 18], ...
    'String','Alternating Colours', ...
    'Horizontalalignment','left', ...
    'Style','checkbox', ...
    'Value',AlternatingColors, ...
    'Enable','on', ...
    'tag','altcolors');

%======

m = size(get(h0,'colormap'),1);
voffset=121;
a1 = axes('Parent',h0, ...
    'Color',Active, ...
    'Units','Pixels', ...
    'Buttondownfcn','md_colormap down', ...
    'Position',[11 voffset 370 20], ...
    'tag','Colorbar', ...
    'xtick',[], ...
    'ytick',[], ...
    'xlim',[0 1]+[-1 1]/m/2, ...
    'ylim',[0 1], ...
    'box','on', ...
    'userdata',m, ...
    'layer','top');
image('Parent',a1, ...
    'Buttondownfcn','md_colormap down', ...
    'cdata',1:m, ...
    'xdata',[0 1], ...
    'ydata',[1/3 2/3], ...
    'tag','ColorbarImage');
image('Parent',a1, ...
    'Buttondownfcn','md_colormap down', ...
    'cdata',1:m, ...
    'xdata',[0 1], ...
    'ydata',[1 4/3], ...
    'tag','ColorbarImage2');

%=====

voffset=36;
uih=zeros(3,4);
uih=cv_edit(h0,Inactive, 11,voffset+50,uih,1,1,'red'           ,'Red');
uih=cv_edit(h0,Inactive, 11,voffset+25,uih,2,1,'green'         ,'Green');
uih=cv_edit(h0,Inactive, 11,voffset+00,uih,3,1,'blue'          ,'Blue');

%======

uih=cv_edit(h0,Inactive,141,voffset+50,uih,1,2,'HSV-hue'       ,'Hue');
uih=cv_edit(h0,Inactive,141,voffset+25,uih,2,2,'HSV-saturation','Saturation');
uih=cv_edit(h0,Inactive,141,voffset+00,uih,3,2,'value'         ,'Value');

%======

uih=cv_edit(h0,Inactive,281,voffset+50,uih,1,3,'HLS-hue'       ,'Hue');
uih=cv_edit(h0,Inactive,281,voffset+25,uih,2,3,'lightness'     ,'Lightness');
uih=cv_edit(h0,Inactive,281,voffset+00,uih,3,3,'HLS-saturation','Saturation');

%======

uih=cv_edit(h0,Inactive, 11,voffset+50,uih,1,4,'cyan'          ,'Cyan');
uih=cv_edit(h0,Inactive, 11,voffset+25,uih,2,4,'magenta'       ,'Magenta');
uih=cv_edit(h0,Inactive, 11,voffset+00,uih,3,4,'yellow'        ,'Yellow');

%======

voffset=11;
uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Callback','md_colormap select', ...
    'Position',[11 voffset 100 18], ...
    'String','Select Colour...', ...
    'Enable','on', ...
    'Tag','selcolor');
uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Callback','md_colormap close', ...
    'Position',[261 voffset 100 18], ...
    'String','Close', ...
    'Enable','on', ...
    'Tag','close');

%======

if matlabversionnumber > 5.01
    h1vec=allchild(h0)';
    c10=char(10);
    for h1=h1vec,
        t=get(h1,'tag');
        if ~isempty(t)
            switch t
                case {'red','green','blue','cyan','magenta','yellow'}
                    Str=['Specify ' t ' colour component.'];
                case 'HSV-hue'
                    Str='Specify HSV hue: 0=red, 85=green, 170=blue.';
                case {'HSV-saturation','HLS-saturation'}
                    Str='Specify saturation: 0=white, 255=full colour.';
                case 'value'
                    Str='Specify gray component value: 0=black, 255=full colour.';
                case 'HLS-hue'
                    Str='Specify HLS hue: 85=red, 170=green, 0=blue.';
                case 'lightness'
                    Str='Specify lightness: 0=black, 128=full colour, 255=white.';
                case 'index'
                    Str=['Specify relative colour map index: location of current colour in colour map.',c10,'Alternative click and drag colour along colour bar.'];
                case 'name'
                    Str='Specify name of colour map.';
                case 'colorspace'
                    Str='Select colour space.';
                case 'selcolor'
                    Str='Select from standard colour interface.';
                case 'close'
                    Str='Close this interface without further confirmation.';
                case 'dragorder'
                    Str='Maintain colour order while dragging colours along colour bar.';
                case 'uniform'
                    Str='Force colours to be separated by equal intervals.';
                otherwise
                    Str='';
            end
            if ~isempty(Str)
                set(h1,'tooltip',Str);
            end
        end
    end
end

%=====

set(h0,'userdata',{S uih 1})
set(uih(1),'userdata',{S -1})
updateinterface(h0,S,1,uih)

if nargout==1
    set(h0,'tag','FUNCTION MODE')
    waitfor(h0,'visible');
    UD=get(h0,'userdata');
    S1=UD{1};
    delete(h0);
end


function updateinterface(F,S,currentcolor,uih)
a1=findobj(F,'tag','Colorbar');
N=size(S.Colors,1);


m = get(a1,'userdata');
set(F,'colormap',clrmap(S,m));
xdata=[0 1];
if S.AlternatingColors
    corf=(1+1/m);
    cors=-1/(2*m);
    xdata=[0 1]+[-1 1]/(2*m)-[-1 1]/(2*N);
    m=N;
end
cbI=findobj(a1,'tag','ColorbarImage');
set(cbI,'xdata',xdata,'cdata',1:m);

index=(0:N-1)/(N-1);
uniform=1;
if S.AlternatingColors
    index=(0.5+(0:N-1))/N*corf+cors;
elseif ~isempty(S.Index)
    uniform=0;
    index=max(0,min(1,S.Index));
    if index(1)~=0
        index(1)=0;
    end
    if index(end)~=1
        index(end)=1;
    end
end

t=findall(a1,'type','patch');
if length(t)~=N
    delete(t);
    for i=1:N
        t=patch(index(i)+[0 -1 1]/100,[-0.3 -1.3 -1.3]/4,1, ...
            'Buttondownfcn','md_colormap down', ...
            'parent',a1, ...
            'edgecolor','k', ...
            'facecolor','w', ...
            'clipping','off');
        if i==currentcolor
            set(t,'facecolor','k')
        end
    end
else
    for i=1:N
        set(t(i),'xdata',index(i)+[0 -1 1]/100, ...
            'ydata',[-0.3 -1.3 -1.3]/4, ...
            'facecolor','w');
        if i==currentcolor
            set(t(i),'facecolor','k')
        end
    end
end

try
    Inactive = qp_settings('UIInActiveColor');
    Active   = qp_settings('UIActiveColor'); 
catch
    Inactive = get(0,'defaultuicontrolbackgroundcolor');
    Active   = [1 1 1];
end

cpstr=sprintf('%.4f',index(currentcolor));
if uniform
    set(findobj(F,'tag','index'),'enable','inactive','backgroundcolor',Inactive,'string',cpstr)
elseif currentcolor==1 || currentcolor==N
    set(findobj(F,'tag','index'),'enable','inactive','backgroundcolor',Inactive,'string',cpstr)
else
    set(findobj(F,'tag','index'),'enable','on','backgroundcolor',Active,'string',cpstr)
end

Prev=get(uih(1),'userdata');
if currentcolor~=Prev{2} || ~isequal(S.Colors,Prev{1}.Colors)
    set(uih(1),'userdata',{S currentcolor})
    switch S.Space
        case 'RGB'
            set(uih(:,1),'backgroundcolor',Active)
            set(uih(:,[2 3]),'backgroundcolor',Inactive)
            set(uih(:,1:3,:),'visible','on')
            set(uih(:,4,:),'visible','off')
            RGB=S.Colors(currentcolor,:);
            HSV=rgb2hsv(RGB);
            HLS=rgb2hls(RGB);
            CMY=1-RGB;
        case 'HSV'
            set(uih(:,2),'backgroundcolor',Active)
            set(uih(:,[1 3]),'backgroundcolor',Inactive)
            set(uih(:,1:3,:),'visible','on')
            set(uih(:,4,:),'visible','off')
            HSV=S.Colors(currentcolor,:);
            RGB=hsv2rgb(HSV);
            HLS=rgb2hls(RGB);
            CMY=1-RGB;
        case 'HLS'
            set(uih(:,3),'backgroundcolor',Active)
            set(uih(:,[1 2]),'backgroundcolor',Inactive)
            set(uih(:,1:3,:),'visible','on')
            set(uih(:,4,:),'visible','off')
            HLS=S.Colors(currentcolor,:);
            RGB=hls2rgb(HLS);
            HSV=rgb2hsv(RGB);
            CMY=1-RGB;
        case 'CMY'
            set(uih(:,4),'backgroundcolor',Active)
            set(uih(:,[2 3]),'backgroundcolor',Inactive)
            set(uih(:,2:4,:),'visible','on')
            set(uih(:,1,:),'visible','off')
            CMY=S.Colors(currentcolor,:);
            RGB=1-CMY;
            HSV=rgb2hsv(RGB);
            HLS=rgb2hls(RGB);
    end
    RGB=round(RGB*255);
    HSV=round(HSV*255);
    HLS=round(HLS*255);
    CMY=round(CMY*255);

    set(uih(1,1),'string',num2str(RGB(1)))
    set(uih(2,1),'string',num2str(RGB(2)))
    set(uih(3,1),'string',num2str(RGB(3)))

    set(uih(1,2),'string',num2str(HSV(1)))
    set(uih(2,2),'string',num2str(HSV(2)))
    set(uih(3,2),'string',num2str(HSV(3)))

    set(uih(1,3),'string',num2str(HLS(1)))
    set(uih(2,3),'string',num2str(HLS(2)))
    set(uih(3,3),'string',num2str(HLS(3)))

    set(uih(1,4),'string',num2str(CMY(1)))
    set(uih(2,4),'string',num2str(CMY(2)))
    set(uih(3,4),'string',num2str(CMY(3)))
end

function [S,currentcolor]=moveit(S,currentcolor,cp,F)
deletepoint=cp(1,2)<-1 || cp(1,2)>2;
a1=findobj(F,'tag','Colorbar');
N=size(S.Colors,1);
m = get(a1,'userdata');
uniform=1;
index=(0:N-1)/(N-1);
if S.AlternatingColors
    index=(0.5+(0:N-1))/N*(1+1/m)-1/(2*m);
elseif ~isempty(S.Index);
    index=S.Index;
    uniform=0;
end
if deletepoint && N>2
    S.Colors(currentcolor,:)=[];
    if ~isempty(S.Index)
        S.Index(currentcolor)=[];
        if currentcolor==1 || currentcolor==N
            S.Index=(S.Index-S.Index(1))/(S.Index(end)-S.Index(1));
        end
    end
    if currentcolor==N
        currentcolor=N-1;
    end
else
    cp=cp(1);
    if ~get(findobj(F,'tag','dragorder'),'value')
        while currentcolor>1 && index(currentcolor-1)>cp
            index(currentcolor+[-1 0])=index(currentcolor+[0 -1]);
            S.Colors(currentcolor+[-1 0],:)=S.Colors(currentcolor+[0 -1],:);
            currentcolor=currentcolor-1;
        end
        if currentcolor>1 && index(currentcolor-1)==cp
            cp=cp+eps;
        end
        while currentcolor<N && index(currentcolor+1)<cp
            index(currentcolor+[1 0])=index(currentcolor+[0 1]);
            S.Colors(currentcolor+[1 0],:)=S.Colors(currentcolor+[0 1],:);
            currentcolor=currentcolor+1;
        end
        if currentcolor<N && index(currentcolor+1)==cp
            cp=cp-eps;
        end
    elseif ~uniform
        if currentcolor>1
            cp=max(cp,S.Index(currentcolor-1)+eps);
        end
        if currentcolor<N
            cp=min(cp,S.Index(currentcolor+1)-eps);
        end
    end
    if ~uniform
        index(currentcolor)=cp;
        S.Index=index;
        if S.Index(1)~=0 || S.Index(end)~=1
            S.Index=(S.Index-S.Index(1))/(S.Index(end)-S.Index(1));
        end
    end
end


function uih=cv_edit(h0,Inactive,hoffset,voffset,uih,i,j,tag,str)
uih(i,j,2) = uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Position',[hoffset voffset 60 18], ...
    'String',str, ...
    'Horizontalalignment','left', ...
    'Style','text', ...
    'Enable','on');
uih(i,j,1) = uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Callback',['md_colormap ' tag], ...
    'Position',[hoffset+60 voffset 40 20], ...
    'String','0', ...
    'Horizontalalignment','right', ...
    'Style','edit', ...
    'Enable','on', ...
    'tag',tag);


function varargout = GMTclr(cmd,filename,varargin)
% Description: http://gmtrac.soest.hawaii.edu/doc/latest/GMT_Docs.html#color-palette-tables
switch cmd
    case 'read'
        [p,label] = fileparts(filename);
        %
        fid = fopen(filename,'r','n','UTF-8');
        oldPos = ftell(fid);
        Line = fgetl(fid);
        lineNr = 1;
        while ischar(Line) && ~isempty(Line) && Line(1)=='#'
            if lineNr==1
                quotes = find(Line == '"');
                if length(quotes)==2
                    label = Line(quotes(1)+1:quotes(2)-1);
                end
            end
            %
            iCM = strfind(Line,'COLOR_MODEL');
            if ~isempty(iCM)
                ColorModel = sscanf(Line(iCM:end),'COLOR_MODEL = %s');
            end
            %
            oldPos = ftell(fid);
            Line = fgetl(fid);
            lineNr = lineNr+1;
        end
        %
        % check the first palette line
        [val,n,err] = sscanf(Line,'%i');
        if n~=8 || ~isempty(err)
            fclose(fid);
            error('Expecting 8 numbers on data lines defining the palette in %s\nLine read: %s',filename,Line)
        end
        %
        % read the actual palette
        fseek(fid,oldPos,-1);
        [map,n] = fscanf(fid,'%i',[8 inf]);
        %
        if n/8 ~= round(n/8)
            error('Error reading %s: expecting to obtain a multiple of 8 integers',filename)
        end
        %
        map = map';
        index = [map(:,1);map(end,5)];
        index = (index-index(1))/(index(end)-index(1));
        map = [map(:,2:4);map(end,6:8)];
        %
        fclose(fid);
        %
        SS.Name=label;
        SS.Space=ColorModel;
        SS.Colors=map/255;
        SS.AlternatingColors=0;
        SS.Index=checkindex(index);
        varargout = {SS};
    case 'write'
        SS = varargin{1};
        SS = convertcolors(SS,{'RGB','HSV'}); % RGB, HSV, and CMYK supported by GMT
        clrs = SS.Colors;
        %
        fid = fopen(filename,'w','n','UTF-8');
        fprintf(fid,'# autogenerated GMT palette "%s"\n',SS.Name);
        fprintf(fid,'# %s\n',md_colormap_revision);
        fprintf(fid,'# COLOR_MODEL = %s\n',SS.Space);
        map = [round(9000*SS.Index(:))-5000 clrs*255];
        map = [map(1:end-1,:) map(2:end,:)];
        fprintf(fid,'%5i %3i %3i %3i %5i %3i %3i %3i\n',map');
        fclose(fid);
end


function varargout = QGISclr(cmd,filename,varargin)
% Description: ??
switch cmd
    case 'read'
        file = xmlread(filename);
        if ~strcmp(char(file.getDocumentElement.getNodeName),'qgis_style')
            error('File doesn''t seem to be XML file of "qgis_style"')
        end
        ramps = file.getElementsByTagName('colorramps');
        if ramps.getLength ~= 1
            error('File %s contains %i color ramps, expecting 1',filename,ramps.getLength)
        end
        ramp = ramps.item(0).getChildNodes.item(0);
        %
        label = char(ramp.getAttribute('name'));
        props = ramp.getElementsByTagName('prop');
        if props.getLength ~= 3
            error('File %s contains %i prop fields for the colorramp object, expecting 3',filename,props.getLength)
        end
        color1 = props.item(0);
        color2 = props.item(1);
        colors = props.item(2);
        %
        color1 = char(color1.getAttribute('v'));
        color1 = sscanf(color1,'%i,%i,%i,%i');
        color2 = char(color2.getAttribute('v'));
        color2 = sscanf(color2,'%i,%i,%i,%i');
        %
        stops = char(colors.getAttribute('k'));
        if ~strcmp(stops,'stops')
            error('Attribute k of third prop field reads "%s" while expecting "stops"',stops) 
        end
        stops = char(colors.getAttribute('v'));
        [map,n] = sscanf(stops,'%f;%i,%i,%i,%i:',[5 inf]);
        map = [0 color1';map';1 color2'];
        %
        SS.Name=label;
        SS.Space='RGB';
        SS.Colors=map(:,2:4)/255;
        SS.AlternatingColors=0;
        SS.Index=checkindex(map(:,1));
        varargout = {SS};
    case 'write'
        SS = varargin{1};
        SS = convertcolors(SS,'RGB'); % only RGB supported
        clrs = SS.Colors*255;
        %
        fid = fopen(filename,'w','n','UTF-8');
        fprintf(fid,'<!DOCTYPE qgis_style>');
        fprintf(fid,'<qgis_style version="1">');
        if 1
            fprintf(fid,'<symbols/>');
            fprintf(fid,'<colorramps>');
            if 1
                fprintf(fid,'<colorramp type="gradient" name="%s">',SS.Name);
                if 1
                    fprintf(fid,'<prop k="color1" v="%i,%i,%i,255"/>',clrs(1,:));
                    fprintf(fid,'<prop k="color2" v="%i,%i,%i,255"/>',clrs(end,:));
                    stopv = [SS.Index(2:end-1)' clrs(2:end-1,:)];
                    stops = sprintf('%.4f;%i,%i,%i,255:',stopv');
                    fprintf(fid,'<prop k="stops" v="%s"/>',stops(1:end-1));
                end
                fprintf(fid,'</colorramp>');
            end
            fprintf(fid,'</colorramps>');
        end
        fprintf(fid,'</qgis_style>');
        fclose(fid);
end


function varargout = CSS3clr(cmd,filename,varargin)
% Description: https://www.w3.org/TR/css3-images/#gradients
switch cmd
    case 'read'
        [p,label] = fileparts(filename);
        %
        fid = fopen(filename,'r','n','UTF-8');
        Line = strtrim(fgetl(fid));
        while 1
            if isempty(Line) || (length(Line)>=2 && strcmp(Line(1:2),'//'))
                Line = strtrim(fgetl(fid));
            elseif length(Line)>=2 && strcmp(Line(1:2),'/*')
                Line = strtrim(fgetl(fid));
                while length(Line)<2 || ~strcmp(Line(1:2),'*/')
                    Line = strtrim(fgetl(fid));
                end
                Line = strtrim(fgetl(fid));
            else
                break
            end
        end
        %
        pos = ftell(fid);
        if length(Line)<15 || ~strcmpi(Line(1:15),'linear-gradient')
            fclose(fid);
            error('First data line of CSS3 file should start with "linear-gradient". Reading "%s"',Line)
        end
        fseek(fid,pos,-1);
        %
        C = cell(1000,1);
        C{1} = Line;
        N = 1;
        while 1
            Line = fgetl(fid);
            if ~ischar(Line)
                break
            else
                N = N+1;
                if N>length(C)
                    C{2*(N-1)} = '';
                end
                C{N} = strtrim(Line);
            end
        end
        fclose(fid);
        C = cat(2,C{1:N});
        ob = find(C=='(');
        cb = find(C==')');
        cob = NaN(size(ob));
        %
        for icb = 1:length(cb)
            iob = max(find(ob<cb(icb) & isnan(cob)));
            cob(iob) = icb;
            if iob==1
                break
            end
        end
        %
        if isnan(cob(1))
            error('No closing bracket found for linear-gradient')
        end
        args = C(ob(1)+1:cb(cob(1))-1);
        nb = cob(1);
        cb = cb(1:nb-1)-ob(1);
        ob = ob(2:nb)-ob(1);
        cob = cob(2:nb);
        cm = find(args==',');
        %
        for i = 1:length(cm)
            for j = 1:length(ob)
                if cm(i)>ob(j) && cm(i)<cb(cob(j))
                    cm(i) = NaN;
                    break
                end
            end
        end
        cm(isnan(cm))=[];
        args = mat2cell(args,1,[cm(1) diff([cm length(args)])])';
        for i = 1:length(args)-1
            args{i} = args{i}(1:end-1);
        end
        %
        %dir = args{1};
        stops = args(2:end);
        map = zeros(length(stops),4);
        for i = 1:length(stops)
            A = sscanf(stops{i},'rgb(%i,%i,%i) %f',4);
            map(i,:) = A;
        end
        %
        SS.Name=label;
        SS.Space='RGB';
        SS.Colors=map(:,1:3)/255;
        SS.AlternatingColors=0;
        SS.Index=checkindex(map(:,4)/100);
        varargout = {SS};
    case 'write'
        SS = varargin{1};
        SS = convertcolors(SS,'RGB'); % only RGB supported
        clrs = SS.Colors*255;
        %
        fid = fopen(filename,'w','n','UTF-8');
        fprintf(fid,'/*\n');
        fprintf(fid,'   CSS3 gradient\n');
        fprintf(fid,'   %s\n',md_colormap_revision);
        fprintf(fid,'*/\n\n');
        fprintf(fid,'linear-gradient(\n');
        fprintf(fid,'  0deg,\n');
        stopv = [clrs 100*SS.Index(:)];
        stops = sprintf('  rgb(%3i,%3i,%3i) %7.3f%%,\n',stopv');
        fprintf(fid,'%s\n',stops(1:end-2)); % remove last comma, add new \n
        fprintf(fid,'  );\n');
        fclose(fid);
end


function varargout = GIMPclr(cmd,filename,varargin)
% Description: https://marc.info/?t=102275194900003&r=1&w=1
switch cmd
    case 'read'
        fid = fopen(filename,'r','n','UTF-8');
        Line = fgetl(fid);
        if ~strcmpi(Line,'GIMP Gradient')
            fclose(fid);
            error('First line of GIMP file should read "GIMP Gradient"')
        end
        Line = fgetl(fid);
        if length(Line)<5 || ~strcmpi(Line(1:5),'Name:')
            fclose(fid);
            error('Second line of GIMP file should start with "Name:"')
        end
        label = strtrim(Line(6:end));
        Line = fgetl(fid);
        N = sscanf(Line,'%i');
        [A,c] = fscanf(fid,'%f %f %f %f %f %f %f %f %f %f %f %i %i',[13 inf]);
        if c ~= N*13
            fclose(fid);
            error('Unable to read %i records from GIMP file (%.2g records read).',N,c/13)
        end
        fclose(fid);
        N = size(A,2);
        map = reshape(A([1 4:6 3 8:10],:),[4 2*N])';
        map(find(all(diff(map)==0,2))+1,:) = [];
        %
        SS.Name=label;
        SS.Space='RGB';
        SS.Colors=map(:,2:4);
        SS.AlternatingColors=0;
        SS.Index=checkindex(map(:,1));
        varargout = {SS};
    case 'write'
        SS = varargin{1};
        SS = convertcolors(SS,'RGB'); % only RGB supported
        clrs = SS.Colors;
        %
        fid = fopen(filename,'w','n','UTF-8');
        fprintf(fid,'GIMP Gradient\n');
        fprintf(fid,'Name: %s\n',SS.Name);
        fprintf(fid,'%i\n',size(clrs,1)-1);
        i1 = SS.Index(1:end-1)';
        i2 = SS.Index(2:end)';
        im = (i1+i2)/3;
        o1 = ones(size(i1));
        stopv = [i1 im i2 clrs(1:end-1,:) o1 clrs(2:end,:) o1];
        stops = sprintf('%8.6f %8.6f %8.6f %8.6f %8.6f %8.6f %8.6f %8.6f %8.6f %8.6f %8.6f 0 0\n',stopv');
        %stops = strrep(stops,'.',','); % period or colon depending on locale for GIMP 1.3
        fprintf(fid,'%s',stops);
        fclose(fid);
end


function varargout = gnuplot_clr(cmd,filename,varargin)
% Description: http://gnuplot.sourceforge.net/docs_5.0/gnuplot.pdf#section*.296
switch cmd
    case 'read'
        [p,label] = fileparts(filename);
        %
        fid = fopen(filename,'r','n','UTF-8');
        C = textscan(fid,'%f %f %f %f','commentstyle','#');
        fclose(fid);
        %
        if isempty(C{1})
            error('Data format in file does not match 3 or 4 floats per line as expected for GNU Plot file.')
        elseif all(isnan(C{4}))
            map = [C{1} C{2} C{3}];
            N = size(map,1);
            f = (0:(N-1))/(N-1);
        else
            map = [C{2} C{3} C{4}];
            N = size(map,1);
            f = C{1};
        end
        %
        checkmap(map,1);
        %
        SS.Name=label;
        SS.Space='RGB';
        SS.Colors=map;
        SS.AlternatingColors=0;
        SS.Index=checkindex(f);
        varargout = {SS};
    case 'write'
        SS = varargin{1};
        SS = convertcolors(SS,'RGB'); % only RGB supported
        clrs = SS.Colors;
        %
        fid = fopen(filename,'w','n','UTF-8');
        fprintf(fid,'# Gnuplot colour map\n');
        fprintf(fid,'# %s\n',md_colormap_revision);
        stopv = [SS.Index(:) clrs];
        fprintf(fid,'%7.5f %7.5f %7.5f %7.5f\n',stopv');
        fclose(fid);
end


function varargout = POVRAYclr(cmd,filename,varargin)
switch cmd
    case 'read'
        fid = fopen(filename,'r','n','UTF-8');
        Line = strtrim(fgetl(fid));
        while isempty(Line) || (length(Line)>=2 && strcmp(Line(1:2),'//'))
            Line = strtrim(fgetl(fid));
        end
        %
        if length(Line)<8 || ~strcmpi(Line(1:8),'#declare')
            fclose(fid);
            error('First data line of POV-Ray file should start with "#declare"')
        end
        [dec,Line] = strtok(Line);
        label = strtok(Line); % label is second argument on this line
        %
        Line = strtrim(fgetl(fid));
        if length(Line)<9 || ~strcmpi(Line(1:9),'color_map')
            fclose(fid);
            error('Second data line of POV-Ray file should start with "color_map"')
        end
        if Line(end)~='{'
            fclose(fid);
            error('Second data line of POV-Ray file should end with "{"')
        end
        %
        C = textscan(fid,'[%f color rgbf <%f,%f,%f,%f>]');
        Line = strtrim(fgetl(fid));
        if ~strcmp(Line,'}')
            fclose(fid);
            error('The color_map record of POV-Ray file should end with "}"')
        end
        fclose(fid);
        %
        map = [C{2} C{3} C{4}];
        f = C{1};
        checkmap(map,1);
        %
        SS.Name=label;
        SS.Space='RGB';
        SS.Colors=map;
        SS.AlternatingColors=0;
        SS.Index=checkindex(f);
        varargout = {SS};
    case 'write'
        SS = varargin{1};
        SS = convertcolors(SS,'RGB'); % only RGB supported
        clrs = SS.Colors;
        %
        fid = fopen(filename,'w','n','UTF-8');
        fprintf(fid,'// autogenerated povray color map "%s"\n',SS.Name);
        fprintf(fid,'// %s\n',md_colormap_revision);
        fprintf(fid,'#declare %s =\n',SS.Name);
        fprintf(fid,'color_map {\n');
        stopv = [SS.Index(:) clrs clrs(:,1)]; stopv(:,end)=0;
        fprintf(fid,'  [%7.5f color rgbf <%6.4f,%6.4f,%6.4f,%6.4f>]\n',stopv');
        fprintf(fid,'}\n');
        fclose(fid);
end


function varargout = PGclr(cmd,filename,varargin)
switch cmd
    case 'read'
        [p,label] = fileparts(filename);
        %
        fid = fopen(filename,'r','n','UTF-8');
        C = textscan(fid,'%d %d %d %d %d','emptyvalue',-999);
        fclose(fid);
        %
        if isempty(C{1})
            error('Data format in file does not match 5 integers per line as expected for PostGIS file.')
        else
            map = double([C{1} C{2} C{3} C{4} C{5}]);
            if any(any(map(:,2:5)==-999))
                error('Data format in file does not match 5 integers per line as expected for PostGIS file.')
            end
            map = flipud(map);
            f = (map(:,1)-map(1,1))/(map(end,1)-map(1,1));
            map = map(:,2:4);
        end
        %
        checkmap(map,255);
        %
        SS.Name=label;
        SS.Space='RGB';
        SS.Colors=map/255;
        SS.AlternatingColors=0;
        SS.Index=checkindex(f);
        varargout = {SS};
    case 'write'
        SS = varargin{1};
        SS = convertcolors(SS,'RGB'); % only RGB supported
        clrs = SS.Colors;
        %
        fid = fopen(filename,'w','n','UTF-8');
        map = flipud([round(9000*SS.Index(:))-5000 clrs*255]);
        fprintf(fid,'%-7i %3i %3i %3i\n',map');
        fclose(fid);
end


function varargout = SAO_DS9clr(cmd,filename,varargin)
switch cmd
    case 'read'
        [p,label] = fileparts(filename);
        %
        fid = fopen(filename,'r','n','UTF-8');
        Line = strtrim(fgetl(fid));
        while isempty(Line) || strcmp(Line(1),'#')
            Line = strtrim(fgetl(fid));
        end
        %
        if ~strcmpi(Line,'PSEUDOCOLOR')
            fclose(fid);
            error('First data line of SAO DS9 file expected to read "PSEUDOCOLOR"')
        end
        %
        col = {'RED','GREEN','BLUE'};
        fst = {'first','second','third'};
        cl = cell(2,3);
        for i = 1:3
            COL = [col{i} ':'];
            Line = strtrim(fgetl(fid));
            if ~strcmpi(Line,COL)
                fclose(fid);
                error('Trying to read %s data table. Expecting "%s", read: "%s"',fst{i},COL,Line)
            end
            v = fscanf(fid,' (%f,%f)');
            v = reshape(v,[2 length(v)/2])';
            cl{1,i} = checkindex(v(:,1));
            cl{2,i} = v(:,2);
        end
        fclose(fid);
        f = unique(cat(1,cl{1,:}));
        map = zeros(length(f),3);
        for i = 1:3
            map(:,i) = interp1(cl{1,i},cl{2,i},f);
        end
        %
        SS.Name=label;
        SS.Space='RGB';
        SS.Colors=map;
        SS.AlternatingColors=0;
        SS.Index=f;
        varargout = {SS};
    case 'write'
        SS = varargin{1};
        SS = convertcolors(SS,'RGB'); % only RGB supported
        clrs = SS.Colors;
        %
        fid = fopen(filename,'w','n','UTF-8');
        fprintf(fid,'# SAOimage color table\n');
        fprintf(fid,'# %s\n',SS.Name);
        fprintf(fid,'# created by %s\n',md_colormap_revision);
        fprintf(fid,'PSEUDOCOLOR\n');
        fprintf(fid,'RED:\n');
        stopv = [SS.Index(:) clrs];
        stops = sprintf('(%.4g,%.4g)',stopv(:,[1 2])');
        fprintf(fid,'%s\n',stops);
        fprintf(fid,'GREEN:\n');
        stops = sprintf('(%.4g,%.4g)',stopv(:,[1 3])');
        fprintf(fid,'%s\n',stops);
        fprintf(fid,'BLUE:\n');
        stops = sprintf('(%.4g,%.4g)',stopv(:,[1 4])');
        fprintf(fid,'%s\n',stops);
        fclose(fid);
end


function varargout = SVGclr(cmd,filename,varargin)
switch cmd
    case 'read'
        file = xmlread(filename);
        if ~strcmp(char(file.getDocumentElement.getNodeName),'svg')
            error('File doesn''t seem to be SVG file')
        end
        ramps = file.getElementsByTagName('linearGradient');
        if ramps.getLength ~= 1
            error('File %s contains %i linear gradients, expecting 1',filename,ramps.getLength)
        end
        ramp = ramps.item(0);
        %
        label = char(ramp.getAttribute('id'));
        stops = ramp.getElementsByTagName('stop');
        N = stops.getLength;
        if N == 0
            error('File %s does not contain any colour stops',filename)
        end
        f = zeros(N,1);
        map = zeros(N,3);
        %
        for i = 1:N
            stop = stops.item(i-1);
            f(i) = sscanf(char(stop.getAttribute('offset')),'%f');
            map(i,:) = sscanf(char(stop.getAttribute('stop-color')),'rgb(%d,%d,%d',[1 3]);
            % stop-opacity
        end
        checkmap(map,255)
        %
        SS.Name=label;
        SS.Space='RGB';
        SS.Colors=map/255;
        SS.AlternatingColors=0;
        SS.Index=checkindex(f/100);
        varargout = {SS};
    case 'write'
        SS = varargin{1};
        SS = convertcolors(SS,'RGB'); % only RGB supported
        clrs = SS.Colors*255;
        %
        mdcrev = md_colormap_revision;
        [mdc,rev] = strtok(mdcrev);
        rev = strtrim(rev);
        %
        fid = fopen(filename,'w','n','UTF-8');
        fprintf(fid,'<?xml version="1.0" encoding="UTF-8"?>\n');
        fprintf(fid,'<svg xmlns="http://www.w3.org/2000/svg" version="1.1" width="300px" height="45px" viewBox="0 0 300 45">\n');
        fprintf(fid,'  <g>\n');
        fprintf(fid,'    <defs>\n');
        fprintf(fid,'      <linearGradient id="%s" gradientUnits="objectBoundingBox" spreadMethod="pad" x1="0%%" x2="100%%" y1="0%%" y2="0%%">\n',SS.Name);
        stopv = [100*SS.Index(:) clrs clrs(:,1)]; stopv(:,end)=1;
        fprintf(fid,'        <stop offset="%.2f%%" stop-color="rgb(%i, %i, %i)" stop-opacity="%.4f"/>\n',stopv');
        fprintf(fid,'      </linearGradient>\n');
        fprintf(fid,'    </defs>\n');
        fprintf(fid,'    <rect fill="url(#%s)" x="4" y="4" width="292" height="37" stroke="black" stroke-width="1"/>\n',SS.Name);
        fprintf(fid,'  </g>\n');
        fprintf(fid,'  <metadata>\n');
        fprintf(fid,'    <creator name="%s" version="%s"/>\n',mdc,rev);
        fprintf(fid,'    <created date="%s"/>\n',datestr(now));
        fprintf(fid,'  </metadata>\n');
        fprintf(fid,'</svg>\n');
        fclose(fid);
end


function checkmap(map,maxv)
for i = 1:3
    switch i
        case 1
            j = isnan(map(:));
        case 2
            j = map(:)<0;
        case 3
            j = map(:)>maxv;
    end
    if any(j)
        iw = find(j);
        iw = iw(1);
        iRGB = floor(iw/N);
        iCOL = iw - iRGB*N;
        error('Invalid color value %i read (red=%f, green=%f, blue=%f).\n Expecting values in range 0 to %i.',iCOL,map(iCOL,:),maxv)
    end
end



function index = checkindex(index)
for i = 2:length(index)
    index(i) = max(index(i-1)+eps,index(i));
end
for i = length(index)-1:-1:1
    index(i) = min(index(i+1)-eps,index(i));
end


function rev = md_colormap_revision
url = '$HeadURL$';
rev = '$Id$';
rev = [url(11:end-2) ' ' sscanf(rev,'%*[^ ] %*[^ ] %[^ ]%[ ]%[^ ]%[ ]%[^ ] ')];