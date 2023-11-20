function Out = md_paper(cmd,varargin)
%MD_PAPER Add border to plot.
%   MD_PAPER(FIG,PAPERTYPE,BORDERTYPE) sets the paper type for the
%   figure FIG and adds an empty border to it. Right click on the border to
%   fill in the texts via a dialog. PAPERTYPE should equal a paper type
%   supported by MATLAB followed by 'p' (portrait) or 'l' (landscape), e.g.
%   'a4p'. BORDERTYPE should equal one of the following strings
%      'none'       no border (default) just set paper size
%      '1box'       Framed figure with just one 2cm high text box and 1 cm
%                   margins at all sides.
%      '2box'       Non-framed figure with two textboxes with 3 cm margin
%                   at left side and 1 cm margins at the other sides.
%      '7box'       Framed figure with seven textboxes (classic Delft
%                   Hydraulics layout) with 1 cm margins at all sides.
%   or an explicit border structure BSTRUCT may be specified.
%                                                         |             |
%                   |             |                       |    .7box.   |
%                   |    .1box.   |         .2box.        |_________ _ _|
%                   |_____________|     ___ _________     |         |_|_|
%                   |             |    |   |         |    |_________|___|
%                   |_____________|    |___|_________|    |_________|_|_|
%
%   If no figure FIG is specified, the settings will be applied to the
%   current figure.
%
%   MD_PAPER(FIG,PAPERTYPE,BORDERTYPE,CTEXTS) does the same thing, but also
%   fills in the texts by using entries from the CTEXTS cell array of
%   strings. Right click on the border or texts to add or edit texts.
%
%   MD_PAPER(...,'OptionName1',OptionValue1,'OptionName2',OptionValue2,...)
%   uses non-default values for the specified options.
%
%   Supported options (also valid fields for border structure BSTRUCT):
%      'Margin'     [left bottom right top] margin
%      'Border'     draw border 1=yes/0=no
%      'LineWidth'  line width of borders and boxes
%      'Color'      line and text color
%
%   Additional fields for border structure BSTRUCT:
%      'Box'        matrix containing indices of textboxes
%      'HTabs'      relative widths of boxes
%                   length of the vector should match size(Box,2)
%      'HRange'     (maximum) width of all boxes together
%      'VTabs'      relative heights of boxes
%                   length of the vector should match size(Box,1)
%      'VRange'     (maximum) height of all boxes together
%      'Bold'       flags for printing text in bold font
%                   length of the vector should match the number
%                   of textboxes
%      'BorderText' cell array containing default texts; the length of the
%                   vector should match the number of textboxes, or
%                   alternatively one may specify fields 'BorderText1',
%                   'BorderText2', etc.
%
%   hBorder = MD_PAPER('no edit',...) right click editing disabled. Use
%   MD_PAPER('edit',hBorder) to edit the texts via a dialog.
%
%   BSTRUCT = MD_PAPER(FIG,'getprops') returns the BSTRUCT of the border of
%   the figure FIG.
%
%   NOTE: There are some compatibility problems with the LEGEND function.
%   When the border is added, all subplots are made slightly smaller. The
%   LEGEND function detects this and resets the subplots to their original
%   size. As a workaround use the following approach:
%
%         AX1=subplot(2,1,1);
%         x=0:.1:10; plot(x,sin(x));
%         md_paper('a4p','wl');
%         legend(AX1,'sine');
%
%
%   Backward compatibility:
%
%   MD_PAPER(ORIENT) where ORIENT can be either 'portrait' or 'landscape'
%   adds "Deltares (date and time)" to a figure and sets the page size to
%   A4 portrait/landscape.
%
%   MD_PAPER(ORIENT,'String') where ORIENT can be either 'portrait' or
%   'landscape' adds "String (date and time)" to a figure and sets the
%   page size to A4 portrait/landscape.
%
%   MD_PAPER(ORIENT,'String1','String2',...) where ORIENT can be either
%   'portrait' or 'landscape' adds the 7 box border to the figure and sets
%   the page size to A4 portrait/landscape. Right click on the text to edit
%   the texts.

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


if nargin==0
    cmd='portrait';
end

INP=varargin;

NoEdit=0;
if length(cmd)==1 && ishandle(cmd)
    fg = cmd;
    cmd = INP{1};
    INP = INP(2:end);
else
    fg = []; % will trigger call to gcf later ...
end
if strcmpi(cmd,'no edit')
    NoEdit=1;
    cmd=INP{1};
    INP=INP(2:end);
end

if ischar(cmd)
    lcmd = lower(cmd);
else
    INP  = [{cmd} INP];
    lcmd = '';
end
switch lcmd
    case {'apply','done'}
        Fig=gcbf;
        gcba=get(Fig,'userdata');
        if ~ishandle(gcba)
            delete(gcbf);
            return
        end
        BFormat = get(gcba,'userdata');
        ExpandP = getappdata(get(gcba,'parent'),'ExpandPAR');
        for i = 1:max(BFormat.Box(:))
            hedittext=findobj(Fig,'tag',sprintf('Text%i',i));
            hplottext=findobj(gcba,'tag',sprintf('plottext%i',i));
            str = get(hedittext,'string');
            BFormat.(sprintf('BorderText%i',i)) = serialize(str);
            if isempty(str)
                str = {' '};
            end
            if ~isempty(ExpandP)
                str = qp_strrep(str,ExpandP.PAR);
            end
            set(hplottext,'string',str)
        end
        leftpage=findobj(Fig,'tag','LeftPage');
        switch get(get(gcba,'parent'),'paperorientation')
            case 'portrait'
                if get(leftpage,'value') %=1
                    set(gcba,'xdir','reverse');
                else %=0
                    set(gcba,'xdir','normal');
                end
            otherwise
                if get(leftpage,'value') %=1
                    set(gcba,'ydir','reverse');
                else %=0
                    set(gcba,'ydir','normal');
                end
        end
        set(gcba,'userdata',BFormat)
        if strcmp(lcmd,'done')
            delete(gcbf);
        end
        
    case {'edit','editmodal'}
        if (nargin==1) && (isempty(gcbf) || ~strcmp(get(gcbf,'selectiontype'),'alt'))
            return
        elseif nargin==2
            gcba=INP{1};
        else
            gcba=get(gcbo,'parent');
        end
        if strcmp(get(gcba,'type'),'figure')
            gcba=findobj(gcba,'type','axes','tag','border');
        end
        %first check whether it already exists ...
        Fig=findobj(allchild(0),'type','figure','tag','MD_PAPER Border Editor');
        if ~isempty(Fig)
            for f=transpose(Fig)
                if isequal(get(f,'userdata'),gcba)
                    set(f,'visible','on');
                    return
                end
            end
        end
        Fig=Local_ui_paper(gcba);
        if strcmpi(lcmd,'editmodal')
            set(Fig,'windowstyle','modal')
        end
        
        fig=get(gcba,'parent');
        if isnumeric(fig)
            HandleStr=[' ' num2str(fig)];
        elseif ~isempty(get(fig,'Number'))
            HandleStr=[' ' num2str(get(fig,'Number'))];
        else
            HandleStr='';
        end
        StringStr=get(fig,'name');
        if strcmp(get(fig,'numbertitle'),'on') && ~isempty(HandleStr)
            if isempty(StringStr)
                StringStr=['Figure No.' HandleStr];
            else
                StringStr=['Figure No.' HandleStr ':' StringStr];
            end
        end
        set(Fig,'name',[get(Fig,'name') ' for ' StringStr]);
        
        BFormat = get(gcba,'userdata');
        for i = 1:max(BFormat.Box(:))
            hedittext=findobj(Fig,'tag',sprintf('Text%i',i));
            set(hedittext,'string',deserialize(BFormat.(sprintf('BorderText%i',i))))
        end
        set(Fig,'userdata',gcba);
        leftpage=findobj(Fig,'tag','LeftPage');
        switch get(get(gcba,'parent'),'paperorientation')
            case 'portrait'
                set(leftpage,'value',strcmp(get(gcba,'xdir'),'reverse'));
            otherwise
                set(leftpage,'value',strcmp(get(gcba,'ydir'),'reverse'));
        end
        if strcmpi(lcmd,'editmodal')
            waitfor(Fig)
        end
        
    case 'remove'
        if isempty(fg)
            fg = gcf;
        end
        switch get(fg,'type')
            case 'figure'
                hOldBorder = findobj(fg,'type','axes','tag','border');
            case 'axes'
                hOldBorder = fg;
        end
        if ~isempty(hOldBorder)
            fg = get(hOldBorder,'parent');
            delete(hOldBorder)
            MPE = getappdata(fg,'MaximumPlotExtent');
            remapaxes(fg,MPE,[0 0 1 1])
            rmappdata(fg,'MaximumPlotExtent');
        end
        
    case {'bordertypes','borderstyles'}
        Out = {'none','1box','2box','7box','<custom>'};
        
    case {'getprops','setprops'}
        if isempty(fg)
            fg = gcf;
        end
        switch get(fg,'type')
            case 'figure'
                hBorder = findobj(fg,'type','axes','tag','border');
            case 'axes'
                hBorder = fg;
        end
        if length(hBorder)>1
            error('Function doesn''t support multiple borders.')
        elseif isempty(hBorder)
            Out = [];
            return
        end
        iBrdr = get(hBorder,'userdata');
        ExpandP = getappdata(get(hBorder,'parent'),'ExpandPAR');
        if strcmp(lcmd,'setprops')
            i = 0;
            while 1
                i = i+1;
                hplottext = findall(hBorder,'tag',sprintf('plottext%i',i));
                if isempty(hplottext) || ...
                        i>length(INP) || (~ischar(INP{i}) && ~iscellstr(INP{i}))
                    break
                end
                str  = INP{i};
                if iscell(str)
                    celstr = str;
                    if isempty(str)
                        str = '';
                    else
                        str = str(:)';
                        str(2,:)={'\n{}'};
                        str(2,end)={''};
                        str = strcat(str{:});
                    end
                else
                    celstr = strrep(str,'\n{}',char(13));
                    celstr = splitcellstr(celstr,char(13));
                end
                if ~isempty(ExpandP)
                    celstr = qp_strrep(celstr,ExpandP.PAR);
                end
                iBrdr.(sprintf('BorderText%i',i)) = str;
                set(hplottext,'string',celstr);
            end
            set(hBorder,'userdata',iBrdr)
        else
            Out = iBrdr;
        end
        
    otherwise
        
        Orientation=lcmd;
        if (strcmp(Orientation,'portrait') || strcmp(Orientation,'landscape')) && length(INP)<2
            % MD_PAPER(H,ORIENT)
            % MD_PAPER(H,ORIENT,STRING)
            if isempty(fg)
                fg = gcf;
            end
            set(fg,'PaperType','a4','PaperOrientation',Orientation);
            hBorder=SimpleBorder(fg,INP{:});
        else
            % MD_PAPER(H,ORIENT,STRING1,STRING2,...)
            % MD_PAPER(H,TYPEORIENT) - no border, just page shape
            % MD_PAPER(H,TYPEORIENT,BORDERTYPE,...)
            % MD_PAPER(H,BORDERTYPE,...)
            BorderTypes = md_paper('bordertypes');
            if strcmp(Orientation,'portrait') || strcmp(Orientation,'landscape')
                Orientation=['a4' Orientation(1)];
                BFormat='7box';
                INP={INP};
            elseif any(strcmp(Orientation,BorderTypes))
                BFormat=Orientation;
                Orientation='';
            else
                if isempty(INP)
                    BFormat='none';
                else
                    BFormat=INP{1};
                    INP(1)=[];
                end
            end
            %
            if isempty(Orientation)
                fgOptions = {};
            else
                switch lower(Orientation(end))
                    case 'p'
                        fgOptions = {'PaperType',Orientation(1:end-1),'PaperOrientation','portrait'};
                    case 'l'
                        fgOptions = {'PaperType',Orientation(1:end-1),'PaperOrientation','landscape'};
                    otherwise
                        error('Unrecognized paper type/orientation');
                end
            end
            [hBorder,fg]=Local_createborder(fg,NoEdit,fgOptions,BFormat,INP{:});
        end
        if ~isempty(Orientation)
            AdjustFigPos(fg)
        end
        if nargout>0
            Out = hBorder;
        end
end


function organization=getorg
try
    organization = qp_settings('organizationname');
catch
    organization = 'Deltares';
end


function hBorder=SimpleBorder(fg,varargin)
if ~isempty(varargin)
    if isempty(varargin{1})
        PlotText=locDateStr;
    else
        PlotText=[varargin{1},' (',locDateStr,')'];
    end
else
    PlotText=[getorg ' (',locDateStr,')'];
end
%
[ax,allchld,xmax,ymax,hBorder]=CreateBorderAxes(fg);
%
switch get(fg,'PaperOrientation')
    case 'portrait'
        text(0.98*xmax,0.02*ymax, ...
            PlotText, ...
            'parent',hBorder', ...
            'fontsize',5, ...
            'horizontalalignment','right', ...
            'verticalalignment','bottom');
    case {'landscape','rotated'}
        text(0.02*xmax,0.02*ymax, ...
            PlotText, ...
            'parent',hBorder', ...
            'fontsize',5, ...
            'rotation',270, ...
            'horizontalalignment','right', ...
            'verticalalignment','bottom');
end
%
set(fg,'CurrentAxes',ax);
set(fg,'children',[allchld;hBorder]);


function [hBorder,fg]=Local_createborder(fg,NoEdit,fgOptions,BFormat,varargin)
if nargin<4
    Strings={};
    INP={};
elseif ~isempty(varargin) && iscell(varargin{1})
    Strings=varargin{1};
    INP=varargin(2:end);
else
    Strings={};
    INP=varargin;
end
%
% Set/unset buttondownfunction
%
if NoEdit
    BDFunction='';
else
    BDFunction='md_paper edit';
end
%
% Set up border defaults ...
%
Border=1;
Margin=[1 1 1 1];
LineWidth=1;
Color='k';
HTabs=1; HRange=inf;
VTabs=[0.5 0.5]; VRange=2;
Box=[1;1];
Bold=0;
PlotText={};
BName='<custom>';
if ischar(BFormat)
    BName=BFormat;
elseif isstruct(BFormat)
    %
    % Border information from structure ...
    %
    if isfield(BFormat,'BName')
        BName=BFormat.BName;
    end
end
%
switch lower(BName)
    %
    % predefined border type
    %
    case {'1box',''}
        BName = '1box';
        PlotText={' '};
    case 'none'
        BName = 'none';
        Border=0;
        Margin=[0 0 0 0];
        LineWidth=1.5;
        Color='k';
        HTabs=[]; HRange=0;
        VTabs=[]; VRange=0;
        Box=[];
        Bold=[];
        PlotText={};
    case {'7box','wl'}
        BName = '7box';
        Border=1;
        Margin=[1 1 1 1];
        LineWidth=1.5;
        Color='k';
        HTabs=[0.68 0.16 0.16]; HRange=19;
        VTabs=[1 1 1]/3;        VRange=2.7;
        Box=[1 2 3
            1 4 4
            7 5 6];
        Bold=[0 0 0 0 0 0 1];
        PlotText={' '  ' '  ' '  ' '  ' '  ' '  getorg};
    case {'2box','spankracht'}
        BName = '2box';
        Border=0;
        Margin=[3 1 1 1];
        LineWidth=0.5;
        Color='k';
        HTabs=[0.32 0.68]; HRange=inf;
        VTabs=[0.5 0.5];   VRange=1.4;
        Box=[1 2
            1 2];
        Bold=[0 0];
        PlotText={' '  ' '};
    case {'<custom>'}
        %
        % custom border
        %
        if isfield(BFormat,'Name')
            BName=BFormat.Name;
        end
        if isfield(BFormat,'Border')
            Border=BFormat.Border;
        end
        if isfield(BFormat,'Margin')
            Margin=BFormat.Margin;
        end
        if isfield(BFormat,'LineWidth')
            LineWidth=BFormat.LineWidth;
        end
        if isfield(BFormat,'Color')
            Color=BFormat.Color;
        end
        if isfield(BFormat,'HTabs')
            HTabs=BFormat.HTabs;
        end
        if isfield(BFormat,'HRange')
            HRange=BFormat.HRange;
        end
        if isfield(BFormat,'VTabs')
            VTabs=BFormat.VTabs;
        end
        if isfield(BFormat,'VRange')
            VRange=BFormat.VRange;
        end
        if isfield(BFormat,'Box')
            Box=BFormat.Box;
            Bold=zeros(1,max(Box(:)));
            PlotText(1,1:max(Box(:)))={''};
        end
        if isfield(BFormat,'Bold')
            Bold=BFormat.Bold;
        end
    otherwise
        error('Unsupported border format: %s',BFormat)
end
%------------------------------------------------
if isstruct(BFormat)
    if isfield(BFormat,'PlotText')
        PlotText=BFormat.PlotText;
    end
    if isfield(BFormat,'BorderText')
        PlotText=BFormat.BorderText;
    end
    for i = 1:length(PlotText)
        BText = sprintf('BorderText%i',i);
        if isfield(BFormat,BText)
            PlotText{i} = BFormat.(BText);
        end
    end
end
%------------------------------------------------
ncol = length(HTabs(:));
nrow = length(VTabs(:));
if ~isequal(size(Box),[nrow ncol])
    if numel(Box)==nrow*ncol
        Box = reshape(Box,[nrow ncol]);
    else
        error('Inconsistent frame type')
    end
end
if length(Bold(:))~=max(Box(:))
    error('Length of bold vector does not match number of texts')
end
if ~iscell(PlotText)
    error('PlotText should be a cell array')
elseif length(PlotText(:))~=max(Box(:))
    error('Length of plot text vector does not match number of texts')
end
%
% Process options
%
i=1;
while i<=length(INP)
    if ~ischar(INP{i})
        error('Invalid option.')
    end
    switch lower(INP{i})
        case 'margin'
            i=i+1;
            if isequal(size(INP{i}),[1 1])
                Margin=[INP{i} INP{i} INP{i} INP{i}];
            else
                Margin=INP{i};
            end
        case 'linewidth'
            i=i+1;
            LineWidth=INP{i};
        case 'color'
            i=i+1;
            Color=INP{i};
        case 'border'
            i=i+1;
            if ischar(INP{i})
                Border=isempty(strmatch(INP{i},{'off','no'}));
            else
                Border=INP{i};
            end
        otherwise
            error('Invalid option: %s',INP{i})
    end
    i=i+1;
end
%
% Store fields in records
%
BFormat=[];
BFormat.Name=BName;
BFormat.Border=Border;
BFormat.Margin=Margin;
BFormat.LineWidth=LineWidth;
BFormat.Color=Color;
BFormat.HTabs=HTabs;
BFormat.HRange=HRange;
BFormat.VTabs=VTabs;
BFormat.VRange=VRange;
BFormat.Box=Box;
BFormat.Bold=Bold;
%
if isempty(fg)
    fg = gcf;
end
if ~isempty(fgOptions)
    set(fg,fgOptions{:})
end
%
for i=1:length(PlotText)
    if i<=length(Strings)
        PlotText{i}=Strings{i};
    end
    if ischar(PlotText{i})
        PlotText{i}=PlotText(i);
    end
    BFormat.(sprintf('BorderText%i',i)) = '';
end
%
[ax,allchld,xmax,ymax,hBorder]=CreateBorderAxes(fg);
set(hBorder,'userdata',BFormat)
%
Box=fliplr(Box');
switch get(fg,'PaperOrientation')
    case 'portrait'
        %
        % Draw border ...
        %
        L=line([Margin(1) xmax-Margin(3) xmax-Margin(3) Margin(1) Margin(1)], ...
            [Margin(2) Margin(2) ymax-Margin(4) ymax-Margin(4) Margin(2)], ...
            'parent',hBorder,'color',Color,'linewidth',LineWidth, ...
            'tag','border','visible','off');
        if Border
            set(L,'visible','on')
        end
        %
        % Plot boxes and texts ...
        %
        HRange=min(HRange,xmax-Margin(1)-Margin(3));
        VRange=min(VRange,ymax-Margin(2)-Margin(4));
        if isempty(HRange)
            HRange=0;
        end
        if isempty(VRange)
            VRange=0;
        end
        HTabs=cumsum([0 HTabs])*HRange+Margin(1);
        VTabs=cumsum([0 VTabs])*VRange+Margin(2);
        Boxs=unique(Box(:))';
        for b=Boxs
            [m,n]=find(Box==b);
            m1=min(m); m2=max(m); n1=min(n); n2=max(n);
            if b~=0
                line([HTabs(m1) HTabs(m2+1) HTabs(m2+1) HTabs(m1)   HTabs(m1)], ...
                    [VTabs(n1) VTabs(n1)   VTabs(n2+1) VTabs(n2+1) VTabs(n1)], ...
                    'parent',hBorder, ...
                    'color',Color, ...
                    'linewidth',LineWidth, ...
                    'buttondownfcn',BDFunction);
            end
            if b>0
                T=text((HTabs(m1)+HTabs(m2+1))/2, ...
                    (VTabs(n1)+VTabs(n2+1))/2, ...
                    '', ...
                    'parent',hBorder, ...
                    'horizontalalignment','center', ...
                    'verticalalignment','middle', ...
                    'fontname','helvetica', ...
                    'tag',sprintf('plottext%i',b), ...
                    'buttondownfcn',BDFunction, ...
                    'color',Color);
                if Bold(b)
                    set(T,'fontweight','bold');
                end
            elseif b<0
                pos=[HTabs(m1) VTabs(n1) HTabs(m2+1)-HTabs(m1) VTabs(n2+1)-VTabs(n1)];
                pos(1:2)=pos(1:2)+0.05*pos(3:4);
                pos(3:4)=0.9*pos(3:4);
                switch b
                    case -1
                        xx_logo('wl',hBorder,pos,LineWidth,Color,[])
                    case -2
                        xx_logo('ut',hBorder,pos,LineWidth,'none',Color)
                    case -3
                        xx_logo('deltares',hBorder,pos,LineWidth,'none',Color)
                end
            end
        end
        %
        % Shift axes ...
        %
        plotbox=[Margin(1)+0.1 Margin(2)+VRange+0.1 ...
            xmax-Margin(1)-Margin(3)-0.2 ymax-VRange-Margin(2)-Margin(4)-0.2];
        plotbox=plotbox./[xmax ymax xmax ymax];
    otherwise
        %
        % Draw border ...
        %
        L=line([Margin(2) xmax-Margin(4) xmax-Margin(4) Margin(2) Margin(2)], ...
            [Margin(3) Margin(3) ymax-Margin(1) ymax-Margin(1) Margin(3)], ...
            'parent',hBorder,'color',Color,'linewidth',LineWidth, ...
            'tag','border','visible','off');
        if Border
            set(L,'visible','on');
        end
        %
        % Plot boxes and texts ...
        %
        HRange=min(HRange,ymax-Margin(1)-Margin(3));
        VRange=min(VRange,xmax-Margin(2)-Margin(4));
        if isempty(HRange)
            HRange=0;
        end
        if isempty(VRange)
            VRange=0;
        end
        HTabs=cumsum([0 HTabs])*HRange+Margin(1);
        VTabs=cumsum([0 VTabs])*VRange+Margin(2);
        Boxs=unique(Box(:))';
        for b=Boxs
            [m,n]=find(Box==b);
            m1=min(m); m2=max(m); n1=min(n); n2=max(n);
            if b~=0
                line([VTabs(n1) VTabs(n1)   VTabs(n2+1) VTabs(n2+1) VTabs(n1)], ...
                    ymax-[HTabs(m1) HTabs(m2+1) HTabs(m2+1) HTabs(m1)   HTabs(m1)], ...
                    'parent',hBorder, ...
                    'color',Color, ...
                    'linewidth',LineWidth, ...
                    'buttondownfcn',BDFunction);
            end
            if b>0
                T=text((VTabs(n1)+VTabs(n2+1))/2, ...
                    ymax-(HTabs(m1)+HTabs(m2+1))/2, ...
                    '', ...
                    'parent',hBorder, ...
                    'horizontalalignment','center', ...
                    'verticalalignment','middle', ...
                    'fontname','helvetica', ...
                    'tag',sprintf('plottext%i',b), ...
                    'buttondownfcn',BDFunction, ...
                    'color',Color, ...
                    'rotation',270);
                if Bold(b)
                    set(T,'fontweight','bold');
                end
            elseif b<0
                pos=[VTabs(n1) ymax-HTabs(m2+1) VTabs(n2+1)-VTabs(n1) HTabs(m2+1)-HTabs(m1)];
                pos(1:2)=pos(1:2)+0.05*pos(3:4);
                pos(3:4)=0.9*pos(3:4);
                pos(5)=270*pi/180;
                switch b
                    case -1
                        xx_logo('wl',hBorder,pos,LineWidth,Color,[])
                    case -2
                        xx_logo('ut',hBorder,pos,LineWidth,'none',Color)
                    case -3
                        xx_logo('deltares',hBorder,pos,LineWidth,'none',Color)
                end
            end
        end
        %
        % Shift axes ...
        %
        plotbox=[Margin(2)+VRange+0.1 Margin(3)+0.1 ...
            xmax-VRange-Margin(2)-Margin(4)-0.2 ymax-Margin(1)-Margin(3)-0.2];
        plotbox=plotbox./[xmax ymax xmax ymax];
end
if all(plotbox>0)
    oldplotbox = getappdata(fg,'MaximumPlotExtent');
    if isempty(oldplotbox)
        oldplotbox = [0 0 1 1];
    end
    if ~isequal(plotbox,oldplotbox)
        remapaxes(fg,oldplotbox,plotbox)
    end
    setappdata(fg,'MaximumPlotExtent',plotbox)
end
set(fg,'CurrentAxes',ax);
set(fg,'children',[allchld;hBorder]);
md_paper('setprops',PlotText{:})


function remapaxes(fg,from,to)
allax = findall(fg,'type','axes');
allax = setdiff(allax,findobj(fg,'type','axes','tag','border'));
for i=1:length(allax)
    % skip Colorbar and legend, they will move automatically
    if ~isappdata(allax(i),'NonDataObject')
        axu = get(allax(i),'units');
        set(allax(i),'units','normalized');
        pos_i=get(allax(i),'position');
        n_pos_i(1)=to(1)+(pos_i(1)-from(1))*to(3)/from(3);
        n_pos_i(2)=to(2)+(pos_i(2)-from(2))*to(4)/from(4);
        n_pos_i(3)=pos_i(3)*to(3)/from(3);
        n_pos_i(4)=pos_i(4)*to(4)/from(4);
        set(allax(i),'position',n_pos_i);
        set(allax(i),'units',axu)
    end
end


function [ax,allchld,xmax,ymax,hBorder]=CreateBorderAxes(fg)
ax=get(fg,'CurrentAxes');
if isempty(ax)
    ax = subplot(1,1,1,'parent',fg);
end
allchld=allchild(fg);
pu = get(fg,'paperunits');
set(fg,'paperunits','centimeter');
xmax=get(fg,'papersize');
ymax=xmax(2);
xmax=xmax(1);
set(fg,'paperposition',[0 0 xmax ymax]);
set(fg,'paperunits',pu)
hBorder = findall(gcf,'type','axes','tag','border');
if ~isempty(hBorder)
    delete(allchild(hBorder));
    allchld(ismember(allchld,hBorder))=[];
    set(hBorder, ...
        'xlim',[0 xmax], ...
        'ylim',[0 ymax]);
else
    hBorder=axes('units','normalized', ...
        'position',[0 0 1 1], ...
        'parent',fg, ...
        'tag','border', ...
        'xlimmode','manual', ...
        'ylimmode','manual', ...
        'xlim',[0 xmax], ...
        'ylim',[0 ymax], ...
        'visible','off');
    setappdata(hBorder,'NonDataObject',[]);
    if matlabversionnumber >= 9.05 % 2018b
        disableDefaultInteractivity(hBorder)
    end
end


function AdjustFigPos(fg)
if ~strcmp(get(fg,'WindowStyle'),'docked')
    set(fg,'units','pixels')
    %
    funits0=get(fg,'paperunits');
    set(fg,'paperunits','centimeters');
    PSize=get(fg,'papersize');
    set(fg,'paperunits',funits0);
    %
    maxdim=qp_getscreen(fg);
    if strcmp(get(fg,'PaperOrientation'),'portrait')
        pos1=round(PSize*min(fliplr(maxdim(3:4))./PSize));
        pos2=round(PSize*min(maxdim(3:4)./PSize));
        pos=min(pos1,pos2);
    else % 'landscape','rotated'
        pos1=round(PSize*min(maxdim(3:4)./PSize));
        pos2=round(PSize*min(fliplr(maxdim(3:4))./PSize));
        pos=min(pos1,pos2);
    end
    pos=pos*0.85;
    pos=[maxdim(1)+(maxdim(3)-pos(1))/2 maxdim(2)+(maxdim(4)-pos(2))/2 pos];
    set(fg,'position',pos);
end

function Str=locDateStr
t=[datestr(now,13) ' on ' datestr(now,8) ' '];
x=clock;
if x(3)>3
    t=[t num2str(x(3)) 'th'];
elseif x(3)==1
    t=[t num2str(x(3)) 'st'];
elseif x(3)==2
    t=[t num2str(x(3)) 'nd'];
elseif x(3)==3
    t=[t num2str(x(3)) 'rd'];
end
Str=[t ' ' datestr(now,3) ' ' datestr(now,10)];


function fig = Local_ui_paper(hBorder)
fg = get(hBorder,'parent');
vs = get(fg,'visible');

BFormat=get(hBorder,'userdata');
if isempty(BFormat) % backward compatibility and update
    BFormat.Border= 1;
    BFormat.Margin= [1 1 1 1];
    BFormat.LineWidth= 1.5;
    BFormat.Color= 'k';
    BFormat.HTabs= [0.68 0.16 0.16];
    BFormat.HRange= 19;
    BFormat.VTabs= [1 1 1]/3;
    BFormat.VRange= 2.7;
    BFormat.Box= [1 2 3; 1 4 4; 7 5 6];
    BFormat.Bold= [0 0 0 0 0 0 1];
    for i = 1:7
        hplottext=findobj(hBorder,'tag',sprintf('plottext%i',i));
        str = get(hplottext,'string');
        BFormat.(sprintf('BorderText%i',i)) = serialize(str);
    end
    set(hBorder,'userdata',BFormat)
end
Width = 560;
HRng  = Width-15-5*length(BFormat.HTabs);
HTabs = cumsum([0 BFormat.HTabs]);
N     = size(BFormat.Box,1);

maxdim=qp_getscreen(fg);
rps = get(fg,'position');
pos = [max(rps(1:2),maxdim(1:2)+[80 115]) Width 40+25*N];
h0 = figure('Units','pixels', ...
    'Color',get(0,'defaultuicontrolbackgroundcolor'), ...
    'HandleVisibility','off', ...
    'IntegerHandle','off', ...
    'MenuBar','none', ...
    'Name','Border manager', ...
    'NumberTitle','off', ...
    'Position',pos, ...
    'Resize','off', ...
    'Visible','off', ...
    'Tag','MD_PAPER Border Editor');

Box=fliplr(BFormat.Box');
for b=1:max(Box(:))
    [m,n]=find(Box==b);
    m1=min(m); m2=max(m); n1=min(n); n2=max(n);
    uicontrol('Parent',h0, ...
        'Units','pixels', ...
        'BackgroundColor',[1 1 1], ...
        'ListboxTop',0, ...
        'FontUnits','pixels', ...
        'FontSize',12, ...
        'Max',1+n2-n1, ...
        'HorizontalAlignment','left', ...
        'Position',[10+5*(m1-1)+HRng*HTabs(m1) 10+25*(n1-1) HRng*(HTabs(m2+1)-HTabs(m1))+5*(m2-m1) 25*(n2-n1+1)-5], ...
        'Style','edit', ...
        'Tag',sprintf('Text%i',b));
end

uicontrol('Parent',h0, ...
    'Units','pixels', ...
    'ListboxTop',0, ...
    'FontUnits','pixels', ...
    'FontSize',12, ...
    'HorizontalAlignment','left', ...
    'Position',[10 10+25*N 85 20], ...
    'Style','checkbox', ...
    'BackgroundColor',get(h0,'color'), ...
    'String','left page', ...
    'Visible',vs, ...
    'Tag','LeftPage');

uicontrol('Parent',h0, ...
    'Units','pixels', ...
    'ListboxTop',0, ...
    'FontUnits','pixels', ...
    'FontSize',12, ...
    'Position',[Width-175 10+25*N 80 20], ...
    'String','apply', ...
    'Callback','md_paper apply', ...
    'Visible',vs, ...
    'Tag','Apply');

uicontrol('Parent',h0, ...
    'Units','pixels', ...
    'ListboxTop',0, ...
    'FontUnits','pixels', ...
    'FontSize',12, ...
    'Position',[Width-90 10+25*N 80 20], ...
    'String','done', ...
    'Callback','md_paper done', ...
    'Tag','Done');

set(h0,'Visible','on')
if nargout>0
    fig = h0;
end


function str = serialize(str)
if iscell(str)
    if isempty(str)
        str = '';
    else
        str = str';
        str(2,:)={'\n{}'};
        str(2,end)={''};
        str = strcat(str{:});
    end
end

function str = deserialize(str)
str = strrep(str,'\n{}',char(13));
str = splitcellstr(str,char(13));
