function H = ui_message(Cmd,varargin)
%UI_MESSAGE Graphical display for errors/warnings.
%   UI_MESSAGE shows a resizable dialog for log messages. The log messages
%   persist even when the dialog is temporarily closed.
%
%   UI_MESSAGE(CMD,Message) adds the Message to the messages in the dialog
%   and depending on the CMD value it may draw a separator line, open the
%   dialog (if it had been closed) or bring it to front, and sound a beep.
%   The table below indicates the actions taken.
%
%   CMD       | draw separator | open dialog | sound beep
%   ----------|----------------|-------------|------------
%   'error'   |       X        |      X      |     X
%   'warning' |       X        |      X      |     -
%   'message' |       X        |      -      |     -
%   ''        |       -        |      -      |     -
%
%   UI_MESSAGE('max',MaximumNumberOfMessages) changes the maximum number of
%   messages; multiple regular messages (i.e. those without separator)
%   count as 1. The default value of the maximum number of messages is 10.
%
%   Example
%      ui_message('','First message')
%      ui_message('','Second message continues first')
%      ui_message('warning','A warning separated from regular messages!')
%      ui_message('error','An error too, but forces a show and beep!!')

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

persistent UD

XX.Clr.LightGray=get(0,'defaultuicontrolbackground');
XX.Clr.White=[1 1 1];

XX.Margin=10;
XX.Txt.Height=18;
XX.But.Height=20;
XX.Slider=20;

if nargin==1 && isequal(Cmd,'resize')
    fig=gcbf;
    %
    % Get new and old figure size (note: for this to work the figure size
    % must be initialized before the first call).
    %
    PrevSize = getappdata(fig,'FigureSize');
    MinSize = getappdata(fig,'MinimumFigureSize');
    if isempty(MinSize)
        MinSize = PrevSize;
        setappdata(fig,'MinimumFigureSize',MinSize)
    end
    NewPos = get(fig,'position');
    NewSize=NewPos(3:4);
    if any(NewSize<MinSize)
        NewSize=max(NewSize,MinSize);
        NewPos(2)=NewPos(2)+NewPos(4)-NewSize(2);
        NewPos(3:4)=NewSize;
        set(fig,'position',NewPos)
    end
    %
    % Define some shift operators
    %
    stretchhor   = [0 0 NewSize(1)-PrevSize(1) 0];
    stretchhor50 = [0 0 (NewSize(1)-PrevSize(1))/2 0];
    shifthor50   = [(NewSize(1)-PrevSize(1))/2 0 0 0];
    stretchver   = [0 0 0 NewSize(2)-PrevSize(2)];
    %
    btn=findobj(fig,'tag','clear');
    shiftcontrol(btn,stretchhor50)
    %
    btn=findobj(fig,'tag','close');
    shiftcontrol(btn,shifthor50+stretchhor50)
    %
    btn=findobj(fig,'tag','errorlist');
    c=get(btn,'listboxtop');
    shiftcontrol(btn,stretchhor+stretchver)
    set(btn,'listboxtop',c)
    %
    % Store the new figure size for usage during next resize command
    %
    setappdata(fig,'FigureSize',NewSize);
    %
    if nargout>0
        H=fig;
    end
    return
end

fig=findobj(allchild(0),'tag','UI_MESSAGE window');
if nargin>1 && isequal(Cmd,'reset')
    delete(fig)
    fig = [];
end

if isempty(UD)
    if ~isempty(fig) && ishandle(fig)
        UD=get(fig,'userdata');
    else
        UD.MaxNMessages=10;
        UD.errors={};
        UD.MessageOffset=[];
        UD.LastType='error';
    end
end

MaxNMessages=UD.MaxNMessages;
errors=UD.errors;
MessageOffset=UD.MessageOffset;
LastType=UD.LastType;

if isempty(fig) || ~ishandle(fig)
    
    ListWidth=400;  MinListWidth=40;
    ListHeight=200; MinListHeight=2*XX.Txt.Height;
    
    Fig_Width=ListWidth+2*XX.Margin;
    Fig_Height=3*XX.Margin+ListHeight+XX.But.Height;
    
    Min_Fig_Width=MinListWidth+2*XX.Margin;
    Min_Fig_Height=3*XX.Margin+MinListHeight+XX.But.Height;
    
    ss = qp_getscreen;
    swidth = ss(3);
    sheight = ss(4);
    left = ss(1)+(swidth-Fig_Width)/2;
    bottom = ss(2)+(sheight-Fig_Height)/2;
    rect = [left bottom Fig_Width Fig_Height];
    
    fig=qp_uifigure('Message Window','','UI_MESSAGE window',rect);
    set(fig,'closerequestfcn','ui_message close','resize','on','resizefcn','ui_message resize')
    if ~isstandalone && matlabversionnumber >= 7 && usejava('jvm')
        set(fig,'DockControls','on')
    end
    
    setappdata(fig,'WL_UserInterface',1)
    setappdata(fig,'FigureSize',[Fig_Width Fig_Height])
    setappdata(fig,'MinimumFigureSize',[Min_Fig_Width Min_Fig_Height])
    m1=uimenu('label','&File','parent',fig);
    uimenu('label','Cl&ear', ...
        'callback','ui_message clear', ...
        'tag','clearmenu', ...
        'parent',m1);
    uimenu('label','Save &As...', ...
        'callback','ui_message saveas', ...
        'tag','saveasmenu', ...
        'parent',m1);
    uimenu('label','&Close', ...
        'callback','ui_message close', ...
        'tag','closemenu', ...
        'separator','on', ...
        'parent',m1);
    
    m1=uimenu('label','&Edit','parent',fig);
    uimenu('label','&Copy to Clipboard', ...
        'callback','ui_message clipboard', ...
        'tag','clipboard', ...
        'parent',m1);
    
    rect = [XX.Margin XX.Margin (Fig_Width-3*XX.Margin)/2 XX.But.Height];
    uicontrol('style','pushbutton', ...
        'position',rect, ...
        'string','Clear', ...
        'parent',fig, ...
        'tag','clear', ...
        'callback','ui_message clear');
    
    rect(1) = (Fig_Width+XX.Margin)/2;
    uicontrol('style','pushbutton', ...
        'position',rect, ...
        'string','Close', ...
        'parent',fig, ...
        'tag','close', ...
        'callback','ui_message close');
    
    rect(1) = XX.Margin;
    rect(2) = rect(2)+rect(4)+XX.Margin;
    rect(3) = Fig_Width-2*XX.Margin;
    rect(4) = ListHeight;
    uicontrol('style','listbox', ...
        'callback','ui_message select', ...
        'position',rect, ...
        'parent',fig, ...
        'string',errors, ...
        'max',2, ...
        'tag','errorlist', ...
        'backgroundcolor',XX.Clr.White, ...
        'horizontalalignment','left', ...
        'enable','on');
    set(fig,'visible','on','userdata',UD);
end

if nargout>0
    H=fig;
end
if nargin==0
    set(fig,'visible','on')
else
    if nargin>1
        Msg = varargin{1};
    else
        Msg = '';
    end
    switch lower(Cmd)
        case {'error','warning','message',''}
            if nargin>2
                Msg = sprintf(varargin{:});
            end
            if strcmp(Cmd,'error')
                stdbeep
                figure(fig)
            elseif strcmp(Cmd,'warning')
                figure(fig)
            end
            Separator={};
            if isempty(errors)
                MessageOffset=1;
            else
                if ~isempty(Cmd) || ~isempty(LastType)
                    Separator={'---------'};
                    MessageOffset=[MessageOffset length(errors)+2];
                end
            end
            if isempty(Msg)
                Msg={};
            elseif iscellstr(Msg)
                % Does any cellstring contain char(10) ?
                i0=0;
                for j=1:length(Msg)
                    if size(Msg{j},1)==1
                        LBr=strfind([char(10) Msg{j} char(10)],char(10));
                        for i=1:(length(LBr)-1)
                            cMsg{i0+i}=Msg{j}(LBr(i):(LBr(i+1)-2));
                        end
                    else
                        for i=1:size(Msg{j})
                            cMsg{i0+i}=Msg{j}(i,:);
                        end
                    end
                    i0=length(cMsg);
                end
                Msg=cMsg;
            elseif ischar(Msg)
                if size(Msg,1)>1
                    Msg=cellstr(Msg);
                else
                    % Does it contain char(10) ?
                    LBr=strfind([char(10) Msg char(10)],char(10));
                    for i=1:(length(LBr)-1)
                        cMsg{i}=Msg(LBr(i):(LBr(i+1)-2));
                    end
                    Msg=cMsg;
                end
            else
                return
            end
            while length(Msg)>1 && isempty(Msg{end})
                Msg(end)=[];
            end
            Msg = strrep(Msg,char(9),'   ');
            errors=[errors(:)' Separator(:)' Msg(:)'];
            if length(MessageOffset)>MaxNMessages
                MessageOffset(1)=[];
                errors(1:(MessageOffset(1)-1))=[];
                MessageOffset=MessageOffset-MessageOffset(1)+1;
            end
            NL=length(Msg);
            set(findobj(fig,'tag','errorlist'),'string',errors,'value',length(errors)+1-(1:NL));
            LastType=Cmd;
        case 'select'
            if strcmp(get(gcbf,'SelectionType'),'open')
                S = get(gcbo,'string');
                v = get(gcbo,'value');
                s = S{v};
                if strncmp(s,'http://',7)
                    web(s,'-browser')
                elseif strncmp(s,'In ',3) && ~isempty(strfind(s,' at line ')) && ~isstandalone
                    i = strfind(s,' at line ');
                    file = s(4:i-1);
                    line = sscanf(s(i+9:end),'%i',1);
                    file = which(file);
                    opentoline(file,line,0)
                end
            end
        case 'clear'
            errors={};
            MessageOffset=[];
            set(findobj(fig,'tag','errorlist'),'string',errors,'listboxtop',1);
        case 'clipboard'
            i=sort(get(findobj(fig,'tag','errorlist'),'value')); % always copy lines from top to bottom
            clipboard('copy',sprintf('%s\n',errors{i}))
        case 'saveas'
            [f,p]=uiputfile('*.txt','Save Message List As');
            if ~ischar(f)
                return
            end
            pf=[p f];
            [p,f,e]=fileparts(pf);
            if isempty(e)
                pf=[pf '.txt'];
            end
            fid=fopen(pf,'wt');
            if fid<0
                ui_message('error',['Cannot open output file: ',pf]);
                return
            end
            fprintf(fid,'%s\n',errors{:});
            fclose(fid);
        case 'close'
            set(fig,'visible','off');
        case 'max'
            if ischar(Msg)
                Msg = str2double(Msg);
            end
            MaxNMessages=max(1,Msg);
            while length(MessageOffset)>MaxNMessages
                MessageOffset(1)=[];
                errors(1:(MessageOffset(1)-1))=[];
                MessageOffset=MessageOffset-MessageOffset(1)+1;
            end
            NL=length(Msg);
            set(findobj(fig,'tag','errorlist'),'string',errors,'value',length(errors)+1-(1:NL));
        case 'getall'
            H = errors;
    end
end
UD.errors=errors;
UD.MessageOffset=MessageOffset;
UD.MaxNMessages=MaxNMessages;
UD.LastType=LastType;
set(fig,'userdata',UD);
