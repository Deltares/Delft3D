function FinalAnswer=md_dialog(cmd,varargin)
%MD_DIALOG Simple dialog tool.
%
%   Answer=MD_DIALOG(Title, {Text List}, ...
%     {UI Type List}, {UI Options List}, ...
%     {Default Answer List})
%   Opens dialog and waits for answer. Except for the
%   first argument all arguments must be cell arrays
%   of equal length.
%
%   UI Type List and options:
%   edit        edit field, option: number of edit lines
%               (at most 5)
%   popupmenu   popup menu, option: cell array of choices
%   radiolist   list of mutual exclusive radio button choices
%               option: cell array of radio button strings
%   checkbox    checkbox item, no options
%   editint     edit field for integer, option: [min max]
%   editreal    edit field for floating point value, option:
%               [min max]
%   defedit     single line edit field with standard answers,
%               option: list of standard answers.
%
%   Example
%      md_dialog('Title', ...
%         {'Edit text:','Choose from list:', ...
%          'Select one:','Checkbox for true/false', ...
%          'Positive integer','Fraction', ...
%          'Edit or select:'}, ...
%         {'edit','popupmenu','radiolist','checkbox', ...
%          'editint','editreal','defedit'}, ...
%         {2,{'a','b','c'},{'al','bl','cl'},[],[0 inf], ...
%          [0 1],{'a','b','c'}}, ...
%         {'Double line edit','a','al',0,0,0.25,'g'})

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

if nargin>1
    [Err,FinalAnswer]=createdialog(cmd,varargin{:});
    if ~isempty(Err)
        error(Err);
    end
    return
end
F=gcbf;
switch cmd
    case 'cancel'
        if strcmp(get(F,'windowstyle'),'normal'), delete(F); return; end
        UD=get(F,'userdata');
        UD{3}={};
        set(F,'userdata',UD,'visible','off');
    case 'ok'
        if strcmp(get(F,'windowstyle'),'normal'), delete(F); return; end
        UD=get(F,'userdata');
        set(F,'visible','off');

    case 'entry'
        O=gcbo;
        i=get(O,'userdata');

        UD=get(F,'userdata');
        Type   =UD{1};
        Options=UD{2};
        Answer =UD{3};

        Oi=findall(F,'userdata',i);
        switch Type{i}
            case 'edit'
                Answer{i}=get(O,'string');
            case 'editint'
                Str=get(O,'string');
                Range=Options{i};
                Num=str2num(Str);
                if isequal(size(Num),[1 1]) & Num==round(Num) & Num>=Range(1) & Num<=Range(2)
                    Answer{i}=Num;
                else
                    Num=Answer{i};
                end
                set(O,'string',Num)
            case 'editreal'
                Str=get(O,'string');
                Range=Options{i};
                Num=str2num(Str);
                if isequal(size(Num),[1 1]) & Num>=Range(1) & Num<=Range(2)
                    Answer{i}=Num;
                else
                    Num=Answer{i};
                end
                set(O,'string',Num)
            case 'popupmenu'
                Str=get(O,'string');
                Val=get(O,'value');
                Answer{i}=Str{Val};
            case 'radiolist'
                if get(O,'value')
                    set(Oi,'value',0)
                end
                set(O,'value',1)
                Answer{i}=get(O,'string');
            case 'checkbox'
                Answer{i}=get(O,'value');
            case 'defedit'
                switch get(O,'type')
                    case 'uicontrol'
                        switch get(O,'style')
                            case 'pushbutton'
                                uicm=findobj(Oi,'type','uicontextmenu');
                                set(uicm,'visible','on')
                                return
                            case 'edit'
                                Answer{i}=get(O,'string');
                        end
                    case 'uimenu'
                        Answer{i}=get(O,'label');
                        Ed=findobj(Oi,'style','edit');
                        set(Ed,'string',Answer{i})
                end
        end

        UD{3}=Answer;
        set(F,'userdata',UD);
    otherwise
        error('Unknown command: %s.',cmd)
end


%Type = checkbox, edit, popupmenu
function [Err,Answer]=createdialog(Title,Prompt,Type,Options,Default)
Err='';
Answer={};
if nargin<2
    Err='Too few input arguments.'; return
end
if ~ischar(Title)
    Err='Title argument should be string (1st argument).'; return
end
if ~iscellstr(Prompt)
    Err='Dialog item strings should be combined in a cell array (2nd argument).'; return
end
if nargin<3 | isempty(Type)
    Type=repmat({'edit'},size(Prompt));
elseif ~isequal(size(Type),size(Prompt))
    Err='Invalid size of type array (3rd argument).'; return
elseif ~iscellstr(Type)
    Err='Type array should be a cell array (3rd argument).'; return
else
    Type=lower(Type);
    T=ismember(Type,{'edit','checkbox','editint','editreal','popupmenu','radiolist','defedit'});
    if any(~T)
        i=find(~T);
        Err=sprintf('Invalid type: %s.',Type{i(1)}); return
    end
end
if nargin<4 | isempty(Options)
    Options=repmat({[]},size(Prompt));
elseif ~iscell(Options)
    Err='Options array (4th argument) should be a cell array.'; return
elseif ~isequal(size(Options),size(Prompt))
    Err='Invalid size of options array (4th argument).'; return
end
AutoDefault=0;
if nargin<5 | isempty(Default)
    Default=repmat({''},size(Prompt));
    AutoDefault=1;
elseif ~iscell(Default)
    Err='Default array (5th argument) should be a cell array.'; return
elseif ~isequal(size(Default),size(Prompt))
    Err='Invalid size of default array (5th argument).'; return
end
%
% check combinations of type, options and defaults.
%
nItem=prod(size(Prompt));
for i=1:nItem
    switch Type{i}
        case {'popupmenu','radiolist'}
            if isempty(Options{i})
                Err=sprintf('String list expected as option for item %i.',i); return
            else
                if ~iscellstr(Options{i})
                    Err=sprintf('Invalid option  for item %i.',i); return
                end
            end
            if ~ischar(Default{i})
                Err=sprintf('Invalid default value for item %i.',i); return
            elseif isempty(strmatch(Default{i},Options{i},'exact'))
                Err=sprintf('Default value not in string list for item %i.',i); return
            end
        case {'defedit'}
            if isempty(Options{i})
                Err=sprintf('String list expected as option for item %i.',i); return
            else
                if ~iscellstr(Options{i})
                    Err=sprintf('Invalid option  for item %i.',i); return
                end
            end
            if ~ischar(Default{i})
                Err=sprintf('Invalid default value for item %i.',i); return
            end
        case 'edit'
            if isempty(Options{i})
                Options{i}=1;
            else
                ii=Options{i};
                if ~isnumeric(ii) | ~isequal(size(ii),[1 1]) | ii~=round(ii) | ii<0
                    Err=sprintf('Invalid option  for item %i.',i); return
                end
            end
            if Options{i}==1
                if ~ischar(Default{i})
                    Err=sprintf('Invalid default value for item %i.',i); return
                end
            else % Options{i}>1
                if ischar(Default{i})
                    Default{i}=cellstr(Default{i});
                elseif ~iscellstr(Default{i})
                    Err=sprintf('Invalid default value for item %i.',i); return
                end
            end
        case {'editint','editreal'}
            if isempty(Options{i})
                Options{i}=[-inf inf];
            else
                ii=Options{i};
                if ~isnumeric(ii) | ~isequal(size(ii),[1 2]) | ii(1)>ii(2)
                    Err=sprintf('Invalid option  for item %i.',i); return
                end
            end
            if AutoDefault
                if isfinite(Options{i}(1))
                    Default{i}=Options{i}(1);
                elseif isfinite(Options{i}(2))
                    Default{i}=Options{i}(2);
                else
                    Default{i}=0;
                end
            elseif ~isnumeric(Default{i}) | ~isequal(size(Default{i}),[1 1])
                Err=sprintf('Invalid default value for item %i.',i); return
            else
                ii=Default{i};
                if ii<Options{i}(1) | ii>Options{i}(2)
                    Err=sprintf('Default value out of range for item %i.',i); return
                elseif ii~=round(ii) & isequal(Type{i},'editint')
                    Err=sprintf('Default value for item %i should be integer.',i); return
                end
            end
        case 'checkbox'
            if AutoDefault
                Default{i}=0;
            elseif ~isequal(Default{i},0) & ~isequal(Default{i},1) & ~AutoDefault
                Err=sprintf('Invalid default value for item %i.\nExpected 0 or 1.',i); return
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
width=320;
margin=10;

h0 = figure('Visible','off', ...
    'Units','pixels', ...
    'Color',Inactive, ...
    'IntegerHandle','off', ...
    'MenuBar','none', ...
    'Name',Title, ...
    'Doublebuffer','on', ...
    'CloseRequestFcn','', ...
    'NumberTitle','off', ...
    'Resize','off', ...
    'Handlevisibility','callback', ...
    'Tag','md_dialog');
set(h0,'DefaultUicontrolFontUnits','pixels', ...
    'DefaultUicontrolFontSize',12);

%======

voffset=margin+1;
h1 = uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Callback','md_dialog ok', ...
    'Position',[width-60-margin voffset 60 20], ...
    'String','OK', ...
    'Enable','on');
h1 = uicontrol('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Callback','md_dialog cancel', ...
    'Position',[width-130-margin voffset 60 20], ...
    'String','cancel', ...
    'Enable','on');

%======

for i=length(Prompt):-1:1
    switch Type{i}
        case {'edit','editint','editreal'}
            voffset=voffset+25;
            Max=1;
            if isequal(Type{i},'edit')
                Max=Options{i};
            end
            Max5=min(Max,5);
            h1 = uicontrol('Parent',h0, ...
                'BackgroundColor',Active, ...
                'Callback','md_dialog entry', ...
                'Position',[10 voffset width-2*margin 20*Max5], ...
                'horizontalalignment','left', ...
                'Style','edit', ...
                'String',Default{i}, ...
                'Userdata',i, ...
                'Max',Max, ...
                'Enable','on');
            voffset=voffset+20*Max5;
            h1 = uicontrol('Parent',h0, ...
                'BackgroundColor',Inactive, ...
                'horizontalalignment','left', ...
                'Position',[10 voffset width-2*margin 16], ...
                'Style','text', ...
                'String',Prompt{i}, ...
                'Enable','on');
        case {'defedit'}
            voffset=voffset+25;
            h1 = uicontrol('Parent',h0, ...
                'BackgroundColor',Active, ...
                'Callback','md_dialog entry', ...
                'Position',[10 voffset width-2*margin-20 20], ...
                'horizontalalignment','left', ...
                'Style','edit', ...
                'String',Default{i}, ...
                'Userdata',i, ...
                'Enable','on');
            cdat=[1 1 1 1 1 1 1
                0 1 1 1 1 1 0
                0 0 1 1 1 0 0
                0 0 0 1 0 0 0];
            cdat=cdat(:);
            rgbdat=repmat(Inactive,28,1);
            rgbdat(logical(cdat),1:3)=repmat([0 0 0],sum(cdat),1);
            rgbdat=reshape(rgbdat,[4 7 3]);
            uicm = uicontextmenu('Parent',h0, ...
                'Position',[width-2*margin-10 voffset]+10, ...
                'Userdata',i);
            nRB=length(Options{i});
            for rbi=1:nRB
                uimenu('parent',uicm, ...
                    'label',Options{i}{rbi}, ...
                    'callback','md_dialog entry', ...
                    'userdata',i)
            end
            h1 = uicontrol('Parent',h0, ...
                'BackgroundColor',Inactive, ...
                'Callback','md_dialog entry', ...
                'Position',[width-2*margin-10 voffset 20 20], ...
                'horizontalalignment','left', ...
                'Style','pushbutton', ...
                'String','', ...
                'cdata',rgbdat, ...
                'Userdata',i, ...
                'Enable','on');
            voffset=voffset+20;
            h1 = uicontrol('Parent',h0, ...
                'BackgroundColor',Inactive, ...
                'horizontalalignment','left', ...
                'Position',[10 voffset width-2*margin 16], ...
                'Style','text', ...
                'String',Prompt{i}, ...
                'Enable','on');
        case {'popupmenu'}
            voffset=voffset+25;
            ii=strmatch(Default{i},Options{i},'exact');
            h1 = uicontrol('Parent',h0, ...
                'BackgroundColor',Active, ...
                'Callback','md_dialog entry', ...
                'Position',[10 voffset width-2*margin 20], ...
                'horizontalalignment','left', ...
                'Style','popupmenu', ...
                'String',Options{i}, ...
                'Value',ii, ...
                'Userdata',i, ...
                'Enable','on');
            voffset=voffset+20;
            h1 = uicontrol('Parent',h0, ...
                'BackgroundColor',Inactive, ...
                'horizontalalignment','left', ...
                'Position',[10 voffset width-2*margin 16], ...
                'Style','text', ...
                'String',Prompt{i}, ...
                'Enable','on');
        case {'radiolist'}
            voffset=voffset+25;
            ii=strmatch(Default{i},Options{i},'exact');
            nRB=length(Options{i});
            for rbi=nRB:-1:1
                h1 = uicontrol('Parent',h0, ...
                    'BackgroundColor',Inactive, ...
                    'Callback','md_dialog entry', ...
                    'Position',[10 voffset width-2*margin 20], ...
                    'horizontalalignment','left', ...
                    'Style','radiobutton', ...
                    'String',Options{i}{rbi}, ...
                    'Value',rbi==ii, ...
                    'Userdata',i, ...
                    'Enable','on');
                voffset=voffset+20;
            end
            h1 = uicontrol('Parent',h0, ...
                'BackgroundColor',Inactive, ...
                'horizontalalignment','left', ...
                'Position',[10 voffset width-2*margin 16], ...
                'Style','text', ...
                'String',Prompt{i}, ...
                'Enable','on');
        case 'checkbox'
            voffset=voffset+25;
            h1 = uicontrol('Parent',h0, ...
                'BackgroundColor',Inactive, ...
                'Callback','md_dialog entry', ...
                'Position',[10 voffset width-2*margin 20], ...
                'horizontalalignment','left', ...
                'Style','checkbox', ...
                'String',Prompt{i}, ...
                'Value',Default{i}, ...
                'Userdata',i, ...
                'Enable','on');
    end
end

%======

voffset=voffset+25;
dims=[width voffset+5];
sz=qp_getscreen;
pos(1:2)=sz(1:2)+(sz(3:4)-dims)/2;
pos(3:4)=dims;

set(h0,'Position',pos, ...
    'Visible','on', ...
    'WindowStyle','modal', ...
    'Userdata',{Type Options Default});

waitfor(h0,'visible')
UD=get(h0,'userdata');
delete(h0);
Answer=UD{3};
