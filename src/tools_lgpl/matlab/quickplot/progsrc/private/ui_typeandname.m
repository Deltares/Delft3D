function varargout=ui_typeandname(varargin)
%UI_TYPEANDNAME  Selection dialog with name specification.
%   [SelectedType, SelectedName, SelectedNr] = UI_TYPEANDNAME(Types)
%   creates a dialog in which the user can select one of the type
%   strings specified in the cell string array Types. The selected type
%   string is returned as SelectedType, its number in the list is
%   returned as SelectedNr. The user can also specify a name, which is
%   returned as SelectedName.
%
%   ... = UI_TYPEANDNAME(Types, DefaultType, DefaultName)
%   specifies the default type and name as two additional input arguments.
%
%   ... = UI_TYPEANDNAME(..., 'windowtitle', Title)
%   set the dialog name/title; it is empty by default.
%
%   ... = UI_TYPEANDNAME(..., 'multiselect')
%   allow the user to select zero, one, or multiple types. When using this
%   option, the SelectedType output argument is a cell string instead of a
%   char array.
%
%   [SelectedType, SelectedNr] = UI_TYPEANDNAME(..., 'specifyname', 'off')
%   disable the option to specify a name. The user can only select one (or
%   zero/more when combined with 'multiselect') quantity from the list.
%
%   See also UI_TYPE

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

ListWidth=300;
ListHeight=300;
XX=xx_constants;

selname=[];
windowtitle='';
specifyname=1;
multiselect = false;

varin=varargin;
i=1;
while i<=length(varin)
    if ~ischar(varin{i}) || size(varin{i},1)~=1
        i = i+1;
    else
        switch varin{i}
            case 'windowtitle'
                windowtitle=varin{i+1};
                varin(i:i+1)=[];
            case 'multiselect'
                multiselect = true;
                varin(i) = [];
            case 'specifyname'
                specifyname=varin{i+1};
                if ischar(specifyname)
                    switch lower(specifyname)
                        case {'yes','y','on','true'}
                            specifyname=1;
                        case {'no','n','off','false'}
                            specifyname=0;
                    end
                elseif isnumeric(specifyname)
                    specifyname=logical(specifyname(1));
                end
                varin(i:i+1)=[];
            otherwise
                i=i+1;
        end
    end
end
if specifyname
    if nargout>3
        error('Too many output arguments.')
    end
else
    if nargout>2
        error('Too many output arguments.')
    end
end

switch length(varin)
    case 0
        error('Too few input arguments: missing type list.')
    case 1 % =UI_TYPEANDNAME(Types)
        types=varin{1};
        itypes=1;
    case 2 % =UI_TYPEANDNAME(Types,DefaultType)
        types=varin{1};
        itypes=varin{2};
    case 3 % =UI_TYPEANDNAME(Types,DefaultType,DefaultName)
        if specifyname
            types=varin{1};
            itypes=varin{2};
            selname=varin{3};
            if ~ischar(selname)
                error('Invalid default selection name or unknown keywords.')
            end
        else
            error('Too many input arguments or unknown keywords.')
        end
    otherwise
        error('Too many input arguments or unknown keywords.')
end

if ischar(types)
    types=cellstr(types);
elseif ~iscellstr(types)
    error('Invalid list supplied.')
end

if iscellstr(itypes)
    seltypes = itypes;
    if ~multiselect
        error('DefaultType argument is only allowed to be cell string if the "multiselect" option is specified.')
    else
        [Lia, Locb] = ismember(seltypes, types);
        if any(~Lia)
            wrong_type = seltypes{find(~Lia,1,'first')};
            error('The DefaultType "%s" is invalid.', wrong_type)
        end
        itypes = Locb;
    end

elseif ischar(itypes)
    seltypes = itypes;
    [Lia, Locb] = ismember(seltypes, types);
    if ~Lia
        error('The DefaultType "%s" is invalid.', seltypes)
    else
        itypes = Locb;
    end

else % numeric
    if any(itypes > length(types)) ... % valid entries are not too large
            || any(itypes < 1) ... % positive
            || any(itypes ~= round(itypes)) % integer
        error('The DefaultType selection is invalid (positive integers in the range 1:%i).', length(types))
    elseif ~multiselect && ~isequal(size(itypes),[1 1])
        error('Without "multiselect" the DefaultType should be a single integer.')
    end
end

if isempty(types) % nothing to be selected
    return
end

Fig_Width=ListWidth+2*XX.Margin;
Fig_Height=3*XX.Margin+ListHeight+XX.Txt.Height+XX.But.Height;
if specifyname
    Fig_Height=Fig_Height+XX.Margin+XX.Txt.Height+XX.But.Height;
end
ss = qp_getscreen;
swidth = ss(3);
sheight = ss(4);
left = ss(1)+(swidth-Fig_Width)/2;
bottom = ss(2)+(sheight-Fig_Height)/2;
rect = [left bottom Fig_Width Fig_Height];

fig=qp_uifigure(windowtitle,'','ui_typeandname',rect);
set(fig,'resize','on', ...
    'resizefcn',@ui_typeandname_resize, ...
    'keypressfcn',@ui_typeandname_keypress)

rect(2) = rect(2)+rect(4)+XX.Margin;
rect(4) = ListHeight;
ListBox=uicontrol('style','listbox', ...
    'position',rect, ...
    'tag','list', ...
    'parent',fig, ...
    'string',types, ...
    'value',itypes, ...
    'backgroundcolor',XX.Clr.White, ...
    'keypressfcn',@ui_typeandname_keypress, ...
    'callback','set(gcbf,''userdata'',1)', ...
    'enable','on');
if multiselect
    set(ListBox,'max',2)
end

rect(2) = rect(2)+rect(4);
rect(4) = XX.Txt.Height;
uicontrol('style','text', ...
    'position',rect, ...
    'tag','selecttext', ...
    'horizontalalignment','left', ...
    'string','Select...', ...
    'parent',fig);

rect(1) = XX.Margin;
rect(3) = Fig_Width-2*XX.Margin;
Edit=[];
if specifyname
    if ~ischar(selname)
        selname = default_name(types, itypes);
    end
    rect(2) = rect(2)+rect(4)+XX.Margin;
    Edit=uicontrol('style','edit', ...
        'position',rect, ...
        'tag','edit', ...
        'horizontalalignment','left', ...
        'string',selname, ...
        'parent',fig, ...
        'backgroundcolor',XX.Clr.White, ...
        'keypressfcn',@ui_typeandname_keypress, ...
        'callback','set(gcbf,''userdata'',2)');
    
    rect(2) = rect(2)+rect(4);
    rect(4) = XX.Txt.Height;
    uicontrol('style','text', ...
        'position',rect, ...
        'horizontalalignment','left', ...
        'string','Specify Name...', ...
        'parent',fig);
end

rect = [XX.Margin XX.Margin (Fig_Width-3*XX.Margin)/2 XX.But.Height];
uicontrol('style','pushbutton', ...
    'position',rect, ...
    'string','Cancel', ...
    'tag','cancel', ...
    'parent',fig, ...
    'keypressfcn',@ui_typeandname_keypress, ...
    'callback','set(gcbf,''userdata'',-1)');

rect(1) = (Fig_Width+XX.Margin)/2;
uicontrol('style','pushbutton', ...
    'position',rect, ...
    'string','Continue', ...
    'tag','continue', ...
    'parent',fig, ...
    'keypressfcn',@ui_typeandname_keypress, ...
    'callback','set(gcbf,''userdata'',0)');

set(fig,'visible','on');
if specifyname
    varargout={'','',-1};
else
    varargout={'',-1};
end
while 1
    waitfor(fig,'userdata');
    Cmd=get(fig,'userdata');
    set(fig,'userdata',[]);
    switch Cmd
        case {-1,27} % cancel
            break
        case 0 % continue
            itypes = get(ListBox,'value');
            if multiselect
                seltypes = types(itypes);
            else
                seltypes = types{itypes};
            end
            if specifyname
                selname=get(Edit,'string');
                varargout={seltypes,selname,itypes};
            else
                varargout={seltypes,itypes};
            end
            break;
        case 1 % listbox
            itypes = get(ListBox,'value');
            str = default_name(types, itypes);
            set(Edit,'string',str);
    end
end
delete(fig);

function str = default_name(types, itypes)
switch length(itypes)
    case 0
        str = 'none';
    case 1
        str = types{itypes};
    otherwise
        str = 'multiple';
end

function ui_typeandname_keypress(source,event)
if isequal(get(gcbf,'currentcharacter'),27) % escape
    set(gcbf,'userdata',-1)
end

function ui_typeandname_resize(source,event)
XX=xx_constants;

FPos=get(gcbf,'position');
Fig_Width=FPos(3);
Fig_Height=FPos(4);
%---
ed=findobj(gcbf,'tag','edit');
specifyname=double(~isempty(ed));
ymin=2*XX.Margin+XX.But.Height+specifyname*(XX.Margin+XX.But.Height+XX.Txt.Height);
ListWidth=max(100,Fig_Width-2*XX.Margin);
ListHeight=max(2*XX.Txt.Height,Fig_Height-ymin-XX.Txt.Height-XX.Margin);
Fig_Width=ListWidth+2*XX.Margin;
Fig_Height=ListHeight+ymin+XX.Txt.Height+XX.Margin;
FPos(3)=Fig_Width;
FPos(2)=FPos(2)+FPos(4)-Fig_Height;
FPos(4)=Fig_Height;
set(gcbf,'position',FPos)
%---
cn=findobj(gcbf,'tag','cancel');
set(cn,'position',[XX.Margin XX.Margin (Fig_Width-3*XX.Margin)/2 XX.But.Height ])
co=findobj(gcbf,'tag','continue');
set(co,'position',[(Fig_Width+XX.Margin)/2 XX.Margin (Fig_Width-3*XX.Margin)/2 XX.But.Height ])
%---
ed=findobj(gcbf,'tag','edit');
specifyname=double(~isempty(ed));
set(ed,'position',[XX.Margin 2*XX.Margin+XX.But.Height Fig_Width-2*XX.Margin XX.But.Height ])
%---
ls=findobj(gcbf,'tag','list');
set(ls,'position',[XX.Margin ymin Fig_Width-2*XX.Margin max(1,Fig_Height-ymin-XX.Txt.Height-XX.Margin)])
%---
st=findobj(gcbf,'tag','selecttext');
set(st,'position',[XX.Margin Fig_Height-XX.Margin-XX.Txt.Height Fig_Width-2*XX.Margin XX.Txt.Height ])
%---
