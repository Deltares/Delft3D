function varargout=ui_typeandname(varargin)
%UI_TYPEANDNAME  Selection dialog with name specification.
%   [SelectedType,SelectedName,SelectedNr]=UI_TYPEANDNAME(Types)
%   creates a dialog in which the user can select one of the type
%   strings specified in the cell string array Types. The selected type
%   string is returned as SelectedType, its number in the list is
%   returned as SelectedNr. The user can also specify a name, which is
%   returned as SelectedName.
%
%   Default type and name can be specified as two additional input
%   arguments:
%   ...=UI_TYPEANDNAME(Types,DefaultType,DefaultName)
%
%   The dialog name/title is by default empty. It can be set by
%   specifying the keyword WINDOWTITLE and the title:
%   ...=UI_TYPEANDNAME(...,'windowtitle',Title)
%
%   If the user should only select a quantity from a list (no
%   additional name associated with the selection) you can simplify the
%   dialog accordingly by specifying the keyword SPECIFYNAME and as
%   value NO. The number of output arguments is reduced to two, namely
%   only the SelectedType and SelectedNr:
%   [SelectedType,SelectedNr]=UI_TYPEANDNAME(...,'specifyname','no')
%
%   See also UI_TYPE

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

ListWidth=300;
ListHeight=300;
XX=xx_constants;

selname=[];
windowtitle='';
specifyname=1;

varin=varargin;
i=1;
while i<=length(varin)
    if ~ischar(varin{i}) || size(varin{i},1)~=1
        i = i+1;
    elseif strcmpi(varin{i},'windowtitle') && i<length(varin)
        windowtitle=varin{i+1};
        varin(i:i+1)=[];
    elseif strcmpi(varin{i},'specifyname') && i<length(varin)
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
    else
        i=i+1;
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
        seltype=1;
    case 2 % =UI_TYPEANDNAME(Types,DefaultType)
        types=varin{1};
        seltype=varin{2};
    case 3 % =UI_TYPEANDNAME(Types,DefaultType,DefaultName)
        if specifyname
            types=varin{1};
            seltype=varin{2};
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
if ischar(seltype)
    s=ustrcmpi(seltype,types);
    if s<0
        error('Invalid default type selection string.')
    else
        seltype=s;
    end
else
    if seltype>length(types) || seltype<1 || seltype~=round(seltype) || ~isequal(size(seltype),[1 1])
        error('Invalid default type selection number.')
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

rect(1) = XX.Margin;
rect(3) = Fig_Width-2*XX.Margin;
Edit=[];
if specifyname
    if ~ischar(selname)
        selname=types{seltype};
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

rect(2) = rect(2)+rect(4)+XX.Margin;
rect(4) = ListHeight;
ListBox=uicontrol('style','listbox', ...
    'position',rect, ...
    'tag','list', ...
    'parent',fig, ...
    'string',types, ...
    'value',seltype, ...
    'backgroundcolor',XX.Clr.White, ...
    'keypressfcn',@ui_typeandname_keypress, ...
    'callback','set(gcbf,''userdata'',1)', ...
    'enable','on');

rect(2) = rect(2)+rect(4);
rect(4) = XX.Txt.Height;
uicontrol('style','text', ...
    'position',rect, ...
    'tag','selecttext', ...
    'horizontalalignment','left', ...
    'string','Select...', ...
    'parent',fig);

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
            break;
        case 0 % continue
            selnr=get(ListBox,'value');
            seltype=types{selnr};
            if specifyname
                selname=get(Edit,'string');
                varargout={seltype,selname,selnr};
            else
                varargout={seltype,selnr};
            end
            break;
        case 1 % listbox
            set(Edit,'string',types{get(ListBox,'value')});
    end
end
delete(fig);

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
