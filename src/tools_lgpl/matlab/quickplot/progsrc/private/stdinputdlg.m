function Answer=stdinputdlg(Prompt, Title, NumLines, DefAns)
%STDINPUTDLG Input dialog box using standard settings.

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

if ischar(Prompt)
    NQuestions=1;
    Prompt={Prompt};
elseif iscell(Prompt)
    NQuestions=length(Prompt);
else
    error('Invalid first argument. Expected prompt strings.')
end
switch nargin
    case 2 %Prompt, Title
        DefAns{1:NQuestions}='';
    case 3 %Prompt, Title, DefAns
        DefAns=NumLines;
    case 4 %Prompt, Title, NumLines, DefAns
        %warning('Third argument (NumLines) ignored.');
end

XX=xx_constants;

XX.But.Width=100;
EntryWidth=400;

Fig_Width=max(EntryWidth+2*XX.Margin,3*XX.Margin+2*XX.But.Width);
Fig_Height=2*XX.Margin+XX.But.Height+ ...
    NQuestions*(XX.Margin+XX.But.Height+XX.Txt.Height);

ss = qp_getscreen;
swidth = ss(3);
sheight = ss(4);
left = ss(1)+(swidth-Fig_Width)/2;
bottom = ss(2)+(sheight-Fig_Height)/2;
rect = [left bottom Fig_Width Fig_Height];

Cmd=@stdinputdlg_callback;
fig=qp_uifigure(Title,'Cancel','StdInputDlg',rect,Cmd);

uicontrol('Parent',fig, ...
    'BackgroundColor',XX.Inactive, ...
    'Callback',{Cmd 'OK'}, ...
    'Position',[Fig_Width-(2*XX.Margin+2*XX.But.Width) XX.Margin XX.But.Width XX.But.Height], ...
    'String','OK', ...
    'Tag','OK');
uicontrol('Parent',fig, ...
    'BackgroundColor',XX.Inactive, ...
    'Callback',{Cmd 'Cancel'}, ...
    'Position',[Fig_Width-(XX.Margin+XX.But.Width) XX.Margin XX.But.Width XX.But.Height], ...
    'String','Cancel', ...
    'Tag','Cancel');

voffset=2*XX.Margin+XX.But.Height;

h=[];
for j=1:NQuestions
    i=NQuestions-j+1;
    Lbl=sprintf('Q%2.2i',j);
    h(j)=uicontrol('Parent',fig, ...
        'BackgroundColor',XX.Active, ...
        'style','edit', ...
        'Position',[XX.Margin voffset+(i-1)*(XX.Margin+XX.Txt.Height+XX.But.Height) EntryWidth XX.But.Height], ...
        'Horizontalalignment','left', ...
        'String',DefAns{j}, ...
        'Tag',Lbl);
    uicontrol('Parent',fig, ...
        'style','text', ...
        'Position',[XX.Margin voffset+XX.But.Height+(i-1)*(XX.Margin+XX.Txt.Height+XX.But.Height) EntryWidth XX.Txt.Height], ...
        'Horizontalalignment','left', ...
        'String',Prompt{j});
end
set(fig,'visible','on')
waitfor(fig,'visible')
if ishandle(fig)
    Answer=get(h,'string');
    if ~iscell(Answer), Answer={Answer}; end
    delete(fig);
else
    Answer={};
end

function stdinputdlg_callback(obj,event,Prompt)
switch Prompt
    case 'OK'
        set(gcbf,'visible','off');
    case 'Cancel'
        delete(gcbf);
end
