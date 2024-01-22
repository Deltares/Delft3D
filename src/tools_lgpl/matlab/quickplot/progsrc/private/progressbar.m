function Fg0=progressbar(varargin)
%PROGRESSBAR Display progress bar.
%   This function is an alternative for the wait
%   bar function of MATLAB. This implementation
%   is somewhat faster and displays progress in
%   titlebar as well.
%
%   H = PROGRESSBAR(X)
%   Creates a progress bar of fractional length X.
%
%   PROGRESSBAR(X,H)
%   Updates progress bar H to fractional length X.
%
%   ...,'title',String)
%   Set title of progressbar to specified string.
%
%   ...,'color',Color)
%   Set color of progressbar to specified color.
%
%   ...,'pause','on')
%   Activate pause feature, which pauses the execution at the next
%   progressbar update.
%
%   ...,'cancel',CancelFcn)
%   Activate cancel button (and close window button)
%   and execute CancelFcn when clicked. If CancelFcn
%   is empty, deactivate cancel function.
%
%   H = PROGRESSBAR(...)
%   Returns the handle of the progress bar, or -1 if the progressbar
%   no longer exists.

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

Update=0;
Scan=0;
if nargin==0
    frac=NaN;
    Fg=[];
elseif nargin==1
    frac=varargin{1};
    Fg=[];
elseif nargin==2
    frac=varargin{1};
    Fg=varargin{2};
    if ischar(frac)
        Scan=1;
    end
else
    Scan=1;
end
if Scan
    CellFields={'Name' 'HasDefault' 'Default' 'List' 'CaseSensitive'};
    CellValues={'Fraction'            1 NaN   '' 0
        'Figure'              1 []    '' 0
        'Title'               1 -1    '' 0
        'Cancel'              1 -1    '' 0
        'Pause'               1  '' {'on','off'} 0
        'Color'               1 -1    '' 0};
    [X,err]=procargs(varargin,CellFields,CellValues);
    if ~isempty(err)
        error(err)
    end
    frac=X.Fraction;
    Fg=X.Figure;
    Update=1;
end
if isempty(Fg) && ishandle(frac) && strcmp(get(frac,'type'),'figure')
    Fg=frac;
    frac=NaN;
end
if isempty(Fg) && nargout==0
    Fg = findobj(allchild(0),'flat','Tag','DelftProgressBar');
    if ~isempty(Fg)
        Fg=Fg(1);
    end
end

Width=220;
First=0;
if isempty(Fg)
    sz=[360 60];
    ssz=qp_getscreen;
    pos=[ssz(1:2)+ssz(3:4)/2-sz/2 sz];
    try
        Inactive = qp_settings('UIInActiveColor');
    catch
        Inactive = get(0,'defaultuicontrolbackgroundcolor');
    end

    Fg=figure('position',pos,'numbertitle','off','name','0%','resize','off','menubar','none','color',Inactive,'closerequestfcn','','integerhandle','off','handlevisibility','off','tag','DelftProgressBar');
    setappdata(Fg,'WL_UserInterface',1)
    UD.Title='';
    UD.Pause=uicontrol('parent',Fg,'pos',[240 30 50 20],'style','togglebutton','string','pause','enable','off','tag','pause','value',0,'callback',@dopause);
    UD.Cancel=uicontrol('parent',Fg,'pos',[300 30 50 20],'string','cancel','enable','off','tag','cancel');
    uicontrol('parent',Fg,'pos',[10 30 Width 20],'style','edit','enable','inactive','backgroundcolor',Inactive);
    UD.HBar=uicontrol('parent',Fg,'pos',[10 30 0.1 20],'style','pushbutton','enable','inactive','backgroundcolor',[0 0 1],'foregroundcolor','w','string','0%','tag','bar','userdata',0);
    UD.TRem=uicontrol('parent',Fg,'pos',[10 10 sz(1)-20 18],'style','text','enable','on','string','Time remaining: --','tag','remain','horizontalalignment','left');
    drawnow
    if isnan(frac)
        frac=0;
    end
    UD.FracPrev=1;
    UD.StartTime=now;
    UD.StartFrac=frac;
    First=1;
    set(Fg,'userdata',UD)
elseif ishandle(Fg)
    UD=get(Fg,'userdata');
    if isnan(frac)
        frac=UD.FracPrev;
    end
else
    Fg0=-1;
    return
end
hbar=UD.HBar;
fracprev=UD.FracPrev;
frac=round(max(0,min(1,frac))*100)/100;
if Update
    switch X.Pause
        case {'on','off'}
            set(UD.Pause,'enable',X.Pause)
    end
    %
    if ischar(X.Cancel)
        set(Fg,'closerequestfcn',X.Cancel);
        set(UD.Cancel,'callback',X.Cancel);
        if isempty(X.Cancel)
            set(UD.Cancel,'enable','off')
        else
            set(UD.Cancel,'enable','on')
        end
    end
    if ~isequal(X.Color,-1)
        set(UD.HBar,'backgroundcolor',X.Color);
    end
    if ischar(X.Title)
        if ~isempty(X.Title)
            UD.Title=cat(2,X.Title,' : ');
        else
            UD.Title='';
        end
        fracprev=-1;
    end
end
if ~isequal(frac,fracprev) && ~First
    fracprev=UD.FracPrev;
    UD.FracPrev=frac;
    if frac<0.11
        Str=sprintf('%.0f',frac*100);
        Str2=cat(2,Str,'%');
    else
        Str=sprintf('%.0f%%',frac*100);
        Str2=Str;
    end
    if frac<fracprev || frac<=UD.StartFrac
        % when going back reset clock
        UD.StartTime=now;
        UD.StartFrac=frac;
        TRem = sprintf('--');
    else
        tremain = (1-frac)*24*3600*(now-UD.StartTime)/(frac-UD.StartFrac);
        if tremain>3600
            thrs = floor(tremain/3600);
            tmin = round((tremain-thrs*3600)/60);
            TRem = sprintf('%i hours and %.0f minutes',thrs,tmin);
        elseif tremain>60
            tmin = floor(tremain/60);
            tsec = tremain-tmin*60;
            TRem = sprintf('%i minutes and %.0f seconds',tmin,tsec);
        else
            TRem = sprintf('%.1f seconds',tremain);
        end
    end
    set(hbar,'pos',[10 30 max(0.1,floor(frac*220)) 20],'string',Str);
    set(UD.TRem,'string',['Time remaining: ',TRem]);
    set(Fg,'name',cat(2,UD.Title,Str2),'userdata',UD)
    pause(0)
    %drawnow
elseif Update
    set(Fg,'userdata',UD)
end

if nargout>0
    Fg0=Fg;
end

function dopause(obj,varargin)
waitfor(obj,'value',0)