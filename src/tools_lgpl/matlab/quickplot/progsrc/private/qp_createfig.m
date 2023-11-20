function [fig,figoptions,createops]=qp_createfig(figtype,figname)
%QP_CREATEFIG Create a figure for plotting.
%   set default values

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

fig=[];
figoptions.ProgID='QuickPlot';
figoptions.Editable=1;

% which kind of figure should be created?
labels={'free format figure', ...
    '1 plot - portrait', ...
    '1 plot - landscape', ...
    '2 plots, vertical - portrait', ...
    '2 plots, horizontal - portrait', ...
    '2 plots, vertical - landscape', ...
    '2 plots, horizontal - landscape', ...
    '3 plots, vertical - portrait', ...
    '3 plots, horizontal - landscape', ...
    '4 plots, 2x2 - portrait', ...
    '4 plots, 2x2 - landscape'};
if nargin>0
    if ~isequal(figtype,'quick')
        ifigtype=ustrcmpi(figtype,labels);
        if ifigtype<0
            error('Unknown figure type: %s.',figtype)
        end
        if nargin==1
            figname=figtype;
        end
    end
else
    [figtype,figname]=ui_typeandname(labels);
end

createops = {figtype figname};
if isempty(figtype) % cancel pressed?
    return
end

% create figure
xtraprops={};
if ~isempty(figname)
    xtraprops={'integerhandle','off', ...
        'numbertitle','off', ...
        'name',figname};
end
%
fig = [];
if isequal(figtype,'quick')
    deffig = qp_settings('defaultfigure','');
    if ~isempty(deffig)
        try
            fig=hgload(deffig);
            set(fig,'menubar','none','closerequestfcn','d3d_qp closefigure')
            qp_figurebars(fig)
            %
            set(fig,'name','qp_reset_name','numbertitle','on','integerhandle','on')
            fig=findall(0,'name','qp_reset_name');
            set(fig,'name','')
        catch
            fig=[];
        end
    end
end
%
if isempty(fig)
    Renderers = set(0, 'defaultfigurerenderer');
    if ismember('zbuffer', Renderers)
       renderer = 'zbuffer';
    else
       renderer = 'opengl';
    end
    usr_renderer = qp_settings('defaultrenderer', renderer);
    if ismember(usr_renderer, Renderers)
        renderer = usr_renderer;
    end
    fig=figure('closerequestfcn','d3d_qp closefigure', ...
        'inverthardcopy','off', ...
        'tag','QuickPlot - figure', ...
        'color',qp_settings('defaultfigurecolor')/255, ...
        'renderer',renderer, ...
        'visible','off', ...
        'userdata',figoptions, ...
        xtraprops{:});
    try
        usr_smooth = qp_settings('graphicssmoothing');
        set(fig, 'graphicssmoothing', usr_smooth)
    catch
    end
    qp_figurebars(fig)
end

dfp = qp_settings('defaultfigurepos');
if isnumeric(dfp)
    % manual
    setpixels(fig,'position',dfp)
    MonPos = [];
elseif strcmp(dfp,'qp main')
    % same as QUICKPLOT main window
    mfig=findobj(allchild(0),'flat','tag','Delft3D-QUICKPLOT');
    MonPos = qp_getscreen(mfig);
elseif length(dfp)>8 && strcmp(dfp(1:8),'monitor ')
    screen = sscanf(dfp(9:end),'%i',1);
    MonPos = qp_getscreen('screen',screen);
else % strcmp(dfp,'auto') or unknown
    % nothing to do: keep default
    MonPos = [];
end
if size(MonPos,1)==1
    width = min(MonPos(3),560);
    height = min(MonPos(4),420);
    dfp = [MonPos(1)+floor((MonPos(3)-width)/2) MonPos(2)+max(MonPos(4)-102-height,0) width height];
    setpixels(fig,'position',dfp)
end

uic=findobj(fig,'Tag','animslid');
if isempty(uic)
    qp_createscroller(fig)
end

switch(figtype)
    case '1 plot - portrait'
        figoptions.Editable=0;
        standardfig(1,1,{'plot area'},'a4p');
    case '1 plot - landscape'
        figoptions.Editable=0;
        standardfig(1,1,{'plot area'},'a4l');
    case '2 plots, vertical - portrait'
        figoptions.Editable=0;
        standardfig(2,1,{'upper plot','lower plot'},'a4p');
    case '2 plots, horizontal - portrait'
        figoptions.Editable=0;
        standardfig(1,2,{'left plot','right plot'},'a4p');
    case '2 plots, vertical - landscape'
        figoptions.Editable=0;
        standardfig(2,1,{'upper plot','lower plot'},'a4l');
    case '2 plots, horizontal - landscape'
        figoptions.Editable=0;
        standardfig(1,2,{'left plot','right plot'},'a4l');
    case '3 plots, vertical - portrait'
        figoptions.Editable=0;
        standardfig(3,1,{'upper plot','middle plot','lower plot'},'a4p');
    case '3 plots, horizontal - landscape'
        figoptions.Editable=0;
        standardfig(1,3,{'left plot','center plot','right plot'},'a4l');
    case '4 plots, 2x2 - portrait'
        figoptions.Editable=0;
        standardfig(2,2,{'upper left plot','upper right plot','lower left plot','lower right plot'},'a4p');
    case '4 plots, 2x2 - landscape'
        figoptions.Editable=0;
        standardfig(2,2,{'upper left plot','upper right plot','lower left plot','lower right plot'},'a4l');
    case 'free format figure'
        set(fig, ...
            'papertype','a4', ...
            'paperpositionmode','auto');
        set(findall(fig,'tag','editborder'),'visible','off')
    case 'quick'
%         set(fig, ...
%             'papertype','a4', ...
%             'paperpositionmode','auto');
        set(fig,'inverthardcopy','on');
        set(findall(fig,'tag','editborder'),'visible','off')
    otherwise
        delete(fig);
        fig=[];
        Str=sprintf('Requested figure type not yet implemented.');
        uiwait(msgbox(Str,'modal'));
end
if strcmp(get(fig,'paperpositionmode'),'manual') && ~strcmp(figtype,'quick')
    qp_figaspect(fig)
end
set(fig,'userdata',figoptions,'visible','on');


function standardfig(m,n,tags,orient)
%set(gcf,'color',[1 1 1]);
for i = m*n:-1:1
    ax = subplot(m,n,i);
    qp_defaultaxessettings(ax)
    set(ax,'tag',tags{i});
end
md_paper('no edit',orient,'7box');

function val = getpixels(h,quant)
u = get(h,'units');
set(h,'units','pixels')
val = get(h,quant);
set(h,'units',u)

function setpixels(h,quant,val)
u = get(h,'units');
set(h,'units','pixels')
set(h,quant,val)
set(h,'units',u)
