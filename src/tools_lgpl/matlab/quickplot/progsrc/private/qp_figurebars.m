function qp_figurebars(fig)
%QP_FIGUREBARS Update menu and toolbars for QuickPlot figures.

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

m1=qp_uimenu(fig,'&File', ...
    {'newfigure','&New Figure...',1,1,0,'N'
    'openfigure','&Open Figure...',1,1,0,'O'
    'savefigure','&Save Figure',1,1,1,'S'
    'saveasfigure','Save Figure &As...',1,1,0,''
    'printfigure','&Print/Export...',1,1,1,'P'
    'closefigure','&Close Figure',1,1,1,'C'});
set(m1(1),'tag','QuickPlotFile')

m1=qp_uimenu(fig,'&Edit', ...
    {'clipbitmap','&Bitmap to Clipboard',1,1,0,'B'
    'clipmeta','Windows &Metafile to Clipboard',1,1,0,'M'
    'editborder','Edit &Border',1,1,1,''});
set(m1(1),'tag','QuickPlotEdit')
if isunix
    set(findall(m1(1),'tag','clipbitmap'),'visible','off')
    set(findall(m1(1),'tag','clipmeta'),'visible','off')
end

set(fig,'menubar','none','toolbar','figure')
m1 = findall(fig,'type','uitoolbar');
sm1=qp_toolbarpush(m1,'startanim',1,'Start Animation');
sm2=qp_toolbarpush(m1,'stopanim',0,'Stop Animation');
set(sm2,'enable','off')
uitoggletool('Parent',m1,'Tag','ScribeSelectToolBtn','visible','off')

f=findall(m1,'clickedcallback','filemenufcn(gcbf,''FileOpen'')');
set(f,'cdata',qp_icon('openfigure','nan'));
f=findall(m1,'clickedcallback','filemenufcn(gcbf,''FileSave'')');
set(f,'cdata',qp_icon('savefigure','nan'),'clickedcallback','d3d_qp savefigure')
%m1=uimenu(fig,'label','&Animation','tag','QuickPlotAnimate');
%sm1=uimenu(m1,'label','St&art','callback','d3d_qp startanim','Accelerator','A');
%sm2=uimenu(m1,'label','&Stop','callback','d3d_qp stopanim','Accelerator','H');

if matlabversionnumber >= 9
    % placing ActionPostCallback on zoom and pan doesn't work when zooming and panning without first activating the zoom option.
    % h = zoom(fig);
    % h.ActionPostCallback = @(o,e)fprintf('check!\n');
    % h = pan(fig);   
    % h.ActionPostCallback = @(o,e)fprintf('check too!\n');
else
    %----------------------------------------------------------------------
    fMZO=findall(m1,'tag','figToolRotate3D');
    if isempty(fMZO) % MATLAB 7
        fMZO=findall(m1,'tag','Exploration.Rotate');
    end
    set(fMZO,'clickedcallback','d3d_qp rotate3d');
    %----------------------------------------------------------------------
    fMZO=findall(m1,'tag','figToolZoomIn');
    if isempty(fMZO) % MATLAB 7
        fMZO=findall(m1,'tag','Exploration.ZoomIn');
    end
    set(fMZO,'clickedcallback','d3d_qp zoomin');
    %----------------------------------------------------------------------
    fMZO=findall(m1,'tag','figToolZoomOut');
    if isempty(fMZO) % MATLAB 7
        fMZO=findall(m1,'tag','Exploration.ZoomOut');
    end
    set(fMZO,'clickedcallback','d3d_qp zoomout');
    %----------------------------------------------------------------------
    fMZO=findall(m1,'tag','Exploration.Pan');
    set(fMZO,'clickedcallback','d3d_qp pan');
end
%--------------------------------------------------------------------------
fMZO=findall(m1,'clickedcallback','figure');
if isempty(fMZO) % MATLAB 7
    fMZO=findall(m1,'tag','Standard.NewFigure');
end
set(fMZO,'clickedcallback','d3d_qp newfigure');
%--------------------------------------------------------------------------
fMZO=findall(m1,'clickedcallback','print');
if isempty(fMZO) % MATLAB 7
    fMZO=findall(m1,'tag','Standard.PrintFigure');
end
set(fMZO,'clickedcallback','d3d_qp printfigure');
%--------------------------------------------------------------------------
off(m1,'Exploration.DataCursor')
off(m1,'Annotation.InsertLegend')
off(m1,'Annotation.InsertColorbar')
off(m1,'DataManager.Linking')
off(m1,'Exploration.Brushing')
%--------------------------------------------------------------------------

function off(m1,tag)
set(findall(m1,'tag',tag),'visible','off','separator','off')
