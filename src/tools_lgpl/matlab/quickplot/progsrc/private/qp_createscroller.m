function qp_createscroller(fig)
%QP_CREATESCROLLER Create a QuickPlot animation scroller bar.

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

set(fig,'toolbar','figure')
Inactive = qp_settings('UIInActiveColor');

uic=uicontrol('Parent',fig, ...
    'FontUnits','pixels', ...
    'BackgroundColor',Inactive, ...
    'Enable','off', ...
    'FontSize',12, ...
    'Position',[1 1 100 15], ...
    'Style','slider', ...
    'Tag','animslid');
uislider(uic,'min',1,'max',2,'callback','d3d_qp slider')
uic2=uicontrol('Parent',fig, ...
    'FontUnits','pixels', ...
    'BackgroundColor',Inactive, ...
    'Callback','d3d_qp animpush', ...
    'Enable','on', ...
    'FontSize',12, ...
    'Position',[101 1 15 15], ...
    'String','v', ...
    'Style','pushbutton', ...
    'Tag','animpush');
uicontextmenu('tag','animpushuicontextmenu','Parent',fig)
set(uic2,'tooltip','Click to select item and dimension to animate');
