function update_option_positions(UD,wndw,newTop)
%UPDATE_OPTION_POSITIONS Update vertical position of plot option controls.

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

switch wndw
    case 'main'
        Options = UD.Options;
    case 'plmn'
        Options = UD.PlotMngr.Options;
end

Act = ~strcmp(get(Options.Handles,'enable'),'off') | qp_settings('showinactiveopt');  % <- for debugging useful: show all options
set(Options.Handles(~Act),'visible','off')
ActHandles = Options.Handles(Act);
P = get(ActHandles,{'position'});
if isempty(P)
    P = zeros(0,4);
else
    P = cat(1,P{:});
end

old_offset = get(Options.Slider,'userdata');
if nargin>2
    %
    % called in case of a Plot Options Figure Resize or Dock Command
    %
    P(:,2) = P(:,2)-Options.Top+newTop;
    Options.Top = newTop;
else
    %
    % called from qp_interface_update_options
    %
    if ~isempty(P)
        [f,i,j]=unique(Options.Line(Act));
        P(:,2)=Options.Top-25*(j(1)-j);
        old_offset = 0;
    end
end
%
if isempty(P)
    minP = 999;
else
    minP = min(P(:,2))+old_offset;
end
%
if minP<10
    offset=get(Options.Slider,'value');
    offset=min(max(offset,minP-10),0);
    set(Options.Slider,'min',minP-10,'max',0,'value',offset,'enable','on','userdata',offset);
else
    offset=0;
    set(Options.Slider,'enable','off','value',0,'userdata',0)
end
%
P(:,2)=P(:,2)-offset+old_offset;
P=num2cell(P,2);
set(Options.Handles(Act)',{'position'},P,'visible','on');
%
switch wndw
    case 'main'
        UD.Options = Options;
    case 'plmn'
        UD.PlotMngr.Options = Options;
end
setappdata(UD.MainWin.Fig,'QPHandles',UD)
