function screensize = qp_getscreen(ref,pos)
%QP_GETSCREEN Get position of current screen

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

MonPos = getpixels(0,'MonitorPositions');
if isa(handle(0),'matlab.ui.Root') %HG2
    % [XLL YLL W H] - Y positive up
    MonPos(:,3:4) = MonPos(:,1:2)+MonPos(:,3:4)-1;
else
    % [XLL YLL XUR YUR] - Y positive down
    shift = -min(MonPos(:,2))+1;
    MonPos(:,[2 4]) = MonPos(:,[2 4])+shift;
    MonPos(:,[4 2]) = max(MonPos(:,4)) - MonPos(:,[2 4])+1;
end

if nargin==2
    % screen number directly specified using pos
    if pos>size(MonPos,1)
        pos = 1;
    end
elseif nargin==1
    if isequal(ref,'pointer')
        pos = [get(0,'pointerlocation') 1 1];
    elseif isequal(size(ref),[1 4])
        pos = ref;
    elseif isequal(size(ref),[1 1]) && ishandle(ref) && strcmp(get(ref,'type'),'figure')
        pos = getpixels(ref,'position');
    else
        error('Invalid reference given for qp_getscreen.')
    end
else
    gcf = get(0,'currentFigure');
    qpf = findobj(allchild(0),'flat','tag','Delft3D-QUICKPLOT');
    if ~isempty(gcbf)
        pos = getpixels(gcbf,'position');
    elseif ~isempty(qpf)
        pos = getpixels(qpf,'position');
    elseif ~isempty(gcf)
        pos = getpixels(gcf,'position');
    else
        pos = [get(0,'pointerlocation') 1 1];
    end
end
%
if length(pos)==1
   scr = pos;
else
   posx = pos;
   posx(3:4) = posx(1:2)+posx(3:4);
   %
   posint = [max(repmat(posx(1:2),size(MonPos,1),1),MonPos(:,1:2)) min(repmat(posx(3:4),size(MonPos,1),1),MonPos(:,3:4))];
   area = prod(max(0,posint(:,3:4)-posint(:,1:2)),2);
   [mxarea,scr] = max(area);
end
screensize = MonPos(scr,:);
screensize(3:4) = screensize(3:4)-screensize(1:2)+1;
   
function val = getpixels(h,quant)
u = get(h,'units');
set(h,'units','pixels')
val = get(h,quant);
set(h,'units',u) 