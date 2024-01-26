function NormPos=getnormpos(fig)
%GETNORMPOS Select an area (using normalized units).
%
%   NormPos=GETNORMPOS(FigHandle)
%   if skipped FigHandle is replaced by GCF

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

if nargin==0
    fig=gcf;
end
tmpax=axes('parent',fig,'visible','off','units','normalized','position',[0 0 1 1]);
hvis=get(fig,'handlevisibility');
set(fig,'handlevisibility','on')
figure(fig);
[xd,yd,b] = ginput(1);
if isequal(b,27) %ESC pressed
    NormPos = [];
    return
end
Point1 = get(tmpax,'currentpoint');
Point1 = Point1(1,1:2);
%
units = get(fig,'units');
set(fig,'units','pixels')
rbbox([get(fig,'currentpoint') 0 0],get(fig,'currentpoint'));
Point2 = get(tmpax,'currentpoint');
Point2 = Point2(1,1:2);
%
delete(tmpax)
set(fig, ...
    'units',units, ...
    'handlevisibility',hvis)
%
LowerLeft=min(Point1,Point2);
UpperRight=max(Point1,Point2);
NormPos=[LowerLeft UpperRight-LowerLeft];
