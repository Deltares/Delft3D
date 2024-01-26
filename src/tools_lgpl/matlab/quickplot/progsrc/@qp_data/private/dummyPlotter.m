function h = dummyPlotter(Parent,Data,varargin)
%dummyPlotter Example plot function.

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

PO = initPlotOption;
PO = addPlotOption(PO,'Color','color',[0 0 1]);
PO = addPlotOption(PO,'LineStyle','list','-', ...
    {'none','-','--',':','.-'});
PO = addPlotOption(PO,'Marker','list','none', ...
    {'none','+','o','*','.','x','s','d','v','^','>','<','p','h'});

if isequal(Parent,'getoptions')
    h = PO;
    return
else
    Ops = expandPlotOptions(PO,varargin{:});
end

h = line([0 1],[0 1], ...
    'parent',Parent, ...
    'color',Ops.Color, ...
    'linestyle',Ops.LineStyle, ...
    'marker',Ops.Marker);
