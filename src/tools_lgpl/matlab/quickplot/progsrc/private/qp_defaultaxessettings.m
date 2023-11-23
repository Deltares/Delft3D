function qp_defaultaxessettings(ax)
%QP_DEFAULTAXESSETTINGS Set axes preferences for plot axes.

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

set(ax,'layer','top', ...
       'color',qp_settings('defaultaxescolor')/255);
if qp_settings('boundingbox')
    set(ax,'box','on');
end
if matlabversionnumber>=8.04
    set(ax,'sortMethod','childOrder');
else
    set(ax,'drawmode','fast');
end
if matlabversionnumber >= 9
    for i = 1:length(ax)
        axi = ax(i);
        if isa(axi,'double')
            axi = handle(axi);
        end
        if matlabversionnumber >= 9.10
            axi.XAxis.LimitsChangedFcn = @qp_updateaxes;
            axi.YAxis.LimitsChangedFcn = @qp_updateaxes;
        elseif matlabversionnumber >= 9
            addlistener(axi,'XLim','PostSet',@qp_updateaxes);
            addlistener(axi,'YLim','PostSet',@qp_updateaxes);
        end
    end
end