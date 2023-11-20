function PSout = plotstyles(A,ax)
%PLOTSTYLES Obtain plot styles for selected object.

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

PS = initPlotStyle;
switch A.ValType
    case 'none'
        PS = plotstyles_none(PS,A);
    case {'float','level'}
        PS = plotstyles_float(PS,A);
    case {'vector(xy)','vector(ij)','vector(xyz)','vector(ijk)'}
        PS = plotstyles_vectorxy(PS,A);
    case 'string'
        error('TODO plotoptions string')
    case 'logical'
        PS = plotstyles_logical(PS,A);
    case 'discrete'
        error('TODO plotoptions discrete')
    otherwise
        error('TODO plotoptions otherwise')
end

if nargout==0
    if isempty(PS)
        fprintf('No plot styles defined.\n');
    else
        fprintf('Available plot styles:\n');
        fprintf('   %s\n',PS.Name);
    end
else
    PSout = PS;
end
