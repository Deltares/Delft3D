function [X,Succes]=vs_get(varargin)
%VS_GET Read one or more elements from a NEFIS file.
%   VS_GET uses the same syntax as VS_LET. See that function for
%   input/output details. The difference between VS_GET and VS_LET is the
%   treatment of group dimensions.
%
%   If a single element is read VS_LET returns a single numeric/character
%   array; the first dimension(s) correspond to the group dimensions, the
%   following dimension(s) correspond to the element dimensions. For the
%   same situation, VS_GET will return a cell array corresponding to the
%   group dimensions. Each cell will contian a numeric/character array of
%   which the size matches the element dimensions. If only a single group
%   index is read then the cell content will be returned directly (returned
%   array is identical to result of VS_LET with the group dimension(s)
%   squeezed out).
%
%   If multiple elements are read VS_LET returns a scalar structure with
%   fields containing all data (again both group and element dimensions
%   represented in a single array). VS_GET will return in such cases a
%   structure array. The structure dimensions correspond to the group
%   dimensions; the field dimensions only concern the element dimensions.
%
%   VS_LET is generally more appropriate for time-series, while VS_GET may
%   be easier if data is read from a single group index. If you sometimes
%   read a single group index and sometimes multiple then use VS_LET for
%   consistent results.
%
%   See also VS_USE, VS_DISP, VS_LET, VS_DIFF, VS_FIND, VS_TYPE.

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

if nargout==2
    [X,Succes]=vs_let('vsget',varargin{:});
else
    X=vs_let('vsget',varargin{:});
end
