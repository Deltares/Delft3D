function valOut = valuemap(valIn,MapIn,MapOut)
%VALUEMAP One-to-one mapping of enumerables.
%   OUT = VALUEMAP(IN,MAPIN,MAPOUT) maps the value IN as member of the
%   value list MAPIN to the corresponding value OUT in the value list
%   MAPOUT.
%
%   Example:
%       OUT = VALUEMAP(0,[1 0],{'on' 'off'})
%       return 'off'
%
%   See also: ISMEMBER, STRCMP.

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

if iscellstr(MapIn)
    selected = strcmp(valIn,MapIn);
else
    selected = MapIn==valIn;
end
empty = sum(selected)==0;
if empty
    selected = 1;
end   
if iscellstr(MapOut)
    valOut = MapOut{selected};
else
    valOut = MapOut(selected);
end
if empty
    valOut(1,:) = [];
    valOut = reshape(valOut,[0 0]);
end