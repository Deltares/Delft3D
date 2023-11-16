function B = cast(A,NewClass)
% CAST  Cast a variable to a different data type or class.
%    Helper function existing in recent MATLAB versions. Included for
%    backward compatibility.

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

switch NewClass
    case 'int8'
        B = int8(A);
    case 'uint8'
        B = uint8(A);
    case 'int16'
        B = int16(A);
    case 'uint16'
        B = uint16(A);
    case 'int32'
        B = int32(A);
    case 'uint32'
        B = uint32(A);
    case 'single'
        B = single(A);
    case 'double'
        B = double(A);
    case 'logical'
        B = logical(A);
    case 'char'
        B = char(A);
end