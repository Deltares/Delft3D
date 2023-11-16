function varargout = weir(cmd,varargin)
%WEIR Read/write a weir file.
%   NOTE: This routine has been superseded by D3D_ATTRIB. It has been
%   included for backward compatibility.
%
%   W = WEIR('read',FILENAME) reads the specified file and returns a
%   structure W with fields depending on the content of the file.
%
%   WEIR('write',FILENAME,W) writes the data of structure W to a file with
%   name FILENAME. The structure should specify the file type and include
%   the fields specified for that particular file type indicated above.
%
%   See also: D3D_ATTRIB.

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
    if nargout>0
        varargout=cell(1,nargout);
    end
    return
end

switch cmd
    case 'read'
        Out = d3d_attrib(cmd,varargin{:});
        varargout={Out};
    otherwise
        Out = d3d_attrib(cmd,varargin{:});
        if nargout>0
            varargout{1} = Out;
        end
end
