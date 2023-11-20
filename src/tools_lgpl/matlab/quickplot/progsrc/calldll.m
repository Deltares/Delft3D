function varargout=calldll(varargin)
%CALLDLL Calls a DLL.

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

%
% R2009b doesn't like calling a DLL: suppress the warning.
%
warnstate=[];
try
    warnstate = warning('query','MATLAB:mex:deprecatedExtension');
    warning('off','MATLAB:mex:deprecatedExtension')
catch
end

e=[];
try
    if nargout==0
        feval(varargin{:});
    else
        [varargout{1:nargout}] = feval(varargin{:});
    end
catch e
    % continue
end

%
% Set warning back to original setting.
%
if ~isempty(warnstate)
    warning(warnstate);
end

if ~isempty(e)
    rethrow(e)
end
