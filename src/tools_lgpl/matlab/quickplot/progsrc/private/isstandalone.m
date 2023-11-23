function I = isstandalone
%ISSTANDALONE Determines stand alone execution.
%   I = ISSTANDALONE
%   returns 1 if the program is executed in stand
%   alone (compiled) mode and 0 otherwise.
%
%   See also ISRUNTIME.

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

I=0;
%
% if this is not MATLAB then it cannot be standalone/deployed.
%
if ~isenvironment('MATLAB')
   return
end

lastmsg = lasterr;
try
    %
    % Standard funciton in release 14
    %
    I=isdeployed;
catch
    %
    % Older MATLAB versions do not have that function ...
    % but cannot compile a statement like WHOS, so use that
    % as check.
    %
    lasterr(lastmsg);
    try
        X=whos;
    catch
        lasterr(lastmsg);
        I=1;
    end
end
