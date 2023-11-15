function [reftime,step]=delwaqt0(str)
%DELWAQT0 Parse Delwaq T0 string.
%
%   [RefTime,TimeStep]=DELWAQT0(String) parses the provided string to
%   obtain a reference time RefTime and step TimeStep.

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

t0 = lower(str(1:3));
if strcmp(t0,'t0:') || strcmp(t0,'t0=')
    time = sscanf(str,'%*4c%4d%*c%2d%*c%2d%*c%2d%*c%2d%*c%2d%*7c%8d',7);
    reftime = datenum(time(1:6)');
    if length(time)==6
       step = 1;
    else
       step = time(7);
    end
    if step<0
        step = 1/step;
    end
    switch lower(str(39))
        case {'s',' '}
            unit = 1/(24*3600);
        case 'm'
            unit = 1/(24*60);
        case {'u','h'}
            unit = 1/24;
        case 'd'
            unit = 1;
        otherwise
            unit = NaN;
    end
    step = step * unit;
else
    reftime = datenum(1900,1,1,0,0,0);
    step = 1/(24*3600);
end
