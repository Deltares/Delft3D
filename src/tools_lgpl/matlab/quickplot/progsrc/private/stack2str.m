function str = stack2str(stack,basefunction)
%STACK2STR Convert exception stack into cell string.
%
%   CELLSTR = STACK2STR(STACK)
%   where STACK is a stack as obtained from MException.stack.
%
%   See also MException.

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

stacklen = length(stack);
if nargin==2
    for i = 1:stacklen
        if strcmp(stack(i).name,basefunction)
            stacklen = i;
            break
        end
    end
end
str = repmat({''},stacklen,1);
mpath = multiline(matlabpath,pathsep,'cell');
for i = 1:stacklen
    [p,f] = fileparts(stack(i).file);
    if ~strcmp(f,stack(i).name)
        fcn = sprintf('>%s',stack(i).name);
    else
        fcn = '';
    end
    z = zeros(size(mpath));
    for j = 1:length(mpath)
        if strncmp(p,mpath{j},length(mpath{j}))
            z(j) = length(mpath{j});
        end
    end
    [len,j] = max(z);
    p = p(len+1:end);
    if ~isempty(p) && isequal(p(1),filesep)
        p = p(2:end);
    end
    if ~isempty(p)
        p = [p filesep];
    end
    str{i} = sprintf('In %s%s%s at line %i',p,f,fcn,stack(i).line);
end
