function pathname = check_path(pathname)
%CHECK_PATH Checks if a path exists; tries to correct for non-ASCII errors.
%   CORR_PATH = CHECK_PATH(ORG_PATH) checks whether the specified path ORG_PATH
%   exists. When the path contains non-ASCII characters, the initial path may
%   be invalid, and the routine will search for an existing path that matches
%   the specified path except for the non-ASCII characters. The routine throws
%   an error if the path cannot be found, otherwise it returns the name of the
%   corrected path.
%
%   See also: EXIST.

%----- LGPL --------------------------------------------------------------------
%                                                                               
%   Copyright (C) 2011-2025 Stichting Deltares.                                     
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

pathparts = strsplit(pathname,filesep,'CollapseDelimiters',false);
if length(pathparts)>3 && isempty(pathparts{1}) && isempty(pathparts{2})
    % starts with two filesep, e.g. \\MACHINE\ROOT\FOLDERS\FILE
    base = [filesep filesep pathparts{3}];
    pathname = one_check(base,pathparts(4:end));
else
    % doesn't start with filesep, e.g. DRIVE:\FOLDERS\FILE
    base = [pathparts{1}];
    pathname = one_check(base,pathparts(2:end));
end

function pathname = one_check(base,pathparts)
if isempty(pathparts) % end of pathname reached
    pathname = base;
elseif isempty(pathparts{1}) % double filesep encountered
    pathname = one_check(base,pathparts(2:end));
else % at least one more part in the pathname
    newbase = [base filesep pathparts{1}];
    pathname = [];
    if any(pathparts{1}>127)
        % next filepart contains special characters
        d = dir(base);
        names = {d.name};
        pattern = pathparts{1};
        pattern(pattern>127) = '?';
        options = find(wildstrmatch(pattern,names));
        for i = 1:length(options)
            try
                pathname = one_check([base filesep names{options(i)}],pathparts(2:end));
                break
            catch
            end
        end
    elseif exist(newbase) %#ok<EXIST> % file or directory are both OK
        pathname = one_check(newbase,pathparts(2:end));
    end
    if isempty(pathname)
        spaces = repmat(' ',1,length(base)+1);
        error('Invalid file path:\n%s\n%s^',fullfile(base,pathparts{:}),spaces)
    end
end

