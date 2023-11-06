function file = absfullfile(varargin)
%ABSFULLFILE Build full absolute file name from parts.
%   F = ABSFULLFILE(FOLDERNAME1, FOLDERNAME2, ..., FILENAME) uses the
%   standard MATLAB function FULLFILE to build a full file specification F
%   from the folders and file name specified. In addition to FULLFILE,
%   ABSFULLFILE will verify whether the resulting file specification F is
%   an absolute path. If not, it will prepend the current path PWD.
%
%   See also FULLFILE, PWD.

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

file = varargin{1};

% verify whether the first argument is an absolute location

% option 1: Linux local or UNC network location starting with / or //
if isequal(file(1),'/')
    % fullfile converts / and \ to filesep, make sure to use Linux-style / in result
    file = strrep(fullfile(varargin{:}),filesep,'/');
    return
end

% option 2: Windows UNC location starting with \\
if length(file)>1 && isequal(file(1:2),'\\')
    % fullfile converts / and \ to filesep, make sure to use Windows-style \ in result
    file = strrep(fullfile(varargin{:}),filesep,'\');
    return
end

% Optioen 3: Windows drive location starting with .: or webservice
cl = strfind(file,':');
if ~isempty(cl)
    % assume OK d:\folder\file or http://server/folder/file
    if isequal(cl,2)
        % absolute location starting with drive-letter a:-z:
        % fullfile converts / and \ to filesep, make sure to use Windows-style \ in result
        file = strrep(fullfile(varargin{:}),filesep,'\');
    else
        % absolute location starting with non-drive-letter:, typically webserver locations.
        % fullfile converts / and \ to filesep, make sure to use Linux-style / in result
        file = strrep(fullfile(varargin{:}),filesep,'/');    end
    return
end

% the first entry is not recognized as an absolute location
% so prepend pwd to create an absolute path
file = fullfile(pwd,varargin{:});
