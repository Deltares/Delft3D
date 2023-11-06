function folder = uigetfolder(title, initial_path)
%UIGETFOLDER   Standard Windows browse for folder dialog box.
%
%   folder = uigetfolder(title, initial_path)
%
%   Output: folder       = selected folder (empty string if dialog cancelled)
%   Inputs: title        = title string (OPTIONAL)
%           initial_path = initial path (OPTIONAL, defaults to PWD)
%
%   Examples:   folder = uigetfolder                          - default title and initial path
%               folder = uigetfolder('Select results folder') - default initial path
%               folder = uigetfolder([], 'C:\Program Files')  - default title
%
%   See also UIGETFILE, UIPUTFILE

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

%-----------------------------------------------------------------------------------------------
%#function uigetfolder_win32

if ~strcmp(computer, 'PCWIN')
    error_dialog_handle = errordlg(['The function ', upper(mfilename), ' only works on a MS-Windows PC'], ...
        mfilename, ...
        'modal');
    folder = '';
else
    if nargin < 2
        initial_path = pwd;
    end

    if nargin < 1 | isempty(title)
        title = 'Select a folder';
    end

    % Error checking
    if ~ischar(title)
        error('The title must be a string')
    end
    if ~ischar(initial_path)
        error('The initial path must be a string')
    end
    if ~exist(initial_path, 'dir')
        error(['The initial path: ', initial_path, ' does not exist!'])
    end

    folder = calldll('uigetfolder_win32',title, initial_path);

end
%-----------------------------------------------------------------------------------------------
