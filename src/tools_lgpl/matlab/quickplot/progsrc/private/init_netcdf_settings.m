function init_netcdf_settings
%INIT_NETCDF_SETTINGS Check and set netCDF settings as needed.

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

mlock
persistent run_once
if ~isempty(run_once)
    return
end

run_once = 1;
matlab_netcdf_path = qp_basedir('exe');
if isstandalone
    try
        % Insert a try-catch block here since the setpref command sometimes fails on a write error to matlabprefs.mat.
        setpref('SNCTOOLS','USE_JAVA',true);
    catch
        ui_message('message','Failed to persist preferences during initialization.')
    end
else
    up = [filesep, '..'];
    check_dirs = {...
        [matlab_netcdf_path, filesep, 'netcdf'] ... % new distribution
        [matlab_netcdf_path, up, up, up, up, filesep, 'third_party_open', filesep, 'netcdf', filesep, 'matlab'] ... % Delft3D source tree checkout
        [matlab_netcdf_path, up, up, filesep, 'io', filesep, 'netcdf'] ... % open earth tools checkout
        };
    matlab_netcdf_path = '';
    for i = 1:length(check_dirs)
        if exist(check_dirs{i}, 'dir')
            matlab_netcdf_path = check_dirs{i};
            break
        end
    end

    % if nc_info can be found we assume that the settings were
    % preconfigured correctly either during a previous start of d3d_qp, or
    % via oetsettings, or by the user
    p = which('nc_info');
    if isempty(p)
        if ~isempty(matlab_netcdf_path)
            addpath([matlab_netcdf_path, filesep, 'mexnc'])
            addpath([matlab_netcdf_path, filesep, 'snctools'])
        end

        % check if nc_info can now be found ...
        p = which('nc_info');
        if isempty(p)
            ui_message('message','Unable to locate mexnc and snctools for accessing netCDF files.')
        end
    end
end
if ~isempty(matlab_netcdf_path)
    try
        javaaddpath([matlab_netcdf_path, filesep, 'netcdfAll-4.1.jar'])
    catch
    end
end