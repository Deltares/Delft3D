function make_mex
%MAKE_MEX Build all mex files from source
%   Builds on Linux
%     * exepath           (not on Windows)
%     * reducepoints      (all platforms)
%     * writeavi          (Windows only)
%     * CloseSplashScreen (Windows only)

%----- LGPL --------------------------------------------------------------------
%
%   Copyright (C) 2011-2024 Stichting Deltares.
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

cwd = pwd;

% not on Windows
if ~ispc
   compile('exepath', ...
       [cwd, filesep, 'progsrc', filesep, 'private'], ...
       'exepath.c')
end

% all platforms
compile('reducepoints', ...
    [cwd, filesep, 'progsrc', filesep, 'private'], ...
    'reducepoints.c')

% Windows only
if ispc
   compile('writeavi', ...
       [cwd, filesep, 'progsrc', filesep, 'private'], ...
       'writeavi.cpp', 'vfw32.lib', 'user32.lib')

   compile('CloseSplashScreen', ...
       [cwd, filesep, 'SplashScreen', filesep, 'finish'], ...
       'CloseSplashScreen.cpp', '-I..\include')
end


function compile(caseid, folder, varargin)
fprintf('##teamcity[testStarted name=''%s'']\n', caseid);
try
    cd(folder)
    mex(varargin{:})
    fprintf('##teamcity[testFinished name=''%s'']\n', caseid)
catch
    fprintf('##teamcity[testFailed name=''%s'' message=''case crashed.'']\n', caseid)
end
