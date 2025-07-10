function make_tests(basedir, varargin)
%MAKE_TESTS Build some test binaries

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


curdir = pwd;
addpath(curdir)
if ~exist('mcc')
    error('Cannot find MATLAB compiler. Use another MATLAB installation!')
end
if nargin>0
    cd(basedir);
end
err = [];
try
    if ~exist('testcodes','dir')
        error('Cannot locate source folder "testcodes".')
    end
    cd testcodes
    make_hello_world
    make_matlab_sysinfo
    make_graphics_test
catch err
end
if nargin>0
    cd(curdir);
end
rmpath(curdir)
if ~isempty(err)
    rethrow(err)
end
