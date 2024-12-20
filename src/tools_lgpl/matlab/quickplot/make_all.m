function make_all(release)
%MAKE_ALL Build various tools based on the QUICKPLOT source
%   Builds
%     * Delft3D-MATLAB interface
%     * QUICKPLOT
%     * ECOPLOT
%     * SIM2UGRID
%   all with exactly the same version number.

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

if ~license('checkout','compiler')
    try
        if batchStartupOptionUsed
            fprintf('##teamcity[buildStop comment=''Compiler license currently not available.'' readdToQueue=''true'']\n');
            return
        end
    end
    error('Compiler license currently not available.')
end
curdir = pwd;
sourcedir = [curdir,filesep,'progsrc'];

[qpversion,hash,repo_url] = get_qpversion(sourcedir,'d3d_qp.m');
T = now;

if nargin == 0
    Tvec = datevec(T);
    yr = Tvec(1);
    mn = Tvec(2);
    release = sprintf('Build %d.%2.2d',yr,mn);
end

if strcmp(computer,'PCWIN64')
   make_d3dmatlab(curdir,'version',qpversion,'url',repo_url,'hash',hash,'time',T,'release',release)
end
make_quickplot(curdir,qpversion,repo_url,hash,T)
make_ecoplot(curdir,qpversion,repo_url,hash,T)
make_delwaq2raster(curdir,qpversion,repo_url,hash,T)
make_sim2ugrid(curdir,qpversion,repo_url,hash,T)