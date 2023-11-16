function make_quickplot(basedir,varargin)
%MAKE_QUICKPLOT Compile QUICKPLOT executable
%   Compile MATLAB code to QUICKPLOT executable
%
%   MAKE_QUICKPLOT(BASEDIR)
%   Use specified directory instead of current directory as base directory

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

curdir = pwd;
addpath(curdir)
if matlabversionnumber<7.09
    error('Invalid MATLAB version. Use MATLAB R2009b (7.9) or higher for compiling Delft3D-QUICKPLOT!')
end
if ~exist('mcc')
    error('Cannot find MATLAB compiler. Use another MATLAB installation!')
end
if nargin>0
    cd(basedir);
end
err = [];
try
    localmake(varargin{:});
catch err
end
if nargin>0
    cd(curdir);
end
rmpath(curdir)
if ~isempty(err)
    rethrow(err)
end


function localmake(qpversion,repo_url,hash,T)
if ~exist('progsrc','dir')
    error('Cannot locate source folder "progsrc".')
end
sourcedir=[pwd,filesep,'progsrc'];

if strncmp(fliplr(computer),'46',2)
    tdir = 'quickplot64';
else
    tdir = 'quickplot32';
end
targetname = 'Delft3D-QUICKPLOT';
targetdir = [pwd,filesep,tdir];

if ~exist(targetdir, 'dir')
    fprintf('Creating %s directory ...\n', tdir);
    mkdir(tdir);
end
cd(tdir)
diary make_quickplot_diary

fprintf('Copying files ...\n');
if isunix
    unix('cp -rf ../progsrc/* .');
    unix('mv compileonly/* .');
else
    [s, msg] = dos('xcopy "..\progsrc\*.*" "." /E /Y');
    if s == 0
        [s, msg] = dos('move compileonly\*.*  .');
    end
    if s ~= 0
        error(msg)
    end
end

fprintf('Including netCDF files ...\n');
copyfile('../../../../third_party_open/netcdf/matlab/netcdfAll-4.1.jar','.')
addpath ../../../../third_party_open/netcdf/matlab/mexnc
addpath ../../../../third_party_open/netcdf/matlab/snctools

if nargin<4
    [qpversion,hash,repo_url] = read_identification(sourcedir, 'd3d_qp.m');
    T = now;
end

TStr = datestr(T);
fprintf('\nBuilding %s version %s\n\n', targetname, qpversion);
fprintf('Current date and time           : %s\n', TStr);

fprintf('Modifying files ...\n');
fstrrep([targetdir,filesep,'d3d_qp.m'], '<VERSION>', qpversion)
fstrrep([targetdir,filesep,'d3d_qp.m'], '<CREATIONDATE>', TStr)
fstrrep([targetdir,filesep,'d3d_qp.m'], '<GITHASH>', hash)
fstrrep([targetdir,filesep,'d3d_qp.m'], '<GITREPO>', repo_url)
fstrrep([targetdir,filesep,'wl_identification.c'], '<VERSION>', qpversion)
fstrrep([targetdir,filesep,'wl_identification.c'], '<CREATIONDATE>', TStr)
if ~d3d_qp('version',qpversion,repo_url,hash,TStr)
    error('Unable to write correct d3d_qp.version file.')
end

fprintf('Include GhostScript for printing ...\n');
g = which('-all','gscript');
if ~isempty(g)
    copyfile(g{1},'.')
end

fprintf('Building executable ...\n');
make_exe
if ispc
   movefile('d3d_qp.exe','d3d_qp.exec');
end

fprintf('Cleaning up directory ...\n');
X = {'*.asv'
    '*.bak'
    '*.m'
    '*.c'
    '*.cpp'
    '*.h'
    '*.o'
    '*.obj'
    '*.a'
    '*.lib'
    '*.scc'
    'private'
    'compileonly'
    '@qp_data'
    '@qp_data_resource'};
if isunix
    X = cat(1,X,{'*.dll'
        '*.mexw*'});
else
    X = cat(1,X,{'*.mexglx'
        '*.mexa64'
        '*.exp'});
end
cleanup(X)
diary off
cd ..
fprintf('Finished.\n');
