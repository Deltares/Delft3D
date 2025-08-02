function make_d3dmatlab(basedir,varargin)
%MAKE_D3DMATLAB Prepare Delft3D-MATLAB toolbox for distribution
%   Prepare the files for distribution as Delft3D-MATLAB toolbox.
%
%   MAKE_D3DMATLAB(BASEDIR)
%   Use specified directory instead of current directory as base directory

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
%
% comment lines for
% consistency with
% make_quickplot and
% make_ecoplot
%
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


function localmake(varargin)

if ~exist('progsrc','dir')
    error('Cannot locate source folder "progsrc".')
end
sourcedir=[pwd,filesep,'progsrc'];

% set defaults
[qpversion,hash,repo_url] = get_qpversion;
T = now;

% overrule using input arguments
for i = 1:2:length(varargin)
    switch (varargin{i})
        case 'version'
            qpversion = varargin{i+1};
        case 'url'
            repo_url = varargin{i+1};
        case 'hash'
            hash = varargin{i+1};
        case 'time'
            T = varargin{i+1};
        case 'release'
            release = varargin{i+1};
        otherwise
            error('Unknown argument: %s',varargin{i})
    end
end


tdir = 'delft3d_matlab';
targetname = 'Delft3D-MATLAB interface';
targetdir = [pwd,filesep,tdir];

if ~exist(targetdir, 'dir')
    fprintf('Creating %s directory ...\n', tdir);
    mkdir(tdir);
end
cd(tdir)
% diary make_quickplot_diary % no diary to avoid clutter in the distribution folder ...

fprintf('Copying files ...\n');
exportsrc(sourcedir,targetdir)

fprintf('Including netCDF files ...\n');
if ~exist('netcdf','dir')
    mkdir('netcdf');
end
copyfile('../../../../third_party_open/netcdf/matlab/netcdfAll-4.1.jar','netcdf')
if ~exist('netcdf/mexnc','dir')
    mkdir('netcdf/mexnc');
    exportsrc('../../../../third_party_open/netcdf/matlab/mexnc', 'netcdf/mexnc')
end
if ~exist('netcdf/snctools','dir')
    mkdir('netcdf/snctools');
    exportsrc('../../../../third_party_open/netcdf/matlab/snctools', 'netcdf/snctools')
end

% strip off the platform flag (binaries for Windows and Linux are included)
% ... but don't strip of (changed)
% ... the platform is the last string between brackets
brackets = strfind(qpversion,'(');
qpversion = qpversion(1:max(brackets)-2);
% for the progress statement add the platform statement
qpversion_ = [qpversion, ' (all platforms)'];

DateStr = datestr(floor(T));
DateTimeStr = datestr(T);
fprintf('\nBuilding %s version %s\n\n', targetname, qpversion_);
fprintf('Current date and time           : %s\n', DateTimeStr);

fprintf('Modifying files ...\n');
fstrrep([targetdir,filesep,'d3d_qp.m'], '<VERSION>', qpversion)
fstrrep([targetdir,filesep,'d3d_qp.m'], '<CREATIONDATE>', DateTimeStr)
fstrrep([targetdir,filesep,'d3d_qp.m'], '<GITHASH>', hash)
fstrrep([targetdir,filesep,'d3d_qp.m'], '<GITREPO>', repo_url)
fstrrep([targetdir,filesep,'Contents.m'], '<VERSION>', qpversion)
fstrrep([targetdir,filesep,'Contents.m'], '<RELEASE>', release)
fstrrep([targetdir,filesep,'Contents.m'], '<CREATIONDATE>', DateStr) % MATLAB toolboxes don't have a time stamp

fprintf('Stripping files ...\n');
HeadURL_str = ['Source ', repo_url, ': ', hash];
Id_str = ['Release ', release, ': ', DateTimeStr];
svnstripfile(targetdir, HeadURL_str, Id_str)

%fprintf('Pcoding files ...\n');
%pmfile('dir',targetdir,targetdir,'-verbose')

fprintf('Cleaning up directory ...\n');
X = {'*.asv'
    '*.bak'
    '*.scc'
    'bin'
    'compileonly'};
cleanup(X)

fprintf('Removing unneeded subdirectories ...\n');
X = {'org'};
cleanup(X)
cd ..
fprintf('Finished.\n');


function exportsrc(sourcedir,targetdir)
d = dir(sourcedir);
for i = 1:length(d)
    source = [sourcedir filesep d(i).name];
    target = [targetdir filesep d(i).name];
    if d(i).isdir
        switch d(i).name
            case {'.','..','.svn'}
                % skip
            otherwise
                mkdir(target);
                exportsrc(source,target)
        end
    else
        copyfile(source,target)
    end
end