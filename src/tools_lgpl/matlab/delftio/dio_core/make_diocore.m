function make_diocore
%MAKE_DIOCORE  Build statements for dio_core.dll

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

fprintf('Initializing...\n');
cppfiles0 = dir('*.cpp');
cppfiles0 = {cppfiles0.name};

delftio_folder = '../../../../utils_lgpl/delftio/packages/delftio_shm/src/diof90/';
fprintf('Copying cpp files from %s ...\n', delftio_folder);
copycppfiles(delftio_folder)

fprintf('Compiling and linking...\n');
files = {'dio_core.cpp','dio_shm.cpp','dio_shm_datablock.cpp','dio_shm_f2c_c.cpp','dio_shm_handle.cpp','dio_shm_sync.cpp'};
%
% compile the diocore mex file
% add -v switch for verbose compilation process
%
mex('-DWIN32','-I../../../../utils_lgpl/delftio/packages/delftio_shm/include',files{:})
%
% Optionally include -R2018a switch to use new MX_HAS_INTERLEAVED_COMPLEX memory management
%
%mex('-R2018a','-DWIN32','-I../../../../utils_lgpl/delftio/packages/delftio_shm/include',files{:})

% wait a second ...
pause(1)

fprintf('Cleaning up ...\n');
cppfiles1=dir('*.cpp');
cppfiles1 = {cppfiles1.name};
cppfiles1 = setdiff(cppfiles1,cppfiles0);
for i = 1:length(cppfiles1)
    delete(cppfiles1{i})
end

installdir = '../progsrc/private';
fprintf('Installing library in %s ...\n',installdir);
if exist('dio_core.dll')
    movefile('dio_core.dll',installdir)
elseif exist('dio_core.mexw32')
    movefile('dio_core.mexw32',installdir)
elseif exist('dio_core.mexw64')
    movefile('dio_core.mexw64',installdir)
end

fprintf('Finished.\n');


function copycppfiles(srcpath)
if sscanf(version,'%f',1)<6
    srcpath = strrep(srcpath,'/',filesep);
    d = dir([srcpath '*.cpp']);
    for i = 1:length(d)
        copyfile([srcpath d(i).name],pwd)
    end
else
    copyfile([srcpath '*.cpp'],pwd)
end
