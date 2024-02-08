function matlab_sysinfo()
%MATLAB_SYSINFO Collect system information as visible from MATLAB.
%   Use a compiled version of this tool to identify potential issues when
%   running into issues with QUICKPLOT.

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

c = computer;
fprintf('----------------------------------------\n');
fprintf('Running on computer = %s\n',c);
fprintf('----------------------------------------\n\n');

fprintf('ctfroot = %s\n\n',ctfroot);

fprintf('matlabroot = %s\n\n',matlabroot);

path = getenv('PATH');
fprintf('getenv(PATH) = %s\n\n', path);

if strcmp(c(1:2),'PC')
   [status, result] = system('set PATH');
   fprintf('system(PATH) = %s\n\n', result);
   eql = strfind(result,'=');
   col = strfind(result,';');
   dd = strtrim(result(eql(1)+1:col(1)-1));
   fprintf('exeroot = %s\n\n', dd);
else
   fprintf('no exeroot defined!\n\n');
end

fprintf('prefdir = %s\n\n',qp_prefdir);

d = which('matlab_sysinfo.m');
fprintf('matlab_sysinfo.m => %s\n\n',d);

d = which('matlab_sysinfo.p');
fprintf('matlab_sysinfo.p => %s\n\n',d);

d = which('matlab_sysinfo.exe');
fprintf('matlab_sysinfo.exe => %s\n\n',d);

fprintf('----------------------------------------\n');
fprintf('Environment listing\n');
fprintf('----------------------------------------\n\n');

[stat,result] = system('set');
fprintf('%s\n',result);

fprintf('----------------------------------------\n');
fprintf('normal end of program\n');
fprintf('----------------------------------------\n\n');


function dd = qp_prefdir
c = computer;
if strcmp(c(1:2),'PC')
    % Try %UserProfile% first. This is defined by NT
    dd = getenv('UserProfile');
    if isempty(dd)
        % Try the windows registry next. Win95/98 uses this
        % if User Profiles are turned on (you can check this
        % in the "Passwords" control panel).
        dd = get_profile_dir;
        if isempty(dd)
            % This must be Win95/98 with user profiles off.
            dd = getenv('windir');
        end
    end
    dd = fullfile(dd, 'Application Data', 'Deltares', '');
else % Unix
    dd = fullfile(getenv('HOME'), '.Deltares', '');
end


function profileDir = get_profile_dir
le = lasterr;
try
    profileDir = winqueryreg('HKEY_CURRENT_USER',...
        'Software\Microsoft\Windows\CurrentVersion\ProfileReconciliation',...
        'ProfileDirectory');
catch
    lasterr(le);
    profileDir = '';
end