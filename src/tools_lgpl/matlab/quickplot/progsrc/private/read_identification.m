function [version,hash,repo_url] = read_identification(sourcedir,file)
%READ_INDENTIFICATION determine version number
%   Read the version identification string from the specified file

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

%
% Find the "%VERSION = <VERSION>" line in the specified file.
%
fid = fopen([sourcedir filesep file],'r');
if fid > 0
    str = fgetl(fid);
    while isempty(strmatch('%VERSION =',str))
        str = fgetl(fid);
    end
    fclose(fid);
    %
    % Obtain the version number from the string.
    %
    baseversion = deblank(str(11:end));
    %
    % Determine the latest revision.
    %
    [revstring,repo_url,hash] = determine_revision(sourcedir);
    %
    % Combine version and revision to file version string.
    %
    [a,b] = strtok(baseversion);
    version = sprintf('%s.%s%s',a,revstring,b);
else
    version = 'unknown version';
    repo_url = 'unknown';
    hash = 'unknown';
end
%
% Append platform identifier
%
switch computer
    case 'PCWIN'
        platform = 'Windows 32bit';
    case 'GLNX86'
        platform = 'Linux 32bit';
    case 'PCWIN64'
        platform = 'Windows 64bit';
    case 'GLNXA64'
        platform = 'Linux 64bit';
    case 'MACI64'
        platform = 'Apple 64bit';
    otherwise
        platform = computer;
end
version=sprintf('%s (%s)',version,platform);
%
% Done.
%