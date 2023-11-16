function [version,hash,repo_url] = read_identification(sourcedir,file)
%READ_INDENTIFICATION determine version number
%   Read the version identification string from the specified file

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

%
% Find the "%VERSION = <VERSION>" line in the specified file.
%
fid = fopen([sourcedir filesep file],'r');
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
%
% Append 32 or 64 bit flag
%
if strncmp(fliplr(computer),'46',2)
    version=[version ' (64bit)'];
else
    version=[version ' (32bit)'];
end
%
% Done.
%