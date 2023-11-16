function relfile = relativepath(file,reffile)
%RELATIVEPATH Determine file name relative to second file.
%   RELFIL = RELATIVEPATH(FIL1, FIL2) determines the location of file
%   FIL1 relative to file FIL2. If FIL1 or FIL2 is specified as a relative
%   file name, then it is assumed to be specified relative to the current
%   work directory.
%
%   RELFIL = RELATIVEPATH(FILE, DIR) determines the location of the FILE
%   relative to the specified directory DIR. To avoid confusion between
%   reference files and reference directories (and which directory level
%   should then be taken as reference location) the DIR string should end
%   with a file separator.
%
%   RELFIL = RELATIVEPATH(FILE) determines the location of the FILE
%   relative to the current work directory.
%
%   See also ABSFULLFILE, FULLFILE, FILSEP, FILEPARTS, PWD.

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

[~,f,e] = fileparts(file);
if any(strcmp([f e],{'.','..'}))
    file = [file filesep];
end
file = absfullfile(file);
%
if nargin==2
    [~,f,e] = fileparts(reffile);
    if any(strcmp([f e],{'.','..'}))
        reffile = [reffile filesep];
    end
    reffile = absfullfile(reffile);
    if exist(reffile,'dir')==7 && reffile(end)~=filesep
        error('To avoid confusion, a reference directory should end with "%s".',filesep)
    end
else
    reffile = pwd;
    if reffile(end)~=filesep % pwd could return / on Linux
        reffile(end+1) = filesep;
    end
end
%
minlen = min(length(reffile),length(file));
if ispc
    cheq = lower(reffile(1:minlen))==lower(file(1:minlen));
else
    cheq = reffile(1:minlen)==file(1:minlen);
end
noeq = find(cheq==0);
if isempty(noeq)
    lasteq = minlen;
else
    lasteq = min(noeq)-1;
end
while lasteq>0 && reffile(lasteq)~=filesep
    lasteq = lasteq-1;
end
%
if lasteq==0
    % path aren't equal at all, so return absolute path
    relfile = file;
else
    reldir  = fileparts(reffile(lasteq+1:end));
    folders = splitcellstr(reldir,filesep);
    relfile = [repmat(['..' filesep],1,length(folders)) file(lasteq+1:end)];
end
if isempty(relfile) && file(end)==filesep
    relfile = ['.' filesep];
end