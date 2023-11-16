function cleanup(X)
%CLEANUP  removes files and directories
%   CLEANUP(X) removes the files and directories listed in the cell string X.
%
%   Example: remove all "*.bak" files and the subdirectory "subdir".
%
%      X={'*.bak' 'subdir'}
%      cleanup(X)
%
%   See also: DELETE.

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

if isunix
    % linux rm command is not recursive
    d = dir;
    for id=1:length(d)
        if d(id).isdir && ~isequal(d(id).name(1),'.')
            % recursively call cleanup
            cd(d(id).name)
            cleanup(X);
            cd ..
        end
    end
    % finally process this directory
    for i=1:length(X(:))
        unix(['rm -rf ' X{i}]);
    end
else
    for i=1:length(X(:))
        X{i}=strrep(X{i},'/','\');
        if isdir(X{i})
            [s,msg]=dos(['rmdir /s/q ' X{i}]);
        else
            [s,msg]=dos(['del /s/f/q ' X{i}]);
        end
        if s~=0
            if isdir(X{i})
                [s,msg]=dos(['rmdir /s/q ' X{i}]);
            end
            if s~=0
                error(msg);
            end
        end
    end
end
