function svnstripfile(basedir)
%SVNSTRIPFILE Strip $-sign from SVN keywords in files.
%    SVNSTRIPFILE(BaseDir) recursively processes all .m, .c and .cpp files in
%    the directory BaseDir and below, stripping away the $-signs from the SVN
%    keywords HeadURL and Id (more keywords can easily be added).
%
%    This tool should be run after checking out source code maintained in one
%    location before committing it to another subversion location for
%    distribution. If the $-signs are not stripped, then there is a fair chance
%    that the revision data is used from the second location rather than the
%    information from the first location where the code is actually maintained.

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

d = dir(basedir);
for i = 1:length(d)
    if d(i).isdir
        if strcmp(d(i).name,'.') || strcmp(d(i).name,'..')
            %
            % don't process this directory or higher directory
            %
            continue
        end
        %
        % recursive processing of child directories
        %
        svnstripfile([basedir filesep d(i).name])
    else
        [p,f,e] = fileparts(d(i).name);
        if ~strcmp(e,'.m') && ~strcmp(e,'.c') && ~strcmp(e,'.cpp') && ~strcmp(e,'.ini')
            %
            % only process m, c and cpp files
            %
            continue
        end
        %
        % read file
        %
        filename = [basedir filesep d(i).name];
        fid = fopen(filename,'r');
        c = {};
        while 1
            line = fgetl(fid);
            if ~ischar(line)
                break
            end
            c{end+1} = line;
        end
        fclose(fid);
        %
        % filter lines
        %
        Keywords = {'HeadURL','Id'};
        for l = 1:length(c)
            for k = 1:length(Keywords)
                j = strfind(c{l},['$' Keywords{k} ':']);
                if ~isempty(j)
                    j2 = strfind(c{l},'$');
                    j2 = min(j2(j2>j));
                    c{l} = [c{l}(1:j-1) c{l}(j+1:j2-1) c{l}(j2+1:end)];
                end
            end
        end
        %
        fid = fopen(filename,'w');
        fprintf(fid,'%s\n',c{:});
        fclose(fid);
    end
end