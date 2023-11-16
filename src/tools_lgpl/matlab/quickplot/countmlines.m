function countmlines(maindir)
%COUNTMLINES Count lines in QUICKPLOT source code
%   Count number of lines in the m-files in the
%   directories progsrc and progsrc/private

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

if nargin==0
    maindir = 'progsrc';
end
[X1,n1,nc1] = countmlines_recursive(maindir);
fprintf('Grand total: %i (%i) in %i files\n',n1,nc1,size(X1,1));
[NC1,Reorder] = sort(cat(1,X1{:,3}),1,'descend');
X1 = X1(Reorder,[2 3 1]);
N = ceil(0.05*size(X1,1));
if N>1
    fprintf('The %i biggest files are:\n',N);
else
    fprintf('The biggest file is:\n',N);
end
TX1 = X1(1:N,:)';
fprintf('%i (%i) %s\n',TX1{:});


function [X1,n1,nc1] = countmlines_recursive(maindir)
if maindir(end)~='/' && maindir(end)~='\'
    maindir = [maindir '/'];
end
[X1,n1,nc1]=countmlines_dir(maindir);
d = dir(maindir);
for i = 1:length(d)
    if d(i).isdir && ~strcmp(d(i).name,'.') && ~strcmp(d(i).name,'..')
        [X2,n2,nc2]=countmlines_recursive([maindir d(i).name]);
        X1  = [X1;X2];
        n1  = n1+n2;
        nc1 = nc1+nc2;
    end
end

function [X,total_nl,total_ncl]=countmlines_dir(subdir)
%COUNTMLINES_DIR
%    Count number of lines in the m-files in the
%    specified directory

fprintf('%s:\n',subdir);
X={};
for d=dir([subdir '*.m'])'
    nl=0;
    ncl=0;
    fid=fopen([subdir d.name],'r');
    while ~feof(fid)
        line = fgetl(fid);
        nl=nl+1;
        tok = strtok(line);
        if ~isempty(tok) && tok(1)~='%'
            ncl=ncl+1;
        end
    end
    fclose(fid);
    nl=nl-1;
    X(end+1,1:3)={d.name nl ncl};
    fprintf('%-30s %i (%i)\n',X{end,:});
    X{end,1}=[subdir d.name];
end
total_nl=sum([X{:,2}]);
total_ncl=sum([X{:,3}]);
fprintf('Total for this directory: %i (%i) in %i files\n\n',total_nl,total_ncl, size(X,1));
