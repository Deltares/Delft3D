function str = stack2str(stack,basefunction)
%STACK2STR Convert exception stack into cell string.
%
%   CELLSTR = STACK2STR(STACK)
%   where STACK is a stack as obtained from MException.stack.
%
%   See also MException.

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
%   http://www.deltaressystems.com
%   $HeadURL$
%   $Id$

stacklen = length(stack);
if nargin==2
    for i = 1:stacklen
        if strcmp(stack(i).name,basefunction)
            stacklen = i;
            break
        end
    end
end
% preallocate str to fit stack messages and associated source code lines
str = repmat({''},2*stacklen,1);
mpath = multiline(matlabpath,pathsep,'cell');
i2 = 0;
for i = 1:stacklen
    [p,f] = fileparts(stack(i).file);
    if ~strcmp(f,stack(i).name)
        fcn = sprintf('>%s',stack(i).name);
    else
        fcn = '';
    end
    z = zeros(size(mpath));
    for j = 1:length(mpath)
        if strncmp(p,mpath{j},length(mpath{j}))
            z(j) = length(mpath{j});
        end
    end
    [len,j] = max(z);
    p = p(len+1:end);
    if ~isempty(p) && isequal(p(1),filesep)
        p = p(2:end);
    end
    if ~isempty(p)
        p = [p filesep];
    end
    i2 = i2+1;
    str{i2} = sprintf('In %s%s%s at line %i',p,f,fcn,stack(i).line);
    try
        lineStr = getline([p,f,'.m'],stack(i).line);
        i2 = i2+1;
        str{i2} = lineStr;
    catch
        % couldn't find the line ... don't trigger and error while
        % processing and error
    end
end
str(i2+1:end) = [];

function str = getline(fileName,lineNr)
persistent buffered

% check if file is in buffer
if isstandalone
    qpdir = qp_basedir('exe');
    matlabdir = [qpdir, filesep, '..', filesep, '..', filesep, 'delf3d_matlab'];
    % should we also check private?
    fullFileName = [matlabdir, filesep, fileName];
else
    fullFileName = which(fileName);
end
fileInfo = dir(fullFileName);
if isempty(fileInfo)
    error('Cannot locate the file: %s.',fileName)
end

if isempty(buffered)
    i = [];
else
    i = find(strcmp(fullFileName,buffered.fileNames));
end

% check if file date hasn't changed
if ~isempty(i) && strcmp(fileInfo.date, buffered.fileDates{i})
    % it's a match
    fileListing = buffered.fileListings{i};
else
    fileListing = getfile(fullFileName);
    if isempty(i)
        % create new record
        i = length(buffered.fileNames) + 1;
        buffered.fileNames{i} = fullFileName;
    else
        % overwrite previous record
    end
    buffered.fileDates{i} = fileInfo.date;
    buffered.fileListings{i} = fileListing;
end

% get the line
if lineNr < length(fileListing)
    str = fileListing{lineNr};
else
    error('lineNr larger than file length.')
end

function C = getfile(filename)
fid = fopen(filename,'r');
C = cell(1024,1);
i = 0;
while ~feof(fid)
    i = i+1;
    if i>length(C)
        C{2*i} = '';
    end
    C{i} = fgetl(fid);
end
C(i+1:end) = [];
fclose(fid);
