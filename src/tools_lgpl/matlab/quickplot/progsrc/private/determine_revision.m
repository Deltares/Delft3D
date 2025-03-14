function [revString,repoUrl,hash] = determine_revision(dirname,dbid)
%DETERMINE_REVISION Determine the Git hash or Subversion revision string.
%   STR = DETERMINE_REVISION(DIR) determines a revision string representing
%   the code status in the provided DIR using information from Subversion
%   or Git. For Subversion the string consists of the highest revision
%   number found in the folder and a flag indicating whether the code has
%   been changed. For Git the revision number is replaced by the short hash
%   and the flag.

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

Id = '$Id$';
repoUrl = '$HeadURL$';
hash = 'N/A';
if ~strcmp(Id(2:end-1),'Id')
    % Subversion keyword expansion seems to be active.
    % Use Subversion
    iter = 1;
    found = 0;
    if nargin<2
        dbid = 1;
    end
    while ~found
        switch iter
            case 1
                if strncmp(computer,'PCWIN',5)
                    thisfile = mfilename('fullpath');
                    tools = strfind(thisfile,'tools_lgpl');
                    svnVersion = [thisfile(1:tools-1),'third_party_open/subversion/bin/win32/svnversion.exe'];
                else
                    svnVersion = '/usr/bin/svnversion';
                end
            case 2
                svnPath = getenv('SVN_BIN_PATH');
                svnVersion = [svnPath filesep 'svnversion.exe'];
            case 3
                svnPath = 'c:\Program Files\Subversion\bin';
                svnVersion = [svnPath filesep 'svnversion.exe'];
            case 4
                [s,svnVersion] = system_plain('which svnversion');
                if s~=0
                    svnVersion = 'The WHICH command failed';
                end
            case 5
                svnVersion = '';
                break
        end
        %
        if exist(svnVersion,'file')
            break
        end
        iter = iter+1;
    end

    if ~isempty(svnVersion)
        [s,revString] = system_plain(['"' svnVersion '" "' dirname '"']);
    else
        s = 0;
    end
    if s==0
        changed = ismember('M',revString);
        rev = sscanf(revString,'%i:%i');
        if isempty(rev) %exported
            revMin = 0;
            revMax = 0;
            changed = 1;
        elseif length(rev)==1
            revMin = rev;
            revMax = rev;
        else
            revMin = rev(1);
            revMax = rev(2);
        end
    else
        dprintf(dbid,'Unable to execute SVNVERSION program.\nUsing built-in implementation of svnversion.\n')
        [revMin,revMax,changed] = svnversion(dirname,dbid);
    end

    if revMax<0
        revString = '[unknown revision]';
    else
        revString = sprintf('%05.5i',revMax);
        if changed || revMin<revMax
            revString = [revString ' (changed)'];
        end
    end

    repoUrl = repoUrl(11:end-23);
else
    % Use Git

    % get hash
    cwd = pwd;
    cd(dirname)
    [a,b] = system_plain('git -P log -n 10 -v --decorate')
    [a,b] = system_plain('git -P log -n 1 -v --decorate');
    % if we could remove -n 1, we could look for the latest hash available
    % at the origin, but that triggers a pager to wait for keypresses. The
    % option --no-pager before log seems to work on the command line, but
    % not when called via system for some reason.
    if a ~= 0
        revString = 'unknown';
        repoUrl   = 'unknown';
        hash      = 'unknown';
    else
        [commit, b] = strtok(b);
        [hash, b] = strtok(b);
        b = strsplit(b, local_newline);
        
        if ~isempty(strfind(b{1},'ref/merge-requests'))
            % We're probably working on a merge-request ... ignore the
            % local (merge) commit
            hasLocalCommits = false;
        else
            hasLocalCommits = isempty(strfind(b{1}, 'origin/'));
        end

        % get repository
        [a, b] = system_plain('git remote -v');
        [origin, b] = strtok(b);
        [repoUrl, b] = strtok(b);

        % git describe
        %[a,b] = system_plain(['git describe "' dirname '"']);
        % returns something like: DIMRset_2.23.05-4-ge3176daa1
        % but I don't want QUICKPLOT to refer to "DIMRset" tags
        % however, neither should DIMRsets refer to QUICKPLOT tags.

        % get status
        [a, b] = system_plain(['git status "' dirname '"']);
        b = strsplit(b, local_newline);
    
        hasStagedChanges = check_and_list_files(b, 'Changes to be committed:', 'Staged files:\n', false);
    
        hasUnstagedChanges = check_and_list_files(b, 'Changes not staged for commit:', 'Modified files:\n', false);
    
        hasUntrackedChanges = check_and_list_files(b, 'Untracked files:', 'Untracked files:\n', true);

        % we should also check if we have local commits to be pushed.
        revString = hash(1:9);
        if hasLocalCommits || hasStagedChanges || hasUnstagedChanges || hasUntrackedChanges
            revString = [revString ' (changed)'];
        end
    end
    cd(cwd)
end

function checkResult = check_and_list_files(b, checkString, printString, mexExcept)
checkResult = false;
TAB = sprintf('\t');
isCheckString = strncmp(b, checkString, length(checkString));
if any(isCheckString)
    i = find(isCheckString) + 1;
    % skip lines starting with '(use' such as
    % (use "git add <file>..." to update what will be committed)
    i = i + 1;
    while i < length(b) && strcmp(strtok(b{i}), '(use')
        i = i + 1;
    end
    while i < length(b) && strcmp(b{i}(1), TAB)
        file = b{i}(2:end);
        % skip 'modified:' substring if found ...
        if strncmp(file, 'modified:', 9)
            file = strtrim(file(10:end));
        end
        folderAndFile = strsplit(file, '/');
        if length(folderAndFile) == 1 || strcmp(folderAndFile{1}, 'private')
            % file in current folder
            % or file in private folder or below
            if mexExcept && ~isempty(strfind(folderAndFile{end}, '.mex'))
                % we need to ignore added mex files for the build process
            else
                if ~checkResult
                    fprintf(printString);
                    checkResult = true;
                end
                fprintf(' * %s\n', file);
            end
        end
        i = i + 1;
    end
    if checkResult
        fprintf('\n');
    end
end


function [min_update,max_update,changed] = svnversion(dirname,dbid)
min_update = inf;
max_update = -inf;
changed = 0;
d = dir(dirname);
for i = 1:length(d)
    if ismember(d(i).name,{'.','..','.svn'})
        % do nothing
    elseif d(i).isdir
        [min1,max1,changed1] = svnversion(fullfile(dirname,d(i).name),dbid);
        min_update = min(min_update,min1);
        max_update = max(max_update,max1);
        changed = changed | changed1;
    end
end
entries = get_svn_entries(dirname);
ref = fullfile(dirname,'.svn/text-base');
for i = 1:length(entries)
    reffile = fullfile(ref,[entries(i).filename '.svn-base']);
    newfile = fullfile(dirname,entries(i).filename);
    min_update = min(min_update,entries(i).last_updated);
    max_update = max(max_update,entries(i).last_updated);
    if ~exist(newfile,'file')
        % file has been removed
        dprintf(dbid,'File removed: "%s"\n',newfile);
        changed = 1;
    else
        changed = changed | is_file_modified(reffile,newfile,dbid);
    end
end

function entries = get_svn_entries(dirname)
entries = [];
fid = fopen(fullfile(dirname,'.svn','entries'),'r');
if fid<0
    % no subversion directory
    return
end
str = fread(fid,[1 inf],'*char');
fclose(fid);
entry = strfind(str,char(12));
%
substr = str(1:entry(1)-1);
lines = strfind(substr,local_newline);
updatestr = substr(lines(3)+1:lines(4)-1);
updatenr = str2double(updatestr);
%
j = 0;
for i=1:length(entry)-1
    substr = str(entry(i)+2:entry(i+1)-1);
    lines = strfind(substr,local_newline);
    if strcmp(substr(lines(1)+1:lines(2)-1),'file')
        j = j+1;
        entries(j).filename = substr(1:lines(1)-1);
        revstr = substr(lines(9)+1:lines(10)-1);
        entries(j).last_revised = str2double(revstr);
        updatestr = substr(lines(2)+1:lines(3)-1);
        if isempty(updatestr)
            entries(j).last_updated = updatenr;
        else
            entries(j).last_updated = str2double(updatestr);
        end
    end
end

function changed = is_file_modified(reffile,newfile,dbid)
fid = fopen(reffile,'r');
file1 = fread(fid,[1 inf],'*char');
fclose(fid);
%
fid = fopen(newfile,'r');
file2 = fread(fid,[1 inf],'*char');
fclose(fid);
%
changed = 1;
if isequal(file1,file2)
    changed = 0;
else
    Ids = [];
    for keyw = {'Id','Date','Author','Revision','HeadURL'}
        kw = keyw{1};
        Ids = cat(2,Ids,strfind(file1,['$' kw '$']));
    end
    Ids = sort(Ids);
    for i = 1:length(Ids)
        kw = sscanf(file1(Ids(i)+1:end),'%[A-Za-z]');
        if ~strcmp(file2(Ids(i)+(0:length(kw))),['$' kw])
            break
        else
            Amp = strfind(file2(Ids(i)+1:end),'$');
            if isempty(Amp)
                break
            else
                file2 = cat(2,file2(1:Ids(i)),kw,file2(Ids(i)+Amp(1):end));
            end
        end
    end
    if isequal(file1,file2)
        changed = 0;
    end
end

if changed
    dprintf(dbid,'File changed: "%s"\n',newfile);
end


function dprintf(fid,varargin)
if fid~=0
    fprintf(fid,varargin{:});
end


function s = local_newline
if matlabversionnumber > 9.01
    s = newline;
else
    s = char(10);
end


function [err,output] = system_plain(cmd)
[err,output] = system(cmd);
output = strip_ansi(output);


function s = strip_ansi(s)
% https://en.wikipedia.org/wiki/ANSI_escape_code

% CSI sequences
parameter_bytes = '[0-9:;<=>?]*';
intermediate_bytes = '[!"#$%&''()*+,-./]*';
final_byte = '[@A-Z\[\\\]\^_`a-z{|}~]';
s = regexprep(s, [char(27),'\[',parameter_bytes,intermediate_bytes,final_byte],'');

% Escape sequences
s = regexprep(s, [char(27) '.'],'');
