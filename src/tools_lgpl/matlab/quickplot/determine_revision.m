function [revstring,repo_url,hash] = determine_revision(dirname,dbid)
%DETERMINE_REVISION Determine the Git hash or Subversion revision string.
%   STR = DETERMINE_REVISION(DIR) determines a revision string representing
%   the code status in the provided DIR using information from Subversion
%   or Git. For Subversion the string consists of the highest revision
%   number found in the folder and a flag indicating whether the code has
%   been changed. For Git the revision number is replaced by the short hash
%   and the flag.

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

Id = '$Id$';
repo_url = '$HeadURL$';
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
                    SvnVersion = '../../../../third_party_open/subversion/bin/win32/svnversion.exe';
                else
                    SvnVersion = '/usr/bin/svnversion';
                end
            case 2
                if strncmp(SvnVersion,'../',3)
                    SvnVersion = SvnVersion(4:end); % one level less deep
                end
            case 3
                svnbin = getenv('SVN_BIN_PATH');
                SvnVersion = [svnbin filesep 'svnversion.exe'];
            case 4
                svnbin = 'c:\Program Files\Subversion\bin';
                SvnVersion = [svnbin filesep 'svnversion.exe'];
            case 5
                [s,SvnVersion] = system('which svnversion');
                if s~=0
                    SvnVersion = 'The WHICH command failed';
                end
            case 6
                dprintf(dbid,'Unable to locate SVNVERSION program.\nUsing built-in implementation of svnversion.\n')
                [revmin,revmax,changed] = svnversion(dirname,dbid);
                return
        end
        %
        if exist(SvnVersion,'file')
            break
        end
        iter = iter+1;
    end

    [s,revstring] = system(['"' SvnVersion '" "' dirname '"']);
    if s==0
        changed = ismember('M',revstring);
        rev = sscanf(revstring,'%i:%i');
        if isempty(rev) %exported
            revmin = 0;
            revmax = 0;
            changed = 1;
        elseif length(rev)==1
            revmin = rev;
            revmax = rev;
        else
            revmin = rev(1);
            revmax = rev(2);
        end
    else
        dprintf(dbid,'Unable to execute SVNVERSION program.\nUsing built-in implementation of svnversion.\n')
        [revmin,revmax,changed] = svnversion(dirname,dbid);
    end

    if revmax<0
        revstring = '[unknown revision]';
    else
        revstring = sprintf('%05.5i',revmax);
        if changed || revmin<revmax
            revstring = [revstring ' (changed)'];
        end
    end

    repo_url = repo_url(11:end-23);
else
    % Use Git

    % get hash
    [a,b] = system('git log -n 1 -v --decorate');
    [commit,b] = strtok(b);
    [hash,b] = strtok(b);
    b = strsplit(b,local_newline);
    has_local_commits = isempty(strfind(b{1},'origin/'));
    % if we could remove -n 1, we could look for the latest hash available
    % at the origin, but that triggers a pager to wait for keypresses. The
    % option --no-pagers before log seems to work on the command line, but
    % not when called via system for some reason.

    % get repository
    [a,b] = system('git remote -v');
    [origin,b] = strtok(b);
    [repo_url,b] = strtok(b);

    % git describe
    %[a,b] = system(['git describe "' dirname '"']);
    % returns something like: DIMRset_2.23.05-4-ge3176daa1
    % but I don't want QUICKPLOT to refer to "DIMRset" tags
    % however, neither should DIMRsets refer to QUICKPLOT tags.

    % get status
    [a,b] = system(['git status "' dirname '"']);
    b = strsplit(b,local_newline);
    staged = strncmp(b,'Changes to be committed:',24);
    unstaged = strncmp(b,'Changes not staged for commit:',30);
    untracked = strncmp(b,'Untracked files:',16);

    % we should also check if we have local commits to be pushed.
    revstring = hash(1:9);
    if has_local_commits || any(staged) || any(unstaged) || any(untracked)
        revstring = [revstring ' (changed)'];
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