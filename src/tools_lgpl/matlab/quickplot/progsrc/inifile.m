function varargout=inifile(cmd,varargin)
%INIFILE Read/write INI files.
%   Info = INIFILE('open', FileName)
%   Open and read the INI file; return the data to the workspace in a set of
%   nested cell arrays.
%
%   Info = INIFILE('new')
%   Create a new INI file structure in memory.
%
%   Info = INIFILE('write', FileName, Info)
%   Open and write the INI file; the data in the file is overwritten without
%   asking.
%
%
%   ListOfChapters = INIFILE('chapters', Info)
%   Retrieve the list of Chapter names as a cell array of strings.
%
%   IndexChapter = INIFILE('chapters', Info, C)
%   Retrieve the chapter indices that match the specified chapter name C. Use
%   'chaptersi' for a case insensitive match of the chapter name.
%
%   [BOOL, IndexChapter] = INIFILE('exists', Info, C)
%   Check whether a chapter named C exists in the the Info data set, and return
%   return the index or indices of the chapters matching the specified name.
%   Use 'existsi' for a case insensitive match of the Chapter name.
%
%   [Info, IndexChapter] = INIFILE('add', Info, C)
%   Add a new chapter with name C to the data set. The updated data set is
%   returned, as well as the index IndexChapter of the newly created group.
%   Calling this function repeatedly with the same chapter name will  create
%   multiple chapter instances with name C.
%
%   Info = INIFILE('delete', Info, C)
%   Info = INIFILE('delete', Info, C, CN)
%   Delete the chapter(s) with name C from the data set. The updated data set
%   is returned. If there are multiple instances use CN to specify the index
%   or indices of the group(s) to be removed. Use 'deletei' for a case
%   insensitive match of the chapter name.
%
%
%   ListOfKeywords = INIFILE('keywords', Info, C)
%   Retrieve the list of keywords in specified chapter C as a cell array of
%   strings. Use 'keywordsi' for a case insensitive match of the Chapter name.
%
%   N = INIFILE('exists', Info, C, K)
%   Check whether a chapter C and keyword K combination exists in the Info
%   data set. If the chapter name occurs once, N will be the number of
%   occurrences of the keyword in that chapter. If the chapter name occurs
%   multiple times, N will be a M-by-1 array where M is the number of times
%   that the chapter name occurs in the Info data set. Each element of N
%   equals the number of times the keyword name occurs in each chapter.
%
%   [Val, CN] = INIFILE('cgetstring', Info, C, K, DefS)
%   Retrieve the value of the keyword K of chapter C from the Info data set.
%   The return variable Val is an M x 1 cell array of strings where M equals
%   the total number of occurrences of the keyword keyword K in a chapter C.
%   Use 'cgetstringi' for a case insensitive match of the chapter and keyword
%   names. The chapter and keyword may also be specified by their index instead
%   of their name. The optional second argument CN returns the index of the
%   chapter (or indices of the chapters) from which the keyword K was retrieved.
%   If the chapter/keyword pair does not exist, the default string DefS will be
%   returned; if DefS is not specified, an error will be raised. If multiple
%   chapters match C, and some of those chapters don't include K then an error
%   will be raised if DefS is not specified unless the second output argument
%   iChap is requested.
%
%   Variations (all come with '...i' alternatives):
%      'getstring'   returns a string if the chapter/keyword pair occurs once,
%                    and a cell array of strings otherwise.
%      'cget'        returns a cell array in which strings that represent
%                    numbers have been replaced by those numbers.
%      'get'         as 'cget' but returns a numerical array if all strings
%                    could be converted to numbers, and returns a string if the
%                    chapter/keyword pair occurs once.
%      'hcgetstring' returns a cell array of strings which are:
%                     * equal to the original strings if the strings don't
%                       contain any hash signs
%                     * equal to the string between the first two # signs if
%                       the string contains multiple hash signs and the first
%                       hash sign is the first non-space character, and
%                     * equal to the string before the first # sign in all
%                       other cases.
%      'hgetstring'  equal to 'getstring' except for the initial parsing of the
%                    hash signs.
%      'hcget'       equal to 'cget' except for the initial parsing of the hash
%                    signs.
%      'hget'        equal to 'get' except for the initial parsing of the hash
%                    signs.
%
%
%   Info = INIFILE('set', Info, C, K, Value)
%   Set the keyword K in chapter C to the indicated Value. The updated data set
%   is returned. If the chapter and/or keyword do not exist, they are created.
%   Use 'seti' for a case insensitive match of the chapter and keyword names.
%   The chapter and keyword may also be specified by their index instead of
%   their name.
%   NOTE: If Value equals [], the keyword is deleted but use 'delete' instead.
%
%   Info = INIFILE('delete', Info, C, K)
%   Delete the specified keyword K from the specified chapter C. The updated
%   data set is returned. Use 'deletei' for a case insensitive match of the
%   chapter and keyword names. The chapter and keyword may also be specified
%   by their index instead of their name.

%   Old behavior to be removed.
%
%   Info=INIFILE('set',Info,Chapter,[])
%   Delete Chapter from the data set. The updated data set is returned.
%   Superseeded by 'delete'.
%
%   Info=INIFILE('set',Info,Chapter,Keyword,[])
%   Delete the specified Keyword from the specified Chapter in the data set.
%   The updated data set is returned. Superseeded by 'delete'.

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

lcmd = lower(cmd);
switch lcmd
    case 'open'
        varargout{1} = readfile(varargin{:});
    case {'chapters','chaptersi'}
        varargout{1} = getChaptersInFile(lcmd,varargin{:});
    case {'keywords','keywordsi'}
        varargout{1} = getKeysInChapter(lcmd,varargin{:});
    case {'exists','existsi'}
        try
            if nargin==3
                % only Chapter
                if strcmp(lcmd,'exists')
                    lcmd = 'chapters';
                    chap = varargin{2};
                else
                    lcmd = 'chaptersi';
                    chap = lower(varargin{2});
                end
                A = getChaptersInFile(lcmd,varargin{1});
                Amatch = strcmp(chap,A);
                varargout{1} = sum(Amatch);
                if nargout>1
                    varargout{2} = find(Amatch);
                end
                return
            else
                % Chapter/Keyword pair
                [A,iChap] = inifile(lcmd,varargin{1:2});
                if strcmp(lcmd,'exists')
                    lcmd = 'keywords';
                    keyw = varargin{3};
                else
                    lcmd = 'keywordsi';
                    keyw = lower(varargin{3});
                end
                %
                M = zeros(size(iChap));
                for i = 1:length(iChap)
                    A = getKeysInChapter(lcmd,varargin{1},iChap(i));
                    M(i) = sum(strcmp(keyw,A));
                end
                varargout{1} = M;
                return
            end
        catch
            varargout{1} = false;
        end
    case {'hget' ,'get' ,'hgetstring' ,'getstring' ,'hcget' ,'cget' ,'hcgetstring' ,'cgetstring' , ...
          'hgeti','geti','hgetstringi','getstringi','hcgeti','cgeti','hcgetstringi','cgetstringi'}
        [varargout{1:max(nargout,1)}] = getfield(lcmd,varargin{:});
    case {'set','seti','add','addi'}
        [varargout{1:max(nargout,1)}] = setfield(lcmd,varargin{:});
    case {'delete','remove','deletei','removei'}
        varargout{1} = setfield(lcmd,varargin{:},[]);
    case 'write'
        FI = writefile(varargin{:});
        if nargout>0
            varargout = {FI};
        end
    case 'new'
        varargout{1} = newfile;
    otherwise
        error('Unknown command: %s',cmd)
end


function FI=newfile
S=cell(0,2);
FI.FileName='new file';
FI.FileType='INI file';
FI.Data=S;


function FI=readfile(filename,varargin)
fid=fopen(filename,'rt');
if fid<0
    error('Can''t open file: %s.',filename)
end
Line = textscan(fid,'%s','delimiter','\n','whitespace','');
Line = Line{1};
fclose(fid);
Line = strtrim(Line);
Line(cellfun('isempty',Line))=[];
%
ichp = 0;
% preallocate space for 1000 chapters
PreAllocChap = 1000;
S = cell(PreAllocChap,2);
for i = 1:length(Line)
    ln = Line{i};
    if ln(1)=='['
        % remove unused preallocated key fields
        if ichp>0
            S{ichp,2} = strtrim(K(1:ikey,:));
        end
        % if we reach the preallocated chapter array length, double its length
        if ichp==PreAllocChap
            PreAllocChap = 2*PreAllocChap;
            S{PreAllocChap,1} = [];
        end
        % create a new chapter and preallocate array space for keys
        ichp = ichp+1;
        PreAllocKey = 1000;
        K = cell(PreAllocKey,2);
        S{ichp,1} = ln(2:end-1);
        ikey = 0;
    else
        % if we find lines before a chapter, add a dumy chapter
        if ichp==0
            ichp = ichp+1;
            PreAllocKey = 1000;
            K = cell(PreAllocKey,2);
            S{ichp,1} = '';
            ikey = 0;
        end
        % if we reach the preallocated key array length, double its length
        if ikey==PreAllocKey
            PreAllocKey = 2*PreAllocKey;
            K{PreAllocKey,1} = [];
        end
        ikey = ikey+1;
        % process the key
        eq = strfind(ln,'=');
        if ~isempty(eq)
            K{ikey,1} = ln(1:eq(1)-1);
            K{ikey,2} = ln(eq(1)+1:end);
        elseif ikey>1 && ~isempty(K{ikey-1,2}) && K{ikey-1,2}(end) == '\'
            ikey = ikey-1;
            K{ikey,2} = [K{ikey,2}(1:end-1) ' ' ln];
        else
            K{ikey,1} = '';
            K{ikey,2} = ln;
        end
    end
end
% remove any superfluous preallocated cells for keys and chapters
if ichp>0
    S{ichp,2} = strtrim(K(1:ikey,:));
end
S = S(1:ichp,:);
%
FI.FileName=filename;
FI.FileType='INI file';
FI.Blank = 'valid';
FI.Data=S;
%
i = 1;
while i < length(varargin)
    if strcmpi(varargin{i},'blank')
        FI.Blank = varargin{i+1};
        i = i+2;
    else
        i = i+1;
    end
end


function FI = writefile(filename,FI,formatStyle)
FI.FileName = filename;
S = FI.Data;
fid = fopen(filename,'wt');
if fid<0
    error('Error opening %s.',filename)
end
if nargin<3
    formatStyle = 'standard';
end
compact = false;
pretty  = false;
switch formatStyle
    case 'compact'
        compact = true;
    case 'standard'
    case 'pretty'
        pretty = true;
end
if pretty
    indent = '    ';
else
    indent = '';
end
%
% Keywords without a Chapter title should be written first.
%
for i = 1:size(S,1)
    if isempty(S{i,1})
        S = cat(1,S(i,:),S(1:i-1,:),S(i+1:end,:));
    end
end
if compact
    format        = '%s=%s\n';
    format_spaces = '%s\n';
else
    maxkeywordlength = 0;
    for i = 1:size(S,1)
        SF = S{i,2};
        for j = 1:size(SF,1)
            maxkeywordlength = max(maxkeywordlength,length(SF{j,1}));
        end
    end
    if pretty
        maxkeywordlength = maxkeywordlength+1;
    end
    format        = [indent,'%-',num2str(maxkeywordlength),'s= %s\n'];
    format_spaces = [indent,repmat(' ',1,maxkeywordlength),'  %s\n'];
end
for i = 1:size(S,1)
    if ~isempty(S{i,1})
        fprintf(fid,'[%s]\n',S{i,1});
    end
    SF = S{i,2};
    for j = 1:size(SF,1)
        Str = SF{j,2};
        if ~ischar(Str)
            Str = sprintf('%g ',Str);
            Str(end) = [];
        end
        if isempty(SF{j,1})
            fprintf(fid,format_spaces,Str);
        else
            fprintf(fid,format,SF{j,1},Str);
        end
    end
    if pretty
        fprintf(fid,'\n');
    end
end
fclose(fid);


function Chapters = getChaptersInFile(cmd,FI,grpS)
CaseInsensitive = cmd(end)=='i';
Chapters = FI.Data(:,1);
if nargin>2
    if CaseInsensitive
        Chapters = find(strcmpi(Chapters,grpS));
    else
        Chapters = find(strcmp(Chapters,grpS));
    end
elseif CaseInsensitive
    Chapters = lower(Chapters);
end


function Keywords = getKeysInChapter(cmd,FI,grpS)
S = FI.Data;
CaseInsensitive = cmd(end)=='i';
if ischar(grpS)
    if isequal(grpS,'*')
        grp = 1:size(S,1);
    else
        if CaseInsensitive
            grp = strcmpi(grpS,S(:,1));
        else
            grp = strcmp(grpS,S(:,1));
        end
        grp = find(grp);
    end
elseif isnumeric(grpS) && all(grpS(:)<=size(S,1))
    grp = grpS(:)';
else
    grp = [];
end
if isempty(grp)
    error('Chapter ''%s'' does not exist.',var2str(grpS))
elseif length(grp)>1
    error('Can''t retrieve keywords for multiple chapters at once.')
end
Keywords = S{grp,2}(:,1);
if CaseInsensitive
    Keywords = lower(Keywords);
end


function [val,iGRP]=getfield(cmd,FI,grpS,keyS,def)
BlankIsEmpty = nargin>=5 && isfield(FI,'Blank') && strcmp(FI.Blank,'default');
if cmd(1)=='h'
    ProcessHashes = 1;
    cmd = cmd(2:end);
else
    ProcessHashes = 0;
end
CaseInsensitive = cmd(end)=='i';
if CaseInsensitive
    cmd = cmd(1:end-1);
end
CellOutput = cmd(1)=='c';
if CellOutput
    cmd = cmd(2:end);
end
S = FI.Data;
if ischar(grpS)
    if isequal(grpS,'*')
        grp = 1:size(S,1);
    else
        if CaseInsensitive
            grp = strcmpi(grpS,S(:,1));
        else
            grp = strcmp(grpS,S(:,1));
        end
        grp = find(grp);
    end
elseif isnumeric(grpS) && all(grpS(:)<=size(S,1))
    grp = grpS;
    grpS = sprintf('group#%i',grp);
else
    grp = [];
end
if isempty(grp)
    if nargin>=5
        if CellOutput
            val = {def};
        else
            val = def;
        end
        return
    elseif CellOutput
        val = {};
        return
    end
    error('Chapter ''%s'' does not exist',var2str(grpS))
end
Keywords = cat(1,S{grp,2});
%
iGRP = zeros(length(Keywords),1);
o = 0;
for i = 1:length(grp)
    nKeyw = size(S{grp(i),2},1);
    iGRP(o+(1:nKeyw)) = grp(i);
    o = o+nKeyw;
end
%
if ischar(keyS)
    keyS = deblank(keyS);
    if CaseInsensitive
        key = strcmpi(keyS,Keywords(:,1));
    else
        key = strcmp(keyS,Keywords(:,1));
    end
    key = find(key);
else
    key = keyS;
    if length(grp)>1
        error('Keyword indexing not supported for multiple chapters at once.')
    end
end
vgrp = iGRP(key);
%
if nargin >= 5
    mgrp = grp(~ismember(grp,vgrp));
    iGRP = cat(1,vgrp,mgrp);
    [iGRP,ordered] = sort(iGRP);
    mgrp = mgrp(:);
    val = repmat({def},[numel(vgrp)+numel(mgrp) 1]);
    val(1:length(key)) = Keywords(key,2);
    val = val(ordered);
elseif any(~ismember(grp,vgrp))
    if nargout > 1
        iGRP = vgrp;
        val = Keywords(key,2);
    elseif length(grp) == 1
        error('Keyword ''%s'' not found in Chapter ''%s''.',keyS,grpS)
    else
        error('Keyword ''%s'' not found in %i of the selected chapters.',keyS,sum(~ismember(grp,vgrp)))
    end
else
    iGRP = vgrp;
    val = Keywords(key,2);
end
%
% convert global group index to index within selected groups
%
[~,iGRP] = ismember(iGRP,grp);
%
if ProcessHashes
    val = rmhash(val);
end
%
if ~strcmp(cmd,'getstring')
    anychar = false;
    for i = 1:length(val)
        if isempty(val{i}) && BlankIsEmpty
            val{i} = def;
        end
        if ischar(val{i})
            [lni,~,err,SF2i]=sscanf(val{i},'%f',[1 inf]);
            if isempty(err) && SF2i>length(val{i})
                val{i} = lni;
            end
            anychar = anychar | ischar(val{i});
        end
    end
    if ~CellOutput && (length(val) == 1 || ~anychar)
        val = cat(1,val{:});
    end
elseif ~CellOutput && length(val) == 1
    val = val{1};
end


function str = rmhash(str)
if iscell(str)
    for i = 1:length(str)
        str{i} = rmhash(str{i});
    end
elseif ischar(str)
    hashes = strfind(str,'#');
    if length(hashes)>1
        str1 = deblank(str(1:hashes(1)-1));
        if isempty(str1)
            str = str(hashes(1)+1:hashes(2)-1);
        else
            str = str1;
        end
    elseif length(hashes)==1
        str = str(1:hashes(1)-1);
    end
    str = deblank(str);
end


function [FI,iGRP]=setfield(cmd,FI,grpS,varargin)
S = FI.Data;
CaseInsensitive = cmd(end)=='i';
if nargin==3
    % create the group ...
    if ischar(grpS)
        %if ~AddCommand
        %    warning('Use the ''add'' command to add new groups.')
        %end
        grp = size(FI.Data,1)+1;
        FI.Data(grp,:) = {grpS cell(0,2)};
        if nargout>1
            iGRP = grp;
        end
        return
    else
        error('Invalid group name %s -- expecting string -- while trying to add a group.',var2str(grpS))
    end
elseif ischar(grpS)
    % find a group by name
    if isequal(grpS,'*')
        grp = 1:size(S,1);
    else
        if CaseInsensitive
            grp = strcmpi(grpS,S(:,1));
        else
            grp = strcmp(grpS,S(:,1));
        end
        grp = find(grp);
    end
elseif isnumeric(grpS)
    % find a group by index
    if any(grpS(:)>size(S,1))
        error('Invalid group index: index larger than number of groups in file.')
    elseif any(grpS(:)<1)
        error('Invalid group index: index less than 1.')
    end
    grp = grpS;
else
    % unrecognized group identifier
    error('Unrecognized group indentifier %s specified as 3rd argument to INIFILE call.',var2str(grpS))
end
if isempty(grp)
    if isempty(varargin{1}) && isnumeric(varargin{1})
        % remove a non-existing group. Done!
        return
    end
    % create group and keyword ...
    S(end+1,1:2) = {grpS cell(0,2)};
    grp = size(S,1);
end
if ischar(grpS) && isnumeric(varargin{1})
    % group index
    igrp = varargin{1};
    if isequal(igrp,length(grp)+1)
        S(end+1,1:2) = {grpS cell(0,2)};
        grp = size(S,1);
    else
        grp = grp(igrp);
    end
    kS = 2;
else
    kS = 1;
end
%
if length(varargin)>=kS
    keyS = varargin{kS};
end
if ischar(keyS)
    keyS = deblank(keyS);
end
%
if length(varargin)>=kS+1
    val = varargin{kS+1};
    DeleteKey = isempty(val) && isnumeric(val);
    iv = 0;
    for i=1:length(grp)
        Keywords=S{grp(i),2};
        if isnumeric(keyS)
            key = keyS;
        elseif CaseInsensitive
            key = strcmpi(keyS,Keywords(:,1));
        else
            key = strcmp(keyS,Keywords(:,1));
        end
        if any(key)
            % key exists
            if DeleteKey
                S{grp,2}(key,:)=[];
            elseif iscell(val)
                % assign different value per record
                key=find(key);
                iv = iv+1;
                if iv<=length(val)
                    S{grp,2}{key(1),2}=val{iv};
                    if length(key)>1
                        S{grp,2}(key(2:end),:)=[];
                    end
                end
            else
                % assign same value to all records
                key=find(key);
                S{grp,2}{key(1),2}=val;
                if length(key)>1
                    S{grp,2}(key(2:end),:)=[];
                end
            end
        else
            % key doesn't exist
            if DeleteKey
                % nothing to do
            elseif iscell(val)
                % assign different value per record
                iv = iv+1;
                if iv<=length(val)
                    S{grp(i),2}(end+1,1:2)={keyS val{iv}};
                end
            else
                % assign same value to all records
                S{grp(i),2}(end+1,1:2)={keyS val};
            end
        end
    end
    %
    if iscell(val) && iv~=numel(val)
        error('Mismatch between the number of matching chapters (%i) and number of values given (%i)',iv,numel(val))
    end
elseif isempty(keyS) && isnumeric(keyS)
    % remove group
    S(grp,:) = [];
else
    error('No value specified during INIFILE SET call.')
end
FI.Data=S;
