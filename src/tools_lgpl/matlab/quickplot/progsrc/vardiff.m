function out=vardiff(var1,var2,fid,formatflag,var1name,var2name)
%VARDIFF Determines the differences between two variables.
%   VARDIFF(var1,var2) lists the differences between the two
%   specified variables-files.
%
%   different=VARDIFF(var1,var2) returns the lowest appropriate
%   number in the following list
%     0   if the variables are identical (no NaNs found),
%     1   if the variables are identical (NaNs found),
%     2   if the variables are of different size, class or they are
%         structures with different fields.
%     2+N if the data is different in the Nth level, for matrices
%         this will be at most 3, for cell arrays and structures this
%         can become higher than 3. This basically indicates that you
%         need N subscripts to see the difference.
%   The function does not show the differences as text.
%
%   different=VARDIFF(var1,var2,fid) returns the number as described
%   above and writes the difference log to the file indicated by the
%   fid argument.
%
%   See also ISEQUAL.

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

if nargin<2
    error('Not enough input arguments.')
end
if nargin<3
    fid = double(nargout==0);
end
if nargin>3
    formatflag = lower(formatflag);
    switch formatflag
        case 'html'
            br='<br>\n';
        case {'latex','latex-longtable'}
            br='\\newline\n';
        otherwise
            br='\n';
    end
else
    formatflag='';
    br='\n';
end
if fid
    if nargin<6
        var2name = inputname(2);
    end
    if nargin<5
        var1name = inputname(1);
    end
    printdiff(fid,br,'init',formatflag,var1name,var2name)
end

DiffFound = 0;
try
    if isequal(var1,var2)
        myfprintf(fid,['The variables are identical and they don''t contain NaNs.' br]);
    else
        DiffFound = 1 + detailedcheck(var1,var2,fid,formatflag,br,'');
        switch DiffFound
            case 1
                myfprintf(fid,['The variables are identical, but they do contain NaNs.' br])
            otherwise
                switch formatflag
                    case 'latex'
                        myfprintf(fid,['\\end{tabular}' br]);
                    case 'latex-longtable'
                        myfprintf(fid,'\\end{longtable}\n');
                    otherwise
                        % nothing to end
                end
        end
    end
catch Ex
    Ex2.message = ['An unexpected error occurred while comparing the files:\n' Ex.message];
    Ex2.identifier = '';
    Ex2.stack = Ex.stack;
    rethrow(Ex2)
end
if nargout>0
    out = DiffFound;
end


% -----------------------------------------------------------------------
%  Function used for printing ...
% -----------------------------------------------------------------------
function printdiff(fid,br,pflag,formatflag,varargin)
persistent var1 var2 firstdiff
if ~isequal(pflag,'init') 
    if firstdiff
        startEnvironment(fid,var1,var2,formatflag)
        firstdiff = false;
    end
end
switch pflag
    case 'init'  % varname1, varname2
        s1=varargin{1};
        s2=varargin{2};
        if isempty(s1)
            var1='VAR1';
        else
            var1=s1;
        end
        if isempty(s2)
            var2='VAR2';
        else
            var2=s2;
        end
        myfprintf(fid,['Comparing ',var1,' with ',var2, br]);
        firstdiff = true;
    case 'class'  % classvar1, classvar2, subscript string
        cls1=varargin{1};
        cls2=varargin{2};
        substr=varargin{3};
        switch formatflag
            case {'latex','latex-longtable'}
                myfprintf(fid,'\\STRUT %s class & %s & %s \\\\ \\hline\n',substr,cls1,cls2);
            otherwise
                myfprintf(fid,['Class (%s) of %s%s differs from class (%s) of %s%s.' br],cls1,var1,substr,cls2,var2,substr);
        end
    case 'size'   % classvar1, classvar2, subscript string
        sz1=varargin{1};
        sz2=varargin{2};
        substr=varargin{3};
        sz1_str = sprintf('%i ',sz1);
        sz1_str = ['[' sz1_str(1:end-1) ']'];
        sz2_str = sprintf('%i ',sz2);
        sz2_str = ['[' sz2_str(1:end-1) ']'];
        switch formatflag
            case {'latex','latex-longtable'}
                myfprintf(fid,'\\STRUT %s size & %s & %s \\\\ \\hline\n',substr,sz1_str,sz2_str);
            otherwise
                myfprintf(fid,['Size of %s%s is %s, but size of %s%s is %s.' br],var1,substr,sz1_str,var2,substr,sz2_str);
        end
    case 'data'  % subscript string
        d1_str=var2str(varargin{1});
        d2_str=var2str(varargin{2});
        substr=varargin{3};
        switch formatflag
            case {'latex','latex-longtable'}
                if isequal(d1_str,d2_str) || size(d1_str,1)>1 || size(d2_str,1)>1
                    myfprintf(fid,'\\STRUT %s & \\multicolumn{2}{c}{%s} \\\\ \\hline\n',substr,'data differs');
                else
                    myfprintf(fid,'\\STRUT %s & %s & %s \\\\ \\hline\n',substr,d1_str,d2_str);
                end
            otherwise
                myfprintf(fid,['Data of %s%s differs from data contained in %s%s.' br],var1,substr,var2,substr);
        end
    case 'fieldnames'  % fieldnames1,fieldnames2,subscript string
        fn1=varargin{1};
        fn2=varargin{2};
        substr=varargin{3};
        sfn1=setdiff(fn1,fn2);
        sfn2=setdiff(fn2,fn1);
        if ~isempty(sfn1)
            switch formatflag
                case {'latex','latex-longtable'}
                    lend = '';
                    for i = 1:length(sfn1)
                        if i == length(sfn1)
                            lend = ' \hline';
                        end
                        myfprintf(fid,'\\STRUT %s.%s & %s & %s \\\\%s\n',substr,sfn1{i},'\cmark','\xmark',lend);
                    end
                otherwise
                    myfprintf(fid,['%s%s contains the following fields not part of %s%s:' br],var1,substr,var2,substr);
                    myfprintf(fid,['  %s' br],sfn1{:});
            end
        end
        if ~isempty(sfn2)
            switch formatflag
                case {'latex','latex-longtable'}
                    lend = '';
                    for i = 1:length(sfn2)
                        if i == length(sfn2)
                            lend = ' \hline';
                        end
                        myfprintf(fid,'\\STRUT %s.%s & %s & %s \\\\%s\n',substr,sfn2{i},'\xmark','\cmark',lend);
                    end
                otherwise
                    myfprintf(fid,['%s%s contains the following fields not part of %s%s:' br],var2,substr,var1,substr);
                    myfprintf(fid,['  %s' br],sfn2{:});
            end
        end
        if isempty(sfn1) && isempty(sfn2)
            sfn1 = char(fn1);
            sfn2 = char(fn2);
            [f1,i1] = sort(fn1);
            [f2,i2] = sort(fn2);
            [ii,r2] = sort(i2);
            switch formatflag
                case {'latex','latex-longtable'}
                    myfprintf(fid,'\\STRUT %s field & %s & %s \\\\\n',substr,'','');
                    lend = '';
                    for i = 1:length(fn1)
                        if i == length(fn1)
                            lend = ' \hline';
                        end
                        if strcmp(fn1{i},fn2{i})
                            myfprintf(fid,'\\STRUT field %2i & %s & %s \\\\%s\n',i,sfn1(i,:),'[same]',lend);
                        else
                            myfprintf(fid,'\\STRUT field %2i & %s & %s \\\\%s\n',i,sfn1(i,:),sfn2(i,:),lend);
                        end
                    end
                otherwise
                    myfprintf(fid,['The order of fields in %s%s differs from those in %s%s:' br],var2,substr,var1,substr);
                    for i = 1:length(fn1)
                        if strcmp(fn1{i},fn2{i})
                            myfprintf(fid,['  %2i %s [same]' br],i,sfn1(i,:));
                        else
                            myfprintf(fid,['  %2i %s [%s] - %2i %s [%s]' br],i,sfn1(i,:),var1,i1(r2(i)),sfn2(i,:),var2);
                        end
                    end
            end
        end
end


function startEnvironment(fid,var1,var2,formatflag)
switch formatflag
    case 'latex'
        myfprintf(fid,'%s\n','\begin{tabular}{|l|l|l|}');
        myfprintf(fid,'%s\n',['\hline \STRUT & ',var1,' & ',var2,' \\ \hline']);
    case 'latex-longtable'
        myfprintf(fid,'%s\n','\begin{longtable}{|l|l|l|}');
        myfprintf(fid,'%s\n','\hiderowcolors');
        myfprintf(fid,'%s\n','\caption{Overview of the differences} \\');
        myfprintf(fid,'%s\n','\showrowcolors');
        myfprintf(fid,'%s\n','\hline');
        myfprintf(fid,'%s%s%s%s%s\n','\rowcolor{magenta!25!cyan!50} \STRUT & \textbf{',var1,'} & \textbf{',var2,'} \\ [1ex] \hline');
        myfprintf(fid,'%s\n','\endfirsthead');
        myfprintf(fid,'%s\n','%');
        myfprintf(fid,'%s\n','\hiderowcolors');
        myfprintf(fid,'%s\n','\multicolumn{3}{c}{{\STRUT \tablename\ \thetable{} -- continued from previous page}} \\ [1ex] \hline');
        myfprintf(fid,'%s\n','\showrowcolors');
        myfprintf(fid,'%s%s%s%s%s\n','\rowcolor{magenta!25!cyan!50} \STRUT & \textbf{',var1,'} & \textbf{',var2,'} \\ [1ex] \hline');
        myfprintf(fid,'%s\n','\endhead');
        myfprintf(fid,'%s\n','%');
        myfprintf(fid,'%s\n','\hline');
        myfprintf(fid,'%s\n','\hiderowcolors');
        myfprintf(fid,'%s\n','\multicolumn{3}{r}{{\STRUT \tablename \thetable{} -- continued on next page}} \\');
        myfprintf(fid,'%s\n','\showrowcolors');
        myfprintf(fid,'%s\n','\endfoot');
        myfprintf(fid,'%s\n','%');
        myfprintf(fid,'%s\n','\endlastfoot');
    otherwise
        % nothing to start
end


% -----------------------------------------------------------------------
%  Function used for recursive checking ...
% -----------------------------------------------------------------------
function DiffFound = detailedcheck(s1,s2,fid,formatflag,br,substr)
DiffFound = 0;
if ~isequal(class(s1),class(s2))  % different classes?
    DiffFound = 1;
    printdiff(fid,br,'class',formatflag,class(s1),class(s2),substr);
elseif ~isequal(size(s1),size(s2))  % different size?
    DiffFound = 1;
    printdiff(fid,br,'size',formatflag,size(s1),size(s2),substr);
elseif iscell(s1)  % & s2 is also cell! if cell -> check per element
    if ndims(s1)==2 && min(size(s1))==1
        % vector
        ivec = {[]};
    else
        ivec = cell(1,ndims(s1));
    end
    CellEqual = cellfun(@(x,y)isequal(x,y),s1,s2);
    for i=1:numel(s1)  % s2 has same size!
        if CellEqual(i)
            continue
        end
        if length(ivec)>1
            [ivec{:}] = ind2sub(size(s1),i);
            istr = sprintf('%i,',ivec{:});
            switch formatflag
                case {'latex','latex-longtable'}
                    str = sprintf('%s\\{%s\\}',substr,istr(1:end-1));
                otherwise
                    str = sprintf('%s{%s}',substr,istr(1:end-1));
            end
        else
            switch formatflag
                case {'latex','latex-longtable'}
                    str = sprintf('%s\\{%i\\}',substr,i);
                otherwise
                    str = sprintf('%s{%i}',substr,i);
            end
        end
        if ~isequal(s1{i},s2{i})
            Diff = detailedcheck(s1{i},s2{i},fid,formatflag,br,str);
            if Diff
                if ~DiffFound
                    DiffFound=Diff;
                else
                    DiffFound=min(Diff,DiffFound);
                end
            end
        end
    end
    if DiffFound
        DiffFound=DiffFound+1;
    end
elseif isstruct(s1) || isobject(s1)
    if isobject(s1)  % in case of objects convert into structures for detailed check
        s1=struct(s1);
        s2=struct(s2);
    end
    fn1=fieldnames(s1);
    fn2=fieldnames(s2);
    nf=length(fn1);
    [sfn1,i1] = sort(fn1);
    [sfn2,i2] = sort(fn2);
    if ~isequal(fn1,fn2) && ~isequal(sfn1,sfn2) % fieldnames the same?
        DiffFound = 1;
        printdiff(fid,br,'fieldnames',formatflag,fn1,fn2,substr);
        rfn1 = setdiff(fn1,fn2);
        rfn2 = setdiff(fn2,fn1);
        s1 = rmfield(s1,rfn1);
        s2 = rmfield(s2,rfn2);
        if fid == 0
            return
        end
        switch formatflag
            case {'latex','latex-longtable'}
                % print nothing
            otherwise
                myfprintf(fid,'Checking other fields ...\n')
        end
        fn1=fieldnames(s1);
        fn2=fieldnames(s2);
        nf=length(fn1);
        [sfn1,i1] = sort(fn1);
        [sfn2,i2] = sort(fn2);
    end
    if ~isequal(fn1,fn2) && isequal(sfn1,sfn2)
        s1=struct2cell(s1);
        s2=struct2cell(s2);
        s1 = s1(i1,:);
        s2 = s2(i2,:);
        fields = sfn1;
        printdiff(fid,br,'fieldnames',formatflag,fn1,fn2,substr);
    else
        s1=struct2cell(s1);
        s2=struct2cell(s2);
        fields = fn1;
    end
    j=0;
    for i=1:numel(s1)  % s2 has same size! (array size is the same and fields are the same)
        j=j+1;
        if j>nf
            j=1;
        end
        s1i = s1{i};
        s2i = s2{i};
        if ~isequal(s1i,s2i) || ~isequal(class(s1i),class(s2i)) 
            if numel(s1)~=nf
                Nsubstr=sprintf('%s(%i).%s',substr,(i-j)/nf+1,fields{j});
            else
                Nsubstr=sprintf('%s.%s',substr,fields{j});
            end
            Diff = detailedcheck(s1i,s2i,fid,formatflag,br,Nsubstr);
            if Diff
                Diff = Diff+1;
                if ~DiffFound
                    DiffFound=Diff;
                else
                    DiffFound=min(Diff,DiffFound);
                end
            end
        end
    end
else  % some numeric type of equal size
    if isempty(s1)  % same size, numeric, empty -> no difference
        return
    elseif isa(s1,'double') || isa(s1,'single')
        NaNorEqual=(isnan(s1) & isnan(s2)) | (s1==s2);
        DiffFound=~all(NaNorEqual(:));
    else
        DiffFound=~isequal(s1,s2);
    end
    if DiffFound
        printdiff(fid,br,'data',formatflag,s1,s2,substr);
    end
end

function myfprintf(fid,varargin)
if fid
    fprintf(fid,varargin{:});
end
