function [Sout,Ssep]=multiline(Sin,varargin)
%MULTILINE Converts a string containing LineFeeds to a char matrix.
%   STRMAT = MULTILINE(S,C,...) splits the input string S at the characters
%   listed in the char vector C. Each section forms a line of the char
%   matrix STRMAT (left aligned). Enter \n for char(10):linefeed and \t for
%   char(9):tab. If C is not specified, the string S is split at linefeeds
%   (i.e. the default value of C is char(10).
%
%   The options may contain one or more of the following keyword-value
%   pairs:
%    * 'empty': value 'keep' indicates that empty strings are kept in the
%               output of STRMAT (separator at begin or end of string, or
%               multiple consecutive separators). Use value 'skip' to
%               remove empty strings. The default value is 'keep'.
%    * 'output': The value 'char' indicates output in the form of an NxM
%               char matrix where N is the number of substrings and M is
%               the (maximum) length of the substrings. The value 'cell'
%               converts the input string into an Nx1 cell string instead
%               of a char matrix. The value 'cellrow' converts the input
%               string into an 1xN cell string instead  of a char matrix,
%               i.e. MULTILINE(...,'cell')'. The default value is 'char'.
%
%   Example:
%      Str=multiline(sprintf('%i\n',1:10:31),'empty','skip')
%      % gives the 4 x 2 char matrix
%
%      Str = 1
%            11
%            21
%            31
%
%     By default (or with empty set to keep) the resulting char matrix
%     would be 5 x 2 and include one extra blank line since the original
%     string ends in \n.
%
%   See also CELLSTR, STR2MAT.

%   Old syntax without keyword 'output':
%
%   CELSTR = MULTILINE(...,'cell') converts the input string into an Nx1
%   cell string instead of a char matrix.
%
%   CELSTR = MULTILINE(...,'cellrow') converts the input string into an 1xN
%   cell string instead of a char matrix, i.e. MULTILINE(...,'cell')'.

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

cArray=char(10);
cellOutput=0;
keepEmpty=1;
if nargin>1
    Inp=varargin;
    i = 1;
    while i < length(Inp)
        if ischar(Inp{i})
            switch lower(Inp{i})
                case 'empty'
                    switch lower(Inp{i+1})
                        case 'keep'
                            keepEmpty = 1;
                        case 'skip'
                            keepEmpty = 0;
                        otherwise
                            error('Expecting value "keep" or "skip" after keyword "empty".')
                    end
                    Inp(i:i+1) = [];
                case 'output'
                    switch lower(Inp{i+1})
                        case 'char'
                            cellOutput = 0;
                        case 'cell'
                            cellOutput = 1;
                        case 'cellrow'
                            cellOutput = 2;
                        otherwise
                            error('Expecting value "char", "cell" or "cellrow" after keyword "output".')
                    end
                    Inp(i:i+1) = [];
                otherwise
                    % string not recognized as keyword
                    i = i+1;
            end
        end
    end
    %
    % backward compatible support for some values without keyword
    %
    if isequal('cellrow',lower(Inp{end}))
        cellOutput = 2;
        Inp(end)   = [];
    elseif isequal('cell',lower(Inp{end}))
        cellOutput = 1;
        Inp(end)   = [];
    end
    %
    if length(Inp)>1
        error('Too many input arguments.')
    elseif length(Inp)==1
        cArray = Inp{1};
        cArray = strrep(cArray,'\t',char(9));
        cArray = strrep(cArray,'\n',char(10));
    end
end
if nargin<1 || ~ischar(Sin)
    error('Invalid input argument.')
elseif isempty(Sin)
    if cellOutput
        Sout={};
    else
        Sout='';
    end
    if nargout>1
        Ssep='';
    end
elseif ndims(Sin)~=2 || min(size(Sin))~=1
    error('Invalid input argument.')
else
    LineFeed=find(ismember(Sin,cArray));
    if nargout>1
        Ssep=Sin(LineFeed);
    end
    Start=[1 LineFeed+1];
    End=[LineFeed-1 length(Sin)];
    %
    if ~keepEmpty
        NE = Start<=End;
        Start = Start(NE);
        End   = End(NE);
    end
    %
    if cellOutput
        if cellOutput==2
            Sout=cell(1,length(Start));
        else
            Sout=cell(length(Start),1);
        end
        for k=1:length(Start)
            Sout{k}=deblank(Sin(Start(k):End(k)));
        end
    else
        maxStrL=max(End-Start)+1;
        Sout=repmat(' ',length(Start),maxStrL);
        for k=1:length(Start)
            Sout(k,1:(End(k)-Start(k)+1))=Sin(Start(k):End(k));
        end
    end
end
