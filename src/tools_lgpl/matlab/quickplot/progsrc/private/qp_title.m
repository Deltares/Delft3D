function qp_title(cmd,varargin)
%QP_TITLE Set automatic title and parameters.

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

if ~ischar(cmd)
    hAx = cmd;
    cmd = 'setauto';
    ops = varargin;
else
    % cmd = cmd;
    hAx = varargin{1};
    ops = varargin(2:end);
end

hTl = get(hAx,'title');
switch cmd
    case 'setauto'
        % store new string as titleauto string - encode line breaks
        Str = ops{1};
        if iscell(Str)
            Str_ = strjoin(Str(:)','\n{}');
        else
            Str_ = Str;
        end
        setappdata(hAx,'titleauto',Str_)

        % store additional parameters
        for i = 2:2:length(ops)
            setappdata(hAx,['title' ops{i}],ops{i+1})
        end

    case 'update'
        % no other input arguments, just update
        if ~isempty(ops)
            error('Too many input arguments')
        end
end

% check for user specified title
if isappdata(hAx,'title')
    Str = getappdata(hAx,'title');

% check for automatic title
elseif isappdata(hAx,'titleauto')
    Str = getappdata(hAx,'titleauto');

% fall back in case no title has been set
else
    Str = '';
end

% check if title contains any string that needs expansion
perc = strfind(Str,'%');
i = 2;
while i <= length(perc)
    substr = Str(perc(i-1)+1:perc(i)-1);
    key = ['title' substr];
    if isappdata(hAx,key)
        newstr = getappdata(hAx,key);
        Str = [Str(1:perc(i-1)-1) newstr Str(perc(i)+1:end)];
        i = i+1;
        perc(i:end) = perc(i:end) + length(newstr) - length(substr) - 2;
    end
    i = i+1;
end

% check for encoded line breaks
if strcontains(Str,'\n{}')
    Str = strsplit(Str,'\\n{}');
end

% update the string
set(hTl,'string',Str)


function TF = strcontains(Str,Pat) % Backward compatible version of contains
TF = ~isempty(strfind(Str,Pat));


function S = strjoin(C,sym)
if isempty(C)
    S = '';
else
    C = C(:)';
    C(2,:) = {sym};
    C{2,end} = '';
    S = strcat(C{:});
end