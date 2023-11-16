function varargout=fopen(varargin)
%FOPEN  Open file.
%   This is a wrapper function for the builtin FOPEN for debugging purposes.
%   This function should support all the functionality provided by the original
%   function (help description available via iofun/fopen) with one addition:
%   this version keeps track of the call stacks when FOPEN is used to open a
%   file.
%
%   FIDS = FOPEN('all') returns like the original FOPEN function a row vector
%   containing the file identifiers for all the files currently opened by the
%   user (but not 1 or 2). However, it also prints for each file identifier the
%   the call stack of the time when it was opened.
%
%   To suppress warnings about name conflict type:
%   warning('off','MATLAB:dispatcher:nameConflict')
%
%   See also: iofun/fopen

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

nout = max(1,nargout);
[varargout{1:nout}] = builtin('fopen',varargin{:});
if isnumeric(varargout{1}) && ~isempty(varargout{1})
    OpenedFiles = getappdata(0,'fopen_debug_callstacks');
    fid = varargout{1};
    if strcmpi(varargin{1},'all')
        for i=1:length(fid)
            fprintf('FILE HANDLE = %i: ''%s''\n',fid(i),builtin('fopen',fid(i)));
            if fid(i)<=length(OpenedFiles)
                stack = OpenedFiles{fid(i)};
                if isempty(stack)
                    fprintf('  The associated fopen statement has not been recorded.\n')
                elseif ischar(stack)
                    fprintf('  The associated fopen statement was called from the command line.\n')
                else
                    for j=1:length(stack)
                        [p,f,e]=fileparts(stack(j).file);
                        cmd=sprintf('matlab: opentoline(''%s'',%i,1)',stack(j).file,stack(j).line);
                        if strcmp(f,stack(j).name)
                            fprintf('  In <a href="%s">%s at %i</a>\n',cmd,f,stack(j).line)
                        else
                            fprintf('  In <a href="%s">%s>%s at %i</a>\n',cmd,f,stack(j).name,stack(j).line)
                        end
                    end
                end
            else
                fprintf('  The associated fopen statement has not been recorded.\n')
            end
        end
    elseif fid>0
        stack = dbstack(1,'-completenames');
        if isempty(stack)
            OpenedFiles{fid} = 'command line';
        else
            OpenedFiles{fid} = stack;
        end
        setappdata(0,'fopen_debug_callstacks',OpenedFiles)
    end
end
