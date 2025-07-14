function varargout = imod(cmd,varargin)
%IMOF Reading of iMOD data files.
%   IMOD supports currently only idf-files.
%
%   F = IMOD('open', FILENAME) opens the file named FILENAME and parses the
%   content. It returns a data structure F representing the content of the
%   file.
%
%   DATA = IMOD('read', F, FIELD) retrieves the data for the specified FIELD
%   from either the data structure F or directly from the associated file, and
%   returns the array DATA. FIELD and DATA are can be
%       FIELD          | DATA
%       ------------------------------------------------------------------
%       'x'            | 1xC array of interface locations in X direction
%       'y'            | 1xR array of interface locations in Y direction
%       'z'            | 1x2 array containing top/bottom values (if available)
%       'val'          | (R-1)x(C-1) array of cell centre values
%
%   [DATA1, DATA2, ...] = IMOD('read', F, FIELD1, FIELD2, ...) generalized form
%   returning data for multiple fields.
%
%   See also QPFOPEN, QPREAD.

%----- LGPL --------------------------------------------------------------------
%                                                                               
%   Copyright (C) 2011-2025 Stichting Deltares.                                     
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

switch lower(cmd)
    case 'open'
        S = file_opener(varargin{:});
        if nargout > 0
            varargout = {S};
        end

    case 'read'
        [varargout{1:nargout}] = file_reader(varargin{:});
end


function S = file_opener(filename)
switch check_filetype(filename)
    case 'iMOD-idf'
        S = idf_opener(filename);
end


function filetype = check_filetype(filename)
if ~exist(filename,'file')
    error('File "%s" not found.', filename)
end
% assume that it's always an IDF file ...
filetype = 'iMOD-idf';


function S = idf_opener(filename)
S.filetype = 'iMOD-idf';
S.filename = filename;
[S.name,S.date,S.layer] = interpret_idf_filename(filename);
fid = fopen(filename,'r','l'); % always little endian
if fid < 0
    error('Unable to open the file "%s".', filename)
end
try
    fs = fread(fid,1,'uint32');
    switch fs
        case 1271
            S.precision = 'float32';
            S.n_bytes = 4;
        case 2295
            S.precision = 'float64';
            S.n_bytes = 8;
        otherwise
            error('Unexpected value %i for IDF record length.',fs)
    end
    S.n_columns = fread(fid, 1, 'uint32');
    S.n_rows = fread(fid, 1, 'uint32');
    S.x_range = fread(fid, [1,2], S.precision);
    S.y_range = fread(fid, [1,2], S.precision);
    S.val_range = fread(fid, [1,2], S.precision);
    S.no_data = fread(fid, 1, S.precision);
    tmp = fread(fid,4,'uint8');
    S.equidistant = ~tmp(1);
    S.use_top_bot = tmp(2);
    if S.equidistant
        S.column_width = fread(fid, 1, S.precision);
        S.row_height = fread(fid, 1, S.precision);
    end
    if S.use_top_bot
        S.z_range = fliplr(fread(fid, [1,2], S.precision));
    end
    if ~S.equidistant
        S.column_width = fread(fid, [1,S.n_columns], S.precision);
        S.row_height = fread(fid, [1,S.n_rows], S.precision);
    end
    % skip the data
    S.offset = ftell(fid);
    fseek(fid, S.n_rows*S.n_columns*S.n_bytes, 0);
    % check optional arguments
    tmp = fread(fid,1,'uint8');
    if tmp == 1
        nline = fread(fid, 1, 'uint8');
        S.comment = fread(fid, [1,4*nline], 'uchar');
    end
    fclose(fid);
catch e
    fclose(fid);
    rethrow(e)
end


function varargout = file_reader(S, varargin)
switch S.filetype
    case 'iMOD-idf'
        [varargout{1:nargout}] = idf_reader(S, varargin{:});
end


function varargout = idf_reader(S, varargin)
data = cell(1,nargout);
for i = 1:min(nargin-1,nargout)
    switch varargin{i}
        case 'x'
            if S.equidistant
                data{i} = S.x_range(1) + (0:S.n_columns)*S.column_width;
            else
                data{i} = S.x_range(1) + [0,cumsum(S.column_width)];
            end

        case 'y'
            if S.equidistant
                data{i} = S.y_range(1) + (0:S.n_rows)*S.row_height;
            else
                data{i} = S.y_range(1) + [0,cumsum(S.row_height)]';
            end

        case 'z'
            if isfield(S,'z_range')
                data{i} = S.z_range;
            else
                data{i} = [];
            end

        case 'val'
            fid = fopen(S.filename);
            fseek(fid, S.offset, 0);
            val = fread(fid, [S.n_columns,S.n_rows], S.precision).';
            val(val == S.no_data) = NaN;
            data{i} = val;
            fclose(fid);
    end
end
varargout = data;

function [name,date,layer] = interpret_idf_filename(filename)
[~,f] = fileparts(filename);
parts = strsplit(f,'_');

name = parts{1};

if length(parts) > 1
    try
        date = datenum(sscanf(parts{2},'%4d%2d%2d%2d%2d%2d',[1,6]));
    catch
        date = [];
    end
else
    date = [];
end

if length(parts) > 2 && strcmp(parts{3},'L')
    layer = str2double(parts{3}(2:end));
else
    layer = [];
end