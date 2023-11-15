function Out = surfer(cmd,varargin)
%SURFER Read/write SURFER grid and blanking files.
%   FILEDATA = SURFER('open',FILENAME) opens the FILENAME as a SURFER file.
%   If the file extension is 'BLN', the file is opened as a blanking file
%   and as a grid file otherwise. For a blanking file the returned data
%   structure is compatible with Tekal landboundary files; all data will be
%   read. For grid files, the routine only determines the basic
%   characteristics such as the dimensions of the grid; in this case the
%   call does not immediately read the actual data.
%
%   DATA = SURFER('read',FILEDATA) read the data from a SURFER grid file
%   previously opened using an SURFER('open',FILENAME) call.
%
%   DATA = SURFER('read',FILENAME) opens the FILENAME and immediately
%   reads the data.
%
%   SURFER('write',FILENAME,FILEDATA,DATA)
%     *  FILENAME string indicating the name of the file to be written.
%     *  FILEDATA structure that mirrors the structure as obtained from a
%        SURFER('open',FILENAME) call.
%     *  DATA matrix which overrules the Data field of FILEDATA if
%        specified (only implemented for grid files).
%
%   See also BIL, ARCGRID, TEKAL.

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
    error('Too few input arguments specified')
end
switch cmd
    case 'open'
        Out=Local_open_file(varargin{:});
    case 'read'
        Out=Local_read_file(varargin{:});
    case 'write'
        Local_write_file(varargin{:});
    otherwise
        error('Unknown command: "%s"',cmd)
end


function Structure=Local_open_file(filename)
if (nargin==0) || strcmp(filename,'?')
    [fn,fp]=uigetfile('*.grd');
    if ~ischar(fn)
        return
    end
    filename=[fp fn];
end
[fp,fn,fe] = fileparts(filename);
%
fid=fopen(filename,'r','n','US-ASCII');
if fid<0
    error('Unable to open file "%s"',filename)
end
Exception = [];
Structure.FileName = filename;
try
    switch lower(fe)
        case '.bln'
            Structure.FileType = 'tekal';
            Structure.combinelines = 1;
            Structure=Local_open_blnfile(Structure,fid);
        otherwise
            Structure.FileType = 'SURFER';
            Structure=Local_open_grdfile(Structure,fid);
    end
catch Exception
end
fclose(fid);
if ~isempty(Exception)
    rethrow(Exception)
end


function Structure=Local_open_blnfile(Structure,fid)
%
% Blanking File Format
% 
% Blanking files [.BLN] are used to represent boundary information and are
% also used for blanking polygonal regions within grids. Blanking files are
% comma-delimited ASCII files consisting of a repeating series of polygon,
% polyline, or point definitions. Each definition has the following format:
% 
% npts, flag             This is the header for the first boundary in the
%                        blanking file. 
%                        npts: The number of XY coordinate pairs used to
%                              define this boundary.
%                        flag: Defines the type of blanking. If you are
%                              using this boundary as a blanking file, a
%                              value of 0 is used to blank outside the
%                              defined boundary, a value of 1 blanks inside
%                              the defined boundary.
% x1, y1                 first coordinate pair of the boundary
% x2, y2                 second coordinate pair
% ...                    coordinate pairs
% xn, yn                 This is the last coordinate pair of the boundary.
%                        For a closed polygon, this point must be identical
%                        to the first coordinate pair.
% 
% Multiple Polygons and Polylines
% 
% The sequence may be repeated any number of times within the same file to
% define multiple polygons and polylines. When using multiple polylines in
% a blanking operation, the flag value in the polygon header should be 1
% for all boundaries; otherwise, the entire grid is blanked. If you need to
% blank outside of multiple polygons, create a composite polygon to avoid
% blanking the entire grid.
% 
% Blanking Flag
% 
% The blanking flag parameter is used to determine blanking inside (1) or
% blanking outside (0). The XY coordinates used in the blanking file are in
% the same units used for the XY coordinates in the grid file. When
% creating a [.BLN] with the Digitize command, the default blanking flag is
% a 1. If you need to blank outside the polygon, open the [.BLN] file after
% saving, change the blanking flag to a 0, and save the [.BLN].
% 
% Optional Z coordinate
% 
% An optional Z coordinate may be specified after the X and Y coordinates
% for specifying breaklines. When specifying files that define breaklines,
% the flag parameter is ignored.
%
i = 1;
while ~feof(fid)
    % read first node
    headerline = fgetl(fid);
    [a,c]=sscanf(headerline,'%i%*[ ,]%i',3);
    switch c
        case 0
            error('Unable to interpret block header from BLN file:\n%s\nThis line should contain the number of points and the blanking flag.',headerline)
        case 1
            nPnts = a(1);
            a(2) = 1;
        case 2
            nPnts = a(1);
            if a(2)~=0 && a(2)~=1
                error('Unable to interpret block header from BLN file:\n%s\nUnknown blanking flag value "%i".',headerline,a(2))
            end
        case 3
            error('Unable to interpret block header from BLN file:\n%s\nExpected only two integers on this line.',headerline)
    end
    Structure.Field(i).Name = sprintf('Line %i',i);
    Structure.Field(i).Comments = {};
    Structure.Field(i).Flag = a(2);
    %
    % read first point
    firstpoint = fgetl(fid);
    [Pnt,nCoords]=sscanf(firstpoint,['%f%*[ ,' char(9) char(13) char(10) ']'],4);
    if nCoords<2
        error('Too few values on coordinate line (expecting 2 or 3):\n%s',firstpoint)
    elseif nCoords>3
        error('Too many values on coordinate line (expecting 2 or 3):\n%s',firstpoint)
    end
    Structure.Field(i).Size = [nPnts nCoords];
    Pnts = zeros(nPnts,nCoords);
    Pnts(1,:) = Pnt(:)';
    %
    % read all other points - not the quickest way, but allows any string
    % after the numbers
    for l = 2:nPnts
        line = fgetl(fid);
        Pnts(l,:) = sscanf(line,['%f%*[ ,' char(9) char(13) char(10) ']'],[1 nCoords]);
    end
    Structure.Field(i).Data = Pnts;
    Structure.Field(i).DataTp = 'numeric';
    %
    i = i+1;
end


function Structure=Local_open_grdfile(Structure,fid)
idHdr = fread(fid,[1 4],'*char');
Structure.Format  = idHdr;
Structure.Version = 1;
%
switch idHdr
    case 'DSAA' % SURFER ASCII grid
        fgetl(fid);
        %
        n = sscanf(fgetl(fid),'%i %i',2);
        Structure.NCols     = n(1);
        Structure.NRows     = n(2);
        %
        xlim = sscanf(fgetl(fid),'%f %f',2);
        ylim = sscanf(fgetl(fid),'%f %f',2);
        zlim = sscanf(fgetl(fid),'%f %f',2);
        %
        Structure.CellSize  = [(xlim(2)-xlim(1))/(Structure.NCols-1) (ylim(2)-ylim(1))/(Structure.NRows-1)];
        Structure.XCorner   = xlim(1) - Structure.CellSize(1)/2;
        Structure.YCorner   = ylim(1) - Structure.CellSize(2)/2;
        Structure.ZRange    = zlim;
        Structure.Rotation  = 0;
        Structure.NoData    = 1.70141e+38;
        %
        Structure.DataStart = ftell(fid);
    case 'DSBB' % SURFER 6 grid file
        % http://www.geospatialdesigns.com/surfer6_format.htm
        n = fread(fid,[1 2],'uint16=>double');
        Structure.NCols     = n(1);
        Structure.NRows     = n(2);
        %
        limits = fread(fid, [1 6],'double');
        xlim = limits(1:2);
        ylim = limits(3:4);
        zlim = limits(5:6);
        %
        Structure.CellSize  = [(xlim(2)-xlim(1))/(Structure.NCols-1) (ylim(2)-ylim(1))/(Structure.NRows-1)];
        Structure.XCorner   = xlim(1) - Structure.CellSize(1)/2;
        Structure.YCorner   = ylim(1) - Structure.CellSize(2)/2;
        Structure.ZRange    = zlim;
        Structure.Rotation  = 0;
        Structure.NoData    = 1.70141e+38;
        %
        Structure.DataStart = ftell(fid);
    case 'DSRB' % SURFER 7 grid file
        % http://www.geospatialdesigns.com/surfer7_format.htm
        szHdr   = fread(fid,1,'uint32=>double'); % 4
        Header  = fread(fid,[1 szHdr],'uint8');
        Version = double(Header(1)); % 1
        if Version~=1
            error('Version %i of SURFER 7 binary grid not yet supported',Version)
        elseif szHdr~=4
            error('Size of header (%i) does not match expected size (4)',szHdr)
        end
        Structure.Version = Version;
        %
        % Following blocks could be in any order
        %
        szDat = [];
        while ~feof(fid);
            idBlk = fread(fid,[1 4],'*char'); % GRID
            szBlk = fread(fid,1,'uint32=>double'); % 72
            switch idBlk
                case 'GRID' % grid info block
                    if szBlk~=72
                        error('Size of grid block (%i) does not match expected size (72)',szBlk)
                    end
                    n = fread(fid,[1 2],'uint32=>double');
                    Structure.NRows     = n(1);
                    Structure.NCols     = n(2);
                    x = fread(fid,[1 8],'double');
                    Structure.XCorner   = x(1) - x(3)/2;
                    Structure.YCorner   = x(2) - x(4)/2;
                    Structure.CellSize  = x(3:4);
                    Structure.ZRange    = x(5:6);
                    Structure.Rotation  = x(7);
                    Structure.NoData    = x(8);
                case 'DATA' % data block
                    szDat = szBlk;
                    Structure.DataStart = ftell(fid);
                    fseek(fid,szDat,0); % skip data
                case 'FLTI' % fault info block
            end
        end
        szDatExpect = 8*Structure.NRows*Structure.NCols;
        if isempty(szDat)
            error('Missing DATA block in SURFER DSRB file.')
        elseif szDat~=szDatExpect
            error('Size of data block (%i) does not match expected size (%i)',szDat,szDatExpect)
        end
    otherwise
        error('First 4 bytes not recognized: "%s"',idHdr)
end

Structure.x = Structure.XCorner+(0.5:Structure.NCols-0.5)*Structure.CellSize(1);
Structure.y = Structure.YCorner-(0.5:Structure.NRows-0.5)*Structure.CellSize(2)+(Structure.NRows)*Structure.CellSize(2);


function DATA = Local_read_file(filename)
if nargin==0
    Structure = Local_open_file('');
elseif ischar(filename)
    Structure = Local_open_file(filename);
else
    Structure = filename;
    if isfield(Structure,'Data')
        DATA = Structure.Data;
        return
    end
end
if ~strcmp(Structure.FileType,'SURFER')
    error('Specified file is not a SURFER grid file.')
end
fid = fopen(Structure.FileName,'r','n','US-ASCII');
fseek(fid,Structure.DataStart,-1);
sz = [Structure.NCols Structure.NRows];
switch Structure.Format
    case 'DSAA'
        DATA = fscanf(fid,'%f',sz);
    case 'DSBB'
        DATA = fread(fid,sz,'float32');
    case 'DSRB'
        DATA = fread(fid,sz,'double');
        DATA(DATA==Structure.NoData) = NaN;
end
DATA(DATA<Structure.ZRange(1) | DATA>Structure.ZRange(2)) = NaN;
fclose(fid);


function Local_write_file(varargin)
filename = varargin{1};
Structure = varargin{2};
fid = fopen(filename,'w','n','US-ASCII');
Exception = [];
try
    switch Structure.FileType
        case 'tekal'
            Local_write_blnfile(fid,varargin{2:end})
        case 'SURFER'
            Local_write_grdfile(fid,varargin{2:end})
    end
catch Exception
end
fclose(fid);
if ~isempty(Exception)
    rethrow(Exception)
end


function Local_write_blnfile(fid,Structure)
for i = 1:length(Structure.Field)
    if isfield(Structure.Field,'Flag') && ~isempty(Structure.Field(i).Flag)
        flag = Structure.Field(i).Flag;
    else
        flag = 1;
    end
    fprintf(fid,'%i,%i\n',size(Structure.Field(i).Data,1),flag);
    nCoords = size(Structure.Field(i).Data,2);
    switch nCoords
        case 2
            fprintf(fid,'%.15g,%.15g\n',Structure.Field(i).Data');
        case 3
            fprintf(fid,'%.15g,%.15g,%.15g\n',Structure.Field(i).Data');
        otherwise
            error('Number of columns equals %i; only 2 or 3 columns supported.',nCoords)
    end
        
end


function Local_write_grdfile(fid,Structure,DATA)
switch Structure.Format
    case 'DSAA'
        fprintf(fid,'DSAA\n');
        fprintf(fid,'%i %i\n', Structure.NCols,Structure.NRows);
        fprintf(fid,'%f %f\n', ...
            Structure.XCorner + Structure.CellSize(1)/2 + [0 (Structure.NCols-1)*Structure.CellSize(1)]);
        fprintf(fid,'%f %f\n', ...
            Structure.YCorner + Structure.CellSize(2)/2 + [0 (Structure.NRows-1)*Structure.CellSize(2)]);
        fprintf(fid,'%g %g\n', ...
            min(DATA(:)), ...
            max(DATA(:)));
        DATA(isnan(DATA)) = 1.70141e+38;
        fprintf(fid,' %g',DATA);
    case 'DSBB'
        fwrite(fid,'DSBB','char');
        fwrite(fid,[Structure.NCols Structure.NRows],'uint16');
        limits(6) = max(DATA(:));
        limits(5) = min(DATA(:));
        limits(4) = Structure.YCorner + (Structure.NRows-0.5)*Structure.CellSize(2);
        limits(3) = Structure.YCorner + 0.5*Structure.CellSize(2);
        limits(2) = Structure.XCorner + (Structure.NCols-0.5)*Structure.CellSize(1);
        limits(1) = Structure.XCorner + 0.5*Structure.CellSize(1);
        fwrite(fid,limits,'double');
        DATA(isnan(DATA)) = 1.70141e+38;
        fwrite(fid,DATA,'float32');
    case 'DSRB'
        fwrite(fid,'DSRB','char');
        fwrite(fid,4,'uint32');
        fwrite(fid,1,'uint32');
        %
        fwrite(fid,'GRID','char');
        fwrite(fid,72,'uint32');
        fwrite(fid,[Structure.NRows Structure.NCols],'uint32');
        limits(8) = Structure.NoData;
        limits(7) = Structure.Rotation;
        limits(6) = max(DATA(:));
        limits(5) = min(DATA(:));
        limits(4) = Structure.CellSize(2);
        limits(3) = Structure.CellSize(1);
        limits(2) = Structure.YCorner + 0.5*Structure.CellSize(2);
        limits(1) = Structure.XCorner + 0.5*Structure.CellSize(1);
        fwrite(fid,limits,'double');
        %
        fwrite(fid,'DATA','char');
        fwrite(fid,8*Structure.NRows*Structure.NCols,'uint32');
        DATA(isnan(DATA)) = Structure.NoData;
        fwrite(fid,DATA,'double');
end
