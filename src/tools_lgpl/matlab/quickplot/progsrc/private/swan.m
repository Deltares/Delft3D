function varargout=swan(cmd,varargin)
%SWAN Read/write SWAN files.

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
    error('Missing command.')
end

switch lower(cmd)
    case 'read'
        try
            Data = Local_read_swan(varargin{:});
            try
                [pn,fn]=fileparts(Data.FileName);
                DataIn = Local_read_swanin(fullfile(pn,'INPUT'));
                Data.Parameters = DataIn.Parameters;
            catch
            end
        catch
            try
                DataIn = Local_read_swanin(varargin{1});
                Data = Local_read_swan(DataIn.TableOutput,varargin{2});
                Data.Parameters = DataIn.Parameters;
            catch
            end
        end
        varargout = {Data};
        %case 'write'
    otherwise
        error('Unknown command: %s',cmd)
end


function FI = Local_read_swan(filename,dims)
fid = fopen(filename,'r','n','US-ASCII');
if fid<0
    error('Error opening file: %s',filename)
end
FI.FileName = filename;
FI.FileType = 'SWAN-output';
FI.Comments = {};
%
% Scan header (optional)
%
IsComment = 1;
while IsComment
    loc = ftell(fid);
    Line = fgetl(fid);
    if ~ischar(Line) || length(Line)<1
        IsComment = 0;
    else
        IsComment = Line(1)=='%';
        if IsComment
            FI.Comments{end+1} = Line;
        end
    end
end
fseek(fid,loc,-1);
%
% Count number of values in the data line
%
temp = sscanf(Line,'%f');
nvalperline = length(temp);
if nvalperline==0
    fclose(fid);
    error('No data found in Line ''%s''.',Line)
end
%
% Read data from file
%
[FI.Data, Nread] = fscanf(fid,'%f',[nvalperline inf]);
fclose(fid);
%
% Check data read
%
temp = Nread/nvalperline;
if nargin<2
    dims = temp;
end
if temp ~= round(temp)
    error('Invalid number of values in file.')
elseif temp < prod(dims)
    error('Insufficient number of values in file.')
elseif temp > prod(dims)
    error('Too many values in file.')
end
%
FI.Data = FI.Data';
FI.Data = reshape(FI.Data,[dims nvalperline]);
for i=1:nvalperline
    FI.Parameters{i} = sprintf('field %i of %s',i,FI.FileName);
end

function FI = Local_read_swanin(filename)
fid = fopen(filename,'r','n','US-ASCII');
if fid<0
    error('Error opening file: %s',filename)
end
FI.FileName = filename;
FI.FileType = 'SWAN-input';
FI.Comments = {};
%
Line = '';
while ischar(Line) && isempty(Line)
    Line = fgetl(fid);
end
if ~ischar(Line)
    fclose(fid);
    error('No data in file: %s',filename)
elseif length(Line)>200
    fclose(fid);
    error('First line too long (>200): %s',filename)
elseif ~ismember(Line(1),['A':'Z' 'a':'z' ' $'])
    fclose(fid);
    error('Invalid first character ''%s''',Line(1))
end
TableLine = 0;
while ~TableLine
    Line = fgetl(fid);
    if ~ischar(Line)
        fclose(fid);
        error('No table record found in file: %s',filename)
    else
        TableLine = ~isempty(strmatch('tab',lower(strtrim(Line))));
    end
end
%
Line = strtrim(Line);
while ~isempty(strmatch(Line(end-1:end),{' _',' &'},'exact'))
    Line2 = fgetl(fid);
    Line2 = strtrim(Line2);
    Line = [Line(1:end-1) Line2];
end
quotes=strfind(Line,'''');
%
[pn,fn]=fileparts(FI.FileName);
FI.TableOutput = fullfile(pn,Line(quotes(3)+1:quotes(4)-1));
FI.Parameters = {};
%
Pars = Line(quotes(4)+1:end);
keyw = {
    'HSWE'   'HSWELL'
    'HS'     'HSIGN'
    'DIR'    'DIR'
    'PDIR'   'PDIR'
    'TDIR'   'TDIR'
    'TM01'   'TM01'
    'RTM01'  'RTM01'
    'RTP'    'RTP'
    'PER'    'PER'
    'RPER'   'RPER'
    'TMM10'  'TMM10'
    'RTMM10' 'RTMM10'
    'TM02'   'TM02'
    'FSPR'   'FSPR'
    'DSPR'   'DSPR'
    'DEP'    'DEPTH'
    'VEL'    'VEL'
    'FRC'    'FRCOEF'
    'WIND'   'WIND'
    'DISS'   'DISSIP'
    'QB'     'QB'
    'TRA'    'TRANSP'
    'FOR'    'FORCE'
    'UBOT'   'UBOT'
    'URMS'   'URMS'
    'WLEN'   'WLENGTH'
    'STEE'   'STEEPNESS'
    'DHS'    'DHSIGN'
    'DRTM01' 'DRTM01'
    'LEAK'   'LEAK'
    'XP'     'XP'
    'YP'     'YP'
    'DIST'   'DIST'
    'SETUP'  'SETUP'
    'OUT'    '*OUTPUT'};
while ~isempty(Pars)
    [Par,Pars] = strtok(Pars);
    Par = upper(Par);
    %
    found = 0;
    for i=1:size(keyw,1)
        keyword = keyw{i,1};
        lkw = length(keyword);
        if length(Par)>=lkw && strcmp(Par(1:lkw),keyword)
            found = 1;
            break
        end
    end
    %
    if ~found
        error('Unexpected keyword: %s.',Par)
    end
    %
    keyword = keyw{i,2};
    switch keyword
        case '*OUTPUT'
            break
        case {'VEL','FORCE','TRANSP'}
            FI.Parameters(end+1:end+2)={[keyword '-X'],[keyword '-Y']};
        otherwise
            FI.Parameters{end+1}=keyword;
    end
end
%
fclose(fid);
