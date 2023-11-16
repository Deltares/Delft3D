function [varargout] = aukepc(cmd,varargin)
%AUKEPC Read AUKE/pc files.
%
%   FileInfo = AUKEPC('open','FileName');
%
%   Data = AUKEPC('read',FileInfo,Channel,UseZeroLevel);
%        where Channel is either a channel name or one or
%        more channel numbers. If UseZeroLevel is 0 the
%        zerolevel is not used, otherwise it is used (default).

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

switch lower(cmd)
    case 'open'
        varargout = cell(1,max(nargout,1));
        [varargout{:}] = Local_aukepc_open(varargin{:});
    case 'read'
        varargout = cell(1,max(nargout,1));
        [varargout{:}] = Local_aukepc_read(varargin{:});
end


function FI = Local_aukepc_open(filename)
% Open AUKE/pc file
% INPUT: filename
% OUTPUT: FileInfo about the sequences in the data file.

if nargin<1
    [fn,pn] = uigetfile('*.seq');
    if ~ischar(fn)
        FI = [];
        return
    end
    filename = [pn fn];
end

% By now we should have the final filename. Has it an extension?
lastdot = max(strfind(filename,'.'));
lastsep = max(strfind(filename,filesep));
extension = '';
lwc = 0;
if ~isempty(lastdot) & (isempty(lastsep) | (lastdot>lastsep)) % has extension!
    extension = filename(lastdot:end);
    lwc = extension-upper(extension);
    filebase = filename(1:(lastdot-1));
else
    filebase = filename;
end
FI.FileName = filebase;
FI.Extensions = {char('.SEQ'+lwc) char('.DAT'+lwc)};
fid = fopen(strcat(filebase,FI.Extensions{1}),'r','n','US-ASCII');
Conv0 = 0;
Conv1 = 1;
zerolev = 0;

Ch = 0;

while ~feof(fid)
    Line = upper(fgetl(fid));
    if length(Line) >= 6
        switch Line(1:6)
            case 'A/D-CO' % A/D-CONV
                FI.LowStored = Local_getseq(Line,'LOWSTORED');
                FI.HighStored = Local_getseq(Line,'HIGHSTORED');
                FI.LowUsed = Local_getseq(Line,'LOWUSED');
                FI.HighUsed = Local_getseq(Line,'HIGHUSED');
            case 'DATATY' % DATATYPE
                if ~isempty(strfind(Line,'R4'))
                    FI.Type = 'R4';
                    FI.Frmt = 'float32';
                elseif ~isempty(strfind(Line,'I2'))
                    FI.Type = 'I2';
                    FI.Frmt = 'int16';
                end
            case 'EQ,SER' % EQ,SERIES
                FI.StartTime = Local_getseq(Line,'LOW');
                FI.EndTime = Local_getseq(Line,'HIGH');
                FI.Freq = Local_getseq(Line,'FREQ');
                if ~isempty(FI.Freq),
                    FI.Dt = 1/FI.Freq;
                else
                    FI.Dt = Local_getseq(Line,'STEP');
                end
            case 'SERIES' % SERIES
                Ch = Ch+1;
                FI.Channel(Ch).Name = strtok(Line(8:end));
            case 'CALIBR' % CALIBR
                if Ch==0
                    error('Reading CALIBR before SERIES')
                end
                FI.Channel(Ch).Conv1 = Local_getseq(Line,'C1');
                FI.Channel(Ch).Conv0 = Local_getseq(Line,'C0');
            case 'ZEROLE' % ZEROLEVEL
                if Ch==0
                    error('Reading ZEROLEVEL before SERIES')
                end
                FI.Channel(Ch).ZeroLvl = sscanf(Line(111:end),'%f',1);
        end
    end
end
for Ch=1:length(FI.Channel)
    if ~strcmp(FI.Type,'R4')
        FI.Channel(Ch).Conv1 = FI.Channel(Ch).Conv1 * (FI.HighUsed-FI.LowUsed) / ...
            (FI.HighStored-FI.LowStored);
        FI.Channel(Ch).Conv0 = FI.Channel(Ch).Conv0 + 0.5 *( ...
            - (FI.LowStored + FI.HighStored) * FI.Channel(Ch).Conv1 ...
            + FI.LowUsed + FI.HighUsed);
    else
        FI.Channel(Ch).Conv1 = 1;
        FI.Channel(Ch).Conv0 = 0;
    end
end
fclose(fid);


function x = Local_getseq(Str,keyword)
i = min(strfind(Str,keyword));
if isempty(i)
    x = [];
    return
end
j = min(strfind(Str(i:end),'='));
if isempty(j)
    x = [];
    return
end
i = i+j;
x = sscanf(Str(i:end),'%f',1);


function Data = Local_aukepc_read(FI,Channel,UseZLvl)
% Read AUKE/pc file
% INPUT: FI = FileInfo as obtained from AUKEPC('open',...)
%        Channel = Channel number(s) to be read
%        UseZLvl = Correct zerolevel

if nargin<3
    UseZLvl  =[];
end
if ischar(Channel)
    Channels = {FI.Channel.Name};
    i = ustrcmpi(Channel,Channels);
    if i<0
        error('Channel %s does not exist',Channel)
    end
    Ch = i;
elseif (Channel>length(FI.Channel)) | (Channel<=0)
    error('Channel number invalid')
else
    Ch = Channel;
end
Ch = Ch(:);
fid = fopen(strcat(FI.FileName,FI.Extensions{2}),'r');

nsamp = round((FI.EndTime-FI.StartTime)/FI.Dt+1);
nCh = length(FI.Channel);
nChRead = length(Ch);

if strcmp(FI.Type,'R4') %16384/4
    SegL = 4046;
else %16384/2
    SegL = 8192;
end

i = 0;
Data = zeros(nsamp,nChRead);
while i < nsamp
    jMax = min(nsamp-i,SegL);
    r = fread(fid,[nCh jMax],FI.Frmt);
    Data(i+1:i+jMax,:) = r(Ch,:)';
    i = i+jMax;
end

fclose(fid);

for i = 1:nChRead
    if ~strcmp(FI.Type,'R4')
        Data(:,i) = Data(:,i)*FI.Channel(Ch(i)).Conv1+FI.Channel(Ch(i)).Conv0;
    end
    if isempty(UseZLvl) | UseZLvl
        if isfield(FI.Channel(Ch(i)),'ZeroLevel')
            Data(:,i) = Data(:,i)-FI.Channel(Ch(i)).ZeroLevel;
        end
    end
end
