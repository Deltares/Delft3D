function S=readswan(FileName)
%READSWAN Read SWAN 1D and 2D spectral files.

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

S.FileName=FileName;
S.FileType='SWAN spectral';
S.Check='NotOK';
fid = fopen(FileName,'r','n','US-ASCII');
if fid<0
    error('Cannot open %s',FileName)
end
%
% Check first line and stop
%
Line = fgetl(fid);
Version = sscanf(Line,'SWAN %f',1);
if isempty(Version)
    fclose(fid);
    return
end
S.Version = Version;
if Version>1
    warning(sprintf('SWAN spectral version %g not yet supported!',Version))
end
%
% Skip Comments
%
Line = fgetl(fid);
iqnt = 1;
iloc = 0;
TimeCoding = -999;
ct = 0; % current time point
Zero = [];
while ischar(Line)
    if isempty(Line)
        %
        % Skip empty lines although there probably shouldn't be any except
        % maybe at the end of the file.
        %
    elseif Line(1)=='$'
        %
        % Skip lines starting with $
        %
    else
       key = upper(strtok(Line));
        switch key
            case 'TIME'
                % time coding option
                Line = fgetl(fid);
                TimeCoding = sscanf(Line,'%i',1);
            case {'LOCATIONS','LONLAT'}  % locations in x-y-space
                % number of locations
                Line = fgetl(fid);
                NLoc = sscanf(Line,'%i',1);
                % coordinates
                S.LocationsXY = fscanf(fid,'%f',[2 NLoc])';
                if strcmp(key,'LONLAT')
                   S.UnitXY = 'deg';
                else
                   S.UnitXY = 'm';
                end
                fgetl(fid);
                %
            case {'AFREQ','RFREQ'} % absolute frequencies in Hz
                switch upper(strtok(Line))
                    case 'AFREQ'
                        S.FreqType = 'absolute';
                    case 'RFREQ'
                        S.FreqType = 'relative';
                end
                % number of frequencies
                Line = fgetl(fid);
                NFreq = sscanf(Line,'%i',1);
                % frequencies
                S.Frequencies = fscanf(fid,'%f',[1 NFreq]);
                fgetl(fid);
                %
            case {'NDIR','CDIR'} % spectral nautical or Cartesian directions in degr
                switch upper(strtok(Line))
                    case 'NDIR'
                        S.DirectionType = 'nautical';
                    case 'CDIR'
                        S.DirectionType = 'Cartesian';
                end
                % number of directions
                Line = fgetl(fid);
                NDir = sscanf(Line,'%i',1);
                % directions
                S.Directions = fscanf(fid,'%f',[1 NDir]);
                fgetl(fid);
                %
            case 'QUANT'
                % number of quantities in table
                Line = fgetl(fid);
                NQuant = sscanf(Line,'%i',1);
                for i=1:NQuant
                    % EnDens             energy densities in J/m2/Hz/degr
                    QuantLine = fgetl(fid);
                    S.Quant(i).Name = strtok(QuantLine);
                    % J/m2/Hz/degr       unit
                    UnitLine = fgetl(fid);
                    S.Quant(i).Unit = strtok(UnitLine);
                    %    -0.9900E+02     exception value
                    ExceptLine = fgetl(fid);
                    S.Quant(i).MissingValue = sscanf(ExceptLine,'%f',1);
                end
                if isfield(S,'Directions')
                    S.Spectrum = cell(NQuant,NLoc);
                else
                    S.Spectrum = cell(NLoc,1);
                end
                %
            case 'FACTOR'
                f = fscanf(fid,'%f',1);
                iloc = iloc+1;
                if iloc>NLoc
                    iloc = 1;
                    iqnt = iqnt+1;
                end
                [Data,NVal] = fscanf(fid,'%f',[NDir NFreq]);
                if NVal~=NDir*NFreq
                    Line = fgetl(fid);
                    fclose(fid);
                    error('Unexpected ''%s'' in spectral block in file %s',deblank(Line),S.FileName)
                end
                Data = f*Data';
                ct = max(ct,1);
                S.Spectrum{iqnt,iloc,ct} = Data;
                fgetl(fid);
                %
            case 'ZERO'
                iloc = iloc+1;
                if iloc>NLoc
                    iloc = 1;
                    iqnt = iqnt+1;
                end
                ct = max(ct,1);
                if isempty(Zero)
                    Zero = zeros(NFreq,NDir);
                end
                S.Spectrum{iqnt,iloc,ct} = Zero;
                %
            case 'NODATA'
                iloc = iloc+1;
                if iloc>NLoc
                    iloc = 1;
                    iqnt = iqnt+1;
                end
                %
            case 'LOCATION'
                iloc = sscanf(Line,' LOCATION %i',1);
                Data = fscanf(fid,'%f',[NQuant NFreq])';
                for i=1:NQuant
                    Data(Data(:,i)==S.Quant(i).MissingValue,i)=NaN;
                end
                ct = max(ct,1);
                S.Spectrum{iloc,ct} = Data;
                fgetl(fid);
                %
            otherwise
                DateTime = scan_for_datetime(Line,TimeCoding);
                if isempty(DateTime)
                    fclose(fid);
                    error('Unknown keyword %s in file %s',strtok(Line),S.FileName)
                else
                    iloc = 0;
                    iqnt = 1;
                    ct = ct+1;
                    S.Time(ct) = datenum(DateTime);
                end
        end
    end
    Line = fgetl(fid);
end
fclose(fid);
% remove NODATA stations
Empty = cellfun('isempty',S.Spectrum);
%
S.Check='OK';

function DateTime = scan_for_datetime(Str,Format)
DateTime = [];
two_digit_year = 1;
switch Format
    case -999
        % no format defined yet
    case 1
        % YYYYMMDD.HHMMSS
        DateTime = sscanf(Str,'%4d%2d%2d.%2d%2d%2d',[1 6]);
        if length(DateTime)~=6
            DateTime = [];
        end
        two_digit_year = 0;
    case 2
        % YY-MMM-DD.HH:MM:SS
        DateTime = sscanf(Str,'%2d-%3c-%2d.%2d:%2d:%2d',[1 6]);
        if length(DateTime)~=8
            DateTime = [];
        else
            month = strmatch(char(DateTime(2:4)),{'Jan','Feb','Mar','Apr', ...
                'May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'});
            if isempty(month)
                DateTime = [];
            else
                DateTime = [DateTime(1) month DateTime(5:end)];
            end
        end
    case 3
        % DD/MM/YY.HH:MM:SS
        DateTime = sscanf(Str,'%2d/%2d/%2d.%2d:%2d:%2d',[1 6]);
        if length(DateTime)~=6
            DateTime = [];
        else
            DateTime = DateTime([3 2 1 4 5 6]);
        end
    case 4
        % HH:MM:SS
        DateTime = sscanf(Str,'%2d:%2d:%2d',[1 3]);
        if length(DateTime)~=3
            DateTime = [];
        else
            DateTime = [0 0 0 DateTime];
        end
    case 5
        % YY/MM/DD.HH.MM.SS
        DateTime = sscanf(Str,'%2d/%2d/%2d.%2d:%2d:%2d',[1 6]);
        if length(DateTime)~=6
            DateTime = [];
        end
    case 6
        % YYMMDDHHSS
        DateTime = sscanf(Str,'%2d%2d%2d%2d%2d',[1 5]);
        if length(DateTime)~=5
            DateTime = [];
        else
            DateTime(6) = 0;
        end
end
%
if two_digit_year & ~isempty(DateTime)
    if DateTime(1)<60
        DateTime(1) = DateTime(1)+2000;
    else
        DateTime(1) = DateTime(1)+1900;
    end
end
