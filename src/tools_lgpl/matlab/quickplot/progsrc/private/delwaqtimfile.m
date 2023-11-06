function Struct = delwaqtimfile(FileName);
%DELWAQTIMFILE Reads in a Delwaq .tim input file (Lex Yacc type).

%   Any spaces, tabs and commas (outside strings) should be ignored. For the
%   time being I assume that there are no commas; the rest is handled
%   correctly I think.

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

Struct.FileName = FileName;
Struct.FileType = 'DelwaqTimFile';
fid = fopen(FileName,'r','n','US-ASCII');
i = 0;
Table(1).Name='First Table';
while ~feof(fid)
    i = i+1;
    %
    % Table(i).TimeUnit = 'autodetect';
    %
    % Keywords other than parameters
    %
    while ~feof(fid)
        Line = fgetl(fid);
        if ~ischar(Line)
            fclose(fid);
            error('End of file reached while searching for ''parameter'' keyword')
        end
        [keyword,value] = strtok(Line);
        switch lower(keyword)
            case {'*','#'}
                % skip comment line
            case 'table-name'
                Table(i).Name = deblank(sscanf(value,' ''%[^'']'));
            case 'contents'
                Table(i).Contents = deblank(sscanf(value,' ''%[^'']')); %uniform, linear, step, 3d-profile
            case 'geo-coordinates'
                Table(i).GeoCoords = sscanf(value,' %f',[1 3]);
            case 'location'
                Table(i).Location = deblank(sscanf(value,' ''%[^'']'));
            case 'interpolation'
                Table(i).Interpolation = deblank(sscanf(value,' ''%[^'']')); % linear, block
            case 'extrapolation'
                Table(i).Extrapolation = deblank(sscanf(value,' ''%[^'']')); % periodic, constant, none
            case 'records-in-table'
                Table(i).NRecords = sscanf(value,'%i');
            case 'metric' % metric coordinates
                [keyword,value] = strtok(value); % remove string 'coordinates'
                [Table(i).MetricCoords,n,e,ii] = sscanf(value,' %f',[1 3]); % could be followed by layer
                if ii<length(value)
                    value=value(ii:end);
                    k = sscanf(value,'layer %f',1);
                    if ~isempty(k)
                        Table(i).Layer = k;
                    end
                end
            case 'layer'
                Table(i).Layer = sscanf(value,' %f');
            case 'time-unit'
                TUnit_str = deblank(sscanf(value,' ''%[^'']'));
                switch lower(TUnit_str)
                    case 'date'
                        %yyyymmdd hhmmss
                    case 'years'
                    case 'decades'
                    case 'ddhhmmss'
                    case {'day','days'}
                        Table(i).TimeUnit = 1;
                    case {'hour','hours','hr','hrs'}
                        Table(i).TimeUnit = 1/24;
                    case {'minute','minutes','min','mins'}
                        Table(i).TimeUnit = 1/1440;
                    case {'second','seconds','sec','secs','s'}
                        Table(i).TimeUnit = 1/86400;
                    otherwise
                        fclose(fid);
                        error(['Unknown time unit: ',TUnit_str]);
                end
            case 'time-step'
                Table(i).TimeStep = sscanf(value,' %f');
            case 'reference-time'
                RefTime = sscanf(value,' %i %f');
                Y = floor(RefTime(1)/10000);
                M = floor(RefTime(1)/100-Y*100);
                D = floor(RefTime(1)-Y*10000-M*100);
                if length(RefTime)>1
                    h = floor(RefTime(2)/10000);
                    m = floor(RefTime(2)/100-h*100);
                    s = floor(RefTime(2)-h*10000-m*100);
                else
                    h = 0;
                    m = 0;
                    s = 0;
                end
                Table(i).RefTime = datenum(Y,M,D,h,m,s);
            case 'constant'
                Table(i).TimeFunc = 'constant';
            case 'time-function'
                Table(i).TimeFunc = deblank(sscanf(value,' ''%[^'']')); % astronomic, harmonic, equidistant, non-equidistant
            case 'parameter'
                break
            case '*'
            otherwise
                fclose(fid);
                error('Unkown keyword: %s',keyword)
        end
    end
    %
    % Parameters
    %
    startpoint = [];
    j = 0;
    while ~feof(fid)
        switch lower(keyword)
            case 'parameter'
                j = j+1;
                [par,N,errmsg,nextidx] = sscanf(value,' ''%[^'']''');
                Table(i).Parameter(j).Name = deblank(par);
                unit = sscanf(value(nextidx:end),' unit ''%[^'']');
                Table(i).Parameter(j).Unit = deblank(unit);
            case '*'
            otherwise
                break
        end
        %
        startpoint = ftell(fid);
        Line = fgetl(fid);
        [keyword,value] = strtok(Line);
    end
    %
    % Data
    %
    Table(i).StartPoint = startpoint;
    fseek(fid,startpoint,-1);
    Table(i).Data = fscanf(fid,' %f',[length(Table(i).Parameter) inf])';
end
fclose(fid);
Struct.Table = Table;



function simpleloctable(FileName)
Struct.FileName = FileName;
Struct.FileType = 'DelwaqTimFile';
fid = fopen(FileName,'r','n','US-ASCII');
Line = fgetl(fid);
lowLine = lower(Line);
[value,n,e,i] = sscanf(lowLine,'location , x , y , z , time');
if ~isempty(e)
    fclose(fid);
    error('First line of file does not start with correct text!')
end
%
%Location, X, Y, Z, Time, "Salinity","NH4","Sups.solids","RcNit"
%                         "g/l", "mg/l", "mg/l","1/d"
%                         1.0,1.0,1.0,1.0
%"Location 1", 15000., 20000., 0., 2000/01/01-00:00:00, 30.0, 1.0, 100.0,0.1
%                                  2000/02/01-00:01:00, 31.0, 1.1, 120.0,0.15
%                                  2000/03/04-12:53:24, 28.0, 3.1, 120.0,0.12
%"Location 2", 15000., 15000., 1., 2000/01/01-00:00:00, 30.4, 1.0, 100.0,0.25
%                                  2000/02/01-00:00:00, 31.4, 1.1, 120.0,0.3
%
%1. line: quantity names
%2. line: unit strings
%3. line: scaling factos
%following lines: "location",x,y,z,yyyy/mm/ss-hh:mm:ss, values
% values only for second and following lines of same location
%
fclose(fid);
