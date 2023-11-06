function FI = waterml2(cmd,varargin)
%WATERML2 Read WaterML 2.0 files.
%   STRUCT = WATERML2('open',FILENAME) opens the selected WaterML 2.0 file.
%
%   See also XMLREAD.

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
        FI = LocalWaterML2Open(varargin{:});
    otherwise
        error('Unknown command: %s',var2str(cmd)) 
end

function FI = LocalWaterML2Open(FileName)
FI.FileName = FileName;
FI.FileType = 'WaterML2';
% First check whether this is an XML file to prevent the error message:
% [Fatal Error] FileName:1:1: Content is not allowed in prolog.
fid = fopen(FileName,'r');
firstchar = fread(fid,[1 1],'*char');
fclose(fid);
if ~isequal(firstchar,'<')
    error('WaterML2 file should start with <?xml')
end
FI.XML = xmlread(FileName);
%
Doc        = FI.XML.getFirstChild;
NameSpaces = xparse('getNameSpaces',Doc);
%
try
    FI.Dictionary = getDictionary(Doc,NameSpaces);
catch
    FI.Dictionary = [];
end
try
    FI.Location = getLocations(Doc,NameSpaces);
catch
    FI.Location = [];
end
FI.TimeSeries = getTimeSeries(Doc,NameSpaces,FI.Location);

function D = getDictionary(Doc,NameSpaces)
D.XML = xparse('getRecursiveNamedChildNS',Doc,NameSpaces,wml2,'localDictionary',gml,'Dictionary',gml,'dictionaryEntry');
nQ = length(D.XML);
D.Name = cell(nQ,1);
for i = 1:nQ
    Name = xparse('getRecursiveNamedChildNS',D.XML(i),NameSpaces,gml,'Definition',gml,'name');
    D.Name{i} = char(Name.getTextContent);
end

function L = getLocations(Doc,NameSpaces)
L.XML = xparse('getRecursiveNamedChildNS',Doc,NameSpaces,wml2,'samplingFeatureMember',wml2,'MonitoringPoint');
nL = length(L.XML);
L.Id = cell(nL,1);
L.Name = cell(nL,1);
L.Descr = cell(nL,1);
L.Coord = zeros(nL,2);
for i = 1:nL
    [L.Id{i},L.Name{i},L.Descr{i},L.Coord(i,1:2)] = getMonitoringPoint(L.XML(i),NameSpaces);
end

function [Id,Name,Description,Coord] = getMonitoringPoint(MP,NameSpaces)
Id= xparse('getAttributeNS',MP,NameSpaces,gml,'id');
Name = xparse('getChar',xparse('getRecursiveNamedChildNS',MP,NameSpaces,gml,'name'));
Description = xparse('getChar',xparse('getRecursiveNamedChildNS',MP,NameSpaces,gml,'description'));
if isempty(Name)
    SF = xparse('getRecursiveNamedChildNS',MP,NameSpaces,sa,'sampledFeature');
    [Name,err] = xparse('getAttributeNS',SF,NameSpaces,xlink,'title');
end
Coord = xparse('getRecursiveNamedChildNS',MP,NameSpaces,sams,'shape',gml,'Point',gml,'pos');
Coord = sscanf(char(Coord.getTextContent),'%f',[1 2]);

function TS = getTimeSeries(Doc,NameSpaces,Locations)
TS.XML = xparse('getRecursiveNamedChildNS',Doc,NameSpaces,wml2,'observationMember');
nTS = length(TS.XML);
if nTS==0
    error('At least one observationMember element required in WaterML2 file.')
end
TS.Name = cell(nTS,1);
TS.QuantityName = cell(nTS,1);
TS.QuantityUnit = cell(nTS,1);
TS.LocationName = cell(nTS,1);
TS.LocationCoord = NaN(nTS,2);
TS.Series = cell(nTS,1);
for i = 1:nTS
    TSeries  = xparse('getRecursiveNamedChildNS',TS.XML(i),NameSpaces,om,'OM_Observation');
    if length(TSeries)>1
        error('Only one OM_Observation element allowed in observationMember.')
    end
    P = xparse('getRecursiveNamedChildNS',TSeries,NameSpaces,om,'observedProperty');
    TS.QuantityName{i} = xparse('getAttributeNS',P,NameSpaces,xlink,'title');
    %
    L = xparse('getRecursiveNamedChildNS',TSeries,NameSpaces,om,'featureOfInterest');
    if length(L)==1
        MP = xparse('getRecursiveNamedChildNS',L,NameSpaces,wml2,'MonitoringPoint');
        if ~isempty(MP)
            [Id,Name,Description,Coord] = getMonitoringPoint(MP,NameSpaces);
            if ~isempty(Name)
                TS.LocationName{i} = Name;
            elseif ~isempty(Description)
                TS.LocationName{i} = Description;
            end
            TS.LocationCoord(i,:) = Coord;
        end
        %
        if isempty(TS.LocationName{i})
            [TS.LocationName{i},err] = xparse('getAttributeNS',L,NameSpaces,xlink,'title');
            if err
                [TS.LocationName{i},err] = xparse('getAttributeNS',L,NameSpaces,xlink,'href');
                if err || isempty(TS.LocationName{i})
                    TS.LocationName{i} = '';
                elseif TS.LocationName{i}(1)=='#' && ~isempty(Locations)
                    idxLoc = strcmp(TS.LocationName{i}(2:end),Locations.Id);
                    if sum(idxLoc)==1
                        if ~isempty(Locations.Name{idxLoc})
                            TS.LocationName{i} = Locations.Name{idxLoc};
                        elseif ~isempty(Locations.Descr{idxLoc})
                            TS.LocationName{i} = Locations.Descr{idxLoc};
                        else
                            TS.LocationName{i} = TS.LocationName{i}(2:end);
                        end
                        TS.LocationCoord(i,:) = Locations.Coord(idxLoc,:);
                    end
                end
            end
        end
    else
        TS.LocationName{i} = '';
    end
    %
    D = xparse('getRecursiveNamedChildNS',TSeries,NameSpaces,om,'result',wml2,'MeasurementTimeseries');
    TS.Name{i} = xparse('getAttributeNS',D,NameSpaces,gml,'id');
    %
    try
        % equidistant time series
        BeginTime = iso8601datetime(xparse('getRecursiveNamedChildNS',D,NameSpaces,wml2,'metadata',wml2,'MeasurementTimeseriesMetadata',wml2,'baseTime'));
        SP = xparse('getRecursiveNamedChildNS',D,NameSpaces,wml2,'metadata',wml2,'MeasurementTimeseriesMetadata',wml2,'spacing');
        TimeStep = iso8601period(char(SP.getTextContent));
    catch
        BeginTime = [];
        TimeStep = [];
    end
    %
    U = xparse('getRecursiveNamedChildNS',D,NameSpaces,wml2,'defaultPointMetadata',wml2,'DefaultTVPMeasurementMetadata',wml2,'uom');
    TS.QuantityUnit{i} = char(U.getAttribute('code'));
    TS.Series{i} = getTimeSeriesData(D,NameSpaces,BeginTime,TimeStep);
end

function TSD = getTimeSeriesData(D,NameSpaces,BeginTime,TimeStep)
TSV = xparse('getNamedChildrenNS',D,NameSpaces,wml2,'point');
nTSV = length(TSV);
TSD = NaN(nTSV,2);
noData = true(nTSV,1);
for i = 1:nTSV
    TSVi = xparse('getNamedChildNS',TSV(i),NameSpaces,wml2,'MeasurementTVP');
    if isempty(BeginTime)
        TSD(i,1) = iso8601datetime(xparse('getNamedChildNS',TSVi,NameSpaces,wml2,'time'));
        noData(i) = false;
    else
        TSD(i,1) = datenum(datevec(BeginTime) + (i-1)*TimeStep);
    end
    try
        V = xparse('getNamedChildNS',TSVi,NameSpaces,wml2,'value');
        TSD(i,2) = str2double(char(V.getTextContent));
        noData(i) = false;
    catch
    end
end
if any(noData)
    TSD(noData,:)=[];
end
%----------------------

function P = iso8601period(S)
% P[n]Y[n]M[n]DT[n]H[n]M[n]S or P[n]W
% The smallest value used may also have a decimal fraction, which may be
% specified with either a comma or a full stop.
%
% P[YYYY]-[MM]-[DD]T[hh]:[mm]:[ss]
P = zeros(1,6);
S = strrep(S,',','.');
if S(1)~='P'
    error('Unable to interpret period string "%s"',S)
else
    [V,n,err,idx] = sscanf(S(2:end),'%f%1s');
    if round(n/2)*2~=n
        error('Unable to interpret period string "%s"',S)
    elseif ~isempty(err)
        s = S(1+idx);
    else
        s = '';
    end
    if strcmp(s,'') || strcmp(s,'T')
        if strcmp(s,'T')
            [V2,n,err] = sscanf(S(2+idx:end),'%f%1s');
            if ~isempty(err) || round(n/2)*2~=n
                error('Unable to interpret period string "%s"',S)
            end
        else
            V2 = [];
        end
        for i = 1:2:length(V)
            switch char(V(i+1))
                case 'Y'
                    P(1) = V(i);
                case 'M'
                    P(2) = V(i);
                case 'D'
                    P(3) = V(i);
                otherwise
                    error('Unable to interpret period string "%s"',S)
            end
        end
        for i = 1:2:length(V2)
            switch char(V2(i+1))
                case 'H'
                    P(4) = V2(i);
                case 'M'
                    P(5) = V2(i);
                case 'S'
                    P(6) = V2(i);
                otherwise
                    error('Unable to interpret period string "%s"',S)
            end
        end
    elseif strcmp(s,'-')
        [P,n,err] = sscanf(S,'P%4d-%2d-%2dT%2d:%2d:%2d',[1 6]);
        if ~isempty(err) || n<6
            error('Unable to interpret period string "%s"',S)
        end
    else
        error('Unable to interpret period string "%s"',S)
    end
end

function [T,TZshift] = iso8601datetime(S)
str = char(S.getTextContent);
[datetime,N,err,i] = sscanf(str,'%4i-%2d-%2dT%2d:%2d:%2d',[1 6]);
if N==1
    [datetime,N,err,i] = sscanf(str,'%4i%2d%2dT%2d%2d%2d',[1 6]);
end
TZ = deblank(str(i:end));
if isempty(TZ) || strcmp(TZ,'Z')
    TZshift = 0;
else
    [TZshift,NTZ,err] = sscanf(TZ,'%1[+-]%d:%d',[1 3]);
    switch NTZ
        case 0
            err='Missing time zone sign';
        case 1
            err='Missing time zone hour';
        case 2
            if TZshift(2)>100
                TZshift(2) = fix(TZshift(2)/100)+rem(TZshift(2),100)/60;
            end
            TZshift = (44-TZshift(1))*TZshift(2);
        case 3
            TZshift = (44-TZshift(1))*(TZshift(2)+TZshift(3)/60);
    end
end
if ~isempty(err)
    error('%s while parsing time string "%s"',err,str)
end
if nargout==1
    T = datenum(datetime); % ignoring time zone for the moment
else
    T = datenum(datetime - [0 0 0 TZshift 0 0]);
end

function wml2 = wml2
wml2 = 'http://www.opengis.net/waterml/2.0';

function gml = gml
gml = 'http://www.opengis.net/gml/3.2';

function om = om
om = 'http://www.opengis.net/om/2.0';

function xlink = xlink
xlink = 'http://www.w3.org/1999/xlink';

function sa = sa
sa = 'http://www.opengis.net/sampling/2.0';

function sams = sams
sams = 'http://www.opengis.net/samplingSpatial/2.0';
