function tba = tekal2tba(Tekal)
%TEKAL2TBA Parses comments of a TEKAL to determine tidal analysis data.
%   TBA = TEKAL2TBA(TEKALFILE) parses the comments in a previously opened
%   TEKAL file to determine whether the file contains Delft3D-TRIANA
%   Table-A tidal analysis data. If it does contain such data it will
%   return a structure containing it.
%
%   TBA = TEKAL2TBA(FILENAME) opens the file and determines whether the
%   file contains Delft3D-TRIANA Table-A tidal analysis data. If it does
%   contain such data it will return a structure containing it.
%
%   See also TBA_PLOTELLIPSES.

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

if ischar(Tekal)
    Tekal = tekal('open',Tekal);
end
cmp = parse_tba(Tekal);

nfld = length(Tekal.Field);
stn = cell(1,nfld);
for t = 1:nfld
    switch cmp(t).Quant
        case 'water levels'
            stn{t} = Tekal.Field(t).Data(:,[1:2 2 3:4]);
            stn{t}(:,3) = -1;
        otherwise
            stn{t} = Tekal.Field(t).Data(:,1:5);
    end
end
Cmps = unique({cmp.Name});
Qnts = unique({cmp.Quant});
Locs = unique(cat(1,stn{:}),'rows');
Info = repmat(NaN,[length(Cmps) length(Qnts) size(Locs,1) 8]);

for t = 1:nfld
    icmp = strmatch(cmp(t).Name,Cmps,'exact');
    iqnt = strmatch(cmp(t).Quant,Qnts,'exact');
    [dummy,iloc] = ismember(stn{1},Locs,'rows');
    switch cmp(t).Quant
        case 'water levels'
            data = Tekal.Field(t).Data(:,5:end);
        otherwise
            data = Tekal.Field(t).Data(:,6:end);
    end
    Info(icmp,iqnt,iloc,:) = reshape(data,[1 1 size(data)]);
end
Info(Info==999.999) = NaN;

tba.FileName    = Tekal.FileName;
tba.Components  = Cmps;
tba.Quantities  = Qnts;
tba.Locations.M = Locs(:,1);
tba.Locations.N = Locs(:,2);
tba.Locations.K = Locs(:,3);
tba.Locations.X = Locs(:,4);
tba.Locations.Y = Locs(:,5);
tba.Statistics  = {...
    'computed amplitude','computed phase', ...
    'observed amplitude','observed phase', ...
    'amplitude difference','phase difference', ...
    'amplitude ratio','vector difference'};
tba.Info        = Info;

function component = parse_tba(Tekal)
nfld = length(Tekal.Field);
component(nfld).Name = '';
for t = 1:nfld
    C = Tekal.Field(t).Comments;
    for i = 1:length(C)
        cmp = sscanf(C{i},'%*[^T]Tidal component%*[^:]: %s',1);
        itd = sscanf(C{i},'%*[^I]ITDATE%*[^:]: %d/%d/%d %d:%d:%d',[1 6]);
        per = sscanf(C{i},'%*[^A]Analysed period%*[^:]: %f, %f',[1 2]);
        tz  = sscanf(C{i},'%*[^T]Time zone%*[^:]: %100c',1);
        qnt = sscanf(C{i},'%*[^>]>>%[^<]<<',1);
        qnt = strtrim(qnt);
        %
        if ~isempty(cmp)
            component(t).Name = cmp;
        elseif ~isempty(itd)
            component(t).RefDate = datenum(itd);
        elseif ~isempty(per)
            component(t).Period = component(t).RefDate + per/1440;
        elseif ~isempty(tz)
            component(t).TimeZone = tz;
        elseif ~isempty(qnt)
            component(t).Quant = qnt;
        end
    end
end
