function varargout=shipmafil(FI,domain,field,cmd,varargin)
%SHIPMAFIL QP support for Shipma project files.
%   Domains                 = XXXFIL(FI,[],'domains')
%   DataProps               = XXXFIL(FI,Domain)
%   Size                    = XXXFIL(FI,Domain,DataFld,'size')
%   Times                   = XXXFIL(FI,Domain,DataFld,'times',T)
%   StNames                 = XXXFIL(FI,Domain,DataFld,'stations')
%   SubFields               = XXXFIL(FI,Domain,DataFld,'subfields')
%   [TZshift   ,TZstr  ]    = XXXFIL(FI,Domain,DataFld,'timezone')
%   [Data      ,NewFI]      = XXXFIL(FI,Domain,DataFld,'data',subf,t,station,m,n,k)
%   [Data      ,NewFI]      = XXXFIL(FI,Domain,DataFld,'celldata',subf,t,station,m,n,k)
%   [Data      ,NewFI]      = XXXFIL(FI,Domain,DataFld,'griddata',subf,t,station,m,n,k)
%   [Data      ,NewFI]      = XXXFIL(FI,Domain,DataFld,'gridcelldata',subf,t,station,m,n,k)
%                             XXXFIL(FI,[],'options',OptionsFigure,'initialize')
%   [NewFI     ,cmdargs]    = XXXFIL(FI,[],'options',OptionsFigure,OptionsCommand, ...)
%
%   The DataFld can only be either an element of the DataProps structure.

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
%   All values and logos of, and references to, "Delft3D" and "Deltares"
%   are registered trademarks of Stichting Deltares, and remain the property of
%   Stichting Deltares. All rights reserved.
%
%-------------------------------------------------------------------------------
%   http://www.deltaressystems.com
%   $HeadURL$
%   $Id$

%========================= GENERAL CODE =======================================

T_=1; ST_=2; M_=3; N_=4; K_=5;

if nargin<2
    error('Not enough input arguments')
elseif nargin==2
    varargout={infile(FI,domain)};
    return
elseif ischar(field)
    switch field
        case 'options'
            [varargout{1:2}]=options(FI,cmd,varargin{:});
        case 'domains'
            varargout={domains(FI)};
        case 'dimensions'
            varargout={dimensions(FI)};
        case 'locations'
            varargout={locations(FI)};
        case 'quantities'
            varargout={quantities(FI)};
        case 'getparams'
            varargout={getparams(FI,domain)};
        case 'data'
            [varargout{1:2}]=getdata(FI,cmd,varargin{:});
    end
    return
else
    Props=field;
end

cmd=lower(cmd);
switch cmd
    case 'size'
        varargout={getsize(FI,Props)};
        return
    case 'times'
        varargout={readtim(FI,Props,varargin{:})};
        return
    case 'timezone'
        [varargout{1:2}]=gettimezone(FI,domain,Props);
        return
    case 'stations'
        varargout={{}};
        return
    case 'subfields'
        varargout={{}};
        return
    case 'plotoptions'
        varargout = {[]};
        return
    case 'plot'
        selfplot(FI,Props);
        varargout={[] FI};
        return
    otherwise
        [XYRead,DataRead,DataInCell]=gridcelldata(cmd);
end

DimFlag=Props.DimFlag;

% initialize and read indices ...
idx={[] [] 0 0 0};
fidx=find(DimFlag);
idx(fidx(1:length(varargin)))=varargin;

sz=getsize(FI,Props);
if DimFlag(T_)
    if isempty(idx{T_})
        idx{T_}=sz(T_);
    end
elseif all(Props.Var>=0)
    idx{T_}=0;
end

prj = Props.Project;
PRJ = FI.Project(prj);
cse = Props.Case;
scn = Props.Scenery;
man = Props.Manoeuvre;
Name = Props.Name;
if cse==0
    i = findstr(Name,':');
    if isempty(i)
        Name = strtok(Name);
    else
        Name = Name(i+2:end);
    end
end

switch Name
    case 'ship snapshots'
        step = qp_settings('shipma_timestep');
        Series = PRJ.Cases.Data(cse).TimeSeries;
        times = delwaq('read',Series,1,1,0)*3600*24;
        timereq = 0:step:times(end)+1;
        for i=1:length(timereq)
            [mn,timereq(i)]=min(abs(times-timereq(i)));
        end
        idx{1}=timereq;
end

% read data ...
if all(Props.Var>=0)
    [T,val1] = delwaq('read',PRJ.Cases.Data(cse).TimeSeries,Props.Var,1,idx{1});
else
    T=[];
end

% generate output ...
if Props.NVal==0
    switch Name
        case 'fairway contour'
            FileName = PRJ.Sceneries.Data(scn).fairwayContourFile;
            if exist(FileName,'file')
                [Ans.X,Ans.Y] = landboundary('read',FileName,'autocorrect');
            else
                Ans.X=[];
                Ans.Y=[];
            end
        case 'bank suction lines'
            bsFI = tekal('open',PRJ.Sceneries.Data(scn).banksuctionFile,'nskipdatalines',1);
            XY = tekal('read',bsFI,1:2);
            Ans.X = [XY{1}(:,1);NaN;XY{2}(:,1)];
            Ans.Y = [XY{1}(:,2);NaN;XY{2}(:,2)];
        case {'desired ship track','route waypoints'}
            if cse>0
                [Ans.X,Ans.Y] = landboundary('read',PRJ.Cases.Data(cse).trackFile,'autocorrect');
            else
                Ans.X = PRJ.Manoeuvres.Data(man).track(:,1);
                Ans.Y = PRJ.Manoeuvres.Data(man).track(:,2);
            end
        case 'distance ticks'
            step = qp_settings('shipma_spacestep');
            width = qp_settings('shipma_tickwidth');
            if qp_settings('shipma_distance_along_desired_track')
                [x,y] = landboundary('read',PRJ.Cases.Data(cse).trackFile,'autocorrect');
            else
                x = squeeze(val1(1,1,:));
                y = squeeze(val1(2,1,:));
            end
            d = pathdistance(x,y);
            doublepoints = find(diff(d)==0)+1;
            x(doublepoints) = [];
            y(doublepoints) = [];
            d(doublepoints) = [];
            dtick = (0:step:max(d))';
            xtick = interp1(d,x,dtick);
            ytick = interp1(d,y,dtick);
            itick = min(length(d)-1,floor(interp1(d,1:length(d),dtick)));
            dx = diff(x);
            dy = diff(y);
            ds = sqrt(dx.^2+dy.^2);
            dx = dx./ds;
            dy = dy./ds;
            hwidth = width/2;
            x0 = [xtick-hwidth*dy(itick) xtick+hwidth*dy(itick)];
            y0 = [ytick+hwidth*dx(itick) ytick-hwidth*dx(itick)];
            x0 = x0';
            x0(3,:) = NaN;
            y0 = y0';
            y0(3,:) = NaN;
            Ans.X = x0(:);
            Ans.Y = y0(:);
        case 'realized ship track'
            Ans.X = squeeze(val1(1,1,:));
            Ans.Y = squeeze(val1(2,1,:));
        case 'swept path'
            val1 = squeeze(val1)';
            x = val1(:,1);
            y = val1(:,2);
            alf = val1(:,3)*pi/180;
            lat_offset = val1(:,4);
            swept_port = val1(:,5) - lat_offset;
            swept_star = val1(:,6) - lat_offset;
            sppx = x + cos(alf).*swept_port;
            sppy = y - sin(alf).*swept_port;
            spsx = x + cos(alf).*swept_star;
            spsy = y - sin(alf).*swept_star;
            Ans.X = [sppx;spsx(end-1:-1:1); sppx(1)];
            Ans.Y = [sppy;spsy(end-1:-1:1); sppy(1)];
        case {'ship','ship snapshots'} 
            ship = PRJ.Cases.Data(cse).shipNr;
            icontour = ustrcmpi('contour',{PRJ.Ships.Data(ship).Props.Quant});
            contour = PRJ.Ships.Data(ship).Props(icontour).Value;
            %
            lenC = size(contour,1);
            numS = size(val1,3);
            Ans.X = repmat(NaN,(lenC+1)*numS-1,1);
            Ans.Y = Ans.X;
            for i = 1:numS
                alf = val1(3,1,i)*pi/180;
                Ans.X((i-1)*(lenC+1)+(1:lenC)) = contour(:,1)*sin(alf)+contour(:,2)*cos(alf)+val1(1,1,i);
                Ans.Y((i-1)*(lenC+1)+(1:lenC)) = -contour(:,2)*sin(alf)+contour(:,1)*cos(alf)+val1(2,1,i);
            end
        case 'ship at distance ticks'
            ship = PRJ.Cases.Data(cse).shipNr;
            icontour = ustrcmpi('contour',{PRJ.Ships.Data(ship).Props.Quant});
            contour = PRJ.Ships.Data(ship).Props(icontour).Value;
            %
            step = qp_settings('shipma_spacestep');
            if qp_settings('shipma_distance_along_desired_track')
                d = val1(4,:);
            else
                d = pathdistance(val1(1,:),val1(2,:));
            end
            dtick = (0:step:max(d))';
            itick = crossings(d,dtick);
            i = 1:size(val1,3);
            xtick = interp1(i,val1(1,:),itick);
            ytick = interp1(i,val1(2,:),itick);
            atick = val1(3,floor(itick));
            atic2 = val1(3,ceil(itick));
            for i = 1:length(itick)
                f = itick(i)-floor(itick(i));
                if atick(i)>270 && atic2(i)<90
                    atick(i) = (atick(i)-360)*(1-f) + atick(i)*f;
                    if atick(i)<0
                        atick(i) = atick(i)+360;
                    end
                elseif atick(i)<90 && atic2(i)>270
                    atick(i) = atick(i)*(1-f) + (atick(i)-360)*f;
                    if atick(i)<0
                        atick(i) = atick(i)+360;
                    end
                else
                    atick(i) = atick(i)*(1-f) + atick(i)*f;
                end
            end
            %
            lenC = size(contour,1);
            numS = length(xtick);
            Ans.X = repmat(NaN,(lenC+1)*numS-1,1);
            Ans.Y = Ans.X;
            for i = 1:numS
                alf = atick(i)*pi/180;
                Ans.X((i-1)*(lenC+1)+(1:lenC)) = contour(:,1)*sin(alf)+contour(:,2)*cos(alf)+xtick(i);
                Ans.Y((i-1)*(lenC+1)+(1:lenC)) = -contour(:,2)*sin(alf)+contour(:,1)*cos(alf)+ytick(i);
            end
    end
elseif Props.NVal==1
    switch Name
        case 'depth'
            if cse>0
                btFN = PRJ.Cases.Data(cse).bottomFile;
            else
                btFN = PRJ.Sceneries.Data(scn).bottomFile;
            end
            btFI = samples('read',btFN);
            Ans.XYZ = reshape(btFI.XYZ,[1 size(btFI.XYZ,1) 1 3]);
            Ans.TRI = delaunay(btFI.XYZ(:,1),btFI.XYZ(:,2));
            Ans.Val = btFI.XYZ(:,3);
        case 'ship speed'
            Ans.X   = val1(3,:)';
            Ans.Val = sqrt(val1(1,:).^2 + val1(2,:).^2)';
            if ~qp_settings('shipma_distance_along_desired_track')
                Ans.X = realized_track_distance(PRJ,cse,idx);
            end
        otherwise
            Ans.X   = val1(2,:)';
            Ans.Val = val1(1,:)';
            if strcmp(Name,'propeller speed') % convert from rev/s to rev/min
                Ans.Val = Ans.Val*60;
            end
            if ~qp_settings('shipma_distance_along_desired_track')
                Ans.X = realized_track_distance(PRJ,cse,idx);
            end
    end
elseif Props.NVal==4
    switch Name
        case 'distance value at ticks'
            step = qp_settings('shipma_spacestep');
            if qp_settings('shipma_distance_along_desired_track')
                [x,y] = landboundary('read',PRJ.Cases.Data(cse).trackFile,'autocorrect');
            else
                x = squeeze(val1(1,1,:));
                y = squeeze(val1(2,1,:));
            end
            d = pathdistance(x,y);
            doublepoints = find(diff(d)==0)+1;
            x(doublepoints) = [];
            y(doublepoints) = [];
            d(doublepoints) = [];
            dtick = (0:step:max(d))';
            Ans.X = interp1(d,x,dtick);
            Ans.Y = interp1(d,y,dtick);
            Ans.Val = cell(size(dtick));
            for i = 1:length(dtick)
                Ans.Val{i} = sprintf('%i',dtick(i));
            end
    end
elseif Props.NVal==2
    switch Name
        case 'ship speed'
            Ans.X = val1(3,:)';
            Ans.XComp = val1(1,:)';
            Ans.YComp = val1(2,:)';
        case 'wind'
            if cse>0
                fld = PRJ.Cases.Data(cse).windNr;
            else
                fld = Props.Manoeuvre;
            end
            wFI = shipma('openpar',PRJ.Environments.Winds.Data(fld).file,'wind');
            Ans.XYZ = reshape(wFI.XY,[1 size(wFI.XY,1) 1 2]);
            Ans.TRI = delaunay(wFI.XY(:,1),wFI.XY(:,2));
            toDir = wFI.WindFromDir*pi/180-pi;
            Ans.XComp = wFI.WindMagnitude.*sin(toDir);
            Ans.YComp = wFI.WindMagnitude.*cos(toDir);
        case 'waves'
            if cse>0
                fld = PRJ.Cases.Data(cse).wavesNr;
            else
                fld = Props.Manoeuvre;
            end
            wFI = shipma('openpar',PRJ.Environments.Waves.Data(fld).file,'waves');
            Ans.XYZ = reshape(wFI.XY,[1 size(wFI.XY,1) 1 2]);
            Ans.TRI = delaunay(wFI.XY(:,1),wFI.XY(:,2));
            toDir = wFI.WaveToDir*pi/180;
            Ans.XComp = wFI.WaveHeight.*sin(toDir);
            Ans.YComp = wFI.WaveHeight.*cos(toDir);
        case 'swell'
            if cse>0
                fld = PRJ.Cases.Data(cse).swellNr;
            else
                fld = Props.Manoeuvre;
            end
            wFI = shipma('openpar',PRJ.Environments.Swells.Data(fld).file,'swell');
            Ans.XYZ = reshape(wFI.XY,[1 size(wFI.XY,1) 1 2]);
            Ans.TRI = delaunay(wFI.XY(:,1),wFI.XY(:,2));
            toDir = wFI.WaveToDir*pi/180;
            Ans.XComp = wFI.WaveHeight.*sin(toDir);
            Ans.YComp = wFI.WaveHeight.*cos(toDir);
        case 'current'
            if cse>0
                fld = PRJ.Cases.Data(cse).currentNr;
            else
                fld = Props.Manoeuvre;
            end
            wFI = shipma('openpar',PRJ.Environments.Currents.Data(fld).file,'current');
            Ans.XYZ = reshape(wFI.XY,[1 size(wFI.XY,1) 1 2]);
            Ans.TRI = delaunay(wFI.XY(:,1),wFI.XY(:,2));
            toDir = wFI.CurrentToDir*pi/180;
            Ans.XComp = wFI.CurrentMagnitude.*sin(toDir);
            Ans.YComp = wFI.CurrentMagnitude.*cos(toDir);
        otherwise
            Ans.XComp = val1;
            Ans.YComp = val2;
    end
end
if isfield(Ans,'X') || isfield(Ans,'XYZ')
    Ans.XUnits = 'm';
end
if isfield(Ans,'Y') || isfield(Ans,'XYZ')
    Ans.YUnits = 'm';
end
if ~isempty(T)
    Ans.Time = T;
end

varargout={Ans FI};
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function PAR = getparams(FI,varargin)
if nargin==2
    domain = varargin{1};
    prj    = FI.Case.Project(domain);
    cse    = FI.Case.Case(domain);
elseif nargin==3
    prj    = varargin{1};
    cse    = varargin{2};
end
%
PAR.organization = protectstring(qp_settings('organizationname'));
PAR.filename     = protectstring(FI.FileName);
PAR.project      = protectstring(FI.Project(prj).Name);
PAR.case         = protectstring(FI.Project(prj).Cases.Names{cse});
PAR.ship         = protectstring(FI.Project(prj).Cases.Data(cse).shipId);
PAR.wind         = protectstring(FI.Project(prj).Cases.Data(cse).windId);
PAR.waves        = protectstring(FI.Project(prj).Cases.Data(cse).wavesId);
PAR.current      = protectstring(FI.Project(prj).Cases.Data(cse).currentId);
PAR.swell        = protectstring(FI.Project(prj).Cases.Data(cse).swellId);
PAR.scenery      = protectstring(FI.Project(prj).Cases.Data(cse).sceneryId);
%
if isempty(FI.Project(prj).Cases.Data(cse).TimeSeries)
    shipma = '';
else
    headerLine = FI.Project(prj).Cases.Data(cse).TimeSeries.Header(1,:);
    [shipma,rem] = strtok(headerLine);
    version = strtok(rem);
    shipma = [shipma ' ' version];
end
PAR.shipma       = shipma;
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function selfplot(FI,Props)
persistent inSelfPlot
%
% use persistent variable to prevent the selfplot command from recursively
% calling itself if one of the selected variables does not exist.
%
if isequal(inSelfPlot,1)
    return
end
inSelfPlot = 1;

prj = Props.Project;
cse = Props.Case;
%
% These parameters need to be unprotected
%
PARFIL.filename1    = FI.FileName;
PARFIL.domain1      = [FI.Project(prj).Name '/' FI.Project(prj).Cases.Names{cse}];

session    = qp_settings(['shipma_session_for_' FI.Project(prj).Name],'');
if isempty(session)
    selfplot_builtin
else
    [p,f,e]=fileparts(session);
    switch e
        case '.qpses'
            d3d_qp('openfigure',session,PARFIL)
        otherwise
            d3d_qp('run',session,'-par',PARFIL)
    end
end
%--------
d3d_qp('selectfile',PARFIL.filename1)
d3d_qp('selectdomain',PARFIL.domain1)
d3d_qp('selectfield','default figures')

inSelfPlot=[];
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function selfplot_builtin
cartoyellow = [254 197 68]/255;
texts_template = get_shipma_bordertexts;
%
if d3d_qp('selectfield','desired ship track')
    a=d3d_qp('loaddata'); % get ship track for auto zoom limits
    xrange = [min(a.X) max(a.X)];
    xrange = xrange + [-1 1]*0.1*diff(xrange);
    yrange = [min(a.Y) max(a.Y)];
    yrange = yrange + [-1 1]*0.1*diff(yrange);
    fac = 1.25;
    yrange = mean(yrange)+[-1 1]*max(diff(xrange)*fac,diff(yrange))/2;
    xrange = mean(xrange)+[-1 1]*diff(yrange)/fac/2;
else
    xrange = [];
end
%
for c = {'a' 'a1' 'a2'}
    if qp_settings(['shipma_fig' c{1}])
        caption = 'Overview plot of track';
        fignr = 'Fig. A';
        cstep = qp_settings('shipma_figa_contourstep');
        cmax  = qp_settings('shipma_figa_contourmax');
        switch c{1}
            case 'a'
                if qp_settings('shipma_figa_depth')
                    caption = [caption ' and depth'];
                end
            case 'a1'
                caption = 'Zoomed plot of track';
                if qp_settings('shipma_figa_depth')
                    caption = [caption ' and depth'];
                end
                fignr = 'Fig. A1';
                zoombox = qp_settings('shipma_figa1_zoombox');
                if any(isnan(zoombox))
                    ui_message('error','Skipping Fig A1 because zoombox coordinates are incomplete')
                    continue
                else
                    zoombox(1:2) = sort(zoombox(1:2));
                    zoombox(3:4) = sort(zoombox(3:4));
                end
            case 'a2'
                quant = qp_settings('shipma_figa2_quantity');
                cstep = qp_settings('shipma_figa2_contourstep');
                cmax  = qp_settings('shipma_figa2_contourmax');
                %
                switch quant
                    case {'wind','waves','swell'}
                        caption = ['Track plot and ' quant];
                    case {'current'}
                        caption = ['Track plot and ' quant 's'];
                end
                fignr = 'Fig. A2';
        end
        texts = qp_strrep(texts_template,'%caption%',caption);
        texts = qp_strrep(texts,'%fignr%',fignr);
        d3d_qp('newfigure','1 plot - portrait',['SHIPMA Fig ' upper(c{1})])
        d3d_qp('setasparametersource')
        d3d_qp('figureborder',texts{:})
        switch c{1}
            case {'a','a1'}
                if qp_settings('shipma_figa_depth') && d3d_qp('selectfield','depth')
                    d3d_qp('colourmap','navdepth')
                    d3d_qp('presenttype','contour patches')
                    d3d_qp('thresholds',[-inf 0:cstep:cmax])
                    d3d_qp('colbarhorz',1)
                    d3d_qp('addtoplot')
                end
            case 'a2'
                if d3d_qp('selectfield',quant)
                    d3d_qp('component','magnitude')
                    d3d_qp('presenttype','contour patches')
                    d3d_qp('thresholds',0:cstep:cmax)
                    d3d_qp('colourmap','revhot')
                    d3d_qp('addtoplot')
                    d3d_qp('component','vector')
                    d3d_qp('colourvectors',0)
                    d3d_qp('colour',[ 0 0 1 ])
                    if ~isempty(xrange)
                        d3d_qp('thinfld','distance')
                        d3d_qp('thindist',diff(xrange)/100)
                    end
                    d3d_qp('addtoplot')
                end
        end
        if qp_settings('shipma_figa_fairway') && d3d_qp('selectfield','fairway contour')
            d3d_qp('linestyle','-')
            d3d_qp('colour',[ 0 0 0 ])
            d3d_qp('fillpolygons',1)
            d3d_qp('facecolour',cartoyellow)
            d3d_qp('addtoplot')
        end
        if qp_settings('shipma_figa_banksuction') && d3d_qp('selectfield','bank suction lines')
            d3d_qp('linestyle','--')
            d3d_qp('colour',[ 0 0 0 ])
            d3d_qp('fillpolygons',0)
            d3d_qp('addtoplot')
        end
        if d3d_qp('selectfield','desired ship track')
            d3d_qp('linestyle','-')
            d3d_qp('colour',[ 0 0 0 ])
            d3d_qp('addtoplot')
        end
        if d3d_qp('selectfield','ship snapshots')
            d3d_qp('linestyle','-')
            d3d_qp('colour',[ 0 0 0 ])
            d3d_qp('fillpolygons',1)
            d3d_qp('facecolour',[ 1 0 0 ])
            d3d_qp('addtoplot')
        end
        if d3d_qp('selectfield','distance ticks')
            d3d_qp('colour',[ 0 0 0 ])
            d3d_qp('addtoplot')
        end
        if d3d_qp('selectfield','distance value at ticks')
            d3d_qp('colour',[ 0 0 0 ])
            d3d_qp('presenttype','labels')
            d3d_qp('fontsize',6)
            d3d_qp('addtoplot')
        end
        d3d_qp('axesboxed',1)
        switch c{1}
            case 'a1'
                % impacts also a2
                xrange = zoombox(1:2);
                yrange = zoombox(3:4);
        end
        if ~isempty(xrange)
            d3d_qp('axeslimits',xrange,yrange)
        end
    end
end
%--------
if qp_settings('shipma_figb')
    caption = 'Propeller speed, ship speed and rudder angle plots';
    fignr = 'Fig. B';
    texts = qp_strrep(texts_template,'%caption%',caption);
    texts = qp_strrep(texts,'%fignr%',fignr);
    d3d_qp('newfigure','3 plots, vertical - portrait','SHIPMA Fig B')
    d3d_qp('setasparametersource')
    d3d_qp('figureborder',texts{:})
    %--
    qpsa('upper plot')
    d3d_qp('allt',1)
    if d3d_qp('selectfield','propeller speed')
        d3d_qp('axestype','Distance-Val')
        d3d_qp('colour',[ 0 0 0 ])
        d3d_qp('linestyle','-')
        d3d_qp('addtoplot')
    end
    d3d_qp('axesgrid',1,1)
    d3d_qp('axesboxed',1)
    %--
    qpsa('middle plot')
    if d3d_qp('selectfield','ship speed')
        d3d_qp('colour',[ 0 0 0 ])
        d3d_qp('addtoplot')
    end
    d3d_qp('axesgrid',1,1)
    d3d_qp('axesboxed',1)
    %--
    qpsa('lower plot')
    if d3d_qp('selectfield','rudder angle')
        d3d_qp('colour',[ 0 0 0 ])
        d3d_qp('addtoplot')
    end
    d3d_qp('axesgrid',1,1)
    d3d_qp('axesboxed',1)
end
%--------
if qp_settings('shipma_figc')
    caption = {'Swept path and depth along track','Starboard side (dashed) port side (solid)'};
    fignr = 'Fig. C';
    texts = qp_strrep(texts_template,'%caption%',caption);
    texts = qp_strrep(texts,'%fignr%',fignr);
    d3d_qp('newfigure','2 plots, vertical - portrait','SHIPMA Fig C')
    d3d_qp('setasparametersource')
    d3d_qp('figureborder',texts{:})
    %--
    d3d_qp('selectaxes','upper plot')
    set(qpsa,'ydir','reverse')
    if d3d_qp('selectfield','swept path port side')
        d3d_qp('axestype','Distance-Val')
        d3d_qp('linestyle','-')
        d3d_qp('colour',[ 0 0 0 ])
        d3d_qp('addtoplot')
    end
    if d3d_qp('selectfield','swept path starboard side')
        d3d_qp('linestyle','--')
        d3d_qp('colour',[ 0 0 0 ])
        d3d_qp('addtoplot')
    end
    d3d_qp('axesgrid',1,1)
    d3d_qp('axesboxed',1)
    d3d_qp('ylabel','swept path (%unit%)')
    %--
    d3d_qp('selectaxes','lower plot')
    if d3d_qp('selectfield','water depth')
        d3d_qp('linestyle','-')
        d3d_qp('colour',[ 0 0 0 ])
        d3d_qp('addtoplot')
    end
    d3d_qp('axesgrid',1,1)
    d3d_qp('axesboxed',1)
end
%--------
lstyle = {'-','--','-.',':'};
lstylename = {'solid','dashed','dash-dotted','dotted'};
i=0;
if qp_settings('shipma_figd')
    lines = {'' '' '' ''};
    quants = {};
    if qp_settings('shipma_figd_wind')
        i=i+1;
        lines{1} = lstyle{i};
        quants{i} = sprintf('wind (%s)',lstylename{i});
    end
    if qp_settings('shipma_figd_waves')
        i=i+1;
        lines{2} = lstyle{i};
        quants{i} = sprintf('waves (%s)',lstylename{i});
    end
    if qp_settings('shipma_figd_swell')
        i=i+1;
        lines{3} = lstyle{i};
        quants{i} = sprintf('swell (%s)',lstylename{i});
    end
    if qp_settings('shipma_figd_banksuction')
        i=i+1;
        lines{4} = lstyle{i};
        quants{i} = sprintf('bank suction (%s)',lstylename{i});
    end
    if ~isempty(quants)
        quantstr = sprintf('%s, ',quants{:});
        quantstr(1) = upper(quantstr(1));
        quantstr = quantstr(1:end-2);
        commas = strfind(quantstr,',');
        if ~isempty(commas)
            quantstr = [quantstr(1:commas(end)-1) ' and' quantstr(commas(end)+1:end)];
        end
        caption = {'External forces plots' quantstr};
    else
        caption = 'External forces plots';
    end
    fignr = 'Fig. D';
    texts = qp_strrep(texts_template,'%caption%',caption);
    texts = qp_strrep(texts,'%fignr%',fignr);
    d3d_qp('newfigure','3 plots, vertical - portrait','SHIPMA Fig D')
    d3d_qp('setasparametersource')
    d3d_qp('figureborder',texts{:})
    %--
    d3d_qp('selectaxes','upper plot')
    if qp_settings('shipma_figd_wind') && d3d_qp('selectfield','longitudinal wind force')
        d3d_qp('axestype','Distance-Val')
        d3d_qp('linestyle',lines{1})
        d3d_qp('colour',[ 0 0 0 ])
        d3d_qp('addtoplot')
    end
    if qp_settings('shipma_figd_waves') && d3d_qp('selectfield','longitudinal wave force')
        d3d_qp('linestyle',lines{2})
        d3d_qp('colour',[ 0 0 0 ])
        d3d_qp('addtoplot')
    end
    if qp_settings('shipma_figd_swell') && d3d_qp('selectfield','longitudinal swell force')
        d3d_qp('linestyle',lines{3})
        d3d_qp('colour',[ 0 0 0 ])
        d3d_qp('addtoplot')
    end
    if qp_settings('shipma_figd_banksuction') && d3d_qp('selectfield','longitudinal bank suction force')
        d3d_qp('linestyle',lines{4})
        d3d_qp('colour',[ 0 0 0 ])
        d3d_qp('addtoplot')
    end
    d3d_qp('axesgrid',1,1)
    d3d_qp('axesboxed',1)
    d3d_qp('ylabel','longitudinal forces (%unit%) \rightarrow')
    %--
    d3d_qp('selectaxes','middle plot')
    if qp_settings('shipma_figd_wind') && d3d_qp('selectfield','transverse wind force')
        d3d_qp('linestyle',lines{1})
        d3d_qp('colour',[ 0 0 0 ])
        d3d_qp('addtoplot')
    end
    if qp_settings('shipma_figd_waves') && d3d_qp('selectfield','transverse wave force')
        d3d_qp('linestyle',lines{2})
        d3d_qp('colour',[ 0 0 0 ])
        d3d_qp('addtoplot')
    end
    if qp_settings('shipma_figd_swell') && d3d_qp('selectfield','transverse swell force')
        d3d_qp('linestyle',lines{3})
        d3d_qp('colour',[ 0 0 0 ])
        d3d_qp('addtoplot')
    end
    if qp_settings('shipma_figd_banksuction') && d3d_qp('selectfield','transverse bank suction force')
        d3d_qp('linestyle',lines{4})
        d3d_qp('colour',[ 0 0 0 ])
        d3d_qp('addtoplot')
    end
    d3d_qp('axesgrid',1,1)
    d3d_qp('axesboxed',1)
    d3d_qp('ylabel','transverse forces (%unit%) \rightarrow')
    %--
    d3d_qp('selectaxes','lower plot')
    if qp_settings('shipma_figd_wind') && d3d_qp('selectfield','wind moment on ship')
        d3d_qp('linestyle',lines{1})
        d3d_qp('colour',[ 0 0 0 ])
        d3d_qp('addtoplot')
    end
    if qp_settings('shipma_figd_waves') && d3d_qp('selectfield','wave moment')
        d3d_qp('linestyle',lines{2})
        d3d_qp('colour',[ 0 0 0 ])
        d3d_qp('addtoplot')
    end
    if qp_settings('shipma_figd_swell') && d3d_qp('selectfield','swell moment')
        d3d_qp('linestyle',lines{3})
        d3d_qp('colour',[ 0 0 0 ])
        d3d_qp('addtoplot')
    end
    if qp_settings('shipma_figd_banksuction') && d3d_qp('selectfield','moment due to bank suction')
        d3d_qp('linestyle',lines{4})
        d3d_qp('colour',[ 0 0 0 ])
        d3d_qp('addtoplot')
    end
    d3d_qp('axesgrid',1,1)
    d3d_qp('axesboxed',1)
    d3d_qp('ylabel','moment (%unit%) \rightarrow')
end
%--------
if qp_settings('shipma_fige')
    if qp_settings('shipma_fige_tugs')
        if qp_settings('shipma_fige_thrusters')
            caption = {'Tug and thrusters forces plots','Tug forces (solid) and thruster forces (dashed)'};
            lines = {'-' '--'};
        else
            caption = 'Tug forces plots';
            lines = {'-' ''};
        end
    elseif qp_settings('shipma_fige_thrusters')
        caption = 'Tug forces plots';
        lines = {'' '-'};
    else
        caption = '';
    end
    fignr = 'Fig. E';
    texts = qp_strrep(texts_template,'%caption%',caption);
    texts = qp_strrep(texts,'%fignr%',fignr);
    d3d_qp('newfigure','3 plots, vertical - portrait','SHIPMA Fig E')
    d3d_qp('setasparametersource')
    d3d_qp('figureborder',texts{:})
    %--
    d3d_qp('selectaxes','upper plot')
    if qp_settings('shipma_fige_tugs') && d3d_qp('selectfield','longitudinal total tug force')
        d3d_qp('axestype','Distance-Val')
        d3d_qp('linestyle',lines{1})
        d3d_qp('colour',[ 0 0 0 ])
        d3d_qp('addtoplot')
    end
    d3d_qp('axesgrid',1,1)
    d3d_qp('axesboxed',1)
    d3d_qp('ylabel','longitudinal force (%unit%) \rightarrow')
    %--
    d3d_qp('selectaxes','middle plot')
    if qp_settings('shipma_fige_tugs') && d3d_qp('selectfield','transverse total tug force')
        d3d_qp('linestyle',lines{1})
        d3d_qp('colour',[ 0 0 0 ])
        d3d_qp('addtoplot')
    end
    if qp_settings('shipma_fige_thrusters') && d3d_qp('selectfield','transverse thruster force')
        d3d_qp('linestyle',lines{2})
        d3d_qp('colour',[ 0 0 0 ])
        d3d_qp('addtoplot')
    end
    d3d_qp('axesgrid',1,1)
    d3d_qp('axesboxed',1)
    d3d_qp('ylabel','transverse force (%unit%) \rightarrow')
    %--
    d3d_qp('selectaxes','lower plot')
    if qp_settings('shipma_fige_tugs') && d3d_qp('selectfield','total tug moment')
        d3d_qp('linestyle',lines{1})
        d3d_qp('colour',[ 0 0 0 ])
        d3d_qp('addtoplot')
    end
    if qp_settings('shipma_fige_thrusters') && d3d_qp('selectfield','moment due to thrusters')
        d3d_qp('linestyle',lines{2})
        d3d_qp('colour',[ 0 0 0 ])
        d3d_qp('addtoplot')
    end
    d3d_qp('axesgrid',1,1)
    d3d_qp('axesboxed',1)
    d3d_qp('ylabel','moment (%unit%) \rightarrow')
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function Out=domains(FI)
Out=FI.Case.Name;
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function Out=infile(FI,domain)
if domain > length(FI.Case.Project)
    prj  = 'none';
    cse  = 0;
else
    prj  = FI.Case.Project(domain);
    cse  = FI.Case.Case(domain);
    Proj = FI.Project(prj);
end
scn = 0;
man = 0;
%
V=inf; % unknown/variable number of points indicated by infinity
PropNames={'Name'                       'Units' 'DimFlag'   'DataInCell' 'NVal' 'Geom'   'Coords' 'ClosedPoly' 'Project' 'Case' 'Scenery' 'Manoeuvre' 'Var'   };
Sep      ={'-------'                    ''      [0 0 0 0 0] 0             0     ''       ''       0            prj       cse    scn       man          0      }; 
if domain > length(FI.Case.Project)
    Out=cell2struct(Sep,PropNames,2);
    Out(1:end,:)=[];
    return
elseif cse>0
    scn=FI.Project(prj).Cases.Data(cse).sceneryNr;
    DataProps={'default figures'        ''      [0 0 0 0 0] 0            -2     ''       ''       0            prj       cse    scn       man          0
        '-------'                       ''      [0 0 0 0 0] 0             0     ''       ''       0            prj       cse    scn       man          0
        'desired ship track'            ''      [0 0 0 0 0] 0             0     'POLYL'  'xy'     0            prj       cse    scn       man          -1
        'realized ship track'           ''      [9 0 0 0 0] 0             0     'PNT'    'xy'     0            prj       cse    scn       man          0
        'distance ticks'                ''      [0 0 0 0 0] 0             0     'POLYL'  'xy'     0            prj       cse    scn       man          0
        'distance value at ticks'       ''      [0 0 0 0 0] 0             4     'sQUAD'  'xy'     0            prj       cse    scn       man          0
        'ship at distance ticks'        ''      [0 0 0 0 0] 0             0     'POLYG'  'xy'     1            prj       cse    scn       man          0
        'ship snapshots'                ''      [0 0 0 0 0] 0             0     'POLYG'  'xy'     1            prj       cse    scn       man          0
        'ship'                          ''      [9 0 0 0 0] 0             0     'POLYG'  'xy'     1            prj       cse    scn       man          0
        'swept path'                    ''      [0 0 0 0 0] 0             0     'POLYG'  'xy'     1            prj       cse    scn       man          0
        'fairway contour'               ''      [0 0 0 0 0] 0             0     'POLYG'  'xy'     1            prj       cse    scn       man          -1
        'bank suction lines'            ''      [0 0 0 0 0] 0             0     'POLYL'  'xy'     0            prj       cse    scn       man          -1
        '-------'                       ''      [0 0 0 0 0] 0             0     ''       ''       0            prj       cse    scn       man          0
        'wind'                          'm/s'   [0 0 V 0 0] 0             2     'TRI'    'xy'     0            prj       cse    scn       man          -1
        'waves'                         'm'     [0 0 V 0 0] 0             2     'TRI'    'xy'     0            prj       cse    scn       man          -1
        'swell'                         'm'     [0 0 V 0 0] 0             2     'TRI'    'xy'     0            prj       cse    scn       man          -1
        'current'                       'm/s'   [0 0 V 0 0] 0             2     'TRI'    'xy'     0            prj       cse    scn       man          -1
        'depth'                         'm'     [0 0 V 0 0] 0             1     'TRI'    'xy'     0            prj       cse    scn       man          -1
        '-------'                       ''      [0 0 0 0 0] 0             0     ''       ''       0            prj       cse    scn       man          0
        'ship speed'                    'm/s'   [9 0 0 0 0] 0             1     'PNT'    'd'      0            prj       cse    scn       man          0
        'his-data'                      ''      [9 0 0 0 0] 0             1     'PNT'    'd'      0            prj       cse    scn       man          0       };
    Out=cell2struct(DataProps,PropNames,2);
    %
    if isempty(Proj.Cases.Data(cse).TimeSeries)
        hisvars={};
    else
        hisvars = Proj.Cases.Data(cse).TimeSeries.SubsName;
    end
    startVal = length(Out)-1;
    nVal = length(hisvars);
    Out = cat(1,Out(1:startVal),repmat(Out(end),nVal,1));
    track = find(strcmpi('track [m]',hisvars));
    for i=1:nVal
        var = hisvars{i};
        uStart = strfind(var,'[');
        name = translate(deblank(var(1:uStart-1)));
        unit = var(uStart+1:end-1);
        %
        if strcmp(name,'propeller speed') % change unit to rev/min
            unit = 'rev/min';
        end
        %
        hisvars{i} = name;
        Out(startVal+i).Name = name;
        Out(startVal+i).Units = unit;
        Out(startVal+i).Var = [i track];
    end
else
    DataProps={};
    %
    man = 0;
    for scn = 1:length(FI.Project(prj).Sceneries.Names)
        nm = FI.Project(prj).Sceneries.Names{scn};
        sc = sprintf('scenery %s: ',nm);
        DataProps = cat(1,DataProps, ...
        {[sc 'fairway contour']         ''      [0 0 0 0 0] 0             0     'POLYG'  'xy'     1            prj       cse    scn       man          -1
        [sc 'bank suction lines']       ''      [0 0 0 0 0] 0             0     'POLYL'  'xy'     0            prj       cse    scn       man          -1
        [sc 'depth']                    'm'     [0 0 V 0 0] 0             1     'TRI'    'xy'     0            prj       cse    scn       man          -1});
    end
    %
    DataProps = cat(1,DataProps,Sep);
    for e = 1:length(FI.Project(prj).Environments.Names)
        Envs = FI.Project(prj).Environments.Names{e};
        env = lower(Envs(1:end-1));
        if strcmp(env,'wave')
            env = 'waves';
        end
        for i = 1:length(FI.Project(prj).Environments.(Envs).Names)
            nm = FI.Project(prj).Environments.(Envs).Names{i};
            qnt = sprintf('%s %s',env,nm);
            switch env
                case {'wind','current'}
                    un = 'm/s';
                    nv = 2;
                case 'waves'
                    un = 'm';
                    nv = 2;
                case 'swell'
                    un = 'm';
                    nv = 2;
                otherwise
                    continue
            end
            scn = Envs;
            man = i;
            DataProps = cat(1,DataProps, ...
                {qnt                    un      [0 0 V 0 0] 0             nv    'TRI'    'xy'     0            prj       cse    scn       man          -1});
        end
    end
    %
    scn = 0;
    DataProps = cat(1,DataProps,Sep);
    for man = 1:length(FI.Project(prj).Manoeuvres.Names)
        nm = FI.Project(prj).Manoeuvres.Names{man};
        track = sprintf('manoeuvre %s: route waypoints',nm);
        DataProps = cat(1,DataProps, ...
        {track                          ''      [0 0 0 0 0] 0             0     'POLYL'  'xy'     0            prj       cse    scn       man          -1});
    end
    %
    DataProps = cat(1,DataProps,Sep);
    Out=cell2struct(DataProps,PropNames,2);
end
%
for i = length(Out):-1:1
    Name = Out(i).Name;
    if Out(i).Case==0
        ic = findstr(Name,':');
        if isempty(ic)
            Name = strtok(Name);
        else
            Name = Name(ic+2:end);
        end
    end
    switch Name
        case 'x'
            Out(i)=[];
        case 'y'
            Out(i)=[];
        case 'depth'
            if cse>0
                btFN = Proj.Cases.Data(cse).bottomFile;
            else
                scn = Out(3).Scenery;
                btFN = Proj.Sceneries.Data(scn).bottomFile;
            end
            if isempty(btFN)
                Out(i)=[];
            elseif ~exist(btFN,'file')
                Out(i)=[];
            end
        case 'wind'
            if cse>0
                if ~Proj.Cases.Data(cse).windIsSelected || Proj.Cases.Data(cse).windNr<0
                    fld = 0;
                else
                    fld = Proj.Cases.Data(cse).windNr;
                    if ~Proj.Environments.Winds.Data(fld).fileSelected
                        fld = 0;
                    end
                end
            else
                fld = Out(i).Manoeuvre;
            end
            if fld==0 || isempty(Proj.Environments.Winds.Data(fld).file)
                Out(i)=[];
            elseif ~exist(Proj.Environments.Winds.Data(fld).file,'file')
                Out(i)=[];
            end
        case 'waves'
            if cse>0
                if ~Proj.Cases.Data(cse).wavesIsSelected || Proj.Cases.Data(cse).wavesNr<0
                    fld = 0;
                else
                    fld = Proj.Cases.Data(cse).wavesNr;
                    if ~Proj.Environments.Waves.Data(fld).fileSelected
                        fld = 0;
                    end
                end
            else
                fld = Out(i).Manoeuvre;
            end
            if fld==0 || isempty(Proj.Environments.Waves.Data(fld).file)
                Out(i)=[];
            elseif ~exist(Proj.Environments.Waves.Data(fld).file,'file')
                Out(i)=[];
            end
        case 'swell'
            if cse>0
                if ~Proj.Cases.Data(cse).swellIsSelected || Proj.Cases.Data(cse).swellNr<0
                    fld = 0;
                else
                    fld = Proj.Cases.Data(cse).swellNr;
                    if ~Proj.Environments.Swells.Data(fld).fileSelected
                        fld = 0;
                    end
                end
            else
                fld = Out(i).Manoeuvre;
            end
            if fld==0 || isempty(Proj.Environments.Swells.Data(fld).file)
                Out(i)=[];
            elseif ~exist(Proj.Environments.Swells.Data(fld).file,'file')
                Out(i)=[];
            end
        case 'current'
            if cse>0
                if ~Proj.Cases.Data(cse).currentIsSelected || Proj.Cases.Data(cse).currentNr<0
                    fld = 0;
                else
                    fld = Proj.Cases.Data(cse).currentNr;
                    if ~Proj.Environments.Currents.Data(fld).fileSelected
                        fld = 0;
                    end
                end
            else
                fld = Out(i).Manoeuvre;
            end
            if fld==0 || isempty(Proj.Environments.Currents.Data(fld).file)
                Out(i)=[];
            elseif ~exist(Proj.Environments.Currents.Data(fld).file,'file')
                Out(i)=[];
            end
        case 'ship speed'
            u = find(strcmpi('longitudinal ship speed',hisvars));
            v = find(strcmpi('transverse ship speed',hisvars));
            if isempty(u) || isempty(v) || isempty(track)
                Out(i)=[];
            else
                Out(i).Var = [u v track];
            end
        case {'realized ship track','distance ticks','distance value at ticks'}
            x = find(strcmpi('x',hisvars));
            y = find(strcmpi('y',hisvars));
            if isempty(x) || isempty(y)
                Out(i)=[];
            else
                Out(i).Var = [x y];
            end
        case 'fairway contour'
            if cse>0
                if ~Proj.Cases.Data(cse).sceneryIsSelected || Proj.Cases.Data(cse).sceneryNr<0
                    scn = 0;
                else
                    scn = Proj.Cases.Data(cse).sceneryNr;
                end
            else
                scn = Out(i).Scenery;
            end
            if scn==0 || ~exist(Proj.Sceneries.Data(scn).fairwayContourFile,'file')
                Out(i)=[];
            end
        case 'bank suction lines'
            if cse>0
                if ~Proj.Cases.Data(cse).sceneryIsSelected || Proj.Cases.Data(cse).sceneryNr<0
                    scn = 0;
                else
                    scn = Proj.Cases.Data(cse).sceneryNr;
                end
            else
                scn = Out(i).Scenery;
            end
            if scn==0 || ~exist(Proj.Sceneries.Data(scn).banksuctionFile,'file')
                Out(i)=[];
            end
        case 'desired ship track'
            if ~exist(Proj.Cases.Data(cse).trackFile,'file')
                Out(i)=[];
            end
        case {'ship','ship snapshots','ship at distance ticks','swept path'}
            x = find(strcmpi('x',hisvars));
            y = find(strcmpi('y',hisvars));
            dir = find(strcmpi('heading',hisvars));
            Out(i).Var = [x y dir];
            %
            ship = Proj.Cases.Data(cse).shipNr;
            if ship>0
                icontour = ustrcmpi('contour',{Proj.Ships.Data(ship).Props.Quant});
                contour = Proj.Ships.Data(ship).Props(icontour).Value;
            end
            %
            if isempty(x) || isempty(y) || isempty(dir) || ship<=0
                Out(i)=[];
            else
                switch Out(i).Name
                    case 'ship'
                        if isempty(contour)
                            Out(i) = [];
                        end
                    case {'ship at distance ticks','ship snapshots'}
                        Out(i).Var(4) = track;
                        if isempty(contour)
                            Out(i) = [];
                        end
                    case 'swept path'
                        loff = find(strcmpi('lateral offset (from desired track)',hisvars));
                        spp = find(strcmpi('swept path port side',hisvars));
                        sps = find(strcmpi('swept path starboard side',hisvars));
                        Out(i).Var(1,4:6) = [loff spp sps];
                end
            end
    end
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function sz=getsize(FI,Props)
T_=1; ST_=2; M_=3; N_=4; K_=5;
sz=[0 0 0 0 0];
%======================== SPECIFIC CODE =======================================
prj = Props.Project;
cse = Props.Case;
if Props.DimFlag(T_)
    sz(T_) = FI.Project(prj).Cases.Data(cse).TimeSeries.NTimes;
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function T=readtim(FI,Props,t)
prj = Props.Project;
cse = Props.Case;
%======================== SPECIFIC CODE =======================================
T = delwaq('read',FI.Project(prj).Cases.Data(cse).TimeSeries,1,1,t);
% -----------------------------------------------------------------------------

% -------------------------------------------------------------------------
function [NewFI,cmdargs]=options(FI,mfig,cmd,varargin)
T_=1; ST_=2; M_=3; N_=4; K_=5;
%======================== SPECIFIC CODE ===================================
Inactive=get(0,'defaultuicontrolbackground');
Active=[1 1 1];
NewFI=FI;
cmd=lower(cmd);
cmdargs={};
switch cmd
    case 'initialize'
        optfig(mfig);
        %
        h=findobj(mfig,'tag','spacestepval');
        step=qp_settings('shipma_spacestep');
        set(h,'string',step)
        %
        h=findobj(mfig,'tag','timestepval');
        step=qp_settings('shipma_timestep');
        set(h,'string',step)
        %
        h=findobj(mfig,'tag','shipma_distance_along_desired_trackval');
        step=qp_settings('shipma_distance_along_desired_track');
        set(h,'value',1+step)
        %
        h=findobj(mfig,'tag','defaultfigures');
        %
        if isempty(FI.Project)
            session = [];
        else
            Projects = FI.Project(1).Name;
            session  = qp_settings(['shipma_session_for_' Projects],'');
        end
        if isempty(session)
            deffg = 1;
        else
            deffg = 2;
        end
        set(h,'value',deffg)
        %
        if deffg==1
            builtin = {'on' 'off'};
        else
            builtin = {'off' 'on'};
            set(findobj(mfig,'tag','session_name'),'string',session)
            [p,f,e] = fileparts(session);
            switch e
                case '.qpses'
                    set(findobj(mfig,'tag','session_txt'),'string','Session')
                otherwise
                    set(findobj(mfig,'tag','session_txt'),'string','Macro')
            end
        end
        a     = findall(mfig,'type','uicontrol');
        atags = get(a,'tag');
        b1    = a(strncmp('fig',atags,3) | strcmp('editborder',atags));
        b2    = a(strncmp('session',atags,5));
        set(b1,'visible',builtin{1})
        set(b2,'visible',builtin{2})
        %
        zb = qp_settings('shipma_figa1_zoombox');
        for cellchari = {'a' 'a1' 'a2' 'b' 'c' 'd' 'e'}
            chari = cellchari{1};
            h=findobj(mfig,'tag',['fig' chari]);
            v=qp_settings(['shipma_fig' chari]);
            set(h,'value',v)
            %
            switch chari
                case {'a2','d','e'}
                    switch chari
                        case 'a2'
                            taglist = {'figa_fairway','figa_banksuction','figa_depth', ...
                                'figa_contourstep','figa_contourstepval', ...
                                'figa_contourmax','figa_contourmaxval', ...
                                'figa1_xmin','figa1_xminval','figa1_xmax','figa1_xmaxval', ...
                                'figa1_ymin','figa1_yminval','figa1_ymax','figa1_ymaxval', ...
                                'figa2_list','figa2_contourstep','figa2_contourstepval', ...
                                'figa2_contourmax','figa2_contourmaxval'};
                            if get(findobj(mfig,'tag','figa1'),'value')
                                set(h,'string','Fig A2, as Fig A1 now showing')
                            else
                                set(h,'string','Fig A2, as Fig A now showing')
                            end
                        case 'd'
                            taglist = {'figd_wind','figd_waves','figd_swell','figd_banksuction'};
                        case 'e'
                            taglist = {'fige_tugs','fige_thrusters'};
                    end
                    for tg = taglist
                        h = findobj(mfig,'tag',tg{1});
                        opt = {};
                        vloc = v;
                        switch tg{1}
                            case {'figa_fairway','figa_banksuction'}
                                vloc = get(findobj(mfig,'tag','figa'),'value') || ...
                                    get(findobj(mfig,'tag','figa1'),'value') || ...
                                    v;
                                v2=qp_settings(['shipma_' tg{1}]);
                                opt = {'value',v2};
                            case 'figa_depth'
                                vloc = get(findobj(mfig,'tag','figa'),'value') || ...
                                    get(findobj(mfig,'tag','figa1'),'value');
                                v2=qp_settings(['shipma_' tg{1}]);
                                opt = {'value',v2};
                            case {'figa_contourstep','figa_contourmax'}
                                vloc = (get(findobj(mfig,'tag','figa'),'value') || ...
                                    get(findobj(mfig,'tag','figa1'),'value')) && ...
                                    get(findobj(mfig,'tag','figa_depth'),'value');
                            case {'figa_contourstepval','figa_contourmaxval'}
                                vloc = (get(findobj(mfig,'tag','figa'),'value') || ...
                                    get(findobj(mfig,'tag','figa1'),'value')) && ...
                                    get(findobj(mfig,'tag','figa_depth'),'value');
                                opt = {'string',qp_settings(['shipma_' tg{1}(1:end-3)])};
                            case {'figa1_xmin','figa1_xmax','figa1_ymin','figa1_ymax'}
                                vloc = get(findobj(mfig,'tag','figa1'),'value');
                            case {'figa1_xminval','figa1_xmaxval','figa1_yminval','figa1_ymaxval'}
                                switch tg{1}
                                    case 'figa1_xminval'
                                        iz=1;
                                    case 'figa1_xmaxval'
                                        iz=2;
                                    case 'figa1_yminval'
                                        iz=3;
                                    case 'figa1_ymaxval'
                                        iz=4;
                                end
                                vloc = get(findobj(mfig,'tag','figa1'),'value');
                                if ~isnan(zb(iz))
                                    opt = {'string',zb(iz)};
                                end
                            case {'figa2_list','figa2_contourstep','figa2_contourmax'}
                            case {'figa2_contourstepval','figa2_contourmaxval'}
                                opt = {'string',qp_settings(['shipma_' tg{1}(1:end-3)])};
                            otherwise
                                v2=qp_settings(['shipma_' tg{1}]);
                                opt = {'value',v2};
                        end
                        if vloc
                            set(h,'enable','on',opt{:})
                            if strcmp(get(h,'style'),'popupmenu')
                                set(h,'backgroundcolor',Active)
                            end
                        else
                            set(h,'enable','off',opt{:})
                            if strcmp(get(h,'style'),'popupmenu')
                                set(h,'backgroundcolor',Inactive)
                            end
                        end
                    end
            end
        end
        %
        h=findobj(mfig,'tag','figa2_list');
        quant=qp_settings('shipma_figa2_quantity');
        str=get(h,'string');
        i=ustrcmpi(quant,str);
        if i>0
            set(h,'value',i)
        end

    case {'defaultfigures','selectsession'}
        h=findobj(mfig,'tag','defaultfigures');
        %
        Projects = {FI.Project.Name};
        fp = qp_settings(['shipma_session_for_' Projects{1}],'');
        if isempty(varargin)
            deffg = get(h,'value');
            if deffg==2 && isempty(fp) || strcmp(cmd,'selectsession')
                curdir = pwd;
                if isempty(fp)
                    cd(fileparts(FI.FileName))
                else
                    cd(fileparts(fp))
                end
                filter = {'*.qpses' 'QUICKPLOT Session File'
                    '*.qplog;*.m' 'QUICKPLOT Macro File'};
                [f,p]=uigetfile(filter,'Select Session File ...');
                cd(curdir)
                if ischar(f)
                    fp = [p f];
                else % Escape / Cancel
                    deffg = 0;
                end
            elseif deffg==2
                deffg = 0;
            elseif deffg==1 && isempty(fp)
                deffg = 0;
            end
        else
            fp = varargin{1};
            if strcmp(fp,'built-in')
                deffg = 1;
            else
                deffg = 2;
            end
        end
        %
        if deffg==2
            if exist(fp,'file')==0
                ui_message('error','Cannot find file "%s"',fp)
                deffg = 0;
            else
                [p,f,e] = fileparts(fp);
                switch e
                    case {'.qpses','.qplog','.m'}
                        % OK
                    otherwise
                        ui_message('error','File extension "%s" not supported',e(2:end))
                        deffg = 0;
                end
            end
        end
        %
        if deffg==0 % Cancel
            fp = qp_settings(['shipma_session_for_' Projects{1}],'');
            if isempty(fp)
                deffg = -1;
            else
                deffg = -2;
            end
        end
        %
        if abs(deffg)==1
            fp = 'built-in';
            builtin = {'on' 'off'};
            %
            if deffg==1
                for i = 1:length(Projects)
                    qp_settings(['shipma_session_for_' Projects{i}],[])
                end
            end
        elseif abs(deffg)==2
            builtin = {'off' 'on'};
            %
            [p,f,e] = fileparts(fp);
            switch e
                case '.qpses'
                    set(findobj(mfig,'tag','session_txt'),'string','Session')
                otherwise
                    set(findobj(mfig,'tag','session_txt'),'string','Macro')
            end
            %
            if deffg==2
                set(findobj(mfig,'tag','session_name'),'string',fp)
                for i = 1:length(Projects)
                    qp_settings(['shipma_session_for_' Projects{i}],fp)
                end
            end
        end
        %
        set(h,'value',abs(deffg))
        a     = findall(mfig,'type','uicontrol');
        atags = get(a,'tag');
        b1    = a(strncmp('fig',atags,3) | strcmp('editborder',atags));
        b2    = a(strncmp('session',atags,5));
        set(b1,'visible',builtin{1})
        set(b2,'visible',builtin{2})
        if deffg>0
            cmdargs={'defaultfigures',fp};
        end

    case {'shipma_spacestep','shipma_timestep','shipma_figa_contourstep','shipma_figa_contourmax','shipma_figa2_contourstep','shipma_figa2_contourmax'}
        h = findobj(mfig,'tag',[cmd(8:end) 'val']);
        if isempty(varargin)
            step = get(h,'string');
        else
            step = varargin{1};
        end
        if ischar(step)
            step = str2double(step);
        end
        set(h,'string',sprintf('%g',step))
        qp_settings(cmd,step)
        cmdargs={cmd,step};
        
    case 'shipma_distance_along_desired_track'
        h = findobj(mfig,'tag',[cmd(8:end) 'val']);
        if isempty(varargin)
            val = get(h,'value')-1;
        else
            val = varargin{1};
        end
        if ischar(val)
            val = str2double(val);
        end
        set(h,'value',1+val)
        qp_settings(cmd,val)
        cmdargs={cmd,val};
        
    case 'shipma_figa1_zoombox'
        if isempty(varargin)
            % get zoombox from Fig A, A1 or A2
            fg = 'A1';
            Figs = findall(0,'type','figure');
            FigsA = [findall(0,'type','figure','name','SHIPMA Fig A');
                findall(0,'type','figure','name','SHIPMA Fig A1');
                findall(0,'type','figure','name','SHIPMA Fig A2')];
            isA = ismember(Figs,FigsA);
            if none(isA)
                ui_message('error','No Fig A, A1 or A2 open to get zoombox coordinates from.')
                return
            end
            Figs = Figs(find(isA,1,'first')); % get first (=topmost) figure
            Ax = findall(Figs,'type','axes','tag','plot area');
            if length(Ax)~=1
                ui_message('error','Invalid Fig %s found.',fg)
                return
            end
            xrange = get(Ax,'xlim');
            yrange = get(Ax,'ylim');
            val = [xrange yrange];
        else
            val = varargin{1};
            if ischar(val)
                val = str2vec(val);
            end
            if ~isequal(size(val),[1 4])
                error('Invalid zoombox specified; should be 1x4 array.')
            end
        end
        cmds = {'shipma_figa1_xmin','shipma_figa1_xmax','shipma_figa1_ymin','shipma_figa1_ymax'};
        for i = 1:4
            options(FI,mfig,cmds{i},val(i));
        end
        if ~qp_settings('shipma_figa1')
            d3d_qp('fileoptions','shipma_figa1',1)
        end
        cmdargs={cmd,val};
        
    case {'shipma_figa1_xmin','shipma_figa1_xmax','shipma_figa1_ymin','shipma_figa1_ymax'}
        zb = qp_settings('shipma_figa1_zoombox');
        h = findobj(mfig,'tag',[cmd(8:end) 'val']);
        if isempty(varargin)
            val = get(h,'string');
        else
            val = varargin{1};
        end
        if ischar(val)
            val = str2double(val);
        end
        if isnan(val)
            set(h,'string','')
        else
            set(h,'string',sprintf('%g',val))
        end
        switch cmd
            case 'shipma_figa1_xmin'
                iz = 1;
            case 'shipma_figa1_xmax'
                iz = 2;
            case 'shipma_figa1_ymin'
                iz = 3;
            case 'shipma_figa1_ymax'
                iz = 4;
        end
        zb(iz) = val;
        qp_settings('shipma_figa1_zoombox',zb)
        cmdargs={cmd,val};
        
    case {'shipma_figa','shipma_figa1','shipma_figa2','shipma_figb','shipma_figc','shipma_figd', ...
            'shipma_figa_fairway','shipma_figa_banksuction','shipma_figa_depth', ...
            'shipma_figd_wind','shipma_figd_waves','shipma_figd_swell','shipma_figd_banksuction', ...
            'shipma_fige','shipma_fige_tugs','shipma_fige_thrusters'}
        h = findobj(mfig,'tag',cmd(8:end));
        if isempty(varargin)
            v = get(h,'value');
        else
            v = varargin{1};
        end
        if ischar(v)
            v = str2double(v);
        end
        set(h,'value',v)
        qp_settings(cmd,v)
        %
        adjustv = 0;
        vloc = v;
        %
        if strcmp(cmd,'shipma_figa1') || strcmp(cmd,'shipma_figa2')
            if get(findobj(mfig,'tag','figa1'),'value')
                set(findobj(mfig,'tag','figa2'),'string','Fig A2, as Fig A1 now showing')
            else
                set(findobj(mfig,'tag','figa2'),'string','Fig A2, as Fig A now showing')
            end
        end
        switch cmd
            case {'shipma_figa','shipma_figa1','shipma_figa2'}
                taglist = {'figa_fairway','figa_banksuction','figa_depth', ...
                    'figa_contourstep','figa_contourstepval', ...
                    'figa_contourmax','figa_contourmaxval', ...
                    'figa1_xmin','figa1_xminval','figa1_xmax','figa1_xmaxval', ...
                    'figa1_ymin','figa1_yminval','figa1_ymax','figa1_ymaxval', ...
                    'figa2_list','figa2_contourstep','figa2_contourstepval', ...
                    'figa2_contourmax','figa2_contourmaxval'};
                adjustv = 1;
            case 'shipma_figa_depth'
                taglist = {'figa_contourstep','figa_contourstepval', ...
                    'figa_contourmax','figa_contourmaxval'};
            case 'shipma_figd'
                taglist = {'figd_wind','figd_waves','figd_swell','figd_banksuction'};
            case 'shipma_fige'
                taglist = {'fige_tugs','fige_thrusters'};
            otherwise
                taglist = {};
        end
        for tg = taglist
            h = findobj(mfig,'tag',tg{1});
            if adjustv
                switch tg{1}
                    case {'figa_fairway','figa_banksuction'}
                        vloc = get(findobj(mfig,'tag','figa'),'value') || ...
                            get(findobj(mfig,'tag','figa1'),'value') || ...
                            get(findobj(mfig,'tag','figa2'),'value');
                    case {'figa_depth'}
                        vloc = get(findobj(mfig,'tag','figa'),'value') || ...
                            get(findobj(mfig,'tag','figa1'),'value');
                    case {'figa_contourstep','figa_contourstepval','figa_contourmax','figa_contourmaxval'}
                        vloc = (get(findobj(mfig,'tag','figa'),'value') || ...
                            get(findobj(mfig,'tag','figa1'),'value')) && ...
                            get(findobj(mfig,'tag','figa_depth'),'value');
                    case {'figa1_xmin','figa1_xminval','figa1_xmax','figa1_xmaxval', ...
                            'figa1_ymin','figa1_yminval','figa1_ymax','figa1_ymaxval', ...
                            'figa1_zoombox'}
                        vloc = get(findobj(mfig,'tag','figa1'),'value');
                    case {'figa2_list','figa2_contourstep','figa2_contourstepval','figa2_contourmax','figa2_contourmaxval'}
                        vloc = get(findobj(mfig,'tag','figa2'),'value');
                    otherwise
                        vloc = 0;
                end
            end
            if vloc
                set(h,'enable','on')
                if strcmp(get(h,'style'),'popupmenu')
                    set(h,'backgroundcolor',Active)
                end
            else
                set(h,'enable','off')
                if strcmp(get(h,'style'),'popupmenu')
                    set(h,'backgroundcolor',Inactive)
                end
            end
        end
        cmdargs={cmd,v};
    case {'shipma_figa2_quantity'}
        h = findobj(mfig,'tag','figa2_list');
        str = get(h,'string');
        if isempty(varargin)
            i = get(h,'value');
            v = str{i};
        else
            v = varargin{1};
        end
        i = ustrcmpi(v,str);
        if i>0
            set(h,'value',i)
            qp_settings(cmd,str{i})
            cmdargs={cmd,str{i}};
        end
    case 'editborder'
        fg=figure('visible','off', ...
            'integerhandle','off', ...
            'position',get(mfig,'position'), ...
            'numbertitle','off', ...
            'name','SHIPMA');
        hBorder=md_paper(fg,'no edit','7box',get_shipma_bordertexts);
        md_paper('editmodal',hBorder)
        bProp = md_paper(hBorder,'getprops');
        for i=1:7
            d3d_qp('fileoptions',sprintf('shipma_bordertext%i_string',i),bProp.(sprintf('BorderText%i',i)))
        end
        delete(fg)
    case {'shipma_bordertext1_string', 'shipma_bordertext2_string', ...
            'shipma_bordertext3_string', 'shipma_bordertext4_string', ...
            'shipma_bordertext5_string', 'shipma_bordertext6_string', ...
            'shipma_bordertext7_string'}
        % the user interface doesn't call these commands directly, so we
        % we always need a string argument
        str = varargin{1};
        qp_settings(cmd,str)
        cmdargs={cmd,str};
    otherwise
        error('Unknown option command: %s',cmd)
end
% -------------------------------------------------------------------------


function c = get_shipma_bordertexts
c = cell(1,7);
for i = 1:7
    str = qp_settings(sprintf('shipma_bordertext%i_string',i));
    str = strrep(str,'\n{}',char(13));
    str = splitcellstr(str,char(13));
    c{i} = str;
end

% -------------------------------------------------------------------------
function OK=optfig(h0)
Inactive=qp_settings('UIInActiveColor');
Active=qp_settings('UIActiveColor');
FigPos=get(h0,'position');
FigPos(3) = 680;
ssz=qp_getscreen(h0);
FigPos(1) = min(FigPos(1),ssz(1)+ssz(3)-FigPos(3));
set(h0,'position',FigPos)
voffset=FigPos(4)-30;
textwidth=340-80-30; %FigPos(3)-80-30
uicontrol('Parent',h0, ...
    'Style','text', ...
    'BackgroundColor',Inactive, ...
    'Horizontalalignment','left', ...
    'Position',[11 voffset textwidth 18], ...
    'String','Space Step for Distance Ticks (m)', ...
    'Enable','on', ...
    'Tag','spacestep');
uicontrol('Parent',h0, ...
    'Style','edit', ...
    'HorizontalAlignment','right', ...
    'BackgroundColor',Active, ...
    'Callback','d3d_qp fileoptions shipma_spacestep', ...
    'Position',[21+textwidth voffset 80 20], ...
    'Enable','on', ...
    'Tag','spacestepval');
voffset=voffset-25;
uicontrol('Parent',h0, ...
    'Style','text', ...
    'BackgroundColor',Inactive, ...
    'Horizontalalignment','left', ...
    'Position',[11 voffset textwidth-50 18], ...
    'String','Distance Measured Along', ...
    'Enable','on', ...
    'Tag','distance_along_desired_track');
uicontrol('Parent',h0, ...
    'Style','popupmenu', ...
    'String',{'Realized Track','Desired Track'}, ...
    'Value',2, ...
    'HorizontalAlignment','left', ...
    'BackgroundColor',Active, ...
    'Callback','d3d_qp fileoptions shipma_distance_along_desired_track', ...
    'Position',[21+textwidth-50 voffset 80+50 21], ...
    'Enable','on', ...
    'Tag','distance_along_desired_trackval');
voffset=voffset-25;
uicontrol('Parent',h0, ...
    'Style','text', ...
    'BackgroundColor',Inactive, ...
    'Horizontalalignment','left', ...
    'Position',[11 voffset textwidth 18], ...
    'String','Time Step for Ship Snapshots (s)', ...
    'Enable','on', ...
    'Tag','timestep');
uicontrol('Parent',h0, ...
    'Style','edit', ...
    'HorizontalAlignment','right', ...
    'BackgroundColor',Active, ...
    'Callback','d3d_qp fileoptions shipma_timestep', ...
    'Position',[21+textwidth voffset 80 20], ...
    'Enable','on', ...
    'Tag','timestepval');
%
voffset=voffset-30;
uipanel('Parent',h0, ...
    'BackgroundColor',Inactive, ...
    'Units','pixels', ...
    'Position',[11 36 FigPos(3)-20 voffset-20], ...
    'BorderType','etchedin', ...
    'Title','Default Figures                      ');
uicontrol('Parent',h0, ...
    'Style','popupmenu', ...
    'BackgroundColor',Active, ...
    'Horizontalalignment','left', ...
    'Position',[111 voffset 80+50 21], ...
    'String',{'Built-In Layout','User Session'}, ...
    'Callback','d3d_qp fileoptions defaultfigures', ...
    'Value',1, ...
    'Enable','on', ...
    'Tag','defaultfigures');
%
voffset=voffset-25-5;
uicontrol('Parent',h0, ...
    'Style','text', ...
    'BackgroundColor',Inactive, ...
    'Horizontalalignment','left', ...
    'Position',[21 voffset 40 18], ...
    'String','Session', ...
    'Enable','on', ...
    'Tag','session_txt');
uicontrol('Parent',h0, ...
    'Style','edit', ...
    'BackgroundColor',Inactive, ...
    'Horizontalalignment','left', ...
    'Position',[71 voffset FigPos(3)-120 20], ...
    'String','<no file specified: using built-in layout>', ...
    'Enable','inactive', ...
    'Tag','session_name');
uicontrol('Parent',h0, ...
    'Style','pushbutton', ...
    'BackgroundColor',Inactive, ...
    'Horizontalalignment','center', ...
    'Position',[FigPos(3)-40 voffset 20 20], ...
    'String','...', ...
    'Callback','d3d_qp fileoptions selectsession', ...
    'Enable','on', ...
    'Tag','session_select');
voffset=voffset+25;
%
voffset=voffset-25;
uicontrol('Parent',h0, ...
    'Style','pushbutton', ...
    'BackgroundColor',Inactive, ...
    'Horizontalalignment','left', ...
    'Position',[21 voffset 80+50 20], ...
    'String','Edit Border Texts', ...
    'Callback','d3d_qp fileoptions editborder', ...
    'Enable','on', ...
    'Tag','editborder');
voffset=voffset-25;
uicontrol('Parent',h0, ...
    'Style','checkbox', ...
    'BackgroundColor',Inactive, ...
    'Horizontalalignment','left', ...
    'Position',[21 voffset textwidth-10 18], ...
    'String','Fig A, overview plot', ...
    'Callback','d3d_qp fileoptions shipma_figa', ...
    'Enable','on', ...
    'Tag','figa');
voffset=voffset-25;
uicontrol('Parent',h0, ...
    'Style','checkbox', ...
    'BackgroundColor',Inactive, ...
    'Horizontalalignment','left', ...
    'Position',[41 voffset 105 18], ...
    'String','fairway contour', ...
    'Callback','d3d_qp fileoptions shipma_figa_fairway', ...
    'Enable','on', ...
    'Tag','figa_fairway');
uicontrol('Parent',h0, ...
    'Style','checkbox', ...
    'BackgroundColor',Inactive, ...
    'Horizontalalignment','left', ...
    'Position',[156 voffset 80 18], ...
    'String','bank suction', ...
    'Callback','d3d_qp fileoptions shipma_figa_banksuction', ...
    'Enable','on', ...
    'Tag','figa_banksuction');
uicontrol('Parent',h0, ...
    'Style','checkbox', ...
    'BackgroundColor',Inactive, ...
    'Horizontalalignment','left', ...
    'Position',[251 voffset 80 18], ...
    'String','depth', ...
    'Callback','d3d_qp fileoptions shipma_figa_depth', ...
    'Enable','on', ...
    'Tag','figa_depth');
voffset=voffset-25;
uicontrol('Parent',h0, ...
    'Style','text', ...
    'BackgroundColor',Inactive, ...
    'Horizontalalignment','left', ...
    'Position',[41 voffset 80 18], ...
    'String','Contour step', ...
    'Enable','on', ...
    'Tag','figa_contourstep');
uicontrol('Parent',h0, ...
    'Style','edit', ...
    'HorizontalAlignment','right', ...
    'BackgroundColor',Active, ...
    'Callback','d3d_qp fileoptions shipma_figa_contourstep', ...
    'Position',[131 voffset 60 20], ...
    'Enable','on', ...
    'Tag','figa_contourstepval');
uicontrol('Parent',h0, ...
    'Style','text', ...
    'BackgroundColor',Inactive, ...
    'Horizontalalignment','left', ...
    'Position',[201 voffset 40 18], ...
    'String','Max', ...
    'Enable','on', ...
    'Tag','figa_contourmax');
uicontrol('Parent',h0, ...
    'Style','edit', ...
    'HorizontalAlignment','right', ...
    'BackgroundColor',Active, ...
    'Callback','d3d_qp fileoptions shipma_figa_contourmax', ...
    'Position',[251 voffset 80 20], ...
    'Enable','on', ...
    'Tag','figa_contourmaxval');
voffset=voffset-25;
uicontrol('Parent',h0, ...
    'Style','checkbox', ...
    'BackgroundColor',Inactive, ...
    'Horizontalalignment','left', ...
    'Position',[21 voffset textwidth-10 18], ...
    'String','Fig A1, zoomed version of Fig A', ...
    'Callback','d3d_qp fileoptions shipma_figa1', ...
    'Enable','on', ...
    'Tag','figa1');
voffset=voffset-25;
uicontrol('Parent',h0, ...
    'Style','text', ...
    'BackgroundColor',Inactive, ...
    'Horizontalalignment','left', ...
    'Position',[41 voffset 40 18], ...
    'String','X Min', ...
    'Enable','on', ...
    'Tag','figa1_xmin');
uicontrol('Parent',h0, ...
    'Style','edit', ...
    'HorizontalAlignment','right', ...
    'BackgroundColor',Active, ...
    'Callback','d3d_qp fileoptions shipma_figa1_xmin', ...
    'Position',[81 voffset 75 20], ...
    'Enable','on', ...
    'Tag','figa1_xminval');
uicontrol('Parent',h0, ...
    'Style','text', ...
    'BackgroundColor',Inactive, ...
    'Horizontalalignment','left', ...
    'Position',[161 voffset 40 18], ...
    'String','X Max', ...
    'Enable','on', ...
    'Tag','figa1_xmax');
uicontrol('Parent',h0, ...
    'Style','edit', ...
    'HorizontalAlignment','right', ...
    'BackgroundColor',Active, ...
    'Callback','d3d_qp fileoptions shipma_figa1_xmax', ...
    'Position',[201 voffset 75 20], ...
    'Enable','on', ...
    'Tag','figa1_xmaxval');
voffset=voffset-25;
uicontrol('Parent',h0, ...
    'Style','text', ...
    'BackgroundColor',Inactive, ...
    'Horizontalalignment','left', ...
    'Position',[41 voffset 40 18], ...
    'String','Y Min', ...
    'Enable','on', ...
    'Tag','figa1_ymin');
uicontrol('Parent',h0, ...
    'Style','edit', ...
    'HorizontalAlignment','right', ...
    'BackgroundColor',Active, ...
    'Callback','d3d_qp fileoptions shipma_figa1_ymin', ...
    'Position',[81 voffset 75 20], ...
    'Enable','on', ...
    'Tag','figa1_yminval');
uicontrol('Parent',h0, ...
    'Style','text', ...
    'BackgroundColor',Inactive, ...
    'Horizontalalignment','left', ...
    'Position',[161 voffset 40 18], ...
    'String','Y Max', ...
    'Enable','on', ...
    'Tag','figa1_ymax');
uicontrol('Parent',h0, ...
    'Style','edit', ...
    'HorizontalAlignment','right', ...
    'BackgroundColor',Active, ...
    'Callback','d3d_qp fileoptions shipma_figa1_ymax', ...
    'Position',[201 voffset 75 20], ...
    'Enable','on', ...
    'Tag','figa1_ymaxval');
uicontrol('Parent',h0, ...
    'Style','pushbutton', ...
    'HorizontalAlignment','right', ...
    'BackgroundColor',Inactive, ...
    'Callback','d3d_qp fileoptions shipma_figa1_zoombox', ...
    'Position',[286 voffset 45 45], ...
    'Max',2, ...
    'CData',qp_icon('getaxesdims','nan'), ...
    'Enable','on', ...
    'Tooltip','Get zoombox from topmost Fig A, A1 or A2', ...
    'Tag','figa1_zoombox');
voffset=voffset-25;
uicontrol('Parent',h0, ...
    'Style','checkbox', ...
    'BackgroundColor',Inactive, ...
    'Horizontalalignment','left', ...
    'Position',[21 voffset textwidth 18], ...
    'String','Fig A2, as Fig A1 now showing', ...
    'Callback','d3d_qp fileoptions shipma_figa2', ...
    'Enable','on', ...
    'Tag','figa2');
uicontrol('Parent',h0, ...
    'Style','popupmenu', ...
    'BackgroundColor',Active, ...
    'Horizontalalignment','left', ...
    'Position',[21+textwidth voffset 80 20], ...
    'String',{'wind','waves','swell','current'}, ...
    'Callback','d3d_qp fileoptions shipma_figa2_quantity', ...
    'Enable','on', ...
    'Tag','figa2_list');
voffset=voffset-25;
uicontrol('Parent',h0, ...
    'Style','text', ...
    'BackgroundColor',Inactive, ...
    'Horizontalalignment','left', ...
    'Position',[41 voffset 80 18], ...
    'String','Contour step', ...
    'Enable','on', ...
    'Tag','figa2_contourstep');
uicontrol('Parent',h0, ...
    'Style','edit', ...
    'HorizontalAlignment','right', ...
    'BackgroundColor',Active, ...
    'Callback','d3d_qp fileoptions shipma_figa2_contourstep', ...
    'Position',[131 voffset 60 20], ...
    'Enable','on', ...
    'Tag','figa2_contourstepval');
uicontrol('Parent',h0, ...
    'Style','text', ...
    'BackgroundColor',Inactive, ...
    'Horizontalalignment','left', ...
    'Position',[201 voffset 40 18], ...
    'String','Max', ...
    'Enable','on', ...
    'Tag','figa2_contourmax');
uicontrol('Parent',h0, ...
    'Style','edit', ...
    'HorizontalAlignment','right', ...
    'BackgroundColor',Active, ...
    'Callback','d3d_qp fileoptions shipma_figa2_contourmax', ...
    'Position',[251 voffset 80 20], ...
    'Enable','on', ...
    'Tag','figa2_contourmaxval');
voffset=voffset+8*25;
hoffset=340;
uicontrol('Parent',h0, ...
    'Style','checkbox', ...
    'BackgroundColor',Inactive, ...
    'Horizontalalignment','left', ...
    'Position',[hoffset+21 voffset textwidth+60 18], ...
    'String','Fig B, ship & propeller speed and rudder angle plots', ...
    'Callback','d3d_qp fileoptions shipma_figb', ...
    'Enable','on', ...
    'Tag','figb');
voffset=voffset-25;
uicontrol('Parent',h0, ...
    'Style','checkbox', ...
    'BackgroundColor',Inactive, ...
    'Horizontalalignment','left', ...
    'Position',[hoffset+21 voffset textwidth-10 18], ...
    'String','Fig C, swept path and depth plots', ...
    'Callback','d3d_qp fileoptions shipma_figc', ...
    'Enable','on', ...
    'Tag','figc');
voffset=voffset-25;
uicontrol('Parent',h0, ...
    'Style','checkbox', ...
    'BackgroundColor',Inactive, ...
    'Horizontalalignment','left', ...
    'Position',[hoffset+21 voffset textwidth-10 18], ...
    'String','Fig D, external forces plots', ...
    'Callback','d3d_qp fileoptions shipma_figd', ...
    'Enable','on', ...
    'Tag','figd');
voffset=voffset-25;
uicontrol('Parent',h0, ...
    'Style','checkbox', ...
    'BackgroundColor',Inactive, ...
    'Horizontalalignment','left', ...
    'Position',[hoffset+41 voffset 80 18], ...
    'String','wind', ...
    'Callback','d3d_qp fileoptions shipma_figd_wind', ...
    'Enable','on', ...
    'Tag','figd_wind');
uicontrol('Parent',h0, ...
    'Style','checkbox', ...
    'BackgroundColor',Inactive, ...
    'Horizontalalignment','left', ...
    'Position',[hoffset+131 voffset 80 18], ...
    'String','waves', ...
    'Callback','d3d_qp fileoptions shipma_figd_waves', ...
    'Enable','on', ...
    'Tag','figd_waves');
uicontrol('Parent',h0, ...
    'Style','checkbox', ...
    'BackgroundColor',Inactive, ...
    'Horizontalalignment','left', ...
    'Position',[hoffset+221 voffset 80 18], ...
    'String','swell', ...
    'Callback','d3d_qp fileoptions shipma_figd_swell', ...
    'Enable','on', ...
    'Tag','figd_swell');
voffset=voffset-25;
uicontrol('Parent',h0, ...
    'Style','checkbox', ...
    'BackgroundColor',Inactive, ...
    'Horizontalalignment','left', ...
    'Position',[hoffset+41 voffset 80 18], ...
    'String','bank suction', ...
    'Callback','d3d_qp fileoptions shipma_figd_banksuction', ...
    'Enable','on', ...
    'Tag','figd_banksuction');
voffset=voffset-25;
uicontrol('Parent',h0, ...
    'Style','checkbox', ...
    'BackgroundColor',Inactive, ...
    'Horizontalalignment','left', ...
    'Position',[hoffset+21 voffset textwidth-10 18], ...
    'String','Fig E, tugs and thrusters plots', ...
    'Callback','d3d_qp fileoptions shipma_fige', ...
    'Enable','on', ...
    'Tag','fige');
voffset=voffset-25;
uicontrol('Parent',h0, ...
    'Style','checkbox', ...
    'BackgroundColor',Inactive, ...
    'Horizontalalignment','left', ...
    'Position',[hoffset+41 voffset 80 18], ...
    'String','tugs', ...
    'Callback','d3d_qp fileoptions shipma_fige_tugs', ...
    'Enable','on', ...
    'Tag','fige_tugs');
uicontrol('Parent',h0, ...
    'Style','checkbox', ...
    'BackgroundColor',Inactive, ...
    'Horizontalalignment','left', ...
    'Position',[hoffset+131 voffset 80 18], ...
    'String','thrusters', ...
    'Callback','d3d_qp fileoptions shipma_fige_thrusters', ...
    'Enable','on', ...
    'Tag','fige_thrusters');
OK=1;
% -------------------------------------------------------------------------


function s2 = translate(s1)
table = {'track' 'track distance'
    'psi' 'course (earth-fixed coords)'
    'Heading' 'heading'
    'dy' 'lateral offset (from desired track)'
    'dpsi' 'course deviation'
    'u' 'longitudinal ship speed'
    'v' 'transverse ship speed'
    'r' 'rate of turn'
    'rudder1' 'rudder angle'
    'n1' 'propeller speed'
    'h' 'water depth'
    'Vwind' 'wind speed'
    'phiwind' 'wind direction (earth-fixed coords)'
    'Xwind' 'longitudinal wind force'
    'Ywind' 'transverse wind force'
    'Nwind' 'wind moment on ship'
    'Hwave' 'Hs sea waves'
    'phiwave' 'direction sea waves (earth-fixed coords)'
    'Xwave' 'longitudinal wave force'
    'Ywave' 'transverse wave force'
    'Nwave' 'wave moment'
    'Hswl' 'Hs swell waves'
    'phiswl' 'direction swell waves (earth-fixed coords)'
    'Xswl' 'longitudinal swell force'
    'Yswl' 'transverse swell force'
    'Nswl' 'swell moment'
    'uc' 'longitudinal current velocity over ground'
    'vc' 'transverse current velocity over ground'
    'rc' 'rotational current velocity'
    'Xtugs' 'longitudinal total tug force'
    'Ytugs' 'transverse total tug force'
    'Ntugs' 'total tug moment'
    'YThrusters' 'transverse thruster force'
    'NThrusters' 'moment due to thrusters'
    'Xbank' 'longitudinal bank suction force'
    'Ybank' 'transverse bank suction force'
    'Nbank' 'moment due to bank suction'
    'Xhull' 'longitudinal hydrodynamic hull forces'
    'Yhull' 'transverse hydrodynamic hull forces'
    'Nhull' 'moment due to hydrodynamic hull forces'
    'Xrudprop' 'longitudinal control forces'
    'Yrudprop' 'latereal control forces'
    'Nrudprop' 'moment from control forces'
    'dp1' 'rate of rudder change'
    'up' 'ship acceleration in longitudinal direction'
    'vp' 'ship acceleration in transverse direction'
    'rp' 'yaw acceleration'
    'ucp' 'acceleration of current in longitudinal direction'
    'vcp' 'acceleration of current in transverse direction'
    'rcp' 'acceleration of rotational current'
    'Xtug1' 'first tug force in ship''s longitudinal direction'
    'Ytug1' 'first tug force in ship''s transverse direction'
    'Ntug1' 'moment due to first tug forces'
    'Towforce tug1' 'total tow force of first tug'
    'Towdir. tug1' 'tow direction of first tug (rel. to ship)'
    'Xtug2' 'second tug force in ship''s longitudinal direction'
    'Ytug2' 'second tug force in ship''s transverse direction'
    'Ntug2' 'moment due to second tug forces'
    'Towforce tug2' 'total tow force of second tug'
    'Towdir. tug2' 'tow direction of second tug (rel. to ship)'
    'Xtug3' 'third tug force in ship''s longitudinal direction'
    'Ytug3' 'third tug force in ship''s transverse direction'
    'Ntug3' 'moment due to third tug forces'
    'Towforce tug3' 'total tow force of third tug'
    'Towdir. tug3' 'tow direction of third tug (rel. to ship)'
    'Xtug4' 'fourth tug force in ship''s longitudinal direction'
    'Ytug4' 'fourth tug force in ship''s transverse direction'
    'Ntug4' 'moment due to fourth tug forces'
    'Towforce tug4' 'total tow force of fourth tug'
    'Towdir. tug4' 'tow direction of fourth tug (rel. to ship)'
    'Xtug5' 'fifth tug force in ship''s longitudinal direction'
    'Ytug5' 'fifth tug force in ship''s transverse direction'
    'Ntug5' 'moment due to fifth tug forces'
    'Towforce tug5' 'total tow force of fifth tug'
    'Towdir. tug5' 'tow direction of fifth tug (rel. to ship)'
    'Xtug6' 'sixth tug force in ship''s longitudinal direction'
    'Ytug6' 'sixth tug force in ship''s transverse direction'
    'Ntug6' 'moment due to sixth tug forces'
    'Towforce tug6' 'total tow force of sixth tug'
    'Towdir. tug6' 'tow direction of sixth tug (rel. to ship)'
    'Xtug7' 'seventh tug force in ship''s longitudinal direction'
    'Ytug7' 'seventh tug force in ship''s transverse direction'
    'Ntug7' 'moment due to seventh tug forces'
    'Towforce tug7' 'total tow force of seventh tug'
    'Towdir. tug7' 'tow direction of seventh tug (rel. to ship)'
    'Xtug8' 'eighth tug force in ship''s longitudinal direction'
    'Ytug8' 'eighth tug force in ship''s transverse direction'
    'Ntug8' 'moment due to eighth tug forces'
    'Towforce tug8' 'total tow force of eighth tug'
    'Towdir. tug8' 'tow direction of eighth tug (rel. to ship)'
    'swept path port' 'swept path port side'
    'swept path stb.' 'swept path starboard side'};
is1 = strcmpi(s1,table(:,1));
if ~any(is1)
    s2 = s1;
else
    s2 = table{is1,2};
end

function d = realized_track_distance(PRJ,cse,idx)
hisvars = PRJ.Cases.Data(cse).TimeSeries.SubsName;
x = find(strcmpi('x [m]',hisvars));
y = find(strcmpi('y [m]',hisvars));
[T,val1] = delwaq('read',PRJ.Cases.Data(cse).TimeSeries,[x y],1,0);
d = pathdistance(val1(1,:),val1(2,:))';
if ~isequal(idx{1},0)
    d = Ans.X(idx{1});
end


function dc = classify(d,dtick)
if ~isequal(dtick,sort(dtick))
    error('Ticks must be sorted.')
end
dc = zeros(size(d));
for i = 1:length(dtick)
   dc = dc + (d>dtick(i));
end


function id = crossings(d,dtick)
dc = classify(d,dtick); % determine class for each data value
idc = find(diff(dc)); % find class transitions
id = zeros(1,length(idc)*length(dtick)); % a class transition may go through multiple class boundaries
k = 0;
for j = 1:length(idc) % for each class transition
    d1 = d(idc(j));
    c1 = dc(idc(j));
    d2 = d(idc(j)+1);
    c2 = dc(idc(j)+1);
    if c1<c2 % going to higher class
        range = c1+1:c2;
    else % going to lower class
        range = c1:-1:c2+1;
    end
    for c = range
        k = k+1;
        id(k) = idc(j) + (dtick(c)-d1)/(d2-d1);
    end
end
id(:,k+1:end) = [];