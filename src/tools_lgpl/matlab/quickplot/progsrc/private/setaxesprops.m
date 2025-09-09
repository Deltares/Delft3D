function setaxesprops(hAx,FullAxesType,dimension,unit)
%SETAXESPROPS Set appropriate axes properties.

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

if nargin==1
    update_axesprops(hAx)
    return
end

if ~ishandle(hAx) || isempty(FullAxesType)
    return
end
%
BasicAxesType = full2basic_axestype(FullAxesType);
%
currentFullAxesType = getappdata(hAx,'AxesType');
if ~ischar(currentFullAxesType)
    currentFullAxesType = getappdata(hAx,'BasicAxesType');
end
newAxesSettings = true;
if  ischar(currentFullAxesType)
    if ~isequal(currentFullAxesType,FullAxesType)
        warning('AxesType mismatch: %s (set) vs. %s (new).',currentFullAxesType,FullAxesType)
        FullAxesType = currentFullAxesType;
        BasicAxesType = full2basic_axestype(FullAxesType);
    else
        newAxesSettings = false;
    end
end
%
Axes  = splitcellstr(BasicAxesType,'-');
%
if nargin<3
    dimension = {};
end
if nargin<4
    unit = {};
end
%
switch BasicAxesType
    case 'legend'
        set_2d_axes_behavior(hAx)
        sethscale(hAx,1)
        legendbox('init',hAx)
    case {'analog clock','digital clock','calendar page'}
        set_2d_axes_behavior(hAx)
        sethscale(hAx,1)
        md_clock(hAx,BasicAxesType,floor(now))
        set(hAx,'tag',BasicAxesType)
    case 'text'
        set_2d_axes_behavior(hAx)
        set(hAx,'visible','off', ...
            'xlim',[0 1], ...
            'ylim',[0 1])
    otherwise
        generaltype(hAx, Axes, dimension, unit, newAxesSettings)
end
%
if ~isequal(FullAxesType,BasicAxesType)
    setappdata(hAx,'AxesType',FullAxesType)
end
setappdata(hAx,'BasicAxesType',BasicAxesType)
update_axesprops(hAx)


function generaltype(hAx, Axes, dimension, unit, newAxesSettings)
nAxes = length(Axes);
X = 'xyz';
if nAxes==2
    set_2d_axes_behavior(hAx)
end
if nAxes>=2
    if isequal(Axes(1:2),{'Lon','Lat'})
        setappdata(hAx,'LonLat',1)
        sethscale_lonlat(hAx,newAxesSettings)
    elseif isequal(Axes(1:2),{'X','Y'}) && numel(unit)>=2 && isequal(unit{1},unit{2})
        sethscale(hAx,1,newAxesSettings)
    end
end
if length(dimension)<nAxes
    dimension{nAxes}=[];
end
if length(unit)<nAxes
    unit{nAxes} = [];
end
for i = 1:nAxes
    if isempty(getappdata(hAx,[X(i) 'limmode']))
        setappdata(hAx,[X(i) 'limmode'],'auto')
    end
    switch Axes{i}
        case 'Time'
            % Time axis
            if isempty(getappdata(hAx,[X(i) 'tickmode']))
                setappdata(hAx,[X(i) 'tickmode'],'autodate')
            end
            setlabel(hAx,X(i),'time',unit{i},'time')
        case 'Lon'
            % Longitude axis
            if isempty(getappdata(hAx,[X(i) 'tickmode']))
                setappdata(hAx,[X(i) 'tickmode'],'longitude')
            end
            setlabel(hAx,X(i),'longitude','deg','longitude')
        case 'Lat'
            % Latitude axis
            if isempty(getappdata(hAx,[X(i) 'tickmode']))
                setappdata(hAx,[X(i) 'tickmode'],'latitude')
            end
            setlabel(hAx,X(i),'latitude','deg','latitude')
        case 'Z'
            % Elevation axis
            if isempty(getappdata(hAx,[X(i) 'tickmode']))
                setappdata(hAx,[X(i) 'tickmode'],'auto')
            end
            setlabel(hAx,X(i),dimension{i},unit{i},'elevation')
        case {'X','Y'}
            % Horizontal coordinate axis
            if isempty(getappdata(hAx,[X(i) 'tickmode']))
                setappdata(hAx,[X(i) 'tickmode'],'auto')
            end
            if isempty(dimension{i})
                dimension{i} = sprintf('%s coordinate',lower(Axes{i}));
            end
            setlabel(hAx,X(i),dimension{i},unit{i},[lower(Axes{i}) ' coordinate'])
        case {'Distance'}
            % Distance axis
            if isempty(getappdata(hAx,[X(i) 'tickmode']))
                setappdata(hAx,[X(i) 'tickmode'],'auto')
            end
            if isempty(dimension{i})
                dimension{i} = 'distance';
            end
            setlabel(hAx,X(i),dimension{i},unit{i},'distance')
        otherwise
            % Variable axis
            if isempty(getappdata(hAx,[X(i) 'tickmode']))
                setappdata(hAx,[X(i) 'tickmode'],'auto')
            end
            setlabel(hAx,X(i),dimension{i},unit{i},'variable')
    end
end


function sethscale_lonlat(hAx, newAxesSettings)
ylimv = get(hAx,'ylim');
xlimv = get(hAx, 'xlim');
if ylimv(1) <- 90
    ylimv(1) = -90;
    if ylimv(2) <= ylimv(1)
        ylimv(2) = -89;
    end
end
if ylimv(2) > 90
    ylimv(2) = 90;
    if ylimv(1) >= ylimv(2)
        ylimv(1) = 89;
    end
end
lat = mean(ylimv);
lat = min(max(lat, -89), 89);
if xlimv(1) < -360
    xlimv(1) = -360;
    if xlimv(2) <= xlimv(1)
        xlimv(2) = -359;
    end
end
if xlimv(2) > 360
    xlimv(2) = 360;
    if xlimv(1) >= xlimv(2)
        xlimv(1) = 359;
    end
end
set(hAx, 'ylim', ylimv, 'xlim', xlimv)
sethscale(hAx, cos(lat*pi/180), newAxesSettings)


function sethscale(hAx,ratio,newAxesSettings)
if newAxesSettings
    setappdata(hAx,'haspectdefaultvalue',ratio)
    if strcmp(get(hAx,'dataaspectratiomode'),'manual')
        da = get(hAx,'dataaspectratio');
        da(2) = da(1)*ratio;
        set(hAx,'dataaspectratio',da)
    else
        set(hAx,'dataaspectratio',[1 ratio 1/30])
    end
else
    oldratio = getappdata(hAx,'haspectdefaultvalue');
    setappdata(hAx,'haspectdefaultvalue',ratio)
    if strcmp(get(hAx,'dataaspectratiomode'),'manual')
        da = get(hAx,'dataaspectratio');
        if isequal(da(2),da(1)*oldratio)
            da(2) = da(1)*ratio;
            set(hAx,'dataaspectratio',da)
        end
    end
end


function axestype = full2basic_axestype(axestype)
unitsloc=strfind(axestype,' [');
for i=length(unitsloc):-1:1
    unitsclose=strfind(axestype(unitsloc(i):end),']');
    if ~isempty(unitsclose)
        axestype(:,unitsloc(i)+(0:max(unitsclose)-1))=[];
    end
end


function update_axesprops(hAx)
if isequal(getappdata(hAx,'LonLat'),1)
    sethscale_lonlat(hAx,false)
end
% axes2d = isequal(getappdata(hAx,'axes2d'),true);
% if axes2d
%     alldims = 'xy';
% else
%     alldims = 'xyz';
% end
% dar = get(hAx,'dataaspectratio');
% dar = dar/dar(1);
% buf = 1000;
% for d = alldims
%     dlimmode = getappdata(hAx,[d 'limmode']);
%     if strcmp(dlimmode,'auto')
%        lim = limits(hAx,d);
%        set(hAx,[d 'lim'],lim + [-buf buf]*dar(d-'w'))
%     end
% end
for d = 'xyz'
    update_axticks(hAx,d)
end


function setlabel(ax,dir,quantity,unit,type)
if ~ischar(quantity)
    quantity = '';
end
if ~ischar(unit)
    unit = '';
end
setappdata(ax,[dir 'type'],type)
setappdata(ax,[dir 'quantity'],quantity)
setappdata(ax,[dir 'unit'],unit)
update_label(ax,dir)
%
update_axticks(ax,dir)


function update_label(ax,dir)
quantity = getappdata(ax,[dir 'quantity']);
unit     = getappdata(ax,[dir 'unit']);
arrow    = '\rightarrow';
if ~isempty(unit)
    dimstr = sprintf('%s (%s) %s',quantity,unit,arrow);
else
    dimstr = sprintf('%s %s',quantity,arrow);
end
%
setappdata(ax,[dir 'labelauto'],dimstr)
if isappdata(ax,[dir 'label'])
    dimstr = getappdata(ax,[dir 'label']);
    dimstr = qp_strrep(dimstr,'%quantity%',quantity);
    dimstr = qp_strrep(dimstr,'%unit%',unit);
end
if ~isempty(strfind(dimstr,'\n{}'))
    dimstr = strsplit(dimstr,'\\n{}');
end
%
axlabel = get(ax,[dir 'label']);
set(axlabel,'string',dimstr)


function update_axticks(hAx,dir)
type = getappdata(hAx,[dir 'type']);
unit = getappdata(hAx,[dir 'unit']);
units = {'mm' 'm' 'km'};
oldcase = isempty(type);
if oldcase
    type = getappdata(hAx,[dir 'quantity']);
    if isempty(type)
        return
    end
end
switch type
    case {'longitude','latitude'}
        tick(hAx,dir,'autoticks',type);
    case {'time'}
        tick(hAx,dir,'autoticks','autodate');
    case {'distance','x coordinate','y coordinate'}
        if ismember(unit,units)
            distanceticks(hAx,dir)
        end
    otherwise
        if oldcase && ...
                strncmp(type,'distance along',14) && ...
                ismember(unit,units)
            distanceticks(hAx,dir)
        end
end

function distanceticks(ax,dir)
unitQ=[0.001  1  1000];
unitT=[0      1  10000];
unitS={'mm'  'm' 'km'};
%
dx=max(abs(get(ax,[dir 'lim'])));
scale=sum(dx>unitT);
unit =unitS{scale};
%
set(ax,[dir 'ticklabelmode'],'auto',[dir 'tickmode'],'auto');
tick(ax,dir,'%g',1/unitQ(scale))
%
if ~isequal(unit,getappdata(ax,[dir 'unit']))
    setappdata(ax,[dir 'unit'],unit)
    update_label(ax,dir)
end

function set_2d_axes_behavior(ax)
setappdata(ax,'axes2d',true)
try
    set(ax, 'View',[0 90])
    for i = 1:length(ax)
        bh = hggetbehavior(ax(i),'rotate3d');
        set(bh,'Enable',false)
    end
catch
end
