function varargout = wms(cmd, varargin)
%WMS Support for (Tiled) Web Map Services.
%   [img,lon,lat] = WMS('image',SERVICE,LAYER,lonRange,latRange) returns an image with associated latitudes and longitudes. The image will be downloaded from the SERVICE specified for the LAYER selected. The lonRange and latRange specify the requested longitude and latitude ranges.
%
%   SERVICE = WMS('cap',servername)
%   SERVICE = WMS('tms',servername)

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

switch cmd
    case 'image'
        [varargout{1:nargout}] = getimage(varargin{:});
    case 'tms'
        varargout{1} = gettms(varargin{:});
    case {'cap','capabilities'}
        varargout{1} = getcap(varargin{:});
    otherwise
        error('Unknow command "%s"',cmd);
end

function INFO = gettms(servername)
INFO.Type = 'TMS';
INFO.Version = '1.0.0';
INFO.GetMap.URL = 'undefined';
INFO.GetMap.TileLevels = 19;
INFO.GetMap.Formats = {'image/jpeg'};
INFO.Data.Layer.Name = '';
INFO.Data.Layer.LonRange = [-180 180];
INFO.Data.Layer.LatRange = [-85.0511 85.0511]; % arctan(sinh(?))
INFO.Data.SRS = {'EPSG:900913'}; % EPSG:3857
INFO.TileSize = [256 256];
INFO.YOrigin  = 'bottom';
INFO.Servers  = {''};
%
switch servername
    case 'bluemarble'
        INFO.GetMap.URL        = 'http://s3.amazonaws.com/com.modestmaps.bluemarble/${z}-r${y}-c${x}.jpg';
        INFO.GetMap.TileLevels = 9;
        INFO.YOrigin           = 'top';
    case 'bingmaps'
        INFO.GetMap.URL        = 'http://t${s}.tiles.virtualearth.net/tiles/a${quadkey}.jpeg?g=1398';
        INFO.GetMap.TileLevels = 9;
        INFO.YOrigin           = 'top';
        INFO.Servers           = {'0','1','2','3'};
    case 'esri_worldimagery'
        INFO.GetMap.URL        = 'http://server.arcgisonline.com/arcgis/rest/services/World_Imagery/MapServer/tile/${z}/${y}/${x}.jpg';
        INFO.GetMap.TileLevels = 19;
        INFO.YOrigin           = 'top';
        INFO.Servers           = {''};
    case 'openstreetmap'
        INFO.GetMap.URL = 'http://${s}.tile.openstreetmap.org/${z}/${x}/${y}.png';
        INFO.GetMap.TileLevels = 19;
        INFO.YOrigin           = 'top';
        INFO.Servers           = {'a','b','c'};
        INFO.GetMap.Formats = {'image/png'};
        %wms_layer='osm_auto:all'; % <-- only support this one
   	    %wms_layer='europe_wms:hs_srtm_europa';
    %case 'ahn'
    %wms_server='http://geoservices.rijkswaterstaat.nl/actueel_hoogtebestand_nl?service=WMS&';
    %
    %wms_server='http://onearth.jpl.nasa.gov/wms.cgi?';
    %wms_layer='global_mosaic';
    %
    %request=GetMap&layers=global_mosaic&srs=EPSG:4326&format=image/jpeg&styles=&width=512&height=512&bbox=-180,-166,76,90
    %      request=GetMap&layers=global_mosaic&srs=EPSG:4326&width=512&height=512&bbox=-180,-166,76,90&format=image/jpeg&version=1.1.1&styles=
    %      request=GetMap&layers=global_mosaic&srs=EPSG:4326&width=512&height=512&bbox=-180,-166,76,90&format=image/jpeg&version=1.1.1&styles=default
    %      request=GetMap&layers=global_mosaic&srs=EPSG:4326&width=512&height=512&bbox=-180,-166,76,90&format=image/jpeg&styles=default

end

function INFO = getcap(wms_server,version)
if ~strcmp(wms_server(end),'?') && ~strcmp(wms_server(end),'&')
    if isempty(strfind(wms_server,'?'))
        query='?';
    else
        query='&';
    end
else
    query='';
end
url = [wms_server query 'service=WMS&request=GetCapabilities'];
if nargin>1
    url = [url '&version=' version];
end
try
    XML = xmlread(url);
catch e
    error('Unable to parse XML file returned by WMS getCapabilities call. Please check syntax!\n%s',url)
end
INFO.Type = 'WMS';
%INFO.XML = XML;
Doc = XML.getLastChild; % skip <!DOCTYPE ...> and comments at highest level
version = getAttrib(Doc,'version');
v = sscanf(version,'%f',1);
if isequal(char(Doc.getNodeName),'WMT_MS_Capabilities') || ...
        isequal(char(Doc.getNodeName),'WMS_Capabilities')
    % OK
elseif strcmp(version,'1.1.1')
    error('WMT_MS_Capabilities expected as first level element for WMS 1.1.1.\nCheck %s',url)
elseif strcmp(version,'1.3.0')
    error('WMS_Capabilities expected as first level element for WMS 1.3.0.\nCheck %s',url)
elseif isempty(version)
    error('Unrecognized first level XML element returned by server; is this a WMS server?\nCheck %s',url)
elseif nargin==1 && ~isempty(v) && v>1.3
    % try supported version
    INFO = getcap(wms_server,'1.3.0');
    return
elseif nargin==1 && ~isempty(v) && v>1.1
    % try supported version
    INFO = getcap(wms_server,'1.1.1');
    return
else
    error('Unsupported WMS version %s.\nCheck %s',version,url)
end
INFO.Version = version;
%
GetMapReq = xparse('getRecursiveNamedChild',Doc,'Capability','Request','GetMap');
F=xparse('getNamedChildren',GetMapReq,'Format');
INFO.GetMap.Formats = xparse('getChar',F);
GetURL=xparse('getRecursiveNamedChild',GetMapReq,'DCPType','HTTP','Get','OnlineResource');
INFO.GetMap.URL = getAttrib(GetURL,'xlink:href');
%
Layer = xparse('getRecursiveNamedChild',Doc,'Capability','Layer');
INFO.Data.Title = xparse('getChar',xparse('getNamedChildren',Layer,'Title'));
INFO.Data.SRS   = xparse('getChar',xparse('getNamedChildren',Layer,'SRS'));
Layers = xparse('getNamedChildren',Layer,'Layer');
for i = 1:length(Layers)
    INFO.Data.Layer(i).Name  = xparse('getChar',xparse('getNamedChild',Layers(i),'Name'));
    INFO.Data.Layer(i).Title = xparse('getChar',xparse('getNamedChild',Layers(i),'Title'));
    BB = xparse('getNamedChild',Layers(i),'LatLonBoundingBox');
    INFO.Data.Layer(i).LonRange = [getAttrib(BB,'minx','double') getAttrib(BB,'maxx','double')];
    INFO.Data.Layer(i).LatRange = [getAttrib(BB,'miny','double') getAttrib(BB,'maxy','double')];
end

function Val = getAttrib(Element,Attrib,type)
Val = char(Element.getAttribute(Attrib));
if nargin>2
    switch type
        case 'double'
            Val = str2double(Val);
    end
end

function [IMG,lon,lat] = getimage(INFO,wms_layer,lon_range,lat_range)
debug = 0;
% always get data in WGS84 coordinates
wms_proj='EPSG:4326';
if ischar(INFO)
    INFO = getcap(INFO);
end
wms_server = INFO.GetMap.URL;
version    = INFO.Version;
%
if nargin<2
    ilyr=-1;
else
    ilyr = find(ustrcmpi(wms_layer,{INFO.Data.Layer.Name}));
end
if ilyr<0
    if length(INFO.Data.Layer)==1
        ilyr=1;
        wms_layer = INFO.Data.Layer.Name;
    else
        error(['Invalid layer selected. Choose one from' sprintf('\n- %s',INFO.Data.Layer.Name)])
    end
else
    wms_layer = INFO.Data.Layer(ilyr).Name;
end
lyr = INFO.Data.Layer(ilyr);
%
if ismember('image/jpeg',INFO.GetMap.Formats)
    wms_format = 'image/jpeg';
    format     = 'jpg';
elseif ismember('image/png',INFO.GetMap.Formats)
    wms_format = 'image/png';
    format     = 'png';
elseif ismember('image/tiff',INFO.GetMap.Formats)
    wms_format = 'image/tiff';
    format     = 'tiff';
else
    error('Unable to identify supported bitmap format.')
end
%
if ~ismember(wms_proj,INFO.Data.SRS)
    if ismember('EPSG:900913',INFO.Data.SRS) % EPSG:3857
        wms_proj = 'EPSG:900913';
    else
        error('Requested projection "%s" not available from server.',wms_proj)
    end
end
%
if nargin>2
    lon_range(1) = max(lyr.LonRange(1),lon_range(1));
    lon_range(2) = min(lyr.LonRange(2),lon_range(2));
    lat_range(1) = max(lyr.LatRange(1),lat_range(1));
    lat_range(2) = min(lyr.LatRange(2),lat_range(2));
else
    lon_range = lyr.LonRange;
    lat_range = lyr.LatRange;
end
%
switch INFO.Type
    case 'WMS'
        % determine optimal resolution
        res=sqrt((lon_range(2)-lon_range(1))*(lat_range(2)-lat_range(1))/(750)^2);
        ds=[1 1/2 1/4 1/5 1/8 1/10 1/12 1/15 1/20 1/30];
        ds=[ds ds/60];
        [~,i]=min((ds-res).^2);
        ds = ds(i);
        % adjust lon/lat ranges
        lon_range(1)=max(lyr.LonRange(1),floor(lon_range(1)/ds)*ds);
        lon_range(2)=min(lyr.LonRange(2),ceil(lon_range(2)/ds)*ds);
        lat_range(1)=max(lyr.LatRange(1),floor(lat_range(1)/ds)*ds);
        lat_range(2)=min(lyr.LatRange(2),ceil(lat_range(2)/ds)*ds);
        % determine number of pixels
        nx=ceil((lon_range(2)-lon_range(1))/ds);
        ny=ceil((lat_range(2)-lat_range(1))/ds);
        % The query specifies lon,lat range and desired image size in pixels.
        %styles=<blank|'short_int'|...>
        %transparant=<'false'|'true'>
        % Bounding box
        v = sscanf(version,'%f',1);
        if ~isempty(v) && v>=1.3
            bbox = sprintf('%g,%g,%g,%g',lat_range(1),lon_range(1),lat_range(2),lon_range(2));
        else
            bbox = sprintf('%g,%g,%g,%g',lon_range(1),lat_range(1),lon_range(2),lat_range(2));
        end
        url=sprintf('%sservice=WMS&request=GetMap&version=%s&srs=%s&layers=%s&styles=&format=%s&width=%d&height=%d&bbox=%s&',...
            wms_server,version,wms_proj,wms_layer,wms_format,nx,ny,bbox);
        try
            [pix,cmap]=imread(url,format);
        catch e
            error('The WMS call didn''t return a %s file. Please check syntax!\n%s',upper(format),url)
        end
        if isempty(cmap)
            IMG = pix;
        else
            IMG = uint8(255*ind2rgb(pix,cmap));
        end
        %
        lon = lon_range;
        lat = fliplr(lat_range);
    case 'TMS'
        if isempty(strfind(wms_server,'${'))
            wms_server = [wms_server '${version}/${layer}/${z}/${x}/${y}.${format}'];
        end
        %
        wms_server = strrep(wms_server,'${version}',version); %version parameter, set in the config file. Defaults to 1.0.0.
        wms_server = strrep(wms_server,'${format}',format);   %format parameter, set in the config file. Defaults to 'jpg'.
        wms_server = strrep(wms_server,'${layer}',wms_layer); %layer parameter, set in the config file. Defaults to nothing.
        %
        zLvl=19;
        lon_range(2)=lon_range(1)+(lon_range(2)-lon_range(1))*(1-2*eps);
        lat_range(1)=lat_range(2)+(lat_range(1)-lat_range(2))*(1-2*eps);
        %
        [xUL,yUL] = latlon2tile(lat_range(2),lon_range(1),zLvl);
        [xLR,yLR] = latlon2tile(lat_range(1),lon_range(2),zLvl);
        zLvl = round(zLvl-log(sqrt((xLR-xUL+1)*(yLR-yUL+1)*(prod(INFO.TileSize)/512^2)))/log(2));
        zLvl = min(zLvl,INFO.GetMap.TileLevels);
        %
        [xUL,yUL] = latlon2tile(lat_range(2),lon_range(1),zLvl);
        [xLR,yLR] = latlon2tile(lat_range(1),lon_range(2),zLvl);
        %
        if debug
            figure
            line(lon_range([1 2 2 1 1]),lat_range([1 1 2 2 1]),'color','g')
            for xPos = xUL:xLR
                for yPos = yUL:yLR
                    [lat,lon] = tile2latlon(xPos,yPos,zLvl);
                    line(lon([1:4 1]),lat([1:4 1]),'color','r')
                    text(mean(lon),mean(lat),sprintf('[%d,%d]',xPos,yPos),'horizontalAlignment','center','verticalAlignment','middle','color','r')
                end
            end
            drawnow
        end
        %
        IMG=repmat(uint8(0),[INFO.TileSize(2)*(yLR-yUL+1) INFO.TileSize(1)*(xLR-xUL+1) 3]);
        servernr = 1;
        for xPos = xUL:xLR
            for yPos = yUL:yLR
                url = wms_server;
                % http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png -> s = {'a','b','c'}
                % http://{s}.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}
                % http://t{s}.tiles.virtualearth.net/tiles/a'+toQuad({x},{y},{z})+'.jpeg?g=1398
                if strcmp(INFO.YOrigin,'top')
                    yReq = yPos;
                else
                    yReq = 2^zLvl-yPos-1;
                end
                url = strrep(url,'${s}',INFO.Servers{servernr}); %cycle through servers
                url = strrep(url,'${x}',num2str(xPos));          %x position of the tile.
                url = strrep(url,'${y}',num2str(yReq));          %y position of the tile. This can be either from the top or the bottom of the tileset, based on whether the YOrigin parameter is set to true or false.
                url = strrep(url,'${z}',num2str(zLvl));          %zoom level.
                %quadkey algorithm BingMaps
                bin = dec2bin([yPos xPos],zLvl);
                quadkey=dec2base(bin2dec(bin(:)'),4,zLvl);
                url = strrep(url,'${quadkey}',quadkey); %VirtualEarth - variable must be found in the ServerUrl element.
                try
                    [pix,cmap]=imread(url,format);
                catch e
                    error('The Tile Map call didn''t return a %s file. Please check syntax!\n%s',upper(format),url)
                end
                if ~isempty(cmap)
                    pix = uint8(255*ind2rgb(pix,cmap));
                end
                IMG((yPos-yUL)*INFO.TileSize(2)+(1:INFO.TileSize(2)),(xPos-xUL)*INFO.TileSize(1)+(1:INFO.TileSize(1)),:) = pix;
                %
                servernr = mod(servernr,length(INFO.Servers))+1;
            end
        end
        %
        [~,lon] = tile2latlon_ul([xUL xLR+1],0,zLvl);
        [lat,~] = tile2latlon_ul(0,yUL+(yLR-yUL+1)*(0:size(IMG,1))/size(IMG,1),zLvl);
        %
        if debug
            surf(lon,lat,repmat(-1,length(lat),2),'cdata',IMG,'facecolor','texturemap','edgecolor','none','parent',gca);
            set(gca,'zlim',[-2 0])
        end
end

function [xtile,ytile] = latlon2tile(lat, lon, zoom)
% tiles numbered from lon = -180, lat = 85.0511; first tile = [0 0]
% [lat lon] may be any coordinate within the tile
n       = 2^zoom;
xtile   = floor((lon + 180) / 360 * n);
%ytile   = floor((90 - lat) /  180 * n);
lat_rad = lat*pi/180;
ytile   = floor((1 - log(tan(lat_rad) + (1 / cos(lat_rad))) / pi) / 2 * n);

function [lat,lon] = tile2latlon(xtile, ytile, zoom)
% tiles numbered from lon = -180, lat = 85.0511; first tile = [0 0]
% get corners of tile in anti-clockwise direction starting from upper left corner
lat = zeros(1,4);
lon = zeros(1,4);
[lat(1),lon(1)] = tile2latlon_ul(xtile  , ytile  , zoom);
[lat(2),lon(2)] = tile2latlon_ul(xtile  , ytile+1, zoom);
[lat(3),lon(3)] = tile2latlon_ul(xtile+1, ytile+1, zoom);
[lat(4),lon(4)] = tile2latlon_ul(xtile+1, ytile  , zoom);

function [lat,lon] = tile2latlon_ul(xtile, ytile, zoom)
% tiles numbered from lon = -180, lat = 85.0511; first tile = [0 0]
% get upper left corner of tile
n       = 2^zoom;
lon     = xtile / n * 360 - 180;
%lat     = 90 - ytile / n * 180;
lat_rad = atan(sinh(pi * (1 - 2 * ytile / n)));
lat     = lat_rad*180/pi;