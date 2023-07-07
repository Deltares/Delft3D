function plotHisStations(varargin)
%unstruc.plotHisStations Plots all stations present in history data for
%   interactive visualization of timeseries.
%   unstruc.plotHisStations(hisdata) expects output from unstruc.readHis.
%   unstruc.plotHisStations(hisdata, ldbfilename) also plots land boundary.
%
%   See also unstruc.readHis

% $Id: plotHisStations.m 13354 2010-11-02 18:16:37Z dam_ar $

global stat_hdl statname_hdl; % Used for highlighting current station in callback

hisdata = varargin{1};
if nargin >= 2
    ldbfile = varargin{2};
else
    ldbfile = '';
end

fh = gcf; % Use current figure (or open new one)
clf;
fullscreen = get(0,'ScreenSize');
fheight = fullscreen(4)-125;
set(fh, 'Position', [5,50,floor(fheight*0.65),fheight]);
figure(fh);

% Chart on top, timeseries below it.
chart_hdl = subplot('Position',[0.05000    0.53    0.90    0.43]);%2,1,1);
his_hdl   = subplot('Position',[0.05000    0.05    0.90    0.43]);%2,1,1);

%% Plot land boundary (optional)
% Enable this to read and plot land boundary:
if false
global ldbx ldby;
if isempty(ldbx) || isempty(ldby) % Only read LDB once to save time.
    [ldbx, ldby] = readLdb('Awvs_coastline2.ldb');
end

unstruc.plotLdb(chart_hdl, ldbx, ldby);
end % en/disable ldb plot

%% Start plotting stations
nstat = size(hisdata.station_name,2);
axes(chart_hdl);
xmin = min(hisdata.station_x_coord);
xmax = max(hisdata.station_x_coord);
ymin = min(hisdata.station_y_coord);
ymax = max(hisdata.station_y_coord);
dx = xmax-xmin;
dy = ymax-ymin;
margin=.05;
xmin = xmin-margin*dx;
xmax = xmax+margin*dx;
ymin = ymin-margin*dy;
ymax = ymax+margin*dy;

xcurlim = get(chart_hdl, 'XLim');
ycurlim = get(chart_hdl, 'YLim');
if xmin < xcurlim(1) || xmax > xcurlim(2) || ...
   ymin < ycurlim(1) || ymax > ycurlim(2)
    axis(chart_hdl, [xmin, xmax, ymin, ymax]);
end
hold on;
for i=1:nstat
    statname     = deblank(hisdata.station_name(:,i)');

    % Create station as label, and set callback function to call
    % unstruc.plotHis upon a user mouse click.
    hit_hdl = text(hisdata.station_x_coord(i), hisdata.station_y_coord(i),'#');
    set(hit_hdl, 'HorizontalAlignment','center','ButtonDownFcn',{@prv_plothis_wrapper,i},'FontSize',12,'FontWeight','bold','Color',[.6 .6 .6],'Clipping','on');%,'Visible','off');

    statname_hdl = text(hisdata.station_x_coord(i), hisdata.station_y_coord(i), ['  ', statname]);
    set(statname_hdl, 'ButtonDownFcn',{@prv_plothis_wrapper,i},'FontSize',7,'Clipping','on','Interpreter','none');
end
hold off;
xlabel(chart_hdl, '');
ylabel(chart_hdl, '');
set(chart_hdl, 'XTick',[], 'YTick',[], 'Box','on');
title(chart_hdl, 'Click a station for timeseries plot.');

%% Wrapper function to call unstruc.plotHis through mouseclicks
    function prv_plothis_wrapper(src, eventdata, statindex)
	statname     = deblank(hisdata.station_name(:,statindex)');
    fprintf('You clicked station ''%s''.\n', statname);
    unstruc.plotHis(his_hdl, hisdata, statindex);

    % %% Now highlight the selected station
    figure(fh);
    axes(chart_hdl);
    hold on;
    if ~isempty(stat_hdl)
        try
        delete(stat_hdl);
        delete(statname_hdl);
        stat_hdl=[];
        statname_hdl=[];
        catch e
        end
    end
    stat_hdl     = plot(hisdata.station_x_coord(statindex), hisdata.station_y_coord(statindex), 'o','MarkerFaceColor','r','MarkerEdgeColor','r','Clipping','on');
    statname_hdl = text(hisdata.station_x_coord(statindex), hisdata.station_y_coord(statindex), ['  ', statname]);
    set(statname_hdl,'Color','r','FontSize',7,'Clipping','on','Interpreter','none');
    hold off;
end

end