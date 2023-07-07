function plotHis(varargin)
%unstruc.plotHis Plots a single timeseries for waterlevels at one station.
%   unstruc.plotHis(hisdata, statindex) expects hisdata output from
%       unstruc.readHis and statindex is index within station list.

%   See also unstruc.readHis, unstruc.plotHisStations.

% $Id: plotHis.m 13354 2010-11-02 18:16:37Z dam_ar $

[ah,args, nargs] = axescheck(varargin{:});
if isempty(ah)
    ah = gca;
else
    axes(ah);
end
cla;

if nargs < 2
    error('unstruc.plotHis: expected at least two arguments: hisdata and station index.');
end
hisdata   = args{1};
statindex = args{2};
%% Start plotting
statname = deblank(hisdata.station_name(:,statindex)');
lhs = plot(hisdata.time, hisdata.waterlevel(statindex,:));
legend({['Waterlevel at ''', statname, '''']});
title(statname,'Interpreter','none');

%xticks = 7*24*3600+3600*[0 12 24 36 48];
%set(ah, 'Xtick',xticks);
%set(ah, 'XtickLabel',{datestr(xticks(1)/24/3600,'HH:MM'),...
%                      datestr(xticks(2)/24/3600,'HH:MM'),...
%                      datestr(xticks(3)/24/3600,'HH:MM'),...
%                      datestr(xticks(4)/24/3600,'HH:MM')
%                      });
set(ah,'Box','on');

end
