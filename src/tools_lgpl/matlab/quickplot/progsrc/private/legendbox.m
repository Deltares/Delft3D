function out = legendbox(cmd,varargin)
%LEGENDBOX Display legend.

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
    case {'init','initialize'}
        h = varargin{1};
        units = get(h,'units');
        set(h,'units','centimeters')
        p=get(h,'position');
        legendnr = 1;
        allaxes = findall(get(h,'parent'),'type','axes');
        for i = 1:length(allaxes)
            a = allaxes(i);
            if isequal(getappdata(a,'LegendBox'),1)
                name = get(a,'tag');
                n = sscanf(lower(name),'legend %i',1);
                if ~isempty(n)
                    legendnr = max(legendnr,n+1);
                end
            end
        end
        set(h,'handlevisibility','off', ...
            'box','on', ...
            'units',units, ...
            'tag',sprintf('Legend %i',legendnr), ...
            'xtick',[],'ytick',[], ...
            'xlim',[0 p(3)],'ylim',[0 p(4)])
        setappdata(h,'LegendBox',1)
        %setappdata(h,'NonDataObject',1)
        Items.type = 'legend root';
        Items.arguments = [];
        Items.bottom = [];
        Items.handles = [];
        Items.tag = '';
        setappdata(h,'LegendItems',Items)

    case 'add'
        [h,Items,arguments] = getitems(varargin);
        y = Items(end).bottom;
        if isempty(y)
            ylim = get(h,'ylim');
            y = ylim(2)-0.1;
        end
        %
        Items(end+1).type = arguments{1};
        Items(end).arguments = arguments;
        switch arguments{1}
            case 'text'
                % {string}
                ht = text(0.1,y,arguments{2}, ...
                    'parent',h, ...
                    'horizontalalignment','left', ...
                    'verticalalignment','top');
                Items(end).handles = ht;
            case 'line'
                % {handle string}
                hl = arguments{2};
                string = arguments{3};
                if ~isequal(size(hl),[1 1]) || ~ishandle(hl)
                    error('Invalid line handle')
                elseif ~ischar(string) || ~isequal(numel(string),size(string,2))
                    error('Invalid legend string')
                end
                h1 = line([0.1 0.5],y-0.2+[0 0], ...
                    'parent',h, ...
                    'linestyle',get(hl,'linestyle'), ...
                    'linewidth',get(hl,'linewidth'), ...
                    'color',get(hl,'color'));
                h2 = line(0.3,y-0.2, ...
                    'parent',h, ...
                    'marker',get(hl,'marker'), ...
                    'markerfacecolor',get(hl,'markerfacecolor'), ...
                    'markeredgecolor',get(hl,'markeredgecolor'), ...
                    'markersize',get(hl,'markersize'), ...
                    'color',get(hl,'color'));
                ht = text(0.6,y,string, ...
                    'parent',h, ...
                    'horizontalalignment','left', ...
                    'verticalalignment','top');
                Items(end).handles = [h1 h2 ht];
            case 'patch'
                % {handle string}
                hl = arguments{2};
                string = arguments{3};
                if ~isequal(size(hl),[1 1]) || ~ishandle(hl)
                    error('Invalid line handle')
                elseif ~ischar(string) || ~isequal(numel(string),size(string,2))
                    error('Invalid legend string')
                end
                %
                h1 = patch([0.1 0.5 0.5 0.1],y-[0.1 0.1 0.4 0.4],1, ...
                    'parent',h, ...
                    'linestyle',get(hl,'linestyle'), ...
                    'linewidth',get(hl,'linewidth'), ...
                    'facecolor',get(hl,'facecolor'), ...
                    'marker',get(hl,'marker'), ...
                    'markerfacecolor',get(hl,'markerfacecolor'), ...
                    'markeredgecolor',get(hl,'markeredgecolor'), ...
                    'markersize',get(hl,'markersize'), ...
                    'edgecolor',get(hl,'edgecolor'));
                ht = text(0.6,y,string, ...
                    'parent',h, ...
                    'horizontalalignment','left', ...
                    'verticalalignment','top');
                Items(end).handles = [h1 ht];
            otherwise
                error('Unknown legendbox item: %s', arguments{1})
        end
        y = y-0.5;
        Items(end).bottom = y;
        tag = ['TODO-' num2str(rand)];
        Items(end).tag = tag;
        setappdata(h,'LegendItems',Items)
        out = tag;

    case 'delete'
        [h,Items,arguments] = getitems(varargin);
        %
        tags = {Items.tag};
        tag = arguments{1};
        i = ustrcmpi(tag,tags);
        if i<2 % i=-1 (no unique match) and i=1 (Legend Root) are not allowed
            error('Unable to identify legend item "%s".',tag)
        end
        delete(Items(i).handles);
        Items(i) = [];
        setappdata(h,'LegendItems',Items)
        %
        legendbox('refresh',h)

    case 'refresh'
        [h,Items] = getitems(varargin);
        %
        units = get(h,'units');
        set(h,'units','centimeters')
        p=get(h,'position');
        set(h,'units',units, ...
            'xlim',[0 p(3)],'ylim',[0 p(4)])
        %
        ylim = get(h,'ylim');
        y = ylim(2)-0.1;
        for i = 2:length(Items)
            ht = Items(i).handles;
            switch Items(i).type
                case 'text'
                    xy = get(ht,'position');
                    xy(1) = 0.1;
                    xy(2) = y;
                    set(ht,'position',xy)
                case 'line'
                    xy = get(ht(1),'ydata');
                    xy(:) = y-0.2;
                    set(ht(1),'xdata',[0.1 0.5],'ydata',xy)
                    %
                    xy = get(ht(2),'ydata');
                    xy(:) = y-0.2;
                    set(ht(2),'xdata',0.3,'ydata',xy)
                    %
                    xy = get(ht(3),'position');
                    xy(1) = 0.6;
                    xy(2) = y;
                    set(ht(3),'position',xy)
                case 'patch'
                    xy = y-[0.1 0.1 0.4 0.4];
                    set(ht(1),'xdata',[0.1 0.5 0.5 0.1],'ydata',xy)
                    %
                    xy = get(ht(2),'position');
                    xy(1) = 0.6;
                    xy(2) = y;
                    set(ht(2),'position',xy)
            end
            y = y-0.5;
            Items(i).bottom = y;
        end
        %
        setappdata(h,'LegendItems',Items)

    case 'children'
        [h,Items] = getitems(varargin);
        out = {Items(2:end).tag};

    case 'find'
        if nargin>1
            hfig = varargin{1};
        else
            hfig = 0;
        end
        %
        hax = findall(hfig,'type','axes');
        for i = 1:length(hax)
            lbox = getappdata(hax(i),'LegendBox');
            if ~isequal(lbox,1)
                hax(i) = 0;
            end
        end
        hax(hax==0) = [];
        %
        out = hax;
        
    otherwise
        error('Unknown legendbox command: %s',cmd)
end


function [h,Items,arguments] = getitems(arguments)
if isempty(arguments) || ...
        ~isequal(size(arguments{1}),[1 1]) || ...
        ~ishandle(arguments{1})
    h = gcf;
else
    h = arguments{1};
    arguments = arguments(2:end);
end
switch get(h,'type')
    case {'root','figure'}
        h = legendbox('find',h);
        lbox = 0;
    case 'axes'
        lbox = getappdata(h,'LegendBox');
        if ~isequal(lbox,1)
            h = get(h,'parent');
            lbox = 0;
        end
    otherwise
        lbox = 0;
        while ~isequal(get(h,'type'),'figure')
            h = get(h,'parent');
        end
end
if ~lbox
    h = legendbox('find',h);
end
if isempty(h)
    error('Unable to locate legendbox')
end
Items = getappdata(h,'LegendItems');
