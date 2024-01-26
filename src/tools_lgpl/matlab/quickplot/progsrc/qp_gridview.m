function [out,out2]=qp_gridview(cmd,varargin)
%QP_GRIDVIEW Helper routine for Grid View interface.
%   FIG = QP_GRIDVIEW(GRID,RANGE) initialise interface with grid and
%   selected range. The RANGE argument is optional. The GRID may be dropped
%   if the RANGE is not specified. The GRID variable should be structure
%   containing fields
%     * X and Y for a 2D structured grid.
%     * XYZ, TRI for a 2D unstructured grid (XYZ array needs to contain XY
%       data only).
%     * XY, SEG for a network.
%   The function returns a figure handle. The RANGE structure should
%   contain fields Type and Range. Valid range types are 'none', 'point',
%   'range', 'line', 'lineseg', 'pwline', 'genline'. See the 'getrange'
%   call output for the contents of the Range field.
%
%   QP_GRIDVIEW('setgrid',FIG,GRID) update the grid to be used in the
%   specified Grid View interface FIG.
%
%   GRID = QP_GRIDVIEW('getgrid',FIG) retrieve the grid used in the
%   specified Grid View interface FIG.
%
%   QP_GRIDVIEW('setrange',FIG,RANGE) update the selected range of the
%   specified Grid View interface FIG to the specified RANGE.
%
%   RANGE = QP_GRIDVIEW('getrange',FIG) get the currently selected range of
%   the specified Grid View interface FIG. The RANGE structure
%   contains both selection type and selection indices.
%
%   [RANGETYPE,RANGEINDEX] = ... get the currently selected range as a
%   range type and range index instead of one range structure.
%
%   QP_GRIDVIEW('callback',FIG,F,arg1,arg2,...) set the callback function
%   to F with the specified arguments; this function will be called each
%   time selected range changes. The new RANGE should be enquired by means
%   of the 'getrange' call.

%   Obsolete structured grid syntax:
%
%   F = QP_GRIDVIEW(X,Y) initialise interface for a structured grid with
%   given X and Y coordinates.
%
%   F = QP_GRIDVIEW(X,Y,RANGE) initialise interface with grid and range.
%
%   QP_GRIDVIEW('setgrid',F,X,Y) update grid used.

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

if nargin==0
    cmd='initialize';
    UsrRange=[];
    GRID.X=[];
    GRID.Y=[];
elseif ischar(cmd)
    % handled below
else
    if isstruct(cmd)
        GRID=cmd;
        n=1;
    else
        GRID.X=cmd;
        GRID.Y=varargin{1};
        n=2;
    end
    cmd='initialize';
    if nargin>n
        UsrRange=varargin{n};
    else
        UsrRange=[];
    end
end

switch cmd
    case 'resize'
        pos = get(gcbf,'position');
        A = findall(gcbf,'type','axes');
        xlim = get(A,'xlim');
        ylim = get(A,'ylim');
        if (ylim(2)-ylim(1))/(xlim(2)-xlim(1))>pos(4)/pos(3)
            set(A,'xlimmode','auto','plotBoxAspectRatio',[pos(3:4) 1])
        else
            set(A,'ylimmode','auto','plotBoxAspectRatio',[pos(3:4) 1])
        end
    case 'convertmn2xy'
        F=varargin{1};
        G=findobj(F,'tag','GRID');
        GRID=get(G,'userdata');
        switch GRID.Type
            case 'sgrid'
                fprintf('To implement\n');
            case 'ugrid'
                switch GRID.ValLocation
                    case 'NODE'
                        XY = [GRID.X(GRID.Selected.Range) GRID.Y(GRID.Selected.Range)];
                    case 'FACE'
                        Nodes = GRID.FaceNodeConnect(GRID.Selected.Range,:);
                        missing = isnan(Nodes);
                        Nodes(missing) = 1;
                        X = GRID.X(Nodes);
                        Y = GRID.Y(Nodes);
                        X(missing) = 0;
                        Y(missing) = 0;
                        X = sum(X,2);
                        Y = sum(Y,2);
                        nNodes = sum(~missing,2);
                        X = X./nNodes;
                        Y = Y./nNodes;
                        XY = [X Y];
                    case 'EDGE'
                        Nodes = GRID.EdgeNodeConnect(GRID.Selected.Range,:);
                        for i = 2:size(Nodes,1)
                            if ismember(Nodes(i,1),Nodes(i-1,:))
                                if i==2
                                    Nodes(i-1,:) = [Nodes(i-1,Nodes(i-1,:)~=Nodes(i,1)) Nodes(i,1)];
                                end
                                Nodes(i,1) = NaN;
                            elseif ismember(Nodes(i,2),Nodes(i-1,:))
                                if i==2
                                    Nodes(i-1,:) = [Nodes(i-1,Nodes(i-1,:)~=Nodes(i,2)) Nodes(i,2)];
                                end
                                Nodes(i,2) = NaN;
                            end
                        end
                        Nodes = Nodes';
                        Nodes = Nodes(:);
                        Nodes(isnan(Nodes)) = [];
                        XY = [GRID.X(Nodes) GRID.Y(Nodes)];
                end
        end
        GRID.Selected.Type = 'genline';
        GRID.Selected.Range = XY;
        set(G,'userdata',GRID)
        localdrawsel(F)
        out = XY;

    case 'getrange'
        if nargin==1
            F=gcbf;
        else
            F=varargin{1};
        end
        G=findobj(F,'tag','GRID');
        GRID=get(G,'userdata');
        xx=GRID.Selected;
        if ~isfield(xx,'Type')
            xx.Type='none';
            xx.Range=[];
        elseif strcmp(GRID.Type,'sgrid') && size(xx.Range,2)==1
            [I,J]=ind2sub(size(GRID.X),xx.Range);
            xx.Range = [I J];
        end
        if nargout<=1
            out=xx;
        else
            out=xx.Type;
            out2=xx.Range;
        end

    case {'gridrangeup','gridrangemotion'}
        % Process movement and button presses for selecting a grid range
        G=findobj(gcbf,'tag','GRID');
        GRID=get(G,'userdata');
        if length(GRID.Selected.Range)==2
            ij0=GRID.Selected.Range;
        else
            ij0=GRID.Selected.Range([1 3]);
        end
        [i,j]=trackpnt(gcbf);
        i0=ij0(1);
        j0=ij0(2);
        localdrawsel(gcbf,'range',[i0 i j0 j])
        if strcmp(cmd,'gridrangeup')
            normalstate(gcbf)
        end

    case {'draglineup','draglinemotion','dragwholelineup','dragwholelinemotion'}
        % Process movement and button presses for selecting a whole line or
        % line segment
        G=findobj(gcbf,'tag','GRID');
        GRID=get(G,'userdata');
        %
        switch GRID.Type
            case 'sgrid'
                if length(GRID.Selected.Range)>2
                    ij0=GRID.Selected.Range([1 3]);
                else
                    ij0=GRID.Selected.Range;
                end
                i0=ij0(1);
                j0=ij0(2);
                %
                [i,j]=trackpnt(gcbf);
                midist=abs(i-i0);
                mjdist=abs(j-j0);
                switch cmd
                    case {'draglineup','draglinemotion'}
                        if midist<mjdist
                            j1=j;
                            i1=i0;
                        else
                            i1=i;
                            j1=j0;
                        end
                        localdrawsel(gcbf,'range',[i0 i1 j0 j1])
                        %---trackcoord start
                        %trackxy(gcbf,pnt)
                        MN=findobj(gcbf,'tag','MNcoord');
                        set(MN,'string',sprintf('m,n: %i,%i',i1,j1))
                        %---trackcoord stop
                    case {'dragwholelineup','dragwholelinemotion'}
                        if midist<mjdist
                            localdrawsel(gcbf,'line',[i0 0 j0 inf])
                        else
                            localdrawsel(gcbf,'line',[i0 inf j0 0])
                        end
                        %---trackcoord start
                        %trackxy(gcbf,pnt)
                        MN=findobj(gcbf,'tag','MNcoord');
                        set(MN,'string','m,n:')
                        %---trackcoord stop
                end
            case 'ugrid'
                % identify the grid lines
                if isfield(GRID,'GridLines')
                    GLines = GRID.GridLines;
                else
                    GLines = gridlines(GRID);
                end
                if isempty(GLines)
                    % if none exist, stop selection
                    cmd = 'up';
                elseif length(GLines)==1
                    % if one exists, select it and stop selection
                    localdrawsel(gcbf,'pwline',GLines{1})
                    cmd = 'up';
                else
                    % choose one of the lines
                    idx = cat(2,GLines{:});
                    i=trackpnt(gcbf,idx);
                    idx = i; %find(idx==i);
                    if idx(1)<=length(GLines{1})
                        idx = 1;
                    else
                        idx = 2;
                    end
                    if strcmp(cmd(end-1:end),'up')
                        % clear buffer
                        if isfield(GRID,'GridLines')
                            GRID = rmfield(GRID,'GridLines');
                        end
                    else
                        % buffer lines
                        if ~isfield(GRID,'GridLines')
                            GRID.GridLines = GLines;
                        end
                    end
                    set(G,'userdata',GRID)
                    localdrawsel(gcbf,'pwline',GLines{idx})
                end
        end
        if strcmp(cmd(end-1:end),'up')
            normalstate(gcbf)
        end

    case {'pwlinedown','pwlinemotion'}
        G=findobj(gcbf,'tag','GRID');
        GRID=get(G,'userdata');
        NFixed = getappdata(G,'NFixed');
        if isempty(NFixed)
            NFixed = 0;
        end
        %
        [i,j]=trackpnt(gcbf);
        if NFixed==0
            localdrawsel(gcbf,'pwline',[i j])
            switch cmd
                case 'pwlinedown'
                    setappdata(G,'NFixed',1);
            end
            %---trackcoord start
            %trackxy(gcbf,pnt)
            MN=findobj(gcbf,'tag','MNcoord');
            set(MN,'string',sprintf('m,n: %i,%i',i,j))
            %---trackcoord stop
        else
            Range = GRID.Selected.Range;
            Range(NFixed+1,:) = [i j];
            localdrawsel(gcbf,'pwline',Range)
            switch cmd
                case {'pwlinedown'}
                    setappdata(G,'NFixed',NFixed+1);
                    if ~strcmp(get(gcbf,'selectiontype'),'normal')
                        normalstate(gcbf)
                    end
            end
            %---trackcoord start
            %trackxy(gcbf,pnt)
            MN=findobj(gcbf,'tag','MNcoord');
            set(MN,'string',sprintf('m,n: %i,%i',i,j))
            %---trackcoord stop
        end

    case {'genlinedown','genrectdown','genlinemotion','genrectmotion', ...
            'genareadown','genareamotion'}
        G=findobj(gcbf,'tag','GRID');
        GRID=get(G,'userdata');
        NFixed = getappdata(G,'NFixed');
        if isempty(NFixed)
            NFixed = 0;
        end
        %
        pnt=get(get(G,'parent'),'currentpoint');
        pnt=pnt(1,1:2);
        %---trackcoord start
        trackxy(gcbf,pnt)
        MN=findobj(gcbf,'tag','MNcoord');
        set(MN,'string','')
        %---trackcoord stop
        if NFixed==0
            localdrawsel(gcbf,cmd(1:7),pnt)
            if strcmp(cmd(end-3:end),'down')
                setappdata(G,'NFixed',1)
            end
        else
            Range = GRID.Selected.Range;
            if ~isequal(pnt,Range(NFixed,:))
                Range(NFixed+1,:) = pnt;
            end
            localdrawsel(gcbf,cmd(1:7),Range)
            switch cmd
                case {'genlinedown'}
                    if size(Range,1)>NFixed
                        setappdata(G,'NFixed',NFixed+1)
                    end
                    if ~strcmp(get(gcbf,'selectiontype'),'normal')
                        normalstate(gcbf)
                    end
                case {'genareadown','genrectdown'}
                    setappdata(G,'NFixed',NFixed+1)
                    if ~strcmp(get(gcbf,'selectiontype'),'normal') || strcmp(cmd,'genrectdown')
                        switch GRID.ValLocation
                            case 'NODE'
                                X = GRID.X;
                                Y = GRID.Y;
                            case 'EDGE'
                                EdgeConnect = GRID.EdgeNodeConnect;
                                missing = false;
                                X = GRID.X(EdgeConnect);
                                Y = GRID.Y(EdgeConnect);
                            case 'FACE'
                                Faces = GRID.FaceNodeConnect;
                                missing = isnan(Faces);
                                Faces(missing) = 1;
                                X = GRID.X(Faces);
                                Y = GRID.Y(Faces);
                        end
                        if strcmp(cmd,'genareadown')
                            idx = inpolygon(X,Y,Range(:,1),Range(:,2));
                        else
                            idx = X>=min(Range(:,1)) & X<=max(Range(:,1)) & ...
                                  Y>=min(Range(:,2)) & Y<=max(Range(:,2));
                        end
                        if ~strcmp(GRID.ValLocation,'NODE')
                            idx = all(idx | missing,2);
                        end
                        localdrawsel(gcbf,'range',{find(idx)'})
                        normalstate(gcbf)
                    end
            end
        end

    case 'trackcoord'
        % Default mouse tracking function: x,y coordinates in status bar
        G=findobj(gcbf,'tag','GRID');
        pnt=get(get(G,'parent'),'currentpoint');
        if ~isempty(pnt)
            pnt=pnt(1,1:2);
            trackxy(gcbf,pnt)
        end
        MN=findobj(gcbf,'tag','MNcoord');
        set(MN,'string','')

    case {'selpointup','selpointmotion','draglinedown','dragwholelinedown','gridrangedown'}
        [i,j]=trackpnt(gcbf);
        localdrawsel(gcbf,'point',[i,j])
        switch cmd
            case 'selpointup'
                normalstate(gcbf)
            case 'draglinedown'
                set(gcbf,'WindowButtonDownFcn','qp_gridview draglineup')
                set(gcbf,'WindowButtonMotionFcn','qp_gridview draglinemotion')
                set(gcbf,'WindowButtonUpFcn','')
            case 'dragwholelinedown'
                set(gcbf,'WindowButtonDownFcn','qp_gridview dragwholelineup')
                set(gcbf,'WindowButtonMotionFcn','qp_gridview dragwholelinemotion')
                set(gcbf,'WindowButtonupFcn','')
            case 'gridrangedown'
                set(gcbf,'WindowButtonDownFcn','qp_gridview gridrangeup')
                set(gcbf,'WindowButtonMotionFcn','qp_gridview gridrangemotion')
                set(gcbf,'WindowButtonupFcn','')
        end

    case {'pathdown','pathmotion'}
        G=findobj(gcbf,'tag','GRID');
        GRID=get(G,'userdata');
        i=trackpnt(gcbf);
        NFixed = getappdata(G,'NFixed');
        if isempty(NFixed)
            NFixed = 0;
        end
        %
        if NFixed==0
            points = [];
        else
            points = GRID.Selected.Range(1:NFixed,:);
            DistanceState = getappdata(G,'DistanceState');
            if DistanceState.distfromlast(i) == 0
                DistanceState = determine_frompoint(DistanceState,i);
                setappdata(G,'DistanceState',DistanceState)
                if DistanceState.distfromlast(i) ==0
                    i = trackpnt(gcbf,DistanceState.distfromlast~=0);
                end
            end
            frompoint = DistanceState.frompoint;
            ilast = points(end);
            ilist = repmat(NaN,10000,1);
            j = 1;
            while i~=ilast
                ilist(j) = i;
                j = j+1;
                i = frompoint(i);
            end
            i = ilist(j-1:-1:1);
        end
        %
        points = cat(1,points,i);
        localdrawsel(gcbf,'pwline',points)
        switch cmd
            case 'pathdown'
                setappdata(G,'NFixed',size(points,1))
                if strcmp(get(gcbf,'selectiontype'),'normal')
                    set(gcbf,'WindowButtonMotionFcn','qp_gridview pathmotion')
                    ilast = points(end);
                    style = getappdata(gcbf,'pathstyle');
                    DistanceState = init_distance(GRID,style,ilast);
                    %
                    DistanceState = determine_frompoint(DistanceState,ilast);
                    setappdata(G,'DistanceState',DistanceState)
                else
                    normalstate(gcbf)
                end
        end

    case 'gridviewlineseg'
        menusoff(gcbf)
        zoom(gcbf,'off');
        set(gcbf,'WindowButtonDownFcn','qp_gridview draglinedown')
        set(gcbf,'WindowButtonMotionFcn','qp_gridview selpointmotion')
        localdrawsel(gcbf,'none')
        qp_gridview selpointmotion
        
    case 'gridviewline'
        menusoff(gcbf)
        zoom(gcbf,'off');
        set(gcbf,'WindowButtonDownFcn','qp_gridview dragwholelinedown')
        set(gcbf,'WindowButtonMotionFcn','qp_gridview selpointmotion')
        localdrawsel(gcbf,'none')
        qp_gridview selpointmotion
        
    case 'gridviewrange'
        menusoff(gcbf)
        zoom(gcbf,'off');
        set(gcbf,'WindowButtonDownFcn','qp_gridview gridrangedown')
        set(gcbf,'WindowButtonMotionFcn','qp_gridview selpointmotion')
        localdrawsel(gcbf,'none')
        qp_gridview selpointmotion
        
    case 'gridviewpiecewise'
        menusoff(gcbf)
        zoom(gcbf,'off');
        set(gcbf,'WindowButtonDownFcn','qp_gridview pwlinedown')
        set(gcbf,'WindowButtonMotionFcn','qp_gridview pwlinemotion')
        localdrawsel(gcbf,'none')
        qp_gridview pwlinemotion
        
    case 'gridviewarbline'
        menusoff(gcbf)
        zoom(gcbf,'off');
        set(gcbf,'WindowButtonDownFcn','qp_gridview genlinedown')
        set(gcbf,'WindowButtonMotionFcn','qp_gridview genlinemotion')
        localdrawsel(gcbf,'none')
        qp_gridview genlinemotion
        
    case 'gridviewarbrect'
        menusoff(gcbf)
        zoom(gcbf,'off');
        set(gcbf,'WindowButtonDownFcn','qp_gridview genrectdown')
        set(gcbf,'WindowButtonMotionFcn','qp_gridview genrectmotion')
        localdrawsel(gcbf,'none')
        qp_gridview genrectmotion
        
    case 'gridviewarbarea'
        menusoff(gcbf)
        zoom(gcbf,'off');
        set(gcbf,'WindowButtonDownFcn','qp_gridview genareadown')
        set(gcbf,'WindowButtonMotionFcn','qp_gridview genareamotion')
        localdrawsel(gcbf,'none')
        qp_gridview genareamotion
        
    case 'gridviewpoint'
        menusoff(gcbf)
        zoom(gcbf,'off');
        set(gcbf,'WindowButtonMotionFcn','qp_gridview selpointmotion')
        set(gcbf,'WindowButtonUpFcn','qp_gridview selpointup')
        localdrawsel(gcbf,'none')
        qp_gridview selpointmotion
        
    case 'gridviewpath'
        pos=get(gcbf,'position');
        uicm=findobj(gcbf,'tag','gridviewpath_contextmenu');
        set(uicm,'position',get(0,'pointerlocation')-pos(1:2),'visible','on')
        
    case {'gridviewpath_shortest_path','gridviewpath_least_segments'}
        switch cmd
            case 'gridviewpath_shortest_path'
                setappdata(gcbf,'pathstyle','shortest')
            case 'gridviewpath_least_segments'
                setappdata(gcbf,'pathstyle','least')
        end
        menusoff(gcbf)
        zoom(gcbf,'off');
        set(gcbf,'WindowButtonDownFcn','qp_gridview pathdown')
        set(gcbf,'WindowButtonMotionFcn','qp_gridview selpointmotion')
        localdrawsel(gcbf,'none')
        qp_gridview pathmotion
        
    case 'gridviewall'
        % Convenience function to setrange wholegrid
        localdrawsel(gcbf,'wholegrid')
        qp_gridview execcallback

    case 'getgrid'
        F = varargin{1};
        G = findall(F,'tag','GRID');
        out = get(G,'userdata');
        return

    case 'setloc'
        % Change grid location
        F = varargin{1};
        NewLoc = varargin{2};
        %
        G = findall(F,'tag','GRID');
        GRID = get(G,'userdata');
        UseGrid = get(F,'userdata');
        %
        OldLoc = UseGrid{4}; % or GRID.ValLocation
        OldRange = qp_gridview('getrange',F);
        [NewRange,RangeMax] = switchrange(GRID,OldLoc,OldRange,NewLoc);
        %
        GRID.ValLocation = NewLoc;
        UseGrid{4} = NewLoc;
        set(G,'userdata',GRID)
        set(F,'userdata',UseGrid)
        qp_gridview('setrange',F,NewRange)
        %
        out = NewRange;
        out2 = RangeMax;
        
    case 'setgrid'
        % Change grid to new grid
        F=varargin{1};
        ValLocation = '';
        if isstruct(varargin{2})
            GRID = varargin{2};
            if nargin>3
                ValLocation = varargin{3};
            end
        else
            GRID.X=varargin{2};
            if nargin>3
                GRID.Y=varargin{3};
            else
                GRID.Y=[];
            end
        end
        if ~isempty(ValLocation)
            GRID.ValLocation = ValLocation;
        elseif ~isfield(GRID,'ValLocation')
            GRID.ValLocation = 'NODE';
        end
        if isfield(GRID,'Connect')
            GRID.FaceNodeConnect = GRID.Connect;
            GRID = rmfield(GRID,'Connect');
        end
        if isfield(GRID,'EdgeConnect')
            GRID.EdgeNodeConnect = GRID.EdgeConnect;
            GRID = rmfield(GRID,'EdgeConnect');
        end
        if isfield(GRID,'SEG')
            GRID.EdgeNodeConnect = GRID.SEG;
            GRID.FaceNodeConnect = [];
            GRID.X = GRID.XY(:,1);
            GRID.Y = GRID.XY(:,2);
            GRID = rmfield(GRID,'SEG');
            GRID = rmfield(GRID,'XY');
        elseif isfield(GRID,'TRI')
            GRID.FaceNodeConnect = GRID.TRI;
            GRID.XYZ=squeeze(GRID.XYZ(1,:,1,:));
            GRID.X = GRID.XYZ(:,1);
            GRID.Y = GRID.XYZ(:,2);
            GRID = rmfield(GRID,'TRI');
            GRID = rmfield(GRID,'XYZ');
        end
        %
        normalstate(F)
        localdrawgrid(F,GRID)
        localdrawsel(F,'none')

    case 'initialize'
        % Create gridview figure
        MonSize = qp_getscreen;
        unt = get(0,'defaultfigureunits');
        set(0,'defaultfigureunits','pixels')
        pos = get(0,'defaultfigureposition');
        set(0,'defaultfigureunits',unt)
        pos(1:2) = pos(1:2)-1+MonSize(1:2);
        pos(1:2) = min(pos(1:2),MonSize(1:2)+MonSize(3:4)-pos(3:4)-[0 70]);
        if ismember('zbuffer',set(0,'defaultfigurerenderer'))
            renderer = 'zbuffer';
        else
            renderer = 'opengl';
        end
        F = figure('integerhandle','off', ...
            'color',qp_settings('gridviewbackgroundcolor')/255, ...
            'renderer',renderer, ...
            'doublebuffer','on', ...
            'name','Grid View', ...
            'numbertitle','off', ...
            'handlevisibility','off', ...
            'keypressfcn','qp_gridview keypress', ...
            'resizefcn','qp_gridview resize', ...
            'units','pixels', ...
            'position',pos, ...
            'visible','off');
        set(F,'menubar','none')
        TB = uitoolbar('Parent',F);
        if matlabversionnumber > 5.02
            set(F,'ToolBar','none');
        end
        A=axes('parent',F,'unit','normalized','position',[0 0 1 1], ...
            'defaultlineclipping','off');
        uicontrol('parent',F,'style','edit','hittest','off', ...
            'enable','inactive','units','pixels','string','x,y:', ...
            'horizontalalignment','left','position',[1 1 150 20], ...
            'tag','XYcoord');
        uicontrol('parent',F,'style','edit','hittest','off', ...
            'enable','inactive','units','pixels','string','', ...
            'horizontalalignment','left','position',[151 1 70 20], ...
            'tag','MNcoord');
        G=surface([],[],[],'parent',A,'tag','GRID','userdata',GRID);
        selcolor = qp_settings('gridviewselectioncolor')/255;
        %SelectedGrid
        erasemode = {};
        surface([],[],[],'parent',A, ...
            'facecolor',selcolor,'edgecolor','none', ...
            'tag','SELSURF',erasemode{:});
        %SelectedPatch
        patch('vertices',[],'faces',[],'parent',A, ...
            'facecolor',selcolor,'edgecolor','none', ...
            'FaceOffsetFactor',1,'FaceOffsetBias',0, ...
            'tag','SELPATCH',erasemode{:});
        %SelectedLine
        line('xdata',[],'ydata',[],'parent',A, ...
            'color',selcolor,'linestyle','-','linewidth',4, ...
            'tag','SELLINE',erasemode{:});
        %SelectedPoint
        line('xdata',[],'ydata',[],'parent',A, ...
            'color',selcolor,'linestyle','none', ...
            'marker','.','markersize',12, ...
            'tag','SELPOINT',erasemode{:});
        set(A,'color',get(F,'color'),'xtick',[],'ytick',[], ...
            'da',[1 1 1],'view',[0 90],'xcolor',get(F,'color'), ...
            'ycolor',get(F,'color'))
        xl=limits(A,'xlim'); xl=xl+[-1 1]*diff(xl)/20;
        yl=limits(A,'ylim'); yl=yl+[-1 1]*diff(yl)/20;
        if ~isfinite(xl), xl=[0 1]; yl=[0 1]; end
        set(A,'xlim',xl,'ylim',yl)
        set(allchild(A),'clipping','off','hittest','off')
        uim=uimenu('label','&Select','parent',F);
        %---
        qp_menutool(uim,TB,'gridviewpoint', ...
            'Grid &Point','Select point',0)
        %---
        %line,wholeline
        qp_menutool(uim,TB,'gridviewline', ...
            'Grid &Line','Select grid line',1)
        %lineseg,line
        qp_menutool(uim,TB,'gridviewlineseg', ...
            'Grid Line &Segment','Select grid line segment',0)
        %pwline,pwline
        qp_menutool(uim,TB,'gridviewpiecewise', ...
            'Piecewise Grid &Line','Select piecewise grid line',0)
        %path
        qp_menutool(uim,TB,'gridviewpath', ...
            'Shortest &Path','Select shortest path',0)
        uicm=uicontextmenu('parent',F,'tag','gridviewpath_contextmenu');
        qp_uimenu(uicm, ...
            {'gridviewpath_shortest_path','Shortest Path',1,1,0
            'gridviewpath_least_segments','Least Number of Segments',1,1,0});
        %---
        %genline,genline
        qp_menutool(uim,TB,'gridviewarbline', ...
            'A&rbitrary Line','Select arbitrary line',1)
        %---
        %range,gridrange
        qp_menutool(uim,TB,'gridviewrange', ...
            'Grid &Range','Select grid range',1)
        %wholegrid,wholegrid
        qp_menutool(uim,TB,'gridviewall' ...
            ,'Whole &Grid','Select whole grid',0)
        %----
        %genrect,genrect
        qp_menutool(uim,TB,'gridviewarbrect', ...
            'Arbitrary Re&ctangle','Select arbitrary rectangle',1)
        %genarea,genarea
        qp_menutool(uim,TB,'gridviewarbarea', ...
            'Arbitrary &Area','Select arbitrary area',0)
        %---
        menusoff(F)
        GRID.Selected.Type='none';
        GRID.Selected.Range=[];
        set(G,'userdata',GRID)
        qp_gridview('setgrid',F,GRID)
        if ~isempty(UsrRange)
            if isstruct(UsrRange)
                GRID.Selected=UsrRange;
            else
                GRID.Selected.Type='range';
                GRID.Selected.Range=UsrRange;
            end
            qp_gridview('setrange',F,GRID.Selected)
        end
        if nargout>0
            out=F;
        end
        set(F,'visible','on')
        set(F,'windowbuttonmotionfcn','qp_gridview trackcoord')

    case 'keypress'
        % Handle key presses
        F = gcbf;
        ch = get(gcbf,'currentcharacter');
        if ~isempty(ch)
            switch ch
                case 27
                    % ESCAPE character
                    normalstate(F)
                    G = findobj(F,'tag','GRID');
                    GRID = get(G,'userdata');
                    qp_gridview('setrange',F,GRID.Selected)
                otherwise % all other keys
            end
        end

    case 'callback'
        % Set callback function to be executed upon selection change
        F=varargin{1};
        G=findobj(F,'tag','GRID');
        A=get(G,'parent');
        set(A,'userdata',varargin(2:end))

    case 'execcallback'
        % Trigger execution of callback function
        F=gcbf;
        G=findobj(F,'tag','GRID');
        A=get(G,'parent');
        UD=get(A,'userdata');
        if ~isempty(UD)
            feval(UD{1},UD{2:end})
        end

    case 'setrange'
        % Set selection type and range
        F=varargin{1};
        UsrRange=varargin{2};
        %
        set(F,'WindowButtonMotionFcn','qp_gridview trackcoord')
        %
        if ~isempty(UsrRange)
            if ischar(UsrRange)
                selection.Type = UsrRange;
                if nargin>3
                    selection.Range = varargin{3};
                else
                    selection.Range = [];
                end
            elseif isstruct(UsrRange)
                selection = UsrRange;
                if isempty(selection.Range) && ~strcmp(selection.Type,'wholegrid')
                    selection.Type = 'none';
                end
            else
                selection.Type  = 'range';
                selection.Range = UsrRange;
            end
        else
            selection.Type  = 'none';
            selection.Range = [];
        end
        localdrawsel(F,selection)

    otherwise
        fprintf('Unkwown command: %s\n',cmd);
end

function GLines = gridlines(GRID)
i = GRID.Selected.Range(1);
switch GRID.ValLocation
    case 'FACE'
        FNC = GRID.FaceNodeConnect;
        % check if this is a quad, if not then there are no "grid lines" 
        if sum(~isnan(FNC(i,:)))~=4
            GLines = {};
            return
        end
        % cell with four nodes, hence fout edges
        % construct face edge connectivity for full grid
        Missing = isnan(FNC)';
        NFaces = size(FNC,1);
        NMaxNd = size(FNC,2);
        NEdges = sum(~isnan(FNC),2);
        for nd = 4:NMaxNd
            M = isnan(FNC(:,nd));
            if any(M)
                FNC(M,nd) = FNC(M,nd);
            end
        end
        FNC = FNC';
        FNC = FNC(ceil([1:0.5:end 1]),:);
        FNC = reshape(FNC,[2 numel(FNC)/2]);
        FNC = sort(FNC)';
        % FNC now contains NMaxNd node pairs of "edge nodes", give them
        % unique numbers and mark all dummy edges as NaN
        [~,~,iedge] = unique(FNC,'rows');
        iface = repmat(1:NFaces,[NMaxNd 1]);
        iface = iface(:);
        %
        iedge1 = iedge;
        iedge1(Missing) = NaN;
        [uedge1,IA1] = unique(iedge1);
        iedge1(IA1) = NaN;
        [uedge2,IA2] = unique(iedge1);
        M = isnan(uedge2);
        IA2(M) = [];
        uedge2(M) = [];
        EFC = NaN(max(iedge),2);
        EFC(uedge1,1) = iface(IA1);
        EFC(uedge2,2) = iface(IA2);
        %
        FFC = EFC(iedge,:);
        FFC(FFC(:,1)==iface,1)=NaN;
        FFC(FFC(:,2)==iface,2)=NaN;
        FFC = max(FFC,[],2);
        FFC = reshape(FFC,[NMaxNd NFaces])';
        %
        GLines = cell(1,2);
        SingleGridline = 0;
        for d = [1 3 2 4]
            if d<=2
                n = i;
                in = 1;
                gline = zeros(1,2*NFaces);
                gline(in) = n;
                CircularGridline = 0;
            elseif CircularGridline
                % already did a full loop and did administration, so just
                % continue with second direction.
                continue
            end
            n2 = FFC(n,d);
            while ~isnan(n2)
                in = in+1;
                gline(in) = n2;
                if NEdges(n2)~=4 % only continue if this face is also a quad
                    break
                end
                idir = find(FFC(n2,:)==n);
                if idir>2
                    idir = idir-2;
                else
                    idir = idir+2;
                end
                if n2==i 
                    if idir==d % detecting infinite loop
                        CircularGridline = 1;
                        in = in-1;
                        break
                    else % OK, we're going through this face a second time hence we only have one grid line
                        SingleGridline = 1;
                    end
                end
                n = n2;
                n2 = FFC(n,idir);
            end
            if CircularGridline % always d<=2
                if ~SingleGridline
                    % The current list of faces starts and ends in the
                    % selected face. Since this is the face of interest, it
                    % seems to be better to split the loop as far as
                    % possible from this face.
                    im = round(in/2);
                    GLines{d} = gline([im:in 1:im]);
                else % Wow, special case: a single circular grid line!
                    % Halfway might be close to the first face, so we need
                    % to search for the face "furthest away".
                    thisNode = find(gline(1:in)==i);
                    in2 = thisNode(2);
                    if in2 > in/2+1
                        % most faces between 1 and in2
                        im = round(in2/2);
                    else
                        % most faces between in2 and in+1 (=1)
                        im = round((in2+in)/2);
                    end
                    GLines = {gline([im:in 1:in])};
                    break
                end
            elseif d>2 % We have searched in two directions
                if SingleGridline
                    GLines = {gline(1:in)};
                    break
                else
                    GLines{d-2} = gline(1:in);
                end
            else % d<=2
                % flip the faces and continue searching in the other
                % direction
                gline(1:in) = gline(in:-1:1);
                n = i;
            end
        end
    case 'EDGE'
        fprintf('To implement: gridline for EDGE location\n');
    case 'NODE'
        fprintf('To implement: gridline for NODE location\n');
end

function [mdist,indrng,i1,j1]=NearDiag(dist,i0,j0)
sz=size(dist);
szi=sz(1);
szj=sz(2);
ind0=sub2ind(sz,i0,j0);

di=min(i0,j0)-1;
ir=i0-di;
jr=j0-di;
di=min(szi-ir,szj-jr);
ir=ir+(0:di);
jr=jr+(0:di);

ind=sub2ind(sz,ir,jr);
[m1dist,ind1]=min(dist(ind));
ir1=ir(ind1);
jr1=jr(ind1);
ind1=ind(ind1);
if ind1<ind0
    indrng1=ind0:-(szi+1):ind1;
else
    indrng1=ind0:(szi+1):ind1;
end

di=min(i0,szj-j0+1)-1;
ir=i0-di;
jr=j0+di;
di=min(szi-ir,jr-1);
ir=ir+(0:di);
jr=jr-(0:di);

ind=sub2ind(sz,ir,jr);
[m2dist,ind2]=min(dist(ind));
ir2=ir(ind2);
jr2=jr(ind2);
ind2=ind(ind2);
if ind2<ind0
    indrng2=ind0:-(szi-1):ind2;
else
    indrng2=ind0:(szi-1):ind2;
end

if m1dist<m2dist
    indrng=indrng1;
    mdist=m1dist;
    i1=ir1;
    j1=jr1;
else
    indrng=indrng2;
    mdist=m2dist;
    i1=ir2;
    j1=jr2;
end

function qp_menutool(uim,TB,tag,label,tooltip,separator)
prog = 'qp_gridview';
newmenu = uimenu('tag',tag,'label',label, ...
    'parent',uim, ...
    'callback',sprintf('%s %s',prog,tag));
if separator
    set(newmenu,'separator','on')
end
qp_toolbarpush(TB,tag,separator,tooltip,prog);

function localdrawsel(F,varargin)
SelectedGrid=findobj(F,'tag','SELSURF');
SelectedPatch=findobj(F,'tag','SELPATCH');
SelectedLine=findobj(F,'tag','SELLINE');
SelectedPoint=findobj(F,'tag','SELPOINT');
G=findobj(F,'tag','GRID');
GRID=get(G,'userdata');
%
if nargin==1
    selection = GRID.Selected;
elseif nargin==2
    if isstruct(varargin{1})
        selection = varargin{1};
    elseif ischar(varargin{1})
        selection.Type = varargin{1};
        selection.Range = [];
    end
elseif nargin==3
    selection.Type = varargin{1};
    selection.Range = varargin{2};
end
%
Range = selection.Range;
switch selection.Type
    case 'none'
        set(SelectedPoint,'xdata',[],'ydata',[])
        set(SelectedLine ,'xdata',[],'ydata',[])
        set(SelectedPatch,'vertices',[],'faces',[])
        set(SelectedGrid ,'xdata',[],'ydata',[],'zdata',[])

    case 'point'
        switch GRID.Type
            case 'sgrid'
                switch GRID.ValLocation
                    case 'NODE'
                        set(SelectedPatch,'vertices',[],'faces',[])
                        set(SelectedLine,'xdata',[],'ydata',[])
                        set(SelectedPoint, ...
                            'xdata',GRID.X(Range(1),Range(2)), ...
                            'ydata',GRID.Y(Range(1),Range(2)))
                        set(SelectedGrid,'xdata',[],'ydata',[],'zdata',[])
                    case 'EDGE'
                    case 'FACE'
                        set(SelectedPatch,'vertices',[],'faces',[])
                        set(SelectedLine,'xdata',[],'ydata',[])
                        set(SelectedPoint,'xdata',[],'ydata',[])
                        Range = max(Range,2);
                        set(SelectedGrid,'xdata',GRID.X(Range(1)-1:Range(1),Range(2)-1:Range(2)), ...
                                         'ydata',GRID.Y(Range(1)-1:Range(1),Range(2)-1:Range(2)), ...
                                         'zdata',zeros(2,2))
                end
            otherwise
                i = Range(1);
                selection.Range = i; % remove second index for UGRID points
                switch GRID.ValLocation
                    case 'NODE'
                        set(SelectedPatch,'vertices',[],'faces',[])
                        set(SelectedLine,'xdata',[],'ydata',[])
                        set(SelectedPoint, ...
                            'xdata',GRID.X(i), ...
                            'ydata',GRID.Y(i))
                        set(SelectedGrid,'xdata',[],'ydata',[],'zdata',[])
                    case 'EDGE'
                        j = GRID.EdgeNodeConnect(i,:);
                        set(SelectedPatch,'vertices',[],'faces',[])
                        set(SelectedPoint,'xdata',[],'ydata',[])
                        set(SelectedLine, ...
                            'xdata',GRID.X(j), ...
                            'ydata',GRID.Y(j))
                        set(SelectedGrid,'xdata',[],'ydata',[],'zdata',[])
                    case 'FACE'
                        set(SelectedPatch,'vertices',[GRID.X GRID.Y],'faces',GRID.FaceNodeConnect(i,:))
                        set(SelectedLine,'xdata',[],'ydata',[])
                        set(SelectedPoint,'xdata',[],'ydata',[])
                        set(SelectedGrid,'xdata',[],'ydata',[],'zdata',[])
                end
        end

    case {'range','wholegrid'}
        if strcmp(selection.Type,'wholegrid')
            switch GRID.Type
                case 'sgrid'
                    switch GRID.ValLocation
                        case 'NODE'
                            Range = {1:size(GRID.X,1),1:size(GRID.X,2)};
                        case 'EDGE'
                        case 'FACE'
                            Range = {1:size(GRID.X,1),1:size(GRID.X,2)};
                    end
                case 'ugrid'
                    switch GRID.ValLocation
                        case 'NODE'
                            if ~isempty(GRID.FaceNodeConnect)
                                Range = {1:max(GRID.FaceNodeConnect(:))};
                            else
                                Range = {1:max(GRID.EdgeNodeConnect(:))};
                            end
                        case 'EDGE'
                            Range = {1:size(GRID.EdgeNodeConnect,1)};
                        case 'FACE'
                            Range = {1:size(GRID.FaceNodeConnect,1)};
                    end
            end
        elseif ~iscell(Range)
            Range={min(Range(1:2)):max(Range(1:2)) min(Range(3:4)):max(Range(3:4))};
        end
        switch GRID.Type
            case 'sgrid'
                switch GRID.ValLocation
                    case 'NODE'
                        im = Range{1};
                        if length(Range)>=2
                            in = Range{2};
                        else
                            in = 1;
                        end
                        if length(im)==1 && length(in)==1
                            set(SelectedPatch,'vertices',[],'faces',[])
                            set(SelectedLine,'xdata',[],'ydata',[])
                            set(SelectedPoint, ...
                                'xdata',GRID.X(im,in), ...
                                'ydata',GRID.Y(im,in))
                            set(SelectedGrid,'xdata',[],'ydata',[],'zdata',[])
                        elseif length(im)==1 || length(in)==1
                            set(SelectedPatch,'vertices',[],'faces',[])
                            set(SelectedPoint,'xdata',[],'ydata',[])
                            set(SelectedLine, ...
                                'xdata',GRID.X(im,in), ...
                                'ydata',GRID.Y(im,in))
                            set(SelectedGrid,'xdata',[],'ydata',[],'zdata',[])
                        else
                            set(SelectedPoint,'xdata',[],'ydata',[])
                            set(SelectedLine,'xdata',[],'ydata',[])
                            set(SelectedPatch,'vertices',[],'faces',[])
                            set(SelectedGrid, ...
                                'xdata',GRID.X(im,in), ...
                                'ydata',GRID.Y(im,in), ...
                                'zdata',zeros(length(im),length(in)))
                        end
                    case 'FACE'
                        im = Range{1};
                        if im(1)>1
                            im = [im(1)-1 im];
                        end
                        if length(Range)>=2
                            in = Range{2};
                            if in(1)>1
                                in = [in(1)-1 in];
                            end
                        else
                            in = 1;
                        end
                        %
                        set(SelectedPoint,'xdata',[],'ydata',[])
                        set(SelectedLine,'xdata',[],'ydata',[])
                        set(SelectedPatch,'vertices',[],'faces',[])
                        if length(im)<=1 || length(in)<=1
                            set(SelectedGrid,'xdata',[],'ydata',[],'zdata',[]);
                        else
                            set(SelectedGrid, ...
                                'xdata',GRID.X(im,in), ...
                                'ydata',GRID.Y(im,in), ...
                                'zdata',zeros(length(im),length(in)))
                        end
                end
            case 'ugrid'
                CNECT = []; % no faces
                ledge = []; % no edges
                ip = []; % no nodes
                switch GRID.ValLocation
                    case 'NODE'
                        ip    = Range{1};
                        ledge = all(ismember(GRID.EdgeNodeConnect,Range{1}),2);
                        ip    = ip(~ismember(ip,GRID.EdgeNodeConnect(ledge,:)));
                        ledge = find(ledge);
                    case 'EDGE'
                        ledge = Range{1};
                    case 'FACE'
                        CNECT = GRID.FaceNodeConnect(Range{1},:);
                end
                %
                if ~isempty(ledge) && isfield(GRID,'FaceNodeConnect')
                    % determine Edge-Face relationship (code copy)
                    Faces = GRID.FaceNodeConnect;
                    missing = isnan(Faces);
                    NEdges = sum(~missing(:));
                    Edges = zeros(3,NEdges);
                    ie = 0;
                    for j = 1:size(Faces,2)
                        for i = 1:size(Faces,1)
                            if ~isnan(Faces(i,j))
                                ie = ie+1;
                                Edges(1,ie) = Faces(i,j);
                                if j==size(Faces,2) || isnan(Faces(i,j+1))
                                    Edges(2,ie) = Faces(i,1);
                                else
                                    Edges(2,ie) = Faces(i,j+1);
                                end
                                Edges(3,ie) = i;
                            end
                        end
                    end
                    Edges(1:2,:) = sort(Edges(1:2,:));
                    Edges = Edges';
                    % determine for which Faces all Edges have been selected
                    EdgeSel = sort(GRID.EdgeNodeConnect(ledge,:),2);
                    yEdges = ismember(Edges(:,1:2),EdgeSel,'rows');
                    NEdgesIncluded = accumarray(Edges(:,3),double(yEdges));
                    NEdgesTotal    = sum(~missing,2);
                    lface = NEdgesIncluded==NEdgesTotal;
                    CNECT = GRID.FaceNodeConnect(lface,:);
                    % determine which Edges are not part of the selected faces
                    FacesIncluded  = find(lface);
                    faceEdges = ismember(Edges(:,3),FacesIncluded);
                    ledge = ledge(~ismember(EdgeSel,Edges(faceEdges,1:2),'rows'));
                end
                if ~isempty(ledge)
                    Edges = GRID.EdgeNodeConnect(ledge,:)';
                    X = GRID.X(Edges);
                    Y = GRID.Y(Edges);
                    X(3,:) = NaN;
                    Y(3,:) = NaN;
                else
                    X = [];
                    Y = [];
                end
                set(SelectedLine,'xdata',X(:),'ydata',Y(:))
                set(SelectedPoint,'xdata',GRID.X(ip),'ydata',GRID.Y(ip))
                for l = 1:size(CNECT,1)
                    np = sum(~isnan(CNECT(l,:)));
                    if np == 0
                        CNECT(l,:) = 1;
                    else
                        CNECT(l,np+1:end) = CNECT(l,np);
                    end
                end
                set(SelectedPatch,'vertices',[GRID.X GRID.Y],'faces',CNECT)
                set(SelectedGrid,'xdata',[],'ydata',[],'zdata',[])
        end
        
    case 'genline'
        if isempty(Range)
            set(SelectedPoint,'xdata',[],'ydata',[])
            set(SelectedLine,'xdata',[],'ydata',[])
        elseif size(Range,1)==1
            set(SelectedLine,'xdata',[],'ydata',[])
            set(SelectedPoint, ...
                'xdata',Range(:,1), ...
                'ydata',Range(:,2))
        else
            set(SelectedPoint,'xdata',[],'ydata',[])
            set(SelectedLine, ...
                'xdata',Range(:,1), ...
                'ydata',Range(:,2))
        end
        set(SelectedPatch,'vertices',[],'faces',[])
        set(SelectedGrid,'xdata',[],'ydata',[],'zdata',[])

    case 'genarea'
        nPnt = size(Range,1);
        if nPnt==1
            set(SelectedLine,'xdata',[],'ydata',[])
            set(SelectedPoint, ...
                'xdata',Range(:,1), ...
                'ydata',Range(:,2))
            set(SelectedPatch,'vertices',[],'faces',[])
        elseif nPnt==2
            set(SelectedPoint,'xdata',[],'ydata',[])
            set(SelectedLine, ...
                'xdata',Range(:,1), ...
                'ydata',Range(:,2))
            set(SelectedPatch,'vertices',[],'faces',[])
        else
            set(SelectedPoint,'xdata',[],'ydata',[])
            set(SelectedLine,'xdata',[],'ydata',[])
            set(SelectedPatch, ...
                'vertices',Range, ...
                'faces',1:nPnt)
        end
        set(SelectedGrid,'xdata',[],'ydata',[],'zdata',[])
        
    case 'genrect'
        nPnt = size(Range,1);
        if nPnt==1
            set(SelectedPoint, ...
                'xdata',Range(:,1), ...
                'ydata',Range(:,2))
            set(SelectedLine,'xdata',[],'ydata',[])
            set(SelectedPatch,'vertices',[],'faces',[])
        else
            set(SelectedPoint,'xdata',[],'ydata',[])
            set(SelectedLine,'xdata',[],'ydata',[])
            xy=[Range(1,:)
                Range(1,1) Range(2,2)
                Range(2,1) Range(1,2)
                Range(2,:)];
            set(SelectedPatch, ...
                'vertices',xy, ...
                'faces',[1 2 4 3])
        end
        set(SelectedGrid,'xdata',[],'ydata',[],'zdata',[])

    case 'line'
        switch GRID.ValLocation
            case 'NODE'
                if ~isfinite(Range(4))
                    % [i0 0 j0 inf]
                    set(SelectedPoint,'xdata',[],'ydata',[])
                    set(SelectedLine, ...
                        'xdata',GRID.X(Range(1),:), ...
                        'ydata',GRID.Y(Range(1),:))
                else
                    % [i0 inf j0 0]
                    set(SelectedPoint,'xdata',[],'ydata',[])
                    set(SelectedLine, ...
                        'xdata',GRID.X(:,Range(3)), ...
                        'ydata',GRID.Y(:,Range(3)))
                end
                set(SelectedPatch,'vertices',[],'faces',[])
                set(SelectedGrid,'xdata',[],'ydata',[],'zdata',[])
            case 'FACE'
                if ~isfinite(Range(4))
                    % [i0 0 j0 inf]
                    im = Range(1)-[1 0];
                    in = 1:size(GRID.X,2);
                else
                    % [i0 inf j0 0]
                    im = 1:size(GRID.X,1);
                    in = Range(3)-[1 0];
                end
                set(SelectedGrid, ...
                    'xdata',GRID.X(im,in), ...
                    'ydata',GRID.Y(im,in), ...
                    'zdata',zeros(length(im),length(in)))
        end

    case 'pwline'
        switch GRID.Type
            case 'sgrid'
                RNG=piecewise(Range,size(GRID.X));
                i0 = RNG(:,1);
                j0 = RNG(:,2);
                ind=sub2ind(size(GRID.X),i0,j0);
                switch GRID.ValLocation
                    case 'NODE'
                        if length(ind)==1
                            set(SelectedLine,'xdata',[],'ydata',[])
                            set(SelectedPoint, ...
                                'xdata',GRID.X(ind), ...
                                'ydata',GRID.Y(ind))
                        else
                            set(SelectedPoint,'xdata',[],'ydata',[])
                            set(SelectedLine, ...
                                'xdata',GRID.X(ind), ...
                                'ydata',GRID.Y(ind))
                        end
                        set(SelectedPatch,'vertices',[],'faces',[])
                    case 'EDGE'
                    case 'FACE'
                        set(SelectedPoint,'xdata',[],'ydata',[])
                        set(SelectedLine,'xdata',[],'ydata',[])
                        set(SelectedPatch,'vertices',[GRID.X(:) GRID.Y(:)],'faces',[ind ind-1 [ind-1 ind]-size(GRID.X,1)])
                end
            case 'ugrid'
                ind = Range;
                switch GRID.ValLocation
                    case 'NODE'
                        if length(ind)==1
                            set(SelectedLine,'xdata',[],'ydata',[])
                            set(SelectedPoint, ...
                                'xdata',GRID.X(ind), ...
                                'ydata',GRID.Y(ind))
                        else
                            set(SelectedPoint,'xdata',[],'ydata',[])
                            set(SelectedLine, ...
                                'xdata',GRID.X(ind), ...
                                'ydata',GRID.Y(ind))
                        end
                        set(SelectedPatch,'vertices',[],'faces',[])
                    case 'EDGE'
                        X = GRID.X(GRID.EdgeNodeConnect(ind,:));
                        Y = GRID.Y(GRID.EdgeNodeConnect(ind,:));
                        X(:,3) = NaN;
                        Y(:,3) = NaN;
                        X = X';
                        Y = Y';
                        set(SelectedPoint,'xdata',[],'ydata',[])
                        set(SelectedLine, ...
                            'xdata',X(:), ...
                            'ydata',Y(:))
                        set(SelectedPatch,'vertices',[],'faces',[])
                    case 'FACE'
                        set(SelectedPoint,'xdata',[],'ydata',[])
                        set(SelectedLine,'xdata',[],'ydata',[])
                        set(SelectedPatch, ...
                            'vertices',[GRID.X GRID.Y], ...
                            'faces',GRID.FaceNodeConnect(ind,:))
                end
        end
        set(SelectedGrid,'xdata',[],'ydata',[],'zdata',[])
end
GRID.Selected = selection;
set(G,'userdata',GRID)


function localdrawgrid(F,GRID)
zoom(F,'off');
SelectedLine=findobj(F,'tag','SELLINE');
G=findobj(F,'tag','GRID');
Go=findobj(F,'tag','GRIDother');
A=get(SelectedLine,'parent');
delete(G)
delete(Go)
gridcol = qp_settings('gridviewgridcolor')/255;
off = 'off';
%
if isfield(GRID,'FaceNodeConnect') || isfield(GRID,'EdgeNodeConnect') % unstructured 1D or 2D (... or 3D)
    [GRID.Type] = deal('ugrid');
    if length(GRID)>1
        error('Multi-grid model not yet supported by Grid View.')
    end
    if isfield(GRID,'EdgeNodeConnect')
        eConnect = GRID.EdgeNodeConnect;
    else
        Faces = GRID.FaceNodeConnect;
        nc = size(Faces,2);
        iConnect = ceil(([0 0:2*nc-2])/2+0.1);
        eConnect = Faces(:,iConnect);
        ncP = sum(~isnan(Faces),2);
        eConnect(:,1) = Faces(sub2ind(size(Faces),(1:size(Faces,1))',ncP));
        eConnect = unique(sort(reshape(eConnect',[2 numel(Faces)]),1)','rows');
        eConnect(any(isnan(eConnect),2),:) = [];
        GRID.EdgeNodeConnect = eConnect;
    end
    
    % convert EdgeNodeConnect to a long list of node pairs that can be
    % transformed to x,y coordinates of a line. Note that we first create
    % triplets by duplicating the second column, and blank the resulting
    % 3rd X and Y coordinates before plotting.
    iNode = eConnect(:,[1 2 2])';
    iNode = iNode(:);
    if isempty(iNode)
        warning('Empty edge-node-connectivty array!')
    elseif any(isnan(iNode(:)))
        warning('NaN values in the edge-node-connectivity array!')
        iNode = [];
    else
        wrong_iNode = min(iNode);
        if wrong_iNode >= 1 % minimum is not wrong ...
            wrong_iNode = max(iNode);
        end
        if wrong_iNode < 1 || wrong_iNode > length(GRID.X)
            error('Invalid node index found in edge_node_connectivity table. Value (%i) outside range 1:%i.',wrong_iNode,length(GRID.X))
        end
    end
    X = GRID.X(iNode);
    Y = GRID.Y(iNode);
    X(3:3:end) = NaN;
    Y(3:3:end) = NaN;
    G=line('parent',A);
    set(G,'xdata',X(:),'ydata',Y(:),'color',gridcol);
    if ~isempty(X)
        off = 'on';
    end
elseif ~isempty(GRID.X) % structured
    GRID.Type = 'sgrid';
    GRID.X = GRID.X(:,:,1);
    GRID.Y = GRID.Y(:,:,1);
    if qp_settings('gridviewshowindices')
        gridstep = max(10,round(size(GRID.X)/10));
    else
        gridstep = [];
    end
    G=drawgrid(GRID.X,GRID.Y, ...
        'color',gridcol,'fontsize',8,'parent',A,'gridstep',gridstep);
    off='on';
else
    GRID.Type = 'none';
    G=surface([],[],[],'parent',A);
    off='off';
end
set(G(1),'tag','GRID','userdata',GRID)
set(G(2:end),'tag','GRIDother')
set(G,'clipping','off','hittest','off')
c = get(A,'children');
c1 = c(~ismember(c,G));
c2 = c(ismember(c,G));
set(A,'children',cat(1,c1,c2))
%
xl=limits(G,'xlim'); xl=xl+[-1 1]*max(0.00001,abs(diff(xl)*0.01))/20;
yl=limits(G,'ylim'); yl=yl+[-1 1]*max(0.00001,abs(diff(yl)*0.01))/20;
if ~isfinite(xl)
    xl=[0 1];
    yl=[0 1];
end
pos = get(F,'position');
if (yl(2)-yl(1))/(xl(2)-xl(1))>pos(4)/pos(3)
    xl = (xl(1)+xl(2))/2 + [-1 1]*pos(3)/pos(4)*(yl(2)-yl(1))/2;
else
    yl = (yl(1)+yl(2))/2 + [-1 1]*pos(4)/pos(3)*(xl(2)-xl(1))/2;
end
set(A,'xlim',xl,'ylim',yl,'plotBoxAspectRatio',[pos(3:4) 1])
delete(get(A,'zlabel')) % delete the old ZOOMAxesData applicationdata
drawnow
zoom(F,'reset');
zoom(F,off);
%
selectMenu = findall(F,'label','&Select');
set(selectMenu,'enable',off)
set(findall(selectMenu),'enable',off)
set(findall(F,'type','uipushtool'),'enable',off)
if strcmp(GRID.Type,'sgrid')
    set(findall(F,'tag','gridviewarbrect'),'enable','off')
    set(findall(F,'tag','gridviewarbarea'),'enable','off')
    set(findall(F,'tag','gridviewpath'),'enable','off') % currently path doesn't work for structured grid since it uses 1D indexing
else
    set(findall(F,'tag','gridviewrange'),'enable','off')
    set(findall(F,'tag','gridviewpiecewise'),'enable','off')
    set(findall(F,'tag','gridviewlineseg'),'enable','off')
    if ~strcmp(GRID.ValLocation,'FACE')
        set(findall(F,'tag','gridviewline'),'enable','off')
    end
    if strcmp(GRID.Type,'network') || ...
            ~isfield(GRID,'FaceNodeConnect') || ...
            isempty(GRID.FaceNodeConnect)
        set(findall(F,'tag','gridviewarbline'),'enable','off')
    end
end

function menusoff(F)
obj = cat(1,findall(F,'type','uimenu'),findall(F,'type','uipushtool'));
for i=1:length(obj)
    set(obj(i),'enable','off','userdata',get(obj(i),'enable'))
end

function menusreset(F)
obj = cat(1,findall(F,'type','uimenu'),findall(F,'type','uipushtool'));
for i=1:length(obj)
    enabled = get(obj(i),'userdata');
    if isequal(enabled,'on') || isequal(enabled,'off')
        set(obj(i),'enable',enabled)
    end
end

function trackxy(F,pnt)
XY = findobj(F,'tag','XYcoord');
xf = sprintf('%%.%if',min(3,6-floor(log10(abs(pnt(1))))));
yf = sprintf('%%.%if',min(3,6-floor(log10(abs(pnt(2))))));
set(XY,'string',sprintf(['x,y: ',xf,',',yf],pnt))

function [i,j] = trackpnt(F,idx)
% idx is optional subset
if nargin>1 && islogical(idx)
    idx = find(idx);
end
G = findobj(F,'tag','GRID');
GRID = get(G,'userdata');
pnt = get(get(G,'parent'),'currentpoint');
pnt = pnt(1,1:2);
switch GRID.ValLocation
    case 'NODE'
        if isfield(GRID,'X') % ugrid and sgrid
            X = GRID.X;
            Y = GRID.Y;
            if nargin>1
                X = X(idx);
                Y = Y(idx);
            end
            dist = (pnt(1)-X).^2+(pnt(2)-Y).^2;
            mdist = min(dist(:));
            [i,j] = find(dist==mdist);
            i = i(1);
            j = j(1);
            if nargin>1
                i = idx(i);
            end
        else
            i = 1;
            j = 1;
        end
    case 'EDGE'
        Edges = GRID.EdgeNodeConnect;
        X = GRID.X(Edges);
        Y = GRID.Y(Edges);
        %
        LM = (X(:,2)-X(:,1)).^2+(Y(:,2)-Y(:,1)).^2;
        L = ((X(:,2)-X(:,1)).*(pnt(1) - X(:,1)) + (Y(:,2)-Y(:,1)).*(pnt(2) - Y(:,1)))./LM;
        L = max(0,min(L,1));
        d = (pnt(1)-X(:,1)-L.*(X(:,2)-X(:,1))).^2 + (pnt(2)-Y(:,1)-L.*(Y(:,2)-Y(:,1))).^2;
        [dm,i]=min(d);
        j = 1;
    case 'FACE'
        if strcmp(GRID.Type,'ugrid')
            Faces = GRID.FaceNodeConnect;
            if nargin>1
                Faces = Faces(idx,:);
                % should do something to renumber ...
            end
        else
            Faces = repmat(NaN,[size(GRID.X) 4]);
            Faces(:,:,1)         = reshape(1:numel(GRID.X),size(GRID.X));
            Faces(2:end,:,2)     = Faces(1:end-1,:,1);
            Faces(2:end,2:end,3) = Faces(1:end-1,1:end-1,1);
            Faces(:,2:end,4)     = Faces(:,1:end-1,1);
            Faces = reshape(Faces,[numel(GRID.X) 4]);
            %
            % completely remove cells that one missing node
            %
            missing = isnan(Faces);
            Faces(any(missing,2),:) = NaN;
        end
        missing = isnan(Faces);
        Faces(missing) = 1;
        X = GRID.X(Faces);
        Y = GRID.Y(Faces);
        X(missing) = NaN;
        Y(missing) = NaN;
        inside = pnt(1)>=min(X,[],2) & pnt(1)<=max(X,[],2) & ...
                 pnt(2)>=min(Y,[],2) & pnt(2)<=max(Y,[],2);
        i = 0;
        if any(inside)
            ipvec = find(inside)';
            for ip = ipvec
                in = 1:sum(~missing(ip,:));
                if ~isempty(in) && inpolygon(pnt(1),pnt(2),X(ip,in),Y(ip,in))
                    i = ip;
                    break
                end
            end
        end
        %
        if i==0
            % Current point is not inside any polygon, just find "closest".
            % We define the "closest" polygon as the one with the centre
            % closest to the current point.
            np = sum(~missing,2);
            X(missing) = 0;
            X = sum(X,2)./max(np,1);
            Y(missing) = 0;
            Y = sum(Y,2)./max(np,1);
            %
            X(np==0) = NaN;
            Y(np==0) = NaN;
            %
            dist = (pnt(1)-X).^2+(pnt(2)-Y).^2;
            mdist = min(dist(:));
            i = find(dist==mdist);
            i = i(1);
        end
        if strcmp(GRID.Type,'sgrid')
            [i,j]=ind2sub(size(GRID.X),i);
        else
            j = 1;
        end
    otherwise
        i = 1;
        j = 1;
end
%
trackxy(gcbf,pnt)
MN=findobj(gcbf,'tag','MNcoord');
if strcmp(GRID.Type,'sgrid')
    set(MN,'string',sprintf('m,n: %i,%i',i,j))
    %
    if nargout==1
        i=sub2ind(size(GRID.X),i,j);
    end
else
    set(MN,'string',sprintf('%s: %i',lower(GRID.ValLocation),i))
end

function normalstate(F)
zoom(F,'off')
set(F,'WindowButtonDownFcn','')
set(F,'WindowButtonMotionFcn','')
set(F,'WindowButtonUpFcn','')
%
SelectedLine = findobj(F,'tag','SELLINE');
set(SelectedLine,'userdata',[])
%
G=findobj(F,'tag','GRID');
if ~isempty(G)
    if isappdata(G,'NFixed')
        rmappdata(G,'NFixed')
    end
    if isappdata(G,'DistanceState')
        rmappdata(G,'DistanceState')
    end
end
%
menusreset(F)
zoom(F,'inmode');
set(F,'WindowButtonMotionFcn','qp_gridview trackcoord')
qp_gridview execcallback
%qp_gridview('setrange',F,GRID.Selected)


function [NewRange,RangeMax] = switchrange(GRID,OldLoc,OldRange,NewLoc)
switch GRID.Type
    case 'ugrid'
        [NewRange,RangeMax] = switchrange_ugrid(GRID,OldLoc,OldRange,NewLoc);
    case 'sgrid'
        [NewRange,RangeMax] = switchrange_sgrid(GRID,OldLoc,OldRange,NewLoc);
end

function [NewRange,RangeMax] = switchrange_sgrid(GRID,OldLoc,OldRange,NewLoc)
switch NewLoc
    case 'NODE'
        RangeMax = size(GRID.X);
    case 'FACE'
        RangeMax = size(GRID.X);
end
NewRange = OldRange;
if strcmp(OldRange.Type,'genline')
    return
end
%
switch NewLoc
    case 'NODE'
        switch OldLoc
            case 'FACE'
                if strcmp(OldRange.Type,'range')
                    if iscell(OldRange.Range)
                        if OldRange.Range{1}(1)>1
                            NewRange.Range{1} = [OldRange.Range{1}(1)-1 OldRange.Range{1}];
                        end
                        if OldRange.Range{2}(1)>1
                            NewRange.Range{2} = [OldRange.Range{2}(1)-1 OldRange.Range{2}];
                        end
                    else
                        NewRange.Range = [sort(NewRange.Range(1:2)) sort(NewRange.Range(3:4))];
                        NewRange.Range([1 3]) = max(1,NewRange.Range([1 3])-1);
                    end
                end
        end
    case 'FACE'
        switch OldLoc
            case 'NODE'
                if strcmp(OldRange.Type,'range')
                    if iscell(OldRange.Range)
                        if length(OldRange.Range{1})>1
                            NewRange.Range{1} = OldRange.Range{1}(2:end);
                        end
                        if length(OldRange.Range{2})>1
                            NewRange.Range{2} = OldRange.Range{2}(2:end);
                        end
                    else
                        NewRange.Range = [sort(NewRange.Range(1:2)) sort(NewRange.Range(3:4))];
                        NewRange.Range([1 3]) = NewRange.Range([1 3])+1;
                    end
                elseif strcmp(OldRange.Type,'pwline')
                    NewRange.Range = max(OldRange.Range,2);
                end
        end
end

function [NewRange,RangeMax] = switchrange_ugrid(GRID,OldLoc,OldRange,NewLoc)
switch NewLoc
    case 'NODE'
        RangeMax = length(GRID.X);
    case 'EDGE'
        RangeMax = size(GRID.EdgeNodeConnect,1);
    case 'FACE'
        RangeMax = size(GRID.FaceNodeConnect,1);
end
if strcmp(OldRange.Type,'genline')
    NewRange = OldRange;
    return
end
%
switch NewLoc
    case 'NODE'
        switch OldLoc
            case 'EDGE'
                if strcmp(OldRange.Type,'pwline')
                    iEdge = OldRange.Range;
                    iNode = GRID.EdgeNodeConnect(iEdge,:)';
                    for i = 2:size(iNode,2)
                        if i==2
                            if ismember(iNode(1,1),iNode(:,2))
                               iNode(:,1) = iNode([2 1],1);
                            end
                        end
                        iNode(ismember(iNode(:,i),iNode(:,i-1)),i) = NaN;
                    end
                    iNode = iNode(~isnan(iNode));
                    NewRange.Type = 'pwline';
                    NewRange.Range = iNode;
                else
                    % find node numbers associated with the selected edges
                    iEdge = OldRange.Range{1};
                    find_point = length(iEdge)==1;
                    iNode = GRID.EdgeNodeConnect(iEdge,:);
                    iNode = unique(iNode(:));
                    if find_point
                        iNode = iNode(1);
                    end
                    NewRange.Type = 'range';
                    NewRange.Range = {iNode};
                end
            case 'FACE'
                if strcmp(OldRange.Type,'pwline')
                    iFace = OldRange.Range;
                    lCorner = ~ismember(GRID.FaceNodeConnect(iFace(1),:),GRID.FaceNodeConnect(iFace(2),:)) & ~isnan(GRID.FaceNodeConnect(iFace(1),:));
                    iNode0 = GRID.FaceNodeConnect(iFace(1),lCorner);
                    iNode0 = iNode0(1);
                    GRIDreduced = GRID;
                    GRIDreduced.ValLocation = 'NODE';
                    GRIDreduced.FaceNodeConnect = GRID.FaceNodeConnect(iFace,:);
                    %
                    DS0 = init_distance(GRIDreduced,'shortest',iNode0);
                    %
                    DistanceState = determine_frompoint(DS0,iNode0);
                    cl = cell(1,length(iFace)+1);
                    cl{1} = iNode0;
                    for i = 1:length(iFace)
                        if i==length(iFace)
                            lCorner = ~ismember(GRID.FaceNodeConnect(iFace(i),:),GRID.FaceNodeConnect(iFace(i-1),:)) & ~isnan(GRID.FaceNodeConnect(iFace(i),:));
                        else
                            lCorner = ismember(GRID.FaceNodeConnect(iFace(i),:),GRID.FaceNodeConnect(iFace(i+1),:)) & ~isnan(GRID.FaceNodeConnect(iFace(i),:));
                        end
                        iNode1 = GRID.FaceNodeConnect(iFace(i),lCorner);
                        %
                        DistanceState = determine_frompoint(DistanceState,iNode1(1));
                        iNode1(DistanceState.distfromlast(iNode1)==0)=[];
                        if length(iNode1)>1
                            [m,j] = min(DistanceState.distfromlast(iNode1));
                            iNode1 = iNode1(j);
                        end
                        ip = [];
                        iPrev = iNode1;
                        while iPrev~=iNode0
                            ip = [iPrev ip];
                            iPrev = DistanceState.frompoint(iPrev);
                        end
                        cl{i+1} = ip;
                        %
                        iNode0 = iNode1;
                        DS0.ilast = iNode0;
                        DistanceState = determine_frompoint(DS0,iNode0);
                    end
                    NewRange.Type = 'pwline';
                    NewRange.Range = cat(2,cl{:});
                else
                    % find node numbers associated with the selected nodes
                    switch OldRange.Type
                        case 'point'
                            iFace = OldRange.Range(1);
                        case 'range'
                            iFace = OldRange.Range{1};
                    end
                    find_point = length(iFace) == 1;
                    iNode = GRID.FaceNodeConnect(iFace,:);
                    iNode = unique(iNode(~isnan(iNode)));
                    if find_point
                        iNode = iNode(1);
                    end
                    NewRange.Type = 'range';
                    NewRange.Range = {iNode};
                end
            otherwise
                error('Transition from %s to %s not yet implemented',OldLoc,NewLoc)
        end
    case 'EDGE'
        switch OldLoc
            case {'NODE','FACE'}
                if strcmp(OldRange.Type,'pwline')
                    if strcmp(OldLoc,'NODE')
                        iNode = OldRange.Range;
                        [dummy,iEdge]=ismember(sort([iNode(1:end-1) iNode(2:end)],2),sort(GRID.EdgeNodeConnect,2),'rows');
                        NewRange.Type = 'pwline';
                        NewRange.Range = iEdge;
                    else
                        % convert FACE to NODE
                        NodeRange = switchrange(GRID,OldLoc,OldRange,'NODE');
                        % and subsequently NODE to EDGE
                        [NewRange,RangeMax] = switchrange(GRID,'NODE',NodeRange,NewLoc);
                    end
                else
                    if strcmp(OldLoc,'FACE')
                        % find node numbers associated with the selected nodes
                        switch OldRange.Type
                            case 'point'
                                iFace = OldRange.Range(1);
                            otherwise
                                iFace = OldRange.Range{1};
                        end
                        find_point = length(iFace)==1;
                        iNode = GRID.FaceNodeConnect(iFace,:);
                        iNode = unique(iNode(~isnan(iNode)));
                    else
                        switch OldRange.Type
                            case 'point'
                                iNode = OldRange.Range(1);
                            otherwise
                                iNode = OldRange.Range{1};
                        end
                        find_point = length(iNode)==1;
                    end
                    if find_point
                        % find any edge connected to this node
                        lEdge = any(ismember(GRID.EdgeNodeConnect,iNode),2);
                    else
                        % find edges for which all nodes are selected
                        lEdge = all(ismember(GRID.EdgeNodeConnect,iNode),2);
                    end
                    iEdge = find(lEdge);
                    if find_point
                        iEdge = iEdge(1);
                    end
                    NewRange.Type = 'range';
                    NewRange.Range = {iEdge};
                end
            otherwise
                error('Transition from %s to %s not yet implemented',OldLoc,NewLoc)
        end
    case 'FACE'
        switch OldLoc
            case {'NODE','EDGE'}
                if strcmp(OldRange.Type,'pwline')
                    switch OldLoc
                        case 'NODE'
                            iNode = OldRange.Range;
                            lFace = sum(ismember(GRID.FaceNodeConnect,iNode(1:2)) & ~isnan(GRID.FaceNodeConnect),2);
                            iFace1 = find(lFace==2);
                            iFace1 = iFace1(1);
                            lFace = any(ismember(GRID.FaceNodeConnect,iNode) & ~isnan(GRID.FaceNodeConnect),2);
                            iFace = find(lFace);
                            GRIDreduced = GRID;
                            GRIDreduced.ValLocation = 'FACE';
                            GRIDreduced.FaceNodeConnect = GRID.FaceNodeConnect(lFace,:);
                            iFace1 = find(iFace==iFace1);
                            %
                            DS0 = init_distance(GRIDreduced,'shortest',iFace1);
                            %
                            iFace0 = iFace1;
                            DistanceState = determine_frompoint(DS0,iFace0);
                            cl = cell(1,length(iNode)-1);
                            cl{1} = iFace0;
                            for i = 2:length(iNode)-1
                                lFace = sum(ismember(GRIDreduced.FaceNodeConnect,iNode(i:i+1)) & ~isnan(GRIDreduced.FaceNodeConnect),2);
                                iFace1 = find(lFace==2);
                                DistanceState = determine_frompoint(DistanceState,iFace1(1));
                                if length(iFace1)==2
                                    if DistanceState.distfromlast(iFace1(2))==0
                                        iFace1 = iFace1(1);
                                    else
                                        iFace1 = iFace1(2);
                                    end
                                end
                                ip = [];
                                iPrev = iFace1;
                                while iPrev~=iFace0
                                    ip = [iPrev ip];
                                    iPrev = DistanceState.frompoint(iPrev);
                                end
                                cl{i} = ip;
                                %
                                iFace0 = iFace1;
                                DS0.ilast = iFace0;
                                DistanceState = determine_frompoint(DS0,iFace0);
                            end
                            NewRange.Type = 'pwline';
                            NewRange.Range = iFace(cat(2,cl{:}));
                        case 'EDGE'
                            % convert EDGE to NODE
                            NodeRange = switchrange(GRID,OldLoc,OldRange,'NODE');
                            % and subsequently NODE to FACE
                            [NewRange,RangeMax] = switchrange(GRID,'NODE',NodeRange,NewLoc);
                    end
                else
                    switch OldRange.Type
                        case 'point'
                            iLocation = OldRange.Range(1);
                        case 'range'
                            iLocation = OldRange.Range{1};
                    end
                    find_point = length(iLocation)==1;
                    if find_point
                        if strcmp(OldLoc,'NODE')
                            MatchLevel = 1;
                        else
                            MatchLevel = 2;
                        end
                    else
                        MatchLevel = 3;
                    end
                    if strcmp(OldLoc,'EDGE')
                        % find node numbers associated with the selected edges
                        iEdge = iLocation;
                        iNode = GRID.EdgeNodeConnect(iEdge,:);
                        iNode = unique(iNode(:));
                    else
                        iNode = iLocation;
                    end
                    % find faces for which all nodes are selected
                    switch MatchLevel
                        case 1 % select first face matching one node
                            lFace = any(ismember(GRID.FaceNodeConnect,iNode) & ~isnan(GRID.FaceNodeConnect),2);
                            iFace = find(lFace);
                            iFace = iFace(1);
                        case 2 % select first face matching two nodes (one edge)
                            lFace = sum(ismember(GRID.FaceNodeConnect,iNode) & ~isnan(GRID.FaceNodeConnect),2);
                            iFace = find(lFace==2);
                            iFace = iFace(1);
                        case 3 % select all faces for which all nodes match
                            lFace = all(ismember(GRID.FaceNodeConnect,iNode) | isnan(GRID.FaceNodeConnect),2) ...
                                & ~all(isnan(GRID.FaceNodeConnect),2);
                            iFace = find(lFace);
                    end
                    NewRange.Type = 'range';
                    NewRange.Range = {iFace};
                end
            otherwise
                error('Transition from %s to %s not yet implemented',OldLoc,NewLoc)
        end
    otherwise
        error('Transition from %s to %s not yet implemented',OldLoc,NewLoc)
end
if strcmp(NewRange.Type,'pwline') && size(NewRange.Range,1)==1
    NewRange.Range = NewRange.Range(:);
end


function DistanceState = init_distance(GRID,style,ilast)
dist = [];
switch GRID.Type
    case 'ugrid'
        switch GRID.ValLocation
            case 'NODE'
                SEG = GRID.EdgeNodeConnect;
                %
                X = GRID.X;
                Y = GRID.Y;
            case 'EDGE'
                Edges = GRID.EdgeNodeConnect;
                X = GRID.X(Edges);
                Y = GRID.Y(Edges);
                D = sqrt((X(:,1)-X(:,2)).^2 + (Y(:,1)-Y(:,2)).^2);
                X = sum(X,2)/2;
                Y = sum(Y,2)/2;
                % determine neighboring edges
                nedges = size(Edges,1);
                nnodes = size(GRID.X,1);
                NEdgesPerNode = accumarray(Edges(:),1,[nnodes 1]);
                NConnPerNode = NEdgesPerNode.*(NEdgesPerNode-1)/2;
                SEG = zeros(sum(NConnPerNode),2);
                NodeEdge = [Edges(:) (mod(0:2*nedges-1,nedges)+1)'];
                [sNodeEdge,I] = sort(NodeEdge(:,1));
                NodeEdge = NodeEdge(I,:);
                EdgeOffset = [0;cumsum(NEdgesPerNode(1:end-1))];
                j = 0;
                for i = 1:nnodes
                    for e1 = 1:NEdgesPerNode(i)
                        for e2 = e1+1:NEdgesPerNode(i)
                            j = j+1;
                            SEG(j,:) = NodeEdge(EdgeOffset(i)+[e1 e2],2);
                        end
                    end
                end
                % determine distance of edge centres
                dist = sum(D(SEG),2)/2;
            case 'FACE'
                % determine Edge-Face relationship (code copy)
                Faces = GRID.FaceNodeConnect;
                missing = isnan(Faces);
                NEdges = sum(~missing(:));
                Edges = zeros(3,NEdges);
                ie = 0;
                for j = 1:size(Faces,2)
                    for i = 1:size(Faces,1)
                        if ~isnan(Faces(i,j))
                            ie = ie+1;
                            Edges(1,ie) = Faces(i,j);
                            if j==size(Faces,2) || isnan(Faces(i,j+1))
                                Edges(2,ie) = Faces(i,1);
                            else
                                Edges(2,ie) = Faces(i,j+1);
                            end
                            Edges(3,ie) = i;
                        end
                    end
                end
                Edges(1:2,:) = sort(Edges(1:2,:));
                Edges = Edges';
                %
                [c,ia,ic]=unique(Edges(:,1:2),'rows');
                SEG = zeros(size(c,1),2);
                for i = 1:length(ic)
                    if SEG(ic(i),1)==0
                        j = 1;
                    else
                        j = 2;
                    end
                    SEG(ic(i),j) = Edges(i,3);
                end
                SEG(any(SEG==0,2),:) = [];
                %
                Faces(missing) = 1;
                X = GRID.X(Faces);
                Y = GRID.Y(Faces);
                np = sum(~missing,2);
                X(missing) = 0;
                X = sum(X,2)./np;
                Y(missing) = 0;
                Y = sum(Y,2)./np;
        end
        %
        Npnt = size(X,1);
        %
        XY = cat(3,X(SEG),Y(SEG));
        xx = X(ilast);
        yy = Y(ilast);
        %
        [dd,I]=sort((sum(XY(:,1,:),3)/2-xx).^2+(sum(XY(:,2,:),3)/2-yy).^2);
    case 'sgrid'
        switch GRID.ValLocation
            case 'NODE'
                I = reshape(1:numel(GRID.X),size(GRID.X));
                SEGx = [I(1:end-1,:) I(2:end,:)];
                SEGx = reshape(SEGx,[numel(SEGx)/2 2]);
                SEGy = [I(:,1:end-1) I(:,2:end)];
                SEGy = reshape(SEGy,[numel(SEGy)/2 2]);
                Missing = isnan(GRID.X(1:end-1,1:end-1)) | ...
                    isnan(GRID.X(2:end,1:end-1)) | ...
                    isnan(GRID.X(1:end-1,2:end)) | ...
                    isnan(GRID.X(2:end,2:end));
                SEGd1= [I(1:end-1,1:end-1) I(2:end,2:end)];
                SEGd1= reshape(SEGd1,[numel(SEGd1)/2 2]);
                SEGd1(Missing,:)=[];
                SEGd2= [I(2:end,1:end-1) I(1:end-1,2:end)];
                SEGd2= reshape(SEGd2,[numel(SEGd2)/2 2]);
                SEGd2(Missing,:)=[];
                SEG = cat(1,SEGx,SEGy,SEGd1,SEGd2);
                %
                Nseg = size(SEG,1);
                XY = [GRID.X(:) GRID.Y(:)];
                Npnt = size(XY,1);
                XY = reshape(XY(SEG,:),[Nseg 2 2]);
                rm = any(isnan(XY(:,:,1)),2);
                SEG(rm,:)=[];
                XY(rm,:,:)=[];
                [M,N]=ind2sub(size(GRID.X),SEG);
                [m,n]=ind2sub(size(GRID.X),ilast);
                [dd,I]=sort((sum(M,2)/2-m).^2+(sum(N,2)/2-n).^2);
        end
end
SEG=SEG(I,:);
XY=XY(I,:,:);
if isempty(dist)
    switch style
        case 'least' % least number of segments
            dist = repmat(1,size(XY,1),1);
        case 'shortest' % shortest path
            dist = sqrt(sum(diff(XY,1,2).^2,3));
    end
else
    dist = dist(I);
end
%
DistanceState=[];
DistanceState.Npnt=Npnt;
DistanceState.ilast=ilast;
DistanceState.SEG=SEG;
DistanceState.dist=dist;
