function outdata = qp_plotmanager(cmd,UD,logfile,logtype,cmdargs)
%QP_PLOTMANAGER QuickPlot Plot Manager callback functions.

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

mfig = findobj(allchild(0),'flat','tag','Delft3D-QUICKPLOT');
if nargin<2 || isempty(UD)
    if ~isempty(mfig)
        UD = getappdata(mfig,'QPHandles');
    else
        UD = [];
    end
end
if ~isempty(UD)
    Inactive=UD.Inactive;
    Active=UD.Active;
end

if nargin<3
    logfile=0;
    logtype=0;
    cmdargs={};
elseif nargin<5
    cmdargs={};
end

T_=1; ST_=2; M_=3; N_=4; K_=5;
DimStr={'subfield ','timestep ','station ','M=','N=','K='};

switch cmd
    case 'resize'
        PM = UD.PlotMngr;
        %
        % Get new and old figure size (note: for this to work the figure size
        % must be initialized before the first call).
        %
        fig = PM.Fig;
        PrevSize = getappdata(fig,'FigureSize');
        MinSize = getappdata(fig,'MinimumFigureSize');
        if isempty(MinSize)
            MinSize = PrevSize;
            setappdata(fig,'MinimumFigureSize',MinSize)
        end
        NewPos = get(fig,'position');
        NewSize=NewPos(3:4);
        if any(NewSize<MinSize)
            NewSize=max(NewSize,MinSize);
            NewPos(2)=NewPos(2)+NewPos(4)-NewSize(2);
            NewPos(3:4)=NewSize;
            set(fig,'position',NewPos)
        end
        %
        fgprop_shown = isappdata(fig,'FigPropsShown'); % figprop shown before redraw
        axprop_shown = isappdata(fig,'AxPropsShown'); % axprop shown before redraw
        fgprop_hght = getappdata(fig,'FigPropsHeight');
        axprop_hght = getappdata(fig,'AxPropsHeight');
        List  = get(PM.ShowList,'string');
        iList = get(PM.ShowList,'value');
        sList = List{iList};
        List = {'Item(s)','Figure Properties','Axes Properties'};
        %
        fgprop_shw = [0 0 0 0];
        if NewSize(2)>MinSize(2)+fgprop_hght+axprop_hght % show figprop after redraw
            List(strcmp(List,'Figure Properties')) = [];
            if fgprop_shown
                fgprop_hght = 0;
            else
                setappdata(fig,'FigPropsShown',1);
                fgprop_shw(2) = getappdata(fig,'FigPropsShift')+axprop_hght;
            end
        else % don't show figprop after redraw
            if fgprop_shown
                fgprop_hght = -fgprop_hght;
                rmappdata(fig,'FigPropsShown');
                fgprop_shw(2) = -getappdata(fig,'FigPropsShift')-axprop_hght;
            else
                fgprop_hght = 0;
            end
        end
        %
        axprop_shw = [0 0 0 0];
        if NewSize(2)>MinSize(2)+axprop_hght % show axprop after redraw
            List(strcmp(List,'Axes Properties')) = [];
            if axprop_shown
                axprop_hght = 0;
            else
                setappdata(fig,'AxPropsShown',1);
                axprop_shw(2) = getappdata(fig,'AxPropsShift');
            end
        else % don't show axprop after redraw
            if axprop_shown
                axprop_hght = -axprop_hght;
                rmappdata(fig,'AxPropsShown');
                axprop_shw(2) = -getappdata(fig,'AxPropsShift');
            else
                axprop_hght = 0;
            end
        end
        %
        % Define some shift operators
        %
        aligntop   = [0 NewSize(2)-PrevSize(2) 0 0];
        alignright = [NewSize(1)-PrevSize(1) 0 0 0];
        stretchhor = [0 0 NewSize(1)-PrevSize(1) 0];
        fgprop_shf = [0 -fgprop_hght 0 0];
        axprop_shf = [0 -axprop_hght 0 0];
        stretchver = [0 0 0 NewSize(2)-PrevSize(2)];
        stretchitm = stretchver-[0 0 0 fgprop_hght+axprop_hght];
        stretch2   = stretchhor/2;
        shift2     = alignright/2;
        stretch5   = stretchhor/5;
        shift5     = alignright/5;
        %
        % Shift the buttons
        %
        shiftcontrol(PM.FigTxt,aligntop)
        shiftcontrol(PM.FigList,aligntop+stretchhor)
        shiftcontrol(PM.FigAll,aligntop+alignright)
        shiftcontrol(PM.AxTxt,aligntop+fgprop_shf)
        shiftcontrol(PM.AxList,aligntop+stretchhor+fgprop_shf)
        shiftcontrol(PM.AxAll,aligntop+alignright+fgprop_shf)
        shiftcontrol(PM.Show,aligntop+fgprop_shf+axprop_shf)
        shiftcontrol(PM.ShowList,aligntop+stretchhor+fgprop_shf+axprop_shf)
        iList = find(strcmp(List,sList));
        if isempty(iList)
            iList = 1;
            set(PM.ShowList,'string',List,'value',iList)
            qp_plotmanager pmshowselect
        else
            set(PM.ShowList,'string',List,'value',iList)
        end
        %
        shiftcontrol(PM.ItTxt,aligntop+fgprop_shf+axprop_shf)
        shiftcontrol(PM.ItList,stretchhor+stretchitm)
        shiftcontrol(PM.ItUp,aligntop+alignright+fgprop_shf+axprop_shf)
        shiftcontrol(PM.ItDown,alignright)
        %
        if fgprop_shw(2)>0
            set(PM.FigHandles,'visible','on')
        elseif fgprop_shw(2)<0
            set(PM.FigHandles,'visible','off')
        end
        shiftcontrol(PM.FigNameTxt,aligntop+axprop_shf+fgprop_shw)
        shiftcontrol(PM.FigName,aligntop+stretchhor+axprop_shf+fgprop_shw)
        shiftcontrol(PM.FigColorTxt,aligntop+alignright+axprop_shf+fgprop_shw)
        shiftcontrol(PM.FigColor,aligntop+alignright+axprop_shf+fgprop_shw)
        shiftcontrol(PM.FigPaperTypeTxt,aligntop+axprop_shf+fgprop_shw)
        shiftcontrol(PM.FigPaperType,aligntop+axprop_shf+fgprop_shw)
        shiftcontrol(PM.FigPaperOrientation,aligntop+axprop_shf+fgprop_shw)
        shiftcontrol(PM.FigPaperWidth,aligntop+stretch2+axprop_shf+fgprop_shw)
        shiftcontrol(PM.FigPaperX,aligntop+shift2+axprop_shf+fgprop_shw)
        shiftcontrol(PM.FigPaperHeight,aligntop+shift2+stretch2+axprop_shf+fgprop_shw)
        shiftcontrol(PM.FigPaperUnit,aligntop+alignright+axprop_shf+fgprop_shw)
        shiftcontrol(PM.FigBorderStyleTxt,aligntop+axprop_shf+fgprop_shw)
        shiftcontrol(PM.FigBorderStyle,aligntop+axprop_shf+fgprop_shw)
        shiftcontrol(PM.FigBorder,aligntop+axprop_shf+fgprop_shw)
        shiftcontrol(PM.FigRendererTxt,aligntop+stretch2+axprop_shf+fgprop_shw)
        shiftcontrol(PM.FigRendererType,aligntop+shift2+stretch2+axprop_shf+fgprop_shw)
        shiftcontrol(PM.FigSmoothing,aligntop+shift2+stretch2+axprop_shf+fgprop_shw)
        %
        if axprop_shw(2)>0
            set(PM.AxHandles,'visible','on')
        elseif axprop_shw(2)<0
            set(PM.AxHandles,'visible','off')
        end
        shiftcontrol(PM.AxNameTxt,aligntop+fgprop_shf+axprop_shw)
        shiftcontrol(PM.AxName,aligntop+3*stretch5+fgprop_shf+axprop_shw)
        shiftcontrol(PM.AxTypeTxt,aligntop+3*shift5+fgprop_shf+axprop_shw)
        shiftcontrol(PM.AxType,aligntop+3*shift5+2*stretch5+fgprop_shf+axprop_shw)
        shiftcontrol(PM.SecondY,aligntop+alignright+fgprop_shf+axprop_shw)
        %
        shiftcontrol(PM.AxTitleTxt,aligntop+fgprop_shf+axprop_shw)
        shiftcontrol(PM.AxTitleAuto,aligntop+fgprop_shf+axprop_shw)
        shiftcontrol(PM.AxTitle,aligntop+5*stretch5+fgprop_shf+axprop_shw)
        %
        shiftcontrol(PM.AxColorTxt,aligntop+fgprop_shf+axprop_shw)
        shiftcontrol(PM.HasAxColor,aligntop+fgprop_shf+axprop_shw)
        shiftcontrol(PM.AxColor,aligntop+stretch5+fgprop_shf+axprop_shw)
        shiftcontrol(PM.AxLineWTxt,aligntop+shift5+stretch5+fgprop_shf+axprop_shw)
        shiftcontrol(PM.AxLineWidth,aligntop+2*shift5+stretch5+fgprop_shf+axprop_shw)
        shiftcontrol(PM.AxBox,aligntop+3*shift5+stretch5+fgprop_shf+axprop_shw)
        %
        shiftcontrol(PM.AxPosition,aligntop+fgprop_shf+axprop_shw)
        shiftcontrol(PM.AxXLowerLeft,aligntop+stretch5+fgprop_shf+axprop_shw)
        shiftcontrol(PM.AxYLowerLeft,aligntop+shift5+stretch5+fgprop_shf+axprop_shw)
        shiftcontrol(PM.AxWidth,aligntop+2*shift5+stretch5+fgprop_shf+axprop_shw)
        shiftcontrol(PM.AxHeight,aligntop+3*shift5+stretch5+fgprop_shf+axprop_shw)
        shiftcontrol(PM.AxPosUnit,aligntop+4*shift5+stretch5+fgprop_shf+axprop_shw)
        %
        for X = 'XYZ'
            shiftcontrol(PM.(X).Axis,aligntop+fgprop_shf+axprop_shw)
            shiftcontrol(PM.(X).AxisName,aligntop+stretch5+fgprop_shf+axprop_shw)
            shiftcontrol(PM.(X).Grid,aligntop+1*shift5+stretch5+fgprop_shf+axprop_shw)
            if X~='X'
                shiftcontrol(PM.(X).AspectTxt,aligntop+2*shift5+stretch5+fgprop_shf+axprop_shw)
                shiftcontrol(PM.(X).AspectAuto,aligntop+3*shift5+stretch5+fgprop_shf+axprop_shw)
                shiftcontrol(PM.(X).AspectValue,aligntop+4*shift5+stretch5+fgprop_shf+axprop_shw)
            end
            shiftcontrol(PM.(X).Color,aligntop+5*shift5+fgprop_shf+axprop_shw)
            %
            shiftcontrol(PM.(X).LabelTxt,aligntop+fgprop_shf+axprop_shw)
            shiftcontrol(PM.(X).LabelAuto,aligntop+stretch5+fgprop_shf+axprop_shw)
            shiftcontrol(PM.(X).Label,aligntop+shift5+4*stretch5+fgprop_shf+axprop_shw)
            %
            shiftcontrol(PM.(X).LimitTxt,aligntop+fgprop_shf+axprop_shw)
            shiftcontrol(PM.(X).LimitMin,aligntop+stretch5+fgprop_shf+axprop_shw)
            shiftcontrol(PM.(X).LimitMax,aligntop+shift5+stretch5+fgprop_shf+axprop_shw)
            shiftcontrol(PM.(X).Scale,aligntop+2*shift5+stretch5+fgprop_shf+axprop_shw)
            shiftcontrol(PM.(X).Dir,aligntop+3*shift5+stretch5+fgprop_shf+axprop_shw)
            shiftcontrol(PM.(X).Loc,aligntop+4*shift5+stretch5+fgprop_shf+axprop_shw)
        end
        %
        pos = get(PM.Separator,'position');
        pos(1) = pos(1)+NewSize(1)-PrevSize(1);
        pos(4) = NewSize(2);
        set(PM.Separator,'position',pos)
        %
        shiftcontrol(PM.Options.Slider,alignright+stretchver)
        for i = 1:length(PM.Options.Handles)
            shiftcontrol(PM.Options.Handles(i),alignright)
        end
        update_option_positions(UD,'plmn',NewSize(2)-30+1)
        %
        % Store the new figure size for usage during next resize command
        %
        setappdata(fig,'FigureSize',NewSize);
        
    case 'newfigure'
        [h,figops,createops]=qp_createfig(cmdargs{1:min(2,length(cmdargs))});
        if ~isempty(h)
            UDplot=get(h,'userdata');
            UDplot.ProgID='QuickPlot';
            set(h,'userdata',UDplot);
            if ~isempty(h)
                set(UD.PlotMngr.FigList,'value',1,'string',listnames(h,'showType','no','showHandle','no','showTag','no'),'userdata',h);
                qp_plotmanager refreshfigs
            end
            if logfile
                writelog(logfile,logtype,cmd,createops{:});
            end
            if length(cmdargs)>2
                d3d_qp('figureborder',cmdargs{3}{:});
            end
        end
        
    case 'newaxes'
        pos=get(gcbf,'position');
        set(UD.PlotMngr.NewAxMenu,'position',get(0,'pointerlocation')-pos(1:2),'visible','on')

    case {'newaxes_oneplot', 'newaxes_matrix', 'newaxes_specloc'}
        FigIDs=get(UD.PlotMngr.FigList,'userdata');
        if isempty(FigIDs)
            set(UD.PlotMngr.AxList,'string',{''},'userdata',[],'value',1, ...
                'enable','off','backgroundcolor',Inactive);
            set(UD.PlotMngr.DelAx,'enable','off');
            UD.PlotMngr.CurrentAxes=[];
            setappdata(mfig,'QPHandles',UD)
            qp_plotmanager refreshitems
            qp_plotmanager refreshaxprop
        else 
            FigVal=get(UD.PlotMngr.FigList,'value');
            Fig=FigIDs(FigVal);
            if ~ishandle(Fig)
                qp_plotmanager refreshfigs
            else
                [h,createops]=qp_createaxes(Fig,cmd(9:end),cmdargs{:});
                if ~isempty(h)
                    set(UD.PlotMngr.AxList,'value',1,'string',listnames(h),'userdata',h);
                    qp_plotmanager refreshaxes
                    qp_plotmanager refreshfigprop
                end
            end
            if logfile && ~isempty(h)
                writelog(logfile,logtype,cmd,createops{:});
            end
        end
                
    case 'openfigure'
        PAR.X=[];
        PAR = rmfield(PAR,'X');
        figuredir=qp_settings('figuredir');
        if ~isempty(cmdargs)
            [p,f,extension] = fileparts(cmdargs{1});
            f = [f,extension];
            if length(cmdargs)>1
                PAR = cmdargs{2};
            end
        else
            curdir = pwd;
            if exist(figuredir,'dir')
                cd(figuredir)
            end
            filter = {'*.fig;*.qpses' 'All Supported Files'
                '*.fig' 'MATLAB Figure Files'
                '*.qpses' 'QUICKPLOT Session Files'};
            try
                [f,p]=uigetfile(filter,'Open figure ...');
            catch
            end
            cd(curdir)
        end
        if ischar(f)
            figuredir=p;
            qp_settings('figuredir',figuredir)
            %
            pf = fullfile(p,f);
            [p,f,extension] = fileparts(pf);
            switch extension
                case '.qpses'
                    qp_session('rebuild',pf,PAR);
                case '.fig'
                    h=hgload(pf);
                    set(h,'menubar','none','closerequestfcn','d3d_qp closefigure')
                    qp_figurebars(h)
                    %set(cbar,'deletefcn','qp_colorbar delete')
                    hName = listnames(h,'showtype','no','showhandle','no','showtag','no');
                    set(UD.PlotMngr.FigList,'value',1,'string',hName,'userdata',h);
                    qp_plotmanager refreshfigs
                    if logfile
                        writelog(logfile,logtype,cmd,pf);
                    end
            end
        end
        
    case 'refreshfigs'
        if ~isempty(cmdargs)
            Fg=cmdargs{1};
            FgName = listnames(Fg,'showtype','no','showhandle','no','showtag','no');
            set(UD.PlotMngr.FigList,'value',1,'string',FgName,'userdata',Fg);
        end
        
        Figs=get_nondialogs;
        if isempty(Figs)
            set(UD.PlotMngr.FigList,'string',{''},'userdata',[],'value',1, ...
                'enable','off','backgroundcolor',Inactive);
            set(UD.PlotMngr.SavFig,'enable','off');
            set(UD.PlotMngr.ClsFig,'enable','off');
            set(UD.PlotMngr.FigAll,'enable','off');
            set(UD.PlotMngr.NewAx,'enable','off');
        else
            fignames=listnames(Figs,'showtype','no','showhandle','no','showtag','no');
            [fignames,Order]=sort(fignames);
            Figs=Figs(Order);
            FigNms=get(UD.PlotMngr.FigList,'string');
            FigHnd=get(UD.PlotMngr.FigList,'userdata');
            FigVal=get(UD.PlotMngr.FigList,'value');
            if FigVal<=length(FigHnd) && ismember(FigHnd(FigVal),Figs)
                i=find(Figs==FigHnd(FigVal));
            elseif FigVal<=length(FigNms)
                if iscell(FigNms)
                    FigNm=FigNms{FigVal};
                else
                    FigNm=deblank(FigNms(FigVal,:));
                end
                i=ustrcmpi(FigNm,fignames);
                if i<0
                    i=1;
                end
            else
                i=1;
            end
            set(UD.PlotMngr.FigAll,'enable','on');
            enable = 'on';
            backgroundcolor = Active;
            if get(UD.PlotMngr.FigAll,'value')
                enable = 'off';
                backgroundcolor = Inactive;
            end
            set(UD.PlotMngr.FigList,'string',fignames,'userdata',Figs,'value',i, ...
                'enable',enable,'backgroundcolor',backgroundcolor);
            set(UD.PlotMngr.SavFig,'enable','on');
            set(UD.PlotMngr.ClsFig,'enable',enable);
            set(UD.PlotMngr.NewAx,'enable','on');
        end
        qp_plotmanager refreshaxes
        qp_plotmanager refreshfigprop
        d3d_qp update_addtoplot
        
    case 'allfigures'
        if ~isempty(cmdargs)
            allFigs = cmdargs{1};
            set(UD.PlotMngr.FigAll,'value',allFigs)
        else
            allFigs = get(UD.PlotMngr.FigAll,'value');
        end
        if allFigs
            figlistenable='off';
            figlistcolour=Inactive;
        else
            figlistenable='on';
            figlistcolour=Active;
        end
        set(UD.PlotMngr.FigList, ...
            'enable',figlistenable,'backgroundcolor',figlistcolour);
        set(UD.PlotMngr.ClsFig,'enable',figlistenable);
        qp_plotmanager refreshaxes
        qp_plotmanager refreshfigprop
        
    case 'refreshaxes'
        FigIDs=get(UD.PlotMngr.FigList,'userdata');
        if isempty(FigIDs)
            set(UD.PlotMngr.AxList,'string',{''},'userdata',[],'value',1, ...
                'enable','off','backgroundcolor',Inactive);
            set(UD.PlotMngr.DelAx,'enable','off');
            set(UD.PlotMngr.AxAll,'enable','off');
            UD.PlotMngr.CurrentAxes=[];
            setappdata(mfig,'QPHandles',UD)
        else
            FigVal=get(UD.PlotMngr.FigList,'value');
            allfigs=get(UD.PlotMngr.FigAll,'value');
            if allfigs
                Fig=FigIDs;
            else
                Fig=FigIDs(FigVal);
            end
            if any(~ishandle(Fig))
                qp_plotmanager refreshfigs
            else
                Axs=findall(Fig,'type','axes');
                for i=length(Axs):-1:1
                    if strcmp(get(Axs(i),'tag'),'scribeOverlay')
                        Axs(i)=[];
                    elseif isappdata(Axs(i),'NonDataObject')
                        Axs(i)=[];
                    end
                end
                if isempty(Axs)
                    set(UD.PlotMngr.AxList,'string',{''},'userdata',[],'value',1, ...
                        'enable','off','backgroundcolor',Inactive);
                    set(UD.PlotMngr.DelAx,'enable','off');
                    UD.PlotMngr.CurrentAxes=[];
                    setappdata(mfig,'QPHandles',UD)
                else
                    axnames=listnames(Axs);
                    AxVal=get(UD.PlotMngr.AxList,'value');
                    
                    AxH=get(UD.PlotMngr.AxList,'userdata');
                    if AxVal>length(AxH)
                        i=1;
                    else
                        i=find(AxH(AxVal)==Axs);
                        if isempty(i)
                            i=1;
                        end
                    end
                    axallenabled='on';
                    if allfigs
                        axallenabled='off';
                    end
                    set(UD.PlotMngr.AxAll,'enable',axallenabled);
                    axlistenable='on';
                    axlistcolour=Active;
                    if get(UD.PlotMngr.AxAll,'value') || allfigs
                        axlistenable='off';
                        axlistcolour=Inactive;
                    end
                    set(UD.PlotMngr.AxList,'string',axnames,'userdata',Axs,'value',i, ...
                        'enable',axlistenable,'backgroundcolor',axlistcolour);
                    set(UD.PlotMngr.DelAx,'enable',axlistenable);
                    UD.PlotMngr.CurrentAxes=Axs(i);
                    setappdata(mfig,'QPHandles',UD)
                end
            end
        end
        qp_plotmanager refreshitems
        qp_plotmanager refreshaxprop
        qp_plotmanager update_addtoplot
        
    case 'allaxes'
        if ~isempty(cmdargs)
            allAxes = cmdargs{1};
            set(UD.PlotMngr.AxAll,'value',allAxes)
        else
            allAxes = get(UD.PlotMngr.AxAll,'value');
        end
        if allAxes
            axlistenable='off';
            axlistcolour=Inactive;
        else
            axlistenable='on';
            axlistcolour=Active;
        end
        set(UD.PlotMngr.AxList, ...
            'enable',axlistenable,'backgroundcolor',axlistcolour);
        set(UD.PlotMngr.DelAx,'enable',axlistenable);
        qp_plotmanager refreshitems
        qp_plotmanager refreshaxprop
        qp_plotmanager update_addtoplot
        
    case 'pmshowselect'
        ipane = get(UD.PlotMngr.ShowList,'value');
        spane = get(UD.PlotMngr.ShowList,'string');
        h = UD.PlotMngr.Handles;
        for i = 1:length(spane)
            idx = strcmp(UD.PlotMngr.Pane,spane{i});
            if i==ipane
                set(h(idx),'visible','on')
            else
                set(h(idx),'visible','off')
            end
        end
        
    case 'refreshitems'
        AxIDs=get(UD.PlotMngr.AxList,'userdata');
        if isempty(AxIDs)
            set(UD.PlotMngr.ItList,'string',{''},'userdata',[],'value',1, ...
                'enable','off','backgroundcolor',Inactive);
            set(UD.PlotMngr.DelIt,'enable','off');
            set(UD.PlotMngr.ItInfo,'enable','off');
            set(UD.PlotMngr.ItLink,'enable','off');
        else
            Ax = getAx(UD);
            if any(~ishandle(Ax))
                qp_plotmanager refreshaxes
                qp_plotmanager refreshfigprop
            else
                [Items, UserDatas, Tags] = qp_get_items(Ax);
                %---
                separator='------';
                if isempty(Items)
                    set(UD.PlotMngr.ItList,'string',{''},'userdata',[],'value',1, ...
                        'enable','off','backgroundcolor',Inactive);
                    set(UD.PlotMngr.DelIt,'enable','off');
                    set(UD.PlotMngr.ItInfo,'enable','off');
                    set(UD.PlotMngr.ItLink,'enable','off');
                else
                    prevseparator=0;
                    it=length(Items);
                    if it>0
                        prevseparator=1;
                        Nms = cell(1,it);
                    end
                    while it>=1
                        %
                        % Backward compatible with cell version of PlotState ...
                        %
                        if isequal(UserDatas{it},'---')
                            Nms{it}=separator;
                            if prevseparator
                                Nms(it)=[];
                                UserDatas(it)=[];
                                Items(it)=[];
                                Tags(it)=[];
                            end
                            prevseparator=1;
                        elseif iscell(UserDatas{it}.PlotState)
                            Nms{it}=UserDatas{it}.PlotState{2}.Name;
                            prevseparator=0;
                        elseif isfield(UserDatas{it}.PlotState,'Props')
                            Nms{it}=UserDatas{it}.PlotState.Props.Name;
                            prevseparator=0;
                        elseif isfield(UserDatas{it}.PlotState,'Name')
                            Nms{it}=UserDatas{it}.PlotState.Name;
                            prevseparator=0;
                        else
                            Nms{it}='<unknown item>';
                            prevseparator=0;
                        end
                        it=it-1;
                    end
                    if prevseparator
                        Nms(1)=[];
                        UserDatas(1)=[];
                        Items(1)=[];
                        Tags(1)=[];
                    end
                    for it=1:length(Items)
                        it_same_name=find(strcmp(Nms{it},Nms));
                        extend=1;
                        out_of_options=0;
                        while length(it_same_name)>1 && ~strcmp(Nms{it},separator)
                            extrastr = repmat({''},1,length(Items));
                            cancut = 0;
                            for itloc=it_same_name
                                switch extend
                                    case 1
                                        cancut = 1;
                                        if isfield(UserDatas{itloc}.PlotState.FI,'Name')
                                            extrastr{itloc}=UserDatas{itloc}.PlotState.FI.Name;
                                        end
                                    case 2
                                        if isfield(UserDatas{itloc}.PlotState,'Selected')
                                            stat=UserDatas{itloc}.PlotState.Selected{ST_};
                                        else
                                            stat=[];
                                        end
                                        if ~isempty(stat)
                                            %stats=qpread(UserDatas{itloc}.PlotState.FI,UserDatas{itloc}.PlotState.Props,'stations');
                                            stats=UserDatas{itloc}.PlotState.Stations;
                                            if length(stat)>1
                                                extrastr{itloc}=['ST=' vec2str(stat,'nobrackets')];
                                            elseif stat==0
                                                extrastr{itloc}='all';
                                            elseif iscell(stats)
                                                extrastr{itloc}=stats{stat};
                                            else
                                                extrastr{itloc}=deblank(stats(stat,:));
                                            end
                                        end
                                    case 3
                                        if isfield(UserDatas{itloc}.PlotState,'SubField') && ~isempty(UserDatas{itloc}.PlotState.SubField)
                                            subflds=qpread(UserDatas{itloc}.PlotState.FI,UserDatas{itloc}.PlotState.Props,'subfields');
                                            extrastr{itloc}=subflds{UserDatas{itloc}.PlotState.SubField{1}};
                                        else
                                            extrastr{itloc}='';
                                        end
                                    case 4
                                        if isfield(UserDatas{itloc}.PlotState,'Selected')
                                            m = UserDatas{itloc}.PlotState.Selected{M_};
                                        else
                                            m = [];
                                        end
                                        if iscell(m)
                                            extrastr{itloc}=[m{1} ' line'];
                                        elseif isequal(m,0)
                                            extrastr{itloc}='All M';
                                        elseif ~isempty(m)
                                            extrastr{itloc}=['M=' vec2str(m,'nobrackets')];
                                        end
                                    case 5
                                        if isfield(UserDatas{itloc}.PlotState,'Selected')
                                            n = UserDatas{itloc}.PlotState.Selected{N_};
                                        else
                                            n = [];
                                        end
                                        if isequal(n,0)
                                            extrastr{itloc}='All N';
                                        elseif ~isempty(n)
                                            extrastr{itloc}=['N=' vec2str(n,'nobrackets')];
                                        end
                                    case 6
                                        if isfield(UserDatas{itloc}.PlotState,'Selected')
                                            k = UserDatas{itloc}.PlotState.Selected{K_};
                                        else
                                            k = [];
                                        end
                                        if isequal(k,0)
                                            extrastr{itloc}='All K';
                                        elseif iscell(k)
                                            switch k{1}
                                                case 'z'
                                                    extrastr{itloc} = sprintf('at Z=%g',k{2});
                                                case 'dz_below_max'
                                                    extrastr{itloc} = sprintf('at %g below surface',k{2});
                                                case 'dz_above_min'
                                                    extrastr{itloc} = sprintf('at %g above bed',k{2});
                                                case 'depth_frac'
                                                    extrastr{itloc} = sprintf('at %g%% of depth',k{2}*100);
                                            end
                                        elseif ~isempty(k)
                                            extrastr{itloc}=['K=' vec2str(k,'nobrackets')];
                                        end
                                    case 7
                                        if isfield(UserDatas{itloc}.PlotState,'Ops') && isfield(UserDatas{itloc}.PlotState.Ops,'presentationtype')
                                            extrastr{itloc}=UserDatas{itloc}.PlotState.Ops.presentationtype;
                                        end
                                    case 8
                                        if isfield(UserDatas{itloc}.PlotState,'Selected')
                                            t = UserDatas{itloc}.PlotState.Selected{T_};
                                        else
                                            t = [];
                                        end
                                        if isequal(t,0)
                                            extrastr{itloc}='All TS';
                                        elseif ~isempty(t)
                                            extrastr{itloc}=['TS=' vec2str(t,'nobrackets')];
                                        end
                                    otherwise
                                        out_of_options = 1;
                                        break
                                end
                            end
                            if out_of_options
                                break
                            end
                            it_extra_same=find(strcmp(extrastr{it},extrastr(it_same_name)));
                            if length(it_extra_same)<length(it_same_name)
                                minstrlen = min(cellfun('length',extrastr(it_same_name)));
                                %
                                if cancut
                                    %
                                    % look for first character different
                                    %
                                    c1diff = false;
                                    for i1 = 1:minstrlen
                                        c1 = extrastr{it_same_name(1)}(i1);
                                        for itloc=it_same_name
                                            if extrastr{itloc}(i1)~=c1
                                                c1diff = true;
                                                break
                                            end
                                        end
                                        if c1diff
                                            break
                                        end
                                    end
                                    %
                                    % look for last character different
                                    %
                                    c2diff = false;
                                    for i2 = 0:minstrlen-1
                                        c2 = extrastr{it_same_name(1)}(end-i2);
                                        for itloc=it_same_name
                                            if extrastr{itloc}(end-i2)~=c2
                                                c2diff = true;
                                                break
                                            end
                                        end
                                        if c2diff
                                            break
                                        end
                                    end
                                else
                                    i1 = 1;
                                    i2 = 0;
                                end
                                %
                                for itloc=it_same_name
                                    Nms{itloc}=cat(2,Nms{itloc},' - ',extrastr{itloc}(i1:end-i2));
                                end
                                it_same_name=it_same_name(it_extra_same);
                            end
                            extend=extend+1;
                        end
                    end
                    %
                    % Select the item with the same tag.
                    %
                    val = [];
                    valOld = get(UD.PlotMngr.ItList,'value');
                    InfoOld = get(UD.PlotMngr.ItList,'userdata');
                    if iscell(InfoOld)
                        TagOld = InfoOld{1};
                        for i = length(valOld):-1:1
                            val(i) = ustrcmpi(TagOld{valOld(i)},Tags);
                        end
                        val(val<0) = [];
                    end
                    %
                    % If no item with same name was found, then select the
                    % first item that's not a separator.
                    %
                    if isempty(val)
                        val=1;
                        while val<length(Nms) && strcmp(Nms{val},separator)
                            val=val+1;
                        end
                    end
                    %
                    % if there are only separators, select none
                    %
                    if all(strcmp(Nms(val),separator))
                        val=[];
                    end
                    set(UD.PlotMngr.ItList,'string',Nms, ...
                        'userdata',{Tags Items},'value',val, ...
                        'enable','on','backgroundcolor',Active);
                    %
                    % buttons should not be enabled if a separator is selected
                    %
                    enable='on';
                    if isempty(val)
                        enable='off';
                    end
                    set(UD.PlotMngr.DelIt,'enable',enable);
                    set(UD.PlotMngr.ItInfo,'enable',enable);
                    set(UD.PlotMngr.ItLink,'enable',enable);
                end
            end
        end
        qp_plotmanager updatearrows
        qp_plotmanager refreshitemprop
        d3d_qp update_addtoplot

    case 'updatearrows'
        Ax = getAx(UD);
        if strcmp(get(UD.PlotMngr.ItList,'enable'),'off') || length(Ax)>1
            set(UD.PlotMngr.ItUp,'enable','inactive', ...
                'cdata',getappdata(UD.PlotMngr.ItUp,'ArrowInactive'));
            set(UD.PlotMngr.ItDown,'enable','inactive', ...
                'cdata',getappdata(UD.PlotMngr.ItDown,'ArrowInactive'));
        else
            it  = get(UD.PlotMngr.ItList,'value');
            its = get(UD.PlotMngr.ItList,'string');
            if length(it)~=1 || it==1
                set(UD.PlotMngr.ItUp,'enable','inactive', ...
                    'cdata',getappdata(UD.PlotMngr.ItUp,'ArrowInactive'));
            else
                set(UD.PlotMngr.ItUp,'enable','on', ...
                    'cdata',getappdata(UD.PlotMngr.ItUp,'ArrowActive'));
            end
            if length(it)~=1 || it==length(its)
                set(UD.PlotMngr.ItDown,'enable','inactive', ...
                    'cdata',getappdata(UD.PlotMngr.ItDown,'ArrowInactive'));
            else
                set(UD.PlotMngr.ItDown,'enable','on', ...
                    'cdata',getappdata(UD.PlotMngr.ItDown,'ArrowActive'));
            end
        end

    case {'moveitemup','moveitemdown','moveitemtoback'}
        Ax = getAx(UD);
        if any(~ishandle(Ax))
            qp_plotmanager refreshaxes
            qp_plotmanager refreshfigprop
        else
            pfig = get(Ax,'parent');
            ItInfo = get(UD.PlotMngr.ItList,'userdata');
            ItVal = get(UD.PlotMngr.ItList,'value');
            ItTags = ItInfo{1};
            ItHand = ItInfo{2};
            %
            ItTag1 = ItTags{ItVal};
            hIt1 = findall(pfig,'tag',ItTag1); % the object itself
            if length(hIt1)>1
                hItem1 = hIt1(~cellfun('isempty',get(hIt1,'userdata')));
            else
                hItem1 = hIt1;
            end
            ZCurrent1 = getappdata(hItem1,'Level');
            %
            while 1
                switch cmd
                    case 'moveitemup'
                        ItVal2 = ItVal-1;
                    case {'moveitemdown','moveitemtoback'}
                        ItVal2 = ItVal+1;
                end
                if ItVal2>length(ItTags) || ItVal2<1
                    break
                end
                %
                ItTag2 = ItTags{ItVal2};
                hIt2 = findall(pfig,'tag',ItTag2); % the object itself
                if length(hIt2)>1
                    hItem2 = hIt2(~cellfun('isempty',get(hIt2,'userdata')));
                else
                    hItem2 = hIt2;
                end
                ZCurrent2 = getappdata(hItem2,'Level');
                %
                setzcoord(hIt1,ZCurrent2)
                setappdata(hItem1,'Level',ZCurrent2)
                setzcoord(hIt2,ZCurrent1)
                setappdata(hItem2,'Level',ZCurrent1)
                %
                children = allchild(Ax);
                child1 = find(ismember(children,hIt1));
                child2 = find(ismember(children,hIt2));
                BeforeBoth = children(1:min([min(child1) min(child2)])-1);
                AfterBoth = children(max([max(child1) max(child2)])+1:end);
                switch cmd
                    case 'moveitemup'
                        children = [BeforeBoth; children(child1); ...
                            children(child2); AfterBoth];
                    case {'moveitemdown','moveitemtoback'}
                        children = [BeforeBoth; children(child2); ...
                            children(child1); AfterBoth];
                end
                set(Ax,'children',children)
                %
                if ~strcmp(cmd,'moveitemtoback')
                    break
                end
                %
                ItTags([ItVal ItVal2]) = ItTags([ItVal2 ItVal]);
                ItVal = ItVal2;
            end
            %
            qp_plotmanager refreshitems
        end

    case 'itemlist'
        Ax = getAx(UD);
        if any(~ishandle(Ax))
            qp_plotmanager refreshaxes
            qp_plotmanager refreshfigprop
        else
            ItInfo=get(UD.PlotMngr.ItList,'userdata');
            ItVal=get(UD.PlotMngr.ItList,'value');
            ItTags=ItInfo{1};
            ItIDs=ItInfo{2};
            OK=1;
            ItVal(ItIDs(ItVal)==0)=[];
            for itVal=ItVal
                ItTag=ItTags{itVal};
                hIt=findall(Ax,'tag',ItTag);
                if isempty(hIt)
                    qp_plotmanager refreshitems
                    qp_plotmanager refreshaxprop
                    OK=0;
                    break
                end
            end
            if OK
                set(UD.PlotMngr.ItList,'value',ItVal);
                if length(ItVal)==1
                    set(UD.PlotMngr.DelIt,'enable','on');
                    set(UD.PlotMngr.ItLink,'enable','on');
                    set(UD.PlotMngr.ItInfo,'enable','on');
                else
                    if isempty(ItVal)
                        set(UD.PlotMngr.DelIt,'enable','off');
                        set(UD.PlotMngr.ItLink,'enable','off');
                    end
                    set(UD.PlotMngr.ItInfo,'enable','off');
                end
            end
        end
        qp_plotmanager updatearrows
        qp_plotmanager refreshitemprop
        
    case 'iteminfo'
        Ax = getAx(UD);
        if any(~ishandle(Ax))
            qp_plotmanager refreshaxes
            qp_plotmanager refreshfigprop
        else
            pfig=get(Ax,'parent');
            ItIDs=get(UD.PlotMngr.ItList,'userdata');
            ItVal=get(UD.PlotMngr.ItList,'value');
            %ItTags=get(UD.PlotMngr.ItList,'string');
            ItTags=ItIDs{1};
            ItTag=ItTags{ItVal};
            
            hIt=findall(pfig,'tag',ItTag);
            UserDatas=get(hIt,'userdata');
            if iscell(UserDatas)
                UserDatas=UserDatas(~cellfun('isempty',UserDatas));
                UserDatas=UserDatas{1};
            end
            if isstruct(UserDatas)
                if isfield(UserDatas,'XInfo') && ~isempty(UserDatas.XInfo)
                    locStruct.DataInfo=UserDatas.XInfo;
                end
                %
                % Backward compatible with cell version of PlotState ...
                %
                if iscell(UserDatas.PlotState)
                    nm=UserDatas.PlotState{2}.Name;
                    locStruct.PlotInfo=UserDatas.PlotState{end};
                else
                    nm=UserDatas.PlotState.Props.Name;
                    locStruct.PlotInfo=UserDatas.PlotState.Ops;
                end
                ui_inspectstruct(locStruct,nm);
            end
            
        end
        
    case 'deleteaxes'
        Ax = getAx(UD);
        if length(Ax)==1 && ishandle(Ax)
            pfig=get(Ax,'parent');
            Items=allchild(Ax);
            Tags=cget(Items,'tag');
            UserDatas=cget(Items,'userdata');
            TUDvalid=~cellfun('isempty',Tags) & ~cellfun('isempty',UserDatas);
            Tags=Tags(TUDvalid);
            QPTag=strncmp('QPPlotTag',Tags,9);
            Tags=Tags(QPTag);
            Tags=unique(Tags);
            for i=1:length(Tags)
                ItTag=Tags{i};
                hAnIt=findall(pfig,'userdata',ItTag);
                if ~isempty(hAnIt)
                    delete(hAnIt)
                end
            end
            delete(getappdata(Ax,'linkedaxes'))
            delete(Ax)
            if logfile
                writelog(logfile,logtype,cmd);
            end
        end
        qp_plotmanager refreshaxes
        qp_plotmanager refreshfigprop
        
    case 'deleteitems'
        Ax = getAx(UD);
        hItList = UD.PlotMngr.ItList;
        if any(~ishandle(Ax))
            qp_plotmanager refreshaxes
            qp_plotmanager refreshfigprop
        else
            pfig=get(Ax,'parent');
            if iscell(pfig)
                pfig=unique(cat(1,pfig{:}));
            end
            ItIDs=get(hItList,'userdata');
            ItVal=get(hItList,'value');
            ItTags=ItIDs{1};
            for itVal=ItVal
                ItTag=ItTags{itVal};
                hIt=findall(pfig,'tag',ItTag); % the object itself
                hAnIt=findall(pfig,'userdata',ItTag); % animation list items
                delete(hIt)
                if ~isempty(hAnIt)
                    uicm=get(hAnIt(1),'parent');
                    delete(hAnIt)
                    ItFig=get(uicm,'parent');
                    animsld=findobj(ItFig,'tag','animslid');
                    if isempty(get(uicm,'children'))
                        animpush=findobj(ItFig,'tag','animpush');
                        set(animpush,'enable','off')
                    end
                    SliderUD=get(animsld,'userdata');
                    iobj=1;
                    while iobj<=length(SliderUD)
                        if strcmp(SliderUD(iobj).Tag,ItTag)
                            SliderUD(iobj)=[];
                        end
                        iobj=iobj+1;
                    end
                    animsldEnab='on';
                    if isempty(SliderUD)
                        animsldEnab='off';
                    end
                    set(animsld,'enable',animsldEnab,'userdata',SliderUD)
                end
            end
            if length(ItVal)==1
                if ItVal<size(ItIDs{1},1)
                    set(hItList,'value',ItVal+1)
                elseif ItVal>1
                    set(hItList,'value',ItVal-1)
                end
            end
            setaxesprops(Ax)
            qp_plotmanager refreshitems
            qp_plotmanager refreshaxprop
        end
        
    case 'linkitems'
        Ax = getAx(UD);
        if any(~ishandle(Ax))
            qp_plotmanager refreshaxes
            qp_plotmanager refreshfigprop
        else
            pfig=get(Ax,'parent');
            if iscell(pfig)
                pfig=unique(cat(1,pfig{:}));
            end
            uicm=findall(pfig,'type','uicontextmenu');
            checked=findall(uicm,'checked','on');
            ItIDs=get(UD.PlotMngr.ItList,'userdata');
            ItVal=get(UD.PlotMngr.ItList,'value');
            ItTags=ItIDs{1};
            CanAnim={};
            for itVal=ItVal(1)
                ItTag=ItTags{itVal};
                %hIt=findall(Ax,'tag',ItTag); % the object itself
                hAnIt=findall(pfig,'userdata',ItTag); % animation list items
                hCanAnim=get(hAnIt,'children');
                for h=hCanAnim'
                    AnimInfo=get(h,'userdata');
                    nsteps=length(AnimInfo.Values);
                    CanAnim{end+1,1}=cat(2,get(h,'label'),sprintf(' (%i steps)',nsteps));
                end
            end
            for itVal=ItVal
                ItTag=ItTags{itVal};
                %hIt=findall(Ax,'tag',ItTag); % the object itself
                hAnIt=findall(pfig,'userdata',ItTag); % animation list items
                hCanAnim=get(hAnIt,'children');
                CanAnimL={};
                for h=hCanAnim'
                    AnimInfo=get(h,'userdata');
                    nsteps=length(AnimInfo.Values);
                    CanAnimL{end+1,1}=cat(2,get(h,'label'),sprintf(' (%i steps)',nsteps));
                end
                CanAnim=intersect(CanAnim,CanAnimL);
                
            end
            if isempty(CanAnim)
                if length(ItVal)>1
                    ui_message('error','Selected items do not have any animation field in common.')
                else
                    ui_message('error','This item has no animation field.')
                end
                return
            end
            if length(CanAnim)>1
                if ~isempty(cmdargs)
                    i = ustrcmpi(cmdargs{1},CanAnim);
                else
                    i = -1;
                end
                if i>0
                    AnimateThis=CanAnim{i};
                else
                    AnimateThis=ui_type(CanAnim);
                end
            else
                AnimateThis=CanAnim{1};
            end
            if isempty(AnimateThis)
                return
            end
            set(checked,'checked','off')
            AnimObj=[];
            for itVal=ItVal
                ItTag=ItTags{itVal};
                %hIt=findall(Ax,'tag',ItTag); % the object itself
                hAnIt=findall(pfig,'userdata',ItTag); % animation list items
                hCanAnim=get(hAnIt,'children');
                for h=hCanAnim'
                    AnimInfo=get(h,'userdata');
                    nsteps=length(AnimInfo.Values);
                    AniStr=cat(2,get(h,'label'),sprintf(' (%i steps)',nsteps));
                    if strcmp(AniStr,AnimateThis)
                        set(h,'checked','on')
                        AnimObj(end+1).Fld=AnimInfo.Dim;
                        NSteps=length(AnimInfo.Values);
                        AnimObj(end).Tag=ItTag;
                        AnimObj(end).Label=AnimInfo.Label;
                        AnimObj(end).Values=AnimInfo.Values;
                    end
                end
            end
            animsld=findobj(pfig,'tag','animslid');
            hIt=findall(pfig,'tag',ItTags{ItVal(1)}); % get the first object
            hItUD=get(hIt,'userdata');
            if iscell(hItUD) % object consisting of multiple HG objects
                mainIt=find(~cellfun('isempty',hItUD));
                hItUD=hItUD{mainIt(1)};
            end
            %
            % Backward compatible with cell version of PlotState ...
            %
            if iscell(hItUD.PlotState)
                hItUD.PlotState=plotstatestruct(hItUD.PlotState);
            end
            t_=AnimObj(1).Fld;
            if t_==0
                t=hItUD.PlotState.SubField{1};
            else
                t=hItUD.PlotState.Selected{t_};
            end
            %AnimMax=NSteps;
            %sstep=[min(1/(AnimMax-1),.1) min(10/(AnimMax-1),.9)];
            %set(animsld,'userdata',AnimObj,'value',1,'sliderstep',sstep,'Max',AnimMax,'enable','on','value',t)
            uislider(animsld,'min',1,'value',t,'max',NSteps)
            set(animsld,'userdata',AnimObj,'enable','on')
            Str=sprintf('%i',t);
            if t_==0
                [Chk,sflds] = qp_getdata(hItUD.PlotState.FI, ...
                    hItUD.PlotState.Domain, ...
                    hItUD.PlotState.Props,'subfields',t);
                if Chk
                    Str=sflds{1};
                end
            end
            set(animsld,'tooltip',[DimStr{t_+1},Str]);
            qck_anim('slider',pfig(1))
        end
        
    case 'selectedfigure'
        FigureHandles=get(UD.PlotMngr.FigList,'userdata');
        if get(UD.PlotMngr.FigAll,'value')
            iFg=1:length(FigureHandles);
        else
            iFg=get(UD.PlotMngr.FigList,'value');
        end
        if isempty(FigureHandles)
            outdata = [];
        else
            outdata = FigureHandles(iFg);
        end
        
    case 'selectfigure'
        if ~isempty(cmdargs)
            h = cmdargs{1};
            if ischar(h)
                %
                FigureHandles=get(UD.PlotMngr.FigList,'userdata');
                Names = get(FigureHandles,'name');
                iFg = ustrcmpi(h,Names);
                if iFg<0
                    %
                    % If name not found in list of figures, try once
                    % refreshing the list of figures.
                    %
                    qp_plotmanager refreshfigs
                    FigureHandles=get(UD.PlotMngr.FigList,'userdata');
                    Names = get(FigureHandles,'name');
                    [iFg,iAll] = ustrcmpi(h,Names);
                    if iFg<0 
                        if ~isempty(iAll) && length(cmdargs)>1
                            ii = cmdargs{2};
                            if ii>=1 && ii<=length(iAll)
                                iFg = iAll(ii);
                            else
                                error('Invalid sequence number %g during selection of figure "%s"',ii,h)
                            end
                        else
                            ui_message('warning','Multiple figures match specified name "%s"; selecting first match.',h)
                        end
                    end
                end
                %
                % If still not found, continue without selecting the requested
                % axes.
                %
                if iFg>0
                    set(UD.PlotMngr.FigList,'value',iFg)
                end
                %
            elseif any(ishandle(h))
                %
                h(~ishandle(h))=[];
                for i = length(h):-1:1
                    while ~isequal(get(h(i),'type'),'figure') && ~isequal(get(h(i),'type'),'root')
                        h(i) = get(h(i),'parent');
                    end
                    if isequal(get(h(i),'type'),'root')
                        h(i) = [];
                    end
                end
                h = unique(h);
                %
                FigureHandles=get(UD.PlotMngr.FigList,'userdata');
                iFg=find(ismember(FigureHandles,h));
                if length(iFg)<length(h)
                    %
                    % If a handle not found in list of figures, try
                    % once refreshing the list of figures.
                    %
                    qp_plotmanager refreshfigs
                    FigureHandles=get(UD.PlotMngr.FigList,'userdata');
                    iFg=find(ismember(FigureHandles,h));
                end
                %
                % If still not found, continue without selecting the requested
                % figure.
                %
                if ~isempty(iFg)
                    d3d_qp('allfigures',length(iFg)>1)
                    if length(iFg)==1
                        set(UD.PlotMngr.FigList,'value',iFg)
                    end
                end
            end
        else
            FigureHandles = get(UD.PlotMngr.FigList,'userdata');
            iFg = get(UD.PlotMngr.FigList,'value');
        end
        qp_plotmanager refreshaxes
        qp_plotmanager update_addtoplot
        qp_plotmanager refreshfigprop
        if logfile && iFg>0
            names = get(FigureHandles,'name');
            nr   = {};
            if iscell(names)
                name  = names{iFg};
                same = strcmp(name,names);
                if sum(same)>1
                    same(iFg+1:end) = 0;
                    nr = {sum(same)};
                end
            else
                name = names;
            end
            writelog(logfile,logtype,cmd,name,nr{:});
        end

    case 'selectedaxes'
        AxesHandles=get(UD.PlotMngr.AxList,'userdata');
        if any(~ishandle(AxesHandles))
            qp_plotmanager refreshaxes
            AxesHandles=get(UD.PlotMngr.AxList,'userdata');
        end
        if get(UD.PlotMngr.AxAll,'value') || get(UD.PlotMngr.FigAll,'value')
            iAx=1:length(AxesHandles);
        else
            iAx=get(UD.PlotMngr.AxList,'value');
        end
        if isempty(AxesHandles)
            outdata = [];
        else
            outdata = AxesHandles(iAx);
        end
        
    case 'selectaxes'
        if ~isempty(cmdargs)
            h = cmdargs{1};
            if ischar(h)
                %
                AxesHandles=get(UD.PlotMngr.AxList,'userdata');
                Tags = get(AxesHandles,'tag');
                iAx = ustrcmpi(h,Tags);
                if iAx<0
                    %
                    % If tag not found in list of axes, try once refreshing the
                    % list of axes.
                    %
                    qp_plotmanager refreshaxes
                    qp_plotmanager refreshfigprop
                    AxesHandles=get(UD.PlotMngr.AxList,'userdata');
                    Tags = get(AxesHandles,'tag');
                    [iAx,iAll] = ustrcmpi(h,Tags);
                    if iAx<0 
                        if ~isempty(iAll) && length(cmdargs)>1
                            ii = cmdargs{2};
                            if ii>=1 && ii<=length(iAll)
                                iAx = iAll(ii);
                            else
                                error('Invalid sequence number %g during selection of axes "%s"',ii,h)
                            end
                        else
                            ui_message('warning','Multiple axes match specified name "%s"; selecting first match.',h)
                        end
                    end
                end
                %
                % If still not found, continue without selecting the requested
                % axes.
                %
                if iAx>0
                    set(UD.PlotMngr.AxList,'value',iAx)
                end
                %
            elseif any(ishandle(h))
                %
                h(~ishandle(h))=[];
                for i = length(h):-1:1
                    while ~isequal(get(h(i),'type'),'axes') && ~isequal(get(h(i),'type'),'figure') && ~isequal(get(h(i),'type'),'root')
                        h(i) = get(h(i),'parent');
                    end
                    if isequal(get(h(i),'type'),'figure') || isequal(get(h(i),'type'),'root')
                        h(i) = [];
                    end
                end
                h = unique(h);
                %
                hFig = get(h,'parent');
                if iscell(hFig)
                    hFig = cat(1,hFig{:});
                end
                d3d_qp('selectfigure',hFig);
                %
                AxesHandles=get(UD.PlotMngr.AxList,'userdata');
                iAx=find(ismember(AxesHandles,h));
                if ~isempty(iAx)
                    d3d_qp('allaxes',length(iAx)>1)
                    if length(iAx)==1
                        set(UD.PlotMngr.AxList,'value',iAx)
                    end
                end
                %
            end
        else
            AxesHandles=get(UD.PlotMngr.AxList,'userdata');
            iAx = get(UD.PlotMngr.AxList,'value');
        end
        qp_plotmanager refreshaxes % this call triggers also refreshaxprop
        d3d_qp update_addtoplot
        %qp_plotmanager refreshaxprop
        if logfile && iAx>0
            tags = get(AxesHandles,'tag');
            nr   = {};
            if iscell(tags)
                tag  = tags{iAx};
                same = strcmp(tag,tags);
                if sum(same)>1
                    same(iAx+1:end) = 0;
                    nr = {sum(same)};
                end
            else
                tag = tags;
            end
            writelog(logfile,logtype,cmd,tag,nr{:});
        end
        
    case 'selecteditem'
        ItemNames=get(UD.PlotMngr.ItList,'string');
        ItemHandles=get(UD.PlotMngr.ItList,'userdata');
        iItem = get(UD.PlotMngr.ItList,'value');
        nItems = max(1,length(iItem));
        outdata(nItems).Name='dummy';
        outdata(nItems).Tag='dummy';
        if isempty(iItem)
            outdata(1)=[];
        else
            for i = nItems:-1:1
                outdata(i).Name = ItemNames{iItem(i)};
                outdata(i).Tag = ItemHandles{1}{iItem(i)};
            end
        end
        
    case 'selectitem'
        if ~isempty(cmdargs)
            h = cmdargs{1};
            if isstruct(h)
                if ~isfield(h,'Tag')
                    error('Invalid structure provided select item call: missing Tag field!')
                end
                Tags = {h(:).Tag};
            elseif isnumeric(h)
                Tags = get(h,'tag');
                if isempty(Tags)
                    Tags = {};
                elseif ischar(Tags)
                    Tags = {Tags};
                end
            end
            %
            allObjs = findall(0);
            allTags = get(allObjs,'tag');
            %
            allObjs = allObjs(ismember(allTags,Tags));
            allAxs = get(allObjs,'parent');
            if iscell(allAxs)
                allAxs = cat(1,allAxs{:});
            end
            %
            if ~isempty(allAxs)
                d3d_qp('selectaxes',allAxs)
                %
                ItemHandles = get(UD.PlotMngr.ItList,'userdata');
                iIt = find(ismember(ItemHandles{1},Tags));
                set(UD.PlotMngr.ItList,'value',iIt)
            end
        else
            get(gcbo,'tag')
        end
        qp_plotmanager updatearrows
        qp_plotmanager refreshitemprop
        d3d_qp update_addtoplot

    case 'refreshfigprop'
        fig = qpsf;
        PM = UD.PlotMngr;
        if length(fig)==1
            set([PM.FigNameTxt PM.FigColorTxt ...
                PM.FigPaperTypeTxt PM.FigBorderStyleTxt PM.FigPaperX],'enable','on')
            set([PM.FigName ...
                PM.FigPaperType PM.FigBorderStyle PM.FigPaperOrientation], ...
                'enable','on', ...
                'backgroundcolor',Active)
            %
            set(PM.FigName,'string',get(fig,'name'))
            set(PM.FigColor,'backgroundcolor',get(fig,'color'), ...
                'enable','on')
            %
            pt = get(fig,'PaperType');
            pts = get(PM.FigPaperType,'string');
            pti = find(strcmp(pt,pts));
            if isempty(pti)
                pti = length(pts);
                set(fig,'PaperType','<custom>')
            end
            set(PM.FigPaperType,'value',pti)
            po = get(fig,'PaperOrientation');
            set(PM.FigPaperOrientation,'value',find(strcmp(po,get(PM.FigPaperOrientation,'string'))))
            %
            pu = get(fig,'PaperUnit');
            pus = get(PM.FigPaperUnit,'string');
            pui = find(strcmp(pu,pus));
            if isempty(pui) % paperunit is normalized; this doesn't work for editing paper size
                pui = 1;
                set(fig,'PaperUnit',pus{pui});
            end
            %
            sz = get(fig,'PaperSize');
            if pti==length(pts)
                set([PM.FigPaperWidth PM.FigPaperHeight PM.FigPaperUnit], ...
                    'enable','on', ...
                    'backgroundcolor',Active)
            else
                set([PM.FigPaperWidth PM.FigPaperHeight PM.FigPaperUnit], ...
                    'enable','off', ...
                    'backgroundcolor',Inactive)
            end
            set(PM.FigPaperWidth,'string',sprintf('%.1f',sz(1)))
            set(PM.FigPaperHeight,'string',sprintf('%.1f',sz(2)))
            set(PM.FigPaperUnit,'value',pui)
            %
            hBrdr = md_paper(fig,'getprops');
            if isempty(hBrdr)
                set(PM.FigBorderStyle,'value',1);
                set(PM.FigBorder,'enable','off')
            else
                set(PM.FigBorderStyle,'value',find(strcmp(hBrdr.Name,get(PM.FigBorderStyle,'string'))))
                set(PM.FigBorder,'enable','on')
            end
            %
            set(PM.FigRendererTxt, 'enable', 'on')
            set(PM.FigRendererType, 'enable', 'on', 'backgroundcolor', Active)
            iRenderer = find(strcmp(get(fig,'Renderer'), get(PM.FigRendererType,'string')));
            set(PM.FigRendererType, 'value', iRenderer)
            %
            try
                gsmo = get(fig,'GraphicsSmoothing');
                set(PM.FigSmoothing, 'enable', 'on')
                smo  = valuemap(gsmo,{'on' 'off'},[1 0]);
                set(PM.FigSmoothing, 'value', smo)
            catch
                set(PM.FigSmoothing, 'enable', 'off', 'value', 0)
            end
        else
            set(PM.FigName,'string','')
            set(PM.FigColor,'backgroundcolor',Inactive, ...
                'enable','off')
            set([PM.FigNameTxt PM.FigColorTxt ...
                PM.FigPaperTypeTxt PM.FigBorderStyleTxt PM.FigPaperX ...
                PM.FigRendererTxt],'enable','off')
            set([PM.FigSmoothing],'enable','off','Value', 0)
            set([PM.FigName ...
                PM.FigPaperType PM.FigBorderStyle PM.FigPaperOrientation ...
                PM.FigPaperWidth PM.FigPaperHeight PM.FigPaperUnit ...
                PM.FigRendererType], ...
                'enable','off', ...
                'backgroundcolor',Inactive)
            set(PM.FigBorder,'enable','off')
        end

    case {'secondy','secondy_left','secondy_right'}
        ax = qpsa;
        linkedax = getappdata(ax,'linkedaxestype');
        if isempty(linkedax)
            set(ax,'xlimmode','manual')
            set(ax,'position',get(ax,'position'))
            na = axes('parent',get(ax,'parent'), ...
                'units',get(ax,'units'), ...
                'position',get(ax,'position'));
            if strcmp(get(ax,'dataaspectratiomode'),'manual')
                set(na,'dataaspectratio',get(ax,'dataaspectratio'))
            end
            if strcmp(get(ax,'plotboxaspectratiomode'),'manual')
                set(na,'plotboxaspectratio',get(ax,'plotboxaspectratio'))
            end
            set(na,'color','none', ...
                'xtick',[], ...
                'xcolor',get(ax,'xcolor'), ...
                'xdir',get(ax,'xdir'), ...
                'xscale',get(ax,'xscale'), ...
                'xlim',get(ax,'xlim'), ...
                'linewidth',get(ax,'linewidth'), ...
                'ycolor',get(ax,'ycolor'), ...
                'yaxislocation','right')
            if strcmp(get(ax,'yaxislocation'),'right')
                set(ax,'yaxislocation','left')
                %
                set(na,'ydir',get(ax,'ydir'), ...
                    'yscale',get(ax,'yscale'))
                ylm = get(ax,'ylimmode');
                if strcmp(ylm,'manual')
                    set(na,'ylim',get(ax,'ylim'))
                    set(ax,'ylimmode','auto')
                end
                ylabel(na,get(get(ax,'ylabel'),'string'))
                ylabel(ax,'')
                yappdata = {'yquantity','yunit','ylabelauto'};
                for i = 1:length(yappdata)
                    yad = yappdata{i};
                    if isappdata(ax,yad)
                        setappdata(na,yad,getappdata(ax,yad))
                        rmappdata(ax,yad)
                    end
                end
                %
                c = get(ax,'children');
                set(c,'parent',na)
            end
            setappdata(ax,'linkedaxestype','SecondY')
            setappdata(ax,'linkedaxes',na)
            setappdata(na,'linkedaxestype','SecondY')
            setappdata(na,'linkedaxes',ax)
            setappdata(na,'NonDataObject',1)
            qp_plotmanager refreshaxprop
        elseif strcmp(linkedax,'SecondY')
            switch cmd
                case 'secondy'
                    pos=get(gcbf,'position');
                    set(UD.PlotMngr.SecondYMenu,'position',get(0,'pointerlocation')-pos(1:2),'visible','on')
                case 'secondy_left'
                    na = getappdata(ax,'linkedaxes');
                    set(ax,'yaxislocation','right')
                    %
                    set(ax,'ydir',get(na,'ydir'), ...
                        'yscale',get(na,'yscale'), ...
                        'ycolor',get(na,'ycolor'))
                    ylm = get(na,'ylimmode');
                    if strcmp(ylm,'manual')
                        set(ax,'ylim',get(na,'ylim'))
                    end
                    ylabel(ax,get(get(na,'ylabel'),'string'))
                    yappdata = {'yquantity','yunit','ylabelauto'};
                    for i = 1:length(yappdata)
                        yad = yappdata{i};
                        if isappdata(na,yad)
                            setappdata(ax,yad,getappdata(na,yad))
                        end
                    end
                    %
                    delete(get(ax,'children'))
                    c = get(na,'children');
                    set(c,'parent',ax)
                    %
                    rmappdata(ax,'linkedaxestype')
                    rmappdata(ax,'linkedaxes')
                    delete(na)
                    qp_plotmanager refreshaxprop
                case 'secondy_right'
                    na = getappdata(ax,'linkedaxes');
                    rmappdata(ax,'linkedaxestype')
                    rmappdata(ax,'linkedaxes')
                    delete(na)
                    qp_plotmanager refreshaxprop
            end
        end

    case 'refreshaxprop'
        ax = qpsa;
        PM = UD.PlotMngr;
        if length(ax)==1
            secondy = strcmp(getappdata(ax,'linkedaxestype'),'SecondY');
            axes2d = isequal(getappdata(ax,'axes2D'),true);
            if strcmp(getappdata(ax,'BasicAxesType'),'Lon-Lat')
                types = get(PM.GeoDataMenu,'children');
                anyfound = 0;
                for i = 1:length(types)
                    tg = get(types(i),'tag');
                    if geodatafil('file_exists',tg(9:end))
                        set(types(i),'enable','on')
                        anyfound = 1;
                    else
                        set(types(i),'enable','off')
                    end
                end
                if anyfound
                    set(PM.GeoData,'enable','on')
                else
                   set(PM.GeoData,'enable','off')
                end
            else
                set(PM.GeoData,'enable','off')
            end
            %
            if 0 % ~isappdata(ax,'linkedaxestype') || secondy
                set(PM.SecondY,'enable','on')
            else
                set(PM.SecondY,'enable','off')
            end
            set([PM.AxNameTxt PM.AxTitleTxt PM.AxColorTxt PM.AxLineWTxt ...
                PM.AxPosition],'enable','on')
            set(PM.AxName, ...
                'enable','on', ...
                'backgroundcolor',Active)
            %
            set(PM.AxName,'string',get(ax,'tag'))
            axt = getappdata(ax,'BasicAxesType');
            set(PM.AxTypeTxt,'enable','on')
            if isempty(axt) || strcmp(axt,'undefined')
                axt = {'undefined','Time-Val','Time-Z','Time-X','X-Time','Distance-Val','X-Y','X-Y-Z','Lon-Lat','analog clock','digital clock','calendar page','legend'};
                set(PM.AxType, ...
                    'backgroundcolor',Active, ...
                    'value',1, ...
                    'string',axt, ...
                    'enable','on')
                axesNames = {'-','-'};
            else
                set(PM.AxType, ...
                    'value',1, ...
                    'string',{axt}, ...
                    'enable','inactive')
                axesNames = strsplit(axt,'-');
                if length(axesNames)==1
                    axesNames = {'-','-'};
                end
            end
            %
            if isappdata(ax,'title')
                set(PM.AxTitleAuto,'value',0, ...
                    'enable','on')
                set(PM.AxTitle,'string',getappdata(ax,'title'), ...
                    'backgroundcolor',Active, ...
                    'enable','on')
            else
                set(PM.AxTitleAuto,'value',1, ...
                    'enable','on')
                str = get(get(ax,'title'),'string');
                if iscell(str)
                    str = strjoin(str(:)','\\n{}');
                end
                set(PM.AxTitle,'string',str, ...
                    'backgroundcolor',Inactive, ...
                    'enable','off')
            end
            %
            clr = get(ax,'color');
            if isequal(clr,'none')
                set(PM.HasAxColor,'enable','on','value',0)
                set(PM.AxColor,'string','X','backgroundcolor',Inactive, ...
                    'enable','on')
            else
                set(PM.HasAxColor,'enable','on','value',1)
                set(PM.AxColor,'backgroundcolor',clr,'string','', ...
                    'enable','on')
            end
            lbx  = valuemap(get(ax,'box'),{'on' 'off'},[1 0]);
            lw   = get(ax,'linewidth');
            posu = get(ax,'unit');
            if ismember(posu,{'points','pixels','characters'})
                set(ax,'unit','centimeters')
                posu = 'centimenters';
            end
            pos = get(ax,'position');
            das = get(ax,'dataaspectratio');
            das = das/das(1);
            dasmode = get(ax,'dataaspectratiomode');
            hordasdefaultvalue = getappdata(ax,'haspectdefaultvalue');
            %
            XYZ = 'XYZ';
            for d = 1:3
                X = XYZ(d);
                x = lower(X);
                %
                axq = ax;
                if X == 'Z'
                    vw = get(ax,'view');
                    if secondy
                        axq = getappdata(ax,'linkedaxes');
                        x = 'y';
                        axesNames(2:3) = {'Y1','Y2'};
                    elseif length(axesNames)<3
                        axesNames{3} = '-';
                    end
                end
                if strcmp(axesNames{d},'-')
                    set(PM.(X).All,'enable','off')
                    if X~='X'
                        set([PM.(X).AspectValue], ...
                            'string','', ...
                            'backgroundcolor',Inactive)
                    end
                    set(PM.(X).Color,'backgroundcolor',Inactive)
                    set(PM.(X).AxisName,'string','-')
                    set([PM.(X).Label PM.(X).LimitMin PM.(X).LimitMax], ...
                        'string','', ...
                        'backgroundcolor',Inactive)
                    set([PM.(X).Scale PM.(X).Dir PM.(X).Loc], ...
                        'value',1, ...
                        'backgroundcolor',Inactive)
                    continue
                else
                    set(PM.(X).All,'enable','on')
                    set(PM.(X).AxisName,'string',axesNames{d})
                    if ~strcmp(X,'X')
                        if isequal(dasmode,'auto')
                            set([PM.(X).AspectAuto], ...
                                'value',1)
                            set([PM.(X).AspectValue], ...
                                'string','', ...
                                'userdata',1/das(d), ...
                                'enable','off', ...
                                'uicontextmenu',[], ...
                                'backgroundcolor',Inactive)
                        else
                            if strcmp(X,'Y') && ~isempty(hordasdefaultvalue)
                                useUCM = {'UIContextMenu',PM.YAspectContextMenu};
                            else
                                useUCM = {};
                            end
                            set([PM.(X).AspectAuto], ...
                                'value',0)
                            set([PM.(X).AspectValue], ...
                                'string',num2str(1/das(d)), ...
                                'userdata',1/das(d), ...
                                useUCM{:}, ...
                                'backgroundcolor',Active)
                        end
                    end
                end
                %
                Xlim = get(axq,[x 'lim']);
                %
                if strcmp(get(axq,[x 'dir']),'reverse')
                    Xlim = fliplr(Xlim);
                end
                set(PM.(X).LimitMin, ...
                    'string',num2str(Xlim(1)), ...
                    'backgroundcolor',Active)
                set(PM.(X).LimitMax, ...
                    'string',num2str(Xlim(2)), ...
                    'backgroundcolor',Active)
                %
                Xgr  = valuemap(get(axq,[x 'grid']),{'on' 'off'},[1 0]);
                Xsc  = valuemap(get(axq,[x 'scale']),{'linear','log'},[1 2]);
                switch x
                    case 'x'
                        Xlc  = valuemap(get(axq,'xaxislocation'),{'top','bottom'},[1 2]);
                    case 'y'
                        Xlc  = valuemap(get(axq,'yaxislocation'),{'left','right'},[1 2]);
                    case 'z'
                        Xlc = [];
                end
                Xdr  = valuemap(get(axq,[x 'dir']),{'normal' 'reverse'},[1 2]);
                Xcl  = get(axq,[x 'color']);
                %
                if all(Xlim>0)
                    set(PM.(X).Scale,'value',Xsc, ...
                        'backgroundcolor',Active)
                else
                    set(PM.(X).Scale,'value',1, ...
                        'backgroundcolor',Inactive, ...
                        'enable','off')
                end
                set(PM.(X).Grid,'value',Xgr)
                if X=='Z' && secondy
                    set(PM.(X).Loc,'value',2, ...
                        'backgroundcolor',Inactive, ...
                        'enable','off')
                elseif isempty(Xlc)
                    set(PM.(X).Loc,'value',1, ...
                        'backgroundcolor',Inactive, ...
                        'enable','off')
                elseif X=='Y' && secondy
                    set(PM.(X).Loc,'value',Xlc, ...
                        'backgroundcolor',Inactive, ...
                        'enable','off')
                else
                    set(PM.(X).Loc,'value',Xlc, ...
                        'backgroundcolor',Active)
                end
                set(PM.(X).Dir,'value',Xdr, ...
                    'backgroundcolor',Active)
                set(PM.(X).Color,'backgroundcolor',Xcl)
                if isappdata(axq,[x 'label'])
                    set(PM.(X).LabelAuto,'value',0)
                    str=getappdata(axq,[x 'label']);
                    set(PM.(X).Label,'string',str, ...
                        'backgroundcolor',Active)
                else
                    set(PM.(X).LabelAuto,'value',1)
                    str=get(get(axq,[x 'label']),'string');
                    if iscell(str)
                        str = strjoin(str(:)','\\n{}');
                    end
                    set(PM.(X).Label,'string',str, ...
                        'backgroundcolor',Inactive, ...
                        'enable','off')
                end
            end
            set(PM.AxBox,'value',lbx, ...
                'enable','on')
            set(PM.AxLineWidth,'string',num2str(lw), ...
                'backgroundcolor',Active, ...
                'enable','on')
            %
            posu  = valuemap(posu, ...
                {'centimeters','inches','normalized'}, ...
                1:3);
            set(PM.AxPosUnit,'value',posu, ...
                'backgroundcolor',Active, ...
                'enable','on')
            if posu==3
                pos = pos*100;
            end
            set(PM.AxXLowerLeft,'string',num2str(pos(1)), ...
                'backgroundcolor',Active, ...
                'enable','on')
            set(PM.AxYLowerLeft,'string',num2str(pos(2)), ...
                'backgroundcolor',Active, ...
                'enable','on')
            set(PM.AxWidth,'string',num2str(pos(3)), ...
                'backgroundcolor',Active, ...
                'enable','on')
            set(PM.AxHeight,'string',num2str(pos(4)), ...
                'backgroundcolor',Active, ...
                'enable','on')
        else
            set([PM.AxTitleTxt PM.AxTitleAuto ...
                PM.AxNameTxt PM.AxTypeTxt PM.SecondY ...
                PM.AxColorTxt PM.AxLineWTxt PM.AxPosition ...
                PM.X.All PM.Y.All PM.Z.All],'enable','off')
            set([PM.AxPosUnit PM.X.Color PM.Y.Color PM.Z.Color], ...
                'backgroundcolor',Inactive, ...
                'enable','off')
            set(PM.AxType, ...
                'backgroundcolor',Inactive, ...
                'enable','off', ...
                'value',1, ...
                'string',{' '})
            set([PM.AxName PM.AxTitle PM.AxColor ...
                PM.AxXLowerLeft PM.AxYLowerLeft PM.AxWidth PM.AxHeight ...
                PM.AxLineWidth ...
                PM.X.LimitMin PM.X.LimitMax PM.X.Label ...
                PM.Y.LimitMin PM.Y.LimitMax PM.Y.Label ...
                PM.Z.LimitMin PM.Z.LimitMax PM.Z.Label], ...
                'backgroundcolor',Inactive, ...
                'string','', ...
                'enable','off')
            set([PM.X.Grid PM.Y.Grid PM.Z.Grid PM.AxBox], ...
                'backgroundcolor',Inactive, ...
                'value',0, ...
                'enable','off')
            set([PM.HasAxColor ...
                PM.X.Loc PM.X.Scale ...
                PM.Y.Loc PM.Y.Scale ...
                PM.Z.Loc PM.Z.Scale], ...
                'backgroundcolor',Inactive, ...
                'value',1, ...
                'enable','off')
            set(PM.GeoData,'enable','off')
        end

    case 'refreshitemprop'
        It = getItem(UD);
        hOptions = UD.PlotMngr.Options.Handles;
        if isempty(It) || length(It)>1 || ~ishandle(It)
            % no item or multiple items selected - for the time being can't
            % visualize options. Call qp_update_options with empty Ops to
            % hide them all.
            Ops = [];
        else
            ItData = get(It,'UserData');
            if isfield(ItData.PlotState,'Ops')
                Ops = ItData.PlotState.Ops;
            else
                Ops = [];
            end
        end
        try
            qp_update_options(hOptions,UD,Ops)
            update_option_positions(UD,'plmn')
        catch Ex
            qp_error(sprintf('Catch in qp_plotmanager\\%s:',cmd),Ex)
        end
end

function Ax = getAx(UD)
AxIDs=get(UD.PlotMngr.AxList,'userdata');
AxVal=get(UD.PlotMngr.AxList,'value');
if get(UD.PlotMngr.FigAll,'value') || ...
        get(UD.PlotMngr.AxAll,'value') || ...
        isempty(AxIDs)
    Ax=AxIDs;
else
    Ax=AxIDs(AxVal);
end

function It = getItem(UD)
ItData=get(UD.PlotMngr.ItList,'userdata');
if ~iscell(ItData)
    It = [];
else
    ItIDs=ItData{2};
    ItVal=get(UD.PlotMngr.ItList,'value'); % <-- use ItData{1} - ItData{2} is invalid if animated or linked
    if isempty(ItIDs)
        It=[];
    else
        It=ItIDs(ItVal);
    end
end

function v = cget(handle,prop)
v = get(handle,prop);
if isempty(v)
    v = {};
elseif ~iscell(v)
    v = {v};
end

function S = strjoin(C,sym)
if isempty(C)
    S = '';
else
    C = C(:)';
    C(2,:) = {sym};
    C{2,end} = '';
    S = strcat(C{:});
end