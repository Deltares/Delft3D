function qp_prefs(UD,mfig,cmd,cmdargs)
%QP_PREFS QuickPlot preferences dialog callback functions.

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
    case 'preferences'
        qp_preferences_interface;
        
    case 'organizationname'
        orgn=findobj(gcbf,'tag','organizationname');
        name=strtrim(get(orgn,'string'));
        qp_settings('organizationname',name);
        
    case 'timezonehandling'
        tzh=findobj(gcbf,'tag','timezonehandling');
        tze=findobj(gcbf,'tag','enforcedtimezone');
        itzh=get(tzh,'value');
        tzhstr=get(tzh,'string');
        if itzh<3
            qp_settings('timezone',tzhstr{itzh})
            set(tze,'enable','off')
        else % timezonehandling = enforced
            set(tze,'enable','on')
            itze=get(tze,'value');
            tzestr=get(tze,'string');
            qp_settings('timezone',tzestr{itze})
        end
        d3d_qp updatetimezone

    case 'enforcedtimezone'
        tze=findobj(gcbf,'tag','enforcedtimezone');
        % timezonehandling = enforced
        set(tze,'enable','on')
        itze=get(tze,'value');
        tzestr=get(tze,'string');
        qp_settings('timezone',tzestr{itze})
        d3d_qp updatetimezone
        
    case 'update_showversion'
        sv=get(gcbo,'value');
        if sv
            qp_settings('showversion','on')
            d3d_qp showversion
        else
            qp_settings('showversion','off')
            d3d_qp hideversion
        end
        
    case 'prefpane'
        currentpane = get(gcbf,'userdata');
        newpane = get(gcbo,'value');
        panehandles = get(gcbo,'userdata');
        set(panehandles{currentpane},'visible','off')
        set(panehandles{newpane},'visible','on')
        set(gcbf,'userdata',newpane)

    case {'defaultnewfigure','defaultloadfigure'}
        rb1=findobj(gcbf,'tag','defaultnewfigure');
        dfct=findobj(gcbf,'tag','defaultfigurecolortext');
        dfc=findobj(gcbf,'callback','d3d_qp defaultfigurecolor');
        rb2=findobj(gcbf,'tag','defaultloadfigure');
        select=findobj(gcbf,'callback','d3d_qp defaultfigure');
        df=findobj(gcbf,'tag','defaultfigure');
        if strcmp(cmd,'defaultnewfigure')
            set(rb1,'value',1)
            set([dfct dfc],'enable','on')
            set(dfc,'backgroundcolor',qp_settings('defaultfigurecolor')/255)
            set(rb2,'value',0)
            set(select,'enable','off')
            set(df,'string','','enable','off')
            qp_settings('defaultfigure',[])
        else
            set(rb1,'value',0)
            set([dfct dfc],'enable','off')
            set(dfc,'backgroundcolor',get(gcbf,'color'))
            set(rb2,'value',1)
            set(select,'enable','on')
            set(df,'string','','enable','inactive')
            d3d_qp defaultfigure
        end

    case 'defaultfigure'
        currentdir=pwd;
        try
            orig_fn=qp_settings('defaultfigure');
            targetdir='';
            if ~isempty(orig_fn)
                targetdir=fileparts(orig_fn);
            end
            if isempty(targetdir)
                targetdir=qp_basedir('exe');
            end
            cd(targetdir);
            [fn,pn]=uigetfile({'*.fig', 'MATLAB Figures (*.fig)'},'Select Default Figure File ...');
            cd(currentdir);
            if ischar(fn)
                fn = [pn fn];
                set(findobj(gcbf,'tag','defaultfigure'),'string',fn)
                qp_settings('defaultfigure',fn);
            elseif isempty(orig_fn)
                d3d_qp defaultnewfigure
            end
        catch
            cd(currentdir);
        end
        
    case 'defaultfigurepos'
        Inactive = UD.Inactive;
        Active   = UD.Active;
        %
        dfpm = findobj(gcbf,'tag','defaultfigurepos-menu');
        dfpe = findobj(gcbf,'tag','defaultfigurepos-edit');
        FigPos = get(dfpm,'string');
        fpval = get(dfpm,'value');
        if fpval==2 % Manual
            if isequal(gcbo,dfpe)
                fp = get(dfpe,'string');
                pos = sscanf(fp,'%i')';
                if length(pos)~=4 || pos(3)<=0 || pos(4)<=0
                    pos = [];
                end
            else
                pos = [];
            end
            if isempty(pos)
                f = qp_createfig('quick',[]);
                set(f,'units','pixels')
                pos = get(f,'position');
                delete(f)
            end
            fp = sprintf('%i %i %i %i',pos);
            set(dfpe,'enable','on', ...
                'string',fp, ...
                'backgroundcolor',Active)
            qp_settings('defaultfigurepos',fp)
        else
            qp_settings('defaultfigurepos',lower(FigPos{fpval}))
            set(dfpe,'enable','off', ...
                'string','', ...
                'backgroundcolor',Inactive)
        end
        
    case 'defaultrenderer'
        dfr = findobj(gcbf,'tag','defaultrenderer');
        Renderers = get(dfr,'string');
        rval = get(dfr,'value');
        rndr = Renderers{rval};
        qp_settings('defaultrenderer', rndr)
        
    case 'defaultsmoothing'
        dfs = findobj(gcbf,'tag','defaultsmoothing');
        smo = get(dfs,'value');
        qp_settings('graphicssmoothing', smo)
        
    case {'gridviewbackgroundcolor','gridviewgridcolor', ...
            'gridviewselectioncolor','gridviewlandboundarycolor', ...
            'defaultfigurecolor','defaultaxescolor'}
        clr = qp_settings(cmd)/255;
        newclr = uisetcolor(clr);
        if ~isequal(newclr,0)
            qp_settings(cmd,newclr*255)
            set(gcbo,'backgroundcolor',newclr)
            switch cmd
                case 'gridviewbackgroundcolor'
                    set(UD.GridView.Fig,'color',newclr)
                case 'gridviewgridcolor'
                    tags = {'GRID','GRIDother'};
                    for i=1:length(tags)
                        G=findobj(UD.GridView.Fig,'tag',tags{i});
                        recolor(G,clr,newclr)
                    end
                case 'gridviewselectioncolor'
                    tags = {'SELSURF','SELPATCH','SELLINE'};
                    for i=1:length(tags)
                        G=findobj(UD.GridView.Fig,'tag',tags{i});
                        recolor(G,clr,newclr)
                    end
                case 'gridviewlandboundarycolor'
                    G=findall(UD.GridView.Fig,'tag','landboundary');
                    recolor(G,clr,newclr)
                case {'defaultfigurecolor','defaultaxescolor'}
                    % Nothing to do. These options affect only subsequent actions.
            end
        end

    case {'colorbar_ratio','export_max_ntimes'}
        newval = round(str2double(get(gcbo,'string')));
        qp_settings(cmd,newval);
        set(gcbo,'string',num2str(newval))
        
    case {'gridviewshowindices','boundingbox','v6zoombehavior','showinactiveopt'}
        newval = get(gcbo,'value');
        qp_settings(cmd,newval);
        if newval
            newval = 'on';
        else
            newval = 'off';
        end
        %
        switch cmd
            case 'v6zoombehavior'
                qp_prefs(UD,mfig,'v6zoomswitch',newval)
        end
        
    case 'v6zoomswitch'
        F = findall(0,'type','figure');
        zoomon = logical(zeros(size(F)));
        for i = 1:length(F)
            zoomon(i) = ~strcmp(zoom(F(i),'getmode'),'off');
            if zoomon(i)
                zoom(F(i),'off')
            end
        end
        zoom('v6',cmdargs)
        for i = 1:length(F)
            if zoomon(i)
                zoom(F(i),'on')
                if strcmp(get(F(i),'name'),'Grid View')
                    set(F(i),'WindowButtonMotionFcn','qp_gridview trackcoord')
                end
            end
        end
        
    case 'changefont'
        uicontrolfont = qp_fontsettings('Font');
        uicontrolfont = uisetfont(uicontrolfont);
        if ~isequal(uicontrolfont,0)
            qp_fontsettings(uicontrolfont);
            figs = [mfig UD.PlotMngr.Fig UD.FilOpt.Fig UD.GridView.Fig UD.ComLine.Fig gcbf];
            MWfig = findall(0,'tag','UI_MESSAGE window');
            if ~isempty(MWfig)
                figs(end+1)=MWfig;
            end
            UICONTROL = findall(figs,'type','uicontrol');
            set(UICONTROL,uicontrolfont)
        end
        
    case 'filefilterselection'
        filsel=findobj(gcbf,'tag','filefilterselection');
        ifilters=get(filsel,'value');
        if length(ifilters)>15
           ui_message('error','For stability reasons, please select at most 15 filters.')
           % make sure ui_message isn't on top of the preferences dialog
           set(gcbf,'visible','off') % hide it
           figure(gcbf) % and show again
           %
           set(filsel,'value',get(filsel,'userdata'))
        else
           set(filsel,'userdata',ifilters)
           filters=get(filsel,'string');
           filterstring=sprintf('"%s",',filters{ifilters});
           filterstring(end)=[];
           qp_settings('filefilterselection',filterstring);
        end

    case 'netcdf_use_fillvalue'
        newval = get(gcbo,'value');
        string = {'exact_match','valid_range'};
        newval = string{newval};
        qp_settings(cmd,newval);
        
    otherwise
        error('Unknown command in qp_settings: %s',cmd)
end

