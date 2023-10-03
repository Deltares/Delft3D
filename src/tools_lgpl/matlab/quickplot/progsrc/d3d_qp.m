function varargout = d3d_qp(cmd,varargin)
%D3D_QP QuickPlot user interface: plotting interface for Delft3D output data.
%   To start the interface type: d3d_qp
%
%   See also QPFOPEN, QPREAD.

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
try
    if nargin==0
        cmd='initialize';
    end
    if nargout>0
        varargout = d3d_qp_core(cmd,varargin{:});
    else
        d3d_qp_core(cmd,varargin{:});
    end
catch Ex
    qp_error(sprintf('Catch in d3d_qp\\%s:',cmd),Ex,'d3d_qp_core')
end

function outdata=d3d_qp_core(cmd,varargin)
%VERSION = 2.70
qpversionbase = 'v<VERSION>';
gitrepo = '<GITREPO>';
githash = '<GITHASH>';
qpcreationdate = '<CREATIONDATE>';
%
persistent qpversion logfile logtype

if isempty(qpversion)
    if isequal(qpversionbase(1:2),'v<')
        qpversion='source code version';
    else
        qpversion=qpversionbase;
    end
    if isempty(strfind(qpversion,'('))
        if strncmp(fliplr(computer),'46',2)
            nbits=64;
        else
            nbits=32;
        end
        qpversion=sprintf('%s (%ibit)',qpversion,nbits);
    end
    logfile=0;
    logtype=1;
end

T_=1; ST_=2; M_=3; N_=4; K_=5;

if ~ischar(cmd)
    ui_message('error','Invalid command: must be char array')
    return
end
cmd=lower(cmd);
if nargout~=0
    if strcmp(cmd,'initialize')
        outdata = {[]};
    elseif strcmp(cmd,'iswl')
        outdata = {isequal(qp_settings('WLextensions','off'),'on')};
        return
    elseif strcmp(cmd,'version')
        if nargin>1
            outdata = {qp_checkversion(varargin{:})};
        else
            outdata = {qpversion};
        end
        return
    elseif isstandalone % allow standalone auto start ...
        outdata = {[]};
    elseif none(strcmp(cmd,{'loaddata','selected','selectedfigure','selectedaxes','selecteditem','selectfield','selectedfield','qpmanual','matlabmanual','selecteddomain'}))
        error('Too many output arguments.')
    end
end

if isempty(gcbf) || ~strcmp(get(gcbf,'tag'),'Delft3D-QUICKPLOT')
    mfig=findobj(allchild(0),'flat','tag','Delft3D-QUICKPLOT');
    
    if isempty(mfig) && none(strcmp(cmd,{'initialize','initialize_background','closefigure','printfigure','dayok'}))
        d3d_qp
        mfig=findobj(allchild(0),'flat','tag','Delft3D-QUICKPLOT');
        if isempty(mfig)
            %
            % if quickplot did not initialize, then stop
            %
            return
        end
    end
else
    mfig=gcbf;
end
UOH=[];
if ~isempty(mfig)
    UD=getappdata(mfig,'QPHandles');
    if ~isempty(UD)
        UOH=UD.Options.Handles;
        Inactive=UD.Inactive;
        Active=UD.Active;
    else
    end
else
    UD=[];
end

cmdargs=varargin;

if strncmp(cmd,'geodata_wms',11)
    cmdfull = cmd;
    cmd = 'geodata_wms';
end
switch cmd
    case {'slider','startanim','animselect','animpush','stopanim'}
        qck_anim(cmd,cmdargs{:});

    case 'set'
        qp_settings(cmdargs{:})
        
    case 'debug'
        error('Insert a break point HERE for debugging!')

    case {'gridviewpoint','gridviewline','gridviewlineseg', ...
            'gridviewpiecewise','gridviewarbline','gridviewrange', ...
            'gridviewall','gridviewarbrect','gridviewarbarea', ...
            'gridviewpath','gridviewpath_shortest_path', ...
            'gridviewpath_least_segments'}
        qp_gridview(cmd,cmdargs{:});
        
    case {'newfigure', 'newaxes', 'openfigure', 'refreshfigs', ...
            'allfigures', 'refreshaxes', 'allaxes', 'pmshowselect', ...
            'refreshitems', 'itemlist', 'iteminfo', 'deleteaxes', ...
            'deleteitems', 'linkitems', 'selectfigure', ...
            'selectaxes', 'selectitem','refreshfigprop', ...
            'refreshaxprop','moveitemup','moveitemdown', ...
            'updatearrows', 'newaxes_oneplot', 'newaxes_matrix', ...
            'newaxes_specloc', 'secondy', 'secondy_left', ...
            'secondy_right', 'moveitemtoback','refreshitemprop'}
        qp_plotmanager(cmd,UD,logfile,logtype,cmdargs);
        
    case {'selectedfigure', 'selectedaxes', 'selecteditem'}
        outdata = {qp_plotmanager(cmd,UD,logfile,logtype,cmdargs)};
        
    case {'geodata','geodata_gshhs','geodata_border','geodata_river','geodata_wms'}
        if length(cmd)>7
            subtype = cmd(9:end);
        elseif ~isempty(cmdargs)
            subtype = cmdargs{1};
        else
            subtype = '';
        end
        if isempty(cmdargs) && isempty(subtype)
            pos=get(gcbf,'position');
            set(UD.PlotMngr.GeoDataMenu,'position',get(0,'pointerlocation')-pos(1:2),'visible','on')
        else
            pfig = qpsf;
            parent = qpsa;
            PS.FI.FileType = 'geodata';
            PS.Domain = [];
            toback = 0;
            switch subtype
                case {'gshhs','shore lines'}
                    PS.Props.Name = 'shore lines';
                case {'border','country and state borders'}
                    PS.Props.Name = 'country and state borders';
                case {'river','rivers'}
                    PS.Props.Name = 'rivers';
                otherwise
                    if ~strncmp(subtype,'wms',3)
                        error('Unknown geodata plot type: %s',subtype)
                    elseif strncmp(subtype,'wms/',4)
                        PS.Props.Name = subtype;
                    else
                        cmd = cmdfull;
                        PS.Props.Name = ['wms/' cmd(13:end)];
                    end
                    toback = 1;
            end
            PS.Props.DimFlag = zeros(1,5);
            PS.Props.NVal = -1;
            PS.SubField = {};
            PS.Selected = cell(1,5);
            PS.Parent = parent;
            PS.Handles = [];
            PS.Stations = [];
            PS.Ops.version = 1.3;
            PS.Ops.axestype = getappdata(parent,'AxesType');
            PS.Ops.basicaxestype = getappdata(parent,'BasicAxesType');
            [hNew,Error,Info]=qp_plot(PS);
            set(UD.PlotMngr.FigList,'value',1,'string',listnames(pfig,'showType','no','showHandle','no','showTag','no'),'userdata',pfig);
            set(UD.PlotMngr.ItList,'value',[]) % clear item selection such that new item will be selected
            d3d_qp refreshfigs
            if toback
                d3d_qp moveitemtoback
            end
        end
        
    case 'plotmanagerresize'
        if ~isempty(UD)
            qp_plotmanager('resize',UD,logfile,logtype);
        end
        
    case 'fileoptionsresize'
        if ~isempty(UD)
            fp = get(UD.FilOpt.Fig,'pos');
            cp = get(UD.FilOpt.Close,'pos');
            cp(1) = max(1,fp(3)-169);
            cp(3) = max(1,fp(3)-cp(1)-9);
            set(UD.FilOpt.Close,'pos',cp)
        end
        
    case 'optionsresize'
        pos = get(gcbf,'pos');
        pos(3) = 180;
        if pos(4)<100
            pos(2) = pos(2) + pos(4) - 100;
            pos(4) = 100;
        end
        set(gcbf,'pos',pos)
        %
        update_option_positions(UD,'main',pos(4)-30+1)
        %
        spos = get(UD.Options.Slider,'position');
        spos(4) = pos(4)-15;
        set(UD.Options.Slider,'position',spos)
        %
        dpos = get(UD.Options.Dock,'position');
        dpos(2) = pos(4)-15;
        set(UD.Options.Dock,'position',dpos)
        
    case {'initialize','initialize_background'}
        showUI = isequal(cmd,'initialize');
        mfig=findobj(allchild(0),'flat','tag','Delft3D-QUICKPLOT');
        %
        if length(mfig)>1
            delete(mfig(2:length(mfig)));
            mfig=mfig(1);
        elseif isempty(mfig)
            if isstandalone && matlabversionnumber>7.10
                if ~qp_checkversion(qpversionbase,gitrepo,githash,qpcreationdate)
                    return
                end
            end
            %
            if ~isstandalone
                cmdx = qp_settings('autoruncmd','');
                if ~iscell(cmdx)
                    if isempty(cmdx)
                        cmdx = {};
                    else
                        cmdx = {cmdx};
                    end
                end
                for i = 1:length(cmdx)
                    try
                        eval(cmdx{i});
                    catch
                        warning('Error caught while running: %s',cmdx{i})
                    end
                end
            end
            %
            mfig=qp_interface(showUI);
            if showUI && strcmp(qp_settings('PlotMngrVisible','off'),'on')
                d3d_qp plotmngr
            end
            if strcmp(qp_settings('showversion','off'),'on')
                set(mfig,'name',cat(2,'Delft3D-QUICKPLOT ',qpversion));
            end
            if qp_settings('v6zoombehavior') && matlabversionnumber >= 7 && matlabversionnumber < 9
                qp_prefs(UD,mfig,'v6zoomswitch','on')
            end
            if isstandalone
                set(findobj(mfig,'tag','loaddata'),'visible','off')
                set(findobj(mfig,'tag','scriptfiletomcw'),'visible','off')
            else
                set(findobj(mfig,'tag','aboutmatlab'),'visible','off')
            end
            qp_updaterecentfiles(mfig)
        end
        if showUI
            figure(mfig);
        end
        init_netcdf_settings
        if isstandalone
            try
                CloseSplashScreen;
            end
        else
            check_nonprivate_files
        end
        d3d_qp updatedomains
        d3d_qp refreshfigs
        set(mfig,'CloseRequestFcn','d3d_qp close');
        
    case {'openfile','reloadfile','openurl'}
        OpenFile=findobj(mfig,'tag','openfile','type','uipushtool');
        Handle_SelectFile=findobj(mfig,'tag','selectfile');
        File=get(Handle_SelectFile,'userdata');
        sel=get(Handle_SelectFile,'value');
        Str=get(Handle_SelectFile,'string');
        %
        NewRecord = [];
        if strcmp(cmd,'reloadfile')
            NewRecord = qp_refresh(File(sel));
        elseif isempty(cmdargs)
            if strcmp(cmd,'openurl')
                [NewRecord,FileName,Tp,Otherargs]=qp_proxy('openurl');
            else
                pn=get(OpenFile,'userdata');
                [NewRecord,FileName,Tp,Otherargs]=qp_proxy('opennew',pn);
            end
        else
            [NewRecord,FileName,Tp,Otherargs]=qp_proxy('open',cmdargs{:});
        end
        %
        if isempty(NewRecord)
            return
        else
            FileName = NewRecord.Name;
            Otherargs = NewRecord.Otherargs;
        end
        %
        if isempty(File)
            Str={abbrevfn(FileName,60)};
            sel=1;
            File=NewRecord;
        else
            FileNameList={File.Name};
            sel=find(strcmp(FileName,FileNameList));
            if isempty(sel)
                sel=length(File)+1;
            end
            Str{sel}=abbrevfn(FileName,60);
            File(sel)=NewRecord;
        end
        set(Handle_SelectFile,'userdata',File,'string',Str,'value',sel,'enable','on','backgroundcolor',Active);
        
        Handle_ReloadFile=findobj(mfig,'tag','reloadfile');
        set(Handle_ReloadFile,'enable','on');
        Handle_CloseFile=findobj(mfig,'tag','closefile');
        set(Handle_CloseFile,'enable','on');
        Handle_CloseAllFiles=findobj(mfig,'tag','closeallfiles');
        set(Handle_CloseAllFiles,'enable','on');
        set(findobj(mfig,'tag','fileinfo'),'enable','on');
        %
        RecentFiles = cell(9,1);
        for i = 1:9
            RecentFiles{i} = qp_settings(sprintf('File%2.2i',i),'');
        end
        ThisFile = sprintf('''%s'' ',FileName,Otherargs{:});
        ThisFile(end)=[];
        ii = find(strcmp(ThisFile,RecentFiles));
        if isempty(ii)
            ii = 9;
        else
            ii = ii(1);
        end
        for i = ii:-1:2
            if ~isempty(RecentFiles{i-1,1})
                qp_settings(sprintf('File%2.2i',i),RecentFiles{i-1,1})
                RecentFiles{i}=RecentFiles{i-1,1};
            end
        end
        qp_settings('File01',ThisFile)
        qp_updaterecentfiles(mfig)
        %
        if logfile
            writelog(logfile,logtype,cmd,FileName,Otherargs{:});
        end
        d3d_qp selectfile*
        
    case {'difffiles','diff_files','diff_files_one_domain'}
        pos = get(mfig,'position');
        pos(4) = 310;
        %
        dfig = findall(0,'type','figure','tag','Diff Files');
        delete(dfig);
        %
        dfig = qp_uifigure('Diff Files','','Diff Files',pos);
        %
        voffset=pos(4)-29;
        w = (pos(3)-40)/3;
        uicontrol('Parent',dfig, ...
            'Enable','off', ...
            'Position',[11 voffset w 18], ...
            'String','Domains Per File', ...
            'Style','text', ...
            'Horizontalalignment','left', ...
            'Tag','txt_domain');
        uicontrol('Parent',dfig, ...
            'Callback','d3d_qp all_domains', ...
            'Enable','off', ...
            'Position',[21+w voffset w 18], ...
            'String','All Defined', ...
            'Style','radiobutton', ...
            'Horizontalalignment','left', ...
            'Value',1, ...
            'Tag','all_domains');
        h1D = uicontrol('Parent',dfig, ...
            'Callback','d3d_qp one_domain', ...
            'Enable','off', ...
            'Position',[31+2*w voffset w 18], ...
            'String','Select One', ...
            'Style','radiobutton', ...
            'Horizontalalignment','left', ...
            'Value',0, ...
            'Tag','one_domain');
        voffset=voffset-30;
        uicontrol('Parent',dfig, ...
            'Enable','on', ...
            'Position',[11 voffset pos(3)-20 18], ...
            'String','Compare File', ...
            'Style','text', ...
            'Horizontalalignment','left', ...
            'Tag','txt_difffile1');
        voffset=voffset-20;
        L1 = uicontrol('Parent',dfig, ...
            'Callback','d3d_qp difffile1', ...
            'Enable','off', ...
            'Position',[11 voffset pos(3)-40 20], ...
            'String',' ', ...
            'BackgroundColor',Inactive, ...
            'Style','popupmenu', ...
            'Tag','difffile1', ...
            'Value',1);
        uicontrol('Parent',dfig, ...
            'Style','pushbutton', ...
            'CData',qp_icon('openfile','nan'), ...
            'Enable','on', ...
            'Position',[pos(3)-30 voffset 20 20], ...
            'String',' ', ...
            'Callback','d3d_qp openfile1', ...
            'Tooltip','Open a data file', ...
            'Tag','openfile1');
        voffset=voffset-30;
        uicontrol('Parent',dfig, ...
            'Enable','off', ...
            'Position',[11 voffset 80 18], ...
            'String','Domain', ...
            'Style','text', ...
            'Horizontalalignment','left', ...
            'Tag','txt_diffdomain1');
        dom1 = uicontrol('Parent',dfig, ...
            'Callback','d3d_qp diffdomain1', ...
            'Enable','off', ...
            'Position',[101 voffset pos(3)-110 20], ...
            'String','-- none defined --', ...
            'BackgroundColor',Inactive, ...
            'Style','popupmenu', ...
            'Tag','diffdomain1', ...
            'Value',1);
        %
        voffset=voffset-30;
        uicontrol('Parent',dfig, ...
            'Enable','on', ...
            'Position',[11 voffset pos(3)-20 18], ...
            'String','Reference File', ...
            'Style','text', ...
            'Horizontalalignment','left', ...
            'Tag','txt_difffile2');
        voffset=voffset-20;
        L2 = uicontrol('Parent',dfig, ...
            'Callback','d3d_qp difffile2', ...
            'Enable','off', ...
            'Position',[11 voffset pos(3)-40 20], ...
            'String',' ', ...
            'BackgroundColor',Inactive, ...
            'Style','popupmenu', ...
            'Tag','difffile2', ...
            'Value',1);
        uicontrol('Parent',dfig, ...
            'Style','pushbutton', ...
            'CData',qp_icon('openfile','nan'), ...
            'Enable','on', ...
            'Position',[pos(3)-30 voffset 20 20], ...
            'String',' ', ...
            'Callback','d3d_qp openfile2', ...
            'Tooltip','Open a data file', ...
            'Tag','openfile2');
        voffset=voffset-30;
        uicontrol('Parent',dfig, ...
            'Enable','off', ...
            'Position',[11 voffset 80 18], ...
            'String','Domain', ...
            'Style','text', ...
            'Horizontalalignment','left', ...
            'Tag','txt_diffdomain2');
        dom2 = uicontrol('Parent',dfig, ...
            'Callback','d3d_qp diffdomain2', ...
            'Enable','off', ...
            'Position',[101 voffset pos(3)-110 20], ...
            'String','-- none defined --', ...
            'BackgroundColor',Inactive, ...
            'Style','popupmenu', ...
            'Tag','diffdomain2', ...
            'Value',1);
        %
        voffset=voffset-30;
        DTp = uicontrol('Parent',dfig, ...
            'Enable','on', ...
            'Position',[11 voffset w 20], ...
            'Style','text', ...
            'String','Differencing Method', ...
            'HorizontalAlignment','left', ...
            'Tag','txt_difftype');
        DTp = uicontrol('Parent',dfig, ...
            'Enable','on', ...
            'Position',[21+w voffset pos(3)-30-w 20], ...
            'Style','popupmenu', ...
            'String',{'Simply by Index', 'Correct for Renumbering'}, ...
            'BackgroundColor',Active, ...
            'HorizontalAlignment','left', ...
            'Tag','difftype', ...
            'Tooltip','Select the method of differencing');
        DTpStr = {'index','renum'};
        %
        voffset=voffset-30;
        uicontrol('Parent',dfig, ...
            'Enable','on', ...
            'Position',[11 voffset pos(3)-20 18], ...
            'String','Label', ...
            'Style','text', ...
            'Horizontalalignment','left', ...
            'Tag','txt_difflabel');
        voffset=voffset-20;
        Lb = uicontrol('Parent',dfig, ...
            'Callback','d3d_qp difflabel', ...
            'Enable','off', ...
            'Position',[11 voffset pos(3)-40 20], ...
            'String','Data File Equals Reference File', ...
            'BackgroundColor',Inactive, ...
            'HorizontalAlignment','left', ...
            'Style','edit', ...
            'Tag','difflabel', ...
            'Tooltip','Specify a name to identify this pair of files');
        setappdata(Lb,'LabelMode','auto')
        setappdata(Lb,'Label','')
        %
        voffset=voffset-30;
        width = (pos(3)-30)/2;
        uicontrol('Parent',dfig, ...
            'Style','pushbutton', ...
            'Enable','on', ...
            'Position',[11 voffset width 20], ...
            'String','Cancel', ...
            'Callback','d3d_qp diffcancel', ...
            'Tag','diffcancel');
        uicontrol('Parent',dfig, ...
            'Style','pushbutton', ...
            'Enable','off', ...
            'Position',[pos(3)-10-width voffset width 20], ...
            'String','Define', ...
            'Callback','d3d_qp diffdefine', ...
            'Tag','diffdefine', ...
            'Tooltip','Define new data resources as data in file A minus data in file B');
        %
        Handle_SelectFile=findobj(mfig,'tag','selectfile');
        File=get(Handle_SelectFile,'userdata');
        Str=get(Handle_SelectFile,'string');
        NrInList=get(Handle_SelectFile,'value');
        if ~isempty(File)
            set([L1 L2], ...
                'enable','on', ...
                'BackgroundColor',Active, ...
                'string',Str, ...
                'value',NrInList)
            set(L1,'userdata',File)
        end
        %
        interactive = 1;
        if ~isempty(cmdargs)
            switch cmd
                case 'diff_files_one_domain'
                    d3d_qp('difffile1',cmdargs{1})
                    d3d_qp('one_domain')
                    if nargin>2
                        domList = get(dom1,'string');
                        idom = ustrcmpi(cmdargs{2},domList);
                        if idom<0
                            ui_message('warning','Unable to identify domain ''%s'' in file %s.',cmdargs{2},cmdargs{1})
                        else
                            set(dom1,'value',idom)
                        end
                    end
                    if nargin>3
                        d3d_qp('difffile2',cmdargs{3})
                        interactive = isequal(cmdargs{1},cmdargs{3});
                    end
                    if nargin>4
                        domList = get(dom2,'string');
                        idom = ustrcmpi(cmdargs{4},domList);
                        if idom<0
                            ui_message('warning','Unable to identify domain ''%s'' in file %s.',cmdargs{4},cmdargs{3})
                        else
                            set(dom2,'value',idom)
                        end
                    end
                    if nargin>5
                        d3d_qp('difflabel',cmdargs{5})
                    end
                    if nargin>6
                        iTp = ustrcmpi(varargin{6},DTpStr);
                        if iTp > 0
                            set(DTp,'value',iTp)
                        end
                    end
                otherwise
                    d3d_qp('difffile1',cmdargs{1})
                    if nargin>2
                        d3d_qp('difffile2',cmdargs{2})
                        interactive = isequal(cmdargs{1},cmdargs{2});
                    end
                    if nargin>3
                        d3d_qp('difflabel',cmdargs{3})
                    end
                    if nargin>4
                        iTp = ustrcmpi(varargin{4},DTpStr);
                        if iTp > 0
                            set(DTp,'value',iTp)
                        end
                    end
            end
        elseif ~isempty(File)
            d3d_qp('difffile1')
            d3d_qp('difffile2')
        end
        %
        if interactive
            set(dfig,'visible','on','windowstyle','modal')
            waitfor(dfig,'visible','off')
        else
            d3d_qp('diffdefine')
        end
        %
        Label = get(Lb,'string');
        Indices = get(dfig,'userdata');
        DiffFile = get(L1,'userdata');
        DiffType = DTpStr{get(DTp,'value')};
        if strcmp(get(h1D,'enable'),'on') && get(h1D,'value')
            Domains = [get(dom1,'value') get(dom2,'value')];
            domList = get(dom1,'string');
            if iscell(domList)
                Domain1 = domList{Domains(1)};
            else
                Domain1 = '';
            end
            domList = get(dom2,'string');
            if iscell(domList)
                Domain2 = domList{Domains(2)};
            else
                Domain2 = '';
            end
            cmd = 'diff_files_one_domain';
        else
            Domains = [];
            cmd = 'diff_files';
        end
        delete(dfig)
        %
        if ~isempty(Indices)
            FileName=Label;
            NewRecord.QPF=1;
            NewRecord.Name=FileName;
            NewRecord.Data.Files=DiffFile(Indices);
            NewRecord.Data.DiffDomain = Domains;
            NewRecord.Data.DiffType = DiffType;
            NewRecord.FileType='diff';
            NewRecord.Options=0;
            NewRecord.Otherargs={};
            %
            if isempty(File)
                Str={abbrevfn(FileName,60)};
                NrInList=1;
                File=NewRecord;
            else
                FileNameList={File.Name};
                NrInList=find(strcmp(FileName,FileNameList));
                if isempty(NrInList)
                    NrInList=length(File)+1;
                end
                Str{NrInList}=abbrevfn(FileName,60);
                File(NrInList)=NewRecord;
            end
            set(Handle_SelectFile,'userdata',File,'string',Str,'value',NrInList,'enable','on','backgroundcolor',Active);
            d3d_qp selectfile*
            %
            if logfile
                switch cmd
                    case 'diff_files_one_domain'
                        writelog(logfile,logtype,cmd,NewRecord.Data.Files(1).Name,Domain1,NewRecord.Data.Files(2).Name,Domain2,Label,DiffType);
                    otherwise
                        writelog(logfile,logtype,cmd,NewRecord.Data.Files(1).Name,NewRecord.Data.Files(2).Name,Label,DiffType);
                end
            end
        end
        
    case {'all_domains','one_domain'}
        hDiff = findall(0,'tag','Diff Files');
        if length(hDiff)~=1, return, end
        hDomSelAll = findobj(hDiff,'tag','all_domains');
        hDomSelOne = findobj(hDiff,'tag','one_domain');
        if strcmp(cmd,'all_domains')
            set(hDomSelAll,'value',1)
            set(hDomSelOne,'value',0)
            for filnr = '12'
                hDomText = findobj(hDiff,'tag',['txt_diffdomain' filnr]);
                hDom     = findobj(hDiff,'tag',['diffdomain' filnr]);
                Domains  = get(hDom,'userdata');
                if ~isempty(Domains)
                    set(hDomText,'enable','off')
                    set(hDom, ...
                        'enable','off', ...
                        'backgroundcolor',Inactive, ...
                        'value',1, ...
                        'string','-- all domains --')
                end
            end
        else
            set(hDomSelOne,'value',1)
            set(hDomSelAll,'value',0)
            for filnr = '12'
                hDomText = findobj(hDiff,'tag',['txt_diffdomain' filnr]);
                hDom     = findobj(hDiff,'tag',['diffdomain' filnr]);
                Domains  = get(hDomText,'userdata');
                if ~isempty(Domains) && ~strcmp(get(hDomText,'enable'),'on')
                    set(hDomText,'enable','on')
                    set(hDom, ...
                        'enable','on', ...
                        'backgroundcolor',Active, ...
                        'string',Domains, ...
                        'value',length(Domains))
                end
            end
        end
        d3d_qp difflabel
        
    case {'openfile1','openfile2'}
        hDiff = findall(0,'tag','Diff Files');
        if length(hDiff)~=1, return, end
        hDiffFile1 = findobj(hDiff,'tag','difffile1');
        hDiffFile2 = findobj(hDiff,'tag','difffile2');
        Handle_FileI = findobj(hDiff,'tag',['difffile' cmd(end)]);
        File=get(hDiffFile1,'userdata');
        Str=get(hDiffFile1,'string');
        %
        NrInList=get(Handle_FileI,'value');
        %
        if ~isempty(cmdargs)
            [NewRecord,FileName,Tp,Otherargs]=qp_proxy('open',cmdargs{:});
        else
            if ~isempty(File)
                pn = fileparts(File(NrInList).Name);
            else
                OpenFile=findobj(mfig,'tag','openfile','type','uipushtool');
                pn=get(OpenFile,'userdata');
            end
            [NewRecord,FileName,Tp,Otherargs]=qp_proxy('opennew',pn);
        end
        %
        if ~isempty(NewRecord)
            if isempty(File)
                Str={abbrevfn(FileName,60)};
                NrInList=1;
                File=NewRecord;
            else
                FileNameList={File.Name};
                NrInList=find(strcmp(FileName,FileNameList));
                if isempty(NrInList)
                    NrInList=length(File)+1;
                    Str{NrInList}=abbrevfn(FileName,60);
                    File(NrInList)=NewRecord;
                end
            end
            set(hDiffFile1,'userdata',File);
            set([hDiffFile1 hDiffFile2],'string',Str,'enable','on','backgroundcolor',Active);
            d3d_qp(['difffile' cmd(end)],NrInList)
        end
        
    case 'difflabel'
        hDiff = findall(0,'tag','Diff Files');
        if length(hDiff)~=1, return, end
        hDiffFile1 = findobj(hDiff,'tag','difffile1');
        hDiffDom1 = findobj(hDiff,'tag','diffdomain1');
        File = get(hDiffFile1,'userdata');
        %
        NrInList1 = get(hDiffFile1,'value');
        hDiffFile2 = findobj(hDiff,'tag','difffile2');
        hDiffDom2 = findobj(hDiff,'tag','diffdomain2');
        %
        NrInList2 = get(hDiffFile2,'value');
        hDiffLabel = findobj(hDiff,'tag','difflabel');
        hDiffDefine = findobj(hDiff,'tag','diffdefine');
        %
        hAllDomains = findobj(hDiff,'tag','all_domains');
        multiDomains = get(hAllDomains,'value');
        domList1 = get(hDiffDom1,'userdata');
        if isempty(domList1)
            Dom1 = '';
        elseif multiDomains
            if length(domList1) == 1
                Dom1 = domList1{1};
            else
                Dom1 = domList1;
            end
        else
            Dom1 = domList1{get(hDiffDom1,'value')};
        end
        domList2 = get(hDiffDom2,'userdata');
        if isempty(domList2)
            Dom2 = '';
        elseif multiDomains
            if length(domList1) == 1
                Dom2 = domList2{1};
            else
                Dom2 = domList2;
            end
        else
            Dom2 = domList2{get(hDiffDom2,'value')};
        end
        compatible_domains = (ischar(Dom1) & ischar(Dom2)) | ...
            isequal(Dom1, Dom2);
        %
        if ~isempty(cmdargs)
            setappdata(hDiffLabel,'LabelMode','manual')
            setappdata(hDiffLabel,'Label',cmdargs{1})
            set(hDiffLabel,'String',cmdargs{1})
        end
        if ~compatible_domains
            if strcmp(get(hDiffLabel,'enable'),'on')
                set(hDiffLabel,'userdata',get(hDiffLabel,'string'))
            end
            set(hDiffLabel,'String','Incompatible Selection of Domains', ...
                'BackgroundColor',Inactive, ...
                'Enable','off')
            set(hDiffDefine,'Enable','off')
        elseif NrInList1 == NrInList2
            if strcmp(get(hDiffLabel,'enable'),'on')
                set(hDiffLabel,'userdata',get(hDiffLabel,'string'))
            end
            set(hDiffLabel,'String','Data File Equals Reference File', ...
                'BackgroundColor',Inactive, ...
                'Enable','off')
            set(hDiffDefine,'Enable','off')
        else
            if isequal(gcbo,hDiffLabel)
                % user edited the label
                Label = get(hDiffLabel,'string');
                LabelRef = getappdata(hDiffLabel,'Label');
                if ~isequal(Label,LabelRef)
                    setappdata(hDiffLabel,'LabelMode','manual')
                    setappdata(hDiffLabel,'Label',Label)
                end
            else
                if isequal(getappdata(hDiffLabel,'LabelMode'),'auto')
                    File1 = File(NrInList1).Name;
                    Sep1 = [0 sort([strfind(File1,'/') strfind(File1,'\')]) length(File1)+1];
                    File2 = File(NrInList2).Name;
                    Sep2 = [0 sort([strfind(File2,'/') strfind(File2,'\')]) length(File2)+1];
                    for i=1:min(length(Sep1),length(Sep2))-1
                        if ~isequal(File1(Sep1(i)+1:Sep1(i+1)-1),File2(Sep2(i)+1:Sep2(i+1)-1))
                            break
                        end
                    end
                    for j=1:min(length(Sep1),length(Sep2))-1
                        if ~isequal(File1(Sep1(end-j)+1:Sep1(end-j+1)-1),File2(Sep2(end-j)+1:Sep2(end-j+1)-1))
                            break
                        end
                    end
                    Pre = File1(1:Sep1(i));
                    Post = File1(Sep1(end-j+1):end);
                    Mid1 = File1(Sep1(i)+1:min(end,Sep1(end-j+1)));
                    Mid2 = File2(Sep2(i)+1:min(end,Sep2(end-j+1)));
                    if isempty(Mid1)
                        Mid1 = '.';
                    elseif isempty(Mid2)
                        Mid2 = '.';
                    end
                    if ~isempty(Post) && ~isempty(Pre)
                        Label = [Pre '[(' Mid1 ') - (' Mid2 ')]' Post];
                    elseif ~isempty(Pre)
                        Label = [Pre '[(' Mid1 ') - (' Mid2 ')]'];
                    elseif ~isempty(Post)
                        Label = ['[(' Mid1 ') - (' Mid2 ')]' Post];
                    else
                        Label = ['(' Mid1 ') - (' Mid2 ')'];
                    end
                    set(hDiffLabel,'String',Label)
                    setappdata(hDiffLabel,'Label',Label)
                else
                    Label = getappdata(hDiffLabel,'Label');
                    set(hDiffLabel,'String',Label)
                end
            end
            set(hDiffLabel, ...
                'BackgroundColor',Active, ...
                'Enable','on')
            set(hDiffDefine,'Enable','on')
        end
        
    case {'difffile1','difffile2'}
        filnr = cmd(end);
        hDiff = findall(0,'tag','Diff Files');
        if length(hDiff)~=1, return, end
        hDiffFile1 = findobj(hDiff,'tag','difffile1');
        File=get(hDiffFile1,'userdata');
        %
        Handle_SelectFile=findobj(hDiff,'tag',cmd);
        %
        if nargin==2
            NrInList = cmdargs{1};
            if ischar(NrInList)
                if isstruct(File)
                    Names = {File.Name}';
                else
                    Names = {};
                end
                NrInList = ustrcmpi(cmdargs{1},Names);
                if NrInList<0
                    d3d_qp(['openfile' cmd(end)],cmdargs{1})
                    return
                else
                    set(Handle_SelectFile,'value',NrInList)
                end
            elseif isequal(size(NrInList),[1 1]) && NrInList<=length(File) && NrInList>=1 && NrInList==round(NrInList)
                set(Handle_SelectFile,'value',NrInList)
            else
                NrInList=get(Handle_SelectFile,'value');
            end
        else
            NrInList=get(Handle_SelectFile,'value');
        end
        %
        [Success,Domains] = qp_getdata(File(NrInList),'domains');
        hDomSelText = findobj(hDiff,'tag','txt_domain');
        hDomSelAll  = findobj(hDiff,'tag','all_domains');
        hDomSelOne  = findobj(hDiff,'tag','one_domain');
        hDomText    = findobj(hDiff,'tag',['txt_diffdomain' filnr]);
        filnr2 = char(sum('12')-filnr);
        hDomText2   = findobj(hDiff,'tag',['txt_diffdomain' filnr2]);
        hDom = findobj(hDiff,'tag',['diffdomain' filnr]);
        if ~Success || isempty(Domains)
            set(hDomText,'enable','off', ...
                'userdata',{})
            set(hDom,'enable','off', ...
                'value',1, ...
                'string','-- none defined --', ...
                'backgroundcolor',Inactive, ...
                'userdata',{})
            if isempty(get(hDomText2,'userdata'))
                set([hDomSelText,hDomSelAll,hDomSelOne],'enable','off')
            end
        else
            set(hDomText,'userdata',Domains)
            set([hDomSelText,hDomSelAll,hDomSelOne],'enable','on')
            if get(hDomSelAll,'value')
                set(hDom,'enable','off', ...
                    'value',1, ...
                    'string','-- all domains --', ...
                    'backgroundcolor',Inactive)
            else
                set(hDomText,'enable','on')
                set(hDom,'enable','on', ...
                    'value',1, ...
                    'string',Domains, ...
                    'backgroundcolor',Active)
                set(hDom,'value',length(Domains))
            end
        end
        switch filnr
            case '1'
                SelA = 'Select data file A';
            case '2'
                SelA = 'Select reference data file B';
        end
        set(Handle_SelectFile, ...
            'tooltip', [SelA char(10) ...
            ' ' char(10) ...
            'Currently selected:' char(10) ...
            File(NrInList).Name char(10) ...
            'Type: ' File(NrInList).FileType])
        d3d_qp difflabel
        
    case {'diffdomain1','diffdomain2'}
        % nothing to do yet
        
    case 'diffcancel'
        hDiff = findall(0,'tag','Diff Files');
        if length(hDiff)~=1, return, end
        set(hDiff,'visible','off')
        
    case 'diffdefine'
        hDiff = findall(0,'tag','Diff Files');
        if length(hDiff)~=1, return, end
        hDiffFile1 = findobj(hDiff,'tag','difffile1');
        NrInList1 = get(hDiffFile1,'value');
        hDiffFile2 = findobj(hDiff,'tag','difffile2');
        NrInList2 = get(hDiffFile2,'value');
        set(hDiff,'userdata',[NrInList1 NrInList2]);
        set(hDiff,'visible','off')
        
    case {'closefile','closeallfiles'}
        Handle_SelectFile=findobj(mfig,'tag','selectfile');
        File=get(Handle_SelectFile,'userdata');
        Str=get(Handle_SelectFile,'string');
        NrInList=get(Handle_SelectFile,'value');
        if strcmp(cmd,'closeallfiles')
            for i = 1:length(File)
                try
                    qp_proxy('clear',File(i))
                end
            end
            File(:)=[];
            Str(:)=[];
        elseif ~isempty(File)
            try
                qp_proxy('clear',File(NrInList))
            end
            File(NrInList)=[];
            Str(NrInList)=[];
        end
        if isempty(File)
            set(Handle_SelectFile,'enable','off','backgroundcolor',Inactive);
            Handle_ReloadFile=findobj(mfig,'tag','reloadfile');
            set(Handle_ReloadFile,'enable','off');
            Handle_CloseFile=findobj(mfig,'tag','closefile');
            set(Handle_CloseFile,'enable','off');
            Handle_CloseAllFiles=findobj(mfig,'tag','closeallfiles');
            set(Handle_CloseAllFiles,'enable','off');
            set(findobj(mfig,'tag','fileinfo'),'enable','off');
            Str=' ';
            NrInList=1;
        else
            NrInList=min(NrInList,length(File));
        end
        set(Handle_SelectFile,'userdata',File,'string',Str,'value',NrInList);
        d3d_qp selectfile*
        if logfile
            writelog(logfile,logtype,cmd);
        end
        
    case 'selectfile'
        Handle_SelectFile=findobj(mfig,'tag','selectfile');
        File=get(Handle_SelectFile,'userdata');
        if ~isempty(File)
            if isempty(cmdargs)
                NrInList=get(Handle_SelectFile,'value');
            else
                Files={File.Name};
                NrInList=ustrcmpi(cmdargs{1},Files);
                if NrInList<0
                    error('Cannot select file: %s',cmdargs{1})
                else
                    set(Handle_SelectFile,'value',NrInList);
                end
            end
            if logfile
                writelog(logfile,logtype,cmd,File(NrInList).Name);
            end
        end
        d3d_qp selectfile*
        
    case 'selectfile*'
        Handle_SelectFile=findobj(mfig,'tag','selectfile');
        
        Handle_FO=findobj(mfig,'tag','fileoptions');
        
        File=get(Handle_SelectFile,'userdata');
        OptionsControls=setdiff(allchild(UD.FilOpt.Fig),UD.FilOpt.Close);
        if isempty(File)
            
            set(Handle_FO,'enable','off','state','off')
            delete(OptionsControls)
            set(UD.FilOpt.Fig,'visible','off')
            
        else
            NrInList=get(Handle_SelectFile,'value');
            
            delete(OptionsControls)
            if File(NrInList).Options
                set(Handle_FO,'enable','on')
                Chk=qp_getdata(File(NrInList),'options',UD.FilOpt.Fig,'initialize');
            else
                set(Handle_FO,'enable','off','state','off')
                set(UD.FilOpt.Fig,'visible','off')
            end
            %
            Handle_FileOpt=findobj(mfig,'tag','fileoptions');
            if strcmp(File(NrInList).FileType,'<user defined variables>')
                set(Handle_FileOpt,'tooltip','Perform computations on variables')
                %
                set(Handle_SelectFile,'tooltip', ...
                    ['Select a data file for plotting' char(10) ...
                    ' ' char(10) ...
                    'Currently selected:' char(10) ...
                    'user defined variables'])
            else
                set(Handle_FileOpt,'tooltip','Set file dependent options')
                %
                set(Handle_SelectFile,'tooltip', ...
                    ['Select a data file for plotting' char(10) ...
                    ' ' char(10) ...
                    'Currently selected:' char(10) ...
                    File(NrInList).Name char(10) ...
                    'Type: ' File(NrInList).FileType])
            end
            
            OpenFile=findobj(mfig,'tag','openfile','type','uipushtool');
            Handle_ReloadFile=findobj(mfig,'tag','reloadfile');
            Handle_CloseFile=findobj(mfig,'tag','closefile');
            switch File(NrInList).FileType
                case '<user defined variables>'
                    set(Handle_ReloadFile,'enable','off');
                    set(Handle_CloseFile,'enable','off');
                case 'diff'
                    set(Handle_ReloadFile,'enable','on');
                    set(Handle_CloseFile,'enable','on');
                otherwise
                    pn = fileparts(File(NrInList).Name);
                    set(OpenFile,'userdata',pn);
                    set(Handle_ReloadFile,'enable','on');
                    set(Handle_CloseFile,'enable','on');
            end
        end
        d3d_qp updatedomains
        
    case 'fileinfo'
        Fil=qpfile;
        Data=qp_unwrapfi(Fil);
        ui_inspectstruct(Data,Fil.Name)
        
    case 'fileoptions'
        if nargin==1
            if strcmp(get(UD.FilOpt.Fig,'visible'),'on')
                set(UD.FilOpt.Fig,'visible','off')
                set(UD.MainWin.FlOps,'state','off')
            else
                set(UD.FilOpt.Fig,'visible','on')
                set(UD.MainWin.FlOps,'state','on')
            end
        else
            Handle_SelectFile=findobj(mfig,'tag','selectfile');
            File=get(Handle_SelectFile,'userdata');
            NrInList=get(Handle_SelectFile,'value');
            [Chk,NewFileInfo,cmdargs]=qp_getdata(File(NrInList),'options',UD.FilOpt.Fig,varargin{:});
            if Chk && ~isequal(NewFileInfo,[]),
                File(NrInList)=NewFileInfo;
                set(Handle_SelectFile,'userdata',File,'value',NrInList);
                d3d_qp updatedomains
            end
            if ~isempty(cmdargs)
                if logfile
                    writelog(logfile,logtype,cmd,cmdargs{:});
                end
            end
        end
        
    case 'updatedomains'
        Handle_SelectFile=findobj(mfig,'tag','selectfile');
        File=get(Handle_SelectFile,'userdata');
        NrInList=get(Handle_SelectFile,'value');
        Succes=~isempty(File);
        
        domaintxt=findobj(mfig,'tag','domain');
        domains=findobj(mfig,'tag','selectdomain');
        if ~Succes
            set(domains,'string',' ','value',1,'enable','off','backgroundcolor',Inactive);
            set(domaintxt,'enable','off','string','Domain');
        else
            Info=File(NrInList);
            [Chk,domainstr]=qp_getdata(Info,'domainname');
            [Chk,Domains]=qp_getdata(Info,'domains');
            if ~Chk || isempty(Domains)
                set(domains,'string',' ','value',1,'enable','off','backgroundcolor',Inactive);
                set(domaintxt,'enable','off','string',domainstr);
            else
                dm=length(Domains);
                if strcmp(get(domains,'enable'),'on')
                    pdomains=get(domains,'string');
                    dm=get(domains,'value');
                    pdomain=pdomains{dm};
                    dm=ustrcmpi(pdomain,Domains); % first check equality and longer names
                    if dm<0 % no matches
                        dm=ustrcmpi(Domains,pdomain); % check (equality and) shorter names
                        if dm<0 % still no matches
                            dm=length(Domains);
                        end
                    end
                end
                set(domains,'string',Domains,'value',dm,'enable','on','backgroundcolor',Active);
                set(domaintxt,'enable','on','string',domainstr);
            end
        end
        d3d_qp updatedatafields
        
    case 'selecteddomain'
        domains = findobj(mfig,'tag','selectdomain');
        Domains = get(domains,'string');
        i = get(domains,'value');
        outdata = {i, Domains{i}};
        
    case 'selectdomain'
        domains = findobj(mfig,'tag','selectdomain');
        Domains = get(domains,'string');
        if ~isempty(cmdargs)
            if ischar(cmdargs{1})
                i = ustrcmpi(cmdargs{1},Domains);
            elseif isnumeric(cmdargs{1})
                i = cmdargs{1};
                cmdargs{1} = sprintf('%g',i); % only for error handling
            end
            if i < 0 || i > length(Domains)
                error('Cannot select field: %s',cmdargs{1})
            else
                set(domains,'value',i);
            end
        end
        d3d_qp updatedatafields
        if logfile
            writelog(logfile,logtype,cmd,Domains{get(domains,'value')});
        end
        
    case 'updatedatafields'
        set(mfig,'pointer','watch')
        Props=[];
        lasterr('');
        Handle_SelectFile=findobj(mfig,'tag','selectfile');
        File=get(Handle_SelectFile,'userdata');
        NrInList=get(Handle_SelectFile,'value');
        Succes=~isempty(File);
        if Succes
            Info=File(NrInList);
        end
        
        datafields=findobj(mfig,'tag','selectfield');
        if ~Succes
            set(datafields,'string',' ','value',1,'enable','off','backgroundcolor',Inactive,'userdata',Props);
        else
            Handle_Domain=findobj(mfig,'tag','selectdomain');
            DomainNr=get(Handle_Domain,'value');
            %
            [Chk,Props]=qp_getdata(Info,DomainNr);
            if ~Chk
                set(datafields, ...
                    'string',sprintf('<%s not supported>',qp_gettype(Info)), ...
                    'value',1, ...
                    'enable','off', ...
                    'backgroundcolor',Inactive, ...
                    'userdata',Props);
            else
                names={Props.Name};
                if isempty(names),
                    set(datafields,'string','<no datafields found>','value',1,'enable','off','backgroundcolor',Inactive,'userdata',Props);
                else
                    df=1;
                    if strcmp(get(datafields,'enable'),'on')
                        pnames=get(datafields,'string');
                        df=get(datafields,'value');
                        pname=pnames{df};
                        df=ustrcmpi(pname,names); % first check equality and longer names
                        if df<0, % no matches
                            df=ustrcmpi(names,pname); % check (equality and) shorter names
                            if df<0, % still no matches
                                df=1;
                            end
                        end
                    end
                    set(datafields,'string',names,'value',df,'enable','on','backgroundcolor',Active,'userdata',Props);
                end
            end
        end
        d3d_qp updatefieldprop
        set(mfig,'pointer','arrow')
        
    case 'setasparametersource'
        Handle_SelectFile=findobj(mfig,'tag','selectfile');
        File = get(Handle_SelectFile,'userdata');
        Fig  = qpsf;
        if ~isempty(File) && ~isempty(Fig)
            NrInList = get(Handle_SelectFile,'value');
            Info     = File(NrInList);
            Handle_Domain   = findobj(mfig,'tag','selectdomain');
            DomainNr        = get(Handle_Domain,'value');
            [Chk,PAR]       = qp_getdata(Info,DomainNr,'getparams');
            %
            Expand.FileName = Info.Name;
            if ~isempty(Info.Otherargs)
                Expand.FileName = [{Info.Name} Info.Otherargs];
            end
            Expand.Domain   = [];
            if strcmp(get(Handle_Domain,'enable'),'on')
                Domains       = get(Handle_Domain,'string');
                Expand.Domain = Domains{DomainNr};
            end
            Expand.PAR      = PAR;
            setappdata(Fig,'ExpandPAR',Expand)
            if logfile
                writelog(logfile,logtype,cmd);
            end
        end
        
    case 'selectedfield'
        sf   = findobj(mfig,'tag','selectfield');
        ifld = get(sf,'value');
        ud   = get(sf,'userdata');
        outdata = {ud(ifld)};
        
    case {'selectfield','selectsubfield'}
        sf=findobj(mfig,'tag',cmd);
        flds=get(sf,'string');
        found=1;
        if ~isempty(cmdargs)
            i=ustrcmpi(cmdargs{1},flds,4); % only allow longer names to match
            if i<0
                found=0;
                if nargout==0
                    error('Cannot select %s: %s',cmd(7:end),cmdargs{1})
                end
            else
                set(sf,'value',i);
            end
        end
        d3d_qp updatefieldprop
        if logfile
            writelog(logfile,logtype,cmd,flds{get(sf,'value')});
        end
        if nargout>0
            outdata = {found};
        end
        
    case 'updatetimezone'
        TZhandling=qp_settings('timezone');
        MW = UD.MainWin;
        %
        File=get(MW.File,'userdata');
        NrInList=get(MW.File,'value');
        if ~isempty(File)
            Info=File(NrInList);
            DomainNr=get(MW.DList,'value');
            Props=get(MW.Field,'userdata');
            fld=get(MW.Field,'value');
        end
        %
        if strcmpi(TZhandling,'ignored')
            set(MW.TZdata,'userdata',NaN,'visible','off')
            set(MW.TZtxt,'visible','off')
        else
            if ~isempty(File) && ...
                    ~isempty(Props) && ...
                    ~strncmp('---',Props(fld).Name,3) && ...
                    isfield(Props,'DimFlag') && ...
                    (Props(fld).DimFlag(T_)==1 || Props(fld).DimFlag(T_)==2)
                [Chk,TZshift,TZstr]=qp_getdata(Info,DomainNr,Props(fld),'timezone');
                if isnan(TZshift)
                    TZstr = 'unknown';
                elseif ~strcmpi(TZhandling,'as in dataset')
                    [TZshift,TZstr] = gettimezone(TZhandling);
                end
            else
                TZstr   = 'N/A';
                TZshift = NaN;
            end
            if isempty(TZstr)
                if TZshift>0
                    TZstr = sprintf('UTC+%i',TZshift);
                elseif TZshift<0
                    TZstr = sprintf('UTC%i',TZshift);
                else
                    TZstr = 'UTC';
                end
            end
            if isnan(TZshift)
                set(MW.TZdata,'string',TZstr,'enable','off','userdata',NaN,'visible','on')
                set(MW.TZtxt,'enable','off','visible','on')
            else
                set(MW.TZdata,'string',TZstr,'enable','on','userdata',TZshift,'visible','on')
                set(MW.TZtxt,'enable','on','visible','on')
            end
        end
        if get(MW.TList,'userdata')
            Times   = getappdata(MW.TList,'original_times');
            TZshift = getappdata(MW.TList,'original_tzshift');
            tzreq = get(findobj(mfig,'tag','datatimezone'),'userdata');
            if ~isnan(TZshift) && ~isnan(tzreq) && ...
                    (Props(fld).DimFlag(T_)==1 || Props(fld).DimFlag(T_)==2)
                Times = Times + (tzreq-TZshift)/24;
            end
            Str = qp_time2str(Times,Props(fld).DimFlag(T_));
            set(MW.TList,'string',Str)
        end
        qp_interface_update_options(mfig,UD);
        
    case 'updatefieldprop'
        qp_updatefieldprop(UD);
        d3d_qp('gridview_update')
        
    case 'showtimeswitch'
        UDSts=findobj(mfig,'tag','showtimes');
        if ~isempty(cmdargs)
            Sts=cmdargs{1};
            if ~isequal(Sts,0) && ~isequal(Sts,1)
                error('Invalid argument specified for %s.',cmd)
            end
            set(UDSts,'value',Sts);
        end
        d3d_qp showtimes
        if logfile
            writelog(logfile,logtype,cmd,get(UDSts,'value'));
        end
        
    case 'showtimes'
        st=UD.MainWin.ShowT; %findobj(mfig,'tag','showtimes')
        tl=UD.MainWin.TList; %findobj(mfig,'tag','timelist')
        if get(st,'value') && strcmp(get(st,'enable'),'on')
            if ~get(tl,'userdata')
                datafields=findobj(mfig,'tag','selectfield');
                fld=get(datafields,'value');
                Props=get(datafields,'userdata');
                
                Handle_SelectFile=findobj(mfig,'tag','selectfile');
                File=get(Handle_SelectFile,'userdata');
                NrInList=get(Handle_SelectFile,'value');
                Succes=~isempty(File);
                if Succes
                    Info=File(NrInList);
                end
                
                Handle_Domain=findobj(mfig,'tag','selectdomain');
                DomainNr=get(Handle_Domain,'value');
                
                set(mfig,'pointer','watch')
                [Chk,TZshift,TZstr]=qp_getdata(Info,DomainNr,Props(fld),'timezone');
                [Chk,Times]=qp_getdata(Info,DomainNr,Props(fld),'times');
                set(mfig,'pointer','arrow')
                if size(Times,1)==1
                    Times=Times';
                end
                setappdata(tl,'original_times',Times)
                setappdata(tl,'original_tzshift',TZshift)
                
                tzreq = get(findobj(mfig,'tag','datatimezone'),'userdata');
                if ~isnan(TZshift) && ~isnan(tzreq) && ...
                        (Props(fld).DimFlag(T_)==1 || Props(fld).DimFlag(T_)==2)
                    Times = Times + (tzreq-TZshift)/24;
                end
                Str = qp_time2str(Times,Props(fld).DimFlag(T_));
                set(tl,'string',Str,'userdata',1)
            end
            id=get(findobj(mfig,'tag','t=?'),'userdata');
            set(tl,'value',id,'backgroundcolor',Active,'enable','on')
        else
            set(tl,'value',[],'backgroundcolor',Inactive,'enable','off')
        end
        
    case 'station'
        sts=get(UD.MainWin.StList,'string');
        if ~isempty(cmdargs)
            if isnumeric(cmdargs{1}) && isequal(size(cmdargs{1}),[1 1])
                i = cmdargs{1};
                nsts = max(size(sts,1),length(sts));
                if i<1 || i>nsts
                    ui_message('warning', ...
                        'Requested station number %i bigger than number of stations %i.',i,nsts);
                    i = get(UD.MainWin.StList,'value');
                else
                    i = cmdargs{1};
                end
            else
                stn=deblank(cmdargs{1});
                [i,iall]=ustrcmpi(stn,sts);
            end
            if i<0
                if length(iall)>1
                    ui_message('warning', ...
                        'Multiple station names match ''%s'', selecting first',stn);
                    set(UD.MainWin.StList,'value',iall(1));
                else
                    error('Cannot select station: %s',cmdargs{1})
                end
            else
                set(UD.MainWin.StList,'value',i);
            end
        end
        pst=get(UD.MainWin.StList,'value');
        set(UD.MainWin.EditS,'userdata',pst,'string',sprintf('%i',pst))
        statud=get(UD.MainWin.AllS,'userdata');
        statud{2}=pst;
        set(UD.MainWin.AllS,'userdata',statud)
        if logfile
            if iscell(sts)
                stn=sts{pst};
            else
                stn=sts(pst,:);
            end
            writelog(logfile,logtype,cmd,stn);
        end
        
    case 'timelist'
        pUDM=get(UD.MainWin.AllT,'userdata');
        id=get(UD.MainWin.TList,'value');
        set(UD.MainWin.EditT,'string',vec2str(id,'noones','nobrackets'),'userdata',id);
        pUDM{2}=id;
        set(UD.MainWin.AllT,'userdata',pUDM);
        d3d_qp updateoptions
        % do as if the values were entered in the editt edit box
        if logfile
            writelog(logfile,logtype,'editt',id);
        end
        
    case {'allm','alln','allk','allt','alls','allm*','alln*','allk*','allt*','alls*', ...
            'alldim6','alldim7','alldim8','alldim9','alldim10', ...
            'alldim6*','alldim7*','alldim8*','alldim9*','alldim10*'}
        M=upper(cmd(4:end));
        if M(end)=='*'
            M = M(1:end-1);
        end
        MW=UD.MainWin;
        UDAllM=getfield(MW,strcat('All',M));
        UDEditM=getfield(MW,strcat('Edit',M));
        pUDM=get(UDAllM,'userdata');
        %
        if strcmp(M,'M') || strcmp(M,'N')
            if ~strcmp(getvalstr(MW.HSelType),'M range and N range')
                d3d_qp('m,n*')
            end
        end
        %
        if isempty(cmdargs)
            SwitchToAll=get(UDAllM,'value');
        else
            SwitchToAll=cmdargs{1};
            if isequal(SwitchToAll,'0') || isequal(SwitchToAll,'1')
                SwitchToAll = str2num(SwitchToAll);
            elseif ~isequal(SwitchToAll,0) && ~isequal(SwitchToAll,1)
                error('Invalid argument specified for %s.',cmd)
            end
            set(UDAllM,'value',SwitchToAll);
        end
        if SwitchToAll
            pUDM{1}=1;
            set(UDEditM,'enable','off','backgroundcolor',Inactive)
            if M=='S'
                set(findobj(mfig,'tag','stationlist'),'enable','off','backgroundcolor',Inactive)
            end
        else
            pUDM{1}=0;
            set(UDEditM,'enable','on','backgroundcolor',Active)
            if strcmp(M,'S')
                set(findobj(mfig,'tag','stationlist'),'enable','on','backgroundcolor',Active)
            elseif strcmp(M,'T')
                set(findobj(mfig,'tag','quickview'),'string','Quick View');
            end
        end
        set(UDAllM,'userdata',pUDM);
        %----------
        if strcmp(M,'T')
            if SwitchToAll
                set(MW.ShowT,'enable','off')
                d3d_qp showtimes
            else
                UDMaxM=getfield(MW,strcat('Max',M));
                maxm=get(UDMaxM,'userdata');
                if maxm<=30000 % enable timelist only if there are less than 30000 times
                    set(MW.ShowT,'enable','on')
                    d3d_qp showtimes
                end
            end
        end
        d3d_qp updateoptions
        if logfile
            writelog(logfile,logtype,cmd(1:4),SwitchToAll);
        end
        
        if ~strcmp(cmd(end),'*') && (strcmp(M,'M') || strcmp(M,'N'))
            d3d_qp('gridview_update');
        end
        
    case {'editm','editn','editk','editt','edits','editm*','editn*','editk*','editt*','edits*', ...
            'editdim6','editdim7','editdim8','editdim9','editdim10', ...
            'editdim6*','editdim7*','editdim8*','editdim9*','editdim10*'}
        M=upper(cmd(5:end));
        if M(end)=='*'
            M = M(1:end-1);
        end
        if length(M)==1
            m_=find('TSMNK'==M);
        else
            m_=str2double(M(4:end));
        end
        %
        MW=UD.MainWin;
        UDAllM=MW.(['All' M]);
        UDEditM=MW.(['Edit' M]);
        UDMaxM=MW.(['Max' M]);
        maxm=get(UDMaxM,'userdata');
        pUDM=get(UDAllM,'userdata');
        
        fld=get(MW.Field,'value');
        Props=get(MW.Field,'userdata');
        if isempty(Props)
            return
        end
        DimFlag=Props(fld).DimFlag;
        
        try
            if isempty(cmdargs)
                mstr=get(UDEditM,'string');
            else
                mstr=cmdargs{1};
            end
            if ischar(mstr)
                if DimFlag(m_)==7
                    m=str2vec(mstr);
                else
                    m=str2vec(mstr,'range',[1 maxm],'applylimit');
                end
            else
                mstr(mstr>maxm)=[];
                m=mstr(:)';
            end
        catch
            lasterr('')
            m=1;
        end
        if isempty(m)
            m=1;
        end
        if ~strcmp(M,'T')
            if strcmp(M,'M') || strcmp(M,'N')
                if ~strcmp(getvalstr(MW.HSelType),'M range and N range')
                    d3d_qp('m,n*')
                end
            end
            %
            % often only subranges are acceptable: m1:m2, not m1:mi:m2 or [m1 m2 m4]
            %
            if ~isequal(m,m(1):m(end)) && DimFlag(m_)~=6 && DimFlag(m_)~=16
                m=m(1);
            end
            switch DimFlag(m_)
                case {1,11} % All, Range, Element
                    % nothing to do
                case {2,12} % All, Range
                    if isequal(size(m),[1 1])
                        if m==1
                            m=1:maxm;
                            
                        else
                            m=1:m;
                        end
                    end
                case {3,13} % All, Element
                    m=m(1);
                case {4,14,inf} % All
                    % cannot occur
                case {5,15} % Element
                    m=m(1);
                case {6,16} % Any list of elements
                    m=unique(m);
                case {7} % Element from list
                    labels = getappdata(UDEditM,'dimlabels');
                    m = m(ismember(m,labels));
                    if isempty(m)
                        m = get(UDEditM,'userdata');
                    end
            end
            %
        else
            m=unique(m);
            st=findobj(mfig,'tag','showtimes');
            tl=findobj(mfig,'tag','timelist');
            if get(MW.ShowT,'value') && strcmp(get(MW.ShowT,'enable'),'on')
                set(MW.TList,'value',m);
            end
        end
        set(UDEditM,'string',vec2str(m,'nobrackets','noones'),'userdata',m);
        pUDM{2}=m;
        set(UDAllM,'userdata',pUDM);
        d3d_qp updateoptions
        if logfile
            writelog(logfile,logtype,cmd(1:5),m);
        end
        
        if ~strcmp(cmd(end),'*') && (strcmp(M,'M') || strcmp(M,'N'))
            d3d_qp('gridview_update');
        end
        
    case 'savexy'
        MW=UD.MainWin;
        filtertbl = qp_filefilters('files-with-lines');
        [fn,fp,tp] = uiputfile(filtertbl(:,1:2),'Save as');
        if ischar(fn)
            fn = [fp fn];
            xy = get(MW.EditXY,'userdata');
            switch filtertbl{tp,3}
                case '>tekal'
                    landboundary('write',fn,xy)
                case 'shape'
                    shape('write',fn,'polyline',{xy})
                case 'AutoCAD DXF'
                    F = figure('IntegerHandle','off','visible','off');
                    A = axes('parent',F,'visible','off');
                    line(xy(:,1),xy(:,2),'parent',A);
                    dxf('save',A,fn)
                    delete(F)
                case 'ArcInfoUngenerate'
                    ai_ungen('write',fn,xy)
                case 'BNA File'
                    bna('write',fn,xy)
                otherwise
                    ui_message('error','Saving X,Y path as %s is not yet supported.',filtertbl{tp,2}(1:end-1)) % just "... File" not "...Files"
            end
        end
        
    case 'convertmn2m'
        MW=UD.MainWin;
        MN = get(MW.EditMN,'userdata');
        d3d_qp('allm*',0)
        d3d_qp('editm*',MN')
        
    case 'convertmn2xy'
        XY = qp_gridview('convertmn2xy',UD.GridView.Fig);
        d3d_qp('editxy*',XY)
        
    case 'reversemn'
        MW=UD.MainWin;
        MN = get(MW.EditMN,'userdata');
        d3d_qp('editmn*',flipud(MN))
        
    case {'editmn*','editmn','editxy*','editxy','loadxy'}
        MW=UD.MainWin;
        isMN = isequal(cmd(5:6),'mn');
        if isMN
            UDEditMN=MW.EditMN;
            if ~strcmp(getvalstr(MW.HSelType),'(M,N) point/path')
                d3d_qp('(mn)*')
            end
        else
            UDEditMN=MW.EditXY;
            if ~strcmp(getvalstr(MW.HSelType),'(X,Y) point/path')
                d3d_qp('(xy)*')
            end
        end
        try
            if strcmp(cmd,'loadxy')
                if ~isempty(cmdargs)
                    xyfile = cmdargs(1);
                else
                    xyfile = {};
                end
                [FI,FileName,Tp,Otherargs]=qp_proxy('openldb',xyfile{:});
                if isempty(FI) % cancel pressed
                    return
                else
                    qnt = qpread(FI);
                    if ~ismember('line',{qnt.Name})
                        ui_message('error','This Tekal file is not supported as supplier of X,Y path.')
                        return
                    else
                        LDB=qpread(FI,'line','griddata');
                        mnstr = [LDB.X LDB.Y];
                        if any(isnan(mnstr(end,:)))
                            mnstr(end,:) = [];
                        end
                    end
                end
            elseif isempty(cmdargs)
                mnstr=get(UDEditMN,'string');
            else
                mnstr=cmdargs{1};
            end
            if ischar(mnstr)
                mn=str2num(mnstr);
                if isempty(mn)
                    if size(mnstr,1)>1
                        mnstr=mnstr';
                        mnstr(end+1,:)=' ';
                        mnstr=mnstr(:)';
                    end
                    mn=sscanf(mnstr,'%g%*[,; ]');
                end
            else
                mn=mnstr;
            end
            switch get(UD.MainWin.N,'enable')
                case 'on'
                    if size(mn,1)==1 && size(mn,2)>2
                        L = length(mn);
                        if L~=2*round(L/2)
                            %ui_message('error','Even number of numbers expected: ignoring last value');
                            mn = mn(1:end-1);
                            L = length(mn);
                        end
                        mn = reshape(mn,[2 L/2])';
                    elseif size(mn,2)==1 && size(mn,1)>=1
                        L = length(mn);
                        if L~=2*round(L/2)
                            %ui_message('error','Even number of numbers expected: ignoring last value');
                            mn = mn(1:end-1);
                            L = length(mn);
                        end
                        mn = reshape(mn,[2 L/2])';
                    elseif size(mn,2)>2
                        %ui_message('error','Two columns expected: ignoring superfluous columns')
                        mn = mn(:,1:2);
                    end
                    dims=2;
                case 'off'
                    dims=1;
                    if isMN
                        mn=mn(:);
                    end
            end
        catch
            lasterr('')
            mn=[];
        end
        if ~isempty(mn)
            if isMN
                if any(mn(:)~=round(mn(:)))
                    mn=get(UDEditMN,'userdata');
                else
                    % clip all indices to range
                    if dims==2
                        % structured 2D domain
                        mmax = get(MW.MaxM,'userdata');
                        nmax = get(MW.MaxN,'userdata');
                        [mnexp,mn]=piecewise(mn,[mmax nmax]);
                    else
                        %unstructured domain
                        mmax = get(MW.MaxM,'userdata');
                        %
                        firstproblem = min(find(any(mn<1 | mn>mmax)));
                        if ~isempty(firstproblem)
                            mn = mn(1:firstproblem-1);
                        end
                    end
                end
            else
                mn(any(isnan(mn) | ~isfinite(mn),2),:)=NaN;
            end
        end
        set(UDEditMN,'userdata',mn)
        if isempty(mn)
            mnstr='';
            if ~isMN
                set(MW.SaveXY,'enable','off')
            end
        else
            if isMN
                if dims==2
                    mnstr=sprintf('%i, %i; ',mn');
                else
                    mnstr=sprintf('%i; ',mn');
                end
            else
                mnstr=cell(1,size(mn,1));
                for i=1:size(mn,1)
                    xf = sprintf('%%.%if',min(3,6-floor(log10(abs(mn(i,1))))));
                    yf = sprintf('%%.%if',min(3,6-floor(log10(abs(mn(i,2))))));
                    mnstr{i} = sprintf([xf,', ',yf,'; '],mn(i,:));
                end
                mnstr = cat(2,mnstr{:});
                set(MW.SaveXY,'enable','on')
            end
            mnstr(end-1:end)=[];
        end
        set(UDEditMN,'string',mnstr)
        d3d_qp updateoptions
        if logfile
            wrcmd=cmd;
            if cmd(end)=='*'
                wrcmd=cmd(1:end-1);
            end
            if strcmp(wrcmd,'loadxy')
                writelog(logfile,logtype,wrcmd,FI.Name);
            else
                writelog(logfile,logtype,wrcmd,mnstr);
            end
        end
        
        if cmd(end)~='*'
            d3d_qp('gridview_update');
        end
        
    case {'editz'}
        if isempty(cmdargs)
            z=str2num(get(UD.MainWin.EditZ,'string'));
        else
            z=cmdargs{1};
        end
        if isempty(z)
            z=[];
        elseif ~isequal(size(z),[1 1])
            z=z(1);
            switch getvalstr(UD.MainWin.VSelType)
                case {'dZ below surface','dZ above bed'}
                    z = max(0,z);
                case 'depth percentage'
                    z = min(max(0,z),100);
            end
        end
        set(UD.MainWin.EditZ,'string',sprintf('%g',z),'userdata',z)
        d3d_qp updateoptions
        if logfile
            writelog(logfile,logtype,cmd,z);
        end
        
    case {'selected','defvariable','loaddata','quickview','updateoptions','exportdata','addtoplot','addtoplot_left','addtoplot_right'}
        Handle_SelectFile=findobj(mfig,'tag','selectfile');
        File=get(Handle_SelectFile,'userdata');
        NrInList=get(Handle_SelectFile,'value');
        Succes=~isempty(File);
        Info=[];
        if strcmp(cmd,'addtoplot') && isempty(cmdargs)
            Parent=UD.PlotMngr.CurrentAxes;
            if ishandle(Parent)
                lat = getappdata(Parent,'linkedaxestype');
                if ~isempty(lat)
                    if strcmp(lat,'SecondY')
                        pos=get(gcbf,'position');
                        set(UD.MainWin.Add2PlotMenu,'position',get(0,'pointerlocation')-pos(1:2),'visible','on')
                        return
                    end
                end
            end
        end
        if Succes
            Info=File(NrInList);
            [DomainNr,Props,subf,selected,stats,Ops]=qp_interface_update_options(mfig,UD);
            if ~strcmp(cmd,'updateoptions') && iscell(selected{K_}) && isempty(selected{K_}{2})
                switch selected{K_}{1}
                    case {'z'}
                        error('No horizontal slice level Z specified!')
                    case {'dz_below_max', 'dz_above_min'}
                        error('No horizontal slice level dZ specified!')
                    case 'percentage depth'
                        error('No depth percentage for horizontal slice specified!')
                end
            end
            if isempty(Ops)
                cmd='error';
            end
        else
            cmd='error';
        end
        
        switch cmd
            case 'error'
                
            case 'selected'
                outdata.Domain = DomainNr;
                outdata.Quantity = Props;
                outdata.Subfield = subf;
                outdata.Time = selected{1};
                outdata.Station = selected{2};
                outdata.M = selected{3};
                outdata.N = selected{4};
                outdata.K = selected{5};
                outdata = {outdata};
                return
            
            case 'defvariable'
                Handle_SelectFile=findobj(mfig,'tag','selectfile');
                File=get(Handle_SelectFile,'userdata');
                sel=get(Handle_SelectFile,'value');
                Str=get(Handle_SelectFile,'string');
                UDV='<user defined variables>';
                FileName=UDV;
                FileNameList={File.Name};
                NrInList=strmatch(FileName,FileNameList,'exact');
                if ~isempty(NrInList) && ~isempty(File(NrInList).Data)
                    Vars={File(NrInList).Data.Name};
                else
                    Vars={};
                end
                if isempty(cmdargs)
                    VarName=Props.Name;
                    accept=0;
                else
                    VarName=cmdargs{1};
                    accept=isempty(strmatch(VarName,Vars,'exact'));
                end
                prompt={'Name of Variable:'};
                
                def={VarName};
                dlgTitle='Specify Unique Name of Variable';
                lineNo=1;
                if ~isempty(strmatch(VarName,Vars,'exact'))
                    prompt={'Name of Variable (the Current Name is Not Unique):'};
                end
                while ~accept
                    answer=stdinputdlg(prompt,dlgTitle,lineNo,def);
                    if isempty(answer)
                        break
                    end
                    VarName=answer{1};
                    accept=isempty(strmatch(VarName,Vars,'exact'));
                end
                if accept
                    if isempty(NrInList)
                        NrInList=length(File)+1;
                        File(NrInList).Data=[];
                    end
                    Str{NrInList}=abbrevfn(FileName,60);
                    File(NrInList).QPF=1;
                    File(NrInList).Name=FileName;
                    if isempty(File(NrInList).Data)
                        i=1;
                    else
                        i=length(File(NrInList).Data)+1;
                    end
                    File(NrInList).Data(i).Name=VarName;
                    if isfield(Props,'Units')
                        File(NrInList).Data(i).Units=Props.Units;
                    else
                        File(NrInList).Data(i).Units='';
                    end
                    if isfield(Props,'Geom')
                        File(NrInList).Data(i).Geom=Props.Geom;
                    else
                        File(NrInList).Data(i).Geom='';
                    end
                    if isfield(Props,'Coords')
                        File(NrInList).Data(i).Coords=Props.Coords;
                    else
                        File(NrInList).Data(i).Coords='';
                    end
                    File(NrInList).Data(i).FileInfo=Info;
                    File(NrInList).Data(i).Domain=DomainNr;
                    File(NrInList).Data(i).Props=Props;
                    File(NrInList).Data(i).Selected=selected;
                    File(NrInList).Data(i).SubField=subf;
                    File(NrInList).Data(i).DimFlag=Props.DimFlag;
                    if iscell(selected{3})
                        File(NrInList).Data(i).DimFlag(3) = inf;
                        File(NrInList).Data(i).DimFlag(4) = 0;
                    elseif iscell(selected{5})
                        File(NrInList).Data(i).DimFlag(5) = 0;
                    end
                    File(NrInList).Data(i).DataInCell=0;
                    if isfield(Props,'DataInCell')
                        File(NrInList).Data(i).DataInCell=Props.DataInCell;
                    end
                    if isfield(Props,'ClosedPoly')
                        File(NrInList).Data(i).ClosedPoly=Props.ClosedPoly;
                    else
                        File(NrInList).Data(i).ClosedPoly=0;
                    end
                    File(NrInList).Data(i).Tri=0;
                    if isfield(Props,'Geom') && isequal(Props.Geom,'TRI')
                        File(NrInList).Data(i).Tri=1;
                    elseif isfield(Props,'Tri')
                        File(NrInList).Data(i).Tri=Props.Tri;
                    end
                    File(NrInList).FileType='<user defined variables>';
                    File(NrInList).Options=1;
                    set(Handle_SelectFile,'userdata',File,'string',Str,'enable','on','backgroundcolor',Active);
                    if isequal(Str{sel},UDV)
                        d3d_qp selectfile*
                    end
                    if logfile
                        writelog(logfile,logtype,cmd,VarName);
                    end
                end
                
            case 'exportdata'
                lasterr('');
                try
                    set(mfig,'pointer','watch')
                    DS.FI=Info;
                    DS.Domain=DomainNr;
                    DS.Props=Props;
                    DS.SubField=subf;
                    DS.Selected=selected;
                    DS.Ops=Ops;
                    cmdargs=qp_export(getappdata(findobj(UD.Options.Handles,'tag','exportdata'),'exporttype'),DS,cmdargs);
                    set(mfig,'pointer','arrow')
                catch Ex
                    cmdargs={};
                    set(mfig,'pointer','arrow')
                    qp_error('Catch in d3d_qp\exportdata',Ex)
                end
                if ~isempty(cmdargs) && logfile
                    writelog(logfile,logtype,cmd,cmdargs{:});
                end
                
            case {'quickview','addtoplot','addtoplot_left','addtoplot_right','loaddata'}
                set(mfig,'pointer','watch')
                % minimal addpath d:\src\trunk_os\src\tools_lgpl\matlab\quickplot\progsrc\drawnow needed for updating pointer
                if strcmp(cmd,'loaddata')
                    % no drawnow
                elseif matlabversionnumber>=7.06 % 'update' option available as of 2008a
                    drawnow('update')
                else
                    drawnow
                end
                try
                    if strcmp(cmd,'addtoplot_left')
                        cmdargs = {'left'};
                        cmd = 'addtoplot';
                    elseif strcmp(cmd,'addtoplot_right')
                        cmdargs = {'right'};
                        cmd = 'addtoplot';
                    end
                    if logfile
                        writelog(logfile,logtype,cmd,cmdargs{:});
                    end
                    hNew=[];
                    %
                    % animate time dimension when appropriate ...
                    %
                    T=1;
                    Animate = getappdata(findobj(mfig,'tag','quickview'),'animate');
                    if strcmp(cmd,'loaddata')
                        % keep all selected times
                    elseif Animate && Props.DimFlag(T_)
                        if selected{T_}==0
                            maxt=get(findobj(mfig,'tag','max_t'),'userdata');
                            T=1:maxt;
                        else
                            T=selected{T_};
                        end
                        selected{T_}=T(1);
                    end
                    
                    if strcmp(cmd,'loaddata') || isequal(Props.NVal, -2)
                        % selfplotfig will create its own figure
                        Parent=0;
                        pfig=[];
                    elseif strcmp(cmd,'quickview')
                        pfig=qp_createfig('quick','');
                        ax=findall(pfig,'type','axes');
                        for i=length(ax):-1:1
                            if strcmp(get(ax(i),'tag'),'scribeOverlay')
                                ax(i)=[];
                            elseif isappdata(ax(i),'NonDataObject')
                                ax(i)=[];
                            elseif isappdata(ax(i),'AxesType')
                                ax(i)=[];
                            end
                        end
                        if isempty(ax)
                            %Parent=qp_createaxes(pfig,'oneplot');
                            Parent = axes('Parent',pfig);
                            qp_defaultaxessettings(Parent)
                        else
                            Parent=ax(1);
                        end
                    else
                        Parent=UD.PlotMngr.CurrentAxes;
                        if ~isempty(cmdargs) && strcmp(cmdargs{1},'right')
                            Parent=getappdata(Parent,'linkedaxes');
                        end
                        if ishandle(Parent)
                            pfig=get(Parent,'parent');
                        else
                            pfig=[];
                            Parent=[];
                        end
                    end
                    
                    if ishandle(Parent)
                        PS.FI=Info;
                        
                        PS.Domain=DomainNr;
                        PS.Props=Props;
                        PS.SubField=subf;
                        PS.Selected=selected;
                        if strcmp(cmd,'loaddata')
                            PS.Parent='loaddata';
                        else
                            PS.Parent=Parent;
                        end
                        PS.Handles=hNew;
                        PS.Stations=stats;
                        PS.Ops=Ops;
                        [hNew,Error,Info]=qp_plot(PS);
                    end
                    
                    if strcmp(cmd,'loaddata')
                        % hNew contains the data ...
                        if ~Error
                            % update FileInfo ...
                            File(NrInList)=Info;
                            set(Handle_SelectFile, 'userdata', File);
                            % return data ...
                            if nargout>0
                                outdata = {hNew};
                            else
                                assignin('base', 'data', hNew)
                            end
                        end
                    else
                        if ~isempty(pfig)
                            set(UD.PlotMngr.FigList,'value',1,'string',listnames(pfig,'showType','no','showHandle','no','showTag','no'),'userdata',pfig);
                            set(UD.PlotMngr.ItList,'value',[]) % clear item selection such that new item will be selected
                            d3d_qp refreshfigs
                        end
                        qp_updatescroller(hNew,pfig)
                        
                        if Animate
                            qck_anim('start',pfig,T);
                        end
                    end
                catch Ex
                    qp_error('Catch in d3d_qp\quickview',Ex)
                end
                set(mfig,'pointer','arrow')
        end
        d3d_qp update_addtoplot
        
    case {'colourmapeditor'}
        Hcmap=findobj(UOH,'tag','colourmap=?');
        cmaps=get(Hcmap,'string');
        cmapname=cmaps{get(Hcmap,'value')};
        cmapstruct=qp_colormap(':getstruct',cmapname);
        uicontrolfont = qp_fontsettings('DefaultUicontrolFont');
        cmapstruct=md_colormap(cmapstruct,uicontrolfont);
        cmaps=qp_colormap(':reload');
        newcmapname=cmapstruct.Name;
        i=ustrcmpi(newcmapname,cmaps);
        if i<0
            i=ustrcmpi(cmapname,cmaps);
            if i<0
                i=1;
            end
        end
        set(Hcmap,'value',1,'string',cmaps,'value',i);
        %
        d3d_qp colourmap*
        %
        if logfile
            writelog(logfile,logtype,'colourmap',cmaps{get(Hcmap,'value')});
        end
        
    case {'colourmap','colourmap*'}
        % command requires an input string
        Hcmaplist=findobj(UOH,'tag','colourmap=?');
        cmaps=get(Hcmaplist,'string');
        if ~isempty(cmdargs)
            i=ustrcmpi(cmdargs{1},cmaps);
            if i<0
                error('Invalid %s: %s',cmd,cmdargs{1})
            else
                set(Hcmaplist,'value',i);
            end
        end
        cmapsel=get(Hcmaplist,'value');
        cmapprev=findobj(UOH,'tag','colourmapbutton');
        pos=get(cmapprev,'pos');
        reqcmapl=pos(3);
        cmaps=get(Hcmaplist,'string');
        cmap=qp_colormap(cmaps{cmapsel},reqcmapl);
        cmaplength=size(cmap,1);
        cmap=cmap(floor(cmaplength*(0:(reqcmapl-1))/reqcmapl)+1,:);
        set(cmapprev,'cdata',repmat(reshape(cmap,[1 size(cmap)]),18,1))
        if logfile && cmd(end)~='*'
            writelog(logfile,logtype,cmd,cmaps{get(Hcmaplist,'value')});
        end
        
    case {'selectiontype','axestype','plotcoordinate','component','plottype', ...
            'climmode','presenttype','vecscalem','vertscalem','thinfld', ...
            'linestyle','marker','threshdistr','horizontalalignment', ...
            'verticalalignment','exporttype','trackcolour','vectorcolour', ...
            'dataunits','vectorstyle','angleconvention','axestimezone', ...
            'operator'}
        % commands require an input string
        %
        % nothing to do except refreshing the options
        if strcmp(cmd,'plottype')
            cmd='component';
        end
        modelist=findobj(UOH,'tag',[cmd '=?']);
        if strcmp(get(modelist,'enable'),'on')
            modes=get(modelist,'string');
            trigger={};
            if ~isempty(cmdargs)
                i=ustrcmpi(cmdargs{1},modes);
                if i<0 && strcmp(cmd,'angleconvention')
                    ibracket = strfind(cmdargs{1},'[');
                    if ~isempty(ibracket)
                        newmode = cmdargs{1}(1:ibracket(1)+1);
                        i=ustrcmpi(newmode,modes);
                    end
                end
                if i<0
                    if strcmp(cmd,'exporttype') && strcmp(cmdargs{1},'mat file')
                        %
                        % convert 'mat file' export type to 'mat file (v6)' for
                        % consistency
                        %
                        i=ustrcmpi('mat file (v6)',modes);
                        if i<0
                            error('Invalid %s: %s',cmd,cmdargs{1})
                        else
                            set(modelist,'value',i)
                        end
                    elseif strcmp(cmd,'dataunits')
                        %
                        % if it is not a mode string, it must be interpreted as a unit
                        % string. Make sure that dataunits=? is set to 'Other'
                        %
                        set(modelist,'value',find(strcmp('Other',modes)))
                        modelist=findobj(UOH,'tag',[cmd '=!']);
                        set(modelist,'string',cmdargs{1})
                    elseif strcmp(cmd,'angleconvention')
                        switch lower(cmdargs{1})
                            case 'nautical'
                                i=ustrcmpi('Nautical To [-',modes);
                            case 'nautical positive'
                                i=ustrcmpi('Nautical To [0',modes);
                            case 'cartesian'
                                i=ustrcmpi('Cartesian To [-',modes);
                            case 'cartesian positive'
                                i=ustrcmpi('Cartesian To [0',modes);
                            otherwise
                                error('Invalid %s: %s',cmd,cmdargs{1})
                        end
                        set(modelist,'value',i)
                    else
                        error('Invalid %s: %s',cmd,cmdargs{1})
                    end
                else
                    set(modelist,'value',i);
                    if strcmp(modes{i},'angle')
                        % old option 'angle (radians)' or 'angle (degrees)'
                        ob = strfind(cmdargs{1},'(');
                        cb = strfind(cmdargs{1},')');
                        trigger = {'dataunits',cmdargs{1}(ob+1:cb-1)};
                    end
                end
            elseif strcmp(cmd,'dataunits')
                modelist=gcbo; % either dataunits=? or dataunits=!
            end
            d3d_qp updateoptions
            if logfile
                if strcmp(get(modelist,'tag'),'dataunits=!')
                    writelog(logfile,logtype,cmd,get(modelist,'string'));
                else
                    writelog(logfile,logtype,cmd,modes{get(modelist,'value')});
                end
            end
            if ~isempty(trigger)
                d3d_qp(trigger{:});
            end
        end
        
    case {'colour','facecolour','markercolour','markerfillcolour','textboxfacecolour', ...
            'textboxcolour'}
        % commands require an input color
        %
        switch cmd
            case 'textboxcolour'
                cmd='textboxfacecolour';
        end
        clrh=findobj(UOH,'tag',[cmd '=?']);
        if isempty(cmdargs)
            clr=uisetcolor(get(clrh,'userdata'),'Specify the colour ...');
        else
            clr=cmdargs{1};
        end
        if ischar(clr)
            clr = str2color(clr);
        end
        if isequal(size(clr),[1 3]) && all(clr>=0 & clr<=1)
            set(clrh,'backgroundcolor',clr,'userdata',clr)
        elseif ~isempty(cmdargs)
            error('Invalid colour specification.');
        end
        if logfile
            writelog(logfile,logtype,cmd,clr);
        end
        
    case {'colourtracks','colourvectors','usemarkercolour', ...
            'usemarkerfillcolour','colclassify','colourbar','colourdams', ...
            'textbox','fillpolygons','colvector','coldams','unicolour'}
        % commands require an input logical
        %
        % nothing do except refreshing the options
        switch cmd
            case 'coldams'
                cmd='colourdams';
            case 'colvector'
                cmd='colourvectors';
        end
        tag=cmd;
        switch cmd
            case 'textbox'
                tag='textbox=?';
        end
        cb=findobj(UOH,'tag',tag);
        if ~isempty(cmdargs)
            Log=cmdargs{1};
            if ~isequal(Log,0) && ~isequal(Log,1)
                error('Invalid argument specified for %s.',cmd)
            end
            set(cb,'value',Log);
        end
        d3d_qp updateoptions
        if logfile
            writelog(logfile,logtype,cmd,get(cb,'value'));
        end
        
    case {'colbarhorz','climsymm','extend2edge','clipnans'}
        % nothing do
        cb=findobj(UOH,'tag',cmd);
        if ~isempty(cmdargs)
            Log=cmdargs{1};
            if ~isequal(Log,0) && ~isequal(Log,1)
                error('Invalid argument specified for %s.',cmd)
            end
            set(cb,'value',Log);
        end
        if logfile
            writelog(logfile,logtype,cmd,get(cb,'value'));
        end
        
    case {'numformat','expformat'}
        cv=findobj(UOH,'tag',[cmd '=?']);
        if isempty(cmdargs)
            Str=get(cv,'string');
        else
            Str=cmdargs{1};
        end
        set(cv,'string',Str)
        if logfile
            writelog(logfile,logtype,cmd,Str);
        end
        
    case {'thresholds','thresholdstep'}
        cv=findobj(UOH,'tag','thresholds=?');
        if isempty(cmdargs)
            Str=get(cv,'string');
        else
            Str=cmdargs{1};
        end
        if strcmp(cmd,'thresholdstep')
            if ischar(Str)
                c.step=str2double(Str);
            else
                c.step=Str;
            end
        elseif ischar(Str)
            if length(Str)>1 && Str(1)==':' && Str(end)==':'
                c=[];
                c.step=str2double(Str(2:end-1));
            else
                c=unique(str2vec(Str,'%f'));
            end
        else
            c=Str;
        end
        if isstruct(c)
            if isnan(c.step)
                error('Invalid %s input string: ''%s''',cmd,Str)
            elseif c.step<=0
                error('Threshold step size should be strictly positive')
            end
            cmd='thresholdstep';
        end
        if isstruct(c)
            set(cv,'string',sprintf(':%g:',c.step),'userdata',c)
            c=c.step;
        else
            set(cv,'string',vec2str(c,'noones','nobrackets'),'userdata',c)
        end
        d3d_qp updateoptions
        if logfile
            writelog(logfile,logtype,cmd,c);
        end
        
    case {'xclipping','yclipping','zclipping','clippingvals'}
        cv=findobj(UOH,'tag',[cmd '=?']);
        if isempty(cmdargs)
            Str=get(cv,'string');
        else
            Str=cmdargs{1};
        end
        lasterr('');
        try
            if ischar(Str)
                [c,Str]=realset(Str);
            else
                c=Str;
                Str=realset(c);
            end
        catch Ex
            qp_error(['Catch in d3d_qp\' cmd],Ex)
            c=get(cv,'userdata');
            if isstruct(c)
                Str=realset(c);
            else
                Str=vec2str(c,'noones','nobrackets');
            end
        end
        set(cv,'string',Str,'userdata',c)
        if strcmp(cmd,'thresholds')
            d3d_qp updateoptions
        end
        if logfile
            writelog(logfile,logtype,cmd,Str);
        end
        
    case 'optslider'
        os = gcbo;
        switch os
            case UD.Options.Slider
                Options = UD.Options;
            case UD.PlotMngr.Options.Slider
                Options = UD.PlotMngr.Options;
        end
        UOH = Options.Handles;
        offset=get(os,'value');
        old_offset=get(os,'userdata');
        p=get(UOH,{'position'});
        for i = 1:length(p)
            p{i}(2) = p{i}(2)-offset+old_offset;
        end
        set(UOH,{'position'},p)
        set(os,'userdata',offset)
        
    case 'dock'
        switch get(UD.Options.Dock,'userdata')
            case 0
                %
                % from docked to undocked
                %
                pos = get(mfig,'position');
                pos2 = pos;
                hshift = pos(3) - 180;
                pos2(1) = pos(1) + hshift;
                pos2(3) = 180;
                pos(3) = hshift;
                %
                ofig = qp_uifigure('Plot Options','dock','Plot Options',pos2,'d3d_qp');
                set(ofig,'resizefcn','d3d_qp optionsresize','resize','on')
                %
                set(UD.Options.Dock,'parent',ofig)
                qp_tooltip(UD.Options.Dock,'Dock plot options')
                %
                set(UD.Options.Slider,'parent',ofig)
                %
                set(UOH,'parent',ofig)
                %
                A = allchild(ofig);
                p = get(A,{'position'});
                for i = 1:length(p)
                    p{i}(1) = p{i}(1) - hshift;
                end
                set(A,{'position'},p)
                %
                set(UD.Options.Dock, ...
                    'CData',qp_icon('dock','nan'), ...
                    'UserData',1)
                set(mfig,'position',pos)
                set(ofig,'visible','on')
                set(findall(mfig,'tag','dock','type','uimenu'),'visible','on')
            case 1
                %
                % from undocked to docked
                %
                set(findall(mfig,'tag','dock','type','uimenu'),'visible','off')
                pos = get(mfig,'position');
                hshift = pos(3);
                pos(3) = pos(3) + 180;
                %
                ofig = get(UD.Options.Dock,'parent');
                %
                A = allchild(ofig);
                p = get(A,{'position'});
                for i = 1:length(p)
                    p{i}(1) = p{i}(1) + hshift;
                end
                set(A,{'position'},p)
                %
                spos = get(UD.Options.Slider,'position');
                spos(4) = pos(4)-15;
                set(UD.Options.Slider,'parent',mfig,'position',spos)
                %
                dpos = get(UD.Options.Dock,'position');
                dpos(2) = pos(4)-15;
                set(UD.Options.Dock,'parent',mfig,'position',dpos)
                qp_tooltip(UD.Options.Dock,'Undock plot options')
                %
                set(UOH,'parent',mfig)
                delete(ofig)
                %
                set(UD.Options.Dock, ...
                    'CData',qp_icon('undock','nan'), ...
                    'UserData',0)
                set(mfig,'position',pos)
                %
                update_option_positions(UD,'main',pos(4)-30+1)
        end
        
    case {'climmin','climmax','1vecunit','vscale','thinfact','thindist','thincount','fontsize','markersize','linewidth'}
        switch cmd
            case {'climmin','climmax'}
                pos=0;
                int=0;
            case {'thinfact','thincount'}
                pos=1;
                int=1;
            case {'1vecunit','vscale','thindist','fontsize','markersize','linewidth'}
                pos=1;
                int=0;
        end
        contr=findobj(UOH,'tag',[cmd '=?']);
        if isempty(cmdargs)
            contrv=str2num(get(contr,'string'));
        else
            contrv=cmdargs{1};
        end
        contrvdef=get(contr,'userdata');
        if isempty(contrv)
            contrv=contrvdef;
        elseif ~isequal(size(contrv),[1 1])
            contrv=contrv(1);
        elseif pos && (contrv<=0)
            contrv=contrvdef;
        elseif int && (contrv~=round(contrv) || ~isfinite(contrv))
            contrv=contrvdef;
        end
        set(contr,'string',sprintf('%g',contrv),'userdata',contrv)
        if logfile
            writelog(logfile,logtype,cmd,contrv);
        end
        
    case 'update_addtoplot'
        qv=findobj(mfig, 'tag', 'quickview');
        atp = findobj(mfig, 'tag', 'addtoplot');
        multi = get(UD.PlotMngr.FigAll, 'value') | get(UD.PlotMngr.AxAll, 'value');
        if ~multi && ~isempty(UD.PlotMngr.CurrentAxes) ...
                && ishandle(UD.PlotMngr.CurrentAxes) ...
                && (strcmp(get(qv, 'enable'), 'on') ...
                    || (isfield(UD.State,'axestype') ...
                        && strcmp(UD.State.axestype, 'Time')))
            axestype=getappdata(UD.PlotMngr.CurrentAxes, 'BasicAxesType');
            %
            % Temporarily replace Lon-Lat by X-Y.
            %
            if ischar(axestype)
                axestype=strrep(axestype, 'Lon-Lat', 'X-Y');
            end
            %
            if ~ischar(axestype)
                %
                % new axes type without type assigned.
                %
                atp_on = 1;
            elseif strcmp(UD.State.axestype, 'Time')
                %
                % a time line needs a time axis
                %
                switch axestype
                    case {'analog clock', 'digital clock', 'calendar page'}
                        % only one data set can be added to these types of axes
                        atp_on = isempty(get(UD.PlotMngr.ItList, 'userdata'));
                    otherwise
                        atp_on = ~isempty(strfind(axestype, 'Time'));
                end
            elseif ~ischar(axestype) || strcmp(axestype, UD.State.axestype)
                %
                % perfect match of axes types (including units in case of Val)
                %
                atp_on = 1;
            else
                %
                % no exact match
                %
                if strcmp(strtok(axestype), strtok(UD.State.axestype)) && ...
                        (isempty(strfind(axestype, ' ')) || ...
                        isempty(strfind(UD.State.axestype, ' ')))
                    %
                    % if one of the two axes types lacks a unit specifier,
                    % and the axes types without unit specifier match, then
                    % still okay to combine plots.
                    %
                    atp_on = 1;
                else
                    switch axestype
                        case {'analog clock', 'digital clock', 'calendar page'}
                            % only Time quantity can be added
                            atp_on = 0;
                        otherwise
                            atp_on = 0.5;
                    end
                end
            end
        else
            atp_on = 0;
        end
        switch atp_on
            case 1   % yes!
                set(atp, 'enable', 'on', 'foregroundcolor', 'k')
            case 0.5 % not wise, but maybe ...
                set(atp, 'enable', 'on', 'foregroundcolor', 'r')
            case 0   % no!
                set(atp, 'enable', 'off', 'foregroundcolor', 'k')
        end
        
    case 'close'
        if logfile
            try
                writelog(logfile,logtype,cmd);
                fclose(logfile);
            end
        end
        try
            Pos=get(mfig,'position');
            qp_settings('MainPositionUR',Pos(1:2)+Pos(3:4))
            if isstruct(UD) && isstruct(UD.PlotMngr)
                Pos=get(UD.PlotMngr.Fig,'position');
                qp_settings('PlotMngrPositionUL',Pos(1:2)+[0 Pos(4)])
                Vis=get(UD.PlotMngr.Fig,'visible');
                qp_settings('PlotMngrVisible',Vis)
            end
            qp_settings('<SAVE>')
        catch
        end
        if isstandalone
            close all force
        else
            if isstruct(UD)
                if isstruct(UD.PlotMngr) && ishandle(UD.PlotMngr.Fig)
                    delete(UD.PlotMngr.Fig);
                end
                if isstruct(UD.FilOpt) && ishandle(UD.FilOpt.Fig)
                    delete(UD.FilOpt.Fig);
                end
                if isstruct(UD.GridView) && ishandle(UD.GridView.Fig)
                    delete(UD.GridView.Fig);
                end
                if isstruct(UD.ComLine) && ishandle(UD.ComLine.Fig)
                    delete(UD.ComLine.Fig);
                end
                if isstruct(UD.Options) && ishandle(UD.Options.Slider)
                    delete(get(UD.Options.Slider,'parent'));
                end
            end
            if ishandle(mfig)
                delete(mfig);
            end
        end
        qpversion=[];
        
    case {'qpmanual','matlabmanual'}
        switch cmd
            case 'qpmanual'
                manual = 'Delft3D-QUICKPLOT_User_Manual.pdf';
            case 'matlabmanual'
                manual = 'Delft3D-MATLAB_User_Manual.pdf';
        end
        p = {qp_basedir('exe') getenv('D3D_HOME')};
        found = 0;
        for ip = 1:length(p)
            pth = [p{ip} filesep];
            dp = strfind(pth,filesep);
            for i=length(dp):-1:1
                manp1 = [pth(1:dp(i)) manual];
                manp2 = [pth(1:dp(i)) 'manuals' filesep manual];
                if exist(manp1)
                    manp = manp1;
                    found = 1;
                elseif exist(manp2)
                    manp = manp2;
                    found = 1;
                end
                if found
                    if nargout==0
                        if ~isempty(strfind(manp,' '))
                            manp = sprintf('"%s%"',manp);
                        end
                        system(['start "Manual" ' manp]);
                    end
                    break
                end
            end
            if found
                break
            end
        end
        if nargout>0
            outdata = {found};
        end
        
    case {'about','version'}
        qp_showabout(qpversion,'quickplot',qpcreationdate);
        
    case {'aboutmatlab'}
        qp_showabout(qpversion,'matlab');
        
    case {'plotmngr','hideplotmngr'}
        currentstatus=get(UD.PlotMngr.Fig,'visible');
        if strcmp(cmd,'hideplotmngr')
            currentstatus='on';
        else
            try
                if strcmp(get(UD.PlotMngr.Fig,'windowState'),'minimized')
                    set(UD.PlotMngr.Fig,'windowState','normal')
                end
            catch
                try % before 2018a
                    warnJF = warning('query','MATLAB:HandleGraphics:ObsoletedProperty:JavaFrame');
                    warning('off','MATLAB:HandleGraphics:ObsoletedProperty:JavaFrame')
                    jFrame = get(handle(UD.PlotMngr.Fig),'JavaFrame');
                    warning(warnJF);
                    if jFrame.isMinimized
                        jFrame.setMinimized(0)
                        return
                    end
                catch
                end
            end
        end
        switch currentstatus
            case 'on'
                setvis='off';
                reverse='on';
            case 'off'
                setvis='on';
                reverse='off';
        end
        set(UD.PlotMngr.Fig,'visible',setvis)
        set(findobj(mfig,'label','Hide &Plot Manager'),'visible',setvis)
        set(findobj(mfig,'label','Show &Plot Manager'),'visible',reverse)
        set(UD.MainWin.TB_PM,'state',setvis)
        
    case {'comline','hidecomline'}
        currentstatus=get(UD.ComLine.Fig,'visible');
        if strcmp(cmd,'hidecomline')
            currentstatus='on';
        elseif matlabversionnumber >= 9.04 % 2018a
            winstate = get(UD.ComLine.Fig,'WindowState');
            if ~strcmp(winstate,'normal')
                set(UD.ComLine.Fig,'WindowState','normal')
            end
        else
            try
                jFrame = get(handle(UD.ComLine.Fig),'JavaFrame');
                if jFrame.isMinimized
                    jFrame.setMinimized(0)
                    return
                end
            catch
            end
        end
        switch currentstatus
            case 'on'
                setvis='off';
                reverse='on';
            case 'off'
                setvis='on';
                reverse='off';
        end
        set(UD.ComLine.Fig,'visible',setvis)
        set(findobj(mfig,'label','Hide C&ommand Line'),'visible',setvis)
        set(findobj(mfig,'label','Show C&ommand Line'),'visible',reverse)
        set(UD.MainWin.TB_CL,'state',setvis)
        %
        % Commands not used in QuickPlot code, but useful to include for
        % execution from the command line...
        %
        if 0
            box
            delwaq2raster
            grid
            camlight
            lighting phong;
            material dull;
            imagesc;
            xlim;
            ylim;
            zlim;
            qpsf;
            qpsa;
            qpfile
        end
        
    case 'showmessagewin'
        ui_message
        
    case 'evalhist'
        pos=get(gcbo,'position');
        set(UD.ComLine.Eval,'enable','on')
        set(UD.ComLine.EvalHistMenu,'position',pos(1:2)+pos(3:4)/2,'visible','on')
        
    case 'evalhistmenu'
        set(UD.ComLine.Eval,'string',get(gcbo,'userdata'),'enable','on')
        
    case 'evalcmd'
        evalcmd=get(UD.ComLine.Eval,'string');
        set(UD.ComLine.Eval,'enable','inactive')
        hvoff=findall(0,'handlevisibility','callback');
        set(hvoff,'handlevisibility','off')
        evalerr=0;
        if ~isempty(evalcmd)
            if evalcmd(1)=='>'
                % MATLAB command
                evalcmd=evalcmd(2:end);
                ans='No value returned';
                defaultans=ans;
                try
                    eval(evalcmd);
                    if ~isequal(ans,defaultans)
                        ui_message('warning',var2str(ans))
                    end
                catch Ex
                    qp_error(evalcmd,Ex)
                    evalerr=1;
                end
                %
                % take care of logging ...
                %
                if logfile && ~evalerr
                    writelog(logfile,-logtype,evalcmd);
                end
            else
                % QUICKPLOT command
                %
                % command itself will take care of logging ...
                %
                try
                    [cmd,cmdargs]=qp_cmdstr(evalcmd);
                    d3d_qp(cmd,cmdargs{:});
                catch
                    errmsg=multiline(lasterr,'cell');
                    if isstandalone
                        i=1;
                    else
                        i=2;
                    end
                    ui_message('error',{evalcmd,errmsg{i:end}})
                    evalerr=1;
                end
            end
        end
        set(hvoff,'handlevisibility','callback')
        if ~isempty(evalcmd) && ~evalerr
            evalcmd=get(UD.ComLine.Eval,'string');
            qp_update_evalhistmenu(UD.ComLine.EvalHistMenu,'add',evalcmd)
            set(UD.ComLine.Eval,'string','')
        end
        set(UD.ComLine.Eval,'enable','on')
        
    case 'gridview_selected'
        Rng=qp_gridview('getrange',UD.GridView.Fig);
        if isstruct(Rng) && isfield(Rng,'Type')
            switch Rng.Type
                case 'line'
                    if ~isfinite(Rng.Range(4))
                        d3d_qp('allm*',0)
                        d3d_qp('editm*',Rng.Range(1))
                        d3d_qp('alln*',1)
                    else %~isfinite(Rng.Range(2))
                        d3d_qp('allm*',1)
                        d3d_qp('alln*',0)
                        d3d_qp('editn*',Rng.Range(3))
                    end
                case {'lineseg','range'}
                    if iscell(Rng.Range)
                        d3d_qp('allm*',0)
                        d3d_qp('editm*',Rng.Range{1})
                        if length(Rng.Range)>1
                            d3d_qp('alln*',0)
                            d3d_qp('editn*',Rng.Range{2})
                        end
                    else
                        d3d_qp('allm*',0)
                        r=[Rng.Range(1) Rng.Range(2)];
                        d3d_qp('editm*',min(r):max(r))
                        d3d_qp('alln*',0)
                        r=[Rng.Range(3) Rng.Range(4)];
                        d3d_qp('editn*',min(r):max(r))
                    end
                case 'point'
                    d3d_qp('allm*',0)
                    d3d_qp('editm*',Rng.Range(1))
                    if length(Rng.Range)>1
                        d3d_qp('alln*',0)
                        d3d_qp('editn*',Rng.Range(2))
                    end
                case 'pwline'
                    d3d_qp('editmn*',Rng.Range)
                case 'genline'
                    d3d_qp('editxy*',Rng.Range)
                case 'wholegrid'
                    d3d_qp('allm*',1)
                    d3d_qp('alln*',1)
            end
        end
        
    case 'gridview_update'
        if strcmp(get(UD.GridView.Fig,'visible'),'on')
            MW = UD.MainWin;
            switch getvalstr(MW.HSelType)
                case 'M range and N range'
                    range={};
                    %----
                    if strcmp(get(MW.M,'enable'),'on')
                        allm=get(MW.AllM,'value');
                        if allm
                            maxm=get(MW.MaxM,'userdata');
                            range{1}=1:maxm;
                        else
                            mm=get(MW.EditM,'userdata');
                            range{1}=mm;
                        end
                    elseif strcmp(get(MW.MaxM,'enable'),'on')
                        range{1}=1;
                    end
                    %----
                    if strcmp(get(MW.N,'enable'),'on')
                        alln=get(MW.AllN,'value');
                        if alln
                            maxn=get(MW.MaxN,'userdata');
                            range{2}=1:maxn;
                        else
                            nn=get(MW.EditN,'userdata');
                            range{2}=nn;
                        end
                    elseif strcmp(get(MW.MaxN,'enable'),'on')
                        range{2}=1;
                    end
                    %----
                case '(M,N) point/path'
                    range.Type='pwline';
                    range.Range = get(MW.EditMN,'userdata');
                case '(X,Y) point/path'
                    range.Type='genline';
                    range.Range = get(MW.EditXY,'userdata');
            end
            qp_gridview('setrange',UD.GridView.Fig,range)
        end
        
    case {'showgridviewldb'}
        OpenFile=findobj(mfig,'tag','openfile','type','uipushtool');
        pn=get(OpenFile,'userdata');
        [FI,FileName,Tp,Otherargs]=qp_proxy('openldb',pn);
        GVAxes=findobj(UD.GridView.Fig,'type','axes');
        zoomupdate=0;
        switch Tp
            case {'Tekal','BNA File','ArcInfoUngenerate','ESRI-Shape'}
                qnt = qpread(FI);
                if ~ismember('line',{qnt.Name})
                    ui_message('error','This Tekal file is not supported as supplier of landboundary data.')
                else
                    LDB=qpread(FI,'line','griddata');
                    delete(findall(GVAxes,'tag','landboundary'))
                    ldbcol=qp_settings('gridviewlandboundarycolor')/255;
                    line(LDB.X,LDB.Y,'color',ldbcol,'parent',GVAxes,'hittest','off','tag','landboundary','clipping','off')
                    zoomupdate=1;
                end
            case ''
                delete(findall(GVAxes,'tag','landboundary'))
                zoomupdate=1;
            otherwise
                ui_message('error','%s not supported as supplier of landboundary data.',Tp)
        end
        
        if zoomupdate
            xl=limits(GVAxes,'xlim'); xl=xl+[-1 1]*max(0.00001,abs(diff(xl)*0.01))/20;
            yl=limits(GVAxes,'ylim'); yl=yl+[-1 1]*max(0.00001,abs(diff(yl)*0.01))/20;
            if ~isfinite(xl)
                xl=[0 1];
                yl=[0 1];
            end
            set(GVAxes,'xlim',xl,'ylim',yl)
            delete(get(GVAxes,'zlabel')) % delete the old ZOOMAxesData applicationdata
            zoom(UD.GridView.Fig,'reset');
            zoom(UD.GridView.Fig,'on')
        end
        
    case {'gridview','hidegridview'}
        currentstatus=get(UD.GridView.Fig,'visible');
        if strcmp(cmd,'hidegridview')
            currentstatus='on';
        end
        switch currentstatus
            case 'on'
                setvis='off';
                reverse='on';
            case 'off'
                setvis='on';
                reverse='off';
        end
        set(UD.GridView.Fig,'visible',setvis)
        set(findobj(mfig,'label','Hide &Grid View'),'visible',setvis)
        set(findobj(mfig,'label','Show &Grid View'),'visible',reverse)
        set(UD.MainWin.TB_GV,'state',setvis)
        if strcmp(setvis,'on')
            datafields=findobj(mfig,'tag','selectfield');
            Props=get(datafields,'userdata');
            fld=get(datafields,'value');
            Handle_SelectFile=findobj(mfig,'tag','selectfile');
            File=get(Handle_SelectFile,'userdata');
            NrInList=get(Handle_SelectFile,'value');
            Succes=~isempty(File);
            if ~Succes || isempty(Props) || ~isfield(Props,'DimFlag') || ~isempty(strmatch('---',Props(fld).Name))
                qp_gridview('setgrid',UD.GridView.Fig,[],[])
                set(UD.GridView.Fig,'userdata',[])
            elseif isfield(Props(fld),'UseGrid') && ~isempty(Props(fld).UseGrid) && Props(fld).UseGrid>0
                Info=File(NrInList);
                %
                Handle_Domain=findobj(mfig,'tag','selectdomain');
                DomainNr=get(Handle_Domain,'value');
                %
                try
                    qp_gridviewhelper(UD,Info,DomainNr,Props,fld)
                    d3d_qp('gridview_update')
                catch err
                    qp_gridview('setgrid',UD.GridView.Fig,[],[])
                    set(UD.GridView.Fig,'userdata',[])
                    rethrow(err)
                end
            else
                qp_gridview('setgrid',UD.GridView.Fig,[],[])
                set(UD.GridView.Fig,'userdata',[])
            end
        end
        
    case 'stoprecord'
        stopped=0;
        if (logtype==1 && logfile==0) || logtype>2
            ButtonName='N/A';
            stopped=1;
        else
            rctype={'log','Matlab script'};
            rctype=rctype{logtype};
            ButtonName=questdlg( ...
                sprintf('This will stop writing the %s file',rctype), ...
                rctype, ...
                'OK','Cancel','Cancel');
        end
        if strcmp(ButtonName,'OK')
            fclose(logfile);
            stopped=1;
        end
        if stopped
            set(findobj(mfig,'tag','stoprecord'),'enable','off')
            set(UD.MainWin.StartRec,'enable','on')
            set(UD.MainWin.RecTB,'enable','on')
            logfile=0;
        end
        
    case {'logfile','scriptfile','logfiletomw','scriptfiletomw','recordtomw','scriptfiletomcw'}
        switch cmd
            case 'logfile'
                logtype=1;
                ftype='*.qplog';
            case 'scriptfile'
                logtype=2;
                ftype='*.m';
            case {'recordtomw','logfiletomw'}
                logtype=3; % Quickplot Log file
            case 'scriptfiletomw'
                logtype=4; % MATLAB statements
            case 'scriptfiletomcw'
                logtype=6; % MATLAB statements
        end
        if logtype>2
            logfile=1;
            set(findobj(mfig,'tag','stoprecord'),'enable','on')
            set(UD.MainWin.StartRec,'enable','off')
            set(UD.MainWin.RecTB,'enable','off')
            if logtype==3 || logtype==4
                ui_message
            end
        else
            logfile=0;
            [fn,pn]=uiputfile(ftype);
            if ischar(fn)
                [lp,ln,le]=fileparts(fn);
                if isempty(le)
                    fn=[fn ftype(2:end)];
                end
                try
                    logfile=fopen([pn fn],'w','n','UTF-8');
                catch Ex
                    qp_error('Catch in d3d_qp\logfile',Ex)
                end
                set(findobj(mfig,'tag','stoprecord'),'enable','on')
                set(UD.MainWin.StartRec,'enable','off')
                set(UD.MainWin.RecTB,'enable','off')
            end
        end
    case 'figurename'
        fig = qpsf;
        PM = UD.PlotMngr;
        %
        if length(cmdargs)>1
            nm = cmdargs{1};
        else
            nm = get(PM.FigName,'string');
        end
        set(fig,'name',nm)
        %
        nms = get(PM.FigList,'string');
        nmi = get(PM.FigList,'value');
        nms{nmi} = nm;
        set(PM.FigList,'string',nms)
        set(PM.FigName,'string',nm)
        %
        if logfile
            writelog(logfile,logtype,cmd,nm);
        end
        
    case 'figurepaperunit'
        fig = qpsf;
        PM = UD.PlotMngr;
        %
        pus = get(PM.FigPaperUnit,'string');
        pui = get(PM.FigPaperUnit,'value');
        pu  = pus{pui};
        set(fig,'paperunit',pu)
        %
        d3d_qp refreshfigprop
        
    case 'figurepapertype'
        fig = qpsf;
        PM = UD.PlotMngr;
        %
        pts = get(PM.FigPaperType,'string');
        ors = get(PM.FigPaperOrientation,'string');
        pus = get(PM.FigPaperUnit,'string');
        if ~isempty(cmdargs)
            pt  = cmdargs{1};
            pti = ustrcmpi(pt,pts);
            if pti<0
                if ischar(pt)
                    error('Invalid paper type: %s',pt)
                else
                    error('Invalid paper type: <non string>')
                end
            end
        else
            pti = get(PM.FigPaperType,'value');
            pt  = pts{pti};
        end
        %
        if length(cmdargs)>1
            or  = cmdargs{2};
            ori = ustrcmpi(or,ors);
            if ori<0
                error('Invalid paper orientation')
            end
        else
            ori = get(PM.FigPaperOrientation,'value');
            or  = ors{ori};
        end
        %
        if strcmp(pt,'<custom>')
            if length(cmdargs)>3
                sz = varargin{2};
                pu = varargin{3};
                pui = ustrcmpi(pu,pus);
                if pui<0
                    if ischar(pu)
                        error('Invalid paper unit: %s',pu)
                    else
                        error('Invalid paper unit: <non string>')
                    end
                end
            else
                sz = get(fig,'papersize');
                if ~strcmp(get(PM.FigPaperWidth,'string'),getappdata(PM.FigPaperWidth','refstring'))
                    sz(1) = str2double(get(PM.FigPaperWidth,'string'));
                end
                if ~strcmp(get(PM.FigPaperHeight,'string'),getappdata(PM.FigPaperHeight','refstring'))
                    sz(2) = str2double(get(PM.FigPaperHeight,'string'));
                end
                pui = get(PM.FigPaperUnit,'value');
                pu  = pus{pui};
            end
            psz = {'paperunit' pu 'papersize' sz};
        else
            psz = {};
        end
        %
        set(fig,'papertype',pt,'paperorientation',or,psz{:})
        % reshape the figure to match selected paper size
        qp_figaspect(fig)
        %
        % if the figure has a border, adjust it.
        %
        brdr = md_paper(fig,'getprops');
        if ~isempty(brdr)
            md_paper(fig,'no edit',brdr);
        end
        %
        d3d_qp refreshfigprop
        d3d_qp refreshaxprop
        %
        if logfile
            if strcmp(pt,'<custom>')
                writelog(logfile,logtype,cmd,pt,sz,pu);
            else
                writelog(logfile,logtype,cmd,pt,or);
            end
        end
        
    case 'figureborderstyle'
        fig = qpsf;
        PM = UD.PlotMngr;
        %
        bds = get(PM.FigBorderStyle,'string');
        if ~isempty(cmdargs)
            bd  = cmdargs{1};
            bdi = find(strcmpi(bd,bds));
            if isempty(bdi)
                if ischar(bd)
                    error('Invalid borderstyle: %s', bd)
                else
                    error('Invalid borderstyle: <non string>')
                end
            end
        else
            bdi = get(PM.FigBorderStyle,'value');
            bd  = bds{bdi};
        end
        %
        md_paper(fig,'no edit',bd)
        set(PM.FigBorderStyle,'value',bdi)
        if strcmp(bd,'none')
            set(PM.FigBorder,'enable','off')
        else
            set(PM.FigBorder,'enable','on')
        end
        %
        d3d_qp refreshfigs
        %
        if logfile
            writelog(logfile,logtype,cmd,bd);
        end
        
    case 'figureborder'
        fig = qpsf;
        hBorder = findall(fig,'type','axes','tag','border');
        %
        if ~isempty(cmdargs)
            md_paper(hBorder,'setprops',cmdargs{:})
        else
            md_paper('editmodal',hBorder)
        end
        %
        bProp = md_paper(hBorder,'getprops');
        %
        i = 1;
        str = {};
        while 1
            hTxt = sprintf('BorderText%i',i);
            if ~isfield(bProp,hTxt)
                break
            end
            cstr = bProp.(hTxt);
            str{i} = cstr;
            i = i+1;
        end
        %
        if logfile
            writelog(logfile,logtype,cmd,str{:});
        end
        
    case 'figurecolour'
        fig = qpsf;
        if isempty(cmdargs)
            clr=uisetcolor(get(fig,'color'),'Specify the figure colour ...');
        else
            clr=cmdargs{1};
        end
        if ischar(clr)
            clr = str2color(clr);
        end
        if isequal(size(clr),[1 3])
            set(fig,'color',clr)
            d3d_qp refreshfigprop
            if logfile
                writelog(logfile,logtype,cmd,clr);
            end
        end
        
    case 'figurerenderer'
        fig = qpsf;
        PM = UD.PlotMngr;
        %
        rdrs = get(PM.FigRendererType, 'string');
        if ~isempty(cmdargs)
            rdr = cmdargs{1};
            rdri = ustrcmpi(rdr, rdrs);
            if rdri<0
                if ischar(rdr)
                    error('Invalid renderer: %s', rdr)
                else
                    error('Invalid renderer: <non string>')
                end
            end
        else
            rdri = get(PM.FigRendererType, 'value');
            rdr  = rdrs{rdri};
        end
        %
        set(fig, 'renderer', rdr)
        d3d_qp refreshfigprop
        if logfile
            writelog(logfile, logtype, cmd, rdr);
        end
        
    case 'figuresmoothing'
        fig = qpsf;
        %
        if isempty(cmdargs)
            PM = UD.PlotMngr;
            smo = get(PM.FigSmoothing,'value');
        else
            smo = cmdargs{1};
        end
        if isequal(size(smo),[1 1])
            smotext = valuemap(smo,[1 0],{'on' 'off'});
            set(fig,'graphicssmoothing',smotext)
            d3d_qp refreshfigprop
            if logfile
                writelog(logfile,logtype,cmd,smo);
            end
        end
        
    case 'axesname'
        ax = qpsa;
        PM = UD.PlotMngr;
        %
        if ~isempty(cmdargs)
            nm = cmdargs{1};
        else
            nm = get(PM.AxName,'string');
        end
        set(ax,'tag',nm)
        %
        nms = get(PM.AxList,'string');
        nmi = get(PM.AxList,'value');
        nms(nmi) = listnames(ax);
        set(PM.AxList,'string',nms)
        set(PM.AxName,'string',nm)
        %
        if logfile
            writelog(logfile,logtype,cmd,nm);
        end
        
    case 'setaxestype'
        ax = qpsa;
        PM = UD.PlotMngr;
        %
        tps = get(PM.AxType,'string');
        if ~isempty(cmdargs)
            tp  = cmdargs{1};
            itp = ustrcmpi(tp,tps);
            if itp<0
                if ischar(tp)
                    error('Invalid axestype: %s', tp)
                else
                    error('Invalid axestyp: <non string>')
                end
            end
        else
            tps = get(PM.AxType,'string');
            itp = get(PM.AxType,'value');
        end
        %
        if itp>1 % itp==1: 'undefined'
            tp = tps{itp};
            setaxesprops(ax,tp)
            d3d_qp refreshaxes
            d3d_qp update_addtoplot
            %
            if logfile
                writelog(logfile,logtype,cmd,tp);
            end
        end
        
    case {'axescolour','xcolour','ycolour','zcolour'}
        ax = qpsa;
        if isempty(cmdargs)
            if strcmp(get(gcbo,'style'),'checkbox') && ~get(gcbo,'value')
                clr='none';
            else
                switch cmd
                    case 'axescolour'
                        clr=get(ax,'color');
                        if ischar(clr) % none
                            clr=get(get(ax,'parent'),'color');
                        end
                    case 'xcolour'
                        clr=get(ax,'xcolor');
                    case 'ycolour'
                        clr=get(ax,'ycolor');
                    case 'zcolour'
                        clr=get(ax,'zcolor');
                end
                clr0 = clr;
                clr=uisetcolor(clr,sprintf('Specify the %s colour ...',cmd(1:end-6)));
                if isequal(clr,clr0)
                    % cancel pressed or just confirming what was already selected
                    return
                end
            end
        else
            clr=cmdargs{1};
        end
        if ischar(clr) && ~strcmpi(clr,'none')
            clr = str2color(clr);
        end
        if isequal(size(clr),[1 3]) || (strcmp(cmd,'axescolour') && strcmpi(clr,'none'))
            switch cmd
                case 'axescolour'
                    set(ax,'color',clr)
                case {'xcolour','ycolour','zcolour'}
                    set(ax,[cmd(1) 'color'],clr)
            end
            d3d_qp refreshaxprop
            if logfile
                writelog(logfile,logtype,cmd,clr);
            end
        end
        
    case 'axeslimits'
        ax = qpsa;
        xdr = {'xdir','normal'};
        ydr = {'ydir','normal'};
        zdr = {};
        zlm = [];
        PM = UD.PlotMngr;
        if strcmp(get(PM.Z.LimitMin,'enable'),'on')
            zdr = {'zdir','normal'};
            zlm(1,1) = str2double(get(PM.Z.LimitMin,'string'));
            zlm(1,2) = str2double(get(PM.Z.LimitMax,'string'));
            if zlm(1)>zlm(2)
                zlm = fliplr(zlm);
                zdr{2} = 'reverse';
            end
        end
        if isempty(cmdargs)
            xlm(1,1) = str2double(get(PM.X.LimitMin,'string'));
            xlm(1,2) = str2double(get(PM.X.LimitMax,'string'));
            if xlm(1)>xlm(2)
                xlm = fliplr(xlm);
                xdr{2} = 'reverse';
            end
            ylm(1,1) = str2double(get(PM.Y.LimitMin,'string'));
            ylm(1,2) = str2double(get(PM.Y.LimitMax,'string'));
            if ylm(1)>ylm(2)
                ylm = fliplr(ylm);
                ydr{2} = 'reverse';
            end
        else
            xlm=cmdargs{1};
            ylm=cmdargs{2};
            if ischar(xlm)
                if isequal(xlm,'auto-reverse')
                    xlm    = 'auto';
                    xdr{2} = 'reverse';
                elseif ~isequal(xlm,'auto')
                    xlm = str2vec(xlm);
                end
            elseif xlm(1)>xlm(2)
                xlm = fliplr(xlm);
                xdr{2} = 'reverse';
            end
            if ischar(ylm)
                if isequal(ylm,'auto-reverse')
                    ylm    = 'auto';
                    ydr{2} = 'reverse';
                elseif ~isequal(ylm,'auto')
                    ylm = str2vec(ylm);
                end
            elseif ylm(1)>ylm(2)
                ylm = fliplr(ylm);
                ydr{2} = 'reverse';
            end
            if length(cmdargs)>2
                zlm=cmdargs{3};
                if ischar(zlm)
                    if isequal(zlm,'auto-reverse')
                        zlm    = 'auto';
                        zdr{2} = 'reverse';
                    elseif ~isequal(zlm,'auto')
                        zlm = str2vec(zlm);
                    end
                elseif zlm(1)>zlm(2)
                    zlm = fliplr(zlm);
                    zdr{2} = 'reverse';
                end
            end
        end
        if (isequal(size(xlm),[1 2]) || strcmp(xlm,'auto')) && ...
                (isequal(size(ylm),[1 2]) || strcmp(ylm,'auto')) && ...
                (isequal(size(zlm),[1 2]) || strcmp(zlm,'auto') || isempty(zlm))
            if ischar(xlm)
                set(ax,'xlimmode','auto',xdr{:})
            else
                if any(xlm<0) && strcmp(get(ax,'xscale'),'log')
                    set(ax,'xscale','linear')
                end
                set(ax,'xlim',xlm,xdr{:})
                setappdata(ax,'xlimmode','manual')
            end
            if ischar(ylm)
                set(ax,'ylimmode','auto',ydr{:})
            else
                if any(ylm<0) && strcmp(get(ax,'yscale'),'log')
                    set(ax,'yscale','linear')
                end
                set(ax,'ylim',ylm,ydr{:})
                setappdata(ax,'ylimmode','manual')
            end
            if ~isempty(zlm)
                if ischar(zlm)
                    set(ax,'zlimmode','auto',zdr{:})
                else
                    if any(zlm<0) && strcmp(get(ax,'zscale'),'log')
                        set(ax,'zscale','linear')
                    end
                    set(ax,'zlim',zlm,zdr{:})
                    setappdata(ax,'zlimmode','manual')
                end
            end
            setaxesprops(ax)
            d3d_qp refreshaxprop
            if logfile
                if strcmp(xdr{2},'reverse')
                    if isnumeric(xlm)
                        xlm = fliplr(xlm);
                    else
                        xlm = 'auto-reverse';
                    end
                end
                if strcmp(ydr{2},'reverse')
                    if isnumeric(ylm)
                        ylm = fliplr(ylm);
                    else
                        ylm = 'auto-reverse';
                    end
                end
                if ~isempty(zlm)
                    if strcmp(zdr{2},'reverse')
                        if isnumeric(zlm)
                            zlm = fliplr(zlm);
                        else
                            zlm = 'auto-reverse';
                        end
                    end
                    zlm = {zlm};
                else
                    zlm = {};
                end
                writelog(logfile,logtype,cmd,xlm,ylm,zlm{:});
            end
        end
        
    case 'axesgrid'
        ax = qpsa;
        PM = UD.PlotMngr;
        xgrid = get(PM.X.Grid,'value');
        ygrid = get(PM.Y.Grid,'value');
        zgrid = get(PM.Z.Grid,'value');
        if ~isempty(cmdargs)
            xgrid = cmdargs{1};
            ygrid = cmdargs{2};
            if length(cmdargs)>2
                zgrid = cmdargs{3};
            end
        end
        if isequal(size(xgrid),[1 1]) && isequal(size(ygrid),[1 1]) && isequal(size(zgrid),[1 1])
            xgr = valuemap(xgrid,[1 0],{'on' 'off'});
            ygr = valuemap(ygrid,[1 0],{'on' 'off'});
            zgr = valuemap(zgrid,[1 0],{'on' 'off'});
            set(ax,'xgrid',xgr,'ygrid',ygr,'zgrid',zgr);
            if strcmp(get(PM.Z.Grid,'enable'),'off')
                zgr = [];
            end
            d3d_qp refreshaxprop
            if logfile
                if isempty(zgr)
                    writelog(logfile,logtype,cmd,xgrid,ygrid);
                else
                    writelog(logfile,logtype,cmd,xgrid,ygrid,zgrid);
                end
            end
        end
        
    case 'axesaspectreset'
        ax = qpsa;
        PM = UD.PlotMngr;
        hordas_auto = getappdata(ax,'haspectdefaultvalue');
        if ~isempty(hordas_auto)
            set(PM.Y.AspectValue, ...
                'string',num2str(1/hordas_auto), ...
                'userdata',1/hordas_auto);
        end
        d3d_qp axesaspect
        
    case 'axesaspect'
        ax = qpsa;
        PM = UD.PlotMngr;
        if strcmp(get(PM.Y.AspectTxt,'enable'),'off')
            yaspect = '';
        elseif get(PM.Y.AspectAuto,'value')
            yaspect = 'auto';
        else
            yaspect = str2double(get(PM.Y.AspectValue,'string'));
            if yaspect>0
                % ok
            else
                yaspect = get(PM.Y.AspectValue,'userdata');
            end
        end
        if strcmp(get(PM.Z.AspectTxt,'enable'),'off')
            zaspect = '';
        elseif get(PM.Z.AspectAuto,'value')
            zaspect = 'auto';
        else
            zaspect = str2double(get(PM.Z.AspectValue,'string'));
            if zaspect>0
                % ok
            else
                zaspect = get(PM.Z.AspectValue,'userdata');
            end
            if isempty(zaspect)
                zaspect = 1;
            end
        end
        if ~isempty(cmdargs)
            y = cmdargs{1};
            if strcmpi(y,'auto')
                yaspect = 'auto';
            elseif isscalar(y) && isnumeric(y) && y>0
                yaspect = y;
            end
            if length(cmdargs)>1
                z = cmdargs{2};
                if strcmpi(z,'auto')
                    zaspect = 'auto';
                elseif isscalar(z) && isnumeric(z) && z>0
                    zaspect = z;
                end
            end
        end
        if getappdata(ax,'axes2d')
            zaspect = [];
        end
        if strcmp(yaspect,'auto')
            hordas_auto = getappdata(ax,'haspectautovalue');
            %if isempty(hordas_auto) %truly auto
                set(ax,'dataAspectratioMode','auto');
            %else
            %    set(ax,'dataAspectratio',[1 1/hordas_auto 1]);
            %end
            if ~getappdata(ax,'axes2d')
                zaspect = 'auto';
            end
        else
            y = yaspect;
            if getappdata(ax,'axes2d')
                z = 1;
            elseif strcmp(zaspect,'auto')
                zaspect = 1;
                z = 1;
            else
                z = zaspect;
            end
            set(ax,'dataaspectratio',[1 1/y 1/z]);
        end
        d3d_qp refreshaxprop
        if logfile
            if isempty(zaspect)
                writelog(logfile,logtype,cmd,yaspect);
            else
                writelog(logfile,logtype,cmd,yaspect,zaspect);
            end
        end
        
    case 'axesloc'
        ax = qpsa;
        PM = UD.PlotMngr;
        xlcs = get(PM.X.Loc,'string');
        ylcs = get(PM.Y.Loc,'string');
        if isempty(cmdargs)
            xlc = xlcs{get(PM.X.Loc,'value')};
            ylc = ylcs{get(PM.Y.Loc,'value')};
        else
            xlc = cmdargs{1};
            ylc = cmdargs{2};
        end
        ixlc = ustrcmpi(xlc,xlcs);
        iylc = ustrcmpi(ylc,ylcs);
        if ixlc>0 && iylc>0
            set(ax,'xaxislocation',xlc,'yaxislocation',ylc);
            d3d_qp refreshaxprop
            if logfile
                writelog(logfile,logtype,cmd,xlc,ylc);
            end
        end
        
    case 'axesdir'
        ax = qpsa;
        PM = UD.PlotMngr;
        xdirs = get(PM.X.Dir,'string');
        ydirs = get(PM.Y.Dir,'string');
        zdirs = get(PM.Z.Dir,'string');
        if isempty(cmdargs)
            xdr = xdirs{get(PM.X.Dir,'value')};
            ydr = ydirs{get(PM.Y.Dir,'value')};
            zdr = ydirs{get(PM.Z.Dir,'value')};
        else
            xdr = cmdargs{1};
            ydr = cmdargs{2};
            zdr = cmdargs{3};
        end
        ixdr = ustrcmpi(xdr,xdirs);
        iydr = ustrcmpi(ydr,ydirs);
        izdr = ustrcmpi(zdr,zdirs);
        if ixdr>0 && iydr>0 && izdr>0
            set(ax,'xdir',xdr,'ydir',ydr,'zdir',zdr);
            d3d_qp refreshaxprop
            if logfile
                writelog(logfile,logtype,cmd,xdr,ydr,zdr);
            end
        end
        
    case 'axesscale'
        ax = qpsa;
        PM = UD.PlotMngr;
        scales = get(PM.X.Scale,'string');
        if isempty(cmdargs)
            xsc = scales{get(PM.X.Scale,'value')};
            ysc = scales{get(PM.Y.Scale,'value')};
            zsc = scales{get(PM.Z.Scale,'value')};
        else
            xsc = cmdargs{1};
            ysc = cmdargs{2};
            if length(cmdargs)>2
                zsc = cmdargs{3};
            else
                zsc = scales{get(PM.Z.Scale,'value')};
            end
        end
        ixsc = ustrcmpi(xsc,scales);
        iysc = ustrcmpi(ysc,scales);
        izsc = ustrcmpi(zsc,scales);
        if ixsc>0 && iysc>0 && izsc>0
            set(ax,'xscale',xsc,'yscale',ysc,'zscale',zsc);
            d3d_qp refreshaxprop
            if logfile
                writelog(logfile,logtype,cmd,xsc,ysc,zsc);
            end
        end
        
    case 'axesboxed'
        ax = qpsa;
        if isempty(cmdargs)
            PM = UD.PlotMngr;
            lbox = get(PM.AxBox,'value');
        else
            lbox = cmdargs{1};
        end
        if isequal(size(lbox),[1 1])
            sbox = valuemap(lbox,[1 0],{'on' 'off'});
            set(ax,'box',sbox)
            d3d_qp refreshaxprop
            if logfile
                writelog(logfile,logtype,cmd,lbox);
            end
        end
        
    case 'axeslinewidth'
        ax = qpsa;
        if isempty(cmdargs)
            PM = UD.PlotMngr;
            lw = str2double(get(PM.AxLineWidth,'string'));
        else
            lw = cmdargs{1};
        end
        if isequal(size(lw),[1 1]) && ~isnan(lw)
            set(ax,'linewidth',lw)
            d3d_qp refreshaxprop
            if logfile
                writelog(logfile,logtype,cmd,lw);
            end
        end
        
    case 'axesposition'
        ax = qpsa;
        PM = UD.PlotMngr;
        posi = get(PM.AxPosUnit,'value');
        posilist = get(PM.AxPosUnit,'string');
        if isempty(cmdargs)
            pos(1,1) = str2double(get(PM.AxXLowerLeft,'string'));
            pos(1,2) = str2double(get(PM.AxYLowerLeft,'string'));
            pos(1,3) = str2double(get(PM.AxWidth,'string'));
            pos(1,4) = str2double(get(PM.AxHeight,'string'));
            if posi==3
                pos = pos/100;
            end
        else
            if length(cmdargs)==2 && ischar(cmdargs{1})
                posinew = valuemap(cmdargs{1},posilist,1:length(posi));
                if posinew~=posi
                    set(ax,'unit',valuemap(posinew,1:3,{'centimeters','inches','normalized'}))
                    posi = posinew;
                end
                pos = cmdargs{2};
            else
                pos = cmdargs{1};
            end
            if ischar(pos)
                pos = str2vec(pos);
            end
            if posi==3
                pos = pos/100;
            end
        end
        if isequal(size(pos),[1 4])
            set(ax,'position',pos);
            d3d_qp refreshaxprop
            if logfile
                writelog(logfile,logtype,cmd,pos);
            end
        end
        
    case 'axesposunit'
        ax = qpsa;
        PM = UD.PlotMngr;
        posulist = get(PM.AxPosUnit,'string');
        posilist = 1:length(posulist);
        if isempty(cmdargs)
            posi = get(PM.AxPosUnit,'value');
        else
            posi = valuemap(cmdargs{1},posulist,posilist);
        end
        posslist = {'centimeters','inches','normalized'};
        set(ax,'unit',posslist{posi});
        d3d_qp refreshaxprop
        if logfile
            writelog(logfile,logtype,cmd,posulist{posi});
        end
        
    case {'xlabel','ylabel','zlabel','title'}
        ax = qpsa;
        PM = UD.PlotMngr;
        if strcmp(cmd,'title')
            XLblAuto = PM.('AxTitleAuto');
            XLbl     = PM.('AxTitle');
            xlbl     = 'title';
            x        = 'title';
        else
            x        = cmd(1);
            X        = upper(x);
            XLblAuto = PM.(X).LabelAuto;
            XLbl     = PM.(X).Label;
            xlbl     = [x 'label'];
        end
        %
        if isempty(cmdargs)
            auto = get(XLblAuto,'value');
            if ~auto
                lbl = get(XLbl,'string');
            end
        else
            lbl = varargin{1};
            auto = strcmp(lbl,'<automatic>');
        end
        %
        set(XLblAuto,'value',auto)
        if auto
            if isappdata(ax,xlbl)
                rmappdata(ax,xlbl)
            end
        else
            setappdata(ax,xlbl,lbl)
        end
        %
        if auto
            lbl = '<automatic>';
            if isappdata(ax,[xlbl 'auto'])
                expanded_lbl = getappdata(ax,[xlbl 'auto']);
            else
                expanded_lbl = '';
            end
        else
            quantity = getappdata(ax,[x 'quantity']);
            unit = getappdata(ax,[x 'unit']);
            if isempty(quantity)
                quantity = '';
            end
            if isempty(unit)
                unit = '';
            end
            expanded_lbl = qp_strrep(lbl,'%quantity%',quantity);
            expanded_lbl = qp_strrep(expanded_lbl,'%unit%',unit);
        end
        if ~isempty(strfind(expanded_lbl,'\n{}'))
            expanded_lbl = strsplit(expanded_lbl,'\\n{}');
        end
        %
        switch x
            case 'title'
                qp_title('update',ax)
            case 'x'
                xlabel(ax,expanded_lbl)
            case 'y'
                ylabel(ax,expanded_lbl)
            case 'z'
                zlabel(ax,expanded_lbl)
        end
        if strcmp(cmd,'title')
            d3d_qp refreshaxes
        else
            d3d_qp refreshaxprop
        end
        %
        if logfile
            writelog(logfile,logtype,cmd,lbl);
        end
        
    case 'zoomdown'
        zoom(gcbf,'down');
        qp_updateaxes(gcbf,[])
        
    case 'zoomin'
        %  putdowntext('zoomin',gcbo)
        if matlabversionnumber<7 || qp_settings('v6zoombehavior')
            switch zoom(gcbf,'getmode')
                case {'off','out'}
                    if matlabversionnumber>=7
                        pan(gcbf,'off')
                    end
                    zoom(gcbf,'inmode');
                    set(gcbf,'windowbuttondownfcn','d3d_qp zoomdown');
                case {'in'}
                    zoom(gcbf,'off');
            end
        elseif matlabversionnumber<7.02
            F = gcbf;
            switch zoom(F,'getmode')
                case {'off','out'}
                    zoom(gcbf,'inmode')
                    WBDZIF = getappdata(F,'WrappedButtonDownZoomInFcn');
                    %
                    if isempty(WBDZIF)
                        WBDZIF = get(F,'WindowButtonDownFcn');
                        WBUZIF = get(F,'WindowButtonUpFcn');
                        setappdata(F,'WrappedButtonDownZoomInFcn',WBDZIF)
                        setappdata(F,'WrappedButtonUpZoomInFcn0',WBUZIF)
                    end
                    set(F,'WindowButtonDownFcn','d3d_qp zoomindown');
                    set(F,'WindowButtonUpFcn','d3d_qp zoominup');
                otherwise
                    zoom(F,'off')
            end
        elseif matlabversionnumber<7.03
            F = gcbf;
            switch zoom(F,'getmode')
                case {'off','out'}
                    zoom(gcbf,'inmode')
                    WBDZIF = getappdata(F,'WrappedButtonDownZoomInFcn');
                    mmgr = uigetmodemanager(F);
                    zoominM = mmgr.CurrentMode;
                    %
                    if isempty(WBDZIF)
                        WBDZIF = get(F,'WindowButtonDownFcn');
                        WBUZIF = get(F,'WindowButtonUpFcn');
                        setappdata(F,'WrappedButtonDownZoomInFcn',WBDZIF)
                        setappdata(F,'WrappedButtonUpZoomInFcn0',WBUZIF)
                        zoominM.WindowButtonDownFcn = 'd3d_qp zoomindown';
                        zoominM.WindowButtonUpFcn = 'd3d_qp zoominup';
                    end
                otherwise
                    zoom(F,'off')
            end
        else
            h = zoom(gcbf);
            if strcmpi(get(gcbo,'State'),'on')
                set(h,'Direction','in')
                set(h,'ActionPostCallback',@qp_updateaxes);
                set(h,'Enable','on');
            else
                set(h,'Enable','off');
            end
        end
        
    case 'zoomout'
        %  putdowntext('zoomout',gcbo)
        if matlabversionnumber<7 || qp_settings('v6zoombehavior')
            switch zoom(gcbf,'getmode')
                case {'off','in'}
                    if matlabversionnumber>=7
                        pan(gcbf,'off')
                    end
                    zoom(gcbf,'outmode');
                    set(gcbf,'windowbuttondownfcn','d3d_qp zoomdown');
                case 'out'
                    zoom(gcbf,'off');
            end
        elseif matlabversionnumber<7.02
            F = gcbf;
            switch zoom(F,'getmode')
                case {'off','in'}
                    zoom(gcbf,'outmode')
                    WBDZOF = getappdata(F,'WrappedButtonDownZoomOutFcn');
                    %
                    if isempty(WBDZOF)
                        WBDZOF = get(F,'WindowButtonDownFcn');
                        setappdata(F,'WrappedButtonDownZoomOutFcn',WBDZOF)
                    end
                    set(F,'WindowButtonDownFcn','d3d_qp zoomoutdown');
                otherwise
                    zoom(F,'off')
            end
        elseif matlabversionnumber<7.03
            F = gcbf;
            switch zoom(F,'getmode')
                case {'off','in'}
                    zoom(gcbf,'outmode')
                    WBDZOF = getappdata(F,'WrappedButtonDownZoomOutFcn');
                    mmgr = uigetmodemanager(F);
                    zoomoutM = mmgr.CurrentMode;
                    %
                    if isempty(WBDZOF)
                        WBDZOF = get(F,'WindowButtonDownFcn');
                        setappdata(F,'WrappedButtonDownZoomOutFcn',WBDZOF)
                        zoomoutM.WindowButtonDownFcn = 'd3d_qp zoomoutdown';
                    end
                otherwise
                    zoom(F,'off')
            end
        else
            h = zoom(gcbf);
            if strcmpi(get(gcbo,'State'),'on')
                set(h,'Direction','out')
                set(h,'ActionPostCallback',@qp_updateaxes);
                set(h,'Enable','on');
            else
                set(h,'Enable','off');
            end
        end
        
    case 'pan'
        % putdowntext('pan',gcbo)
        if qp_settings('v6zoombehavior') && ~strcmp(zoom(gcbf,'getmode'),'off')
            zoom(gcbf,'off')
        end
        if matlabversionnumber<7.0
            % pan feature doesn't exist
        elseif matlabversionnumber<7.02
            F = gcbf;
            if strcmpi(get(gcbo,'State'),'on')
                pan(F,'on')
                WBDPF = getappdata(F,'WrappedButtonDownPanFcn');
                %
                if isempty(WBDPF)
                    WBDPF = get(F,'WindowButtonDownFcn');
                    WBUPF = get(F,'WindowButtonUpFcn');
                    setappdata(F,'WrappedButtonDownPanFcn',WBDPF)
                    setappdata(F,'WrappedButtonUpPanFcn',WBUPF)
                end
                set(F,'WindowButtonDownFcn','d3d_qp pandown');
                set(F,'WindowButtonUpFcn','d3d_qp panup');
            else
                pan(F,'off')
            end
        elseif matlabversionnumber<7.03
            F = gcbf;
            if strcmpi(get(gcbo,'State'),'on')
                pan(F,'on')
                WBDPF = getappdata(F,'WrappedButtonDownPanFcn');
                mmgr = uigetmodemanager(F);
                panM = mmgr.CurrentMode;
                %
                if isempty(WBDPF)
                    WBDPF = get(F,'WindowButtonDownFcn');
                    WBUPF = get(F,'WindowButtonUpFcn');
                    setappdata(F,'WrappedButtonDownPanFcn',WBDPF)
                    setappdata(F,'WrappedButtonUpPanFcn',WBUPF)
                    %
                    panM.WindowButtonDownFcn = 'd3d_qp pandown';
                    panM.WindowButtonUpFcn = 'd3d_qp panup';
                end
                %
                UIcm = panM.UIContextMenu;
                Reset = findobj(UIcm,'Label','Reset to Original View');
                setappdata(F,'WrappedResetFcn',get(Reset,'Callback'))
                set(Reset,'Callback','d3d_qp viewreset')
            else
                pan(F,'off')
            end
        else
            h = pan(gcbf);
            if strcmpi(get(gcbo,'State'),'on')
                set(h,'ActionPostCallback',@qp_updateaxes);
                set(h,'Enable','on');
            else
                set(h,'Enable','off');
            end
        end
        
    case 'zoomindown'
        WBDZIF = getappdata(gcbf,'WrappedButtonDownZoomInFcn');
        WBDZIF{1}(gcbf,[],WBDZIF{2:end})
        %
        F = gcbf;
        if strcmp(get(F,'selectionType'),'normal')
            WBUZIF = getappdata(F,'WrappedButtonUpZoomInFcn');
            if isempty(WBUZIF) && ~isempty(get(F,'WindowButtonUpFcn'))
                WBUZIF = get(F,'WindowButtonUpFcn');
                setappdata(F,'WrappedButtonUpZoomInFcn',WBUZIF)
            end
            %
            if matlabversionnumber>=7.02
                mmgr = uigetmodemanager(F);
                zoominM = mmgr.CurrentMode;
                zoominM.WindowButtonUpFcn = 'd3d_qp zoominup';
            elseif ~isempty(get(F,'WindowButtonUpFcn'))
                set(F,'WindowButtonUpFcn','d3d_qp zoominup')
            end
        elseif strcmp(get(F,'selectionType'),'extend')
        else
            UIm = findall(F,'Label','Zoom Out       Shift-Click');
            if isempty(UIm)
                UIm = findall(F,'Label','Zoom Out       Alt-Click');
            end
            WRZIF = get(UIm,'Callback');
            if ~isequal(WRZIF,'d3d_qp zoominout') && ~isempty(WRZIF)
                setappdata(F,'WrappedZoomInOutFcn',WRZIF)
                set(UIm,'Callback','d3d_qp zoominout')
            end
            %
            UIm = findall(F,'Label','Reset to Original View');
            WRF = get(UIm,'Callback');
            if ~isequal(WRF,'d3d_qp viewreset')
                setappdata(F,'WrappedResetFcn',WRF)
                set(UIm,'Callback','d3d_qp viewreset')
            end
        end
        %
        qp_updateaxes(gcbf,[])
        
    case 'zoomoutdown'
        WBDZOF = getappdata(gcbf,'WrappedButtonDownZoomOutFcn');
        WBDZOF{1}(gcbf,[],WBDZOF{2:end})
        %
        F = gcbf;
        if strcmp(get(F,'selectionType'),'normal')
            if matlabversionnumber>=7.02
                WBUZOF = getappdata(F,'WrappedButtonUpZoomOutFcn');
                if isempty(WBUZOF)
                    WBUZOF = get(F,'WindowButtonUpFcn');
                    setappdata(F,'WrappedButtonUpZoomOutFcn',WBUZOF)
                end
                %
                mmgr = uigetmodemanager(F);
                zoomoutM = mmgr.CurrentMode;
                zoomoutM.WindowButtonUpFcn = 'd3d_qp zoomoutup';
            end
        elseif strcmp(get(F,'selectionType'),'extend')
        else
            UIm = findall(F,'Label','Zoom In        Shift-Click');
            if isempty(UIm)
                UIm = findall(F,'Label','Zoom In        Alt-Click');
            end
            WRZOF = get(UIm,'Callback');
            if ~isequal(WRZOF,'d3d_qp zoominout') && ~isempty(WRZOF)
                setappdata(F,'WrappedZoomInOutFcn',WRZOF)
                set(UIm,'Callback','d3d_qp zoominout')
            end
            %
            UIm = findall(F,'Label','Reset to Original View');
            WRF = get(UIm,'Callback');
            if ~isequal(WRF,'d3d_qp viewreset')
                setappdata(F,'WrappedResetFcn',WRF)
                set(UIm,'Callback','d3d_qp viewreset')
            end
        end
        %
        qp_updateaxes(gcbf,[])
        
    case 'zoominup'
        WBUPF = getappdata(gcbf,'WrappedButtonUpZoomInFcn');
        if ~isempty(WBUPF)
            rmappdata(gcbf,'WrappedButtonUpZoomInFcn')
            WBUPF{1}(gcbf,[],WBUPF{2:end})
        else
            WBUPF = getappdata(gcbf,'WrappedButtonUpZoomInFcn0');
            WBUPF{1}(gcbf,[],WBUPF{2:end})
        end
        qp_updateaxes(gcbf,[])
        
    case 'zoomoutup'
        WBUPF = getappdata(gcbf,'WrappedButtonUpZoomOutFcn');
        WBUPF{1}(gcbf,[],WBUPF{2:end})
        qp_updateaxes(gcbf,[])
        
    case 'zoominout'
        WBUPF = getappdata(gcbf,'WrappedZoomInOutFcn');
        WBUPF{1}(gcbo,[],WBUPF{2})
        qp_updateaxes(gcbf,[])
        
    case 'pandown'
        WBDPF = getappdata(gcbf,'WrappedButtonDownPanFcn');
        WBDPF{1}(gcbf,[],WBDPF{2:end})
        if matlabversionnumber<7.02 && ...
                strcmp(get(gcf,'selectionType'),'alt')
            Reset = findall(gcbf,'Label','Reset to Original View');
            if ~strcmp(get(Reset,'Callback'),'d3d_qp viewreset')
                setappdata(gcbf,'WrappedResetFcn',get(Reset,'Callback'))
                set(Reset,'Callback','d3d_qp viewreset')
            end
        end
        
    case 'panup'
        WBUPF = getappdata(gcbf,'WrappedButtonUpPanFcn');
        WBUPF{1}(gcbf,[],WBUPF{2:end})
        qp_updateaxes(gcbf,[])
        
    case 'viewreset'
        WRF = getappdata(gcbf,'WrappedResetFcn');
        WRF{1}(gcbo,[],WRF{2})
        qp_updateaxes(gcbf,[])
        
    case 'rotate3d'
        %  putdowntext('rotate3d',gcbo)
        rotate3d(gcbf,get(gcbo,'State'));
        
    case 'editborder'
        Brdr=findobj(gcbf,'type','axes','tag','border');
        if ~isempty(Brdr)
            md_paper('edit',Brdr)
        end
        
    case {'closefigure','savefigure','saveasfigure','printfigure','clipbitmap','clipmeta'}
        Fig=[];
        if isempty(UD) % if quickplot is not active do not activate it ...
            Fig=gcbf;
        elseif isempty(gcbf) || isequal(get(gcbf,'handlevisibility'),'off')
            FigIDs=get(UD.PlotMngr.FigList,'userdata');
            if get(UD.PlotMngr.FigAll,'value') % all figures
                Fig=FigIDs;
            else
                FigVal=get(UD.PlotMngr.FigList,'value');
                if FigVal<=length(FigIDs)
                    Fig=FigIDs(FigVal);
                end
            end
        else
            Fig=gcbf;
        end
        if ishandle(Fig)
            args={};
            switch cmd
                case 'closefigure'
                    AllObj=findall(Fig);
                    for o = 1:length(AllObj)
                        try
                            set(AllObj(o),'deletefcn','');
                        catch
                            % Some objects (e.g. AnnotationPane) don't have
                            % a deletefcn. Skip these.
                        end
                    end
                    delete(Fig);
                    if ~isempty(UD) % if quickplot is not active do not activate it ...
                        d3d_qp refreshfigs
                    end
                case {'savefigure','saveasfigure'}
                    fullfilename = get(Fig,'filename');
                    set(Fig,'menubar','figure','closerequestfcn','closereq')
                    cbar = findall(Fig,'deletefcn','qp_colorbar delete');
                    set(cbar,'deletefcn','colorbar(''delete'')')
                    if strcmp(cmd,'savefigure') && ~isempty(fullfilename)
                        % fullfilename specified
                    elseif ~isempty(cmdargs)
                        fullfilename = cmdargs{1};
                        saveas(Fig,fullfilename)
                    else
                        [p,f,e] = fileparts(fullfilename);
                        if isempty(f)
                            f = listnames(Fig,'showType','no','showHandle','no','showTag','no');
                            f = str2file(f{1});
                            if isempty(f)
                                f = 'untitled';
                            end
                        end
                        e = '.fig';
                        f = [f e];
                        if ~isempty(p)
                            f = fullfile(p,f);
                        else
                            p = qp_settings('figuredir');
                            if ~isempty(p)
                                f = fullfile(p,f);
                            end
                        end
                        filterspec = {'*.fig'   'MATLAB figure file'
                                      '*.qpses' 'QUICKPLOT session file'};
                        [newfile, newpath] = uiputfile(filterspec, 'Save As', f);
                        if ischar(newfile)
                            qp_settings('figuredir',newpath)
                            if isempty(e)
                                newfile = [newfile e];
                            end
                            fullfilename = fullfile(newpath,newfile);
                        else
                            fullfilename = 0;
                        end
                    end
                    if ischar(fullfilename)
                        [p,f,e] = fileparts(fullfilename);
                        switch e
                            case '.qpses'
                                SES = qp_session('extract',Fig);
                                SER = qp_session('serialize',SES);
                                SER = qp_session('make_expandables',SER,{'filename','domain'});
                                qp_session('save',SER,fullfilename)
                            otherwise
                                saveas(Fig,fullfilename);
                        end
                        set(Fig,'filename',fullfilename);
                    end
                    set(Fig,'menubar','none','closerequestfcn','d3d_qp closefigure')
                    set(cbar,'deletefcn','qp_colorbar delete')
                case {'printfigure','clipbitmap','clipmeta'}
                    if ~strncmp(computer,'PC',2) && ~isempty(strmatch(cmd,{'clipbitmap','clipmeta'}))
                        ui_message('error','Function only available on Windows PC.')
                    elseif isstandalone && matlabversionnumber<6
                        ui_message('error','Printing not available in this standalone version.')
                    else
                        sld=findobj(Fig,'tag','animslid');
                        psh=findobj(Fig,'tag','animpush');
                        set(sld,'vis','off')
                        set(psh,'vis','off')
                        switch cmd
                            case 'clipbitmap'
                                I.PrtID='Bitmap to clipboard';
                                I.Method=2;
                                I.AllFigures=1;
                                I.DPI=72;
                                I.InvertHardcopy=0;
                                md_print(Fig,I)
                            case 'clipmeta'
                                I.PrtID='Metafile to clipboard';
                                I.Method=1;
                                I.AllFigures=1;
                                I.InvertHardcopy=1;
                                md_print(Fig,I)
                            otherwise
                                if ~isempty(cmdargs)
                                    I.PrtID=cmdargs{2};
                                    I.Method=cmdargs{3};
                                    I.DPI=cmdargs{4};
                                    I.AllFigures=1;
                                    I.Color=cmdargs{5};
                                    if length(cmdargs)>5
                                        I.InvertHardcopy=cmdargs{6};
                                    else
                                        I.InvertHardcopy=1;
                                    end
                                    if length(cmdargs)>6
                                        I.PageLabels=cmdargs{7};
                                    else
                                        I.PageLabels=1;
                                    end
                                    args={I cmdargs{1}};
                                else
                                    I.PrtID=qp_settings('print_ID');
                                    I.Method=qp_settings('print_method');
                                    I.DPI=qp_settings('print_DPI');
                                    I.AllFigures=0;
                                    I.Color=qp_settings('print_colour');
                                    I.InvertHardcopy=qp_settings('print_inverthardcopy');
                                    I.PageLabels=qp_settings('print_pagelabels');
                                    I.SelectFrom=get_nondialogs;
                                    [I,FigNew]=md_print('getsettings',Fig,I);
                                    if isempty(I)
                                        return
                                    elseif ~isequal(Fig,FigNew)
                                        set(sld,'vis','on')
                                        set(psh,'vis','on')
                                        Fig=FigNew;
                                        sld=findobj(Fig,'tag','animslid');
                                        psh=findobj(Fig,'tag','animpush');
                                        set(sld,'vis','off')
                                        set(psh,'vis','off')
                                    end
                                    I=rmfield(I,'SelectFrom');
                                    args={I};
                                end
                                if ~isequal(I.PrtID,0)
                                    [I,filename]=md_print(Fig,args{:});
                                end
                                if isequal(I.PrtID,0) || ~ischar(filename)
                                    set(sld,'vis','on')
                                    set(psh,'vis','on')
                                    return
                                end
                                qp_settings('print_ID',I.PrtID);
                                qp_settings('print_method',I.Method);
                                qp_settings('print_DPI',I.DPI);
                                qp_settings('print_colour',I.Color);
                                qp_settings('print_inverthardcopy',I.InvertHardcopy);
                                qp_settings('print_pagelabels',I.PageLabels);
                                args={filename  I.PrtID  I.Method  I.DPI  I.Color  I.InvertHardcopy I.PageLabels};
                        end
                        set(sld,'vis','on')
                        set(psh,'vis','on')
                    end
            end
            if logfile
                writelog(logfile,logtype,cmd,args{:});
            end
        end
        
    case {'run','rerun'}
        PAR.X=[];
        PAR = rmfield(PAR,'X');
        blockcomment=0;
        if isempty(cmdargs)
            [fn,pn]=uigetfile('*.qplog;*.m');
            if ischar(fn)
                runfil=fopen([pn fn],'r','n','UTF-8');
            else
                runfil=-1;
            end
        else
            %
            % Check if -data argument has been specified
            %
            for i = 2:length(cmdargs)
                if isequal('-par',cmdargs{i})
                    PAR = cmdargs{i+1};
                elseif isequal('-data',cmdargs{i})
                    %
                    % Both data file and log file specified. First load
                    % data file and then run log file.
                    %
                    d3d_qp('openfile',cmdargs{i+1:end})
                    cmdargs(i:end)=[];
                    break
                end
            end
            runfil=fopen(cmdargs{1},'r','n','UTF-8');
        end
        cmdargs={};
        if runfil<0
            cmd='';
        end
        if ~isempty(cmd)
            [pn,fn,ex]=fileparts(fopen(runfil));
            runningtype= ex(2:end);
            logfilerun=findobj(mfig,'tag','run','type','uimenu');
            if ~isempty(get(logfilerun,'callback'))
                set(logfilerun,'callback','');
                OtherLog=uimenu(logfilerun,'label','&other ...','separator','on','callback','d3d_qp run');
            else
                OtherLog=findobj(logfilerun,'label','&other ...');
            end
            filename=fopen(runfil);
            %
            c=setdiff(get(logfilerun,'children'),OtherLog);
            LogCallBack=sprintf('d3d_qp run ''%s''',filename);
            LogNr=10;
            LogId=[];
            set(OtherLog,'visible','off')
            for i=1:length(c)
                Str=get(c(i),'callback');
                if strcmp(Str,LogCallBack),
                    Str=get(c(i),'label');
                    LogId=i;
                    LogNr=Str(2)-48;
                    break
                end
            end
            for i=1:length(c)
                Str=get(c(i),'label');
                Nr=Str(2)-48;
                if Nr<LogNr
                    if strcmp(Str(2),'9')
                        delete(c(i));
                    else
                        Str(2)=Str(2)+1;
                        set(c(i),'label',Str);
                    end
                end
            end
            if isempty(LogId)
                uimenu(logfilerun,'label',sprintf('&1 %s',abbrevfn(filename)),'separator','off','callback',LogCallBack,'position',1);
            else
                set(c(LogId),'label',sprintf('&1 %s',abbrevfn(filename)),'position',1);
            end
            set(OtherLog,'visible','on')
            %
            rerun=findobj(mfig,'tag','rerun');
            set(rerun,'enable','on','ClickedCallback',LogCallBack,'TooltipString',sprintf('Run ''%s'' again.',filename))
        end
        %
        stop   = 0;
        inLine = 0;
        while ~isempty(cmd) && ~stop
            cmdstr='';
            while isempty(cmdstr) || ismember(cmdstr(1),'%*#') || blockcomment
                if length(cmdstr)>1 && ismember(cmdstr(1),'%*#')
                    if cmdstr(2)=='['
                        blockcomment=1;
                    elseif cmdstr(2)==']'
                        blockcomment=0;
                    end
                end
                cmdstr=fgetl(runfil);
                inLine=inLine+1;
                if ~ischar(cmdstr)
                    break
                end
                cmdstr=deblank(cmdstr);
            end
            cmdargs={};
            if ischar(cmdstr) && ~isempty(cmdstr)
                cmdstr = qp_strrep(cmdstr,PAR,'$');
                if strcmpi(runningtype,'m') || cmdstr(1)=='>'
                    if cmdstr(1)=='>'
                        cmdstr = cmdstr(2:end);
                    end
                    try
                        eval(cmdstr)
                    catch Ex
                        qp_error(sprintf('Error evaluating line %i:\n%s',inLine,cmdstr),Ex)
                        stop = qp_settings('stopruniferror');
                    end
                else
                    [cmd,cmdargs]=qp_cmdstr(cmdstr);
                    if strcmp(cmd,'run')
                        d3d_qp(cmd,cmdargs{:});
                    else
                        try
                            d3d_qp_core(cmd,cmdargs{:});
                        catch Ex
                            qp_error(sprintf('Error executing command "%s" in line %i',cmd,inLine),Ex)
                            stop = qp_settings('stopruniferror');
                        end
                    end
                end
            else
                cmd='';
            end
        end
        if runfil>0
            try
                fclose(runfil);
            end
        end
        
    case 'closeallfig'
        delete(get_nondialogs)
        d3d_qp refreshfigs
        
    case {'hselectiontype','vselectiontype','hselectiontype*'}
        updategridview=1;
        if cmd(end)=='*'
            updategridview=0;
            cmd=cmd(1:end-1);
        end
        MW=UD.MainWin;
        if cmd(1)=='h'
            MWSelType = MW.HSelType;
        else
            MWSelType = MW.VSelType;
        end
        if ~isempty(cmdargs)
            news = cmdargs{1};
            ops = get(MWSelType,'string');
            newi = strmatch(news,ops,'exact');
            if length(newi)~=1
                newi = strmatch(news,ops);
                if length(newi)~=1
                    newi = get(MWSelType,'value');
                end
            end
        else
            newi = get(MWSelType,'value');
        end
        set(MWSelType,'value',newi)
        news = getvalstr(MWSelType);
        switch news
            case 'M range and N range'
                set([MW.MN MW.EditMN MW.MNrev MW.MN2XY MW.MN2M],'visible','off')
                set([MW.XY MW.EditXY MW.LoadXY MW.SaveXY],'visible','off')
                set([MW.M MW.AllM MW.EditM MW.MaxM],'visible','on')
                set([MW.N MW.AllN MW.EditN MW.MaxN],'visible','on')
            case '(M,N) point/path'
                set([MW.M MW.AllM MW.EditM],'visible','off')
                set([MW.N MW.AllN MW.EditN],'visible','off')
                set([MW.XY MW.EditXY MW.LoadXY MW.SaveXY],'visible','off')
                set([MW.MN MW.EditMN MW.MNrev MW.MN2XY],'enable','on','visible','on')
                %
                Props=get(MW.Field,'userdata');
                fld=get(MW.Field,'value');
                if ~isempty(Props) && isfield(Props,'DimFlag') && Props(fld).DimFlag(M_) && ~Props(fld).DimFlag(N_)
                    set(MW.MN2M,'enable','on','visible','on')
                else
                    set(MW.MN2M,'visible','off')
                end
                set([MW.MaxM MW.MaxN],'visible','on')
            case '(X,Y) point/path'
                set([MW.M MW.AllM MW.EditM MW.MaxM],'visible','off')
                set([MW.N MW.AllN MW.EditN MW.MaxN],'visible','off')
                set([MW.MN MW.EditMN MW.MNrev MW.MN2XY MW.MN2M],'visible','off')
                set([MW.XY MW.EditXY MW.LoadXY MW.SaveXY],'enable','on','visible','on')
            case 'K range'
                set([MW.Z MW.EditZ],'visible','off')
                set([MW.K MW.AllK MW.EditK MW.MaxK],'visible','on')
            case {'Z slice','dZ below surface','dZ above bed'}
                set([MW.K MW.AllK MW.EditK MW.MaxK],'visible','off')
                set([MW.Z MW.EditZ],'visible','on')
                set(MW.Z,'string',strtok(news))
            case {'depth percentage'}
                set([MW.K MW.AllK MW.EditK MW.MaxK],'visible','off')
                set([MW.Z MW.EditZ],'visible','on')
                set(MW.Z,'string','%')
        end
        
        d3d_qp updateoptions
        if updategridview
            d3d_qp('gridview_update');
        end
        if logfile
            wrcmd=cmd;
            if cmd(end)=='*'
                wrcmd=cmd(1:end-1);
            end
            writelog(logfile,logtype,wrcmd,news);
        end
        
    case {'m,n','(mn)','(xy)','m,n*','(mn)*','(xy)*','k','z'}
        if cmd(end)=='*'
            star='*';
            cmd=cmd(1:end-1);
        else
            star='';
        end
        switch cmd
            case 'm,n'
                d3d_qp(['hselectiontype' star],'M range and N range');
            case '(mn)'
                d3d_qp(['hselectiontype' star],'(M,N) point/path');
            case '(xy)'
                d3d_qp(['hselectiontype' star],'(X,Y) point/path');
            case 'k'
                d3d_qp('vselectiontype','K range');
            case 'z'
                d3d_qp('vselectiontype','Z slice');
        end
        
    case 'reset'
        if logfile
            writelog(logfile,logtype,cmd);
        end
        MW=UD.MainWin;
        State=qp_state_startup;
        set(MW.T,'enable','off','UserData',[])
        set(MW.AllT,'enable','off','value',0,'UserData',[])
        set(MW.EditT,'enable','off','string','','backgroundcolor',Inactive,'UserData',[])
        set(MW.MaxT,'enable','off','string','( - )','UserData',1)
        set(MW.ShowT,'enable','off','value',0,'UserData',[])
        set(MW.TList,'enable','off','value',[],'string','','UserData',0)
        set(MW.S,'enable','off','UserData',[])
        set(MW.AllS,'enable','off','value',0,'UserData',[],'visible','off')
        set(MW.EditS,'enable','off','string','','backgroundcolor',Inactive,'UserData',[],'visible','off')
        set(MW.MaxS,'enable','off','string','( - )','UserData',1,'visible','off')
        set(MW.StList,'enable','off','value',1,'string',' ','backgroundcolor',Inactive,'UserData',[],'visible','on')
        set(MW.Stat,'visible','off')
        set(MW.HSelType,'String',{'M range and N range','(M,N) point/path','(X,Y) point/path'},'value',1)
        set(MW.VSelType,'String',{'K range','Z slice','dZ below surface','dZ above bed','depth percentage'},'value',1)
        set([MW.MN MW.MN2XY],'visible','off')
        set(MW.EditMN,'string','','Userdata',[],'visible','off')
        set([MW.XY MW.LoadXY MW.SaveXY],'visible','off')
        set(MW.EditXY,'string','','Userdata',[],'visible','off')
        set(MW.M,'enable','off','UserData',[],'visible','on')
        set(MW.AllM,'enable','off','value',1,'UserData',[],'visible','on')
        set(MW.EditM,'enable','off','string','','backgroundcolor',Inactive,'UserData',[],'visible','on')
        set(MW.MaxM,'enable','off','string','( - )','UserData',1,'visible','on')
        set(MW.N,'enable','off','UserData',[],'visible','on')
        set(MW.AllN,'enable','off','value',1,'UserData',[],'visible','on')
        set(MW.EditN,'enable','off','string','','backgroundcolor',Inactive,'UserData',[],'visible','on')
        set(MW.MaxN,'enable','off','string','( - )','UserData',1,'visible','on')
        set(MW.K,'enable','off','UserData',[],'visible','on')
        set(MW.AllK,'enable','off','value',1,'UserData',[],'visible','on')
        set(MW.EditK,'enable','off','string','','backgroundcolor',Inactive,'UserData',[],'visible','on')
        set(MW.MaxK,'enable','off','string','( - )','UserData',1,'visible','on')
        set(MW.Z,'visible','off')
        set(MW.EditZ,'string','','Userdata',[],'visible','off')
        set(MW.LoadData,'enable','off')
        set(MW.DefVar,'enable','off')
        set(MW.QuickV,'enable','off')
        set(MW.Add2Plot,'enable','off')
        set(UOH,'enable','off','visible','off','backgroundcolor',Inactive)
        %
        % ------ axes type ...
        %
        set(findobj(UOH,'tag','axestype=?'),'value',1,'string',{''})
        %
        % ------ time axis - time zone ...
        %
        set(findobj(UOH,'tag','axestimezone=?'),'value',1)
        %
        % ------ plot coordinate ...
        %
        set(findobj(UOH,'tag','plotcoordinate=?'),'value',1,'string',{' '})
        %
        % ------ component ...
        %
        set(findobj(UOH,'tag','component=?'),'value',1,'string',{' '})
        %
        % ------ presenttype ...
        %
        set(findobj(UOH,'tag','presenttype=?'),'value',1,'string',{' '})
        set(findobj(UOH,'tag','extend2edge'),'value',0)
        %
        % ------ colour tracks ...
        %
        set(findobj(UOH,'tag','trackcolour=?'),'value',1,'string',{' '})
        %
        % ------ colour vectors ...
        %
        set(findobj(UOH,'tag','vectorcolour=?'),'value',1,'string',{' '})
        set(findobj(UOH,'tag','colourvectors'),'value',0)
        %
        % ------ data units ...
        %
        set(findobj(UOH,'tag','dataunits=?'),'value',1)
        %
        % ------ angle convention ...
        %
        set(findobj(UOH,'tag','angleconvention=?'),'value',1)
        %
        % ------ vector style ...
        %
        set(findobj(UOH,'tag','vectorstyle=?'),'value',1)
        %
        % ------ vector scaling ...
        %
        set(findobj(UOH,'tag','vecscalem=?'),'value',1)
        set(findobj(UOH,'tag','1vecunit=?'),'userdata',1,'string','1')
        %
        % ------ vertical scaling ...
        %
        set(findobj(UOH,'tag','vertscalem=?'),'value',1)
        set(findobj(UOH,'tag','vscale=?'),'userdata',1,'string','1')
        %
        % ------ format string
        %
        set(findobj(UOH,'tag','numformat=?'),'string','%.2f')
        %
        % ------ font characteristics
        %
        set(findobj(UOH,'tag','fontsize=?'),'userdata',6,'string','6')
        %
        % ------ label alignment
        %
        set(findobj(UOH,'tag','horizontalalignment=?'),'value',2)
        set(findobj(UOH,'tag','verticalalignment=?'),'value',3)
        %
        % ------ colour dams ...
        %
        set(findobj(UOH,'tag','colourdams'),'value',0)
        %
        % ------ value operator ...
        %
        set(findobj(UOH,'tag','operator'),'value',1)
        %
        % ------ uniform colours ...
        %
        set(findobj(UOH,'tag','unicolour'),'value',0)
        %
        % ------ colour ...
        %
        set(findobj(UOH,'tag','colour=?'),'backgroundcolor',State.colour,'userdata',State.colour)
        %
        % ------ face colour ...
        %
        set(findobj(UOH,'tag','fillpolygons'),'value',0)
        set(findobj(UOH,'tag','facecolour=?'),'backgroundcolor',State.facecolour,'userdata',State.facecolour)
        %
        % ------ text box colour ...
        %
        set(findobj(UOH,'tag','textbox=?'),'value',0)
        set(findobj(UOH,'tag','textboxfacecolour=?'),'backgroundcolor',State.textboxfacecolour,'userdata',State.textboxfacecolour)
        %
        % ------ marker ...
        %
        L=set(mfig,'defaultlinemarker');
        Li=strmatch('none',L,'exact');
        if isempty(Li)
            Li=1;
        end
        set(findobj(UOH,'tag','marker=?'),'value',Li)
        %
        % ------ linestyle ...
        %
        L=set(mfig,'defaultlinelinestyle');
        Li=strmatch('-',L,'exact');
        if isempty(Li)
            Li=1;
        end
        set(findobj(UOH,'tag','linestyle=?'),'value',Li)
        set(findobj(UOH,'tag','linewidth=?'),'userdata',0.5,'string','0.5')
        %
        % ------ markersize ...
        %
        set(findobj(UOH,'tag','markersize=?'),'userdata',6,'string','6')
        %
        % ------ marker colour ...
        %
        set(findobj(UOH,'tag','usemarkercolour'),'value',0)
        set(findobj(UOH,'tag','markercolour=?'),'backgroundcolor',State.markercolour,'userdata',State.markercolour)
        %
        % ------ marker fill colour ...
        %
        set(findobj(UOH,'tag','usemarkerfillcolour'),'value',0)
        set(findobj(UOH,'tag','markerfillcolour=?'),'backgroundcolor',State.markerfillcolour,'userdata',State.markerfillcolour)
        %
        % ------ classify colours ...
        %
        set(findobj(UOH,'tag','colclassify'),'value',0)
        %
        % ------ thresholds ...
        %
        set(findobj(UOH,'tag','thresholds=?'),'string','','userdata',[])
        set(findobj(UOH,'tag','threshdistr=?'),'value',1)
        %
        % ------ colour limits ...
        %
        set(findobj(UOH,'tag','climmode=?'),'value',1)
        set(findobj(UOH,'tag','climmax=?'),'userdata',1,'string','1')
        set(findobj(UOH,'tag','climmax=?'),'userdata',0,'string','0')
        %
        % ------ colour map ...
        %
        d3d_qp('colourmap*','jet')
        %
        % ------ colourbar ...
        %
        set(findobj(UOH,'tag','colourbar'),'value',1)
        set(findobj(UOH,'tag','colbarhorz'),'value',0)
        %
        % ------ thin vector/value field ...
        %
        set(findobj(UOH,'tag','thinfld=?'),'value',1)
        set(findobj(UOH,'tag','thinfact=?'),'userdata',1,'string','1')
        set(findobj(UOH,'tag','thindist=?'),'userdata',50,'string','50')
        set(findobj(UOH,'tag','thincount=?'),'userdata',1000,'string','1000')
        %
        % ------ data clipping values ...
        %
        set(findobj(UOH,'tag','clippingvals=?'),'userdata',-999,'string','-999')
        %
        % ------ clip NaNs ...
        %
        set(findobj(UOH,'tag','clipnans'),'value',1)
        %
        % ------ x/y clipping values ...
        %
        set(findobj(UOH,'tag','xclipping=?'),'string','')
        set(findobj(UOH,'tag','yclipping=?'),'string','')
        %
        % ------ export data ...
        %
        set(findobj(UOH,'tag','exporttype=?'),'value',1,'string',{' '})
        set(findobj(UOH,'tag','expformat=?'),'string','%16.7e')
        
        d3d_qp updatedomains
        
    case 'hideversion'
        set(mfig,'name','Delft3D-QUICKPLOT')
        
    case 'showversion'
        set(mfig,'name',cat(2,'Delft3D-QUICKPLOT ',qpversion))
        
    case 'validation'
        qp_validate(cmdargs{:});
        
    case {'preferences','prefpane', ...
            'defaultnewfigure','defaultloadfigure','defaultfigure', ...
            'gridviewbackgroundcolor','gridviewgridcolor', ...
            'gridviewselectioncolor','gridviewlandboundarycolor', ...
            'defaultfigurecolor','gridviewshowindices','changefont', ...
            'defaultaxescolor','boundingbox','v6zoombehavior', ...
            'organizationname','filefilterselection','colorbar_ratio', ...
            'showinactiveopt', 'defaultfigurepos','timezonehandling', ...
            'enforcedtimezone', 'netcdf_use_fillvalue','export_max_ntimes', ...
            'update_showversion', 'defaultrenderer','defaultsmoothing'}
        qp_prefs(UD,mfig,cmd,cmdargs);
        
    case {'deltaresweb','deltaresweboss'}
        ops={};
        if matlabversionnumber>5
            ops={'-browser'};
        end
        switch cmd
            case 'deltaresweboss'
                site='http://oss.deltares.nl';
            otherwise
                site='http://www.deltaressystems.com';
        end
        try
            stat=web(site,ops{:});
        catch
            stat=1;
        end
        if stat==1
            ui_message('error','Could not find browser to visit %s.',site)
        elseif stat==2
            ui_message('error','Could not start browser to visit %s.',site)
        end
        
    case 'pause'
        hpause = findobj(findobj(allchild(0),'tag','DelftProgressBar'),'tag','pause');
        set(hpause,'value',1)
        waitfor(hpause,'value',0)
        
    case 'move_onscreen'
        if ~isempty(cmdargs)
            movegui(cmdargs{1})
        else
            movegui(UD.MainWin.Fig)
            if ~isempty(UD.Options.Fig)
                movegui(UD.Options.Fig)
            end
            movegui(UD.PlotMngr.Fig)
            movegui(UD.PlotMngr.Fig)
            movegui(UD.FilOpt.Fig)
            movegui(UD.GridView.Fig)
            movegui(UD.ComLine.Fig)
            %
            figs = get(UD.PlotMngr.Fig,'userdata');
            for i = 1:length(figs)
                movegui(figs(i))
            end
            %
            umsg = findall(0,'tag','UI_MESSAGE window');
            if ~isempty(umsg)
                movegui(umsg)
            end
        end
        
    otherwise
        if (strncmpi(cmd,'all',3) || strncmpi(cmd,'edit',4)) && ~isempty(cmdargs)
            if strncmpi(cmd,'all',3)
                % all
                base = cmd(1:3);
                dim = cmd(4:end);
            else
                % edit
                base = cmd(1:4);
                dim = cmd(5:end);
            end
            %
            datafields=findobj(mfig,'tag','selectfield');
            fld=get(datafields,'value');
            Props=get(datafields,'userdata');
            Props=Props(fld);
            %
            if isfield(Props,'DimName')
                idim = find(strncmpi(Props.DimName,dim,length(dim)));
            else
                idim = [];
            end
            if length(idim)~=1
                error('Invalid dimension "%s"',dim)
            else
                if idim<=5
                    x = {'t' 's' 'm' 'n' 'k'};
                    cmd = [base x{idim}];
                else
                    cmd = [base num2str(idim)];
                end
            end
            d3d_qp(cmd,cmdargs{:})
        elseif exist(cmd)==2
            [p,f,e]=fileparts(cmd);
            switch lower(e)
                case {'.qplog','.m'}
                    d3d_qp('run',cmd,cmdargs{:})
                case {'.fig','.qpses'}
                    d3d_qp('openfigure',cmd,cmdargs{:})
                otherwise
                    d3d_qp('openfile',cmd,cmdargs{:})
            end
        else
            error('Unknown command in d3d_qp: %s',cmd)
        end
end

function check_nonprivate_files
thisfile=mfilename('fullpath');
thisdir=fileparts(thisfile);
d=dir([thisdir filesep '*.m']);
names={d.name};
for i=length(names):-1:1
    if ~strcmpi(names{i},'contents.m')
        file=which(names{i});
        filedir=fileparts(file);
        if isequal(filedir,thisdir)
            names(i)=[];
        else
            names{i}=sprintf('%s overruled by %s.',names{i},file);
        end
    else
        names(i)=[];
    end
end
if ~isempty(names)
    fprintf('%s\n','ERROR: Function name conflict detected.', ...
        'This may cause QUICKPLOT to not function properly.', ...
        'QUICKPLOT is unable to access some of its functions in:', ...
        thisdir, ...
        '', ...
        'The following functions are inaccessible:', ...
        names{:}, ...
        '', ...
        'Please remove the overruling functions.')
    beep
end

function clr = str2color(str)
switch str
    case 'r'
        clr=[1 0 0];
    case 'g'
        clr=[0 1 0];
    case 'b'
        clr=[0 0 1];
    case 'c'
        clr=[0 1 1];
    case 'm'
        clr=[1 0 1];
    case 'y'
        clr=[1 1 0];
    case 'k'
        clr=[0 0 0];
    case 'w'
        clr=[1 1 1];
    otherwise
        clr=str2vec(str,'%f');
end

function OK = qp_checkversion(qpversionbase,gitrepo,githash,qpcreationdate)
% Until MATLAB 7.10 (R2010a) it was possible to mix
% c/c++ files in with the MATLAB executable. This was
% used to include the @(#) identification string in the
% executable that could be located using the WHAT tool.
% Unfortunately, this option is no longer supported by
% later MATLAB versions. For later versions we'll need
% to use a separate text file which is easy to mess up
% and therefore we only start QuickPlot if that file is
% consistent with the actual executable.
OK = true;
whatfile = fullfile(qp_basedir('exe'),'d3d_qp.version');
if isequal(qpversionbase(1),'v')
    qpversion = qpversionbase(2:end);
else
    qpversion = qpversionbase;
end
Str = ['@(#)Deltares, Delft3D-QUICKPLOT, Version ' qpversion ', ' qpcreationdate ];
RepoLine = ['Repository : ', gitrepo];
HashLine = ['Source hash: ', githash];
fid = fopen(whatfile,'r','n','UTF-8');
if fid>0
    % file exists, read its contents
    Str2 = fgetl(fid);
    if ~ischar(Str2)
        Str2 = '';
    end
    fclose(fid);
    if ~isequal(Str,Str2)
        % if contents does not match, do as if file
        % does not exist (which will try to write it)
        fid = -1;
    end
end
if fid<0
    % file does not exist, try to write it
    fid = fopen(whatfile,'w','n','UTF-8');
    if fid>0
        % file can be opened for writing, write string
        try
            fprintf(fid,'%s\n',Str,RepoLine,HashLine);
            fclose(fid);
            % reopen the file to check whether string was written correctly
            fid = fopen(whatfile,'r','n','UTF-8');
            Str2 = fgetl(fid);
            if ~ischar(Str2)
                Str2 = '';
            end
            fclose(fid);
        catch
            fid = -1;
        end
    end
end
if fid>0
    if ~isequal(Str,Str2)
        ui_message('error',{['First line in ' whatfile],Str2,'doesn''t match the string',Str,'Please correct.'})
        OK = false;
    end
else
    ui_message('error',{'Copy the following line:',Str,['to ' whatfile ' to start QuickPlot.']})
    OK = false;
end
