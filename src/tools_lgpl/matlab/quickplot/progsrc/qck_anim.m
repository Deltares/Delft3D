function qck_anim(cmd,afig,ANISteps)
%QCK_ANIM Helper function for QuickPlot Animations.

%QCK_ANIM(Cmd,Figure,CmdArgs)

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

if nargin<2
    afig=gcbf;
    if isempty(afig)
        error('This function should not be called manually.')
    end
end

if strcmp(cmd,'animpush')
    pos=get(gcbo,'position');
    uicm=findobj(gcbf,'tag','animpushuicontextmenu');
    set(uicm,'position',pos(1:2)+pos(3:4)/2,'visible','on')
    return
end
%
T_=1; ST_=2; M_=3; N_=4; K_=5;
AnimSlid=findobj(afig,'tag','animslid');
AS=get(AnimSlid,'userdata');
if strcmp(cmd,'animselect')
    h = [];
    par_fig = gcbo;
    while ~isempty(par_fig) && ~isequal(get(par_fig,'type'),'figure')
        par_fig=get(par_fig,'parent');
    end
elseif isempty(AS)
    set(AnimSlid,'enable','off')
    return
else
    t_=AS(1).Fld(1);
    mask=logical(ones(1,length(AS)));
    for iobj=1:length(AS)
        h{iobj}=findall(0,'tag',AS(iobj).Tag);
        if isempty(h{iobj})
            mask(iobj)=0;
        end
    end
    h=h(mask);
    AS=AS(mask);
    set(AnimSlid,'userdata',AS)
    if isempty(AS)
        set(AnimSlid,'enable','off')
        return
    end
    h=cat(1,h{:});
    %
    % get figures in which the items to be animated are located
    %
    par_ax=get(h,'parent');
    if iscell(par_ax)
        par_ax=unique(cat(1,par_ax{:}));
    end
    par_fig=get(par_ax,'parent');
    if iscell(par_fig)
        par_fig=unique(cat(1,par_fig{:}));
    end
    %
    UDh=get(h,'userdata');
    if iscell(UDh)
        UDh=UDh(~cellfun('isempty',UDh));
    else
        UDh={UDh};
    end
    AnimObj=UDh{1};
    %
    % Backward compatible with cell version of PlotState ...
    %
    if iscell(AnimObj.PlotState)
        AnimObj.PlotState=plotstatestruct(AnimObj.PlotState);
    end
    %
    if t_==0
        t = AnimObj.PlotState.SubField{1};
    else
        t=AnimObj.PlotState.Selected{t_};
        if isnumeric(AS(1).Values)
            t=find(AS(1).Values==t);
        end
    end
end
%
switch cmd
    case {'start','startanim'}
        asld=findobj(afig,'tag','animslid');
        sld=findobj(par_fig,'tag','animslid');
        psh=findobj(par_fig,'tag','animpush');
        animstop=findall(par_fig,'tag','stopanim');
        set(animstop,'userdata',0)
        i0=getappdata(asld,'minival');
        i1=getappdata(asld,'maxival');
        OPS.Type = '';
        OPS.background = 0;
        OPS.steps = i0:i1;
        OPS.AnimLoop = 0;
        OPS.maxfps = 25;
        OPS.scriptname = '';
        ANISteps = OPS.steps;
        if nargin<3
            [Cancel,OPS] = runAnimationDialog(i0,i1);
            if Cancel
                return
            end
            ANISteps = OPS.steps;
        end
        streamObj = streamInitialize(OPS, par_fig);
        set(sld,'vis','off')
        set(psh,'vis','off')
        if OPS.background
            set(par_fig,'vis','off')
            pbfig=progressbar('cancel','closereq');
        else
            pbfig=-1;
        end
        vuim=findall(par_fig,'type','uimenu','visible','on');
        %      vtb=findall(par_fig,'type','uitoolbar','visible','on');
        set(vuim,'vis','off')
        %      set(vtb,'vis','off')
        set(findall(par_fig,'tag','startanim'),'enable','off');
        set(findall(par_fig,'tag','stopanim'),'enable','on');
        %
        mversion = matlabversionnumber;
        for fg = par_fig(:)'
            disable_listeners(fg,mversion)
            set(fg,'keypressfcn','qck_anim stopanimkey')
            enable_listeners(fg,mversion)
        end
        try
            NSteps=length(ANISteps);
            doloop=1;
            min_seconds_per_frame=1/OPS.maxfps;
            user_time_new=now*24*3600;
            scriptname = OPS.scriptname;
            while doloop
                if ~OPS.AnimLoop
                    doloop=0;
                end
                for i0 = 1:NSteps
                    user_time_prev = user_time_new;
                    i = ANISteps(i0);
                    for iobj=1:length(UDh)
                        AnimObj=UDh{iobj};
                        %
                        % Backward compatible with cell version of PlotState ...
                        %
                        if iscell(AnimObj.PlotState)
                            AnimObj.PlotState=plotstatestruct(AnimObj.PlotState);
                        end
                        if iscellstr(AS(1).Values)
                            tval = i;
                        else
                            tval = AS(1).Values(i);
                        end
                        if t_==0
                            AnimObj.PlotState.SubField{1} = tval;
                        else
                            AnimObj.PlotState.Selected{t_} = tval;
                        end
                        [hNew,Error,FileInfo]=qp_plot(AnimObj.PlotState);
                        UDh{iobj}=get(hNew(1),'userdata');
                    end
                    if ~isempty(scriptname)
                        local_eval(scriptname,i0);
                    end
                    drawnow
                    if OPS.background
                        H=progressbar(i0/NSteps,pbfig);
                        ish=ishandle(animstop);
                        if (H<0) & any(ish)
                            set(animstop(ish),'userdata',1)
                        end
                    else
                        user_time_new=now*24*3600;
                        user_time_diff = user_time_new-user_time_prev;
                        if user_time_diff<min_seconds_per_frame
                            pause(min_seconds_per_frame-user_time_diff);
                            user_time_new=now*24*3600;
                        end
                    end
                    streamObj = streamAdd(streamObj, par_fig);
                    ish=ishandle(animstop);
                    stop=get(animstop(ish),'userdata');
                    if iscell(stop)
                        stop=cat(1,stop{:});
                        stop=any(stop);
                    end
                    if any(~ish) | stop
                        doloop=0;
                        break
                    end
                end
            end
            streamFinalize(streamObj)
        catch Ex
            qp_error('Catch in qck_anim:',Ex,'d3d_qp_core')
            try
                streamFinalize(streamObj)
            catch
            end
        end
        try
            if length(vuim)>1 & any(ishandle(vuim))
                set(vuim(ishandle(vuim)),'vis','on')
                %         set(vtb,'vis','on')
                par_fig = par_fig(ishandle(par_fig));
                set(findall(par_fig,'tag','startanim'),'enable','on');
                set(findall(par_fig,'tag','stopanim'),'enable','off');
            end
            if ishandle(pbfig)
                delete(pbfig);
            end
            ish=ishandle(par_fig);
            if ish
                for fg = par_fig(ish)'
                    disable_listeners(fg,mversion)
                    set(fg,'keypressfcn','','vis','on')
                    enable_listeners(fg,mversion)
                end
            end
            for i=1:length(sld)
                if ishandle(sld(i))
                    %tt=get(sld(i),'value');
                    %
                    if iscellstr(AS(1).Values)
                        Str=AS(1).Values{t};
                    else
                        Str=sprintf('%i',AS(1).Values(t));
                    end
                    uislider(sld(i),'value',t);
                    set(sld(i),'tooltip',sprintf('%s(%i)=%s',AS(1).Label,t,Str));
                end
            end
            ish=ishandle(sld);
            set(sld(ish),'vis','on')
            ish=ishandle(psh);
            set(psh(ish),'vis','on')
        catch
        end
    case 'slider'
        sld=findobj(afig,'tag','animslid');
        if nargin>2
            t=ANISteps;
        else
            t=getappdata(sld,'currentvalue');
        end
        selected=AnimObj.PlotState.Selected;
        if t_==0
            current_t = AnimObj.PlotState.SubField{1};
        else
            current_t = selected{t_};
            if isnumeric(AS(1).Values)
                current_t = find(AS(1).Values==current_t);
            end
        end
        if t>current_t
            t=min(max(round(t),current_t+1),getappdata(sld,'maxival'));
        elseif t<current_t
            t=max(min(round(t),current_t-1),1);
        end
        %
        sld=findall(par_fig,'tag','animslid');
        uislider(sld,'value',t)
        %
        if iscellstr(AS(1).Values)
            Str=AS(1).Values{t};
        else
            Str=sprintf('%i',AS(1).Values(t));
        end
        set(sld,'tooltip',sprintf('%s(%i)=%s',AS(1).Label,t,Str));
    case 'animselect'
        AnimOpt=get(gcbo,'userdata');
        uicm=findobj(par_fig,'tag','animpushuicontextmenu');
        hChecked=findall(uicm,'checked','on');
        if strcmp(get(gcbo,'checked'),'on')
            if length(hChecked)>1
                animslid     = findobj(afig,'tag','animslid');
                animslid_all = findobj(par_fig,'tag','animslid');
                Ans=questdlg('Unlink object(s)?','','This object','Other objects','None','None');
                AnimTag=get(get(gcbo,'parent'),'userdata');
                for i=1:length(AS)
                    if isequal(AS(i).Tag,AnimTag)
                        break
                    end
                end
                switch Ans
                    case 'This object'
                        set(gcbo,'checked','off');
                        h(strmatch(AS(i).Tag,get(h,'tag')))=[];
                        AS(i)=[];
                        set(animslid_all,'userdata',AS);
                    case 'Other objects'
                        set(hChecked,'checked','off');
                        set(gcbo,'checked','on');
                        AS=AS(i);
                        h=h(strmatch(AS.Tag,get(h,'tag')));
                        set(animslid_all,'userdata',AS);
                    case 'None'
                end
                %
                % get figures in which the items to be animated are located
                %
                par_ax1=get(h,'parent');
                if iscell(par_ax1)
                    par_ax1=unique(cat(1,par_ax1{:}));
                end
                par_fig1=get(par_ax1,'parent');
                if iscell(par_fig1)
                    par_fig1=unique(cat(1,par_fig1{:}));
                end
                %
                notsel_fig = setdiff(par_fig,par_fig1);
                animslid_notsel = findobj(notsel_fig,'tag','animslid');
                set(animslid_notsel,'enable','off','userdata',[])
            end
            return
        end
        set(gcbo,'checked','on')
        AnimTag=get(get(gcbo,'parent'),'userdata');
        %
        animslid     = findobj(afig,'tag','animslid');
        animslid_all = findobj(par_fig,'tag','animslid');
        %
        AnimMax=length(AnimOpt.Values);
        sstep=[min(1/(AnimMax-1),.1) min(10/(AnimMax-1),.9)];
        AS=[];
        AS.Fld=AnimOpt.Dim;
        AS.Tag=AnimTag;
        AS.Label=AnimOpt.Label;
        AS.Values=AnimOpt.Values;
        h=findall(afig,'tag',AS.Tag);
        UDh=get(h,'userdata');
        if iscell(UDh)
            UDh=UDh(~cellfun('isempty',UDh));
            if length(UDh)>1
                error('Problem encountered while searching for object information.');
            end
        else
            UDh={UDh};
        end
        AnimObj=UDh{1};
        %
        % Backward compatible with cell version of PlotState ...
        %
        if iscell(AnimObj.PlotState)
            AnimObj.PlotState=plotstatestruct(AnimObj.PlotState);
        end
        t_=AnimOpt.Dim;
        if t_==0
            t=AnimObj.PlotState.SubField{1};
        else
            t=AnimObj.PlotState.Selected{t_};
            if isnumeric(AS.Values)
                t=find(AS.Values==t);
            end
        end
        %
        NStep=getappdata(animslid,'maxival');
        NoUpdateNec=1;
        ASold=get(animslid,'userdata');
        if isstruct(ASold) && ~isempty(ASold)
            ASoldFld=ASold(1).Fld;
        else
            ASoldFld=0;
        end
        if (NStep==AnimMax) & (AS.Fld==ASoldFld)
            Ans=questdlg('Link with previous object(s)?','','Yes','No','No');
            if strcmp(Ans,'Yes')
                t1=getappdata(animslid,'currentvalue');
                if t~=t1
                    NoUpdateNec=0;
                    t=t1;
                end
                AS=[ASold AS];
            else
                set(hChecked,'checked','off');
            end
        else
            set(hChecked,'checked','off');
        end
        %set(animslid,'userdata',AS,'value',1,'sliderstep',sstep,'Max',AnimMax,'enable','on','value',t)
        uislider(animslid,'value',t,'max',AnimMax)
        set(animslid,'userdata',AS,'enable','on')
        %
        if iscellstr(AS(1).Values)
            Str=AS(1).Values{t};
        else
            Str=sprintf('%i',AS(1).Values(t));
        end
        set(animslid,'tooltip',sprintf('%s(%i)=%s',AS(1).Label,t,Str));
        if NoUpdateNec
            return
        end
    case {'stopanimkey','stopanim'}
        if strcmp(cmd,'stopanimkey')
            if ~isequal(get(afig,'currentcharacter'),8) %Ctrl+H
                return
            end
        end
        animstop=findall(afig,'tag','stopanim');
        set(animstop,'userdata',1)
        return
end

existpar = ishandle(par_fig);
set(par_fig(existpar),'pointer','watch')
for iobj=1:length(UDh)
    AnimObj=UDh{iobj};
    %
    % Backward compatible with cell version of PlotState ...
    %
    if iscell(AnimObj.PlotState)
        AnimObj.PlotState=plotstatestruct(AnimObj.PlotState);
    end
    %
    if iscellstr(AS(1).Values)
        tval = t;
    else
        tval = AS(1).Values(t);
    end
    if t_==0
        AnimObj.PlotState.SubField{1} = tval;
    else
        AnimObj.PlotState.Selected{t_} = tval;
    end
    if ~isempty(findall(0,'tag',AS(iobj).Tag))
        try
            [hNew,Error,FileInfo]=qp_plot(AnimObj.PlotState);
        catch Ex
            qp_error('Catch in qck_anim:',Ex,'qck_anim')
        end
    end
end
set(par_fig(existpar),'pointer','arrow')


function OPS = streamOptions(outputtype, OPS, interactiveMode)
if nargin<3
    interactiveMode = true;
end
switch outputtype
    case 'avi file'
        if interactiveMode
            avi('options')
            % options stored globally
        end
        
    case 'video file'
        if ~isfield(OPS,'vidOps')
            OPS.vidOps = videoOptions('default');
        end
        if interactiveMode
            OPS.vidOps = videoOptions(OPS.vidOps);
        end

    otherwise
        % no options (or not implemented)
end


function vidOps = videoOptions(vidOps)
if isequal(vidOps,'default')
    videoProfs = VideoWriter.getProfiles;
    vidOps = [];
    iProf = max(1,ustrcmpi('MPEG-4', {videoProfs.Name}));
    vidOps.Profile = videoProfs(iProf);
    return
else
    uifig = createVideoOptionsDialog(vidOps);
    waitfor(uifig, 'userdata')
    if ishandle(uifig)
        vidOps = get(uifig,'userdata');
        delete(uifig)
    end
end


function videoDialogCallback(hObj, varargin)
cmd = get(hObj, 'tag');
Inactive=get(0,'defaultuicontrolbackgroundcolor');
Active=[1 1 1];

uifig = get(hObj, 'parent');
hProf = findobj(uifig, 'tag', 'video profile');
hLoss = findobj(uifig, 'tag', 'lossless');
hCRat = findobj(uifig, 'tag', 'cratio');
hCRa2 = findobj(uifig, 'tag', 'cratio text');
hQual = findobj(uifig, 'tag', 'quality');
hQua2 = findobj(uifig, 'tag', 'quality text');
hBitd = findobj(uifig, 'tag', 'bitdepth');
hBit2 = findobj(uifig, 'tag', 'bitdepth text');

iProf = get(hProf, 'value');
Profiles = get(hProf, 'userdata');
Profile = Profiles(iProf);

VW = VideoWriter('dummy',Profile.Name);
setProp = fieldnames(set(VW));

vidOps = get(uifig, 'userdata');
switch cmd
    case 'video profile'
        if ismember('LosslessCompression', setProp)
            Lossless = Profile.LosslessCompression;
            if isfield(vidOps,'LosslessCompression')
                Lossless = vidOps.LosslessCompression;
            end
            set(hLoss, 'value', Lossless, 'enable', 'on')
        else
            Lossless = false;
            set(hLoss, 'value', false, 'enable',  'off')
        end
        if ismember('CompressionRatio', setProp) && ~Lossless
            CRatio = Profile.CompressionRatio;
            if isfield(vidOps,'CompressionRatio')
                CRatio = vidOps.CompressionRatio;
            end
            set(hCRat, 'string', num2str(CRatio), 'enable', 'on', 'backgroundcolor', Active, 'userdata', CRatio)
            set(hCRa2, 'enable', 'on') 
        else
            set(hCRat, 'string', '', 'enable',  'off', 'backgroundcolor', Inactive)
            set(hCRa2, 'enable', 'off') 
        end
        if ismember('Quality', setProp) && ~Lossless
            Quality = Profile.Quality;
            if isfield(vidOps,'Quality')
                Quality = vidOps.Quality;
            end
            set(hQual, 'string', num2str(Quality), 'enable', 'on', 'backgroundcolor', Active, 'userdata', Quality)
            set(hQua2, 'enable', 'on') 
        else
            set(hQual, 'string', '', 'enable',  'off', 'backgroundcolor', Inactive)
            set(hQua2, 'enable', 'off') 
        end
        if ismember('MJ2BitDepth', setProp)
            MJ2BitDepth = Profile.MJ2BitDepth;
            if isfield(vidOps,'MJ2BitDepth')
                MJ2BitDepth = vidOps.MJ2BitDepth;
            end
            set(hBitd, 'string', num2str(MJ2BitDepth), 'enable', 'on', 'backgroundcolor', Active, 'userdata', MJ2BitDepth)
            set(hBit2, 'enable', 'on') 
        else
            set(hBitd, 'string', '', 'enable',  'off', 'backgroundcolor', Inactive)
            set(hBit2, 'enable', 'off') 
        end
        
    case 'lossless'
        Lossless = get(hLoss, 'value');
        if ismember('CompressionRatio', setProp) && ~Lossless
            CRatio = Profile.CompressionRatio;
            if isfield(vidOps,'CompressionRatio')
                CRatio = vidOps.CompressionRatio;
            end
            set(hCRat, 'string', num2str(CRatio), 'enable', 'on', 'backgroundcolor', Active, 'userdata', CRatio)
            set(hCRa2, 'enable', 'on') 
        else
            set(hCRat, 'string', '', 'enable',  'off', 'backgroundcolor', Inactive)
            set(hCRa2, 'enable', 'off') 
        end
        if ismember('Quality', setProp) && ~Lossless
            Quality = Profile.Quality;
            if isfield(vidOps,'Quality')
                Quality = vidOps.Quality;
            end
            set(hQual, 'string', num2str(Quality), 'enable', 'on', 'backgroundcolor', Active, 'userdata', Quality)
            set(hQua2, 'enable', 'on') 
        else
            set(hQual, 'string', '', 'enable',  'off', 'backgroundcolor', Inactive)
            set(hQua2, 'enable', 'off') 
        end
        
    case 'cratio'
        CRatio = str2double(get(hCRat, 'string'));
        if isnan(CRatio) || CRatio <= 1
            ui_message('error', 'CompressionRatio should be a value > 1.')
            CRatio = Profile.CompressionRatio;
        end
        set(hCRat, 'string', num2str(CRatio), 'userdata', CRatio)
        
    case 'quality'
        Quality = str2double(get(hQual, 'string'));
        if isnan(Quality) || Quality < 0 || Quality > 100
            ui_message('error', 'Quality should be a value >= 0 and <= 100.')
            Quality = Profile.Quality;
        end
        set(hCRat, 'string', num2str(Quality), 'userdata', Quality)
        
    case 'bitdepth'
        MJ2BitDepth = str2double(get(hBitd, 'string'));
        if isnan(MJ2BitDepth) || MJ2BitDepth < 1 || MJ2BitDepth > 16 || round(MJ2BitDepth)~=MJ2BitDepth
            ui_message('error', 'Bit Depth should be an integer in the range 1 to 16.')
            MJ2BitDepth = Profile.MJ2BitDepth;
        end
        set(hBitd, 'string', num2str(MJ2BitDepth), 'userdata', MJ2BitDepth)
        
    case 'cancel'
        delete(uifig)
        return
        
    case 'ok'
        vidOps.Profile = Profile;
        Lossless = false;
        if ismember('LosslessCompression', setProp)
            Lossless = logical(get(hLoss,'value'));
            vidOps.LosslessCompression = Lossless;
        elseif isfield(vidOps, 'LosslessCompression')
            vidOps = rmfield(vidOps, 'LosslessCompression');
        end
        if ismember('CompressionRatio', setProp) && ~Lossless
            vidOps.CompressionRatio = get(hCRat,'userdata');
        elseif isfield(vidOps, 'CompressionRatio')
            vidOps = rmfield(vidOps, 'CompressionRatio');
        end
        if ismember('Quality', setProp) && ~Lossless
            vidOps.Quality = get(hQual,'userdata');
        elseif isfield(vidOps, 'Quality')
            vidOps = rmfield(vidOps, 'Quality');
        end
        if ismember('MJ2BitDepth', setProp) && ~isempty(get(hBitd,'userdata'))
            vidOps.MJ2BitDepth = get(hBitd,'userdata');
        elseif isfield(vidOps, 'MJ2BitDepth')
            vidOps = rmfield(vidOps, 'MJ2BitDepth');
        end
        %
        set(uifig, 'userdata', vidOps)
        
    otherwise
        ui_message('error', 'Command %s not yet implemented in videoDialogCallback.', cmd)
end


function a = createVideoOptionsDialog(vidOps)
Inactive=get(0,'defaultuicontrolbackgroundcolor');
Active=[1 1 1];

Width = 400;
LineHeight = 25;
ss=qp_getscreen;
figsize=[Width 45+6*LineHeight];
VOffset=figsize(2)-5;

a=qp_uifigure('Video Settings','','QuickPlot video options dialog',[ss(1:2)+(ss(3:4)-figsize)/2 figsize]);
%
% Output format ...
%
VOffset=VOffset-LineHeight;
b = uicontrol('Parent',a, ...
    'BackgroundColor',Inactive, ...
    'Position',[10 VOffset 50 18], ...
    'HorizontalAlignment','left', ...
    'String','Profile', ...
    'Style','text', ...
    'Tag','');

Profiles = VideoWriter.getProfiles;
ProfNames = {Profiles.Name};
iProf = max(1, ustrcmpi(vidOps.Profile.Name, ProfNames));
hProf = uicontrol('Parent',a, ...
    'BackgroundColor',Active, ...
    'Position',[60 VOffset Width-70 20], ...
    'HorizontalAlignment','right', ...
    'callback',@videoDialogCallback, ...
    'String',ProfNames, ...
    'Value',iProf, ...
    'Style','popupmenu', ...
    'Tag','video profile', ...
    'Tooltip','Select the video file type.', ...
    'UserData',Profiles);
%
% Lossless compression ...
%
VOffset=VOffset-LineHeight;
b = uicontrol('Parent',a, ...
    'Position',[60 VOffset Width-170 18], ...
    'HorizontalAlignment','left', ...
    'String','Lossless Compression', ...
    'Style','checkbox', ...
    'callback',@videoDialogCallback, ...
    'Value',0, ...
    'Tooltip','Select whether lossless compression should be used.', ...
    'Tag','lossless');
%
% Compression Ratio ...
%
VOffset=VOffset-LineHeight;
b = uicontrol('Parent',a, ...
    'Position',[60 VOffset Width-170 18], ...
    'HorizontalAlignment','left', ...
    'String','Compression Ratio', ...
    'Style','text', ...
    'Tag','cratio text');

b = uicontrol('Parent',a, ...
    'BackgroundColor',Active, ...
    'Position',[Width-110 VOffset 100 20], ...
    'HorizontalAlignment','right', ...
    'callback',@videoDialogCallback, ...
    'String','', ...
    'Style','edit', ...
    'Tooltip','The compression ratio is the ratio between the number of bytes in the input image and the number of bytes in the compressed image. Compression Ratio >= 1.', ...
    'Tag','cratio');
%
% Quality ...
%
VOffset=VOffset-LineHeight;
b = uicontrol('Parent',a, ...
    'Position',[60 VOffset Width-170 18], ...
    'HorizontalAlignment','left', ...
    'String','Quality', ...
    'Style','text', ...
    'Tag','quality text');

b = uicontrol('Parent',a, ...
    'BackgroundColor',Active, ...
    'Position',[Width-110 VOffset 100 20], ...
    'HorizontalAlignment','right', ...
    'callback',@videoDialogCallback, ...
    'String','', ...
    'Style','edit', ...
    'Tooltip', 'Video quality, specified as an integer in the range, [0,100]. Higher quality numbers result in higher video quality and larger file sizes. Lower quality numbers result in lower video quality and smaller file sizes.', ...
    'Tag','quality');
%
% MJ2BitDepth ...
%
VOffset=VOffset-LineHeight;
b = uicontrol('Parent',a, ...
    'Position',[60 VOffset Width-170 18], ...
    'HorizontalAlignment','left', ...
    'String','Bit Depth', ...
    'Style','text', ...
    'Tag','bitdepth text');

b = uicontrol('Parent',a, ...
    'BackgroundColor',Active, ...
    'Position',[Width-110 VOffset 100 20], ...
    'HorizontalAlignment','right', ...
    'callback',@videoDialogCallback, ...
    'String','', ...
    'Style','edit', ...
    'Tooltip', 'Bit depth for Motion JPEG 2000 files, specified as an integer in the range [1,16]. The bit depth is the number of least-significant bits in the input image data. Use empty for automatic.', ...
    'Tag','bitdepth');

%    'Colormap'

%
% Cancel or OK  ...
%
W = (Width-30)/2;
b = uicontrol('Parent',a, ...
    'callback',@videoDialogCallback, ...
    'Position',[20+W 10 W 20], ...
    'String','OK', ...
    'Tag','ok');
b = uicontrol('Parent',a, ...
    'callback',@videoDialogCallback, ...
    'Position',[10 10 W 20], ...
    'String','Cancel', ...
    'Tag','cancel');
%
% Show figure ...
%
set(a,'userdata',vidOps)
videoDialogCallback(hProf)
set(a,'visible','on')

function streamObj = streamInitialize(OPS, figures)
persistent savedir
if ~ischar(savedir)
    savedir = '';
elseif ~isempty(savedir)
    if savedir(end) ~= filesep
        savedir(end+1) = filesep;
    end
end

streamObj.Type = '';
streamObj.maxfps = OPS.maxfps;
switch OPS.Type
    case ''
        return

    case 'print/export'
        streamObj.printObj = md_print([]);
        PrtID = streamObj.printObj.PrtID;
        if isequal(PrtID, 0)
            return
        end
        if strcmp(PrtID(max(1,end-4):end), ' file')
            ext = streamObj.printObj.ext;
            [fn, pn] = uiputfile([savedir, '*.', ext], 'Specify location and base ...');
            if ~ischar(fn)
                return
            end
            [p,f,e] = fileparts(fn);
            if isempty(e)
                e = ['.' ext];
            end
            nr = 0;
            ndig = 0;
            if ~ismember(PrtID,{'Multi page PDF file','QUICKPLOT session file'})
                while length(f)>ndig && ismember(f(end-ndig), '0123456789')
                    ndig = ndig+1;
                end
                if ndig == 0
                    ndig = 3;
                else
                    nr = sscanf(f(end-ndig+1:end),'%d');
                    f = f(1:end-ndig);
                end
            end
            streamObj.BaseStr = fullfile(pn,f);
            streamObj.NextNr = nr;
            if ndig>0
                ndigstr = num2str(ndig);
                streamObj.FrmtNr = strcat('%',ndigstr,'.',ndigstr,'i');
            else
                streamObj.FrmtNr = '';
            end
            streamObj.ExtStr = e;
        end

    case 'avi file'
        [fn,pn]=uiputfile([savedir '*.avi'], 'Specify output file ...');
        if ~ischar(fn)
            return
        end
        [p,f,e] = fileparts(fn);
        if isempty(e)
            fn = [fn '.avi'];
        end
        aviObj = avi('initialize');
        aviObj = avi('open', aviObj, [pn, fn]);
        streamObj.aviObj = aviObj;
        streamObj.First = true;

    case 'video file'
        e0 = OPS.vidOps.Profile.FileExtensions{1};
        [fn, pn] = uiputfile([savedir, '*', e0], 'Specify output file ...');
        if ~ischar(fn)
            return
        end
        [p,f,e] = fileparts(fn);
        if isempty(e)
            fn = [fn, e0];
        end
        vidObj = VideoWriter([pn, fn], OPS.vidOps.Profile.Name);
        vidObj.FrameRate = OPS.maxfps;
        open(vidObj);
        streamObj.vidObj = vidObj;

    otherwise
        ext = strtok(OPS.Type);
        [fn, pn] = uiputfile([savedir, '*.', ext], 'Specify location and base ...');
        if ~ischar(fn)
            return
        end
        [p,f,e] = fileparts(fn);
        n = '';
        while length(f)>0 && ismember(f(end),'0123456789')
            n = [f(end),n];
            f = f(1:end-1);
        end
        if isempty(n)
            n = 0;
            ndig = 3;
        else
            ndig = length(n);
            n = str2num(n);
        end
        if ~isempty(strmatch(lower(e),{'.tif','.tiff','.jpg','.jpeg','.png','.bmp'},'exact'))
            e = [e(2:end) '_'];
        elseif ~isempty(strmatch(lower(ext),{'tif','jpg','png','bmp'},'exact'))
            e = [ext '_'];
        else
            e = ext;
        end
        opsarg = {};
        for afgi = 1:length(figures)
            if length(figures)>1
                %
                % add some figure identification. Now: A,B,C, ...
                % thus limited to 26 figures parallel but should be sufficient.
                %
                opsarg = {'subcase' char(64+afgi)};
            end
            streamObj.imgSeries{afgi} = series_init(fullfile(pn,f),n,'digits',ndig,opsarg{:},e);
        end
end
streamObj.Type = OPS.Type;
savedir = pn;


function streamObj = streamAdd(streamObj, figures)
switch streamObj.Type
    case ''
        % nothing
        
    case 'print/export'
        if isfield(streamObj,'BaseStr') % to file ...
            SubCase = '';
            for ifig = 1:length(figures)
                if length(figures) > 1
                    SubCase = char(64+ifig);
                end
                filename = [streamObj.BaseStr sprintf(streamObj.FrmtNr, streamObj.NextNr) SubCase streamObj.ExtStr];
                streamObj.printObj = md_print(figures(ifig), streamObj.printObj, filename, 'keepopen');
            end
            streamObj.NextNr = streamObj.NextNr + 1;
        else % to printer ...
            md_print(figures, streamObj.printObj);
        end
        
    case 'avi file'
        if streamObj.First
            Fig = getframe(figures(1));
            [streamObj.aviObj, OK] = avi('addvideo', streamObj.aviObj, streamObj.maxfps, Fig.cdata);
            if ~OK
                error('Cannot add video stream to output file.')
            end
            streamObj.First = false;
        end
        Fig = getframe(figures(1));
        streamObj.aviObj = avi('addframe', streamObj.aviObj, Fig.cdata);
        
    case 'video file'
        Fig = getframe(figures(1));
        if strcmp(streamObj.vidObj.VideoFormat,'Indexed')
            if isfield(streamObj,'Colormap')
                X = rgb2ind(Fig.cdata,streamObj.Colormap);
                Fig.cdata = X;
                Fig.colormap = streamObj.Colormap;
            else
                [X,map] = rgb2ind(Fig.cdata,256);
                Fig.cdata = X;
                Fig.colormap = map;
                streamObj.Colormap = map;
            end
            writeVideo(streamObj.vidObj, Fig);
        else
            writeVideo(streamObj.vidObj, Fig.cdata);
        end
        
    otherwise
        for ifig = 1:length(figures)
            streamObj.imgSeries{ifig} = series_frame(figures(ifig), streamObj.imgSeries{ifig});
        end
end


function streamFinalize(streamObj)
switch streamObj.Type
    case 'print/export'
        if isfield(streamObj,'BaseStr') % close print file if open ...
            streamObj.printObj = md_print([], streamObj.printObj);
        end
        
    case 'avi file'
        avi('finalize',streamObj.aviObj);
        
    case 'video file'
        close(streamObj.vidObj);
        
    otherwise
        % nothing
end


function local_eval(scriptname, i)
eval(scriptname,'')


function [Cancel,OPS] = runAnimationDialog(MinT,MaxT)
output_ops = {...
    ... % name, background render, options
    'no output'   , 0, 0
    'tif files'   , 1, 0
    'jpg files'   , 1, 0
    'png files'   , 1, 0
    'bmp files'   , 1, 0
    'print/export', 1, 0};
if 1
    % VideoWriter
    output_ops(end+1,:) = {'video file', 0, 1};
end
if strncmp(computer,'PC',2) && matlabversionnumber>=6
    % writeavi option available using Video for Windows
    output_ops(end+1,:) = {'avi file', 0, 1};
    %TODO: Try to render AVI in background by means of hardcopy
end
%
outputtypes = output_ops(:,1)';
output_ops = cell2struct(output_ops, {'Name', 'SupportsBackground', 'HasOptions'},2);

uifig = createAnimationDialog;

Houtp = findobj(uifig, 'tag', 'animation output');
set(Houtp, ...
    'string' ,outputtypes, ...
    'userdata', output_ops);

OPS.maxfps = 25;
Hfps=findobj(uifig,'tag','max_fps');
set(Hfps,'string',num2str(OPS.maxfps),'userdata',OPS.maxfps);

Htim = findobj(uifig, 'tag', 'time steps');
set(Htim, 'string', sprintf('%i:%i', MinT, MaxT), 'userdata', MinT:MaxT);
setappdata(Htim, 'range', [MinT, MaxT]);

waitfor(uifig, 'userdata');
if ishandle(uifig)
    OPS = get(uifig, 'userdata');
    Cancel = 0;
    %
    delete(uifig);
else
    Cancel = 1;
end


function animationDialogCallback(hObj, varargin)
cmd = get(hObj, 'tag');

uifig = get(hObj, 'parent');
Houtp     = findobj(uifig, 'tag', 'animation output');
Hrendback = findobj(uifig, 'tag', 'renderback');
Hanimloop = findobj(uifig, 'tag', 'animloop');
Hoptions  = findobj(uifig, 'tag', 'options');
Htim      = findobj(uifig, 'tag', 'time steps');
Hfps      = findobj(uifig, 'tag', 'max_fps');
Hscr      = findobj(uifig, 'tag', 'script');
Hok       = findobj(uifig, 'tag', 'ok');

output = get(Houtp, 'value');
outputtypes = get(Houtp, 'string');
outputtype = outputtypes{output};

switch cmd
    case 'animation output'
        %
        output_ops = get(Houtp, 'userdata');
        if output_ops(output).SupportsBackground
            set(Hrendback, 'enable', 'on')
        else
            set(Hrendback, 'enable', 'off')
        end
        if output_ops(output).HasOptions
            set(Hoptions,'enable','on')
            OPS = get(Hoptions, 'userdata');
            OPS = streamOptions(outputtype, OPS, false);
            set(Hoptions, 'userdata', OPS)
        else
            set(Hoptions,'enable','off')
        end
        if output==1
            set(Hanimloop,'enable','on')
        else
            set(Hanimloop,'enable','off')
        end
        
    case 'options'
        OPS = get(Hoptions, 'userdata');
        OPS = streamOptions(outputtype, OPS);
        set(Hoptions, 'userdata', OPS)

    case 'time steps'
         Range = getappdata(Htim, 'range');
         try
             ANISteps = str2vec(get(Htim, 'string'), 'range', Range, 'applylimit');
             set(Htim, ...
                 'foregroundcolor','k', ...
                 'userdata', ANISteps)
             % 'string', vec2str(ANISteps, 'noones', ...
             set(Hok,'enable','on') % conditional on other checks?
         catch
             set(Htim, ...
                 'foregroundcolor','r')
             set(Hok,'enable','off')
         end
        
    case 'max_fps'
        newfps = str2num(get(Hfps,'string'));
        if ~isempty(newfps)
            maxfps = newfps(1);
        end
        set(Hfps, 'string', num2str(maxfps), 'userdata', maxfps);
        
    case 'script'
        %no check yet ... 
        %
        %scriptname = get(Hscr, 'string');
        %if exist(scriptname) ~= 2
        %   ui_message('error','A script named ''%s'' does not exist.',scriptname)
        %end
        
    case 'cancel'
        delete(uifig)
        return
        
    case 'ok'
        OPS = get(Hoptions, 'userdata');
        
        if strcmp(get(Hrendback,'enable'),'on')
            OPS.background = get(Hrendback,'value');
        else
            OPS.background = 0;
        end
        if output == 1
            OPS.Type = '';
            OPS.AnimLoop = get(Hanimloop,'value');
        else
            OPS.Type = outputtype;
            OPS.AnimLoop = 0;
        end
        OPS.steps  = get(Htim, 'userdata');
        OPS.maxfps = get(Hfps, 'userdata');
        OPS.scriptname = get(Hscr, 'string');
        set(uifig, 'userdata', OPS)
        
    otherwise
        ui_message('error', 'Command %s not yet implemented in animationDialogCallback.', cmd)
end


function a = createAnimationDialog
Inactive=get(0,'defaultuicontrolbackgroundcolor');
Active=[1 1 1];

LineHeight = 25;
Width = 400;
ss=qp_getscreen;
figsize=[Width 45+6*LineHeight];
VOffset=figsize(2)-5;

a=qp_uifigure('Animation Settings','','QuickPlot animate items',[ss(1:2)+(ss(3:4)-figsize)/2 figsize]);
%
% Output format ...
%
VOffset=VOffset-LineHeight;
b = uicontrol('Parent',a, ...
    'BackgroundColor',Inactive, ...
    'Position',[10 VOffset 50 18], ...
    'HorizontalAlignment','left', ...
    'String','Output', ...
    'Style','text', ...
    'Tag','');

b = uicontrol('Parent',a, ...
    'BackgroundColor',Active, ...
    'Position',[60 VOffset Width-70 20], ...
    'HorizontalAlignment','right', ...
    'callback',@animationDialogCallback, ...
    'String','Animation Output', ...
    'Style','popupmenu', ...
    'Tag','animation output');
%
% Render in background ...
%
VOffset=VOffset-LineHeight;
b = uicontrol('Parent',a, ...
    'Position',[60 VOffset Width-170 18], ...
    'HorizontalAlignment','left', ...
    'String','Render in Background', ...
    'Style','checkbox', ...
    'Enable','off', ...
    'Value',0, ...
    'Tag','renderback');
%
% Render options ...
%
b = uicontrol('Parent',a, ...
    'Position',[Width-110 VOffset 100 20], ...
    'HorizontalAlignment','left', ...
    'callback',@animationDialogCallback, ...
    'String','Options', ...
    'Enable','off', ...
    'Tag','options');
%
% Simulation steps ...
%
VOffset=VOffset-LineHeight;
b = uicontrol('Parent',a, ...
    'BackgroundColor',Inactive, ...
    'Position',[10 VOffset 50 18], ...
    'HorizontalAlignment','left', ...
    'String','Steps', ...
    'Style','text', ...
    'Tag','');

b = uicontrol('Parent',a, ...
    'BackgroundColor',Active, ...
    'Position',[60 VOffset Width-70 20], ...
    'HorizontalAlignment','left', ...
    'callback',@animationDialogCallback, ...
    'String','1', ...
    'Style','edit', ...
    'Tag','time steps');
%
% Loop until stopped  ...
%
VOffset=VOffset-LineHeight;
b = uicontrol('Parent',a, ...
    'Position',[60 VOffset Width-70 18], ...
    'HorizontalAlignment','left', ...
    'String','Loop until Stopped', ...
    'Style','checkbox', ...
    'Value',0, ...
    'Tag','animloop');
%
% Maximum frame rate ...
%
VOffset=VOffset-LineHeight;
b = uicontrol('Parent',a, ...
    'Position',[60 VOffset Width-170 18], ...
    'HorizontalAlignment','left', ...
    'String','Maximum Frame Rate (fps)', ...
    'Style','text');

b = uicontrol('Parent',a, ...
    'BackgroundColor',Active, ...
    'Position',[Width-110 VOffset 100 20], ...
    'HorizontalAlignment','right', ...
    'callback',@animationDialogCallback, ...
    'String','', ...
    'Style','edit', ...
    'Tag','max_fps');
%
% Script name ...
%
VOffset=VOffset-LineHeight;
h1 = uicontrol('Parent',a, ...
    'BackgroundColor',Inactive, ...
    'Position',[10 VOffset 50 18], ...
    'HorizontalAlignment','left', ...
    'String','Script', ...
    'Style','text', ...
    'Tag','script_txt');

h2 = uicontrol('Parent',a, ...
    'BackgroundColor',Active, ...
    'Position',[60 VOffset Width-70 20], ...
    'HorizontalAlignment','left', ...
    'callback',@animationDialogCallback, ...
    'String','', ...
    'Style','edit', ...
    'Tag','script');

if isstandalone
    set(h1,'enable','off')
    set(h2, 'enable', 'off', 'backgroundcolor', Inactive)
end
%
% Cancel or OK  ...
%
W = (Width-30)/2;
b = uicontrol('Parent',a, ...
    'callback',@animationDialogCallback, ...
    'Position',[20+W 10 W 20], ...
    'String','OK', ...
    'Tag','ok');
b = uicontrol('Parent',a, ...
    'callback',@animationDialogCallback, ...
    'Position',[10 10 W 20], ...
    'String','Cancel', ...
    'Tag','cancel');
%
% Show figure ...
%
set(a,'visible','on')


function disable_listeners(fg,mversion)
%Disable listeners
if mversion>=8.04
    mmgr = uigetmodemanager(fg);
    [mmgr.WindowListenerHandles(:).Enabled] = deal(0);
elseif mversion>=7.02
    mmgr = uigetmodemanager(fg);
    set(mmgr.WindowListenerHandles,'Enable','off');
end

function enable_listeners(fg,mversion)
%Enable listeners
if mversion>=8.04
    mmgr = uigetmodemanager(fg);
    [mmgr.WindowListenerHandles(:).Enabled] = deal(1);
elseif mversion>=7.02
    mmgr = uigetmodemanager(fg);
    set(mmgr.WindowListenerHandles,'Enable','on');
end
