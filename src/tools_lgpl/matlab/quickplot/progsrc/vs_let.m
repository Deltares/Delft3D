function [X,Success]=vs_let(varargin)
%VS_LET Read one or more elements from a NEFIS file.
%   X=VS_LET(NFStruct,'GroupName',GroupIndex,'ElementName',ElementIndex)
%   reads the data from the specified element (only specified element
%   indices) in the specified group (only speicified group indices) from
%   the NEFIS file specified by NFStruct into an array X. Any argument or
%   combination of arguments may be missing. During reading a wait bar will
%   show the progress.
%
%   If the file (NFStruct) is not specified, the NEFIS that was last opened
%   by VS_USE will be used to read the data. A file structure NFStruct can
%   be obtained as output argument from the function VS_USE.
%
%   If the group index (GroupIndex) is missing the element will be returned
%   for all group indices. If present the group index should be a 1xNGD
%   cell array where NGD equals the number of group dimensions of the
%   selected group. Each cell should contain the indices of the group
%   dimension concerned to be read.
%
%   If the element index (ElementIndex) is missing the whole element will
%   be returned. If present the element index should be a 1xNED cell
%   array where NED equals the number of element dimensions of the selected
%   element. Each cell should contain the indices of the element dimension
%   concerned to be read.
%
%   If a group or element name is missing or invalid, or if an index is
%   invalid a graphical user interface will appear that allows you to
%   correct your selection. If '*' is specified for the ElementName, all
%   elements in the specified group will be read and the data is returned
%   in a structure with fieldnames that mirror the element names; the
%   ElementIndex is ignored in that case.
%
%   X=VS_LET(...,'quiet') reads the specified values into X without showing
%   the wait bar.
%
%   X=VS_LET(...,'debug') write debug information to a file while reading
%   the data.
%
%   [X,Success]=VS_LET(...) returns as second argument whether the data was
%   read succesfully.
%
%   Example
%      F = vs_use('trim-xxx.dat','trim-xxx.def');
%      X = vs_let(F,'map-series',{1:5},'S1',{1:30 1:20});
%      % returns a 5x30x20 matrix containing the data of the first 5 time
%      % steps (dimension of map-series group) and first 30x20 values of
%      % the water level stored in element S1 of group map-series.
%      % This is more efficiently than reading all data and then indexing:
%      % AllX = vs_let(F,'map-series','S1');
%      % X = AllX(1:5,1:30,1:20);
%
%   See also VS_USE, VS_DISP, VS_GET, VS_DIFF, VS_FIND, VS_TYPE.

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

X=[];
outputtype=1; % 1=vs_let default, 2=vs_get
EmptyStr='                ';
AllStr='** ALL FIELDS **';
if nargout<2
    Success=1;
else
    Success=0;
end

INP = varargin;
INPlen = nargin;
if INPlen>0
    if isequal(INP{1},'vsget')
        outputtype=2; % vs_get
        INP(1)=[];
        INPlen = INPlen-1;
    end
end
gNameDefault = '>>no group specified<<';
eNameDefault = '>>no element specified<<';

showwaitbar=isenvironment('MATLAB');
vs_debug=0;
cmdhelp=0;
vs_warning=1;
VS=vs_use('lastread');
if INPlen==0
    if ~isstruct(VS)
        error('No NEFIS file specified.');
    end
    gIndex={};
    eIndex={};
    gName=gNameDefault;
    eName=eNameDefault;
else
    deleteINP = repmat(false,size(INP));
    for i=1:length(INP)
        if isstruct(INP{i})
            VS=INP{i};
        elseif ischar(INP{i})
            switch lower(INP{i})
                case 'quiet!' % alternative for 2nd output argument
                    showwaitbar=0;
                    Success=0;
                case 'quiet'
                    showwaitbar=0;
                case 'debug'
                    vs_debug=1;
                case '-cmdhelp'
                    cmdhelp=1;
                case 'nowarn'
                    vs_warning=0;
                otherwise
                    % don't set the deleteINP flag
                    continue
            end
        else
            % don't set the deleteINP flag
            continue
        end
        deleteINP(i) = true;
    end
    INP(deleteINP) = [];
    if ~isstruct(VS)
        error('No NEFIS file specified.')
    end
    switch length(INP)
        case 0
            gIndex={};
            eIndex={};
            gName=gNameDefault;
            eName=eNameDefault;
        case 1 % GroupName
            if ~ischar(INP{1})
                error('Invalid group name.')
            end
            gIndex={};
            eIndex={};
            gName=INP{1};
            eName=eNameDefault;
        case 2
            if ~ischar(INP{1})
                error('Invalid group name.')
            end
            if ischar(INP{2}) % GroupName ElementName
                gIndex={};
                eIndex={};
                gName=INP{1};
                eName=INP{2};
            else % GroupName GroupIndex
                gIndex=INP{2};
                eIndex={};
                gName=INP{1};
                eName=eNameDefault;
            end
        case 3
            if ~ischar(INP{1})
                error('Invalid group name.')
            end
            if ischar(INP{2}) % GroupName ElementName ElementIndex
                gIndex={};
                eIndex=INP{3};
                gName=INP{1};
                eName=INP{2};
            else % GroupName GroupIndex ElementName
                if ~ischar(INP{3})
                    error('Missing element name.')
                end
                gIndex=INP{2};
                eIndex={};
                gName=INP{1};
                eName=INP{3};
            end
        case 4
            if ~ischar(INP{1})
                error('Invalid group name.')
            end
            if ~ischar(INP{3})
                error('Invalid element name.')
            end
            gIndex=INP{2};
            eIndex=INP{4};
            gName=INP{1};
            eName=INP{3};
    end
end
if strcmp(eName,'*')
    eName=AllStr;
end

gName=deblank(gName);
eName=deblank(eName);

MsgStr='';
if isempty(VS.GrpDat)
    MsgStr='No data in file.';
    if Success
        uiwait(msgbox(MsgStr));
    else
        X=MsgStr;
    end
    return
elseif ~isequal(gName,gNameDefault)
    i=strmatch(gName,{VS.GrpDat(:).Name},'exact');
    if ~isempty(i)
        if isempty(gIndex)
            gIndex=num2cell(zeros(1,length(VS.GrpDat(i).SizeDim)));
        end
    else
        eName=eNameDefault;
    end
else
    eName=eNameDefault;
end

if ~isequal(eName,eNameDefault)
    id=VS.GrpDat(i).DefIndex;
    if id>0
        ic=VS.GrpDef(id).CelIndex;
        if ic>0
            ElmsInCell=VS.CelDef(ic).Elm;
            if any(ElmsInCell==0)
                MsgStr=sprintf('One or more elements in group ''%s'' undefined.',gName);
            else
                i=ElmsInCell(strmatch(eName,{VS.ElmDef(ElmsInCell).Name},'exact'));
                if ~isempty(i)
                    if length(i)>1
                        ui_message('warning','Element %s occurs multiple times in group %s; reading first one.',eName,gName)
                        i = i(1);
                    end
                    if isempty(eIndex)
                        eIndex=num2cell(zeros(1,length(VS.ElmDef(i).Size)));
                    end
                elseif ~strcmp(eName,AllStr)
                    MsgStr=sprintf('Group ''%s'' has no element called ''%s''.',gName,eName);
                end
            end
        else
            eName=eNameDefault;
            MsgStr=sprintf('Cel definition missing for group ''%s''.',gName);
        end
    else
        eName=eNameDefault;
        MsgStr=sprintf('Group definition missing for group ''%s''.',gName);
    end
end

if ~isempty(MsgStr)
    if Success
        uiwait(msgbox(MsgStr));
    else
        X=MsgStr;
        return
    end
end

if isempty(VS.FileName) & isempty(VS.DatExt)
    if Success
        fprintf(1,'Invalid VS structure specified.\n');
    end
    return
end

intfig=NaN;
AllCorrect=0;
UseGui=0; % isempty(eName);
i=0; i_new=0;
j=0; j_new=0;

while ~AllCorrect
    if UseGui
        if isunix
            Gray=[79 148 205]/255;
            White=Gray;
            ForeGround=[1 1 1];
            ButWidth=170;
            ButHeight=30;
            FWeight='bold';
        else
            White=[1 1 1];
            Gray=[1 1 1]*192/255;
            ForeGround=[0 0 0];
            ButWidth=150;
            ButHeight=20;
            FWeight='normal';
        end
        % interface version
        if ~ishandle(intfig)
            % create Gui
            intfig=figure(...
                'units','normalized', ...
                'position',[.5 .5 .1 .1], ...
                'menu','none', ...
                'units','pixels', ...
                'integerhandle','off', ...
                'numbertitle','off', ...
                'color',Gray, ...
                'name','Select group and element ...');
            %            , ...
            %            'closerequestfcn',' ');
            pos=get(intfig,'position');
            pos(3)=30+2*ButWidth;
            pos(4)=70+8*ButHeight;
            pos(1)=pos(1)-pos(3)/2;
            pos(2)=pos(2)-pos(4)/2;
            set(intfig,'position',pos);
            l(1,8)=uicontrol('style','text', ...
                'string','GROUP name and range:', ...
                'parent',intfig, ...
                'fontweight',FWeight, ...
                'units','pixels', ...
                'backgroundcolor',Gray, ...
                'foregroundcolor',ForeGround, ...
                'horizontalalignment','left', ...
                'position',[10 60+7*ButHeight ButWidth ButHeight]);
            l(1,6)=uicontrol('style','popupmenu', ...
                'string',{VS.GrpDat(:).Name}, ...
                'parent',intfig, ...
                'fontweight',FWeight, ...
                'value',i_new, ...
                'units','pixels', ...
                'backgroundcolor',White, ...
                'foregroundcolor',ForeGround, ...
                'horizontalalignment','left', ...
                'position',[10 60+6*ButHeight ButWidth ButHeight], ...
                'callback','uiresume(gcf)');
            for k=1:5
                l(1,k)=uicontrol('style','edit', ...
                    'parent',intfig, ...
                    'fontweight',FWeight, ...
                    'enable','off', ...
                    'string',' ', ...
                    'units','pixels', ...
                    'backgroundcolor',Gray, ...
                    'foregroundcolor',ForeGround, ...
                    'horizontalalignment','left', ...
                    'position',[10 20+(6-k)*ButHeight ButWidth ButHeight]);
            end
            if ~isempty(gIndex)
                for k=1:length(gIndex)
                    set(l(1,k), ...
                        'string',Local_vec2str(gIndex{k}), ...
                        'backgroundcolor',White, ...
                        'enable','on');
                end
                i=i_new;
            end
            l(1,7)=uicontrol('style','pushbutton', ...
                'string','Cancel', ...
                'parent',intfig, ...
                'fontweight',FWeight, ...
                'units','pixels', ...
                'backgroundcolor',Gray, ...
                'foregroundcolor',ForeGround, ...
                'horizontalalignment','center', ...
                'position',[10 10 ButWidth ButHeight], ...
                'tag','Cancel', ...
                'callback','set(findobj(gcf,''tag'',''Load''),''userdata'',2); uiresume(gcf)');
            id=VS.GrpDat(i_new).DefIndex;
            ElmsInCell=[];
            if id>0
                ic=VS.GrpDef(id).CelIndex;
                if ic>0
                    ElmsInCell=VS.CelDef(ic).Elm;
                end
            end
            if isempty(ElmsInCell)
                ENames={};
            else
                ENames={VS.ElmDef(ElmsInCell).Name}; % 5.3.0.10183 returns {[]} for empty ElmsInCell
            end
            if isempty(ENames)
                ENames={EmptyStr};
                EEnable='off';
                EBckGrndClr=Gray;
                selected=1;
            else
                EEnable='on';
                EBckGrndClr=White;
                ENames{end+1}=AllStr;
                selected = find(ElmsInCell==j_new);
                if isempty(selected)
                    selected=length(ElmsInCell)+1;
                end
            end
            l(2,8)=uicontrol('style','text', ...
                'string','ELEMENT name and range:', ...
                'parent',intfig, ...
                'fontweight',FWeight, ...
                'units','pixels', ...
                'backgroundcolor',Gray, ...
                'foregroundcolor',ForeGround, ...
                'horizontalalignment','left', ...
                'position',[20+ButWidth 60+7*ButHeight ButWidth ButHeight]);
            L=uicontrol('style','text', ...
                'string','-', ...
                'parent',intfig, ...
                'fontweight',FWeight, ...
                'units','pixels', ...
                'backgroundcolor',Gray, ...
                'foregroundcolor',ForeGround, ...
                'horizontalalignment','left', ...
                'position',[20+ButWidth 30+6*ButHeight ButWidth 28]);
            l(2,6)=uicontrol('style','popupmenu', ...
                'string',ENames, ...
                'enable',EEnable, ...
                'parent',intfig, ...
                'fontweight',FWeight, ...
                'value',selected, ...
                'units','pixels', ...
                'backgroundcolor',EBckGrndClr, ...
                'foregroundcolor',ForeGround, ...
                'horizontalalignment','left', ...
                'position',[20+ButWidth 60+6*ButHeight ButWidth ButHeight], ...
                'callback','uiresume(gcf)');
            for k=1:5
                l(2,k)=uicontrol('style','edit', ...
                    'parent',intfig, ...
                    'fontweight',FWeight, ...
                    'enable','off', ...
                    'string',' ', ...
                    'units','pixels', ...
                    'backgroundcolor',Gray, ...
                    'foregroundcolor',ForeGround, ...
                    'horizontalalignment','left', ...
                    'position',[20+ButWidth 20+(6-k)*ButHeight ButWidth ButHeight]);
            end
            if ~isempty(eIndex)
                for k=1:length(eIndex)
                    set(l(2,k), ...
                        'string',Local_vec2str(eIndex{k}), ...
                        'backgroundcolor',White, ...
                        'enable','on');
                end
                j=j_new;
            end
            l(2,7)=uicontrol('style','pushbutton', ...
                'string','Load', ...
                'parent',intfig, ...
                'fontweight',FWeight, ...
                'units','pixels', ...
                'backgroundcolor',Gray, ...
                'foregroundcolor',ForeGround, ...
                'horizontalalignment','center', ...
                'position',[20+ButWidth 10 ButWidth ButHeight], ...
                'userdata',0, ...
                'tag','Load', ...
                'callback','set(gco,''userdata'',1); uiresume(gcf)');
        else
            % bring Gui to front.
            figure(intfig);
        end
        set(l(2,7),'userdata',0);
        while get(l(2,7),'userdata')==0
            i_new=get(l(1,6),'value');
            if i_new~=i
                i=i_new;
                gIndex={};
                gName=VS.GrpDat(i).Name;
                for k=1:5
                    if k<=length(VS.GrpDat(i).SizeDim)
                        set(l(1,k),'enable','on', ...
                            'backgroundcolor',White, ...
                            'string',['[1:',num2str(VS.GrpDat(i).SizeDim(k)),']']);
                    else
                        set(l(1,k),'enable','off', ...
                            'backgroundcolor',Gray, ...
                            'string',' ');
                    end
                end
                id=VS.GrpDat(i).DefIndex;
                ElmsInCell=[];
                if id>0
                    ic=VS.GrpDef(id).CelIndex;
                    if ic>0
                        ElmsInCell=VS.CelDef(ic).Elm;
                    end
                end
                if isempty(ElmsInCell)
                    ENames={};
                else
                    ENames={VS.ElmDef(ElmsInCell).Name}; % 5.3.0.10183 returns {[]} for empty ElmsInCell
                end
                if isempty(ENames)
                    ENames={EmptyStr};
                    EEnable='off';
                    EBckGrndClr=Gray;
                else
                    ENames{end+1}=AllStr;
                    EEnable='on';
                    EBckGrndClr=White;
                end
                set(l(2,6),'string',ENames,'value',1,'enable',EEnable,'backgroundcolor',EBckGrndClr);
                j=0;
            end
            Temp=get(l(2,6),'value');
            eName=ENames{Temp};
            if isempty(ElmsInCell)
                j_new=[];
                eDescr='';
            elseif Temp>length(ElmsInCell) % All Fields
                j_new=[];
                eDescr='';
            else
                j_new=ElmsInCell(Temp);
                eDescr=VS.ElmDef(j_new).Description;
            end
            if isempty(j_new) | isempty(j) | (j_new~=j)
                j=j_new;
                eIndex={};
                set(L,'string',eDescr);
                for k=1:5
                    if ~isempty(j) & k<=length(VS.ElmDef(j).Size)
                        set(l(2,k),'enable','on', ...
                            'backgroundcolor',White, ...
                            'string',['[1:',num2str(VS.ElmDef(j).Size(k)),']']);
                    else
                        set(l(2,k),'enable','off', ...
                            'backgroundcolor', Gray, ...
                            'string',' ');
                    end
                end
            end
            uiwait(intfig);
            if ~ishandle(l(2,7)) | get(l(2,7),'userdata')==2 % Cancel
                if ishandle(intfig)
                    delete(intfig);
                end
                return
            end
        end
        for k=1:length(VS.GrpDat(i).SizeDim)
            gIndex{k}=str2num(get(l(1,k),'string'));
            if isempty(gIndex{k})
                gIndex{k}=1;
            end
        end
        if ~isempty(j)
            for k=1:length(VS.ElmDef(j).Size)
                eIndex{k}=str2num(get(l(2,k),'string'));
                if isempty(eIndex{k})
                    eIndex{k}=1;
                end
            end
        end
    end

    % Check whether all parameters are correct.
    AllCorrect=1;
    if isempty(VS.GrpDat)
        gNames={};
    else
        gNames={VS.GrpDat(:).Name};
    end
    i_new=strmatch(gName,gNames,'exact');
    if isempty(i_new)
        AllCorrect=0;
        MsgStr=sprintf('Group ''%s'' does not exist.',gName);
        if Success
            if ~isequal(gName,gNameDefault)
                uiwait(msgbox(MsgStr));
            end
        else
            X=MsgStr;
            return
        end
        gIndex={};
        eIndex={};
        i_new=1;
        j_new=1;
    else % Group exists
        id_new=VS.GrpDat(i_new).DefIndex;
        ElmsInCell=[];
        if id_new>0
            ic_new=VS.GrpDef(id_new).CelIndex;
            if ic_new>0
                ElmsInCell=VS.CelDef(ic_new).Elm;
            end
        end
        if isempty(ElmsInCell)
            j_new=[];
        else
            j_new=ElmsInCell(strmatch(eName,{VS.ElmDef(ElmsInCell).Name},'exact'));
            if length(j_new)>1
                j_new = j_new(1); % Element .. occurs multiple times in group .. message already given above, so just go ahead
            end
        end
        if (~iscell(gIndex)) | ...
                (~all(sort(size(gIndex))==[1 length(VS.GrpDat(i_new).SizeDim)]))
            AllCorrect=0;
            %      if length(VS.GrpDat(i_new).SizeDim)>0,
            MsgStr=sprintf( ...
                'Group index of ''%s'' should be a %i-dimensional cell vector.', ...
                deblank(gName),length(VS.GrpDat(i_new).SizeDim));
            %      else,
            %        MsgStr=sprintf( ...
            %         'Group ''%s'' does not contain data.', deblank(gName));
            %      end;
            if Success
                uiwait(msgbox(MsgStr));
            else
                X=MsgStr;
                return
            end
            gIndex={};
            eIndex={};
            if isempty(ElmsInCell)
                j_new=[];
            else
                j_new=ElmsInCell(1);
            end
        else
            for k=1:length(VS.GrpDat(i_new).SizeDim)
                if ~((min(gIndex{k})>=1) & ...
                        (max(gIndex{k})<=VS.GrpDat(i_new).SizeDim(k)) & ...
                        all(Local_isinteger(gIndex{k})))
                    if all(size(gIndex{k})==[1 1]) & (gIndex{k}==0) % if zero select all
                        gIndex{k}=1:VS.GrpDat(i_new).SizeDim(k);
                    else
                        MsgStr=sprintf( ...
                            'Group index %i of ''%s'' should contain integers in the range 1:%i.', ...
                            k,deblank(gName),VS.GrpDat(i_new).SizeDim(k));
                        if AllCorrect
                            if Success
                                uiwait(msgbox(MsgStr));
                            else
                                X=MsgStr;
                                return
                            end
                        end
                        AllCorrect=0;
                    end
                end
            end
            if strcmp(eName,AllStr)
                AllCorrect=1;
                eIndex={};
                j_new=ElmsInCell;
            elseif isempty(j_new)
                MsgStr=sprintf('Group ''%s'' has no element called ''%s''.', ...
                    deblank(gName),deblank(eName));
                if AllCorrect
                    if Success
                        if ~isempty(deblank(eName))
                            uiwait(msgbox(MsgStr));
                        end
                    else
                        X=MsgStr;
                        return
                    end
                end
                AllCorrect=0;
                eIndex={};
                if isempty(ElmsInCell)
                    j_new=[];
                else
                    j_new=ElmsInCell(1);
                end
            elseif (~iscell(eIndex)) | ...
                    (~all(sort(size(eIndex))==[1 length(VS.ElmDef(j_new).Size)]))
                MsgStr=sprintf( ...
                    'Element index of ''%s'' should be a %i dimensional cell vector.\n', ...
                    eName,length(VS.ElmDef(j_new).Size));
                eIndex={};
                if AllCorrect
                    if Success
                        uiwait(msgbox(MsgStr));
                    else
                        X=MsgStr;
                        return
                    end
                end
                AllCorrect=0;
            else
                for k=1:length(VS.ElmDef(j_new).Size)
                    if ~((min(eIndex{k})>=1) & ...
                            (max(eIndex{k})<=VS.ElmDef(j_new).Size(k)) & ...
                            all(Local_isinteger(eIndex{k})))
                        if all(size(eIndex{k})==[1 1]) & (eIndex{k}==0) % if zero select all
                            eIndex{k}=1:VS.ElmDef(j_new).Size(k);
                        else
                            MsgStr=sprintf( ...
                                'Element index %i of ''%s'' should contain integers in the range 1:%i.\n', ...
                                k,eName,VS.ElmDef(j_new).Size(k));
                            if AllCorrect
                                if Success
                                    uiwait(msgbox(MsgStr));
                                else
                                    X=MsgStr;
                                    return
                                end
                            end
                            AllCorrect=0;
                        end
                    end
                end
            end
        end
    end
    if ~iscell(gIndex)
        gIndex={};
    end
    if ~iscell(eIndex)
        eIndex={};
    end
    UseGui=1;
    if ~AllCorrect & ~Success
        return
    end
end

i=i_new;
id=VS.GrpDat(i).DefIndex;     % index of group definition
ic=VS.GrpDef(id).CelIndex;    % index of cell definition
ElmsInCell=VS.CelDef(ic).Elm; % elements in cell
j=j_new;
% All correct according to information in VS memory.
% Remove Gui if created.
if ishandle(intfig)
    delete(intfig);
end

if cmdhelp
    CmdStr='Data=vs_let(NFStruct,';
    if outputtype==2 % vs_get
        CmdStr=strrep(CmdStr,'vs_let','vs_get');
    end
    CmdStr=strcat(CmdStr,'''',deblank(gName),''',');
    SubStr='{';
    AllInd=1;
    for k=1:length(gIndex)
        if isequal(gIndex{k},1:VS.GrpDat(i_new).SizeDim(k))
            SubStr(end+1)='0';
        else
            SubStr=strcat(SubStr,Local_vec2str(gIndex{k}));
            AllInd=0;
        end
        if k<length(gIndex)
            SubStr(end+1)=',';
        else
            SubStr(end+1)='}';
        end
    end
    if ~AllInd
        CmdStr=strcat(CmdStr,SubStr,',');
    end
    if isequal(eName,AllStr)
        CmdStr=strcat(CmdStr,'''*''');
    else
        CmdStr=strcat(CmdStr,'''',deblank(eName),'''');
        SubStr='{';
        AllInd=1;
        for k=1:length(eIndex)
            if isequal(eIndex{k},1:VS.ElmDef(j_new).Size(k))
                SubStr(end+1)='0';
            else
                SubStr=strcat(SubStr,Local_vec2str(eIndex{k}));
                AllInd=0;
            end
            if k<length(eIndex)
                SubStr(end+1)=',';
            else
                SubStr(end+1)='}';
            end
        end
        if ~AllInd
            CmdStr=strcat(CmdStr,',',SubStr);
        end
    end
    CmdStr=strcat(CmdStr,');');
    fprintf(CmdStr);
    return
end

if strcmp(eName,AllStr) % all elements
    jn=length(j);
    jnm=jn; % jnm=max(1,jn);
    eName=cell(jnm,1);
    eIndex=cell(jnm,1);
    for j1=1:jnm
        eName{j1}=strrep(VS.ElmDef(j(j1)).Name,'-','_');
        eIndex{j1}=cell(1,length(VS.ElmDef(j(j1)).Size));
        for k=1:length(VS.ElmDef(j(j1)).Size)
            eIndex{j1}{k}=1:VS.ElmDef(j(j1)).Size(k);
        end
    end
else % one element
    jn=0;
    jnm=1; % jnm=max(1,jn);
    eName={eName};
    eIndex={eIndex};
end

% count the number of group indices to be returned
gNIndex = zeros(1,length(gIndex));
for k=1:length(gIndex)
    gNIndex(k)=length(gIndex{k});
end
ReadNGrp=prod(gNIndex);

% count the number of element entries to be returned
eNIndex = cell(1,jnm);
eDimen = cell(1,jnm);
eSize = zeros(1,jnm);
eIndex1 = cell(1,jnm);
for j1=1:jnm
    eNIndex{j1} = zeros(1,length(eIndex{j1}));
    for k=1:length(eIndex{j1})
        eNIndex{j1}(k)=length(eIndex{j1}{k});
    end
    eDimen{j1}=VS.ElmDef(j(j1)).Size;
    eSize(j1)=prod(eDimen{j1});
    % determine scalar index for element selection
    eIndex1{j1} = subcript2ind(eDimen{j1},eIndex{j1});
end

if isfield(VS,'AddressType')
    AddressType = VS.AddressType;
else
    AddressType = 'uint32';
end

P256=[1 256 256^2 256^3];
switch AddressType
    case 'uint32'
        Nil=2^32-1;
        AddressBytes = 4;
    case 'uint64'
        Nil=2^64-1;
        AddressBytes = 8;
end
lasterr('');
try
    if vs_debug
        vs_debug=fopen([tempdir 'vs_let.dbg'],'w');
        if vs_debug<=0
            vs_debug=0;
            if vs_warning
                warning('Cannot open debug file: %svs_let.dbg.',tempdir)
            end
        else
            fprintf(1,'Writing to debug file %i: %svs_let.dbg ...\n',vs_debug,tempdir);
        end
    end

    fidat=0; % make sure that the file handle is defined in case of a crash

    if showwaitbar
        if jn>0  % all elements
            hWaitBar = waitbar(0,['Reading ',deblank(gName),'. Please wait ...']);
        else % one element
            hWaitBar = waitbar(0,['Reading ',deblank(eName{1}),'. Please wait ...']);
        end
        set(hWaitBar,'closerequestfcn','');
    end

    % open data file
    data_file=[VS.FileName,VS.DatExt];
    fidat=fopen(data_file,'r',VS.Format);

    if fidat<0
        error('Cannot not open file: %s.',data_file)
    end

    gDimen=VS.GrpDat(i).SizeDim;

    if vs_debug
        fprintf(vs_debug,'Data file: %s\n',data_file);
        fprintf(vs_debug,'Opened using filehandle: %i.\n\n',fidat);
        fprintf(vs_debug,'Reading ...\nGroup    %i              : %s.\n',i,deblank(gName));
        for k=1:length(gIndex)
            fprintf(vs_debug,'  Index  %i (max:%6i) : %s\n',k,gDimen(k),Local_vec2str(gIndex{k}));
        end
        for j1=1:jnm,
            fprintf(vs_debug,'Element %2i              : %s.\n',j1,deblank(eName{j1}));
            for k=1:length(eIndex{j1})
                fprintf(vs_debug,'  Index  %i              : %s\n',k,Local_vec2str(eIndex{j1}{k}));
            end
        end
    end

    % determine whether there is a variable dimension
    VD=VS.GrpDat(i).VarDim;
    % determine the number of variable dimension
    if VD
        VDIndex=gIndex{VD};
        gDimen(VD)=1;
        VDOffset=prod(gNIndex(1:(VD-1)));
        gIndex{VD}=1;
        VarDimCntMax=length(VDIndex);
        if vs_debug
            fprintf(vs_debug,'This group has a variable dimension (%i) of size %i.\n',VD,VarDimCntMax);
        end
    else
        VDOffset=0;
        VarDimCntMax=1;
        if vs_debug,
            fprintf(vs_debug,'This group has no variable dimension.\n');
        end
    end

    % get the size and ordering of the dimensions in the data file
    gOrder=VS.GrpDat(i).OrderDim;
    if vs_debug
        fprintf(vs_debug,'\nOrder in which the group dimensions are stored:');
        fprintf(vs_debug,' %i',gOrder);
        fprintf(vs_debug,'\n');
        fprintf(vs_debug,'Reordering requested group indices ...\n');
    end

    % reorder the dimensions for data acces
    if VD % make sure that VD comes last
        if gOrder(VD)~=1
            gOrder(VD)=max(gOrder)+1;
        end
    end
    [gOrder,Reorder]=sort(gOrder);          % Reorder
    gDimen=gDimen(Reorder);
    gIndex=gIndex(Reorder);
    gNIndex=gNIndex(Reorder);
    [Dummy,UnReorder]=sort(Reorder);

    if vs_debug
        fprintf(vs_debug,'Reordered group indices:\n');
        for k=1:length(gIndex)
            if k==VD
                fprintf(vs_debug,'  Index  %i (max:   ???) : %s  <-- variable dimension\n',k,Local_vec2str(VDIndex));
            else
                fprintf(vs_debug,'  Index  %i (max:%6i) : %s\n',k,gDimen(k),Local_vec2str(gIndex{k}));
            end
        end
        fprintf(vs_debug,'\n');
    end

    % precompute element offset within cell
    if jn==0 % one element
        ElmOffset=0;
        for k=ElmsInCell
            if k==j % Sum element sizes until loadable element is encountered
                break
            end
            ElmOffset=ElmOffset+VS.ElmDef(k).SizeElm;
        end
        if vs_debug
            fprintf(vs_debug,'Element offset within data set : %i\n\n',ElmOffset);
        end
        ElmOffsetCopy=ElmOffset;              % Save copy for reading of multiple groups in group optimized mode
    else % all elements
        ElmOffset=zeros(1,jnm);
        for j1=1:jnm-1
            ElmOffset(j1+1)=ElmOffset(j1)+VS.ElmDef(ElmsInCell(j1)).SizeElm;
        end
        if vs_debug,
            fprintf(vs_debug,'Element offsets within data set:\n');
            fprintf(vs_debug,'   %7i\n',ElmOffset);
            fprintf(vs_debug,'\n');
        end
        ElmOffsetCopy=ElmOffset;              % Save copy for reading of multiple groups in group optimized mode
        ElmOffset=0;
    end

    % preload NBytesPerCell, DataType, ElmBytes, ValBytes
    NBytesPerCell=sum([VS.ElmDef(ElmsInCell).SizeElm]);
    DataType=[VS.ElmDef(j).Type];
    ValBytes=[VS.ElmDef(j).SizeVal];
    ElmBytes=[VS.ElmDef(j).SizeElm];
    RdType=cell(1,jnm);
    for j1=1:jnm
        switch DataType(j1)
            case 1     % CHARACTE
                RdType{j1}='uchar';
            case 2     % COMPLEX
                switch ValBytes(j1),
                    case 8   % number of bytes equals 8 (4+4)
                        RdType{j1}='float32';
                    case 16  % number of bytes equals 16 (8+8)
                        RdType{j1}='float64';
                    otherwise
                        error('Unable to read COMPLEX data.');
                end
            case 3 % INTEGER
                switch ValBytes(j1),
                    case 4 % number of bytes equals 4
                        RdType{j1}='int32';
                    case 2 % number of bytes equals 2
                        RdType{j1}='int16';
                    otherwise
                        error('Unable to read INTEGER data.');
                end
            case 4 % LOGICAL
                switch ValBytes(j1),
                    case 4 % number of bytes equals 4
                        RdType{j1}='int32';
                    case 2 % number of bytes equals 2
                        RdType{j1}='int16';
                    otherwise
                        error('Unable to read LOGICAL data.');
                end
            case 5 % REAL
                switch ValBytes(j1)
                    case 4 % number of bytes equals 4
                        RdType{j1}='float32';
                    case 8 % number of bytes equals 8
                        RdType{j1}='float64';
                    otherwise
                        error('Unable to read REAL data.');
                end
            otherwise
                Str=sprintf(1,'Unexpected type number %i for %s.\n',DataType(j1),VS.ElmDef(j(j1)).Name);
                error(Str)
        end
        %Initialize dimension for reading ...
        switch DataType(j1)
            case 1     % CHARACTE
                rDimen{j1}=[ValBytes(j1) eSize(j1)];
            case 2
                rDimen{j1}=[2 eSize(j1)];
            case {3,4,5}
                rDimen{j1}=[1 eSize(j1)];
        end
    end

    if vs_debug
        for j1=1:jnm
            fprintf(vs_debug,'Element %i (%s) contains ',j1,deblank(eName{j1}));
            if rDimen{j1}(2)==1
                fprintf(vs_debug,'1 value of type: %s',RdType{j1});
            else
                fprintf(vs_debug,'%i values of type: %s',rDimen{j1}(2),RdType{j1});
            end
            if ValBytes(j1)==1
                fprintf(vs_debug,' (1 byte)\n');
            else
                fprintf(vs_debug,' (%i bytes)\n',ValBytes(j1));
            end
        end
        fprintf(vs_debug,'\nAllocating space for output data ... ');
    end

    % Preallocate some memory ...
    X{2,jnm}=[];
    X(1,:)=eName(:);
    switch outputtype
        case 1 % vs_let
            for j1=1:jnm
                if DataType(j1)==1
                    X{2,j1}=char(zeros(cat(2,ReadNGrp,eNIndex{j1},ValBytes(j1))));
                else
                    X{2,j1}=zeros(cat(2,ReadNGrp,eNIndex{j1}));
                end
            end
        case 2 % vs_get
            % Note this only a small part of the space that is actually required!
            for j1=1:jnm
                X{2,j1}=cell(ReadNGrp,1);
            end
    end

    if vs_debug
        temp_dbg=whos('X');
        fprintf(vs_debug,'%i bytes used for X.\nNote: This includes some string and cell array overhead.\n\n',temp_dbg.bytes);
        fprintf(vs_debug,'X is a %ix%i cell array.\n',temp_dbg.size);
        for j1=1:jnm
            fprintf(vs_debug,'  X{ 1,%2i} = %s\n',j1,X{1,j1});
            fprintf(vs_debug,'  X{ 2,%2i} = ',j1);
            tsz=size(X{2,j1});
            fprintf(vs_debug,'%ix',tsz(1:end-1));
            fprintf(vs_debug,'%i %s array\n',tsz(end),class(X{2,j1}));
        end
        fprintf(vs_debug,'\n');
    end

    %Determine wether all group dimensions related to cell
    %are completely read.
    ReadAllGrpCellDims=1;
    for k=1:length(gIndex)
        SZgIndex(1,k)=length(gIndex{k});
        if SZgIndex(k)~=gDimen(k)
            ReadAllGrpCellDims=0;
        end % SZgIndex(k) can only be gDimen(k) if gIndex{k}=1:gDimen(k) since all doubles were removed
    end

    NRCells=prod(SZgIndex);
    GroupOptimized = ReadAllGrpCellDims & (NRCells>1);
    % if not all cells are read, then no group optimization possible yet
    % if just one cell then no group optimization necessary (and if jn>0 undesirable: extra fseek commands)
    if GroupOptimized
        CelOffset=0;            % dummy offset
        CelSkip=0;              % dummy skip
        %NRCells=prod(SZgIndex);
        NRCellsPerRead=NRCells; % all cells (per VD) will be read at once
        if vs_debug
            fprintf(vs_debug,'Group optimized reading used.\n');
            if NRCellsPerRead>1
                fprintf(vs_debug,'Reading %i cells per fread statement.\n',NRCellsPerRead);
            else
                fprintf(vs_debug,'Reading 1 cell per fread statement.\n');
            end
        end

        % Change RdType and rDimen to include cell dimensions
        for j1=1:jnm
            RdType{j1}=sprintf('%i*%s',prod(rDimen{j1}),RdType{j1});
            rDimen{j1}(2)=rDimen{j1}(2)*NRCells;
        end
        if vs_debug
            for j1=1:jnm
                fprintf(vs_debug,'Reading format adjusted to: %ix%i values using format %s for %s.\n',rDimen{j1},RdType{j1},X{1,j1});
            end
            fprintf(vs_debug,'\n');
        end
    else
        CPfw=cumprod([1 SZgIndex]);
        CPbw=fliplr(cumprod([1 fliplr(SZgIndex)]));
        CelOffset=zeros(prod(SZgIndex),length(gIndex));
        for k=1:length(gIndex)
            CelOffset(:,k)=repmat(reshape(repmat(gIndex{k},CPfw(k),1),SZgIndex(k)*CPfw(k),1),CPbw(k+1),1);
        end
        SkipCellMask=[1 cumprod(gDimen(1:(end-1)))]*NBytesPerCell;
        CelOffset=(CelOffset-1)*(transpose(SkipCellMask));
        NRCells=length(CelOffset); % count the number of cells that will be read (per VD)
        NRCellsPerRead=1;          % one cell per fread statement

        if vs_debug
            fprintf(vs_debug,'Element optimized reading used.\n');
            if VD
                fprintf(vs_debug,'Per variable dimension index ');
            end
            if NRCells==1,
                fprintf(vs_debug,'1 cell will be read.\nThe offset of this cell within the group is ');
            else
                fprintf(vs_debug,'%i cells will be read.\nThe offsets of these cells within the group are:\n',NRCells);
            end
            fprintf(vs_debug,[repmat(' %7i',[1 10]) '\n'],CelOffset);
            fprintf(vs_debug,'\n\n');
        end

        % determine the number of bytes to skip between fread statements
        if jn==0 % one element
            CelSkip=[0; diff(CelOffset)-ElmBytes(1)]; %prod(eDimen{1})*ValBytes(1)
        else % all elements
            % approximation only, since exact number of bytes varies per element.
            CelSkip=[0; diff(CelOffset)-NBytesPerCell];
        end
    end

    if showwaitbar
        Alpha2=NRCells*VarDimCntMax;
        Alpha3=0.02*Alpha2;
        if Alpha2>0
            Alpha2=1/Alpha2;
        end
        Alpha4=Alpha3;
    end

    %
    % Determine the appropriate offset where the cell data is stored.
    % In case of a type '   4' group, the data record follows GROUP record
    % in data file. In case of a type '   5' group (variable dimension) the
    % pointer index table follows the data record. The bare length of the
    % data group is 392+3*AddressBytes.
    %
    Offset=VS.GrpDat(i).Offset+392+3*AddressBytes;
    % for each value of the variable dimension
    % if there is no variable dimension, VarDimCntMax should have been set to 1
    LastVD=-ones(1,4);
    if VD
        fseek(fidat,Offset+AddressBytes,-1);     % Start of first pointer table
        PointerList(4,:)=fread(fidat,[1 256],AddressType);
        if vs_debug
            fprintf(vs_debug,'Pointer list 4 at offset %u:\n',Offset+AddressBytes);
            printPointers(vs_debug,PointerList(4,:),Nil)
            fprintf(vs_debug,'\n');
        end
    end
    for VarDimCnt=1:VarDimCntMax
        % If there is a variable dimension adapt the offset
        if VD
            if vs_debug
                fprintf(vs_debug,'Determine offset of data group\nfor variable dimension index %i (%i of %i) ...\n',VDIndex(VarDimCnt),VarDimCnt,VarDimCntMax);
            end
            Temp=VDIndex(VarDimCnt);
            VDByte=floor(Temp./P256);
            VDByte=VDByte-256*floor(VDByte/256);
            if vs_debug
                fprintf(vs_debug,'Byte representation of %i : %i %i %i %i\n',VDIndex(VarDimCnt),VDByte);
            end
            for bt=[4 3 2]
                if LastVD(bt)~=VDByte(bt)
                    bt1=bt-1;
                    fseek(fidat,PointerList(bt,VDByte(bt)+1),-1);
                    PointerList(bt1,:)=fread(fidat,[1 256],AddressType);
                    LastVD(bt)=VDByte(bt); % copy VDByte into LastVD
                    LastVD(bt1)=Nil;       % make sure that PointerList(bt-2) is reloaded
                    if vs_debug
                        fprintf(vs_debug,'Pointer list %i at offset %u:\n',bt1,PointerList(bt,VDByte(bt)+1));
                        printPointers(vs_debug,PointerList(bt1,:),Nil)
                        fprintf(vs_debug,'\n');
                    end
                end
            end
            Offset=PointerList(1,VDByte(1)+1);
            if vs_debug
                if Offset==Nil
                    fprintf(vs_debug,'The offset of datagroup %i is -1.\n',VDIndex(VarDimCnt));
                else
                    fprintf(vs_debug,'The offset of datagroup %i is %u.\n',VDIndex(VarDimCnt),Offset);
                end
            end
        end
        if GroupOptimized
            VDCellOffset=(VarDimCnt-1)*NRCellsPerRead;
        else
            VDCellOffset=(VarDimCnt-1)*VDOffset;
        end

        Alpha1=(VarDimCnt-1)*NRCells;

        if VD & (Offset==Nil)
            % reading from a pointer that has not been set is not possible
            % zeros and blanks returned by default
            if vs_debug
                fprintf(vs_debug,'  No data to read.\n');
            end
        else
            if GroupOptimized
                status=0;
            else
                if jn>0
                    % one large jump to start of first element
                    status=fseek(fidat,Offset+CelOffset(1),-1);
                    if vs_debug
                        fprintf(vs_debug,'  Jumping to start of first element at %i.\n',Offset+CelOffset(1));
                    end
                else
                    % one large jump directly to start of element
                    status=fseek(fidat,Offset+CelOffset(1)+ElmOffset,-1);
                    if vs_debug
                        fprintf(vs_debug,'  Jumping to start of element at %i.\n',Offset+CelOffset(1)+ElmOffset);
                    end
                end
            end
            if status<0
                if vs_warning
                    warning(Message(0,vs_debug, ...
                        VarDimCnt,Offset,NBytesPerCell,fidat,gName));
                end
            else
                for Cell=1:length(CelOffset)
                    if GroupOptimized
                        IdxCell=1:NRCellsPerRead;
                    else
                        IdxCell=Cell;
                    end
                    if showwaitbar % can be placed outside Cell loop to speed up reading of small groups/cells
                        NewRefresh=Alpha1+Cell; % <-- in that case replace Cell by NrCells
                        if NewRefresh>Alpha4
                            waitbar(NewRefresh*Alpha2);
                            Alpha4=NewRefresh+Alpha3;
                        end
                    end

                    % Go to the appropriate offset for reading
                    if CelSkip(Cell) %>0
                        fread(fidat,CelSkip(Cell),'int8');
                    end

                    for j1=1:jnm
                        % Read total dataset into Temp
                        p=eSize(j1);
                        if GroupOptimized
                            status=fseek(fidat,Offset+ElmOffsetCopy(j1),-1);
                            if vs_debug
                                fprintf(vs_debug,'  Jumping to start of element %i in cell 1 at %i.\n',j1,ftell(fidat));
                                if status>=0
                                    fprintf(vs_debug,'  Reading %s data ...\n',RdType{j1});
                                end
                            end
                            if status<0
                                warning(Message(0,vs_debug, ...
                                    VarDimCnt,Offset,NBytesPerCell,fidat,gName));
                                Temp=[];
                                NRead=0;
                            else
                                [Temp,NRead]=fread(fidat,rDimen{j1},RdType{j1},NBytesPerCell-ElmBytes(j1));
                                if vs_debug
                                    fprintf(vs_debug,'  Reading ended at %i.\n',ftell(fidat));
                                end
                            end
                            %              ElmOffset=ElmOffset+ElmBytes;
                        else
                            if vs_debug
                                fprintf(vs_debug,'  Reading %s data at: %i.\n',RdType{j1},ftell(fidat));
                            end
                            [Temp,NRead]=fread(fidat,rDimen{j1},RdType{j1});
                            if vs_debug
                                fprintf(vs_debug,'  Reading ended at %i.\n',ftell(fidat));
                            end
                        end
                        % Check number of values read and put Temp into the correct index of X
                        if NRead~=prod(rDimen{j1}) %prod(size(Temp))~=prod(rDimen{j1})
                            %NRead,prod(rDimen{j1})
                            %ftell(fidat)
                            if vs_warning
                                warning(Message(1,vs_debug,eName{j1},gName, ...
                                    VarDimCnt,Offset,NBytesPerCell,fidat));
                            end
                        else
                            switch DataType(j1)
                                case 1       % CHARACTE
                                    NValPerElm=rDimen{j1}(1);
                                    Temp=char(Temp);
                                case 2       % COMPLEX
                                    NValPerElm=1;
                                    Temp=complex(Temp(1,:),Temp(2,:)); % Temp=Temp(1,:)+1i*Temp(2,:);
                                case {3,4,5} % INTEGER, LOGICAL, REAL
                                    NValPerElm=1;
                            end
                            switch outputtype
                                case 1 % vs_let
                                    if p*NValPerElm==1
                                        X{2,j1}(VDCellOffset+IdxCell)=Temp(:);
                                    elseif NValPerElm*NRCellsPerRead==1
                                        X{2,j1}(VDCellOffset+IdxCell,:)=Temp(eIndex1{j1});
                                    else
                                        Temp=reshape(Temp,cat(2,NValPerElm,p,NRCellsPerRead));
                                        Temp=Temp(:,eIndex1{j1},:);
                                        Temp=permute(Temp,[3 2 1]); %Cells ElmDims{1:5} Characters
                                        X{2,j1}(VDCellOffset+IdxCell,:)=Temp(:,:);
                                    end
                                case 2 % vs_get
                                    if p*NValPerElm==1
                                        X{2,j1}(VDCellOffset+IdxCell)=num2cell(Temp(:));
                                    elseif NValPerElm*NRCellsPerRead==1
                                        Temp=Temp(eIndex1{j1});
                                        Temp=reshape(Temp,cat(2,eNIndex{j1},1));
                                        X{2,j1}{VDCellOffset+IdxCell}=Temp;
                                    else
                                        Temp=reshape(Temp,[NValPerElm p NRCellsPerRead]);
                                        for c=1:NRCellsPerRead,
                                            T2=Temp(:,eIndex1{j1},c);
                                            T2=transpose(T2);
                                            T2=reshape(T2,cat(2,eNIndex{j1},NValPerElm));
                                            X{2,j1}{VDCellOffset+IdxCell(c)}=T2;
                                        end
                                    end
                            end
                        end
                    end
                end
            end
            if vs_debug
                fprintf(vs_debug,'\n');
            end
        end
    end
    if vs_debug
        fprintf(vs_debug,'\nReshaping data ...\n');
    end

    switch outputtype
        case 1 % vs_let
            for j1=1:jnm
                if DataType(j1)==1
                    X{2,j1}=reshape(X{2,j1},cat(2,gNIndex,eNIndex{j1},ValBytes(j1)));
                else
                    X{2,j1}=reshape(X{2,j1},cat(2,gNIndex,eNIndex{j1}));
                end
                if ~isequal(UnReorder,1:length(UnReorder))
                    if vs_debug
                        fprintf(vs_debug,'Reordering group dimensions ...\n');
                    end
                    X{2,j1}=permute(X{2,j1},[UnReorder length(UnReorder)+1:ndims(X{2,j1})]);
                end
            end
        case 2 % vs_get
            for j1=1:jnm
                X{2,j1}=reshape(X{2,j1},[gNIndex 1]);
                if ~isequal(UnReorder,1:length(UnReorder))
                    if vs_debug
                        fprintf(vs_debug,'Reordering group dimensions ...\n');
                    end
                    X{2,j1}=permute(X{2,j1},UnReorder);
                end
            end
    end
    if vs_debug
        fprintf(vs_debug,'done.\n');
    end
    fclose(fidat);
    if showwaitbar
        delete(hWaitBar)
        drawnow;
    end
    Success=1;
    if vs_debug
        fprintf(vs_debug,'\n\n-------------------------------------------------------\n');
        fprintf(vs_debug,'Successfully finished reading nefis file.\n');
        fprintf(vs_debug,'-------------------------------------------------------\n');
    end
    if vs_debug
        fclose(vs_debug);
    end
catch % end of catch
    if vs_debug
        fclose(vs_debug);
    end
    if fidat>0 % close file if open
        fclose(fidat);
    end
    if showwaitbar & ishandle(hWaitBar) % delete waitbar if exists
        delete(hWaitBar);
    end
    error(lasterr) % error out
end
if jn>0
    eNames= X(1,:);
    uENames = unique(eNames);
    if length(uENames)~=length(eNames)
        % there is at least one duplicate element
        for i = 1:length(uENames)
            j = strmatch(uENames{i},X(1,:),'exact');
            if length(j)>1
                XX = cell(2,length(j));
                XX(2,:) = X(2,j);
                for k = 1:length(j)
                    XX{1,k} = sprintf('Occurence_%i',k);
                end
                X{2,j(1)} = struct(XX{:});
                %
                X(:,j(2:end)) = [];
            end
        end
    end
    X=struct(X{:});
else
    X=X{2,1};
    if (outputtype==2) & isequal(size(X),[1 1]) % vs_get and single cell?
        X=X{1};
    end
end


function Str=Local_vec2str(OrigVec)
% VEC2STR creates a string of the vector
if isempty(OrigVec)
    Str='[]';
elseif length(OrigVec)==1
    Str=sprintf('[ %g ]',OrigVec);
else
    FiniteVec=OrigVec;
    FiniteVec(~isfinite(FiniteVec))=NaN;
    % handle finite values
    B=diff([FiniteVec FiniteVec(end)]);
    C=abs((B(1:end-1)-B(2:end))./max(max(B(2:end),B(1:end-1)),eps))>1e-5 | isnan(B(1:end-1));
    D=[1 find(C)+1];
    E=[FiniteVec(D);B(D);FiniteVec(D)+B(D).*(diff([D length(FiniteVec)+1])-1);diff([D length(FiniteVec)+1])];
    F=find((abs((E(1,2:end)-E(3,1:end-1)-E(2,1:end-1)))<1e-5) & (E(4,2:end)==1) & (E(4,1:end-1)~=1));
    E(3,F)=E(1,1+F);
    E(4,F)=E(4,F)+1;
    E(:,1+F)=[];
    F=find((abs((E(1,2:end)-E(3,1:end-1)-E(2,1:end-1)))<1e-5) & (E(4,2:end)==2));
    E(3,F)=E(1,1+F);
    E(4,F)=E(4,F)+1;
    E(4,1+F)=E(4,1+F)-1;
    E(1,1+F)=E(3,1+F);
    % handle NaNs and infinites
    E(1,~isfinite(E(1,:)))=OrigVec(~isfinite(OrigVec));
    W=E(1,:);
    W(isfinite(W))=0;
    W(isnan(W))=1;
    W(W==inf)=2;
    W(W==-inf)=3;
    lengthW=diff(find(diff([-1 W -1])));
    startW=cumsum([1 lengthW(1:end-1)]);
    valueW=W(startW);
    startW=startW(valueW>0);
    lengthW=lengthW(valueW>0);
    E(4,startW)=lengthW;
    E(:,setdiff(find(W),startW))=[];
    E(2,~isfinite(E(1,:)))=0;
    E(3,~isfinite(E(1,:)))=E(1,~isfinite(E(1,:)));
    % create string
    Str='[';
    for i=1:size(E,2)
        if E(4,i)==1
            Str=[Str sprintf(' %g',E(1,i))];
        elseif E(4,i)==2
            Str=[Str sprintf(' %g %g',E([1 3],i))];
        elseif (E(2,i)==0) | isnan(E(2,i))
            if E(4,i)>3
                if E(1,i)==0
                    Str=[Str sprintf(' zeros(1,%i)',E(4,i))];
                else
                    Str=[Str sprintf(' %g*ones(1,%i)',E(1,i),E(4,i))];
                end
            else
                Str=[Str sprintf(' %g',E(1,i)*ones(1,E(4,i)))];
            end
        else
            if E(2,i)==1
                Str=[Str sprintf(' %g:%g',E([1 3],i))];
            else
                Str=[Str sprintf(' %g:%g:%g',E(1:3,i))];
            end
        end
    end
    Str=[Str ' ]'];
end

function L=Local_isinteger(X)
% Local copy of isinteger
if isempty(X)
    L=logical(X);
else
    L=(X==round(X)) & ~isinf(X);
end

function Str=Message(id,vs_debug,varargin)
switch id
    case 0
        VarDimCnt=varargin{1};
        Offset=varargin{2};
        NBytesPerCell=varargin{3};
        fidat=varargin{4};
        gName=varargin{5};
        Loc=ftell(fidat);
        fseek(fidat,0,1);
        Str=sprintf( ...
            ['Trying to read beyond end of file:\n', ...
            '-> %s\n', ...
            'Cell %i of group %s\n', ...
            'Start indicated at address %u.\n', ...
            'The cell size is %u.\n', ...
            'The file length is %u.\n'], ...
            fopen(fidat),VarDimCnt,gName,Offset,NBytesPerCell,ftell(fidat));
        fseek(fidat,Loc,-1);
    case 1
        eName=deblank(varargin{1});
        gName=deblank(varargin{2});
        VarDimCnt=varargin{3};
        Offset=varargin{4};
        NBytesPerCell=varargin{5};
        fidat=varargin{6};
        Loc=ftell(fidat);
        fseek(fidat,0,1);
        Str=sprintf( ...
            ['Out of data, while reading:\n', ...
            '-> %s\n', ...
            'Element %s of group %s:\n', ...
            'Data of cell %i starts at address %u.\n', ...
            'The cell size is %u.\n', ...
            'The file length is %u.\n'], ...
            fopen(fidat),eName,gName,VarDimCnt,Offset,NBytesPerCell,ftell(fidat));
        fseek(fidat,Loc,-1);
end
if vs_debug
    fprintf(vs_debug,Str);
end

function printPointers(vs_debug,PointerList,Nil)
for i=0:25
    if i==25
        nAddresses=6;
    else
        nAddresses=10;
    end
    sc  = repmat({'       -1'},1,nAddresses);
    HTline = PointerList(i*10+(1:nAddresses));
    sc(HTline~=Nil) = {' %8u'};
    sc{nAddresses+1} = '\n';
    LineFormat = strcat(sc{:});
    fprintf(vs_debug,LineFormat,HTline(HTline~=Nil));
end

function Ind = subcript2ind(Siz,Sub)
Nsub = cellfun('length',Sub);
Nind = prod(Nsub);
for i = 1:length(Nsub)
    Sub{i} = reshape(repmat(Sub{i}(:)',[prod(Nsub(1:i-1)) 1 prod(Nsub(i+1:end))]),Nind,1);
end
if Nind==0
    Ind = zeros(0,1);
else
    Ind = sub2ind([Siz 1],Sub{:});
end
