function qp_validate(varargin)
%QP_VALIDATE Helper function to validate Delft3D-QUICKPLOT.

%----- LGPL --------------------------------------------------------------------
%
%   Copyright (C) 2011-2024 Stichting Deltares.
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

%% default reporting setting
finish = 'default';
teamcity = false;
log_style('latex')
val_dir = '';

%% auto switch on teamcity when MATLAB runs in batch mode
if matlabversionnumber >= 9.06 && batchStartupOptionUsed
    teamcity = true;
end

%% process input arguments: finish, report style, and validation directory
i = 0;
while i < nargin
    i = i+1;
    switch lower(varargin{i})
        case 'finish'
            i = i+1;
            switch lower(varargin{i})
                case {'openlog','none','buildpdf','exit'}
                    finish = lower(varargin{i});
            end
        case 'teamcity'
            teamcity = true;
        case 'no-teamcity'
            teamcity = false;
        case {'latex','html'}
            log_style(lower(varargin{i}))
        otherwise
            val_dir = varargin{i};
    end
end
if strcmp(finish,'default')
    if teamcity
        finish = 'exit';
    else
        finish = 'openlog';
    end
end

%% set validation directory
baseini='validation.ini';
if isempty(val_dir)
    %
    % Would prefer to use uigetdir, but that doesn't compile.
    %
    if matlabversionnumber >= 9.06 && batchStartupOptionUsed
        val_dir = '.';
    else
        [~,val_dir]=uigetfile(baseini,'Select directory containing validation cases');
        if ~ischar(val_dir)
            return
        end
    end
else
    val_dir = absfullfile(val_dir);
end

%% add QUICKPLOT path and switch on diary when runnning stand alone on TeamCity
if isstandalone
    if teamcity
        diary on % will be inserted into build log when QUICKPLOT finishes
    end
else
    qp_path=which('d3d_qp');
    qp_path=fileparts(qp_path);
    addpath(qp_path)
end

%% prepare validation process
extra_files = {'summary','crashed_cases','reading_failed_cases','script_failed_cases','all_cases'};
sametype('');
currdir=pwd;
TC1 = 1;
includes = {};
logid1=-1;
t1=[];
logid2=[];
extlog=[];
FAILED = 'FAILED';
PASSED = 'PASSED';
CREATED = 'CREATED';
NOTAPP = 'N/A';
switch log_style
    case 'latex'
        Color.Page       = 'white';
        Color.Titlebar   = 'magenta!25!cyan!50';
        Color.Table      = {'magenta!10!cyan!20' 'magenta!5!cyan!10'};
        Color.Failed     = 'red!80!black';
        Color.Success    = 'green!80!black';
        Color.Font       = 'black';
        %
        FAILED = '\xmark';
        PASSED = '\cmark';
        CREATED = '\cmark';
        NOTAPP = '-';
        %
        logname='validation_log.tex';
        sc={['\textcolor{',Color.Failed,'}{\xmark}']
            ['\textcolor{',Color.Success,'}{\cmark}']};
        sc3={['\textcolor{',Color.Failed,'}{\xmark}']
            ['\textcolor{',Color.Success,'}{\cmark}']
            ['\textcolor{',Color.Success,'}{\cmark}']};
    otherwise
        Color.Page       = 'FFFFFF';
        Color.Titlebar   = 'BAD4F4';
        Color.Table      = {'DEEBFA' 'E8F1FB'};
        Color.Failed     = 'FF0000';
        Color.Success    = '00AA00';
        Color.Font       = '000000';
        %
        logname='validation_log.html';
        sc={['<font color=',Color.Failed,'>Failed</font> (open <a href="reference">reference folder</a>)']
            ['<font color=',Color.Success,'>Successful</font>']};
        sc3={['<font color=',Color.Failed,'>Failed</font> (open <a href="reference">reference folder</a>)']
            ['<font color=',Color.Success,'>Successful</font> (open <a href="reference">reference folder</a>)']
            ['<font color=',Color.Success,'>Successful</font>']};
end
AnyFail=0;
NTested=0;
NFailed=0;
NReadFailed=0;
NScriptFailed=0;
NCrashed=0;
NSlower=0;
TotTimeRef=0;
TotTime=0;
fs='/'; % always use forward slash here instead of filesep because the backward slash on Windows gives problems in LaTeX ...
sdata=['..',fs,'data',fs];
sref=['..',fs,'reference',fs];
% some file are platform specific, use a prefix to separate them.
if strcmp(computer,'PCWIN64')
    % no prefix on Windows for backward compatibility
    sref_platform_prefix='';
else
    sref_platform_prefix=[lower(computer),'_'];
end
swrk=['..',fs,'work',fs];
slog=['..',fs,'logfiles',fs];
T_=1; ST_=2; M_=3; N_=4; K_=5;
%
Hpb=progressbar('cancel','delete(gcbf)','pause','on');
t_init = now;
pHpb=get(Hpb,'position');
ssz=qp_getscreen;
set(Hpb,'position',[ssz(1)+10 ssz(2)+ssz(4)-pHpb(4)-30 pHpb(3) pHpb(4)])
%
UserInterrupt=0;
if matlabversionnumber<7.03
    error('MATLAB versions older than 7.3 are no longer supported.')
else
    saveops={'-v7.3', '-mat'};
    fprintf('The default character set on this system is %s\n',feature('DefaultCharacterSet'))
end
%
% Allow for a large number of messages
%
ui_message('max',10000)
set(findall(0,'tag','UI_MESSAGE window'),'position',[ssz(1)+10 ssz(2)+40 pHpb(3) ssz(4)-pHpb(4)-120])
DefFigProp.defaultfigure = qp_settings('defaultfigure',-999);
qp_settings('defaultfigure','')
DefFigProp.defaultfigurecolor = qp_settings('defaultfigurecolor',-999);
qp_settings('defaultfigurecolor',get(0,'factoryuicontrolbackgroundcolor')*255)
DefFigProp.defaultaxescolor = qp_settings('defaultaxescolor',-999);
qp_settings('defaultaxescolor',[255 255 255])
DefFigProp.boundingbox = qp_settings('boundingbox',-999);
qp_settings('boundingbox',0)
current_procdef='';
inTable2 = false;
try
    full_ln=fullfile(val_dir,logname);
    if ~localexist(fullfile(val_dir,baseini))
        ui_message('error','Invalid validation directory: ''%s'' is missing.',baseini)
        if ishandle(Hpb)
            delete(Hpb);
        end
        return
    end
    %
    [p,f,e] = fileparts(full_ln);
    extlog.files = extra_files;
    for ifile = 1:length(extra_files)
        fName = extra_files{ifile};
        %
        fileName = [f '_' extra_files{ifile} e];
        extlog.(fName).filename = fileName;
        %
        extlog.(fName).fid = fopen([p filesep fileName],'w','n','US-ASCII');
        extlog.(fName).empty = true;
    end
    %
    logid1=fopen(full_ln,'w','n','US-ASCII');
    if logid1<0
        ui_message('error',{'Cannot open logfile for validation report.','Stopping validation process.'})
        return
    else
        t1 = write_header(logid1,'MAIN',Color,extlog);
    end
    cd(val_dir)
    d=dir('*');
    for i=length(d):-1:1
        if ~d(i).isdir || any(strcmp(d(i).name,{'.','..','common','.svn','.git'}))
            d(i)=[];
        end
    end
    %
    % Sorting required for MATLAB 5.3 ...
    %
    if ~isempty(d)
        [~,I]=sort(upper({d.name}));
        d=d(I);
        [d.dt] = deal(NaN);
    end
    %
    % Start up the QuickPlot interface such that this time is not included
    % in the timing of the first testcase. Hide the plot manager because
    % this dialog affects the timing.
    %
    d3d_qp('hideplotmngr');
    d3d_qp('set','<DEFAULT>');
    new_procdef = fullfile(val_dir,'proc_def.dat');
    if exist(new_procdef,'file')
        current_procdef = qp_settings('delwaq_procdef');
        qp_settings('delwaq_procdef',new_procdef)
    end
    %
    sumt = 0;
    numt = 0;
    for i=1:length(d)
        timid = fopen(fullfile(val_dir,d(i).name,'reference','timing.txt'),'r','n','US-ASCII');
        if timid>0
            [dt2,cnt] = fscanf(timid,'%f',1);
            if cnt==1
                d(i).dt = dt2;
                sumt = sumt + dt2;
                numt = numt + 1;
            end
            fclose(timid);
        end
    end
    %
    if numt>0
        avg_dt = sumt/numt;
    else
        avg_dt = 4; % estimate of average time based on 440 test cases
    end
    case_dt = [d.dt];
    case_dt(isnan(case_dt)) = avg_dt;
    acc_dt = (now-t_init)*86400;
    tot_dt = max(1,acc_dt + sum(case_dt)); % at least a second
    %
    includes = repmat({''},length(d),2);
    %if teamcity
    %    fprintf('##teamcity[testSuiteStarted name=''quickplot tests'']\n');
    %end
    %% loop over validation test cases
    for i=1:length(d)
        progressbar(acc_dt/tot_dt,Hpb,'title',d(i).name);
        ui_message('',['Case: ',d(i).name])
        if teamcity
            fprintf('##teamcity[testStarted name=''%s'']\n', d(i).name);
        else
            fprintf('----- %s %s\n', d(i).name, repmat('-',1,93 - length(d(i).name)));
        end
        includes{i,1} = [d(i).name '/'];
        includes{i,2} = logname;
        NTested=NTested+1;
        DiffFound=0;
        color    = '';
        frcolor  = Color.Success;
        lgcolor  = Color.Font;
        result   = '';
        frresult = PASSED;
        lgresult = NOTAPP;
        logid2=[];
        TC2=1;
        t2=[];
        dt2_old=0;
        Crash = [];
        try
            cd(fullfile(val_dir,d(i).name));
            CaseInfo='case.ini';
            if isempty(dir('data'))
                f = dir('*');
                ensure_directory('data');
                for ii=1:length(f)
                    if ~strcmp(f(ii).name,'.') && ~strcmp(f(ii).name,'..')
                        movefile(f(ii).name,'data');
                    end
                end
            end
            dd=dir(['data' fs '*']);
            if ~localexist(CaseInfo) && length(dd)>2
                CID=inifile('new');
                CID=inifile('set',CID,'','FileName',dd(3).name);
                CID=inifile('set',CID,'logfilecheck','example.qplog','example.png');
                inifile('write',CaseInfo,CID);
            end
            if localexist(CaseInfo)
                CaseInfo=inifile('open',CaseInfo);
                logid2=fopen(logname,'w','n','US-ASCII');
                dt2_old = d(i).dt;
                t2 = write_header(logid2,d(i).name,Color);
                emptyTable2 = true;
                %
                % check for log files to run ...
                %
                if isempty(dir('logfiles'))
                    ensure_directory('logfiles');
                end
                logs=dir(['logfiles' fs '*.qplog']);
                logm=dir(['logfiles' fs '*.m']);
                logs=cat(1,logs,logm);
                %
                % check for reference directory...
                %
                if isempty(dir('reference'))
                    ensure_directory('reference');
                end
                %
                % Switch to work directory...
                %
                if isempty(dir('work'))
                    ensure_directory('work');
                end
                cd('work')
                workdir=pwd;
                %
                % Delete all files in work directory (but make sure that we actually are in the work directory) ...
                %
                if ~isequal(workdir(end-3:end),'work')
                    write_log(logid2,'No work directory?');
                    error('No work directory?')
                end
                delete('*')
                FileName=inifile('getstring',CaseInfo,'','FileName','');
                FileName2=inifile('getstring',CaseInfo,'','FileName2','');
                if length(FileName)<7 || ~isequal(lower(FileName(1:7)),'http://')
                    FileName = portpath(FileName);
                end
                if isempty(FileName2)
                    FileName2={};
                    write_section(logid2,'Opening ''%s''',protected(FileName));
                else
                    FileName2=portpath(FileName2);
                    write_section(logid2,'Opening ''%s'' with ''%s''',protected(FileName),protected(FileName2));
                    FileName2={[sdata,FileName2]};
                end
                if length(FileName)<7 || ~isequal(lower(FileName(1:7)),'http://')
                    FileName = [sdata,FileName];
                end
                d3d_qp('openfile',FileName,FileName2{:});
                FI=qpfile;
                if ~isstruct(FI)
                    error('Error opening file ''%s''.',FileName);
                end
                [ChkOK,Dms]=qp_getdata(FI,'domains');
                write_log(logid2,'Reading domains: %s',sc{ChkOK+1});
                if isempty(Dms)
                    Dms={'no name'};
                end
                dmx=length(Dms);
                Props={};
                for dm=1:dmx
                    [Chk,P]=qp_getdata(FI,dm);
                    write_log(logid2,'Reading fields of domain ''%s'': %s',protected(Dms{dm}),sc{Chk+1});
                    ChkOK = Chk & ChkOK;
                    if ~Chk
                        continue
                    end
                    %
                    szFail = {};
                    szChkOK = true;
                    for p=1:length(P)
                        if strcmp(P(p).Name,'-------')
                            P(p).Size=[0 0 0 0 0];
                        else
                            [Chk,P(p).Size]=qp_getdata(FI,dm,P(p),'size');
                            if ~Chk
                                szFail{end+1} = P(p).Name;
                            end
                            szChkOK = Chk & szChkOK;
                        end
                    end
                    write_log(logid2,'Reading sizes of data fields of domain ''%s'': %s',protected(Dms{dm}),sc{szChkOK+1});
                    if ~szChkOK
                        szFail = sprintf('''%s'', ',szFail{:});
                        write_log(logid2,'Problem encountered while determining the size of %s',szFail(1:end-2));
                    end
                    ChkOK = szChkOK & ChkOK;
                    Props{dm}=P;
                end
                write_log(logid2,'');
                if ChkOK
                    CmpFile='datafields.mat';
                    RefFile=[sref CmpFile];
                    WrkFile=[swrk CmpFile];
                    if localexist(RefFile)
                        cmpFile=localload(RefFile);
                        if isfield(cmpFile,'Props')
                            PrevProps=cmpFile.Props;
                        else
                            PrevProps=cmpFile.Data;
                        end
                        DiffFound = vardiff(Props,PrevProps)>1;
                        write_log(logid2,'Comparing new and old fields:');
                        if DiffFound
                            DiffMessage = 1; % Failed
                            localsave(WrkFile,Props,saveops);
                            if length(Props)~=length(PrevProps)
                                write_log(logid2,'The number of domains differs.');
                                write_log(logid2,'Reference data set contains %i domains.',length(PrevProps));
                                write_log(logid2,'New data set contains %i domains.',length(Props));
                            else
                                JustAddedData=1;
                                JustAddedFields=1;
                                for dm=1:length(Props)
                                    %
                                    % Check for differences in the datafields
                                    Prop = Props{dm};
                                    PropRef = PrevProps{dm};
                                    if isempty(PropRef)
                                        write_log(logid2,'Old datafields record is empty.');
                                        JustAddedData=0;
                                    else
                                        pName = {Prop.Name};
                                        pNameRef = {PropRef.Name};
                                        pnAdded = setdiff(pName,pNameRef);
                                        pnRemoved = setdiff(pNameRef,pName);
                                        if length(Props)>1
                                            write_log_domain(logid2,Dms{dm});
                                        end
                                        if ~isempty(pnAdded)
                                            pnAdded = protected(pnAdded);
                                            write_log(logid2,'New datafields:');
                                            write_list(logid2,pnAdded);
                                            Prop(ismember(pName,pnAdded)) = [];
                                            pName = {Prop.Name};
                                        end
                                        if ~isempty(pnRemoved)
                                            JustAddedData=0;
                                            pnRemoved = protected(pnRemoved);
                                            write_log(logid2,'Deleted datafields:');
                                            write_list(logid2,pnRemoved);
                                            PropRef(ismember(pNameRef,pnRemoved)) = [];
                                            pNameRef = {PropRef.Name};
                                        end
                                        %
                                        % Make sure that the datafields are in the same order
                                        if ~isequal(pName,pNameRef)
                                            write_log(logid2,'Order of datafields changed.');
                                            [~,iProp] = unique(pName);
                                            [~,iPropRef] = unique(pNameRef);
                                            Prop = Prop(iProp);
                                            PropRef = PropRef(iPropRef);
                                        end
                                        %
                                        % Check for differences in the property field names
                                        fProp = fieldnames(Prop);
                                        fPropRef = fieldnames(PropRef);
                                        fAdded = setdiff(fProp,fPropRef);
                                        fRemoved = setdiff(fPropRef,fProp);
                                        if ~isempty(fAdded)
                                            fAdded = protected(fAdded);
                                            write_log(logid2,'New property fields:');
                                            write_list(logid2,fAdded);
                                            Prop = rmfield(Prop,fAdded);
                                            fProp = fieldnames(Prop);
                                        end
                                        if ~isempty(fRemoved)
                                            JustAddedFields=0;
                                            fRemoved = protected(fRemoved);
                                            write_log(logid2,'Deleted property fields:');
                                            write_list(logid2,fRemoved);
                                            PropRef = rmfield(PropRef,fRemoved);
                                            fPropRef = fieldnames(PropRef);
                                        end
                                        %
                                        % Make sure that the property fields are in the same order
                                        if ~isequal(fProp,fPropRef)
                                            write_log(logid2,'Order of property fields changed.');
                                            [~,iProp] = sort(fProp);
                                            [~,iPropRef] = sort(fPropRef);
                                            fProp = fProp(iProp);
                                            %
                                            cProp = struct2cell(Prop);
                                            cProp = cProp(iProp,:);
                                            Prop = cell2struct(cProp,fProp,1);
                                            %
                                            cPropRef = struct2cell(PropRef);
                                            cPropRef = cPropRef(iPropRef,:);
                                            PropRef = cell2struct(cPropRef,fProp,1);
                                        end
                                    end
                                    %
                                    if vardiff(Prop,PropRef)>1
                                        JustAddedData=0;
                                        if isequal(log_style,'latex')
                                            vardiff(Prop,PropRef,logid2,'latex-longtable','new','reference');
                                        else
                                            vardiff(Prop,PropRef,logid2,log_style,'new','reference');
                                        end
                                    end
                                    drawnow
                                end
                                if JustAddedData && JustAddedFields
                                    DiffMessage = 2; % Successful but still option to open reference folder
                                    DiffFound = 0;
                                    if strcmp(frcolor,Color.Success)
                                        frcolor=Color.Font;
                                    end
                                end
                            end
                        else
                            write_log(logid2,'No differences.');
                            DiffMessage = 3; % Successful
                        end
                        write_log(logid2,'Conclusion: %s',sc3{DiffMessage});
                    else
                        localsave(RefFile,Props,saveops);
                        localsave(WrkFile,Props,saveops);
                    end
                    if DiffFound
                        frcolor=Color.Failed;
                        frresult=[FAILED ': Data fields differ.'];
                    end
                else
                    frcolor=Color.Failed;
                    frresult=[FAILED ': Error while reading data.'];
                end
                %
                NP=length(P);
                NL=length(logs);
                NT=NP+NL;
                %
                write_begin_table(logid2,Color)
                inTable2 = true;
                datacheck=inifile('get',CaseInfo,'datacheck','default',1);
                P=Props{dmx};
                try
                    for p=1:NP
                        if progressbar((acc_dt+case_dt(i)*(p-1)/NT)/tot_dt,Hpb)<0
                            write_table2_line(logid2,Color.Table{TC2},'','','',''); % at least one line needed in table
                            write_end_table(logid2,emptyTable2);
                            inTable2 = false;
                            UserInterrupt=1;
                            error('User interrupt');
                        end
                        if ~strcmp(P(p).Name,'-------')
                            if P(p).NVal<0
                                write_table2_line(logid2,Color.Table{TC2},P(p).Name,NOTAPP,'','Check not applicable.');
                                emptyTable2 = false;
                                TC2=3-TC2;
                            elseif ~inifile('get',CaseInfo,'datacheck',P(p).Name,datacheck)
                                write_table2_line(logid2,Color.Table{TC2},P(p).Name,NOTAPP,NOTAPP,'Check skipped.');
                                emptyTable2 = false;
                                TC2=3-TC2;
                            else
                                PName=P(p).Name;
                                PName_double = strmatch(PName,{P(1:p-1).Name},'exact');
                                PName=str2file(PName);
                                CmpFile=[PName '.mat'];
                                if PName_double
                                    CmpFile=[PName sprintf('.(%i).mat',PName_double+1)];
                                end
                                RefFile=[sref CmpFile];
                                WrkFile=[swrk CmpFile];
                                idx={};
                                subf={};
                                if ~isempty(P(p).DimFlag) && P(p).DimFlag(ST_)
                                    idx={1};
                                    if P(p).DimFlag(T_)
                                        if P(p).DimFlag(M_) || P(p).DimFlag(N_)
                                            idx=[{P(p).Size(T_)} idx(:)'];
                                        else
                                            idx=[{1:min(10,P(p).Size(T_))} idx(:)'];
                                        end
                                    end
                                end
                                [Chk,subfields]=qp_getdata(FI,dm,P(p),'subfields');
                                if Chk && ~isempty(subfields)
                                    subf={length(subfields)};
                                end
                                [Chk,Data]=qp_getdata(FI,dm,P(p),'griddata',subf{:},idx{:});
                                if ~Chk && isempty(Data)
                                    write_table2_line(logid2,Color.Table{TC2},P(p).Name,color_write(FAILED,Color.Failed),'','Failed to get data');
                                    emptyTable2 = false;
                                    TC2=3-TC2;
                                    Chk = 0;
                                else
                                    write_table2_line(logid2,Color.Table{TC2},P(p).Name,sc{Chk+1},[],'');
                                    emptyTable2 = false;
                                    TC2=3-TC2;
                                end
                                if ~Chk
                                    frcolor=Color.Failed;
                                    frresult=sprintf('%s: Error retrieving data for ''%s''.',FAILED,protected(P(p).Name));
                                else
                                    try
                                        if localexist(RefFile)
                                            cmpFile=localload(RefFile);
                                            PrevData=cmpFile.Data;
                                        else
                                            PrevData=[];
                                        end
                                        if isstruct(PrevData)
                                            if isfield(Data,'TRI')
                                                Data.TRI = sortrows(sort(Data.TRI')'); %#ok<TRSRT>
                                            end
                                            if isfield(PrevData,'TRI')
                                                PrevData.TRI = sortrows(sort(PrevData.TRI')'); %#ok<TRSRT>
                                            end
                                            %
                                            DiffFound=vardiff(Data,PrevData);
                                            addedfields = {};
                                            if DiffFound==2
                                                newfields=fields(Data);
                                                oldfields=fields(PrevData);
                                                if all(ismember(oldfields,newfields))
                                                    addedfields = setdiff(newfields,oldfields);
                                                    newData = Data;
                                                    for f = 1:length(addedfields)
                                                        newData = rmfield(newData,addedfields{f});
                                                    end
                                                    DiffFound=vardiff(newData,PrevData)>1;
                                                    if ~DiffFound
                                                        DiffFound = -1;
                                                    end
                                                else
                                                    % new data structure misses some fields
                                                    % that were included in the old
                                                    % (reference) data structure
                                                    DiffFound = 1;
                                                end
                                            else
                                                newfields='';
                                                DiffFound=DiffFound>1;
                                            end
                                            if DiffFound
                                                localsave(WrkFile,Data,saveops);
                                                write_table2_line(logid2,[],[],[],1,sc3{2-DiffFound});
                                                emptyTable2 = false;
                                                if ~isempty(addedfields)
                                                    write_log(logid2,'New Fields:');
                                                    write_log(logid2,addedfields);
                                                    if isequal(frcolor,Color.Success) % switch to black colour only if no real error has occurred
                                                        frcolor=Color.Font;
                                                    end
                                                    if DiffFound>0
                                                        write_log(logid2,'');
                                                    end
                                                end
                                                if DiffFound>0
                                                    if ~isempty(addedfields)
                                                        vardiff(newData,PrevData,logid2,log_style,'new','reference');
                                                    else
                                                        vardiff(Data,PrevData,logid2,log_style,'new','reference');
                                                    end
                                                    frcolor=Color.Failed;
                                                end
                                                write_table2_line(logid2,[],[],[],2,'');
                                                ChkOK = DiffFound<=0;
                                                if ~ChkOK
                                                    frresult=sprintf('%s: Data changed for ''%s''.',FAILED,protected(P(p).Name));
                                                end
                                                localsave([PName,'.mat'],Data,saveops);
                                            else
                                                write_table2_line(logid2,[],[],[],sc{2-DiffFound},'');
                                                emptyTable2 = false;
                                            end
                                        else
                                            localsave(RefFile,Data,saveops);
                                            localsave(WrkFile,Data,saveops);
                                            write_table2_line(logid2,[],[],[],CREATED,'');
                                            emptyTable2 = false;
                                            if isequal(frresult,PASSED)
                                                frcolor=Color.Font;
                                                frresult=CREATED;
                                            end
                                        end
                                    catch Crash
                                        frcolor=Color.Failed;
                                        frresult=sprintf('%s: Error checking one or more data fields.',FAILED);
                                        stack_cellstr = cat(1,{Crash.message},stack2str(Crash.stack));
                                        for istack = 1:length(stack_cellstr)
                                            stack_cellstr{istack} = protected(stack_cellstr{istack});
                                        end
                                        write_table2_line(logid2,[],[],[],sc{1},color_write(stack_cellstr,Color.Failed));
                                        emptyTable2 = false;
                                        Crash = [];
                                    end
                                end
                            end
                        else
                            write_table_rule(logid2,Color)
                        end
                        flush(logid2)
                    end
                catch e
                    % no good example yet ...
                end
                write_end_table(logid2,emptyTable2);
                inTable2 = false;
                drawnow
                %
                if isempty(logs)
                    write_rule(logid2);
                    write_log(logid2,'No log files to run for validation.');
                else
                    lgcolor=Color.Success;
                    lgresult=PASSED;
                    for lg=1:NL
                        if progressbar((acc_dt+case_dt(i)*(NP+lg-1)/NT)/tot_dt,Hpb)<0
                            UserInterrupt=1;
                            error('User interrupt');
                        end
                        logf=logs(lg).name;
                        write_rule(logid2);
                        write_section(logid2,'Results for log file: %s',protected(logf));
                        echo_logfile(logid2,[slog,logf]);
                        d3d_qp('reset');
                        d1=dir; d1={d1.name};
                        m1=ui_message('getall');
                        d3d_qp('run',[slog,logf]);
                        d2=dir; d2={d2.name};
                        m2=ui_message('getall');
                        checkfs=setdiff(d2,d1);
                        if length(m2)>length(m1)
                            diffm=m2(length(m1)+2:length(m2));
                            for mi = 1:length(diffm)
                                write_log(logid2,'%s: %s',protected(logf),color_write(protected(diffm{mi}),Color.Failed));
                            end
                        end
                        if isempty(checkfs)
                            write_log(logid2,'No file to check.');
                            lgcolor=Color.Failed;
                            lgresult=sprintf('%s: No check on ''%s''.',FAILED,protected(logf));
                        else
                            %checkf=inifile('get',CaseInfo,'logfilecheck',logf,'');
                            for icheck=1:length(checkfs)
                                checkf=checkfs{icheck};
                                write_log1(logid2,'Checking File ''%s'': ',protected(checkf));
                                showfig=0;
                                [~,~,ext]=fileparts(checkf);
                                % reference files are generic by default
                                reffile=[sref,checkf];
                                args={};
                                switch lower(ext)
                                    case '.png'
                                        % matching DPI for default size: 53, 54, 56, 57 61, 62, 64, 65, 66, 69
                                        args={'skip',256};
                                        showfig=1;
                                        % figures are platform specific
                                        reffile=[sref,sref_platform_prefix,checkf];
                                    case '.asc'
                                        args={'skip',71};
                                    case '.mat'
                                        args={'skip',128};
                                    case '.dbf'
                                        args={'skip',16};
                                end
                                if ~localexist(reffile)
                                    copyfile(checkf,reffile);
                                    write_log(logid2,'reference file created');
                                    if isequal(lgresult,PASSED)
                                        lgcolor=Color.Font;
                                        lgresult=CREATED;
                                    end
                                    if showfig
                                        include_figure(logid2,reffile(4:end));
                                    end
                                else
                                    [Eql,Msg]=filesequal(checkf,reffile,args{:});
                                    diffimg={};
                                    if ~Eql
                                        try
                                            switch lower(ext)
                                                case '.png'
                                                    I1=imread(checkf);
                                                    I2=imread(reffile);
                                                    if ~isequal(size(I1),size(I2))
                                                        Msg='The bitmap sizes are different.';
                                                    else
                                                        I1=double(I1);
                                                        I2=double(I2);
                                                        diffimg=[checkf(1:end-4) '_diff.png'];
                                                        imwrite(1-abs(I1-I2)/255,diffimg);
                                                        diffimg={['work/' diffimg]};
                                                        Msg='The bitmap images are different.';
                                                    end
                                            end
                                        catch
                                        end
                                    end
                                    if ~isempty(Msg)
                                        Msg=cat(2,': ',Msg);
                                    end
                                    write_log(logid2,[sc{1+Eql},Msg]);
                                    if Eql
                                        if showfig
                                            include_figure(logid2,reffile(4:end));
                                        end
                                        % if work file is equal to reference file, delete the work file
                                        delete(checkf)
                                    else
                                        if showfig
                                            include_diff_figures(logid2,[{reffile(4:end)},{['work/' checkf]},diffimg(:)'],Color);
                                        end
                                        lgcolor=Color.Failed;
                                        lgresult=[FAILED ': Log file results differ.'];
                                    end
                                end
                            end
                        end
                        flush(logid2)
                        drawnow
                    end
                end
            else
                frcolor=Color.Font;
                frresult=[FAILED ': case.ini missing.'];
            end
        catch Crash
            if inTable2
                write_end_table(logid2,emptyTable2);
                inTable2  = false;
            end
            color=Color.Failed;
            AnyFail=1;
            if UserInterrupt
                result=[FAILED ': User interrupt.'];
            else
                result=[FAILED ': Validation test crashed - '];
            end
        end
        d3d_qp('closefile');
        figs = get_nondialogs;
        if ~isempty(figs)
            ui_message('warning','%i figures were not closed properly.',length(figs))
            d3d_qp('closeallfig')
        end
        if ~isempty(logid2)
            if ~isempty(Crash)
                write_log(logid2,color_write(protected(Crash.message),Color.Failed,true));
                stack_cellstr = stack2str(Crash.stack);
                for istack = 1:length(stack_cellstr)
                    write_log(logid2,color_write(protected(stack_cellstr{istack}),Color.Failed,true));
                end
            end
            if ~isempty(t2)
                [dt2,dt2_str,slower] = write_footer(logid2,d(i).name,Color,t2,dt2_old);
            end
            fclose(logid2);
            logid2=[];
            %
            if isnan(dt2_old)
                timid = fopen([sref,'timing.txt'],'w','n','US-ASCII');
                fprintf(timid,'%5.1f',dt2);
                fclose(timid);
                timid = fopen([swrk,'timing.txt'],'w','n','US-ASCII');
                fprintf(timid,'%5.1f',dt2);
                fclose(timid);
            end
        elseif ~isempty(Crash)
            result  = [result Crash.message];
            if ~isempty(t2)
                dt2 = (now-t2)*86400;
                dt2_str = duration(dt2);
                slower  = dt2>dt2_old;
            else
                dt2 = 0;
                dt2_str = duration(dt2);
                slower = false;
            end
        end
        CaseReadFailed = strncmp(FAILED,frresult,5);
        CaseScriptFailed = strncmp(FAILED,lgresult,5);
        CaseCrashed = strncmp(FAILED,result,5) & ~(CaseReadFailed | CaseScriptFailed);
        CaseFailed = CaseReadFailed | CaseScriptFailed | CaseCrashed;
        if teamcity && CaseFailed
            if CaseCrashed
                fprintf('##teamcity[testFailed name=''%s'' message=''case crashed.'']\n', d(i).name)
            elseif CaseReadFailed
                fprintf('##teamcity[testFailed name=''%s'' message=''differences when checking data.'']\n', d(i).name)
            elseif CaseScriptFailed
                fprintf('##teamcity[testFailed name=''%s'' message=''differences when running script.'']\n', d(i).name)
            end
        end
        NReadFailed = NReadFailed + CaseReadFailed;
        NScriptFailed = NScriptFailed + CaseScriptFailed;
        NCrashed = NCrashed + CaseCrashed;
        NFailed = NFailed + CaseFailed;
        if isnan(dt2_old)
            TotTimeRef = TotTimeRef + dt2;
        else
            TotTimeRef = TotTimeRef + dt2_old;
            NSlower = NSlower + slower;
        end
        TotTime    = TotTime + dt2;
        AnyFail=AnyFail | CaseFailed;
        if AnyFail && ishandle(Hpb)
            progressbar(Hpb,'color','r')
        elseif ~ishandle(Hpb)
            UserInterrupt=1;
        end
        if ~isempty(result)
            extlog = write_table1_line(logid1,extlog,Color,CaseReadFailed,CaseScriptFailed,CaseCrashed,Color.Table{TC1},d(i).name,color,result,[],[],logname,dt2_str);
            TC1=3-TC1;
        else
            extlog = write_table1_line(logid1,extlog,Color,CaseReadFailed,CaseScriptFailed,CaseCrashed,Color.Table{TC1},d(i).name,frcolor,frresult,lgcolor,lgresult,logname,dt2_str);
            TC1=3-TC1;
        end
        flush(logid1);
        if teamcity
            fprintf('##teamcity[testFinished name=''%s'']\n', d(i).name)
        end
        if UserInterrupt
            break
        end
        acc_dt = acc_dt + case_dt(i);
    end
catch err
    if ~isempty(logid2)
        fclose(logid2);
    end
    if logid1>0
        write_table_error(logid1,extlog,Color,err)
        AnyFail=1;
    else
        write_table_error(logid1,[],Color,err)
    end
end
%if teamcity
%    fprintf('##teamcity[testSuiteFinished name=''quickplot tests'']\n');
%end
if ishandle(Hpb)
    delete(Hpb);
end
cd(currdir)
if ~isempty(current_procdef)
    qp_settings('delwaq_procdef',current_procdef)
end
if logid1>0
    write_summary(logid1,extlog,NFailed,NReadFailed,NScriptFailed,NCrashed,NTested,NSlower,TotTimeRef,TotTime);
    write_includes(logid1,includes);
    if ~isempty(t1)
        write_footer(logid1,'MAIN',Color,t1,NaN);
    end
    for fName = extlog.files
        fclose(extlog.(fName{1}).fid);
    end
    fclose(logid1);
end
fclose('all');
if AnyFail
    ui_message('error','Testbank failed on %i out of %i cases! Check log file.\n',NFailed,NTested)
else
    ui_message('','Testbank completed successfully (%i cases).\n',NTested)
end
qp_settings('defaultfigure',DefFigProp.defaultfigure)
qp_settings('defaultfigurecolor',DefFigProp.defaultfigurecolor)
qp_settings('defaultaxescolor',DefFigProp.defaultaxescolor)
qp_settings('boundingbox',DefFigProp.boundingbox)
switch finish
    case 'openlog'
        if matlabversionnumber>5
            ops={'-browser'};
        else
            ops={};
        end
        try
            web(full_ln,ops{:});
        catch
        end
    case 'buildpdf'
        try
            ui_message('','Building PDF of validation report.')
            cd(val_dir);
            pdflatex = '"C:\Program Files\MiKTeX\miktex\bin\x64\pdflatex"';
            ui_message('','Build iteration 1 of 2.')
            [status,result] = system([pdflatex,' ',logname]);
            if status==0
                ui_message('','Build iteration 2 of 2.')
                [status,result] = system([pdflatex,' ',logname]);
            end
            if status ~= 0
                ui_message('error',result)
            else
                ui_message('','PDF file created.')
            end
        catch Ex
            ui_message('error',e)
            qp_error('Catch while building PDF of validation report:',Ex,'qp_validate')
        end
        cd(currdir)
    case 'exit'
        exit
end


function str = protected(str)
if iscell(str)
    for i = 1:numel(str)
        str{i} = protected(str{i});
    end
else
    switch log_style
        case 'latex'
            str = strrep(str,'\','\char`\\');
            str = strrep(str,'_','\_');
            str = strrep(str,'#','\#');
            str = strrep(str,'$','\$');
            str = strrep(str,'&','\&');
            str = strrep(str,'%','\%');
            str = strrep(str,'^','\^');
            str = strrep(str,'³','$^3$');
        otherwise
            str = strrep(str,'&','&#38;'); % &amp;
            str = strrep(str,'<','&#60;'); % &lt;
            str = strrep(str,'>','&#62;'); % &gt;
            str = strrep(str,'"','&#34;'); % &quot;
            str = strrep(str,'''','&#39;'); % &apos;
    end
end


function str = makelabel(str)
switch log_style
    case 'latex'
        str = strrep(str,'&','_and_');
end



function str = protect_filename(str)
switch log_style
    case 'latex'
        str = strrep(str,' ','" "');
        %str = strrep(str,'_','\_');
end


function t = write_header(logid,casename,Color,extlog)
stalone='';
if isstandalone
    stalone=' (standalone)';
end
c = clock;
versionstr = d3d_qp('version'); % returns "source code version" or "vA.B.hash (platform)"
if versionstr(1) == 'v'
    % remove the leading v
    versionstr = versionstr(2:end);
end
switch log_style
    case 'latex'
        if strcmp(casename,'MAIN')
            % main document
            fprintf(logid,'%s\n','\documentclass[table]{deltares_manual}');
            fprintf(logid,'%s\n','\usepackage{pdflscape}');
            fprintf(logid,'%s\n','\usepackage{pifont}% http://ctan.org/pkg/pifont');
            fprintf(logid,'%s\n','\newcommand{\cmark}{\ding{51}}%');
            fprintf(logid,'%s\n','\newcommand{\xmark}{\ding{55}}%');
            fprintf(logid,'\n');
            fprintf(logid,'%s\n','\begin{document}');
            fprintf(logid,'%% %s\n','\pagestyle{empty}');
            fprintf(logid,'%% %s\n','\includepdf[pages=1,offset=72 -70]{pictures/Delft3D-cover_hydro.pdf} % links-rechts past precies');
            fprintf(logid,'%% %s\n','\cleardoublepage');
            fprintf(logid,'%s%s%s\n','\pagecolor{',Color.Page,'}');
            fprintf(logid,'\n');
            fprintf(logid,'%s\n','\input{common/program_names}');
            fprintf(logid,'\n');
            fprintf(logid,'%s\n','\title{\QUICKPLOT\ Testing}');
            fprintf(logid,'%s\n','\subtitle{Automated testing report}');
            fprintf(logid,'%s\n','\manualtype{Regression Document}');
            fprintf(logid,'%s\n','\distribution{}');
            fprintf(logid,'%s{%s%s}\n','\version',versionstr,stalone);
            fprintf(logid,'%s\n','\deltarestitle');
            fprintf(logid,'%s%s%s%s%s\n','\rowcolors{1}{',Color.Table{1},'}{',Color.Table{2},'}');
            fprintf(logid,'\n');
            fprintf(logid,'%s\n','\begin{landscape}');
            fprintf(logid,'%s{%s} %s{Chap:%s}\n','\chapter','Summary of test results','\label','Summary');
            for fName = extlog.files
                fprintf(logid,'\\input{"%s"}\n',extlog.(fName{1}).filename);
            end
            fprintf(logid,'%s\n\n','\end{landscape}');
        else
            fprintf(logid,'%% %s %s\n','Delft3D-QUICKPLOT validation report for case ',casename);
            casetype = sametype(casename);
            if ~isempty(casetype)
                fprintf(logid,'%s{%s} %s{Chap:%s}\n','\chapter',protected(casetype),'\label',makelabel(casetype));
            end
            fprintf(logid,'%s{%s} %s{Sec:%s}\n','\section',protected(casename),'\label',makelabel(casename));
            fprintf(logid,'\n');
            fprintf(logid,'\n');
            if exist('doc/description.tex','file')
                fprintf(logid,'%s%s%s\n','\input{"',casename,'/doc/description.tex"}');
                fprintf(logid,'\n');
            end
        end
    otherwise
        fprintf(logid,'<html>\n<title>Delft3D-QUICKPLOT validation report</title>\n<body bgcolor=%s><font face=arial>\n',Color.Page);
        fprintf(logid,'<table bgcolor=%s><tr><td colspan=2 bgcolor=%s><b>Deltares validation report</b></td></tr>\n<tr><td>Program:</td><td>Delft3D-QUICKPLOT</td></tr>\n', ...
            Color.Table{1},Color.Titlebar);
        fprintf(logid,'<tr bgcolor=%s><td>Version:</td><td>%s%s</td></tr>\n<tr><td>Date:</td><td>%4.4i-%2.2i-%2.2i %2.2i:%2.2i:%02.0f</td></tr>\n',Color.Table{2},d3d_qp('version'),stalone,c);
        fprintf(logid,'<tr bgcolor=%s><td>MATLAB version:</td><td>%s</td></tr>\n',Color.Table{1},versionstr);
        fprintf(logid,'<tr bgcolor=%s><td>Computer type:</td><td>%s</td></tr>\n',Color.Table{2},computer);
        if strcmp(casename,'MAIN')
            fprintf(logid,'<tr bgcolor=%s><td>Case:</td><td>%s</td></tr>\n',Color.Table{1},casename);
        end
        fprintf(logid,'</table><br>\n');
        if strcmp(casename,'MAIN')
            fprintf(logid,'<table bgcolor=%s>\n<tr bgcolor=%s><td><b>Validation case</b></td><td><b>Result<br>file read</b></td><td><b>Result<br>log files</b></td><td><b>View Log</b></td><td><b>Timing</b></td></tr>\n',Color.Table{1},Color.Titlebar);
        end
end
flush(logid)
t = datenum(c);

function write_table_header(logid,tbl,Color)
fprintf(logid,'%s\n','\begin{longtable}{|p{0.3\linewidth}|p{0.05\linewidth}|p{0.05\linewidth}|p{0.3\linewidth}|p{0.2\linewidth}|}');
fprintf(logid,'%s\n','\hiderowcolors');
switch tbl
    case 'reading_failed_cases'
        fprintf(logid,'%s\n','\caption{Overview of all cases with errors during data reading} \label{Tab:ReadFailedSummary} \\');
    case 'script_failed_cases'
        fprintf(logid,'%s\n','\caption{Overview of all cases with errors during script evaluation} \label{Tab:ScriptFailedSummary} \\');
    case 'crashed_cases'
        fprintf(logid,'%s\n','\caption{Overview of all crashed test cases} \label{Tab:CrashedSummary} \\');
    otherwise
        fprintf(logid,'%s\n','\caption{Overview of all test cases included} \label{Tab:Summary} \\');
end
fprintf(logid,'%s\n','\showrowcolors');
fprintf(logid,'%s\n','\hline');
fprintf(logid,'%s%s%s\n','\rowcolor{',Color.Titlebar,'} \STRUT \textbf{Validation case} & \textbf{Read} & \textbf{Script} & \textbf{Error message} & \textbf{Timing} \\ [1ex] \hline');
fprintf(logid,'%s\n','\endfirsthead');
fprintf(logid,'%%\n');
fprintf(logid,'%s\n','\hiderowcolors');
fprintf(logid,'%s\n','\multicolumn{5}{c}{{\STRUT \tablename\ \thetable{} -- continued from previous page}} \\ [1ex] \hline');
fprintf(logid,'%s\n','\showrowcolors');
fprintf(logid,'%s%s%s\n','\rowcolor{',Color.Titlebar,'} \STRUT \textbf{Validation case} & \textbf{Read} & \textbf{Script} & \textbf{Error message} & \textbf{Timing} \\ [1ex] \hline');
fprintf(logid,'%s\n','\endhead');
fprintf(logid,'%%\n');
fprintf(logid,'%s\n','\hline');
fprintf(logid,'%s\n','\hiderowcolors');
fprintf(logid,'%s\n','\multicolumn{5}{r}{{\STRUT \tablename \thetable{} -- continued on next page}} \\');
fprintf(logid,'%s\n','\showrowcolors');
fprintf(logid,'%s\n','\endfoot');
fprintf(logid,'%%\n');
fprintf(logid,'%s\n','\endlastfoot');



function write_table_error(logid,extlog,Color,err)
msg = stack2str(err.stack);
if isempty(extlog)
    log_style('cmd')
end
switch log_style
    case 'cmd'
        fprintf('The test bench execution failed unexpectedly with the following message:\n');
        fprintf('%s\n',err.message,msg{:});
    case 'latex'
        message = protected(err.message);
        msg = protected(msg);
        fprintf(extlog.summary.fid,'The test bench execution failed unexpectedly with the following message: %s%s%s\\\\\n','\textcolor{',Color.Failed,'}{');
        fprintf(extlog.summary.fid,'%s \\\\\n',message,msg{:});
        fprintf(extlog.summary.fid,'%s\n','}');
    otherwise
        fprintf(logid,'<tr><td colspan=5><font color=%s><b>Test bench execution failed unexpectedly.</b><br>\n',Color.Failed);
        fprintf(logid,'%s<br>',err.message,msg{:});
        fprintf(logid,'</font></td></tr>\n');
end


function extlog = write_table1_line(logid,extlog,Color,CaseReadFailed,CaseScriptFailed,CaseCrashed,bgcolor,casename,frcolor,frresult,lgcolor,lgresult,logname,dt2_str)
switch log_style
    case 'latex'
        message = '';
        if isempty(lgcolor)
            colon = strfind(frresult,':');
            if ~isempty(colon)
                message = frresult(colon+2:end);
                frresult = frresult(1:colon-1); % FAILED
            end
            str = sprintf('\\STRUT \\nameref{Sec:%s} & \\textcolor{%s}{%s} &  & %s & %s \\\\\n',makelabel(casename),frcolor,frresult,message,dt2_str);
        else
            colon = strfind(lgresult,':');
            if ~isempty(colon)
                message = lgresult(colon+2:end); % Log file results differ.
                lgresult = lgresult(1:colon-1); % FAILED
            end
            colon = strfind(frresult,':');
            if ~isempty(colon)
                message = frresult(colon+2:end);
                frresult = frresult(1:colon-1); % FAILED
            end
            str = sprintf('\\STRUT \\nameref{Sec:%s} & \\textcolor{%s}{%s} & \\textcolor{%s}{%s} & %s & %s \\\\\n',makelabel(casename),frcolor,frresult,lgcolor,lgresult,message,dt2_str);
        end
        if extlog.all_cases.empty
            write_table_header(extlog.all_cases.fid,'all_cases',Color)
            extlog.all_cases.empty = false;
        end
        fprintf(extlog.all_cases.fid,'%s',str);
        CaseFailed = [CaseReadFailed, CaseScriptFailed, CaseCrashed];
        FailList = {'reading_failed_cases', 'script_failed_cases', 'crashed_cases'};
        for ifail = 1:3
            if CaseFailed(ifail)
                fcase = FailList{ifail};
                if extlog.(fcase).empty
                    write_table_header(extlog.(fcase).fid,fcase,Color)
                    extlog.(fcase).empty = false;
                end
                fprintf(extlog.(fcase).fid,'%s',str);
            end
        end
    otherwise
        fprintf(logid,'<tr bgcolor=%s><td>%s</td>',bgcolor,casename);
        if isempty(lgcolor)
            fprintf(logid,'<td colspan=2><font color=%s><b>%s</b></font></td>',frcolor,frresult);
        else
            fprintf(logid,'<td><font color=%s><b>%s</b></font></td><td><font color=%s><b>%s</b></font></td>',frcolor,frresult,lgcolor,lgresult);
        end
        fprintf(logid,'<td><a href="%s/%s">Click</a></td><td>%s</td></tr>\n',casename,logname,dt2_str);
end


function write_table2_line(logid,bgcolor,Name,Read,Compare,Message)
switch log_style
    case 'latex'
        % to set the color of a single row use '\rowcolor{...} '
        if ~ischar(Read)
            if isequal(Compare,1) % Compare FAILED ... Message contains "FAILED", actual message will be written by caller
                fprintf(logid,'%s & ',Message);
            elseif isequal(Compare,2) % finish the message
                fprintf(logid,'\\\\\n');
            else
                fprintf(logid,'%s & %s\\\\\n',Compare,Message);
            end
        elseif ~ischar(Compare)
            fprintf(logid,'\\STRUT %s & %s & ',protected(Name),Read);
        else
            fprintf(logid,'\\STRUT %s & %s & %s & %s\\\\\n',protected(Name),Read,Compare,Message);
        end
    otherwise
        if ~ischar(Read)
            if isequal(Compare,1) % Compare FAILED ... Message contains "FAILED", actual message will be written by caller
                fprintf(logid,'<td>%s</td><td>',Message);
            elseif isequal(Compare,2) % finish the message
                fprintf(logid,'</td></tr>\n');
            else
                fprintf(logid,'<td>%s</td><td>%s</td></tr>\n',Compare,Message);
            end
        elseif ~ischar(Compare)
            fprintf(logid,'<tr bgcolor=%s><td>%s</td><td>%s</td>',bgcolor,protected(Name),Read);
        else
            fprintf(logid,'<tr bgcolor=%s><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>\n',bgcolor,protected(Name),Read,Compare,Message);
        end
end


function write_rule(logid)
switch log_style
    case 'latex'
        fprintf(logid,'\n%% --- separator ---\n\n');
    otherwise
        fprintf(logid,'<hr>\n');
end


function write_log1(logid,message,varargin)
if nargin>2
    message = sprintf(message,varargin{:});
end
if ~iscell(message)
    message = {message};
end
switch log_style
    case 'latex'
        fprintf(logid,'%s',message{:});
    otherwise
        fprintf(logid,'%s',message{:});
end


function write_section(logid,message,varargin)
if nargin>2
    message = sprintf(message,varargin{:});
end
if ~iscell(message)
    message = {message};
end
switch log_style
    case 'latex'
        fprintf(logid,'\\section*{%s}\n\n',message{:});
    otherwise
        fprintf(logid,'%s<br>\n',message{:});
end


function echo_logfile(logid,logf)
C = getfile(logf); % syntax highlighting in TeXnicCenter may be off, but the symbols don't need protecting
switch log_style
    case 'latex'
        fprintf(logid,'\\begin{Verbatim}[frame=single, framesep=5pt]\n');
        fprintf(logid,'%s\n',C{:});
        fprintf(logid,'\\end{Verbatim}\n');
    otherwise
        fprintf(logid,'%s<br>\n',C{:});
end


function fileContent = getfile(filename)
fileContent = cell(100,1);
nC = 0;
fid = fopen(filename,'r','n','US-ASCII');
while 1
    str = fgetl(fid);
    if ischar(str)
        nC = nC+1;
        if nC>length(fileContent)
            fileContent{2*nC} = '';
        end
        fileContent{nC} = str;
    else
        break
    end
end
fileContent(nC+1:end) = [];
fclose(fid);


function write_log(logid,message,varargin)
if nargin>2
    message = sprintf(message,varargin{:});
end
if ~iscell(message)
    message = {message};
end
switch log_style
    case 'latex'
        fprintf(logid,'%s\\newline\n',message{:});
    otherwise
        fprintf(logid,'%s<br>\n',message{:});
end


function write_log_domain(logid,domain)
switch log_style
    case 'latex'
        fprintf(logid,'\n\\textbf{Domain ''%s''}\\newline\n',domain);
    otherwise
        fprintf(logid,'<b>Domain ''%s''</b><br>\n',domain);
end


function write_list(logid,list)
if ~iscell(list)
    list = {list};
end
switch log_style
    case 'latex'
        fprintf(logid,'\\begin{itemize}\n');
        fprintf(logid,'  \\item %s\n',list{:});
        fprintf(logid,'\\end{itemize}\n');
    otherwise
        fprintf(logid,'<ul>\n');
        fprintf(logid,'  <li>%s</li>\n',list{:});
        fprintf(logid,'</ul>\n');
end


function str = color_write(str,color,bold)
if nargin<3
    bold = false;
end
if iscellstr(str)
    str = str(:)';
    switch log_style
        case 'latex'
            str(2,:) = {'\newline{}'};
        otherwise
            str(2,:) = {'<br>'};
    end
    str{2,end} = '';
    str = [str{:}];
end
switch log_style
    case 'latex'
        if bold
            str = sprintf('\\textcolor{%s}{\\textbf{%s}}',color,str);
        else
            str = sprintf('\\textcolor{%s}{%s}',color,str);
        end
    otherwise
        if bold
            str = sprintf('<font color=%s><b>%s</b></font>',color,str);
        else
            str = sprintf('<font color=%s>%s</font>',color,str);
        end
end

function include_figure(logid,checkf)
switch log_style
    case 'latex'
        fprintf(logid,'\\includegraphics*[width=50mm]{%s}\n\n',protect_filename(checkf));
    otherwise
        fprintf(logid,'<img src=''%s''><br>\n',checkf);
end


function include_diff_figures(logid,files,Color)
nfiles = length(files);
switch log_style
    case 'latex'
        fprintf(logid,'%s%s%s\n','\begin{tabular}{',repmat('l',1,nfiles),'}');
        fprintf(logid,'%s\n','\hiderowcolors');
        for i = 1:nfiles-1
            fprintf(logid,'%s%s%s','\textbf{',protected(files{i}),'} & ');
        end
        fprintf(logid,'%s%s%s\n','\textbf{',protected(files{end}),'} \\');
        %
        for i = 1:nfiles-1
            fprintf(logid,'%s%s%s','\includegraphics*[width=50mm]{',protect_filename(files{i}),'} & ');
        end
        fprintf(logid,'%s%s%s\n','\includegraphics*[width=50mm]{',protect_filename(files{end}),'} \\');
        fprintf(logid,'%s\n','\showrowcolors');
        fprintf(logid,'%s\n','\end{tabular}\newline');
    otherwise
        fprintf(logid,'<table bgcolor=%s>\n',Color.Table{1});
        fprintf(logid,['<tr>' repmat(['<td width=300 bgcolor=',Color.Titlebar,'>%s</td>'],1,nfiles) '</tr>\n'],files{:});
        fprintf(logid,['<tr>',repmat('<td><img src=''%s''></td>',1,nfiles),'</tr>\n'],files{:});
        fprintf(logid,'</table><br>\n');
end


function write_table_rule(logid,Color)
switch log_style
    case 'latex'
        fprintf(logid,'\\hline\n');
    otherwise
        fprintf(logid,'<tr><td colspan=4 bgcolor=%s></td></tr>\n',Color.Titlebar);
end


function write_begin_table(logid,Color)
switch log_style
    case 'latex'
        fprintf(logid,'%s\n','\begin{longtable}{|p{0.4\linewidth}|p{0.05\linewidth}|p{0.1\linewidth}|p{0.4\linewidth}|}');
        %fprintf(logid,'%s\n','\hiderowcolors');
        %fprintf(logid,'%s\n','\caption{No caption} \\');
        %fprintf(logid,'%s\n','\showrowcolors');
        fprintf(logid,'%s\n','\hline');
        fprintf(logid,'%s%s%s\n','\rowcolor{',Color.Titlebar,'} \STRUT \textbf{Data field} & \textbf{Read} & \textbf{Compare} & \textbf{Error message}\\ [1ex] \hline');
        fprintf(logid,'%s\n','\endfirsthead');
        fprintf(logid,'%%\n');
        fprintf(logid,'%s\n','\hiderowcolors');
        fprintf(logid,'%s\n','\multicolumn{3}{c}{{\STRUT \tablename\ \thetable{} -- continued from previous page}} \\ [1ex] \hline');
        fprintf(logid,'%s\n','\showrowcolors');
        fprintf(logid,'%s%s%s\n','\rowcolor{',Color.Titlebar,'} \STRUT \textbf{Data field} & \textbf{Read} & \textbf{Compare} & \textbf{Error message} \\ [1ex] \hline');
        fprintf(logid,'%s\n','\endhead');
        fprintf(logid,'%%\n');
        fprintf(logid,'%s\n','\hline');
        fprintf(logid,'%s\n','\hiderowcolors');
        fprintf(logid,'%s\n','\multicolumn{3}{r}{{\STRUT \tablename \thetable{} -- continued on next page}} \\');
        fprintf(logid,'%s\n','\showrowcolors');
        fprintf(logid,'%s\n','\endfoot');
        fprintf(logid,'%%\n');
        fprintf(logid,'%s\n','\endlastfoot');
    otherwise
        fprintf(logid,'<table bgcolor=%s><tr><td bgcolor=%s><b>Data field</b></td><td bgcolor=%s><b>Read</b></td><td bgcolor=%s><b>Compare</b></td><td bgcolor=%s><b>Error message</b></td></tr>\n', ...
            Color.Table{1},Color.Titlebar,Color.Titlebar,Color.Titlebar,Color.Titlebar);
end


function write_summary(logid1,extlog,NFailed,NReadFailed,NScriptFailed,NCrashed,NTested,NSlower,TotTimeRef,TotTime)
switch log_style
    case 'latex'
        sumid = extlog.summary.fid;
        fprintf(sumid,'This document reports on the regression testing of QUICKPLOT.\n');
        fprintf(sumid,'Each test consists of the following steps:.\n');

    	fprintf(sumid,'\\begin{itemize}\n');
    	fprintf(sumid,'\\item Opening the data file.\n');
    	fprintf(sumid,'Read failure is reported as an error.\n');
    	fprintf(sumid,'\\item Determining the quantities contained in the file, and compare this against the reference content.\n');
    	fprintf(sumid,'Missing quantities and changed meta data are reported as errors.\n');
    	fprintf(sumid,'\\item For every quantity contained in the file: read the data of the final time step, and compared with reference data.\n');
    	fprintf(sumid,'Changed data or meta data is reported as an error.\n');
    	fprintf(sumid,'\\end{itemize}\n');
    	fprintf(sumid,'All steps above are summarized in the column "Read".\n');
    	fprintf(sumid,'While this first step guarantees that most of the reading works fine, it doesn''t check whether the plotting or exporting works properly.\n');
    	fprintf(sumid,'Therefore the testing continues ...\n\n');

    	fprintf(sumid,'\\begin{itemize}\n');
    	fprintf(sumid,'\\item For most files a script is executed that triggers various variable selection, plotting, and data export options.\n');
    	fprintf(sumid,'\\end{itemize}\n');
    	fprintf(sumid,'All execution errors and changes in the created (data or figure) files will be reported as errors (missing data files are currently not reported as error, but are typically the result of a reported execution error).\n');
    	fprintf(sumid,'See the report per file for a listing of the script executed and the resulting data tested.\n');
    	fprintf(sumid,'The result of this step is reported in the column "Script".\n');
    	fprintf(sumid,'The graphics part of this second step is more sensitive to hardware changes.\n');
    	fprintf(sumid,'A change in screen resolution may, for instance, trigger slight changes in the figures created and hence many tests to fail; therefore, consistent testing hardware is required for this step.\n\n');

        fprintf(sumid,'The reported timing results compare the timing of the new run against the timing of a reference run.\n');
        fprintf(sumid,'The reference timing is not updated, so the overall performance does not necessarily improve with each "faster than before" release.\n');
        fprintf(sumid,'Depending on the other (background) processes running the timing of tests may vary substantially: repeating the same test will result in slightly different timing results (some tests will become faster while other tests will become slower) -- so we typically don''t look at the individual numbers.\n');
        fprintf(sumid,'This also means that you won''t be able to match the reported performance under normal working circumstances with many other programs opened.\n\n');

        if NFailed==0
            fprintf(sumid,'The software was successfully run on %d cases; none of them failed.\n',NTested);
        elseif NFailed==1
            if NTested==1 && NCrashed==1
                fprintf(sumid,'The software was run on 1 case that crashed.\n');
            elseif NTested==1
                fprintf(sumid,'The software was run on 1 case that failed.\n');
            elseif NCrashed==1
                fprintf(sumid,'The software was run on %d cases of which 1 case crashed.\n',NTested);
            else
                fprintf(sumid,'The software was run on %d cases of which 1 case failed.\n',NTested);
            end
            if NReadFailed==1
                fprintf(sumid,'The failed case is shown in Table \\ref{Tab:ReadFailedSummary}.\n');
            elseif NScriptFailed==1
                fprintf(sumid,'The failed case is shown in Table \\ref{Tab:ScriptFailedSummary}.\n');
            else
                fprintf(sumid,'The crashed case is shown in Table \\ref{Tab:CrashedSummary}.\n');
            end
        else
            if NTested==NCrashed
                fprintf(sumid,'The software was run on %d cases that all crashed.\n',NTested);
            elseif NTested==NFailed && NCrashed==0
                fprintf(sumid,'The software was run on %d cases that all failed.\n',NTested);
            elseif NTested==NFailed
                fprintf(sumid,'The software was run on %d cases that all failed (%d crashed).\n',NTested,NCrashed);
            elseif NCrashed==NFailed
                if NFailed==1
                    fprintf(sumid,'The software was run on %d cases of which 1 case crashed.\n',NTested);
                else
                    fprintf(sumid,'The software was run on %d cases of which %d cases crashed.\n',NTested,NFailed);
                end
            elseif NCrashed==0
                if NFailed==1
                    fprintf(sumid,'The software was run on %d cases of which 1 case failed.\n',NTested);
                else
                    fprintf(sumid,'The software was run on %d cases of which %d cases failed.\n',NTested,NFailed);
                end
            else
                fprintf(sumid,'The software was run on %d cases of which %d cases failed (%d crashed).\n',NTested,NFailed,NCrashed);
            end
            if NCrashed==NFailed
                if NCrashed==1
                    fprintf(sumid,'The crashed case is reported in Table \\ref{Tab:CrashedSummary}.\n');
                else
                    fprintf(sumid,'The crashed cases are reported in Table \\ref{Tab:CrashedSummary}.\n');
                end
            elseif NCrashed>0
                if NCrashed==1
                    fprintf(sumid,'The crashed case is reported in Table \\ref{Tab:CrashedSummary}.\n');
                else
                    fprintf(sumid,'The %i crashed cases are reported in Table \\ref{Tab:CrashedSummary}.\n',NCrashed);
                end
            end
            if NReadFailed==0
                if NCrashed == 0
                    fprintf(sumid,'No errors were detected during the reading of the data.\n');
                end
            elseif NReadFailed==1
                fprintf(sumid,'One case failed during the reading of the data; it is reported in Table \\ref{Tab:ReadFailedSummary}.\n');
            else
                fprintf(sumid,'%i cases failed during the reading of the data; they are reported in Table \\ref{Tab:ReadFailedSummary}.\n',NReadFailed);
            end
            if NScriptFailed==0
                if NCrashed == 0
                    fprintf(sumid,'No errors were detected while running the data plotting and exporting scripts.\n');
                end
            elseif NScriptFailed==1
                fprintf(sumid,'One case failed while running the data plotting and exporting scripts; it is reported in Table \\ref{Tab:ScriptFailedSummary}.\n');
            else
                fprintf(sumid,'%i cases failed while running the data plotting and exporting scripts; they are reported in Table \\ref{Tab:ScriptFailedSummary}.\n',NScriptFailed);
            end
        end
        %
        if NTested>0
            fprintf(sumid,'All tested cases are reported in Table \\ref{Tab:Summary}.\n');
        else
            fprintf(sumid,'Nothing to report.\n');
        end
        %
        dTTR = duration(TotTimeRef);
        dTT  = duration(TotTime);
        if abs(TotTime-TotTimeRef)<0.005*TotTimeRef
            fprintf(sumid,'The overall time spent on the test cases (%s) is similar to the reference time (%s);',dTT,dTTR);
        elseif TotTime>TotTimeRef
            fprintf(sumid,'The overall time spent on the test cases has increased by about %.0f \\%% from %s to %s;',(TotTime/TotTimeRef-1)*100,dTTR,dTT);
        else
            fprintf(sumid,'The overall time spent on the test cases has decreased by about %.0f \\%% from %s to %s;',(1-TotTime/TotTimeRef)*100,dTTR,dTT);
        end
        if NSlower==NTested
            fprintf(sumid,' all test cases had slower timing than before.\n');
        elseif NSlower>0
            fprintf(sumid,' %i test cases were slower than before, the other %i test cases had similar or better timing than before.\n',NSlower,NTested-NSlower);
        else
            fprintf(sumid,' all test cases had similar or better timing than before.\n');
        end
        %
        for fName = extlog.files
            if ~extlog.(fName{1}).empty
                write_end_table(extlog.(fName{1}).fid)
            end
        end
    otherwise
        write_end_table(logid1);
end


function write_end_table(logid,emptyTable)
if nargin<2
    emptyTable = false;
end
switch log_style
    case 'latex'
        if emptyTable
            fprintf(logid,'%s\n','\STRUT & & \\');
        end
        fprintf(logid,'%s\n','[1ex] \hline');
        fprintf(logid,'%s\n','\end{longtable}');
    otherwise
        fprintf(logid,'</table><br>\n');
end
fprintf(logid,'\n');


function write_includes(logid,includes)
switch log_style
    case 'latex'
        for i = 1:size(includes,1)
            if ~isempty(includes{i,1})
                fprintf(logid,'\\graphicspath{{"%s/"}}\n',includes{i,1});
                fprintf(logid,'\\input{"%s%s"}\n',includes{i,1},includes{i,2});
            end
        end
    otherwise
        % no includes needed
end

function [dt,dt_str,slower] = write_footer(logid,casename,Color,t0,dt_old)
c      = clock;
dt     = (datenum(c)-t0)*86400;
dt_str = duration(dt);
slower = false;
if isnan(dt_old) || abs(dt-dt_old)<max(0.2,min(dt,dt_old)/30)
    color  = Color.Font;
elseif dt>dt_old
    color  = Color.Failed;
    slower = true;
else
    color  = Color.Success;
end
switch log_style
    case 'latex'
        if strcmp(casename,'MAIN')
            % main document
            fprintf(logid,'\n');
            fprintf(logid,'%% %s\n','\newpage');
            fprintf(logid,'%% %s\n','\pagestyle{empty}');
            fprintf(logid,'%% %s\n','\mbox{}');
            fprintf(logid,'%% %s\n','\includepdf[pages=2,offset=-72 -70]{pictures/delft3d-cover_quickplot.pdf} % links-rechts past precies');
            fprintf(logid,'%s\n','\end{document}');
        else
            write_section(logid,'Timing');
            if ~isequal(color,Color.Font)
                dt_str = ['\textcolor{',color,'}{',dt_str,'} was: ',duration(dt_old)];
            end
            fprintf(logid,'Duration: %s\n',dt_str);
        end
    otherwise
        fprintf(logid,'<table bgcolor=%s><tr><td colspan=2 bgcolor=%s><b>End of validation report</b></td></tr>\n', ...
            Color.Table{1},Color.Titlebar);
        fprintf(logid,'<tr><td>Date:</td><td>%4.4i-%2.2i-%2.2i %2.2i:%2.2i:%02.0f</td></tr>\n',c);
        if ~isequal(color,Color.Font)
            dt_str = ['<font color=',color,'>',dt_str,'</font> was: ',duration(dt_old)];
        end
        fprintf(logid,'<tr><td>Duration:</td><td>%s</td></tr>\n',dt_str);
        fprintf(logid,'</table>\n');
        fprintf(logid,'</font></body>');
end
flush(logid)


function s = duration(dt)
if dt>60
    mdt = floor(dt/60);
    sdt = dt-60*mdt;
    s = sprintf('%im %.1fs',mdt,sdt);
else
    s = sprintf('%.1fs',dt);
end


function X=localexist(file)
%X=exist(file);
X=fopen(file,'r');
if X>0, fclose(X); end
X=X>0;


function flush(logid1)
fseek(logid1,0,-1);
fseek(logid1,0,1);


function ensure_directory(dirname)
if isempty(dir(dirname))
    [parent, thisdir, ext] = fileparts(dirname);
    thisdir = [thisdir ext];
    if ~isempty(parent)
        ensure_directory(parent);
        cd(parent)
    end
    c = computer;
    if strcmp(c(1:2),'PC')
        s = dos(['mkdir "',thisdir,'"']);
    else
        s = unix(['mkdir -p ',thisdir]);
    end
end


function localsave(filename,Data,saveops)
if isstandalone
    save(filename,'Data');
else
    save(filename,'Data',saveops{:});
end


function Data = localload(filename)
if isstandalone
    Data = load(filename);
else
    Data = load(filename,'-mat');
end


function xtype = log_style(type)
persistent current_type
if nargin>0
    current_type = type;
else
    xtype = current_type;
end

function type = sametype(casename)
persistent previous_type
sep = strfind(casename,' - ');
if isempty(sep)
    type = casename;
else
    type = casename(1:sep(1)-1);
end
if isequal(type,previous_type)
    type = '';
else
    previous_type = type;
end
