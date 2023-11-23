function tick(varargin)
%TICK Create ticks and ticklabels.
%   TICK(AXES,AXIS,TICKS,FORMAT,SCALING) changes the tickmarks of the
%   specified axis (a string containing the characters x, y and z) of the
%   specified axes object (defaults to the current axes if not specified).
%   The new tickmarks will be at the specified TICKS locations (defaults to
%   the current tickmark locations if 'current' is specified, or if
%   argument is not specified) and the tickmarklabels are formatted
%   according the FORMAT string (required argument) after multiplication by
%   the SCALING factor (defaults to 1 if not specified).
%
%   Use TICKS = 'autoticks' to automatically determine the tick locations
%   rather than using the current locations (TICKS = 'current') or 
%   specificied locations (TICKS = a vector).
%
%   The FORMAT string can be any valid FPRINTF expression containing one
%   "%-field" for the tickmark value. Other options for the FORMAT string
%   are:
%
%     * 'none' : no tickmarks (ignoring TICKS), no labels
%
%     * ''     : TICKS as specified, no labels
%
%     * 'auto' : automatic tickmarks (ignoring TICKS),
%                automatic tickmarklabels
%
%     * 'autolabel' :
%                TICKS as specified, automatic tickmark
%                formatting: '%g'
%
%     * 'degree' :
%                tickmarklabels formatted as degrees, minutes and
%                seconds
%
%     * 'longitude','latitude' :
%                same as degree with a direction character N,S,W,E
%                instead of a sign.
%
%     * 'autodate' :
%                date format automatically selected
%
%     * 'date' : date formatted ticks; requires another format
%                string:
%
%                TICK(AXES,AXIS,TICKS,'date',DATEFORMAT,SCALING)
%
%                where DATEFORMAT may contain any of the following
%                conversion characters:
%                      %N MATLAB date number,
%                      %A absolute year,
%                      %AD or %BC for automatic AD/BC indicator,
%                      %Y year,
%                      %y last two digits of the year,
%                      %Q number of quarter,
%                      %M number of month,
%                      %P number of (4 week) period within year,
%                      %W number of week,
%                      %n number of day within year,
%                      %D number of day within month,
%                      %w weekday name,
%                      %H hour based on 24 hours clock,
%                      %h hour based on 12 hours clock,
%                      %am or %pm for automatic am/pm indicator,
%                      %AM or %PM for automatic AM/PM indicator,
%                      %m minute,
%                      %s second
%                force number of characters used using standard
%                conversion specification modifiers, e.g. %2m for
%                minutes always indicated using 2 characters and
%                %2.2m to include a leading zero to fill up to two
%                characters. All numbers behave as integers (i.e.
%                identical to %i) except seconds which behaves like
%                a floating point (%f). Use %05.2s for seconds and
%                hundreth of a second with leading zero if the´
%                number of seconds is smaller than 10. Use %.0s for
%                only seconds (no fractional part). The weekday
%                specification supports %w, %1w, %2w and %3w. The
%                month specification supports %O, %1O, %3O.
%
%   TICK(...,'optionname',optionval,...)
%   The following options are supported:
%     * DecSep   Decimal separator. The value should be a single
%                character (default: .)
%     * Language Select the language used for names of months and
%                days: English, Dutch, German, French, Italian, or
%                Spanish (default: English)
%     * SkipNLabels
%                Specifies the number of ticks without labels in between
%                ticks with labels: integer (default: 0). The default
%                settings generates a label for every tick. If you specify
%                N then non-blank labels will be generated for ticks
%                1:(N+1):end.
%     * FirstLabel
%                Special format specification for the first tick.
%
%   Examples:
%     ax=subplot(2,2,1);
%     set(ax,'xlim',[1 32],'xscale','log')
%     tick('x',[1:3 5 7 10 15 22 32],'%2.2X')
%     tick(ax,'y','%.1f')
%
%     ax=subplot(2,2,2);
%     set(ax,'xlim',[0 10000],'ylim',[0 10000]);
%     tick('xy','%.2f km',0.001,'decsep',',')
%
%     ax=subplot(2,2,3); view(3)
%     tick(0:.25:1,'%5.2f')
%
%     ax=subplot(2,2,4);
%     set(ax,'xlim',now+[0 7]);
%     tick(gca,'x','date','%2w %D','firstlabel','%D %3O %Y')

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

DecSep='.';
Language='English';
SkipNLabels=0;
FirstLabel=0;

INP=varargin;
i=1;
while i<length(INP)
    if ischar(INP{i})
        switch lower(INP{i})
            case 'decsep'
                DecSep=INP{i+1};
                INP([i i+1])=[];
            case 'language'
                Language=INP{i+1};
                INP([i i+1])=[];
            case 'skipnlabels'
                SkipNLabels=INP{i+1};
                INP([i i+1])=[];
            case 'firstlabel'
                FirstLabel=INP{i+1};
                INP([i i+1])=[];
            otherwise
                i=i+1;
        end
    else
        i=i+1;
    end
end
NArgs=length(INP);

i=1;
if (NArgs>=i)
    if ~ischar(INP{i}) && isequal(size(INP{i}),[1 1]) && ...
          ishandle(INP{i}) && strcmp(get(INP{i},'type'),'axes')
        handle=INP{i};
        i=i+1;
    else
        handle=gca;
    end
else
    handle=gca;
end

ax='xyz';
if (NArgs>=i)
    if ischar(INP{i}) && ...
          ~isempty(strmatch(sort(lower(INP{i})),{'xyz','yz','xz','z'}))
        ax=sort(lower(INP{i}));
        i=i+1;
    end
end

tckmode='auto';
if (NArgs>=i)
    if ~ischar(INP{i})
        tckmode='spec';
        tck=INP{i};
        i=i+1;
    elseif strcmp(INP{i},'current')
        %tckmode='auto'
        i=i+1;
    elseif strcmp(INP{i},'autoticks')
        tckmode='autoticks';
        i=i+1;
    end
end

if (NArgs<i) || ~ischar(INP{i})
    error('No FORMAT string specified.')
end

frmt=INP{i};
i=i+1;
datefrmt=strcmpi(frmt,'date');
if datefrmt
    if (NArgs<i) || ~ischar(INP{i})
        error('No date format specified.')
    else
        frmt=['date:',INP{i}];
        i=i+1;
    end
else
    datefrmt=strcmpi(frmt,'autodate');
end

scaling=1;
if NArgs>=i
    if isnumeric(INP{i}) && isequal(size(INP{i}),[1 1])
        scaling=INP{i};
        i=i+1;
        if datefrmt
            warning('Scaling factor ignored for date axes.')
        end
    else
        error('Invalid scaling factor: argument %i.',i)
    end
end

if (NArgs==i)
    warning('Argument %i ignored.',i)
elseif (NArgs>i)
    warning('Arguments %i - %i ignored.',i,NArgs)
end

for i=1:length(ax)
    if strcmp(tckmode,'auto') || strcmp(tckmode,'autoticks')
        if datefrmt
            if strcmp(get(handle,[ax(i) 'tickmode']),'auto') || strcmp(tckmode,'autoticks')
                if strcmp(frmt,'autodate')
                    [tck,frmt]=Local_datetick(handle,ax(i));
                else
                    tck=Local_datetick(handle,ax(i));
                end
            else
                tck=get(handle,[ax(i) 'tick']);
            end
        else
            if strcmp(tckmode,'autoticks')
                set(handle,[ax(i) 'tickmode'],'auto')
            end
            tck=get(handle,[ax(i) 'tick']);
        end
    end
    if strcmp(frmt,'autodate')
        [dummytck,frmt]=Local_datetick(handle,ax(i));
    end
    Local_tick(handle,ax(i),tck,frmt,scaling,DecSep,Language,SkipNLabels,FirstLabel)
end


function Local_tick(handle,ax,tck,Frmt,scaling,DecSep,Language,SkipNLabels,FirstLabel)
frmt=lower(Frmt);
switch frmt
    case 'none'
        set(handle,[ax 'tick'],[],[ax 'ticklabel'],[]);
        return
    case 'auto'
        set(handle,[ax 'tickmode'],'auto',[ax 'ticklabelmode'],'auto');
    case 'autolabel'
        set(handle,[ax 'tick'],tck,[ax 'ticklabelmode'],'auto');
    case {'degree','longitude','latitude'}
        tckl=tck;
        tck=scaling*tck(:)';
        tckL = degstr(tck,frmt,'cell');
        set(handle,[ax,'tick'],tckl,[ax,'ticklabel'],tckL)
    otherwise
        if strncmp(frmt,'date:',5)
            % Set axis tick labels
            labels = Local_datestr(tck,Frmt(6:end),Language,FirstLabel);
            set(handle,[ax,'tick'],tck,[ax,'ticklabel'],labels)
        else
            tckL=cell(1,length(tck));
            if ~strcmp(DecSep,'.')
                if ischar(FirstLabel)
                    Frmts = {Frmt FirstLabel};
                else
                    Frmts = {Frmt};
                end
                for l = 1:length(Frmts)
                    Frmt = Frmts{l};
                    Frmt=strrep(Frmt,'.','..?'); % This may also change %3.4f into %3..?4f, which is invalid
                    % Why the question mark in the above expression?
                    % Answer: to make the new string separable for otherwise .. would result in ....
                    %         and strfind('....','..') returns [1 2 3]
                    iperc=strfind(Frmt,'%');
                    iperc=setdiff(setdiff(iperc,iperc+1),iperc-1); % double %% means just %
                    for i=fliplr(iperc) % I expect just one, but let's generalize
                        k=i+1;
                        while k<length(Frmt) && ismember(Frmt(k),'-0123456789')
                            k=k+1;
                        end
                        if k<length(Frmt) && isequal(Frmt(k),'.')
                            Frmt(k+1:k+2)=[]; % remove following .?
                        end
                    end
                    %
                    Frmts{l} = Frmt;
                end
                if ischar(FirstLabel)
                    FirstLabel = Frmts{2};
                end
                Frmt = Frmts{1};
            end
            for i=1:length(tck)
                if i==1 && ischar(FirstLabel)
                    tckL{i}=sprintf(FirstLabel,tck(i)*scaling);
                else
                    tckL{i}=sprintf(Frmt,tck(i)*scaling);
                end
                if ~strcmp(DecSep,'.')
                    k=strfind(tckL{i},'..?');
                    tckL{i}=strrep(tckL{i},'.',DecSep);
                    tckL{i}(k)='.';
                    tckL{i}([k+1 k+2])=[];
                end
            end
            set(handle,[ax 'tick'],tck,[ax 'ticklabel'],tckL);
        end
end
if strcmp(get(handle,[ax 'ticklabelmode']),'manual')
    tckL = get(handle,[ax 'ticklabel']);
    if ischar(tckL)
        tckL = cellstr(tckL);
    end
    noLabel = true(1,length(tckL));
    noLabel(1:(SkipNLabels+1):end) = false;
    tckL(noLabel)={''};
    set(handle,[ax 'ticklabel'],tckL)
end


function Strs = Local_datestr(ticks,frmt,Language,FirstLabel)
ticks=ticks(:);

perc=strfind(frmt,'%');
i=1;
while i<length(perc)
    if perc(i+1)==perc(i)
        perc(i+1)=[];
    end
    i=i+1;
end
Frmt=repmat(' ',size(frmt));
Len=zeros(size(frmt));

switch lower(Language)
    case 'english'
        mths={'January','February','March','April','May','June','July','August','September','October','November','December'};
        week={'Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'};
    case 'dutch'
        mths={'januari','februari','maart','april','mei','juni','juli','augustus','september','oktober','november','december'};
        week={'maandag','dinsdag','woensdag','donderdag','vrijdag','zaterdag','zondag'};
    case 'german'
        mths={'Januar','Februar','März','April','Mai','Juni','Juli','August','September','Oktober','November','Dezember'};
        week={'Montag','Dienstag','Mittwoch','Donnerstag','Freitag','Samstag','Sonntag'};
    case 'french'
        mths={'janvier','février','mars','avril','mai','juin','juillet','août','septembre','octobre','novembre','décembre'};
        week={'lundi','mardi','mercredi','jeudi','vendredi','samedi','dimanche'};
    case 'italian'
        mths={'gennaio','febbraio','marzo','aprile','maggio','giugno','luglio','agosto','settembre','ottobre','novembre','dicembre'};
        week={'lunedì','martedì','mercoledì','giovedì','venerdì','sabato','domenica'};
    case 'spanish'
        mths={'enero','febrero','marzo','abril','mayo','junio','julio','agosto','septiembre','octubre','noviembre','diciembre'};
        week={'lunes','martes','miércoles','jueves','viernes','sábado','domingo'};
end

qrtr = [ 1 1 1 2 2 2 3 3 3 4 4 4];
mths=char(mths)';
mths(mths==32)=2;
week=char(week)';
week(week==32)=2;

for i=length(perc):-1:1
    n1=perc(i);
    frm=sscanf(frmt(n1:end),'%1[%]%[0-9]%1[.]%[0-9]');
    dfrm=frmt(n1+length(frm));
    switch dfrm
        case {'a','p','A','P','B'}
            if length(frmt)>n1+length(frm)
                dfrm(1,2)=frmt(n1+length(frm)+1);
                switch dfrm
                    case {'am','pm','AM','PM','AD','BC'}
                    otherwise
                        dfrm=dfrm(1);
                end
            end
    end
    n2=n1+length(frm)+length(dfrm);
    switch dfrm
        case {'am','pm'}
            frmt=cat(2,frmt(1:(n1-1)),'%cm',frmt(n2:end));
            Frmt(n1)='p';
            Len(n1)=1;
        case {'AM','PM'}
            frmt=cat(2,frmt(1:(n1-1)),'%cM',frmt(n2:end));
            Frmt(n1)='a';
            Len(n1)=1;
        case {'AD','BC'}
            frmt=cat(2,frmt(1:(n1-1)),'%c%c',frmt(n2:end));
            Frmt(n1)='B';
            Len(n1)=2;
        case {'s'}
            frmt=cat(2,frmt(1:(n1-1)),frm,'f',frmt(n2:end));
            Frmt(n1)=dfrm;
            Len(n1)=1;
        case {'O'}
            if strcmp(frm,'%1')
                frmt=cat(2,frmt(1:(n1-1)),'%c',frmt(n2:end));
                Len(n1)=1;
            elseif strcmp(frm,'%3')
                frmt=cat(2,frmt(1:(n1-1)),'%c%c%c',frmt(n2:end));
                Len(n1)=3;
            elseif strcmp(frm,'%')
                n=size(mths,1);
                frmt=cat(2,frmt(1:(n1-1)),repmat('%c',[1 n]),frmt(n2:end));
                Len(n1)=n;
            else
                error('Month string only supports: %%1O and %%3O.')
            end
            Frmt(n1)=dfrm;
        case {'w'}
            if strcmp(frm,'%1')
                frmt=cat(2,frmt(1:(n1-1)),'%c',frmt(n2:end));
                Len(n1)=1;
            elseif strcmp(frm,'%2')
                frmt=cat(2,frmt(1:(n1-1)),'%c%c',frmt(n2:end));
                Len(n1)=2;
            elseif strcmp(frm,'%3')
                frmt=cat(2,frmt(1:(n1-1)),'%c%c%c',frmt(n2:end));
                Len(n1)=3;
            elseif strcmp(frm,'%')
                n=size(week,1);
                frmt=cat(2,frmt(1:(n1-1)),repmat('%c',[1 n]),frmt(n2:end));
                Len(n1)=n;
            else
                error('Weekday string only supports: %%1w, %%2w and %%3w.')
            end
            Frmt(n1)=dfrm;
        case {'A','Y','y','M','P','W','D','H','h','m','Q','N','n'}
            frmt=cat(2,frmt(1:(n1-1)),frm,'i',frmt(n2:end));
            Frmt(n1)=dfrm;
            Len(n1)=1;
        otherwise
            error(['Invalid date format: ''',frm,dfrm,'''.'])
    end
end;
Frmt(Frmt==' ')=[]; % remove blanks
Len(Len==0)=[];     % remove zeros

if isempty(Frmt)
    [Strs{1:length(ticks)}]=deal(frmt);
    if nargin>3 && ischar(FirstLabel)
        Strs(1) = Local_datestr(ticks(1),FirstLabel,Language);
    end
    return
end

pm = 'ap';
bc = ['BA';'CD'];
LenTot=sum(Len);
Offset=[0 cumsum(Len)];

Ticks = zeros(LenTot,length(ticks));
dvticks=datevec(ticks);
%
% Out of range values for day 0, i.e. Dec 31st of the year -1
%
dayzero=all(dvticks(:,1:3)==0,2);
if any(dayzero)
    dvticks(dayzero,1:3)=repmat([-1 12 31],sum(dayzero),1);
end

% ----- compute day of week number ------------------------------------
w = rem(fix(ticks)-2,7);   % Returns day of week number (Mon=1, Sun=7)
j = w<=0;
w(j) = w(j)+7;           % Add 7 to -d values for proper indexing in week
% ----- compute day of week number ------------------------------------

% ----- compute day number --------------------------------------------
y = dvticks(:,1)';
dn0 = 365*y + ceil(y/4)-ceil(y/100)+ceil(y/400) + 1;
dn1 = floor(ticks');
dn = dn1 - dn0 + 1;
% ----- compute day number --------------------------------------------

% ----- compute week number -------------------------------------------
% Determine day of week number for first day of year (Mon=1, Sun=7)
w0 = rem(dn0-2,7);
j = w0<=0; w0(j) = w0(j)+7; % Add 7 to -d values for proper indexing in week

% Shift the first days of the year to the previous year
yw=y;
j = dn<=mod(8-w0,7);
yw(j) = yw(j)-1;
dn0w = 365*yw + ceil(yw/4)-ceil(yw/100)+ceil(yw/400) + 1;
dnw = dn1 - dn0w + 1;

% Determine day of week number for first day of corrected year (Mon=1, Sun=7)
w0 = rem(dn0w-2,7);
j = w0<=0; w0(j) = w0(j)+7; % Add 7 to -d values for proper indexing in week

% Determine week number
wk=floor((dnw-mod(8-w0,7)-0.95)/7)+1;
% ----- compute week number -------------------------------------------

per=min(12,floor((wk-1)/4)+1);

for i=1:length(Frmt)
    switch Frmt(i)
        case 'Y'
            Ticks(Offset(i)+1,:)=y;
        case 'A'
            Ticks(Offset(i)+1,:)=abs(y);
        case 'B'
            Ticks(Offset(i)+(1:2),:)=bc(:,1+(y>=0));
        case 'y'
            Ticks(Offset(i)+1,:)=mod(abs(y),100);
        case 'Q'
            Ticks(Offset(i)+1,:)=qrtr(dvticks(:,2));
        case 'P'
            Ticks(Offset(i)+1,:)=per;
        case 'O'
            Ticks(Offset(i)+(1:Len(i)),:)=mths(1:Len(i),dvticks(:,2));
        case 'M'
            Ticks(Offset(i)+1,:)=dvticks(:,2)';
        case 'W'
            Ticks(Offset(i)+1,:)=wk;
        case 'w'
            Ticks(Offset(i)+(1:Len(i)),:)=week(1:Len(i),w);
        case 'D'
            Ticks(Offset(i)+1,:)=dvticks(:,3)';
        case 'N'
            Ticks(Offset(i)+1,:)=fix(ticks)';
        case 'n'
            Ticks(Offset(i)+1,:)=dn;
        case 'H'
            Ticks(Offset(i)+1,:)=dvticks(:,4)';
        case 'h'
            Ticks(Offset(i)+1,:)=mod(dvticks(:,4)-1,12)'+1;
        case 'm'
            Ticks(Offset(i)+1,:)=dvticks(:,5)';
        case 's' % second
            Ticks(Offset(i)+1,:)=dvticks(:,6)';
            %Ticks(Offset(i)+1,:)=round(dvticks(:,6))';
        case 'u' % hundredth of a second
            Ticks(Offset(i)+1,:)=round(rem(dvticks(:,6),1)*100)';
        case 'a'
            Ticks(Offset(i)+1,:)=upper(pm((dvticks(:,4)>11)+1));
        case 'p'
            Ticks(Offset(i)+1,:)=pm((dvticks(:,4)>11)+1);
    end
end

frmt(end+1)=char(1);

TmpStr=sprintf(frmt,Ticks);
TmpStr(TmpStr==2)=[];

TickSep=find(TmpStr==1);
Start=[1 TickSep+1];
End=[TickSep-1 length(TmpStr)];
Strs=cell(length(TickSep),1);
for k=1:length(TickSep)
    Strs{k}=TmpStr(Start(k):End(k));
end
if nargin>3 && ischar(FirstLabel)
    Strs(1) = Local_datestr(ticks(1),FirstLabel,Language);
end


function [ticks,format]=Local_datetick(handle,ax)
%Similar to DATETICK with the addition of the handle argument
limmanual = strcmp(get(handle,[ax 'limmode']),'manual');
if limmanual
    lim = get(handle,[ax 'lim']);
else
    lim = limits(handle,ax);
    if any(~isfinite(lim)) % no data !?
        lim = get(handle,[ax 'lim']);
    end
end
[ticks,format] = bestscale(lim);
if ~limmanual && length(ticks)>1
    set(handle,[ax 'lim'],[min(lim(1),min(ticks)) max(lim(2),max(ticks))]);
else
    ticks = ticks(ticks>=lim(1) & ticks<=lim(2));
end


function [ticks,format] = bestscale(lim)
%BESTSCALE Returns ticks for "best" scale.
ntickpref=5;

dlim=lim(2)-lim(1);
if dlim==0
    ticks = lim(1);
    format = 'date: %D %3O %Y %H:%2.2m:%02.0s';
    return
end
dt=[365  91   30   14  7  2  1 1/2 1/4 1/8 1/12 1/24 1/48  1/72  1/96  1/144 1/288 1/720 1/1440 1/2880 1/4320 1/5760 1/8640 1/17280 1/43200 1/86400];
%   yr  qrtr mnth 2wk wk 2dy dy 12h 6h  3h   2h   1h  30min 20min 15min 10min 5min  2min   1min    30s    20s   15s     10s     5s      2s      1s
ntick=dlim./dt;
ntick(ntick==0)=eps;
fnc=ntick+(ntickpref^2)./ntick;
[fncmn,i]=min(fnc);

dnum=lim(1);
dvec=datevec(dnum);
switch i
    case 1 % year
        nyr=[100 50 25 20 10 5 4 2 1];
        dt=nyr*365.25;
        ntick=dlim./dt;
        ntick(ntick==0)=eps;
        fnc=ntick+(ntickpref^2)./ntick;
        [fncmn,i]=min(fnc);

        nyr=nyr(i);
        styr=ceil(dvec(1)/nyr)*nyr;
        dvec=[styr 1 1 0 0 0];
        dvecstep=[nyr 0 0 0 0 0];
        format='date:%Y';
    case 2 % quarter
        stqrt=mod(ceil((dvec(2)-3)/3)*3,12)+1;
        dvec=[dvec(1) stqrt 1 0 0 0];
        dvecstep=[0 3 0 0 0 0];
        format='date:Q%Q %Y';
    case 3 % month
        dvec=[dvec(1:2) 1 0 0 0];
        dvecstep=[0 1 0 0 0 0];
        format='date:%O';
    case {4,5} % 2wk, wk
        dvec=[dvec(1:3) 0 0 0];
        dvecstep=[0 0 dt(i) 0 0 0];
        format='date:%D %3O';
    case {6,7} % 2dy, dy
        dvec=[dvec(1:3) 0 0 0];
        dvecstep=[0 0 dt(i) 0 0 0];
        format='date:%D %3O';
    case {8,9,10,11,12} % 12,6,3,2,1 hours
        ii=7;
        %
        nhr=[12 6 3 2 1];
        nhr=nhr(i-ii);
        sthr=rem(ceil(dvec(4)/nhr)*nhr-1,24)+1;
        dvec=[dvec(1:3) sthr 0 0];
        dvecstep=[0 0 0 nhr 0 0];
        format='date:%Hh';
    case {13,14,15,16,17,18,19} % 30,20,15,10,5,2,1 minutes
        ii=12;
        %
        nmn=[30 20 15 10 5 2 1];
        nmn=nmn(i-ii);
        stmn=rem(ceil(dvec(5)/nmn)*nmn-1,60)+1;
        dvec=[dvec(1:4) stmn 0];
        dvecstep=[0 0 0 0 nmn 0];
        format='date:%H:%2.2m';
    case {20,21,22,23,24,25,26} % 30,20,15,10,5,2,1 seconds
        ii=19;
        %
        nsc=[30 20 15 10 5 2 1];
        nsc=nsc(i-ii);
        stsc=rem(ceil(dvec(6)/nsc)*nsc-1,60)+1;
        dvec=[dvec(1:5) stsc];
        dvecstep=[0 0 0 0 0 nsc];
        format='date:%H:%2.2m:%02.0s';
end

ticks=repmat(dvec,11,1)+repmat((0:10)',1,6).*repmat(dvecstep,11,1);
ticks=datenum(ticks(:,1),ticks(:,2),ticks(:,3),ticks(:,4),ticks(:,5),ticks(:,6))';
ticks=unique(ticks);
ticks(ticks>lim(2))=[];
ticks=ticks+datenum(dvecstep(1),dvecstep(2),dvecstep(3),dvecstep(4),dvecstep(5),dvecstep(6))/10000;
