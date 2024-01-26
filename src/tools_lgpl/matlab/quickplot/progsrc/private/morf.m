function Out=morf(cmd,varargin)
%MORF Read Delft3D-MOR morf files.
%   FileData = morf('read',filename);
%     reads and checks data from a morf file.
%   morf('read',filename);
%     reads,checks and plots data from a morf file.
%
%   CheckOK  = morf('check',FileData);
%     checks data for morf file.
%
%   AxesHandle = morf('plot',FileData);
%     plots the tree structure of a morf file.

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
    if nargout>0
        Out=[];
    end
    return
end
switch cmd
    case 'read'
        Structure=Local_read_morf(varargin{:});
        Structure=Local_check_morf(Structure);
        if nargout>0
            if ~isstruct(Structure)
                Out=[];
            elseif strcmp(Structure.Check,'NotOK')
                Out=[];
            else
                Out=Structure;
            end
        else
            Local_plot_morf(0,Structure);
        end
    case 'check'
        if nargin==1
            Out=0;
        else
            Structure=Local_check_morf(varargin{1});
            if nargout>0,
                Out=strcmp(Structure.Check,'OK');
            end
        end
    case {'plot','plottime'}
        if nargin==1
            ax=[];
        else
            ax=Local_plot_morf(strcmp(cmd,'plottime'),varargin{:});
        end
        if nargout>0
            Out=ax;
        end
    otherwise
        error('Unrecognized command: %s',cmd)
end

function Structure=Local_read_morf(filename)
Structure=[];

if nargin==0
    [fn,fp]=uigetfile('morf.*');
    if ~ischar(fn),
        return;
    end
    filename=[fp fn];
end
fid=fopen(filename,'rt','n','US-ASCII');
if fid<0
    error('Cannot open file: %s.',filename)
end
Data={};
DataLine=0;
LineNr=[];
i=0;
while ~feof(fid),
    Line=fgetl(fid);
    i=i+1;
    if ~isempty(Line) && ~strcmp(Line(1),'*')
        DataLine=DataLine+1;
        Data{DataLine}=Line;
        LineNr(DataLine)=i;
        if DataLine==1
            X=strfind(Line,'''');
            if length(X)<4
                fclose(fid);
                error('Error reading line 1: missing case name or label string.');
            end
        end
    end
end
fclose(fid);

%for i=1:DataLine,
%  fprintf(1,'%s\n',Data(I));
%end

% Line 1 : CASE LABEL DUMMY1 DUMMY2
X=strfind(Data{1},'''');
Structure.CASE=Data{1}((X(1)+1):(X(2)-1));
Structure.LABEL=Data{1}((X(3)+1):(X(4)-1));
% Line 2 : INITP
X=sscanf(Data{2},'%i',1);
switch X
    case 0
        Structure.INITP='initial run';
    case 1
        Structure.INITP='decentral restart';
    case 2
        Structure.INITP='central restart';
    otherwise
        Structure.INITP='unknown';
end
% Line 3 : ITO1 ITO2
X=sscanf(Data{3},'%i',2);
Structure.ITO(6)=rem(X(2),100);
Structure.ITO(5)=rem(floor(X(2)/100),100);
Structure.ITO(4)=floor(X(2)/10000);
Structure.ITO(3)=rem(X(1),100);
Structure.ITO(2)=rem(floor(X(1)/100),100);
Structure.ITO(1)=floor(X(1)/10000);
% Line 4 : ITIMA TSCALE
X=sscanf(Data{4},'%f',2);
Structure.ITIMA=X(1);
Structure.TSCALE=X(2);
% Line 5 : NBACK
X=sscanf(Data{5},'%i',1);
switch X,
    case 0,
        Structure.NBACK='no backup';
    case 1,
        Structure.NBACK='decentral backup';
    case 2,
        Structure.NBACK='central backup';
    otherwise,
        Structure.NBACK='unknown';
end
% Line 6 : NINPFI
X=sscanf(Data{6},'%i',1);
Structure.NINPFI=X;
% Line 7 till 6+NINPFI : IPROC IVERS INPUT-FILE-NAME
for i=1:Structure.NINPFI,
    X=sscanf(Data{6+i},'%i',2);
    switch X(1),
        case 1,
            Structure.PhysSubProc(i).IPROC='waves';
        case 2,
            Structure.PhysSubProc(i).IPROC='flow';
        case 3,
            Structure.PhysSubProc(i).IPROC='transport';
        case 4,
            Structure.PhysSubProc(i).IPROC='bottom';
        otherwise,
            Structure.PhysSubProc(i).IPROC='unknown';
    end
    Structure.PhysSubProc(i).IVERS=X(2);
    X=strfind(Data{6+i},'''');
    Structure.PhysSubProc(i).INPUTFILENAME=Data{6+i}((X(1)+1):(X(2)-1));
end
Offset=6+Structure.NINPFI;
% Line Offset+1 : ITLEN
X=sscanf(Data{Offset+1},'%i',1);
Structure.ITLEN=X;
% Line Offset+2 : NORDER NESPRO
X=sscanf(Data{Offset+2},'%i',2);
Structure.NORDER=X(1);
Structure.NESPRO=X(2);
% Line Offset+3 till Offset+2+NORDER
for i=1:Structure.NORDER,
    %   Line : ICHILD IPARNT
    X=sscanf(Data{Offset+2+i},'%i',2);
    Structure.Controller(i).ICHILD=X(1);
    Structure.Controller(i).IPARNT=X(2);
end
if ~isequal(sort(cat(2,Structure.Controller(:).ICHILD)),1:Structure.NORDER)
    error('Invalid child number in child-parent list.')
end
Offset=Offset+2+Structure.NORDER;
% Line Offset+1 till ?

for i=1:Structure.NORDER,
    %   Line : I NCTR ITELCM XTELM
    X=sscanf(Data{Offset+1},'%f',4);
    if ~isequal(i,X(1))
        warning('Invalid I on line %i.',LineNr(Offset+1))
        %X(1)=i;
    end
    I=find([Structure.Controller(:).ICHILD]==X(1));
    Structure.Controller(I).NCTR=X(2);
    Structure.Controller(I).ITELCM=X(3);
    Structure.Controller(I).XTELM=X(4);
    Offset=Offset+1;
    if (Structure.Controller(I).NCTR==4) || (Structure.Controller(I).NCTR==5),
        %     Line : NAMITQ IEPSC CITP
        X=sscanf(Data{Offset+1},'%i',1);
        X=strfind(Data{Offset+1},'''');
        Structure.Controller(I).NAMITQ=Data{Offset+1}((X(1)+1):(X(2)-1));
        X=sscanf(Data{Offset+1}((X(2)+1):end),'%i',2);
        Structure.Controller(I).ITEPSC=X(1);
        if length(X)>1, % CITP might not be included
            Structure.Controller(I).CITP=X(2);
        else
            Structure.Controller(I).CITP=1; % one by default
        end
        Offset=Offset+1;
    end
    %   Line : NTIMIN1 NTIMIN2 NTIMIN3 NTIMIN4
    X=sscanf(Data{Offset+1},'%i',4);
    Structure.Controller(I).NTIMIN=X;
    Offset=Offset+1;
    if (Structure.Controller(I).NTIMIN(4)==1),
        %     Line : IDTSF
        X=sscanf(Data{Offset+1},'%i',1);
        Structure.Controller(I).IDTSF=X;
        Offset=Offset+1;
    elseif (Structure.Controller(I).NTIMIN(4)==2),
        %     Line : IDTSV
        X=sscanf(Data{Offset+1},'%i',1);
        Structure.Controller(I).IDTSV=X;
        Offset=Offset+1;
    elseif (Structure.Controller(I).NTIMIN(4)==4),
        %     Line : IFET IFESDT
        X=sscanf(Data{Offset+1},'%i',2);
        Structure.Controller(I).IFET=X(1);
        Structure.Controller(I).IFESDT=X(2);
        Offset=Offset+1;
    end
end

for i=1:Structure.NESPRO,
    %   LINE : IESP K L M N
    Offset=Offset+1;
    X=sscanf(Data{Offset},'%i',5);
    if length(X)<5
        error('Error reading line %i: %s',LineNr(Offset),Data{Offset})
    end
    if ~isequal(X(1),i)
        warning('Expected element number %i instead of %i.\nFile: %s\nLine %i:%s.',i,X(1),filename,LineNr(Offset),Data{Offset})
    end
    I=i;%I=find([Structure.Controller(:).ICHILD]==X(1));
    Structure.ElemSubProc(I).K=X(2);
    Structure.ElemSubProc(I).L=X(3);
    Structure.ElemSubProc(I).M=X(4);
    Structure.ElemSubProc(I).N=X(5);
    S2=sum(X(2:5)~=0);
    for j=1:S2,
        %     LINE : IM ITA ITB
        X=sscanf(Data{Offset+1},'%i',3);
        IM=X(1);
        Offset=Offset+1;
        Structure.ElemSubProc(I).PhysSubProc(IM).ITA=X(2);
        Structure.ElemSubProc(I).PhysSubProc(IM).ITB=X(3);
    end
end


function Structure=Local_check_morf(Structure)
if ~isstruct(Structure),
    Structure=[];
    Structure.Check='NotOK';
    return;
end
Structure.Check='NotOK';
Contr=1:Structure.NORDER;
Child=[Structure.Controller(:).ICHILD];
Parent=[Structure.Controller(:).IPARNT];
ElProc=setdiff(Child,Parent);
TopProc=setdiff(Parent,Child);
Level=zeros(size(Contr));
Level(ismember(Child,ElProc))=1;
Weight=zeros(size(Contr));
Weight(ismember(Child,ElProc))=1;
N=0;
while (min(Level)==0) && (N<Structure.NORDER)
    for ContrI=Contr
        if (Level(ContrI)==0)
            ChildI=find(Parent==Child(ContrI));
            if all(Level(ChildI)~=0)
                Level(ContrI)=max(Level(ChildI))+1;
                Weight(ContrI)=sum(Weight(ChildI));
            end
        end
    end
    N=N+1;
end
if any(Level==0)
    fprintf(1,'Warning: Tree contains unconnected branches.\n');
end
for ContrI=Contr,
    Structure.Controller(ContrI).Level=Level(ContrI);
    Structure.Controller(ContrI).LevelWeight=Weight(ContrI);
    if (Structure.Controller(ContrI).Level==1) && (Structure.Controller(ContrI).ICHILD>Structure.NESPRO),
        fprintf(1,'Warning: Incorrect numbering of elementary subprocess.\n');
        return;
    end
end
Structure.Check='OK';


function ax=Local_plot_morf(plottime,Structure,ax)
if nargin<2
    ax=[];
    return
elseif ~isstruct(Structure)
    ax=[];
    return
end
if nargin==2
    ax=gca;
    set(ax,'xlim',[0 1], ...
        'ylim',[0 1], ...
        'box','on', ...
        'xticklabel','manual', ...
        'yticklabel','manual', ...
        'xticklabel',[], ...
        'yticklabel',[]);
    if matlabversionnumber>=8.04;
        set(ax,'sortMethod','childOrder');
    else
        set(ax,'drawmode','fast');
    end
else % ax specified
end

Contr=1:length(Structure.Controller);

% determine number of elementary processes
NElmProc=sum([Structure.Controller(:).Level]==1);

% determine top level and top process
% note the number of elementary processes should be
% Structure.Controller(TopProc).Weight
[TopLevel,TopProc]=max([Structure.Controller(:).Level]);

YLim=3*TopLevel+1.5;
XLim=5*NElmProc+1;

set(ax,'xlim',[0 XLim],'ylim',[0 YLim]);
%set(ax,'ytick',0:YLim,'ygrid','on','xtick',0:XLim,'xgrid','on')
XTick=1; YTick=1;
set(ax,'dataaspectratio',[1 1 1]);

% create arrays to be used for x and y offset
XOffset=zeros(Structure.NORDER,1);
YOffset=zeros(Structure.NORDER,1);

refdate=datenum(Structure.ITO(1),Structure.ITO(2),Structure.ITO(3),Structure.ITO(4),Structure.ITO(5),Structure.ITO(6));

LIFOProcList=zeros(Structure.NORDER,1);
LIFOProcList(1)=TopProc;
CurLIFO=1;
XOffset(TopProc)=mean(get(ax,'xlim'))-2*XTick;
YOffset(TopProc)=YTick*(3*(TopLevel-1)+1);
while CurLIFO~=0,
    NChildCurProc=0;
    SumWeights=0;
    ChildCurProc=[];
    for ContrI=Contr, % find children
        if Structure.Controller(ContrI).IPARNT==Structure.Controller(LIFOProcList(CurLIFO)).ICHILD,
            NChildCurProc=NChildCurProc+1;
            ChildCurProc(NChildCurProc)=ContrI;
            SumWeights=SumWeights+Structure.Controller(ContrI).LevelWeight;
        end
    end
    %  fprintf(1,'Controller(%i)=%i\nChildren=',LIFOProcList(CurLIFO),Structure.Controller(LIFOProcList(CurLIFO)).ICHILD);
    %  fprintf(1,' %i',[Structure.Controller(ChildCurProc).ICHILD]);
    %  fprintf(1,'\n');
    if NChildCurProc>0,
        PartSumWeights=0;
        for i=1:NChildCurProc,
            XOffset(ChildCurProc(i))=XOffset(LIFOProcList(CurLIFO))-(5*(SumWeights-1)+4)*XTick/2+(5*PartSumWeights)*XTick+(5*(Structure.Controller(ChildCurProc(i)).LevelWeight-1)+4)*XTick/2;
            YOffset(ChildCurProc(i))=YTick*(3*(Structure.Controller(ChildCurProc(i)).Level-1)+1);
            PartSumWeights=PartSumWeights+Structure.Controller(ChildCurProc(i)).LevelWeight;
        end
        LIFOProcList(CurLIFO-1+(1:NChildCurProc))=ChildCurProc;
        CurLIFO=CurLIFO+NChildCurProc-1;
    else
        CurLIFO=CurLIFO-1;
    end
end

for ContrI=Contr,
    if ContrI~=TopProc,
        Parent=find([Structure.Controller(:).ICHILD]==Structure.Controller(ContrI).IPARNT);
        Line=line([XOffset(ContrI)+[2 2]*XTick XOffset(Parent)+2*XTick], ...
            [YOffset(ContrI)+0.5*YTick YOffset(Parent)+[0.5 0.5]*YTick], ...
            'parent',ax,'color',[0 0 0]);
    else
        Line=line(XOffset(ContrI)+[2 2]*XTick,YOffset(ContrI)+[0.5 2]*YTick, ...
            'parent',ax,'color',[0 0 0]);
    end
end

for ContrI=Contr
    switch Structure.Controller(ContrI).NCTR
        case 1
            Str='1';
        case 2
            Str=sprintf('%i',Structure.Controller(ContrI).ITELCM);
        case 3
            A=refdate+Structure.Controller(ContrI).ITELCM*Structure.TSCALE/60/60/24;
            Str={['until ',datestr(A,13)],[datestr(A,7) ' ' datestr(A,3) ' ' datestr(A,10)]};
        case {4,5}
            if Structure.Controller(ContrI).NCTR==4
                lessthan='<';
            else
                lessthan='>';
            end
            switch Structure.Controller(ContrI).ITEPSC
                case 1
                    qty=['\Delta',Structure.Controller(ContrI).NAMITQ];
                case 2
                    qty=['\delta',Structure.Controller(ContrI).NAMITQ];
                case 3
                    qty='\Delta|U|';
                case 4
                    qty='\delta|U|';
                case 5
                    qty='\Delta|F_W|';
                case 6
                    qty='\delta|F_W|';
            end
            Str={[sprintf('max %i, until\n',Structure.Controller(ContrI).ITELCM),sprintf('%s%s%g',qty,lessthan,Structure.Controller(ContrI).XTELM)]};
    end
    XX=XOffset(ContrI);
    if ContrI~=TopProc,
        Parent=find([Structure.Controller(:).ICHILD]==Structure.Controller(ContrI).IPARNT);
        YY=YOffset(Parent);
    else
        YY=YOffset(ContrI)+3*YTick;
    end
    Box=patch(XX+[.5 .5 3.5 3.5]*XTick,YY-[0.5 1.5 1.5 0.5]*YTick, [1 1 1], ...
        'zdata',[1 1 1 1],'parent',ax,'facecolor',[1 1 1],'edgecolor',[0 0 0]);
    Txt=Local_text(ax,YLim,XX+2*XTick,YY-1*YTick,2,Str);
    if plottime
        Txt=Local_number(ax,YLim,XX+.5*XTick,YY-1.5*YTick,2,sprintf('%i ',Structure.Controller(ContrI).ICHILD));
        Txt=Local_start(ax,YLim,XX+2*XTick,YY-0.5*YTick, ...
            2,Structure.Controller(ContrI),refdate,Structure.TSCALE);
        Txt=Local_end(ax,YLim,XX+2*XTick,YY-0.5*YTick, ...
            2,Structure.Controller(ContrI));
    end
    if (Structure.Controller(ContrI).Level==1),
        Box=patch(XOffset(ContrI)+[0 0 4 4]*XTick,YOffset(ContrI)+[0 1 1 0]*YTick,[1 1 1],'zdata',[1 1 1 1],'parent',ax,'facecolor',[1 1 1],'edgecolor',[0 0 0]);
        if  (Structure.Controller(ContrI).ICHILD>length(Structure.ElemSubProc)),
            set(Box,'facecolor',[1 0 0]);
        else
            ICHILD=Structure.Controller(ContrI).ICHILD;
            if Structure.ElemSubProc(ICHILD).K>0,
                switch Structure.ElemSubProc(ICHILD).K
                    case 1,
                        Str={'Hisw' ''};
                    case 3,
                        Str={'Swan' ''};
                    otherwise
                        Str={'Wav' ''};
                end
                ElmTime=Structure.ElemSubProc(ICHILD).PhysSubProc(1);
                if ~plottime
                    Str(2)=[];
                elseif ElmTime.ITA~=0
                    Str{2}=sprintf('%i-%i',ElmTime.ITA,ElmTime.ITB);
                else
                    Str{2}=sprintf('%i',ElmTime.ITB);
                end
                Txt=Local_text(ax,YLim,XOffset(ContrI)+0.5*XTick, ...
                    YOffset(ContrI)+0.5*YTick, ...
                    2,Str);
            end
            if Structure.ElemSubProc(ICHILD).L>0,
                Str={'Hydr' ''};
                ElmTime=Structure.ElemSubProc(ICHILD).PhysSubProc(2);
                if ~plottime
                    Str(2)=[];
                elseif ElmTime.ITA~=0
                    Str{2}=sprintf('%i-%i',ElmTime.ITA,ElmTime.ITB);
                else
                    Str{2}=sprintf('%i',ElmTime.ITB);
                end
                Txt=Local_text(ax,YLim,XOffset(ContrI)+1.5*XTick, ...
                    YOffset(ContrI)+0.5*YTick, ...
                    2,Str);
            end
            if Structure.ElemSubProc(ICHILD).M>0,
                switch Structure.ElemSubProc(ICHILD).M
                    case 1,
                        Str={'Tot' ''};
                    case 2,
                        Str={'Silt' ''};
                    case 3,
                        Str={'Susp' ''};
                    otherwise
                        Str={'Tran' ''};
                end
                ElmTime=Structure.ElemSubProc(ICHILD).PhysSubProc(3);
                if ~plottime
                    Str(2)=[];
                elseif ElmTime.ITA~=0
                    Str{2}=sprintf('%i-%i',ElmTime.ITA,ElmTime.ITB);
                else
                    Str{2}=sprintf('%i',ElmTime.ITB);
                end
                Txt=Local_text(ax,YLim,XOffset(ContrI)+2.5*XTick, ...
                    YOffset(ContrI)+0.5*YTick, ...
                    2,Str);
            end
            if Structure.ElemSubProc(ICHILD).N>0,
                Str={'Bed' ''};
                ElmTime=Structure.ElemSubProc(ICHILD).PhysSubProc(4);
                if ~plottime
                    Str(2)=[];
                elseif ElmTime.ITA~=0
                    Str{2}=sprintf('%i-%i',ElmTime.ITA,ElmTime.ITB);
                else
                    Str{2}=sprintf('%i',ElmTime.ITB);
                end
                Txt=Local_text(ax,YLim,XOffset(ContrI)+3.5*XTick, ...
                    YOffset(ContrI)+0.5*YTick, ...
                    2,Str);
            end
        end
    end
end
set(ax,'visible','off');
if plottime
    text(XLim,0,sprintf('Tscale: %gs',Structure.TSCALE), ...
        'fontunits','normalized', ...
        'fontsize',0.25/YLim, ...
        'parent',ax, ...
        'color',[0 0 0], ...
        'horizontalalignment','right', ...
        'verticalalignment','bottom');
end
l=findall(ax,'type','line');
p=findall(ax,'type','patch');
set(l,'linewidth',0.00001)
set(p,'linewidth',0.00001)


function Handle=Local_text(ax,YLim,x,y,z,Str)
Handle=text(x,y,z,Str, ...
    'fontunits','normalized', ...
    'fontsize',0.35/YLim, ...
    'parent',ax, ...
    'color',[0 0 0], ...
    'horizontalalignment','center', ...
    'verticalalignment','middle');

function Handle=Local_number(ax,YLim,x,y,z,Str)
Handle=text(x,y,z,Str, ...
    'fontunits','normalized', ...
    'fontsize',0.25/YLim, ...
    'parent',ax, ...
    'color',[0 0 0], ...
    'horizontalalignment','right', ...
    'verticalalignment','baseline');

function Handle=Local_start(ax,YLim,x,y,z,S,refdate,tscale)
Handle=[];
switch S.NTIMIN(1)
    case 1 % start time of controller S.NTIMIN(2)
        Handle=patch(x-0.1+(-.7+[.7 .2 .7 -.7 -.2 -.7 .7])*0.125,y+0.05+(1+[1 0 -1 -1 0 1 1])*0.125,1,'parent',ax,'edgecolor',[0 0 0],'facecolor',[1 1 1]);
        Handle(2)=patch(x-0.1+(-.7+[.2 .5 -.5 -.2 .2])*0.125,y+0.05+(1+[0 .6 .6 0 0])*0.125,1,'parent',ax,'facecolor',[0 0 0]);
        Handle(3)=text(x-0.1-1.4*0.125,y,z,sprintf('%i ',S.NTIMIN(2)), ...
            'fontunits','normalized', ...
            'fontsize',0.25/YLim, ...
            'parent',ax, ...
            'color',[0 0 0], ...
            'horizontalalignment','right', ...
            'verticalalignment','bottom');
    case 2 % end time of controller S.NTIMIN(2)
        Handle=patch(x-0.1+(-.7+[.7 .2 .7 -.7 -.2 -.7 .7])*0.125,y+0.05+(1+[1 0 -1 -1 0 1 1])*0.125,1,'parent',ax,'edgecolor',[0 0 0],'facecolor',[1 1 1]);
        Handle(2)=patch(x-0.1+(-.7+[.4 .7 -.7 -.4 .4])*0.125,y+0.05+(1+[-0.4 -1 -1 -0.4 -0.4])*0.125,1,'parent',ax,'facecolor',[0 0 0]);
        Handle(3)=text(x-0.1-1.4*0.125,y,z,sprintf('%i ',S.NTIMIN(2)), ...
            'fontunits','normalized', ...
            'fontsize',0.25/YLim, ...
            'parent',ax, ...
            'color',[0 0 0], ...
            'horizontalalignment','right', ...
            'verticalalignment','bottom');
    case 3 % central time
        Handle=patch(x-0.1+(-1+sin((0:32)*pi/16))*0.125,y+0.05+(1+cos((0:32)*pi/16))*0.125,1,'parent',ax,'edgecolor',[0 0 0],'facecolor',[1 1 1]);
        Handle=line(x-0.1+(-1+[0 0 0.5])*0.125,y+0.05+(1+[1 0 0.5])*0.125,'parent',ax,'color',[0 0 0]);
    case 4 % fixed time
        A=refdate+S.NTIMIN(2)*tscale/60/60/24;
        Str={[datestr(A,13) ' '],[datestr(A,7) ' ' datestr(A,3) ' ' datestr(A,10) ' ']};
        Handle=text(x,y,z,Str, ...
            'fontunits','normalized', ...
            'fontsize',0.25/YLim, ...
            'parent',ax, ...
            'color',[0 0 0], ...
            'horizontalalignment','right', ...
            'verticalalignment','bottom');
end


function Handle=Local_end(ax,YLim,x,y,z,S)
Handle=[];
switch S.NTIMIN(3)
    case 1 % update central time
        Handle=patch(x+0.1+(1+sin((0:32)*pi/16))*0.125,y+0.05+(1+cos((0:32)*pi/16))*0.125,1,'parent',ax,'edgecolor',[0 0 0],'facecolor',[1 1 1]);
        Handle=line(x+0.1+(1+[0 0 0.5])*0.125,y+0.05+(1+[1 0 0.5])*0.125,'parent',ax,'color',[0 0 0]);
    case 2 % if condition not satisfied: start, else: update
        patch(x+0.25+(.7+[.7 .2 .7 -.7 -.2 -.7 .7])*0.125,y+0.05+(1+[1 0 -1 -1 0 1 1])*0.125,1,'parent',ax,'edgecolor',[0 0 0],'facecolor',[1 1 1]);
        patch(x+0.25+(.7+[.2 .5 -.5 -.2 .2])*0.125,y+0.05+(1+[0 .6 .6 0 0])*0.125,1,'parent',ax,'facecolor',[0 0 0]);
        patch(x+0.6+(1+sin((0:32)*pi/16))*0.125,y+0.05+(1+cos((0:32)*pi/16))*0.125,1,'parent',ax,'edgecolor',[0 0 0],'facecolor',[1 1 1]);
        line(x+0.6+(1+[0 0 0.5])*0.125,y+0.05+(1+[1 0 0.5])*0.125,'parent',ax,'color',[0 0 0]);
        switch S.NTIMIN(4)
            case 1
                Str=sprintf('%i',S.IDTSF);
            case 2
                if length(S.IDTSV)==1
                    Str=sprintf('%i',S.IDTSV);
                elseif length(S.IDTSV)==2
                    Str=sprintf('%i,%i',S.IDTSV(1:2));
                else
                    Str=sprintf('%i,%i, ...',S.IDTSV(1:2));
                end
            case 3
                Str='   last';
                patch(x+0.875+(.7+[.7 .2 .7 -.7 -.2 -.7 .7])*0.125,y+0.075+(1+[1 0 -1 -1 0 1 1])*0.125,1,'parent',ax,'edgecolor',[0 0 0],'facecolor',[1 1 1]);
                patch(x+0.9+(.7+[.7 .2 .7 -.7 -.2 -.7 .7])*0.125,y+0.05+(1+[1 0 -1 -1 0 1 1])*0.125,1,'parent',ax,'edgecolor',[0 0 0],'facecolor',[1 1 1]);
            case 4
                Str=sprintf('   %i',S.IFET);
                switch S.IFESDT
                    case 0
                        patch(x+0.875+(.7+[.7 .2 .7 -.7 -.2 -.7 .7])*0.125,y+0.075+(1+[1 0 -1 -1 0 1 1])*0.125,1,'parent',ax,'edgecolor',[0 0 0],'facecolor',[1 1 1]);
                    case 1
                        Str=[Str ' Wav'];
                    case 2
                        Str=[Str ' Hydr'];
                    case 3
                        Str=[Str ' Tran'];
                    case 4
                        Str=[Str ' Bed'];
                end
                patch(x+0.9+(.7+[.7 .2 .7 -.7 -.2 -.7 .7])*0.125,y+0.05+(1+[1 0 -1 -1 0 1 1])*0.125,1,'parent',ax,'edgecolor',[0 0 0],'facecolor',[1 1 1]);
        end
        Handle(5)=text(x,y,z,[' ?    ,     ',Str], ...
            'fontunits','normalized', ...
            'fontsize',0.25/YLim, ...
            'parent',ax, ...
            'color',[0 0 0], ...
            'horizontalalignment','left', ...
            'verticalalignment','bottom');
    case 3 % no update
        Handle=text(x,y,z,' -', ...
            'fontunits','normalized', ...
            'fontsize',0.25/YLim, ...
            'parent',ax, ...
            'color',[0 0 0], ...
            'horizontalalignment','left', ...
            'verticalalignment','bottom');
    case 4 % if condition not satisfied: start, else update end
        patch(x+0.25+(.7+[.7 .2 .7 -.7 -.2 -.7 .7])*0.125,y+0.05+(1+[1 0 -1 -1 0 1 1])*0.125,1,'parent',ax,'edgecolor',[0 0 0],'facecolor',[1 1 1]);
        patch(x+0.25+(.7+[.2 .5 -.5 -.2 .2])*0.125,y+0.05+(1+[0 .6 .6 0 0])*0.125,1,'parent',ax,'facecolor',[0 0 0]);
        patch(x+0.6+(.7+[.7 .2 .7 -.7 -.2 -.7 .7])*0.125,y+0.05+(1+[1 0 -1 -1 0 1 1])*0.125,1,'parent',ax,'edgecolor',[0 0 0],'facecolor',[1 1 1]);
        patch(x+0.6+(.7+[.4 .7 -.7 -.4 .4])*0.125,y+0.05+(1+[-0.4 -1 -1 -0.4 -0.4])*0.125,1,'parent',ax,'facecolor',[0 0 0]);
        switch S.NTIMIN(4)
            case 1
                Str=sprintf('%i',S.IDTSF);
            case 2
                if length(S.IDTSV)==1
                    Str=sprintf('%i',S.IDTSV);
                elseif length(S.IDTSV)==2
                    Str=sprintf('%i,%i',S.IDTSV(1:2));
                else
                    Str=sprintf('%i,%i, ...',S.IDTSV(1:2));
                end
            case 3
                Str='   last';
                patch(x+0.875+(.7+[.7 .2 .7 -.7 -.2 -.7 .7])*0.125,y+0.075+(1+[1 0 -1 -1 0 1 1])*0.125,1,'parent',ax,'edgecolor',[0 0 0],'facecolor',[1 1 1]);
                patch(x+0.9+(.7+[.7 .2 .7 -.7 -.2 -.7 .7])*0.125,y+0.05+(1+[1 0 -1 -1 0 1 1])*0.125,1,'parent',ax,'edgecolor',[0 0 0],'facecolor',[1 1 1]);
            case 4
                Str=sprintf('   %i',S.IFET);
                switch S.IFESDT
                    case 0
                        patch(x+0.875+(.7+[.7 .2 .7 -.7 -.2 -.7 .7])*0.125,y+0.075+(1+[1 0 -1 -1 0 1 1])*0.125,1,'parent',ax,'edgecolor',[0 0 0],'facecolor',[1 1 1]);
                    case 1
                        Str=[Str ' Wav'];
                    case 2
                        Str=[Str ' Hydr'];
                    case 3
                        Str=[Str ' Tran'];
                    case 4
                        Str=[Str ' Bed'];
                end
                patch(x+0.9+(.7+[.7 .2 .7 -.7 -.2 -.7 .7])*0.125,y+0.05+(1+[1 0 -1 -1 0 1 1])*0.125,1,'parent',ax,'edgecolor',[0 0 0],'facecolor',[1 1 1]);
        end
        Handle(5)=text(x,y,z,[' ?    ,     ',Str], ...
            'fontunits','normalized', ...
            'fontsize',0.25/YLim, ...
            'parent',ax, ...
            'color',[0 0 0], ...
            'horizontalalignment','left', ...
            'verticalalignment','bottom');
end


