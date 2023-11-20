function Out=dxf(cmd,varargin)
% DXF File operations for AutoCad DXF files
%        Data = dxf('read',FileName);
%          reads dxf data.
%
%        Handles = dxf('plot',FileName);
%          reads and plots dxf data.
%
%        Succes = dxf('save',Handle,FileName,...options...);
%          saves graphics objects to a dxf file.
%          option: 'separatelayers'   0 or 1

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
        Out=Local_load_dxf(varargin{:});
    case 'plot'
        Out=Local_plot_dxf(varargin{:});
    case 'save'
        Out=Local_save_dxf(varargin{:});
    otherwise
        if ischar(cmd)
            Str=['unknown command: ',cmd];
        else
            Str='No command string specified.';
        end
        uiwait(msgbox(Str,'modal'));
end


function Data = Local_load_dxf(filename)
Data={};

if nargin==0
    [fn,fp]=uigetfile('*.dxf');
    if ~ischar(fn)
        return
    end
    filename=[fp fn];
end
if (length(filename)<4) | ~strcmp(lower(filename(end+(-3:0))),'.dxf')
    filename=[filename '.dxf'];
end
fid=fopen(filename,'r','n','US-ASCII');
Code=fgetl(fid); Line=fgetl(fid);

while 1
    switch Line
        case 'SECTION'
            Code=fgetl(fid); Line=fgetl(fid); % 2 and SECTION TYPE
            if strcmp(Line,'BLOCKS')
                Blocks = Local_blocks(fid);
            end
            if strcmp(Line,'ENTITIES')
                Data = Local_entities(fid);
            end
        otherwise
            Code=fgetl(fid); Line=fgetl(fid);
            if feof(fid), break; end;
    end
end
fclose(fid);


function Block = Local_blocks(fid)
Block={};
b=1;
Code=fscanf(fid,'%i \n',1); Line=fgetl(fid);
while 1
    while Code~=8
        Code=fscanf(fid,'%i \n',1); Line=fgetl(fid);
    end
    Block{b,1}=Line;
    Code=fgetl(fid); Line=fgetl(fid);
    Block{b,2}=Line;
    %fprintf('Block definition found: %s: %s\n',Block{b,1},Block{b,2})
    b=b+1;
    while 1
        Line=fgetl(fid); Line=fgetl(fid);
        if feof(fid) | strcmp(Line,'ENDSEC')
            return
        elseif strcmp(Line,'BLOCK')
            break
        end
    end
end


function Data = Local_entities(fid)
Data={};
Line=fgetl(fid); Line=fgetl(fid);
i=1;
BufferSize=1000;
Buffer=zeros(BufferSize,3);
PBufferSize=1000;
PBuffer=zeros(PBufferSize,3);
SBufferSize=1000;
SBuffer=zeros(12,SBufferSize);
NPnt=0;
SSol=0;

while 1
    switch Line
        case 'LINE'
            %    fprintf('Reading %s\n',Line);
            Data{i}=[];
            NSegm=0;
            while 1
                while 1
                    Line=fgetl(fid);
                    if ~ischar(Line), break; end
                    if strcmp(strtok(Line),'10'), break; end
                    Line=fgetl(fid);
                end
                if ~ischar(Line), break; end
                NSegm=NSegm+3;
                if NSegm>BufferSize % increase buffer size
                    BufferSize=BufferSize+1000;
                    Buffer(BufferSize,1)=0;
                end
                LineBuffer=fscanf(fid,'%f 20 %f 30 %f 11 %f 21 %f 31 %f',[3 2]);
                Line=fgetl(fid); % remainder of line
                Buffer(NSegm+(-2:-1),1:3)=LineBuffer';
                Buffer(NSegm,:)=NaN;
                while 1
                    Line=fgetl(fid);
                    if ~ischar(Line), break; end
                    if strcmp(strtok(Line),'0'), break; end
                    Line=fgetl(fid);
                end
                if ~ischar(Line), break; end
                Line=fgetl(fid);
                if ~strcmp(Line,'LINE')
                    Data{i}=Buffer(1:NSegm,:)';
                    i=i+1;
                    break
                end
            end
        case {'POLYLINE','LWPOLYLINE','SPLINE'}
            %    fprintf('Reading %s\n',Line);
            Data{i}=[];
            NSegm=0;
            readnextimmediately=0;
            while 1
                Line=fgetl(fid);
                if ~ischar(Line), break; end
                switch strtok(Line)
                    case '0'
                        break;
                    case '10'
                        readnextimmediately=1;
                        break;
                end
                Line=fgetl(fid);
            end
            if ~readnextimmediately
                Line=fgetl(fid);
            end
            while 1
                if ~readnextimmediately
                    while 1
                        Line=fgetl(fid);
                        if ~ischar(Line), break; end;
                        if strcmp(strtok(Line),'10'), break; end;
                        Line=fgetl(fid);
                    end
                    if ~ischar(Line), break; end;
                end
                NSegm=NSegm+1;
                if NSegm>BufferSize, % increase buffer size
                    BufferSize=BufferSize+1000;
                    Buffer(BufferSize,1)=0;
                end
                Tmp=fscanf(fid,'%f 20 %f 30 %f',[1 3]);
                readnextimmediately=0;
                if length(Tmp)==3
                    Buffer(NSegm,1:3)=Tmp;
                    Line=fgetl(fid); % remainder of line
                else
                    Buffer(NSegm,1:2)=Tmp;
                end
                while 1
                    Line=fgetl(fid);
                    if ~ischar(Line), break; end;
                    switch strtok(Line)
                        case '0'
                            break
                        case '10'
                            readnextimmediately=1;
                            break
                    end
                    Line=fgetl(fid);
                end
                if ~readnextimmediately
                    if ~ischar(Line), break; end
                    Line=fgetl(fid);
                    if ~strcmp(Line,'VERTEX')
                        Data{i}=Buffer(1:NSegm,:)';
                        i=i+1;
                        break;
                    end
                end
            end
        case 'POINT'
            %    fprintf('Reading %s\n',Line);
            while 1
                while 1
                    Line=fgetl(fid);
                    if ~ischar(Line), break; end
                    if strcmp(strtok(Line),'10'), break; end
                end
                if ~ischar(Line), break; end
                NPnt=NPnt+2;
                if NPnt>PBufferSize % increase buffer size
                    PBufferSize=PBufferSize+1000;
                    PBuffer(PBufferSize,1)=0;
                end
                PBuffer(NPnt-1,1:3)=[NaN NaN NaN];
                PBuffer(NPnt,1:3)=fscanf(fid,'%f 20 %f 30 %f',[1 3]);
                Line=fgetl(fid); % remainder of line
                while 1
                    Line=fgetl(fid);
                    if ~ischar(Line), break; end
                    if strcmp(strtok(Line),'0'), break; end
                    Line=fgetl(fid);
                end
                if ~ischar(Line), break; end
                Line=fgetl(fid);
                if ~strcmp(Line,'POINT')
                    break
                end
            end
        case 'SOLID'
            %    fprintf('Reading %s\n',Line);
            while 1
                while 1
                    Line=fgetl(fid);
                    if ~ischar(Line), break; end
                    if strcmp(strtok(Line),'10'), break; end
                end
                if ~ischar(Line), break; end
                SSol=SSol+1;
                if SSol>SBufferSize % increase buffer size
                    SBufferSize=SBufferSize+1000;
                    SBuffer(1,SBufferSize)=0;
                end
                Temp=fscanf(fid,'%f 20 %f 30 %f',[3 1]);
                if length(Temp)==3
                    SBuffer(1:3,SSol)=Temp;
                    SBuffer(4:6,SSol)=fscanf(fid,' 11 %f 21 %f 31 %f',[3 1]);
                    SBuffer(10:12,SSol)=fscanf(fid,' 12 %f 22 %f 32 %f',[3 1]);
                    SBuffer(7:9,SSol)=fscanf(fid,' 13 %f 23 %f 33 %f',[3 1]);
                else
                    SBuffer(1:2,SSol)=Temp;
                    SBuffer(4:5,SSol)=fscanf(fid,' 11 %f 21 %f',[2 1]);
                    SBuffer(10:11,SSol)=fscanf(fid,' 12 %f 22 %f',[2 1]);
                    SBuffer(7:8,SSol)=fscanf(fid,' 13 %f 23 %f',[2 1]);
                end
                Line=fgetl(fid); % remainder of line
                while 1
                    Line=fgetl(fid);
                    if ~ischar(Line), break; end
                    if strcmp(strtok(Line),'0'), break; end
                    Line=fgetl(fid);
                end
                if ~ischar(Line), break; end
                Line=fgetl(fid);
                if ~strcmp(Line,'POINT')
                    break
                end
            end
        case 'ARC'
            fprintf('Skipping ARC\n');
            Line=fgetl(fid); Line=fgetl(fid);
            if feof(fid), break; end
        case 'TEXT'
            fprintf('Skipping TEXT\n');
            Line=fgetl(fid); Line=fgetl(fid);
            if feof(fid), break; end
        case 'CIRCLE'
            fprintf('Skipping CIRCLE\n');
            Line=fgetl(fid); Line=fgetl(fid);
            if feof(fid), break; end
        case 'INSERT'
            Line=fgetl(fid); Layer=fgetl(fid); % 8, Layer
            Line=fgetl(fid); Block=fgetl(fid); % 8, Block
            fprintf('Skipping INSERT: %s, %s\n',Layer, Block);
            if feof(fid), break; end
        otherwise
            Line=fgetl(fid); Line=fgetl(fid);
            if feof(fid), break; end
    end
    if ~ischar(Line), break; end
end
if NPnt>0
    Data{i}=PBuffer(1:NPnt,:);
    i=i+1;
end
if SSol>0
    Data{i}=SBuffer(:,1:SSol);
end


function H = Local_plot_dxf(filename)
Data = Local_load_dxf(filename);
H =zeros(1,length(Data));
for i=1:length(Data)
    if size(Data{i},1)==3
        if all(isnan(Data{i}(1,1:2:end))) % points
            H(i)=line(Data{i}(1,:),Data{i}(2,:),Data{i}(3,:),'color','k','marker','.');
        else
            H(i)=line(Data{i}(1,:),Data{i}(2,:),Data{i}(3,:),'color','k');
        end
    else
        N=size(Data{i},1);
        H(i)=patch(Data{i}(1:3:N,:),Data{i}(2:3:N,:),Data{i}(3:3:N,:),'edgecolor','k','facecolor','g');
    end
end


function Succes=Local_save_dxf(handle,filename,varargin)
SeparateLayers=0;
Succes=0;
for i=1:2:length(varargin)
    ops={'separatelayers'};
    j=ustrcmpi(lower(varargin{i}),ops);
    switch j
        case 1, % separatelayers
            SeparateLayers=varargin{i+1};
        otherwise
            error('Unknown option: %s',varargin{i})
    end
end
SaveAx=strcmp(get(handle,'type'),'axes');
%if ~SaveAx,
%   fprintf('Can currently only handle axes handles.\n');
%   return;
%end;
if nargin==1
    [fn,fp]=uiputfile('*.dxf');
    if ~ischar(fn)
        return
    end
    filename=[fp fn];
end
if (length(filename)<4) | ~strcmp(lower(filename(end+(-3:0))),'.dxf')
    filename=[filename '.dxf'];
end
fid=fopen(filename,'w','n','US-ASCII');

% WRITE HEADER **************************************************
fprintf(fid,'0\nSECTION\n2\nHEADER\n');
fprintf(fid,'9\n$ACADVER\n1\nAC1006\n');
if SaveAx
    xlim=get(handle,'xlim');
    ylim=get(handle,'ylim');
    zlim=get(handle,'zlim');
    fprintf(fid,'9\n$EXTMIN\n10\n%f\n20\n%f\n30\n%f\n',xlim(1),ylim(1),zlim(1));
    fprintf(fid,'9\n$EXTMAX\n10\n%f\n20\n%f\n30\n%f\n',xlim(2),ylim(2),zlim(2));
else
    xpage=get(handle,'position');
    ypage=[xpage(2) xpage(2)+xpage(4)];
    xpage=[xpage(1) xpage(1)+xpage(3)];
    fprintf(fid,'9\n$EXTMIN\n10\n%f\n20\n%f\n30\n%f\n',xpage(1),ypage(1),0);
    fprintf(fid,'9\n$EXTMAX\n10\n%f\n20\n%f\n30\n%f\n',xpage(2),ypage(2),0);
end
fprintf(fid,'0\nENDSEC\n');


% WRITE TABLES **************************************************
fprintf(fid,'0\nSECTION\n2\nTABLES\n');

fprintf(fid,'0\nTABLE\n2\nLTYPE\n70\n%i\n',4);
fprintf(fid,'0\nCONTINUOUS\n70\n64\n3\ncontinuous\n72\n65\n73\n0\n40\n0.0\n');
fprintf(fid,'0\nDASHED\n70\n64\n3\ndash dot\n72\n65\n73\n0\n40\n0.000003\n49\n0.000002\n49\n-0.000001\n');
fprintf(fid,'0\nENDTAB\n');

if SaveAx
    A=handle;
else
    A=transpose(findobj(handle,'type','axes','visible','on'));
end
L=transpose(findobj(handle,'type','line','visible','on'));
L=setdiff(L,findobj(L,'marker','none','linestyle','none'));
PA=transpose(findobj(handle,'type','patch','visible','on'));
S=transpose(findobj(handle,'type','surface','visible','on'));
if SeparateLayers
    NObj=length(L)+length(PA)+length(S)+length(A);
    fprintf(fid,'0\nTABLE\n2\nLAYER\n70\n%i\n',NObj);
    for a=A % <---------------------------------------------- AXES
        fprintf(fid,'0\nLAYER\n2\nAxes%bx\n',a);
        fprintf(fid,'70\n64\n62\n7\n6\nCONTINUOUS\n');
    end
    for l=L % <---------------------------------------------- LINES
        fprintf(fid,'0\nLAYER\n2\nLine%bx\n',l);
        fprintf(fid,'70\n64\n62\n7\n6\nCONTINUOUS\n');
    end
    for pa=PA % <-------------------------------------------- PATCHES
        fprintf(fid,'0\nLAYER\n2\nPatch%bx\n',pa);
        fprintf(fid,'70\n64\n62\n7\n6\nCONTINUOUS\n');
    end
    for s=S % <---------------------------------------------- SURFACES
        fprintf(fid,'0\nLAYER\n2\nSurf%bx\n',s);
        fprintf(fid,'70\n64\n62\n7\n6\nCONTINUOUS\n');
    end
    fprintf(fid,'0\nENDTAB\n');
else
    fprintf(fid,'0\nTABLE\n2\nLAYER\n70\n%i\n',1);
    Layer=sprintf('%bx',handle);
    fprintf(fid,'0\nLAYER\n2\n%s\n',Layer); % <------------- AXES/FIGURE
    fprintf(fid,'70\n64\n62\n7\n6\nCONTINUOUS\n');
    fprintf(fid,'0\nENDTAB\n');
end

fprintf(fid,'0\nENDSEC\n');


% WRITE BLOCKS **************************************************
%fprintf(fid,'0\nSECTION\n2\nBLOCKS\n');
%fprintf(fid,'0\nENDSEC\n');

% WRITE ENTITIES ************************************************
fprintf(fid,'0\nSECTION\n2\nENTITIES\n');

% *** DEFAULT LAYER *****
Layer=sprintf('%bx',handle);

% *** AXES **************
for a=A % loop over axes
    if strcmp(get(a,'visible'),'off')
        continue
    end
    if SeparateLayers, Layer=sprintf('Axes%bx',a); end;
    fprintf(fid,'0\nPOLYLINE\n8\n%s\n66\n1\n',Layer);  % start polyline on layer
    fprintf(fid,'10\n0.0\n20\n0.0\n30\n0.0\n'); % '30' field can be non-zero for Z value
    if SaveAx
        IdxH=[1 2 2 2 2 1 1 1 1 1 1 2 2 2 2 1
            1 1 1 2 2 2 2 1 1 2 2 2 2 1 1 1
            1 1 2 2 1 1 2 2 1 1 2 2 1 1 2 2];
    else
        IdxH=[1 2 2 1 1
            1 1 2 2 1
            1 1 1 1 1];
        axpos=get(a,'position');
        xlim=axpos(1)+[0 axpos(3)];
        ylim=axpos(2)+[0 axpos(4)];
        zlim=[0 0];
    end
    for i=IdxH
        fprintf(fid,'0\nVERTEX\n8\n%s\n',Layer); % add a point/vertex of the line on layer
        fprintf(fid,'10\n%f\n20\n%f\n30\n%f\n',xlim(i(1)),ylim(i(2)),zlim(i(3))); % point coordinates
    end
    fprintf(fid,'0\nSEQEND\n'); % end of sequence of vertices
    fprintf(fid,'8\n%s\n',Layer);  % layer name
end

% *** LINES **************
for l=L % loop over lines
    if SeparateLayers, Layer=sprintf('Line%bx',l); end
    pX=get(l,'xdata');
    pY=get(l,'ydata');
    pZ=get(l,'zdata');
    if ~SaveAx
        a=get(l,'parent');
        axpos=get(a,'position');
        xlim=get(a,'xlim');
        pX=axpos(1)+axpos(3)*(pX-xlim(1))/(xlim(2)-xlim(1));
        ylim=get(a,'ylim');
        pY=axpos(2)+axpos(4)*(pY-ylim(1))/(ylim(2)-ylim(1));
        pZ=zeros(size(pZ));
    end
    if isempty(pZ), pZ=zeros(size(pX)); end
    if isequal(size(pX),size(pY),size(pZ))
        endofline=0;
        p0=1;
        while ~endofline
            fprintf(fid,'0\nPOLYLINE\n8\n%s\n66\n1\n',Layer);  % start polyline on layer
            fprintf(fid,'10\n0.0\n20\n0.0\n30\n0.0\n'); % '30' field can be non-zero for Z value
            for p=p0:length(pX) % loop over points <-------------------------------------------- CAN BE VECTORIZED!
                if isnan(pX(p))|isnan(pY(p))|isnan(pZ(p))
                    break
                end
                fprintf(fid,'0\nVERTEX\n8\n%s\n',Layer); % add a point/vertex of the line on layer
                fprintf(fid,'10\n%f\n20\n%f\n30\n%f\n',pX(p),pY(p),pZ(p)); % point coordinates
            end
            fprintf(fid,'0\nSEQEND\n'); % end of sequence of vertices
            fprintf(fid,'8\n%s\n',Layer);  % layer name
            while p<length(pX)
                if isnan(pX(p))|isnan(pY(p))|isnan(pZ(p))
                    p=p+1;
                else
                    break
                end
            end
            if p==length(pX)
                endofline=1;
            else
                p0=p;
            end
        end
    end
end

% *** PATCHES ************
PvertMsg=1;
for pa=PA % loop over patches
    if SeparateLayers, Layer=sprintf('Patch%bx',pa); end
    pCrd=get(pa,'vertices');
    if size(pCrd,2)==2 % no Z data
        pCrd(1,3)=0; % add dummy Z data column
    end
    if ~SaveAx
        a=get(pa,'parent');
        axpos=get(a,'position');
        xlim=get(a,'xlim');
        pCrd(:,1)=axpos(1)+axpos(3)*(pCrd(:,1)-xlim(1))/(xlim(2)-xlim(1));
        ylim=get(a,'ylim');
        pCrd(:,2)=axpos(2)+axpos(4)*(pCrd(:,2)-ylim(1))/(ylim(2)-ylim(1));
        pCrd(:,3)=0;
    end
    pFac=get(pa,'faces');
    if ~isempty(pCrd) & ~isempty(pFac)
        if (size(pFac,2)==3)
            for f=1:size(pFac,1) % loop over faces <----------------------------------------- CAN BE VECTORIZED!
                fprintf(fid,'0\nSOLID\n8\n%s\n',Layer);
                %top left corner
                fprintf(fid,'10\n%f\n20\n%f\n30\n%f\n',pCrd(pFac(f,1),:));
                %top right corner
                fprintf(fid,'11\n%f\n21\n%f\n31\n%f\n',pCrd(pFac(f,2),:));
                %bottom right corner
                fprintf(fid,'12\n%f\n22\n%f\n32\n%f\n',pCrd(pFac(f,3),:));
                %bottom left corner duplicate of point 3 for correct reading by Coreldraw
                %this should not be necessary for AutoCAD.
                fprintf(fid,'13\n%f\n23\n%f\n33\n%f\n',pCrd(pFac(f,3),:));
            end
        elseif (size(pFac,2)==4)
            for f=1:size(pFac,1) % loop over faces <----------------------------------------- CAN BE VECTORIZED!
                fprintf(fid,'0\nSOLID\n8\n%s\n',Layer);
                %top left corner
                fprintf(fid,'10\n%f\n20\n%f\n30\n%f\n',pCrd(pFac(f,1),:));
                %top right corner
                fprintf(fid,'11\n%f\n21\n%f\n31\n%f\n',pCrd(pFac(f,2),:));
                %bottom right corner
                fprintf(fid,'12\n%f\n22\n%f\n32\n%f\n',pCrd(pFac(f,4),:));
                %bottom left corner
                fprintf(fid,'13\n%f\n23\n%f\n33\n%f\n',pCrd(pFac(f,3),:));
            end
        else
            if PvertMsg, fprintf('Patches with more than 3 vertices will be saved as lines.\n'); PvertMsg=0; end
            for f=1:size(pFac,1)
                endofline=0;
                fprintf(fid,'0\nPOLYLINE\n8\n%s\n66\n1\n',Layer);  % start polyline on layer
                fprintf(fid,'10\n0.0\n20\n0.0\n30\n0.0\n'); % '30' field can be non-zero for Z value
                for p=1:size(pFac,2) % loop over points <-------------------------------------------- CAN BE VECTORIZED!
                    fprintf(fid,'0\nVERTEX\n8\n%s\n',Layer); % add a point/vertex of the line on layer
                    fprintf(fid,'10\n%f\n20\n%f\n30\n%f\n',pCrd(pFac(f,p),:)); % point coordinates
                end
                fprintf(fid,'0\nSEQEND\n'); % end of sequence of vertices
                fprintf(fid,'8\n%s\n',Layer);  % layer name
            end
        end
    end
end

% *** SURFACES ***********
for s=S % loop over surfaces
    if SeparateLayers, Layer=sprintf('Surf%bx',s); end
    pX=get(s,'xdata');
    pY=get(s,'ydata');
    pZ=get(s,'zdata');
    if ~SaveAx
        a=get(s,'parent');
        axpos=get(s,'position');
        xlim=get(s,'xlim');
        pX=axpos(1)+axpos(3)*(pX-xlim(1))/(xlim(2)-xlim(1));
        ylim=get(s,'ylim');
        pY=axpos(2)+axpos(4)*(pY-ylim(1))/(ylim(2)-ylim(1));
        pZ=zeros(size(pZ));
    end
    if isequal(size(pX),size(pY),size(pZ))
        for a=1:size(pX,1)-1 % <--------------------------------- CAN BE VECTORIZED!
            for b=1:size(pX,2)-1 % <--------------------------------- CAN BE VECTORIZED!
                if ~any(isnan([pX(a,b) pY(a,b) pZ(a,b) pX(a+1,b) pY(a+1,b) pZ(a+1,b) ...
                        pX(a+1,b+1) pY(a+1,b+1) pZ(a+1,b+1) ...
                        pX(a,b+1) pY(a,b+1) pZ(a,b+1)]))
                    fprintf(fid,'0\nSOLID\n8\n%s\n',Layer);
                    %top left corner
                    fprintf(fid,'10\n%f\n20\n%f\n30\n%f\n',pX(a,b),pY(a,b),pZ(a,b));
                    %top right corner
                    fprintf(fid,'11\n%f\n21\n%f\n31\n%f\n',pX(a+1,b),pY(a+1,b),pZ(a+1,b));
                    %bottom right corner
                    fprintf(fid,'12\n%f\n22\n%f\n32\n%f\n',pX(a,b+1),pY(a,b+1),pZ(a,b+1));
                    %bottom left corner
                    fprintf(fid,'13\n%f\n23\n%f\n33\n%f\n',pX(a+1,b+1),pY(a+1,b+1),pZ(a+1,b+1));
                end
            end
        end
    end
end

% CLOSE FILE ****************************************************
fprintf(fid,'0\nEOF\n');
fclose(fid);
Succes=1;