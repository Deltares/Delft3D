function [Out1,Out2]=jspost(cmd,varargin)
%JSPOST Read JSPost files.
%
%   Struct=JSPOST('open','FileName')
%   opens the specified JSPOST files (pair of STU and PST files).
%
%   [Time,Data]=JSPOST('read',Struct,Substance,Segment,TStep)
%   reads the specified substance (0 for all), specified
%   segment (0 for all) and specified time step (0 for all)
%   from the Delwaq HIS or MAP file.

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

switch lower(cmd)
    case 'open'
        if nargout>1,
            error('Too many output arguments.');
        end;
        Out1=jspost_open(varargin{:});
    case 'read'
        [Out1,Out2]=jspost_read(varargin{:});
    otherwise,
        error('Unknown command: %s.',cmd)
end


function S=jspost_open(filename)

S.Check='NotOK';
S.FileType='JSPost';

if (nargin==0) | strcmp(filename,'?'),
    [fname,fpath]=uigetfile('*.*','Select JSPost file');
    if ~ischar(fname),
        return;
    end;
    filename=fullfile(fpath,fname);
end;

[p,f,e]=fileparts(filename);
S.FileName=[p filesep f];

fstu=fopen([S.FileName '.stu'],'r','n','US-ASCII');
fpst=fopen([S.FileName '.pst'],'r','l','US-ASCII');

if (fpst<0) | (fstu<0)
    if fpst>0
        fclose(fpst);
        error('Cannot open STU file.')
    elseif fstu>0
        fclose(fstu);
        error('Cannot open PST file.')
    else
        error('Cannot open PST nor STU file.')
    end
end

Line=fgetl(fstu);
[X,n,er]=sscanf(Line,' %i');
if n~=2 | ~isempty(er) | any(X<=0)
    fclose(fstu);
    fclose(fpst);
    error('Error reading line 1 of STU file.');
end
NSeg=X(1);
NPar=X(2);

Line=fgetl(fstu);
[X,n,er]=sscanf(Line,' %f');
if n~=3 | ~isempty(er) | any(X<0) | X(1)>X(2) | X(3)>X(2)
    fclose(fstu);
    fclose(fpst);
    error('Error reading line 2 of STU file.');
end
S.Times=X(1):X(3):X(2);

S.Header{1,1}=fgetl(fstu);
S.Header{2,1}=fgetl(fstu);
TimeUnit=fgetl(fstu);
if ~ischar(TimeUnit)
    fclose(fstu);
    fclose(fpst);
    error('Error reading STU header.')
end
TimeUnit=strtrim(TimeUnit);

Str=S.Header{2}(41:end);
[S.T0,S.TStep] = delwaqt0(Str);
switch TimeUnit
    case {'dag','day'}
        S.TStep=1;
    case {'uur','hour'}
        S.TStep=1/24;
    case 'min'
        S.TStep=1/(24*60);
    case 'sec'
        S.TStep=1/(24*3600);
end

for i=1:NPar
    Line=fgetl(fstu);
    if ~ischar(Line)
        fclose(fstu);
        fclose(fpst);
        error('Error reading name of parameter %i.',i)
    end
    S.SubsName{i}=strtrim(Line);
end

for i=1:NSeg
    Line=fgetl(fstu);
    if ~ischar(Line)
        fclose(fstu);
        fclose(fpst);
        error('Error reading name of segment %i.',i)
    end
    S.SegmentName{i}=strtrim(Line);
end

if ~isempty(fscanf(fstu,' %c')) | ~feof(fstu)
    fclose(fstu);
    fclose(fpst);
    error('Error STU file longer than expected.')
end

fclose(fstu);

%
%--- Finally we do something with the PST file ...
%
% Try to figure out the byte order by checking the
% minimum and maximum absolute values not equal to
% zero.
%
DataL=abs(fread(fpst,[NSeg*NPar 1],'float32'));
fclose(fpst);
fpst=fopen([S.FileName '.pst'],'r','b','US-ASCII');
DataB=abs(fread(fpst,[NSeg*NPar 1],'float32'));
MaxDataL=max(DataL(:)); MinDataL=min(DataL(DataL>0));
MaxDataB=max(DataB(:)); MinDataB=min(DataB(DataB>0));
if (MaxDataL>1e15) & (MinDataL<1e-15)
    S.ByteOrder='b';
elseif (MaxDataB>1e15) & (MinDataB<1e-15)
    S.ByteOrder='l';
elseif MaxDataL>MaxDataB
    S.ByteOrder='b';
else
    S.ByteOrder='l';
end
%
% An extra check.
%
fseek(fpst,0,1);
NBytes=ftell(fpst);
NTimes=NBytes/(NSeg*NPar*4);
fclose(fpst);

if NTimes==length(S.Times)
    % do nothing
elseif NTimes~=round(NTimes)
    error('Invalid length of PST file.')
else
    error('Length of PST file does not match STU specifications.')
end

S.Check='OK';


function [OTime,Data]=jspost_read(S,Subs,Seg,Time)

OTime=[];
Data=[];
if nargin<4,
    Time=0;
    if nargin<3,
        Seg=0;
        if nargin<2,
            Subs=0;
            if nargin<1,
                error('No file specified.');
            end;
        end;
    end;
end;

% Time must be a timestep number
NTimes=length(S.Times);
if ~isnumeric(Time)
    error('Invalid timestep')
elseif isequal(Time,0)
    tim=1:NTimes;
else
    tim=Time(:)';
end
ntim=length(tim);

% Subs can be either a substance name or number (0 for all substances)
NSubs=length(S.SubsName);
if ischar(Subs)
    subs=ustrcmpi(Subs,S.SubsName);
    if subs<0
        error('Non-unique substance name.')
    end
elseif ~isnumeric(Subs)
    error('Substance specifier should be name or number.')
elseif isequal(Subs,0)
    subs=1:NSubs;
elseif any(Subs>NSubs | Subs<1)
    error('Substance number out of range.')
else
    subs=Subs;
end
nsubs=length(subs);

% Seg can be either a segment number (or name if appropriate) (0 for all segments)
NSeg=length(S.SegmentName);
if ischar(Seg)
    seg=ustrcmpi(Seg,S.SegmentName);
    if seg<0
        error('Non-unique segment name.')
    end
elseif ~isnumeric(Seg)
    error('Segment specifier should be name or number.')
elseif isequal(Seg,0)
    seg=1:NSeg;
elseif any(Seg>NSeg | Seg<1)
    error('Segment number out of range.')
else
    seg=Seg;
end
nseg=length(seg);

fid=fopen([S.FileName '.pst'],'r',S.ByteOrder,'US-ASCII');
if fid<0
    error('Cannot open PST file.');
end;

NTot=NSubs*NTimes;

OTime=tim';
if isequal(seg,1:NSeg)
    if nsubs==0 | ntim==0
        Data=zeros(nsubs,NSeg,ntim);
    elseif nsubs>1 & ntim>1
        Data=fread(fid,[NTot NSeg],'float32');
        Data=reshape(Data,NSubs,NTimes,NSeg);
        Data=Data(subs,tim,:);
        Data=permute(Data,[1 3 2]);
    elseif nsubs>1
        N=NSubs*(NTimes-1);
        fseek(fid,4*(tim-1)*NSubs,-1);
        Data=fread(fid,[NSubs NSeg],sprintf('%i*float32',NSubs),4*N);
        Data=Data(subs,:);
    elseif ntim>1
        Data=zeros(NTimes,NSeg);
        for i=tim
            fseek(fid,4*(i-1)*NSubs+4*(subs-1),-1);
            Data(i,:)=fread(fid,[1 NSeg],'float32',4*(NTot-1));
        end
        Data=reshape(Data,1,NTimes,NSeg);
        Data=permute(Data,[1 3 2]);
    else
        fseek(fid,4*(tim-1)*NSubs+4*(subs-1),-1);
        Data=fread(fid,[1 NSeg],'float32',4*(NTot-1));
    end
else
    Data=repmat(NaN,[nsubs nseg ntim]);
    for si=1:nseg
        segi=seg(si);
        fseek(fid,(segi-1)*NTot*4,-1);
        if nsubs==0 | ntim==0
        elseif nsubs>1 & ntim>1
            tmpData=fread(fid,[NSubs NTimes],'float32');
            tmpData=tmpData(subs,tim);
            Data(:,si,:)=reshape(tmpData,[nsubs 1 ntim]);
        elseif nsubs>1
            fseek(fid,4*(tim-1)*NSubs,0);
            tmpData=fread(fid,[NSubs 1],'float32');
            Data(:,si,:)=tmpData(subs,:);
        elseif ntim>0
            fseek(fid,4*(subs-1),0);
            tmpData=fread(fid,[1 NTimes],'float32',4*(NSubs-1));
            tmpData=tmpData(:,tim);
            Data(:,si,:)=reshape(tmpData,[1 1 ntim]);
        else
            fseek(fid,4*(tim-1)*NSubs+4*(subs-1),0);
            Data(:,si,:)=fread(fid,1,'float32');
        end
    end
end
fclose(fid);

if S.TStep~=0
    OTime=S.T0+S.Times(OTime)'*S.TStep;
end;
