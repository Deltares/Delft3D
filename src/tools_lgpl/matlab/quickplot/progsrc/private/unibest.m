function [Out1,Out2]=unibest(cmd,varargin)
%UNIBEST Read Unibest files.
%
%   Struct=UNIBEST('open','FileName')
%   opens the specified Unibest file: combination of .fun and
%   .daf files.
%
%   [Time,Data]=UNIBEST('read',Struct,Location,Quant,TStep)
%   reads the data for the specified location (0 for all),
%   specified quantity (0 for all) and specified time step
%   (0 for all) from the Unibest data file.

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
    case 'open'
        if nargout>1
            error('Too many output arguments.')
        end
        Out1=Local_open(varargin{:});
    case 'read'
        [Out1,Out2]=Local_read(varargin{:});
    otherwise
        error('Unknown command: %s.',cmd)
end


function S=Local_open(filename,datafile)

S.Check='NotOK';
S.FileType='Unibest';

if (nargin==0) || strcmp(filename,'?')
    [fname,fpath]=uigetfile('*.fun','Select Unibest file');
    if ~ischar(fname)
        return
    end
    filename=fullfile(fpath,fname);
end
if length(filename)>3 && isequal(lower(filename(end-3:end)),'.daf')
    filename(end-2:end)='fun';
end
S.FileName=filename;

if ~verifyascii(filename)
    error('Unibest .fun-file should be an ASCII file. Reading unexpected characters.')
end

fid=fopen(filename,'rt','n','US-ASCII');
% skip first 8 lines (backward compatible)
for i=1:8
    dummy=fgetl(fid);
end

% read number of functions
Line=fgetl(fid);
if ~ischar(Line)
    fclose(fid);
    error('End of file before data starts')
end
[nfunc,nread,err,nexti]=sscanf(Line,'%i',1);
if ~isempty(err)
    fclose(fid);
    error('Cannot read line 9:\n%s',Line)
elseif ~isempty(deblank(Line(nexti:end)))
    fclose(fid);
    error('Too many numbers encountered on line 9:\n%s',Line)
end
S.NQuant=nfunc;

%skip the next line
Line=fgetl(fid);
lLine=min(length(Line),4);
if ~isequal(dummy(1:lLine),'****')
    fclose(fid);
    error('Expected separator line starting with ****\n%s',Line)
end

for i=1:nfunc
    Line=fgetl(fid);
    if length(Line)<80
        fclose(fid);
        error('Line %i too short\n%s',i+10,Line)
    end
    S.Quant.ShortName{i}=deblank(Line(1:6));
    LongNameNr=deblank(Line(7:36));
    stri=num2str(i);
    lstri=length(stri);
    if length(LongNameNr)<lstri || ~isequal(LongNameNr(end-lstri+1:end),stri)
        fclose(fid);
        error('Number in line %i does not match %i:\n%s',i+10,i,Line)
    end
    LongName=LongNameNr(1:end-lstri);
    while ~isempty(LongName) && (isequal(LongName(end),'.') || isequal(LongName(end),' '))
        LongName=LongName(1:end-1);
    end
    S.Quant.LongName{i}=LongName;
    %ShortName=deblank(Line(37:42));
    %if ~isequal(ShortName,S.Quant.ShortName{i})
    %   fclose(fid);
    %   error('Non-consistent short name in line %i:\n%s',i+10,Line)
    %end
    unitopen=strfind(Line(38:end),'(')+37;
    unitclose=strfind(Line(38:end),')')+37;
    Units=strtrim(Line(unitopen:unitclose));
    S.Quant.Units{i}=Units(2:end-1);

    %[X,nread]=sscanf(Line(52:end),'%f');
    %if nread~=3
    %   fclose(fid);
    %   error('Unexpected number of values at end of line %i\n%s',i+10,Line)
    %end
end
fclose(fid);
S.Check='OK';

if nargin<2
    datafile=[filename(1:end-3) 'daf'];
end
fid=fopen(datafile,'r','l');
if fid<0
    if nargin>1
        error('Error reading file: %s',datafile)
    end
    return
end
S.DataFile=datafile;
[nx,count]=fread(fid,1,'float32');
if nx~=round(nx) || nx<0
    fclose(fid);
    error('Number of points (%g) not valid.',nx)
end
S.RecSize=nx+1;
Parameters=fread(fid,[1 S.RecSize],'float32');
S.DafVersion=Parameters(1);
switch S.DafVersion
    case 0
        zr=2;
        if all(Parameters(2:4)~=0)
            S.RefDate=datenum(Parameters(2),Parameters(3),Parameters(4));
            zr=5;
        end
    case 1
        S.RefDate=datenum(Parameters(2),Parameters(3),Parameters(4));
        S.XYRef=Parameters(5:6);
        S.Orientation=Parameters(7)*pi/180;
        zr=8;
    otherwise
        if S.DafVersion<1
            error('Invalid version number: %g',S.DafVersion)
        end
        zr=length(Parameters);
end
if ~all(Parameters(zr:end)==0)
    error('Unexpected non-zero values in the first record.')
end
S.X=fread(fid,[1 S.RecSize-1],'float32');
fseek(fid,0,1);
DataFileSize=ftell(fid);
fclose(fid);
S.NTimes=(DataFileSize/4/S.RecSize-2)/nfunc;
if S.NTimes<1
    error('File too small.')
elseif S.NTimes~=round(S.NTimes)
    S.Status='incomplete last record';
    S.NTimes=floor(S.NTimes);
else
    S.Status='all records complete';
end


function [OTime,Data]=Local_read(S,Loc,Qnt,Time)
fid=fopen(S.DataFile,'r','l');
NPnt=S.RecSize-1;
%
%--- quant
if isequal(Loc,0)
    loc=1:NPnt;
else
    loc=Loc;
end
nloc=length(loc);
%
%--- quant
if isequal(Qnt,0)
    qnt=1:S.NQuant;
else
    qnt=Qnt;
end
nqnt=length(qnt);
%
%--- times
if isequal(Time,1:S.NTimes) || isequal(Time,0)
    ntim=S.NTimes;
    tim=0;
else
    tim=Time;
    ntim=length(tim);
end
%
%--- read data
%
if tim==0
    fseek(fid,2*S.RecSize*4,-1);
    OTime=fread(fid,[S.NTimes 1],'float32',4*(S.RecSize*S.NQuant-1));
    if nqnt==0 || nloc==0
        Data=zeros(nloc,nqnt,ntim);
    elseif nqnt>1 && nloc>1
        fseek(fid,(2*S.RecSize+1)*4,-1);
        Data=fread(fid,[NPnt S.NTimes*S.NQuant],sprintf('%i*float32',NPnt),4);
        Data=reshape(Data,NPnt,S.NQuant,S.NTimes);
        Data=Data(loc,qnt,:);
    elseif nqnt>1
        fseek(fid,(2*S.RecSize+loc)*4,-1);
        Data=fread(fid,[S.NQuant S.NTimes],'float32',4*NPnt);
        Data=reshape(Data,1,S.NQuant,S.NTimes);
        Data=Data(1,qnt,:);
    elseif nloc>1
        fseek(fid,((2+qnt-1)*S.RecSize+1)*4,-1);
        Data=fread(fid,[NPnt S.NTimes],sprintf('%i*float32',NPnt),(S.RecSize*(S.NQuant-1)+1)*4);
        Data=reshape(Data,NPnt,1,S.NTimes);
        Data=Data(loc,1,:);
    else
        fseek(fid,((2+qnt-1)*S.RecSize+loc)*4,-1);
        Data=fread(fid,[1 S.NTimes],'float32',(S.RecSize*S.NQuant-1)*4);
        Data=reshape(Data,1,1,S.NTimes);
    end
else
    OTime=repmat(NaN,ntim,1);
    Data=repmat(NaN,[nloc nqnt ntim]);
    for ti=1:ntim
        tim1=tim(ti);
        fseek(fid,(2+(tim1-1)*S.NQuant)*S.RecSize*4,-1);
        OTime(ti)=fread(fid,1,'float32');
        if nqnt==0 || nloc==0
        elseif nqnt>1 && nloc>1
            tmpData=fread(fid,[NPnt S.NQuant],sprintf('%i*float32',NPnt),4);
            Data(:,:,ti)=tmpData(loc,qnt);
        elseif nqnt>1
            fseek(fid,(loc-1)*4,0);
            tmpData=fread(fid,[1 S.NQuant],'float32',4*NPnt);
            Data(:,:,ti)=tmpData(1,qnt);
        elseif nloc>1
            fseek(fid,(qnt-1)*S.RecSize*4,0);
            tmpData=fread(fid,[NPnt 1],'float32');
            Data(:,:,ti)=tmpData(loc,1);
        else
            fseek(fid,((qnt-1)*S.RecSize+loc-1)*4,0);
            tmpData=fread(fid,1,'float32');
            Data(:,:,ti)=tmpData;
        end
    end
end
fclose(fid);
%
% Correction for single precision: round to nearest minute (for Unibest
% rounding to the nearest hour would be the same).
%
OTime=round(OTime*24*60)/24/60;


function isASCII = verifyascii(arg)
fid = fopen(arg,'r');
S = fread(fid,[1 100],'uint8');
fclose(fid);
isASCII = ~any(S~=9 & S~=10 & S~=13 & S<32); % TAB,LF,CR allowed