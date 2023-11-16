function Out=telemac(cmd,varargin)
%TELEMAC Read Telemac selafin files.
%   F = TELEMAC('open',FileName)
%   Opens the file and returns structure containing file
%   information.
%
%   Data = TELEMAC('read',F,TimeIndex,VarNr,PntNrs)
%   Read data from file for specified time indices, variable
%   indices and point numbers.

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
        Out=telemac_open(varargin{:});
    case 'read'
        Out=telemac_read(varargin{:});
    otherwise
        error('Unknown command: %s.',cmd)
end


function Struct=telemac_open(filename)
Struct.Check='NotOK';
Struct.FileType='Serafin';

if (nargin==0) || strcmp(filename,'?')
    [fname,fpath]=uigetfile('*.slf','Select Telemac file');
    if ~ischar(fname)
        return
    end
    filename=fullfile(fpath,fname);
end

Struct.FileName   = filename;
Struct.Endianness = 'b';
fid=fopen(Struct.FileName,'r','b','US-ASCII');
if fid<0
    return
end
NBytes=fread(fid,[1 1],'int32');
if NBytes==80
    fseek(fid,0,-1);
else
    fclose(fid);
    Struct.Endianness = 'l';
    fid=fopen(Struct.FileName,'r','l','US-ASCII');
end

% 1 record containing the title of the study (72 characters) and a 8
% characters string indicating the type of format (SERAFIN or SERAFIND)
% [NOTE: the SERAFIN/SERAFIND keyword isn't there in general]
Struct.Title=char(fortranread(fid,[40 2],'uchar')');
if strcmpi(Struct.Title(2,40),'D')
    Struct.Precision = 'float64';
    Struct.PrecBytes = 8;
else
    Struct.Precision = 'float32';
    Struct.PrecBytes = 4;
end

% 1 record containing the two integers NBV(1) and NBV(2) (number of linear
% and quadratic variables, NBV(2) with the value of 0 for Telemac, as
% quadratic values are not saved so far)
Struct.NVar=fortranread(fid,[1 2],'int32');
NVar1=Struct.NVar(1);
NVar2=Struct.NVar(2);
NVar=NVar1+NVar2;

for i=1:NVar
    X=char(fortranread(fid,[16 2],'uchar')');
    Struct.Var(i).Name = deblank(X(1,:));
    Struct.Var(i).Unit = deblank(X(2,:));
    Struct.Var(i).Type = 1 + (i>NVar1);
end

%if IPARAM (3) ? 0: the value corresponds to the x-coordinate of the origin
%                   of the mesh,
%if IPARAM (4) ? 0: the value corresponds to the y-coordinate of the origin
%                   of the mesh,
%if IPARAM (7) ? 0: the value corresponds to the number of planes on the
%                   vertical (3D computation),
%if IPARAM (8) ? 0: the value corresponds to the number of boundary points
%                   (in parallel),
%if IPARAM (9) ? 0: the value corresponds to the number of interface points
%                   (in parallel),
%if IPARAM(8) or IPARAM(9)?0: the array IPOBO below is replaced by the
%                             array KNOLG (total initial number of points).
%                             All the other numbers are local to the
%                             sub-domain, including IKLE.
%if IPARAM (10) = 1: a record containing the computation starting date,
Struct.IParam=fortranread(fid,[1 10],'int32');
if Struct.IParam(10)
    Time=fortranread(fid,[1 6],'int32');
    Struct.RefTime=datenum(Time(1),Time(2),Time(3),Time(4),Time(5),Time(6));
end

%
% Size of grid
% 1 record containing the integers NELEM,NPOIN,NDP,1 (number of elements,
% number of points, number of points per element and the value 1),
X=fortranread(fid,[1 4],'int32');
Struct.Discr(1).NElem=X(1);
Struct.Discr(1).NPnts=X(2);
Struct.Discr(1).NPntsPerElem=X(3);
Struct.Discr(1).Type=X(4); % 1P1 (or 3P2)
if NVar2>0
    X=fortranread(fid,[1 4],'int32');
    Struct.Discr(2).NElem=X(1);
    Struct.Discr(2).NPnts=X(2);
    Struct.Discr(2).NPntsPerElem=X(3);
    Struct.Discr(2).Type=X(4); % 1P1 (or 3P2)
end

%
% Element definition: reference to corner points in global index array
% 1 record containing table IKLE (integer array of dimension (NDP,NELEM)
% which is the connectivity table. Attention: in TELEMAC-2D, the dimensions
% of this array are (NELEM,NDP)),
Struct.Discr(1).Elem=fortranread(fid,[Struct.Discr(1).NPntsPerElem Struct.Discr(1).NElem],'int32')';
if NVar2>0
    Struct.Discr(2).Elem=fortranread(fid,[Struct.Discr(2).NPntsPerElem Struct.Discr(2).NElem],'int32')';
end

%
% Boundary definition: for each point index in array of boundary points (0
% if the point is not a boundary point).
% 1 record containing table IPOBO (integer array of dimension NPOIN); the
% value of one element is 0 for an internal point, and gives the numbering
% of boundary points for the others,
Struct.Discr(1).BoundPnt=fortranread(fid,[Struct.Discr(1).NPnts 1],'int32');
if NVar2>0
    Struct.Discr(2).BoundPnt=fortranread(fid,[Struct.Discr(2).NPnts 1],'int32');
end

%
% X co-ordinates.
% 1 record containing table X (real array of dimension NPOIN containing the
% abscissae of the points),
Struct.Discr(1).X=fortranread(fid,[Struct.Discr(1).NPnts 1],Struct.Precision);
if NVar2>0
    Struct.Discr(2).X=fortranread(fid,[Struct.Discr(2).NPnts 1],Struct.Precision);
end

%
% Y co-ordinates.
% 1 record containing table Y (real array of dimension NPOIN containing the
% ordinates of the points),
Struct.Discr(1).Y=fortranread(fid,[Struct.Discr(1).NPnts 1],Struct.Precision);
if NVar2>0
    Struct.Discr(2).Y=fortranread(fid,[Struct.Discr(2).NPnts 1],Struct.Precision);
end

Struct.Offset=ftell(fid);
Struct.RecordSize=Struct.PrecBytes+2*4+NVar1*(Struct.Discr(1).NPnts*Struct.PrecBytes+2*4);
if NVar2>0
    Struct.RecordSize=Struct.RecordSize+NVar2*(Struct.Discr(2).NPnts*Struct.PrecBytes+2*4);
end

% Struct.IParam(1)==0 one block
% Struct.IParam(1)==1 multiple blocks
fread(fid,[1 1],'int32');
Struct.Times=fread(fid,[1 inf],Struct.Precision,Struct.RecordSize-Struct.PrecBytes)/3600/24;
Struct.NTimes=length(Struct.Times);

fseek(fid,0,1);
FileSize=ftell(fid);
RNTimes=(FileSize-Struct.Offset)/Struct.RecordSize;
if Struct.NTimes~=RNTimes
    warning('%i time steps read, %g time steps according to file size.',Struct.NTimes,RNTimes)
end

fclose(fid);
Struct.Check='OK';


function clidata = telemac_opencli(filename)
fid=fopen(filename,'r','n','US-ASCII');
if fid<0
    clidata = [];
    return
end
clidata = fscanf(fid,'%f',[13 inf]);
% LIHBOR, LIUBOR, LIVBOR, HBOR, UBOR, VBOR, AUBOR, LITBOR, TBOR, ATBOR, BTBOR, N, K
% LIHBOR: depth boundary type codes
%         1. incident wave
%         2. closed boundary (wall)
%         4. free depth
%         5. prescribed depth
% LIUBOR: flowrate/velocity boundary type codes
% LIVBOR: flowrate/velocity boundary type codes
%         0. closed boundary with one or two nil velocity components
%         1. incident wave
%         2. slip or friction
%         4. free velocity
%         5. prescribed flow rate
%         6. prescribed velocity
% HBOR  : prescribed depth if LIHBOR=5
% UBOR  : prescribed u velocity if LIUBOR=6
% VBOR  : prescribed v velocity if LIVBOR=6
% AUBOR : friction coefficient at boundary in case LIUBOR or LIVBOR=2
%         (du/dn = aubor * u, dv/dn = aubor * v)
% LITBOR: tracer boundary type codes
%         2. closed boundary (wall)
%         4. free tracer
%         5. prescribed tracer
% TBOR  : prescribed tracer concentation if LITBOR=5
% ATBOR : coefficient in dT/dn = atbor*t + btbor
% BTBOR : coefficient in dT/dn = atbor*t + btbor
% N     : global index of boundary point
% K     : boundary clour number
fclose(fid);

function Data=telemac_read(Struct,time,var,pnts)
if any(time>Struct.NTimes)
    error('Time step number too large.')
end
fid=fopen(Struct.FileName,'r',Struct.Endianness,'US-ASCII');
if fid<0
    error('Cannot open data file.')
end
if nargin<4
    pnts=1:Struct.Discr.NPnts;
end
Data=zeros(length(time),length(pnts));
for t=1:length(time)
    Offset = Struct.Offset + Struct.RecordSize*(time(t)-1);
    if Struct.IParam(1)==0
    elseif Struct.IParam(1)==1
        if var<=Struct.NVar(1)
            Offset = Offset+Struct.PrecBytes+2*4+(var-1)*(Struct.Discr(1).NPnts*Struct.PrecBytes+2*4);
        elseif var<=Struct.NVar(1)+Struct.NVar(2)
            Offset = Offset+Struct.PrecBytes+2*4+Struct.NVar(1)*(Struct.Discr(1).NPnts*Struct.PrecBytes+2*4)+(var-Struct.NVar(1)-1)*(Struct.Discr(2).NPnts*Struct.PrecBytes+2*4);
        else
            fclose(fid);
            error('Variable number too large.')
        end
        fseek(fid,Offset,-1);
        DataTmp=fortranread(fid,[1 Struct.Discr(1).NPnts],Struct.Precision);
        Data(t,:)=DataTmp(1,pnts);
    end
end
fclose(fid);


function Data=fortranread(fid,size,type)
switch type
    case 'int32'
        NBytesPE=4;
    case 'float32'
        NBytesPE=4;
    case 'float64'
        NBytesPE=8;
    case 'uchar'
        NBytesPE=1;
    otherwise
        error('Undefined type: %s.',type)
end
NBytesExpected=prod(size)*NBytesPE;
NBytes=fread(fid,[1 1],'int32');
if ~isequal(NBytes,NBytesExpected)
    fclose(fid);
    error('Unexpected size at beginning of record: %i (expected: %i)',NBytes,NBytesExpected)
end
Data=fread(fid,size,type);
NBytes=fread(fid,[1 1],'int32');
if ~isequal(NBytes,NBytesExpected)
    fclose(fid);
    error('Unexpected size at end of record: %i (expected: %i)',NBytes,NBytesExpected)
end
