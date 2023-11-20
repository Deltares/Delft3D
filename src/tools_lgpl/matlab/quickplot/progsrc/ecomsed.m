function Out = ecomsed(cmd,varargin)
%ECOMSED Read an ECOMSED data file.
%   S = ECOMSED('open',FILENAME) opens a ECOMSED data file, scans the
%   contents of the file and returns a structure containing all information
%   necessary to read data from the file. Supported files include 
%   * GCMPLT     files containing time history data for all grid elements, 
%   * GCMTSR     files containing time history data for selected grid elements.
%   * MODEL_GRID files containing physical information for model_grid.
%
%   D = ECOMSED('read',S,QUANT,TIMESTEP) reads data of selected quantity
%   from the file specified using the structure S obtained from an ECOMSED
%   open call. Reads only the data for the specified time step(s); defaults
%   to all time steps.
%
%   D = ECOMSED('read',S,QUANT,TIMESTEP,INDEX1,INDEX2,...) reads only the
%   data with the specified indices. It is like reading all data and
%   subsequently selecting a submatrix: D(TIMESTEP,INDEX1,INDEX2,...).
%
%   Example
%      % open data file
%      S = ecomsed('open','gcmplt.0030');
%      % read water level for third time step
%      WL = ecomsed('read',S,'ARCET',3);
%
% Note: in the GCMPLT file (not in the GCMTSR file) the scalar variables 
%       (e.g. T,S) are written in a matrix with one extra dummy layer. They have 
%       KB layers, where KB is the number of interfaces, while the scalars are 
%       defined in only KB-1 sigma layers. The extra dummy layer KB does not 
%       contain zeros, but values very similar (but not exactly identical) to the 
%       values in the last real layer KB-1.
%
%See web : <a href="http://www.hydroqual.com/ehst_ecomsed.html">www.hydroqual.com/ehst_ecomsed.html</a>
%See also: ECOMSED_INP, ECOMSED_VECTOR_CEN, 

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
        Out = open_ecomsed(varargin{:});
    case 'read'
        Out = read_ecomsed(varargin{:});
    otherwise
        error('Invalid command option: %s',cmd)
end


%% Open a data file
function S = open_ecomsed(FileName)
S.FileName = FileName;
S.FileType = 'ecomsed-binary';
% Initial guess: assume a file created on a UNIX system
fid = fopen(FileName,'r','b','US-ASCII');
S.ByteOrder = 'b';
S = process_file(fid,S);

function S = process_file(fid,S)
%% Determine File Type
% GCMPRT: ?? (formatted)
% GCMPLT: IM, JM, KB (unformatted)
% GCMTSR: TOR, TRACER, SEDTRAN, CHEMTRAN (unformatted)
% PART_LOCATION: LL, NN, NPART, TIME (formatted)
% GCM_TRAN: TMIDDLE (unformatted)
% GCM_GEOM: DZ, DZZ (unformatted)

BlockSize = fread(fid,1,'uint32');
fseek(fid,0,-1);
switch BlockSize
    case 12
        S.SubType = 'GCMPLT';
        S = open_gcmplt(fid,S);
        fclose(fid);
    case 31
        S.SubType = 'GCMTSR';
        S = open_gcmtsr(fid,S);
        fclose(fid);
    case 4
        S.SubType = 'GCMTRAN';
        S = open_gcmtran(fid,S);
        fclose(fid);
    case 542008695 % ' New Refined Grid'
        S.SubType = 'MODEL_GRID';
        S = open_grid(fid,S);
        fclose(fid);
    otherwise
        fclose(fid);
        if strcmp(S.ByteOrder,'b')
            % Can't understand the file if it is big-endian. Before giving
            % up, try opening the file as little-endian.
            fid = fopen(S.FileName,'r','l','US-ASCII');
            S.ByteOrder = 'l';
            S = process_file(fid,S);
        else
            % Neither big- nor little-endian byte order worked.
            error('Unexpected first block size (%i) for ECOMSED output file',BlockSize)
        end
end
S.Check = 'OK';


%% Open a grid file
function S = open_grid(fid,S)
fgetl(fid); % New Refined Grid

fgetl(fid); % Sigma Levels Z=0  to -1.0  11 levels 
nLayerInterfaces = fscanf(fid,'%i',1);
S.sigma = fscanf(fid,'%f',nLayerInterfaces);
fgetl(fid);

fgetl(fid); %  Horizontal Segmentations
gridSize = fscanf(fid,'%i',[1 2]);
fgetl(fid);
gridStart = ftell(fid);
gridLine = fgetl(fid);
values = sscanf(gridLine,'%f');
fseek(fid,gridStart,-1);
switch length(values)
   case 7
      %I,J,H1(I,J),H2(I,J),H(I,J),ANG(I,J),COR(I,J)
      gridInfo = fscanf(fid,'%i %i %f %f %f %f %f',[7 inf]);
      Y = gridInfo(7,:);
      X = [];
   case 8
      %I,J,H1(I,J),H2(I,J),H(I,J),ANG(I,J),COR(I,J),DATUM(I,J)
      gridInfo = fscanf(fid,'%i %i %f %f %f %f %f %f',[8 inf]);
      Y = gridInfo(7,:);
      X = [];
   case 9
      %I,J,H1(I,J),H2(I,J),H(I,J),ANG(I,J),YGRID(I,J),XGRID(I,J),DATUM(I,J)
      gridInfo = fscanf(fid,'%i %i %f %f %f %f %f %f %f',[9 inf]);
      Y = gridInfo(7,:);
      X = gridInfo(8,:);
      S.Datum = gridInfo(9,:);
end
I     = gridInfo(1,:);
J     = gridInfo(2,:);
dI    = gridInfo(3,:);
dJ    = gridInfo(4,:);
S.H   = gridInfo(5,:);
Alpha = gridInfo(6,:);

fgetl(fid); % THIN DAM
nDams = fscanf(fid,'%i',1);
damInfo = fscanf(fid,'%i %i %cDIR %i',[4 nDams]);

S.X = repmat(NaN,gridSize);
S.Y = repmat(NaN,gridSize);

Idx = I + gridSize(1)*(J-1);
Alpha = Alpha*pi/180;

T.gridSize = size(S.X);
T.dxI = repmat(NaN,T.gridSize);
T.dxI(Idx) = dI.*cos(Alpha)/2;
T.dyI = repmat(NaN,T.gridSize);
T.dyI(Idx) = dI.*sin(Alpha)/2;
T.dxJ = repmat(NaN,T.gridSize);
T.dxJ(Idx) = dJ.*cos(Alpha+pi/2)/2;
T.dyJ = repmat(NaN,T.gridSize);
T.dyJ(Idx) = dJ.*sin(Alpha+pi/2)/2;

if ~isempty(X)
   S.X(Idx) = X;
   S.Y(Idx) = Y;
else
   S = reconstruct_centre(S,T,Idx);
end
S = centre2corner(S,T);

function G = reconstruct_centre(G,T,Idx)
Xcc = G.X;
Ycc = G.Y;
i_todo = 1:length(Idx);
while ~isempty(i_todo)
   i_mask = repmat(logical(0),size(i_todo));
   for t = 1:length(i_todo)
      i = i_todo(t);
      idx = Idx(i)+[-1 -T.gridSize(1) +1 +T.gridSize(1)];
      if i==1
         i_mask(t) = 1;
         Xcc(Idx(i)) = 0;
         Ycc(Idx(i)) = 0;
      elseif ~all(isnan(Xcc(idx)))
         i_mask(t) = 1;
         DXI = T.dxI(Idx(i));
         DYI = T.dyI(Idx(i));
         DXJ = T.dxJ(Idx(i));
         DYJ = T.dyJ(Idx(i));
         x = repmat(NaN,1,4);
         y = repmat(NaN,1,4);
         if ~isnan(Xcc(idx(1)))
            x(1) = Xcc(idx(1)) + T.dxI(idx(1)) + DXI;
            y(1) = Ycc(idx(1)) + T.dyI(idx(1)) + DYI;
         end
         if ~isnan(Xcc(idx(2)))
            x(2) = Xcc(idx(2)) + T.dxJ(idx(2)) + DXJ;
            y(2) = Ycc(idx(2)) + T.dyJ(idx(2)) + DYJ;
         end
         if ~isnan(Xcc(idx(3)))
            x(3) = Xcc(idx(3)) - T.dxI(idx(3)) - DXI;
            y(3) = Ycc(idx(3)) - T.dyI(idx(3)) - DYI;
         end
         if ~isnan(Xcc(idx(4)))
            x(4) = Xcc(idx(4)) - T.dxJ(idx(4)) - DXJ;
            y(4) = Ycc(idx(4)) - T.dyJ(idx(4)) - DYJ;
         end
         Xcc(Idx(i)) = mean(x(~isnan(x)));
         Ycc(Idx(i)) = mean(y(~isnan(y)));
      end
   end
   if any(i_mask)
      i_todo = i_todo(~i_mask);
   else
      break
   end
end
G.X = Xcc;
G.Y = Ycc;

function G = centre2corner(G,T)
Id = [1 1:T.gridSize(1)-1];
Jd = [1 1:T.gridSize(2)-1];

Xco = cat(3, ...
    G.X-T.dxI-T.dxJ, ...
    G.X(Id,: )+T.dxI(Id,: )-T.dxJ(Id,: ), ...
    G.X(Id,Jd)+T.dxI(Id,Jd)+T.dxJ(Id,Jd), ...
    G.X(:,Jd )-T.dxI(: ,Jd)+T.dxJ(:,Jd));
Yco = cat(3, ...
    G.Y-T.dyI-T.dyJ, ...
    G.Y(Id,: )+T.dyI(Id,: )-T.dyJ(Id,: ), ...
    G.Y(Id,Jd)+T.dyI(Id,Jd)+T.dyJ(Id,Jd), ...
    G.Y(:,Jd )-T.dyI(: ,Jd)+T.dyJ(: ,Jd));

Nvalid = sum(~isnan(Yco),3);
Ndiv = max(Nvalid,1);
Xco(isnan(Xco)) = 0;
Yco(isnan(Yco)) = 0;
G.X = sum(Xco,3)./Ndiv;
G.Y = sum(Yco,3)./Ndiv;
G.X(Nvalid==0) = NaN;
G.Y(Nvalid==0) = NaN;


%% Open a GCMPLT file
function S = open_gcmplt(fid,S)
%% Dimensions
sz = 3;
nm = 'IM,JM,KB';
sizecheck1(fid,nm,sz,4)
S.IM = fread(fid,1,'uint32'); % total number of grid elements in the xi_1 direction
S.JM = fread(fid,1,'uint32'); % total number of grid elements in the xi_2 direction
S.KB = fread(fid,1,'uint32'); % number of sigma levels
sizecheck2(fid,nm,sz,4)
sz2d = [S.IM S.JM];
sz3d = [S.IM S.JM S.KB];

%% Boundaries and bed model
sz = 3;
nm = 'EBCM,QBCM,NCHEMLAY';
sizecheck1(fid,nm,sz,4)
S.EBCM     = fread(fid,1,'uint32'); % maximum number of elevation boundary grid elements
S.QBCM     = fread(fid,1,'uint32'); % maximum number of discharge boundary grid elements
S.NCHEMLAY = fread(fid,1,'uint32'); % number of layers in sediment-bound tracer bed model
sizecheck2(fid,nm,sz,4)

%% Various constants
sz = 3*4+10+3*7;
nm = 'DTI,GRAV,UMOL,TOR,...';
sizecheck1(fid,nm,sz,1)
S.DTI      = fread(fid, 1,'float32'); % time step of the internal mode (sec)
S.GRAV     = fread(fid, 1,'float32'); % gravitational acceleration (m2/sec)
S.UMOL     = fread(fid, 1,'float32'); % constant or background mixing (m2/sec)
S.TOR      = fread(fid,10,'10*uchar=>char')'; % type of run ("BAROTROPIC"/"PROGNOSTIC"/"DIAGNOSTIC")
S.TRACER   = fread(fid, 7,'7*uchar=>char' )'; % control parameter for dissolved tracer transport
S.SEDTRAN  = fread(fid, 7,'7*uchar=>char' )'; % control parameter for sediment transport
S.CHEMTRAN = fread(fid, 7,'7*uchar=>char' )'; % control parameter for sediment-bound tracer transport
sizecheck2(fid,nm,sz,1)

%% Number of elevation boundary elements
S.NUMEBC = checkedread(fid,'NUMEBC',1,'uint32'); % total number of elevation boundary grid elements

%% Indices of elevation boundary elements
sz = [4 S.NUMEBC];
nm = 'IETA,JETA,ICON,JCON';
sizecheck1(fid,nm,sz,4)
S.IETA = zeros(1,S.NUMEBC);
S.JETA = zeros(1,S.NUMEBC);
S.ICON = zeros(1,S.NUMEBC);
S.JCON = zeros(1,S.NUMEBC);
for i = 1:S.NUMEBC
    S.IETA(i) = fread(fid,1,'uint32'); % i number of grid element where elevation is specified
    S.JETA(i) = fread(fid,1,'uint32'); % j number of grid element where elevation is specified
    S.ICON(i) = fread(fid,1,'uint32'); % i number of connecting grid element (nearest interior non-boundary grid element)
    S.JCON(i) = fread(fid,1,'uint32'); % j number of connecting grid element (nearest interior non-boundary grid element)
end
sizecheck2(fid,nm,sz,4)

%% Number of discharge boundary elements
S.NUMQBC = checkedread(fid,'NUMQBC',1,'uint32'); % total number of discharge boundary grid elements

%% Indices of discharge boundary elements
sz = [2 S.NUMQBC];
nm = 'IQC,JQC';
sizecheck1(fid,nm,sz,4)
S.IQC = zeros(1,S.NUMQBC);
S.JQC = zeros(1,S.NUMQBC);
for i = 1:S.NUMQBC
    S.IQC(i) = fread(fid,1,'uint32');
    S.JQC(i) = fread(fid,1,'uint32');
end
sizecheck2(fid,nm,sz,4)

%% Depth
S.H = checkedread(fid,'H',sz2d,'float32'); % average depth of grid element (m)

%% Grid distances
S.DX1 = checkedread(fid,'H1',sz2d,'float32'); % (H1) distance in the xi_1 direction at the center of the grid (m)
S.DX2 = checkedread(fid,'H2',sz2d,'float32'); % (H2) distance in the xi_2 direction at the center of the grid (m)

%% Grid angle
S.ANG = checkedread(fid,'ANG',sz2d,'float32'); % angle between east and the xi_1 direction measured in a counter-clockwise direction (deg)

%% Grid masks
S.DUM = checkedread(fid,'DUM',sz2d,'float32'); % land/water mask at the U interface of the grid element
S.DVM = checkedread(fid,'DVM',sz2d,'float32'); % land/water mask at the V interface of the grid element
S.FSM = checkedread(fid,'FSM',sz2d,'float32'); % land/water mask at the center of the grid element

%% Begin of time dependent data
S.OffsetTimeDepData = ftell(fid);

%% Compute offsets within time dependent data blocks
S.TimeDepData.BlockSize = 0;
S.TimeDepData         = additem(S.TimeDepData,'TMIDDLE' ,1   ,'float32'); % time at the middle of the time interval (days)
S.TimeDepData         = additem(S.TimeDepData,'ARCET'   ,sz2d,'float32'); % free surface elevation of the grid element (m)
if strcmp(S.TOR,'BAROTROPIC')
    S.TimeDepData     = additem(S.TimeDepData,'ARCU'    ,sz2d,'float32'); % velocity component in the xi_1 direction (m/sec)
    S.TimeDepData     = additem(S.TimeDepData,'ARCV'    ,sz2d,'float32'); % velocity component in the xi_2 direction (m/sec)
    S.TimeDepData     = additem(S.TimeDepData,'ARCUX'   ,sz2d,'float32'); % transport component in the xi_1 direction (m2/sec)
    S.TimeDepData     = additem(S.TimeDepData,'ARCVX'   ,sz2d,'float32'); % transport component in the xi_2 direction (m2/sec)
    if strcmp(S.TRACER,'INCLUDE')
        S.TimeDepData = additem(S.TimeDepData,'ARCC'    ,sz2d,'float32'); % conservative tracer concentration
    end
    if strcmp(S.SEDTRAN,'INCLUDE')
        S.TimeDepData = additem(S.TimeDepData,'ARCSED1' ,sz2d,'float32'); % cohesive sediment concentration (mg/l)
        S.TimeDepData = additem(S.TimeDepData,'ARCSED2' ,sz2d,'float32'); % non-cohesive sediment concentration (mg/l)
        S.TimeDepData = additem(S.TimeDepData,'ARCTHIK' ,sz2d,'float32'); % sediment bed elevation change (cm)
    end
    if strcmp(S.CHEMTRAN,'INCLUDE')
        S.TimeDepData = additem(S.TimeDepData,'ARCCHEM1',sz2d,'float32'); % cohesive sediment-bound tracer concentration (ug/l)
        S.TimeDepData = additem(S.TimeDepData,'ARCCHEM2',sz2d,'float32'); % non-cohesive sediment-bound tracer concentration (ug/l)
        sz = [S.IM S.JM S.NCHEMLAY];
        S.TimeDepData = additem(S.TimeDepData,'ARCPBED' ,sz  ,'float32'); % sediment bed concentration of sediment-bound tracer (ppm)
    end
else
    sz = S.KB;
    S.TimeDepData     = additem(S.TimeDepData,'Z'       ,sz  ,'float32'); % depth of the interface between sigma levels; 0.0 at the surface; -1.0 at the bottom
    S.TimeDepData     = additem(S.TimeDepData,'ZZ'      ,sz  ,'float32'); % intermediate depth between sigma levels
    S.TimeDepData     = additem(S.TimeDepData,'DZ'      ,sz  ,'float32'); % thickness of the sigma level; Z(K) - Z(K+1)
    S.TimeDepData     = additem(S.TimeDepData,'ARCU'    ,sz3d,'float32'); % velocity component in the xi_1 direction (m/sec)
    S.TimeDepData     = additem(S.TimeDepData,'ARCV'    ,sz3d,'float32'); % velocity component in the xi_2 direction (m/sec)
    S.TimeDepData     = additem(S.TimeDepData,'ARCUX'   ,sz3d,'float32'); % transport component in the xi_1 direction (m2/sec)
    S.TimeDepData     = additem(S.TimeDepData,'ARCVX'   ,sz3d,'float32'); % transport component in the xi_2 direction (m2/sec)
    S.TimeDepData     = additem(S.TimeDepData,'ARCT'    ,sz3d,'float32'); % temperature of the grid element (oC)
    S.TimeDepData     = additem(S.TimeDepData,'ARCS'    ,sz3d,'float32'); % salinity of the grid element (psu)
    S.TimeDepData     = additem(S.TimeDepData,'ARCKH'   ,sz3d,'float32'); % vertical velocity (m/sec)
    S.TimeDepData     = additem(S.TimeDepData,'ARCKM'   ,sz3d,'float32'); % vertical eddy diffusivity (m2/sec)
    if strcmp(S.TRACER,'INCLUDE')
        S.TimeDepData = additem(S.TimeDepData,'ARCC'    ,sz3d,'float32'); % conservative tracer concentration
    end
    if strcmp(S.SEDTRAN,'INCLUDE')
        S.TimeDepData = additem(S.TimeDepData,'ARCSED1' ,sz3d,'float32'); % cohesive sediment concentration (mg/l)
        S.TimeDepData = additem(S.TimeDepData,'ARCSED2' ,sz3d,'float32'); % non-cohesive sediment concentration (mg/l)
        S.TimeDepData = additem(S.TimeDepData,'ARCTHIK' ,sz2d,'float32'); % sediment bed elevation change (cm)
        S.TimeDepData = additem(S.TimeDepData,'ARCTAU'  ,sz3d,'float32'); % 3D!!! bottom shear stress for use in sediment transport (dynes/cm2)
    end
    if strcmp(S.CHEMTRAN,'INCLUDE')
        S.TimeDepData = additem(S.TimeDepData,'ARCCHEM1',sz3d,'float32'); % cohesive sediment-bound tracer concentration (ug/l)
        S.TimeDepData = additem(S.TimeDepData,'ARCCHEM2',sz3d,'float32'); % non-cohesive sediment-bound tracer concentration (ug/l)
        sz = [S.IM S.JM S.NCHEMLAY];
        S.TimeDepData = additem(S.TimeDepData,'ARCPBED' ,sz  ,'float32'); % sediment bed concentration of sediment-bound tracer (ppm)
    end
end

%% Read times
sizecheck1(fid,'TMIDDLE',1,4)
S.Times = fread(fid,inf,'float32',S.TimeDepData.BlockSize-4);

%% Open a GCMTSR file
function S = open_gcmtsr(fid,S)
%% Various constants
sz = 10+3*7;
nm = 'TOR,TRACER,SEDTRAN,CHEMTRAN';
sizecheck1(fid,nm,sz,1)
S.TOR      = fread(fid,10,'10*uchar=>char')'; % type of run ("BAROTROPIC"/"PROGNOSTIC"/"DIAGNOSTIC")
S.TRACER   = fread(fid, 7, '7*uchar=>char')'; % control parameter for dissolved tracer transport
S.SEDTRAN  = fread(fid, 7, '7*uchar=>char')'; % control parameter for sediment transport
S.CHEMTRAN = fread(fid, 7, '7*uchar=>char')'; % control parameter for sediment-bound tracer transport
sizecheck2(fid,nm,sz,1)

%% Some dimensions
sz = 2;
nm = 'KBM1,NCHEMLAY';
sizecheck1(fid,nm,sz,4)
S.KBM1 = fread(fid,1,'uint32')'; % number of sigma layers (KB-1)
S.NCHEMLAY = fread(fid,1,'uint32'); % number of layers in sediment-bound tracer bed model
sizecheck2(fid,nm,sz,4)

%% Number of elements with elevation
S.EPTS = checkedread(fid,'EPTS',1,'uint32'); % number of grid elements with elevation time series

%% Indices of grid elements with elevation
sz = [2 S.EPTS];
nm = 'INXIE,INXJE';
sizecheck1(fid,nm,sz,4)
S.INXIE = zeros(1,S.EPTS);
S.INXJE = zeros(1,S.EPTS);
for i = 1:S.EPTS
    S.INXIE(i) = fread(fid,1,'uint32'); % i number of user specified grid element
    S.INXJE(i) = fread(fid,1,'uint32'); % j number of user specified grid element
end
sizecheck2(fid,nm,sz,4)

%% Number of elements with current
S.VPTS = checkedread(fid,'VPTS',1,'uint32'); % number of grid elements with current time series

%% Indices of grid elements with current
sz = [2 S.VPTS];
nm = 'INXIV,INXJV';
sizecheck1(fid,nm,sz,4)
S.INXIV = zeros(1,S.VPTS);
S.INXJV = zeros(1,S.VPTS);
for i = 1:S.VPTS
    S.INXIV(i) = fread(fid,1,'uint32'); % i number of user specified grid element
    S.INXJV(i) = fread(fid,1,'uint32'); % j number of user specified grid element
end
sizecheck2(fid,nm,sz,4)

%% Grid angle of current elements
S.ANG = checkedread(fid,'ANG',[1 S.VPTS],'float32'); % angle in degrees between east and xi_1 direction measured in a counter-clockwise direction

%% Number of elements with flux
S.FPTS = checkedread(fid,'FPTS',1,'uint32'); % number of user specified grid elements with cross sectional flux time series

%% Indices of grid elements with current
sz = [4 S.FPTS];
nm = 'ISFLX,JSFLX,DIRFLX,NFLXE';
sizecheck1(fid,nm,sz,4)
S.ISFLX  = zeros(1,S.FPTS);
S.JSFLX  = zeros(1,S.FPTS);
S.DIRFLX = repmat(char(32),[S.FPTS 4]);
S.NFLXE  = zeros(1,S.FPTS);
for i = 1:S.FPTS
    S.ISFLX(i) = fread(fid,1,'uint32'); % i number of user specified grid element in which cross section begins
    S.JSFLX(i) = fread(fid,1,'uint32'); % j number of user specified grid element in which cross section begins
    S.DIRFLX(i,:) = fread(fid,4,'4*uchar=>char')'; % direction of the cross section; = "IDIR" - cross section in the xi_1 direction; = "JDIR" - cross section in the xi_2 direction
    S.NFLXE(i) = fread(fid,1,'uint32'); % number of grid elements in the cross section
end
sizecheck2(fid,nm,sz,4)

%% Begin of time dependent data
S.OffsetTimeDepData = ftell(fid);

%% Compute offsets within time dependent data blocks
S.TimeDepData.BlockSize = 0;
S.TimeDepData         = additem(S.TimeDepData,'TMIDDLE'  ,1                    ,'float32'); % time at the middle of the time interval (days)
S.TimeDepData         = additem(S.TimeDepData,'ESAVE'    ,[1 S.EPTS]           ,'float32'); % surface elevation of the user specified grid element (m)
if strcmp(S.TOR,'BAROTROPIC')					               
    S.TimeDepData     = additem(S.TimeDepData,'UVZSAVE'  ,[2 S.VPTS]           ,'float32'); % U/V velocity averaged at the center of the user specified grid element (m/sec)
    if strcmp(S.TRACER,'INCLUDE')				               
        S.TimeDepData = additem(S.TimeDepData,'C1ZSAVE'  ,[1 S.VPTS]           ,'float32'); % conservative tracer concentration
    end								               
    if strcmp(S.SEDTRAN,'INCLUDE')				               
        S.TimeDepData = additem(S.TimeDepData,'CSAVE'    ,[2 S.VPTS]           ,'float32'); % cohesive and non-cohesive sediment concentration (mg/l)
        S.TimeDepData = additem(S.TimeDepData,'THSAVE'   ,[1 S.VPTS]           ,'float32'); % sediment bed elevation change (cm)
        S.TimeDepData = additem(S.TimeDepData,'TAUSAVE'  ,[1 S.VPTS]           ,'float32'); % bottom shear stress for use in sediment transport (dynes/cm2)
    end								               
    if strcmp(S.CHEMTRAN,'INCLUDE')				               
        S.TimeDepData = additem(S.TimeDepData,'PSAVE'    ,[2 S.VPTS]           ,'float32'); % cohesive and non-cohesive sediment-bound tracer concentration (ug/l)
        S.TimeDepData = additem(S.TimeDepData,'PBEDSAVE' ,[1 S.VPTSS.NCHEMLAY] ,'float32'); % sediment bed concentration of sediment-bound tracer (ppm)
    end								               
    S.TimeDepData     = additem(S.TimeDepData,'CCFLUX'   ,[1 S.FPTS]           ,'float32'); % mass transport averaged at the center of the user specified grid element (m3/sec)
    S.TimeDepData     = additem(S.TimeDepData,'ScalarsBaro',3                  ,'float32');
    % ESUM: average surface elevation in the modeling domain (m)
    % TKE: volume averaged total kinetic energy (joule)
    % APE: volume averaged available potential energy (joule)
else
    S.TimeDepData     = additem(S.TimeDepData,'DZSAVE'   ,[1 S.VPTS]           ,'float32'); % total depth of the user specified grid element (m) = bottom topography + elevation
    S.TimeDepData     = additem(S.TimeDepData,'UVSTZSAVE',[4 S.VPTS S.KBM1]    ,'float32'); % U/V velocity, temperature, salinity averaged at the center of the user specified grid element (m/sec), (oC), (psu)
    %S.TimeDepData    = additem(S.TimeDepData,'KHMZSAVE' ,[2 S.VPTS S.KBM1]    ,'float32'); % vertical eddy diffusivity and viscosity (m2/sec)
    if strcmp(S.TRACER,'INCLUDE')					       
        S.TimeDepData = additem(S.TimeDepData,'C1ZSAVE'  ,[1 S.VPTS S.KBM1]    ,'float32'); % conservative tracer concentration
    end									       
    if strcmp(S.SEDTRAN,'INCLUDE')					       
        S.TimeDepData = additem(S.TimeDepData,'CSAVE'    ,[2 S.VPTS S.KBM1]    ,'float32'); % cohesive and non-cohesive sediment concentration (mg/l)
        S.TimeDepData = additem(S.TimeDepData,'THSAVE'   ,[1 S.VPTS]           ,'float32'); % sediment bed elevation change (cm)
        S.TimeDepData = additem(S.TimeDepData,'TAUSAVE'  ,[1 S.VPTS]           ,'float32'); % bottom shear stress for use in sediment transport (dynes/cm2)
    end
    if strcmp(S.CHEMTRAN,'INCLUDE')
        S.TimeDepData = additem(S.TimeDepData,'PSAVE'    ,[2 S.VPTS S.KBM1]    ,'float32'); % cohesive and non-cohesive sediment-bound tracer concentration (ug/l)
        S.TimeDepData = additem(S.TimeDepData,'PBEDSAVE' ,[1 S.VPTS S.NCHEMLAY],'float32'); % sediment bed concentration of sediment-bound tracer (ppm)
    end
    S.TimeDepData     = additem(S.TimeDepData,'CCFLUX'   ,[1 S.FPTS S.KBM1]    ,'float32'); % mass transport averaged at the center of the user specified grid element (m3/sec)
    S.TimeDepData     = additem(S.TimeDepData,'Scalars'  ,5                    ,'float32');
    % VSTOR: volume storage (m3)
    % EM: excess mass (kg)
    % APEC: excess volume averaged available potential energy (joule)
    % TSUM: volume averaged temperature in the modeling domain (oC)
    % SSUM: volume averaged salinity in the modeling domain (psu)
end

%% Read times
sizecheck1(fid,'TMIDDLE',1,4)
S.Times = fread(fid,inf,'float32',S.TimeDepData.BlockSize-4);

%% Check data block size (begin)
function sizecheck1(fid,blockname,sz,bytes)
blocksize_0 = prod(sz)*bytes;
blocksize_1 = fread(fid,1,'uint32');
if blocksize_1 ~= blocksize_0
    fclose(fid);
    error('Invalid data block "%s" %i while expecting %i',blockname,blocksize_1,blocksize_0)
end

%% Check data block size (end)
function sizecheck2(fid,blockname,sz,bytes)
blocksize_0 = prod(sz)*bytes;
blocksize_1 = fread(fid,1,'uint32');
if blocksize_1 ~= blocksize_0
    fclose(fid);
    error('Invalid data block "%s": start size is %i whereas end size is %i.',blockname,blocksize_0,blocksize_1)
end

%% Read data block including block size check at begin and end
function V = checkedread(fid,blockname,blocksize,type)
switch type
    case {'float32','uint32'}
        bytes = 4;
end
sizecheck1(fid,blockname,blocksize,bytes)
V = reshape(fread(fid,prod(blocksize),type),[blocksize 1]);
sizecheck2(fid,blockname,blocksize,bytes)

%% Add offset, size and type information to data structure
function S = additem(S,blockname,blocksize,type)
switch type
    case {'float32','uint32'}
        bytes = 4;
end
S.(blockname).offset = S.BlockSize;
S.(blockname).size = blocksize;
S.(blockname).type = type;
S.BlockSize = S.BlockSize+8+prod(blocksize)*bytes;

%% Read a time dependent data block
function V = read_ecomsed(S,Qnt,Time,varargin)
Q = S.TimeDepData.(Qnt);
szmax = [length(S.Times) Q.size];
szact = szmax;
if nargin<3 || strcmp(Time,':')
    Time = 1:length(S.Times);
elseif ~ismember(Time,1:length(S.Times))
    error('Invalid time index specified.')
else
    szact(1) = length(Time);
end
for i = 1:length(varargin)
    if strcmp(varargin{i},':')
        % okay, everything requested
    elseif 1+i>length(szmax)
        error('Too many dimensions.')
    elseif ~ismember(varargin{i},1:szmax(1+i))
        error('Invalid dimension %i index specified.',i)
    else
        szact(1+i) = length(varargin{i});
    end
end
V = zeros([szact 1]);
fid = fopen(S.FileName,'r',S.ByteOrder,'US-ASCII');
for t = 1:length(Time)
    fseek(fid,S.OffsetTimeDepData+(Time(t)-1)*S.TimeDepData.BlockSize+Q.offset,-1);
    V1 = checkedread(fid,Qnt,Q.size,Q.type);
    V1 = V1(varargin{:},:);
    V(t,:) = reshape(V1,[1 prod(szact(2:end))]);
end
fclose(fid);
