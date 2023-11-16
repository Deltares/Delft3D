function G = read_ecom_modelgrid(FileName)
%READ_ECOM_MODELGRID Read ECOMSED grid file.
%   G = READ_ECOM_MODELGRID(FILENAME) reads the grid information from the
%   ECOMSED grid file. The grid file may contain only distance information
%   for the grid cell centres; in that case an attempt is made to
%   reconstruct the relative coordinates of the grid. This reconstructed
%   grid may have significant distortions. The function returns a structure
%   with fields X and Y.
%
%   Example
%      G = read_ecom_modelgrid('modelgrid');
%      drawgrid(G.X,G.Y)
%
%   See also QPFOPEN, WLGRID, DRAWGRID, READ_ECOM_CORNERS.

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

fid = fopen(FileName,'r','n','US-ASCII');
Comment = fgetl(fid); % New Refined Grid

Comment = fgetl(fid); % Sigma Levels Z=0  to -1.0  11 levels 
nLayerInterfaces = fscanf(fid,'%i',1);
sigma = fscanf(fid,'%f',nLayerInterfaces);
fgetl(fid);

Comment = fgetl(fid); %  Horizontal Segmentations
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
      D = [];
   case 8
      %I,J,H1(I,J),H2(I,J),H(I,J),ANG(I,J),COR(I,J),DATUM(I,J)
      gridInfo = fscanf(fid,'%i %i %f %f %f %f %f %f',[8 inf]);
      Y = gridInfo(7,:);
      X = [];
      D = [];
   case 9
      %I,J,H1(I,J),H2(I,J),H(I,J),ANG(I,J),YGRID(I,J),XGRID(I,J),DATUM(I,J)
      gridInfo = fscanf(fid,'%i %i %f %f %f %f %f %f %f',[9 inf]);
      Y = gridInfo(7,:);
      X = gridInfo(8,:);
      D = gridInfo(9,:);
end
I = gridInfo(1,:);
J = gridInfo(2,:);
dI = gridInfo(3,:);
dJ = gridInfo(4,:);
H = gridInfo(5,:);
Alpha = gridInfo(6,:);

Comment = fgetl(fid); % THIN DAM
nDams = fscanf(fid,'%i',1);
damInfo = fscanf(fid,'%i %i %cDIR %i',[4 nDams]);

fclose(fid);

G.X = repmat(NaN,gridSize);
G.Y = repmat(NaN,gridSize);

Idx = I + gridSize(1)*(J-1);
if ~isempty(X)
   G.X(Idx) = X;
   G.Y(Idx) = Y;
else
   G = reconstruct_2(G,Idx,dI,dJ,Alpha*pi/180);
end


function G = reconstruct_2(G,Idx,dI,dJ,Alpha)
gridSize = size(G.X);
Xcc = G.X;
Ycc = G.Y;

dxI = repmat(NaN,gridSize);
dxI(Idx) = dI.*cos(Alpha)/2;
dyI = repmat(NaN,gridSize);
dyI(Idx) = dI.*sin(Alpha)/2;
dxJ = repmat(NaN,gridSize);
dxJ(Idx) = dJ.*cos(Alpha+pi/2)/2;
dyJ = repmat(NaN,gridSize);
dyJ(Idx) = dJ.*sin(Alpha+pi/2)/2;

i_todo = 1:length(Idx);
while ~isempty(i_todo)
   i_mask = logical(zeros(size(i_todo)));
   for t = 1:length(i_todo)
      i = i_todo(t);
      idx = Idx(i)+[-1 -gridSize(1) +1 +gridSize(1)];
      if i==1
         i_mask(t) = 1;
         Xcc(Idx(i)) = 0;
         Ycc(Idx(i)) = 0;
      elseif ~all(isnan(Xcc(idx)))
         i_mask(t) = 1;
         DXI = dxI(Idx(i));
         DYI = dyI(Idx(i));
         DXJ = dxJ(Idx(i));
         DYJ = dyJ(Idx(i));
         x = repmat(NaN,1,4);
         y = repmat(NaN,1,4);
         if ~isnan(Xcc(idx(1)))
            x(1) = Xcc(idx(1)) + dxI(idx(1)) + DXI;
            y(1) = Ycc(idx(1)) + dyI(idx(1)) + DYI;
         end
         if ~isnan(Xcc(idx(2)))
            x(2) = Xcc(idx(2)) + dxJ(idx(2)) + DXJ;
            y(2) = Ycc(idx(2)) + dyJ(idx(2)) + DYJ;
         end
         if ~isnan(Xcc(idx(3)))
            x(3) = Xcc(idx(3)) - dxI(idx(3)) - DXI;
            y(3) = Ycc(idx(3)) - dyI(idx(3)) - DYI;
         end
         if ~isnan(Xcc(idx(4)))
            x(4) = Xcc(idx(4)) - dxJ(idx(4)) - DXJ;
            y(4) = Ycc(idx(4)) - dyJ(idx(4)) - DYJ;
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

Id = [1 1:gridSize(1)-1];
Jd = [1 1:gridSize(2)-1];
Xco = cat(3,Xcc-dxI-dxJ,Xcc(Id,:)+dxI(Id,:)-dxJ(Id,:),Xcc(Id,Jd)+dxI(Id,Jd)+dxJ(Id,Jd),Xcc(:,Jd)-dxI(:,Jd)+dxJ(:,Jd));
Yco = cat(3,Ycc-dyI-dyJ,Ycc(Id,:)+dyI(Id,:)-dyJ(Id,:),Ycc(Id,Jd)+dyI(Id,Jd)+dyJ(Id,Jd),Ycc(:,Jd)-dyI(:,Jd)+dyJ(:,Jd));

Nvalid = max(sum(~isnan(Yco),3),1);
Xco(isnan(Xco)) = 0;
Yco(isnan(Yco)) = 0;
G.X = sum(Xco,3)./Nvalid;
G.Y = sum(Yco,3)./Nvalid;


function G = reconstruct_1(G,Idx,dI,dJ,Alpha)
gridSize = size(G.X);
i_todo = 1:length(Idx);
while ~isempty(i_todo)
   i_mask = logical(zeros(size(i_todo)));
   for t = 1:length(i_todo)
      i = i_todo(t);
      idx = Idx(i)+[0 -1 -gridSize(1) -gridSize(1)-1];
      if ~all(isnan(G.X(idx))) | i==1
         i_mask(t) = 1;
         if any(isnan(G.X(idx)))
            dxi = dI(i)*cos(Alpha(i));
            dyi = dI(i)*sin(Alpha(i));
            dxj = dJ(i)*cos(Alpha(i)+pi/2);
            dyj = dJ(i)*sin(Alpha(i)+pi/2);
            if i==1
               G.X(idx(1)) = (dxi+dxj)/2;
               G.Y(idx(1)) = (dyi+dyj)/2;
               G.X(idx(2)) = (-dxi+dxj)/2;
               G.Y(idx(2)) = (-dyi+dyj)/2;
               G.X(idx(3)) = (dxi-dxj)/2;
               G.Y(idx(3)) = (dyi-dyj)/2;
               G.X(idx(4)) = (-dxi-dxj)/2;
               G.Y(idx(4)) = (-dyi-dyj)/2;
            else
               % one corner should be non-NaN
               if isnan(G.X(idx(1)))
                  if ~isnan(G.X(idx(2)))
                     G.X(idx(1)) = G.X(idx(2))+dxi;
                     G.Y(idx(1)) = G.Y(idx(2))+dyi;
                  elseif ~isnan(G.X(idx(3)))
                     G.X(idx(1)) = G.X(idx(3))+dxj;
                     G.Y(idx(1)) = G.Y(idx(3))+dyj;
                  elseif ~isnan(G.X(idx(4)))
                     G.X(idx(1)) = G.X(idx(4))+dxi+dxj;
                     G.Y(idx(1)) = G.Y(idx(4))+dyi+dyj;
                  end
               end
               if isnan(G.X(idx(2)))
                  G.X(idx(2)) = G.X(idx(1))-dxi;
                  G.Y(idx(2)) = G.Y(idx(1))-dyi;
               end
               if isnan(G.X(idx(3)))
                  G.X(idx(3)) = G.X(idx(1))-dxj;
                  G.Y(idx(3)) = G.Y(idx(1))-dyj;
               end
               if isnan(G.X(idx(4)))
                  G.X(idx(4)) = G.X(idx(1))-dxi-dxj;
                  G.Y(idx(4)) = G.Y(idx(1))-dyi-dyj;
               end
            end
         end
      end
   end
   if any(i_mask)
      i_todo = i_todo(~i_mask);
   else
      break
   end
end
