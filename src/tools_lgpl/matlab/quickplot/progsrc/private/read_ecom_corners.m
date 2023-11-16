function Grid = read_ecom_corners(file)
%READ_ECOM_CORNERS Read ECOMSED corners grid file.
%   G = READ_ECOM_CORNERS(FILENAME) reads the grid information from the ECOMSED
%   grid file and returns a structure G with fields X and Y.
%
%   Example
%      G = read_ecom_corners('corners.utm');
%      drawgrid(G.X,G.Y)
%
%   See also QPFOPEN, WLGRID, DRAWGRID, READ_ECOM_MODELGRID.

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

if ischar(file)
   fid = fopen(file,'r','n','US-ASCII');
   if fid<0
      error('Unable to open data file.')
   end
   close_when_finished = 1;
else
   fid = file;
   close_when_finished = 0;
end
Str = fgetl(fid);
Vals = sscanf(Str,'%f',inf);
if length(Vals)~=4 && length(Vals)~=5
   fclose(fid);
   error('Invalid number of columns.')
end
fseek(fid,0,-1);
Data = fscanf(fid,'%f',[length(Vals) inf]);
if ~feof(fid)
   fclose(fid);
   error('Non numeric data encountered in data file.')
end
if close_when_finished
   fclose(fid);
end

if ~all(all(Data(1:2,:)==round(Data(1:2,:))))
   error('Matrix indices contain non-integer values.')
end

Imax = max(Data(1,:));
Jmax = max(Data(2,:));

Grid.X = repmat(NaN,Imax,Jmax);
Grid.Y = repmat(NaN,Imax,Jmax);

Grid.X(Data(1,:)+(Data(2,:)-1)*Imax) = Data(3,:);
Grid.Y(Data(1,:)+(Data(2,:)-1)*Imax) = Data(4,:);

if size(Data,1)==5
   MaskCC = zeros(Imax,Jmax);
   MaskCC(Data(1,:)+(Data(2,:)-1)*Imax) = Data(5,:);
   MaskCR = MaskCC | MaskCC([1 1:end-1],:) | MaskCC(:,[1 1:end-1]) | MaskCC([1 1:end-1],[1 1:end-1]);
   MaskCR = ~MaskCR;
   Grid.X(MaskCR) = NaN;
   Grid.Y(MaskCR) = NaN;
end
