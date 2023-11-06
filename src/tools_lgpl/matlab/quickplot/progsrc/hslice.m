function dataOut = hslice(dataIn,Type,Z0)
%HSLICE Horizontal data slice of 3D data set.
%   DATA2D = HSLICE(DATA3D,Z0) extracts a horizontal data slice out of a 3D
%   data set obtained from QPREAD at the specified level Z0. HSLICE
%   supports both data at grid points ('gridddata') and at cell centres
%   ('gridcelldata') as provided by QPREAD. The output of HSLICE is data
%   structure compatible with the QPREAD output.
%
%   DATA2D = HSLICE(DATA3D,TYPE,ALPHA) extracts a quasi-horizontal data
%   slice out of a 3D data set obtained from QPREAD at the level specified
%   by TYPE and ALPHA. The following values for TYPE are supported:
%
%       TYPE           | meaning of ALPHA
%       ------------------------------------------------------------------
%       'z'            | vertical level ALPHA (equal to HSLICE(DATA3D,ALPHA)
%       'dz_below_max' | ALPHA is the distance dz below top
%       'dz_above_min' | ALPHA is the distance dz below bottom
%       'depth_frac'   | ALPHA is the depth fraction (0 = top, 1 = bottom)
%
%   Example
%      quantities = qpread(FI);
%      ivel = strmatch('velocity',{quantities.Name},'exact');
%      Qvel = quantities(ivel);
%      sz = qpread(FI,Qvel,'size');
%      for t = 1:sz(1); %select time step where 1<=t<=sz(1)
%         data3d = qpread(FI,Qvel,'griddata',t);
%         data2d = hslice(data3d,-3);
%         quiver(data2d.X,data2d.Y,data2d.XComp,data2d.YComp)
%         drawnow
%      end
%
%   See also QPFOPEN, QPREAD, VRANGE.

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

if nargin<2
   error('Not enough input arguments.')
elseif nargin==2
   Z0 = Type;
   Type = 'z';
elseif ~ischar(Type)
   error('Expected string as second input argument.')
else
   % Type: z, dz_below_max, dz_above_min, depth_frac
end
if isempty(Z0)
    error('Empty horizontal slice level ALPHA specified.')
elseif numel(Z0)>1
    error('Horizontal slice level ALPHA should be scalar.')
end

dataOut = dataIn;
flds = {'Val','XComp','YComp','ZComp'};
for i = length(flds):-1:1
   if ~isfield(dataIn,flds{i})
      flds(:,i) = [];
   end
end
nflds = length(flds);

szZ = size(dataIn.Z);
dataOut.Z = repmat(NaN,[szZ(1:end-1) 1]);
for i = 1:nflds
   Fld = flds{i};
   Field3D = getfield(dataIn,Fld);
   szV = size(Field3D);
   Field2D = repmat(NaN,[szV(1:end-1) 1]);
   dataOut = setfield(dataOut,Fld,Field2D);
end

continuous = isequal(szZ,szV);
zdim = length(szZ);
fulldims = repmat({':'},1,zdim-1);
%
switch Type
   case 'dz_above_min'
      Z0 = min(dataIn.Z,[],zdim) + Z0;
   case 'dz_below_max'
      Z0 = max(dataIn.Z,[],zdim) - Z0;
   case 'depth_frac'
      Zmin = min(dataIn.Z,[],zdim);
      Zmax = max(dataIn.Z,[],zdim);
      Z0 = Zmax + (Zmin - Zmax) * Z0;
   case 'z'
      Z0 = repmat(Z0,[szZ(1:zdim-1) 1]);
   otherwise
      error('Unknown slice TYPE.')
end
%
for k = 1:szZ(zdim)-1
   Zk0 = dataIn.Z(fulldims{:},k);
   Zk1 = dataIn.Z(fulldims{:},k+1);
   inRange = (Zk1<=Z0 & Z0<=Zk0) | (Zk0<=Z0 & Z0<=Zk1);
   if any(inRange(:))
      dZ = Zk0(inRange)-Zk1(inRange);
      dZ(dZ==0) = NaN;
      alf = (Z0(inRange)-Zk1(inRange))./dZ;
      dataOut.Z(inRange) = alf.*Zk0(inRange)+(1-alf).*Zk1(inRange);
      for i = 1:nflds
         Fld = flds{i};
         Field3D = getfield(dataIn,Fld);
         Field2D = getfield(dataOut,Fld);
         if continuous
            Vk0 = Field3D(fulldims{:},k);
            Vk1 = Field3D(fulldims{:},k+1);
            Field2D(inRange) = alf.*Vk0(inRange)+(1-alf).*Vk1(inRange);
         else
            Vk0 = Field3D(fulldims{:},k);
            Field2D(inRange) = Vk0(inRange);
         end
         dataOut = setfield(dataOut,Fld,Field2D);
      end
   end
end

dataOut.X = min(dataOut.X,[],zdim);
dataOut.Y = min(dataOut.Y,[],zdim);
