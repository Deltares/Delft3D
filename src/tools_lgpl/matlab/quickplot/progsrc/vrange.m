function dataOut = vrange(dataIn,Zmin,Zmax)
%VRANGE Selection of data based on a vertical coordinate range.
%   DATA3DOUT = VRANGE(DATA3DIN,ZMIN,ZMAX) extracts a horizontal data slice
%   located between vertical coordinates ZMIN and ZMAX out of a 3D data set
%   obtained from QPREAD. VRANGE supports both data at grid points
%   ('gridddata') and at cell centres ('gridcelldata') as provided by
%   QPREAD. The output of VRANGE is data structure compatible with the
%   QPREAD output.
%
%   Example
%      quantities = qpread(FI);
%      ivel = strmatch('velocity',{quantities.Name},'exact');
%      Qvel = quantities(ivel);
%      sz = qpread(FI,Qvel,'size');
%      tLast = sz(1);
%      data3d = qpread(FI,Qvel,'griddata',tLast);
%      sub3d = vrange(data3d,-3,-2);
%
%   See also QPFOPEN, QPREAD, HSLICE, VMEAN.

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

if ~isequal(size(Zmin),[1 1])
   error('Zmin should be scalar')
end
if ~isequal(size(Zmax),[1 1])
   error('Zmax should be scalar')
end
if Zmin>Zmax
   tmp=Zmin;
   Zmin=Zmax;
   Zmax=tmp;
end

dataOut = dataIn;
flds = {'Val','XComp','YComp','ZComp'};
for i = length(flds):-1:1
   if ~isfield(dataIn,flds{i})
      flds(:,i) = [];
   end
end

szZ = size(dataIn.Z);
Fld = flds{1};
Field3D = getfield(dataIn,Fld);
szV = size(Field3D);

continuous = isequal(szZ,szV);
zdim = length(szZ);
fulldims = repmat({':'},1,zdim-1);

if continuous
   for k = 1:szZ(zdim)-1
      Zk0 = dataIn.Z(fulldims{:},k);
      Zk1 = dataIn.Z(fulldims{:},k+1);
      tooLow0  = Zk0<Zmin;
      tooLow1  = Zk1<Zmin;
      tooHigh0 = Zk0>Zmax;
      tooHigh1 = Zk1>Zmax;
      %
      inRange = tooLow0 & tooLow1;
      if any(inRange(:))
         % shift both up
         Zk0(inRange) = Zmin;
         dataOut.Z(fulldims{:},k) = Zk0;
         Zk1(inRange) = Zmin;
         dataOut.Z(fulldims{:},k+1) = Zk1;
      end
      %
      inRange = tooHigh0 & tooHigh1;
      if any(inRange(:))
         % shift both down
         Zk0(inRange) = Zmax;
         dataOut.Z(fulldims{:},k) = Zk0;
         Zk1(inRange) = Zmax;
         dataOut.Z(fulldims{:},k+1) = Zk1;
      end
      %
      inRange = tooLow0 & ~tooLow1;
      if any(inRange(:))
         % shift layer k up
         Zcrit = Zmin;
         k0 = k;
         k1 = k+1;
         dataOut = shiftlayer(k0,k1,inRange,Zcrit,flds,dataIn,dataOut,fulldims);
      end
      %
      inRange = ~tooLow0 & tooLow1;
      if any(inRange(:))
         % shift layer k+1 up
         Zcrit = Zmin;
         k0 = k+1;
         k1 = k;
         dataOut = shiftlayer(k0,k1,inRange,Zcrit,flds,dataIn,dataOut,fulldims);
      end
      %
      inRange = tooHigh0 & ~tooHigh1;
      if any(inRange(:))
         % shift layer k down
         Zcrit = Zmax;
         k0 = k;
         k1 = k+1;
         dataOut = shiftlayer(k0,k1,inRange,Zcrit,flds,dataIn,dataOut,fulldims);
      end
      %
      inRange = ~tooHigh0 & tooHigh1;
      if any(inRange(:))
         % shift layer k+1 down
         Zcrit = Zmax;
         k0 = k+1;
         k1 = k;
         dataOut = shiftlayer(k0,k1,inRange,Zcrit,flds,dataIn,dataOut,fulldims);
      end
   end
else
   % could be done all at once, but that would require more memory
   % for large data sets
   for k = 1:szZ(zdim)
      Zk0 = dataIn.Z(fulldims{:},k);
      Zk0(Zk0<Zmin) = Zmin;
      Zk0(Zk0>Zmax) = Zmax;
      dataOut.Z(fulldims{:},k) = Zk0;
   end
end

function dataOut = shiftlayer(k0,k1,inRange,Zcrit,flds,dataIn,dataOut,fulldims)
Zk0 = dataIn.Z(fulldims{:},k0);
Zk1 = dataIn.Z(fulldims{:},k1);
Zk0r = Zk0(inRange);
Zk1r = Zk1(inRange);
alfa = (Zk1r-Zcrit)./(Zk1r-Zk0r);
for i = 1:length(flds)
   Fld = flds{i};
   Field3D = getfield(dataIn,Fld);
   Vk0 = Field3D(fulldims{:},k0);
   Vk1 = Field3D(fulldims{:},k1);
   %
   Field3D = getfield(dataOut,Fld);
   Vnew = Field3D(fulldims{:},k0);
   Vnew(inRange) = alfa.*Vk0(inRange)+(1-alfa).*Vk1(inRange);
   Field3D(fulldims{:},k0) = Vnew;
   dataOut = setfield(dataOut,Fld,Field3D);
end
%
Zk0 = dataOut.Z(fulldims{:},k0);
Zk0(inRange) = Zcrit;
dataOut.Z(fulldims{:},k0) = Zk0;
