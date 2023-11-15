function dataOut = vmean(dataIn,varargin)
%VMEAN Compute average of data in vertical direction.
%   DATA2D = VMEAN(DATA3D,METHOD) averages the data of a 3D data set
%   obtained from QPREAD over the vertical direction. The averaging method
%   should be 'linear' (default) or 'squared'. The Z coordinate information
%   is removed from the data structure.
%
%   DATA2D = VMEAN(DATA3D,METHOD,'zrange') retains the Z coordinate
%   information as part of the data structure; the Z data of the output
%   argument DATA2D will contain the minimum and maximum Z coordinates.
%
%   Example 1
%      %compute depth average and return z coordinates
%      quantities = qpread(FI);
%      ivel = strmatch(quantities,'velocity');
%      Qvel = quantities(ivel);
%      sz = qpread(FI,Qvel,'size');
%      data3d = qpread(FI,Qvel,'griddata',t);
%      data2d = vmean(data3d,'zrange');
%
%   Example 2
%      %average of certain vertical range
%      sub3d  = vrange(data3d,-3,-2);
%      sub2d  = vmean(sub3d);
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

avgMethod = 'linear';
zMethod = 'noz';

if nargin>1
   for i=1:length(varargin)
      if ~ischar(varargin{i})
         error('invalid argument %i: string expected.',i+1)
      else
         string = lower(varargin{i});
         switch string
            case {'linear','squared'}
               avgMethod = string;
            case {'noz','zrange'}
               zMethod = string;
            otherwise
               error('invalid argument %i: unrecognized string.',i+1)
         end
      end
   end
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
switch zMethod
   case 'noz'
      dataOut=rmfield(dataOut,'Z');
   case 'zrange'
      vdim = length(szZ);
      dataOut.Z = cat(vdim,min(dataIn.Z,[],vdim),max(dataIn.Z,[],vdim));
end
for i = 1:nflds
   Fld = flds{i};
   Field3D = dataIn.(Fld);
   szV = size(Field3D);
   Log2D = all(isnan(Field3D),length(szV));
   Field2D = zeros(size(Log2D));
   Field2D(Log2D) = NaN;
   dataOut.(Fld) = Field2D;
end

continuous = isequal(szZ,szV);
zdim = length(szZ);
fulldims = repmat({':'},1,zdim-1);

Thick=0;
for k = 1:szZ(zdim)-1
   Zk0 = dataIn.Z(fulldims{:},k);
   Zk1 = dataIn.Z(fulldims{:},k+1);
   dZ = abs(Zk1-Zk0);
   for i = 1:nflds
      Fld = flds{i};
      Field3D = dataIn.(Fld);
      Field2D = dataOut.(Fld);
      if continuous
         Vk0 = Field3D(fulldims{:},k);
         Vk1 = Field3D(fulldims{:},k+1);
         Vk0(dZ==0) = 0;
         Vk1(dZ==0) = 0;
         switch avgMethod
            case 'linear'
               Field2D = Field2D+dZ.*(Vk0+Vk1)/2;
            case 'squared'
               Field2D = Field2D+dZ.*(Vk0.^2+Vk0.*Vk1+Vk1.^2)/3;
         end
      else
         Vk0 = Field3D(fulldims{:},k);
         Vk0(dZ==0) = 0;
         switch avgMethod
            case 'linear'
               Field2D = Field2D+dZ.*Vk0;
            case 'squared'
               Field2D = Field2D+dZ.*Vk0.^2;
         end
      end
      dataOut.(Fld) = Field2D;
   end
   Thick = Thick + dZ;
end

s = warning('query','MATLAB:divideByZero');
warning('off',s.identifier);
for i = 1:nflds
   Fld = flds{i};
   Field2D = dataOut.(Fld);
   Field2D = Field2D./Thick;
   %
   switch avgMethod
      case 'linear'
         %Field2D = Field2D;
      case 'squared'
         Field2D = sqrt(Field2D);
   end
   dataOut.(Fld) = Field2D;
end
warning(s.state,s.identifier)

dataOut.X = min(dataOut.X,[],zdim);
dataOut.Y = min(dataOut.Y,[],zdim);
switch zMethod
   case 'zrange'
      dataOut.X = cat(vdim,dataOut.X,dataOut.X);
      dataOut.Y = cat(vdim,dataOut.Y,dataOut.Y);
end
