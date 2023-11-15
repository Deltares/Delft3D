function [U,V]=cur2ca(u,v,alf)
%CUR2CA Rotate velocity components.
%   [U,V] = CUR2CA(U,V,ALPHA) rotate velocity from (xi,eta) components in grid
%   directions to components in cartesian (x,y) directions. The U and V arrays
%   are supposed to be arrays of size NTIMES x NHDIM1 x ... x NHDIMn x KLAYERS
%   whereas the ALPHA array needs to match the size of the horizontal
%   dimensions, i.e. an NHIM1 x ... x NHDIMn array representing the angle
%   between the xi direction and the positive x axis in radians.
%
%   Example
%      % create grid
%      [x,y] = ndgrid(1:30,1:40);
%      X = x-y;
%      Y = y+x;
%      ALPHA = repmat(pi/4,[30 40]);
%      % create data on grid
%      U = repmat(rand(10,30,40),[1 1 1 3]);
%      V = repmat(rand(10,30,40),[1 1 1 3]);
%      V(:,:,:,2) = 0; % no V component for layer 2
%      U(:,:,:,3) = 0; % no U component for layer 3
%      %
%      [UU,VV] = cur2ca(U,V,A);
%      for t = 1:10
%         L1 = quiver(X,Y,squeeze(UU(t,:,:,1)),squeeze(VV(t,:,:,1)),0);
%         set(gca,'DataAspectRatio',[1 1 1])
%         hold on
%         L2 = quiver(X,Y,squeeze(UU(t,:,:,2)),squeeze(VV(t,:,:,2)),0,'r');
%         L3 = quiver(X,Y,squeeze(UU(t,:,:,3)),squeeze(VV(t,:,:,3)),0,'r');
%         if t<10
%            pause
%            delete(L1); delete(L2); delete(L3); 
%         end
%      end
%
%   See also QUIVER.

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

alf = reshape(alf,[1 size(alf)]);
cosalf = cos(alf);
sinalf = sin(alf);

sz = size(u);
U = zeros(size(u));
V = zeros(size(v));
%
KDim = ndims(u)+1;
sz(end+1) = 1;
for i = 2:ndims(u)
    if size(alf,i) ~= size(u,i)
        KDim = i;
        break
    end
end
%
Idx = repmat({':'},1,KDim-2);
nLayers = prod(sz(KDim:end));
%
for i=1:sz(1)
   for k=1:nLayers
      U(i,Idx{:},k) = u(i,Idx{:},k).*cosalf - v(i,Idx{:},k).*sinalf;
      V(i,Idx{:},k) = u(i,Idx{:},k).*sinalf + v(i,Idx{:},k).*cosalf;
   end
end
