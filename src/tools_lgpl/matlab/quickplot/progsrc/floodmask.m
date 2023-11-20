function V = floodmask(V,i,j,criterion)
%FLOODMASK  Find connected points in an array.
%    VMASKED = FLOODMASK(V,I,J) locates the values in V that are not NaN and
%    that are connected (via other non-NaN values) to the start index (I,J) via
%    4-neighborhood connectivity (i.e. only left-right and up-down connections
%    allowed). The returned array VMASKED contains the values of V that satisfy
%    this criterion and NaN everywhere else. The initial value V(I,J) should
%    not be a NaN.
%
%    VMASKED = FLOODMASK(V,I,J,MASKCRIT) uses the MASKCRIT specified instead
%    of the default criterion ISNAN. The MASKCRIT can be any vectorized
%    logical function. MASKCRIT must be a function handle, function name,
%    inline function, or an anonymous function. The MASKCRIT should evaluate
%    to false for the initial value V(I,J). The default MASKCRIT is @isnan.
%
%    Example 1:
%         V = rand(50,30);
%         V(V<0.35) = NaN;
%         [i,j]=find(V==max(V(:)));
%         Vmasked = floodmask(V,i,j);
%
%    Example 2:
%         V = rand(50,30);
%         [i,j]=find(V==max(V(:)));
%         F = inline('x<0.35');
%         Vmasked = floodmask(V,i,j,F);
%
%    Example 3:
%         V = rand(50,30);
%         [i,j]=find(V==max(V(:)));
%         Vmasked = floodmask(V,i,j,@(x)x<0.35);
%
%   See also FIND.

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
%   $Id$  `

if nargin<4
   criterion = @isnan;
end
if nargin<3
   error('Three input arguments required')
elseif ndims(V)>2
   error('First argument should be a 2 dimensional matrix')
elseif ~isequal(size(i),[1 1]) || ~isnumeric(i) || i<1 || i>size(V,1) || double(i)~=round(double(i))
   error('Index I should be a scalar integer value between 1 and %i.',size(V,1))
elseif ~isequal(size(j),[1 1]) || ~isnumeric(j) || j<1 || j>size(V,2) || double(j)~=round(double(j))
   error('Index J should be a scalar integer value between 1 and %i.',size(V,2))
elseif feval(criterion,V(i,j))
   error('Criterion at start point (I,J) should not return false.')
end

Mask = false(size(V));
Mask(i,j) = true;

change = true;
while change
   Mask0 = Mask;
   %
   % Now sweep down,up,right,left through the matrix to find connected points.
   %
   for row=2:size(V,1)
      Mask(row,:) = (Mask(row,:) | Mask(row-1,:)) & ~feval(criterion,V(row,:));
   end
   for row=size(V,1)-1:-1:1
      Mask(row,:) = (Mask(row,:) | Mask(row+1,:)) & ~feval(criterion,V(row,:));
   end
   for col=2:size(V,2)
      Mask(:,col) = (Mask(:,col) | Mask(:,col-1)) & ~feval(criterion,V(:,col));
   end
   for col=size(V,2)-1:-1:1
      Mask(:,col) = (Mask(:,col) | Mask(:,col+1)) & ~feval(criterion,V(:,col));
   end
   %
   change = ~isequal(Mask,Mask0);   
end

V(~Mask)=NaN;
