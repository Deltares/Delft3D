function hOut = columnpatch(X,Y,Val)
%COLUMNPATCH Create stacked patches plot.
%   COLUMNPATCH(X,Y,C) adds a series of patches to the current axes, one
%   patch for every value of M x N matrix C. The patches will be ordered in
%   columns; the corresponding data may be stored per column or row. If the
%   data is ordered column wise, then X should be a vector with N+1 values
%   representing the  borders of the columns, Y should be a M+1 x N matrix
%   with z levels of the borders between the patches per column. If the
%   data is ordered row wise, then X should be a vector with M+1 values
%   representing the borders of the columns, Y should be a M x N+1 matrix
%   with z levels of the borders between the patches per column. Hence, if
%   COLUMNPATCH(X,Y,C) works then COLUMNPATH(X',Y',C') gives the same
%   result.
%
%   Example
%      X = cumsum(rand(1,21),2);  % 21 borders of columns
%      Y = cumsum(rand(11,20),1); % 11 vertical levels per column
%      C = rand(10,20);           % 20 columns of 10 values each
%      columnpatch(X,Y,C) % column wise
%      % columnpatch(X',Y',C') works also
%
%   See also PATCH.

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

if ndims(Val)~=2
   error('C matrix should be 2 dimensional')
elseif ndims(Y)~=2
   error('Y matrix should be 2 dimensional')
elseif ndims(X)~=2
   error('X matrix should be 2 dimensional')
end

if isequal(size(Y),size(Val)+[1 0])
   % each column of Val becomes a column in the plot
   if isequal(numel(X),size(Val,2)+1)
      X = repmat(X(:),1,size(Val,1)+1);
   else
      error('For a column oriented C matrix [%i %i], X must be a vector of length %i.', ...
         size(Val,1),size(Val,2),size(Val,2)+1)
   end
   Val = Val';
   Y = Y';
elseif isequal(size(Y),size(Val)+[0 1])
   % each row of Val becomes a column in the plot
   if isequal(numel(X),size(Val,1)+1)
      X = repmat(X(:),1,size(Val,2)+1);
   else
      error('For a row oriented C matrix [%i %i], X must be a vector of length %i.', ...
         size(Val,1),size(Val,2),size(Val,1)+1)
   end
else
   error('Size of Y matrix [%i %i] must equal [%i %i] (column per column) or [%i %i] (column per row).', ...
      size(Y,1),size(Y,2),size(Val,1),size(Val,2)+1,size(Val,1)+1,size(Val,2))
end

hOld = [];
Ops.colourmap = 1;
Ops.colour = get(gca,'defaultpatchedgecolor');
Ops.presentationtype = 'patches with lines';
Parent = gca;

hNew = genfaces(hOld,Ops,Parent,Val,X,Y); % Z

if nargout>0
   hOut = hNew;
end