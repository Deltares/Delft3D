function varargout=corner2center(varargin)
%CORNER2CENTER Interpolate data from cell corners to cell centers.
%   Interpolates coordinates/data from corners (DP) to
%   centers (S1). Supports 1D, 2D, and 3D data, single
%   block and multiblock. In case the output datasets should
%   have the same size as the input datasets, add the optional
%   argument 'same' to the input arguments.
%
%   XCenter=CORNER2CENTER(XCorner)
%   [XCenter,YCenter,ZCenter]= ...
%       CORNER2CENTER(XCorner,YCorner,ZCorner)
%
%   See also CONV, CONV2, CONVN.

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

if nargin==0
   if nargout>0
      error('Too many output arguments.')
   else
      varargout={};
      return
   end
else % nargin>0
   nINP=nargin;
   ch=logical(zeros(1,length(varargin)));
   for i=1:length(varargin)
      ch(i)=ischar(varargin{i}) & ndims(varargin{i})==2 ...
         & size(varargin{i},1)==1;
   end
   INP=varargin(~ch);
   nINP=length(INP);
   opt=varargin(ch);
   if nINP>1 && nargout<nINP
      error('Not enough output arguments.')
   elseif nargout>nINP
      error('Too many output arguments.')
   end
end

method = 'mean';
detect_size_1_dimensions = 1;
same = 0;
for i=1:length(opt)
   switch lower(opt{i})
      case 'same'
         same = 1;
      case 'reduce_singles'
         detect_size_1_dimensions = 0;
      case 'max'
         method = 'max';
      case 'min'
         method = 'min';
      case 'mean'
         method = 'mean';
      otherwise
         error('Invalid option: %s',opt)
   end
end
varargout = cell(1,nargout);

for a = 1:nINP % for each argument
   if iscell(INP{a})
      varargout{a} = cell(size(INP{a}));
      [varargout{a}{:}] = corner2center(opt{:},INP{a}{:});
   else
      if detect_size_1_dimensions
         sizeCC = min(size(INP{a}),2);
      else
         sizeCC = repmat(2,1,ndims(INP{a}));
      end
      CC = ones(sizeCC);
      if isempty(CC)
         tmp=INP{a};
      else
         CC = CC/sum(CC(:));
         %
         switch method
            case 'mean'
               tmp = convn(INP{a},CC,'valid');
            otherwise
               error('Method not yet implemented.')
         end
      end
      if same && ~isempty(CC)
         varargout{a} = repmat(NaN,size(INP{a}));
         for d = ndims(INP{a}):-1:1
            ind{d} = sizeCC(d):size(INP{a},d);
         end
         varargout{a}(ind{:}) = tmp;
      else
         varargout{a} = tmp;
      end
   end
end
