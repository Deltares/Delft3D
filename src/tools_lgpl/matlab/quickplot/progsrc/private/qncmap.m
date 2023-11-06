function map = qncamp(m)
%QNCMAP    QuickIn color map.
%   QNCMAP(M) returns an M-by-3 matrix containing the QuickIn colormap.
%   QNCMAP, by itself, is the same length as the current colormap.

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

if nargin < 1, m = size(get(gcf,'colormap'),1); end
h = (0:m-1)'/max(m-1,1);
if isempty(h)
    map = [];
else
    j=[.75 .75 0   1   1   0  ;
        .5  .5  0   .75 .75 0  ;
        0   .75 0   0   1   0  ;
        0   .5  0   0   .75 0  ;
        0   .75 .75 0   1   1  ;
        0   .5  .5  0   .75 .75;
        0   0   .75 0   0   1  ;
        0   0   .5  0   0   .75;
        .75 0   .75 1   0   1  ;
        .5  0   .5  .75 0   .75;
        .75 .75 .75 1   1   1  ];
    i=min(11,floor(h*11)+1);
    f=h*11-i+1;
    map=j(i,1:3)+repmat(f,1,3).*(j(i,4:6)-j(i,1:3));
    map(1,:)=[.75 0 0];
    map(end,:)=[.75 .75 .75];
end
