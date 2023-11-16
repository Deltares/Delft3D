function [U,V]=uv2cen(u,v)
%UV2CEN Interpolate velocities.
%   [U2,V2] = UV2CEN(U1,V1) interpolates velocities (U1,V1) from the
%   staggered u and v points (cell edges) to the zeta points (cell
%   centres). U1 and V1 should be nTim x N x M x ... matrices.

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

for i=1:ndims(u)
    idx{i}=1:size(u,i);
end

m=2:size(u,2);
idu=idx; idu{2}=m;
idd=idx; idd{2}=m-1;
%------------
%V(idu{:})=(v(idu{:})+v(idd{:}))/2;
%------------
vv=v(idu{:});
I=~isnan(vv);
vv(isnan(vv))=0;
V(idu{:})=vv;

vv=v(idd{:});
I=I+(~isnan(vv));
vv(isnan(vv))=0;
V(idu{:})=(V(idu{:})+vv)./max(I,1);
%------------
idd=idx; idd{2}=1;
V(idd{:})=NaN;


n=2:size(u,3);
idu=idx; idu{3}=n;
idd=idx; idd{3}=n-1;
%------------
%U(idu{:})=(u(idu{:})+u(idd{:}))/2;
%------------
vv=u(idu{:});
I=~isnan(vv);
vv(isnan(vv))=0;
U(idu{:})=vv;

vv=u(idd{:});
I=I+(~isnan(vv));
vv(isnan(vv))=0;
U(idu{:})=(U(idu{:})+vv)./max(I,1);
%------------
idd=idx; idd{3}=1;
U(idd{:})=NaN;
