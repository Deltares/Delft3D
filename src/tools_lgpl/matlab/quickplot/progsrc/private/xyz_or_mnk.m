function MNK = xyz_or_mnk(Ops,Selected,MNK)
%XYZ_OR_MNK Determine MNK flag for vector components to retrieve.

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

T_=1; ST_=2; M_=3; N_=4; K_=5;

for i=5:-1:1
    multiple(i) = (length(Selected{i})>1) | isequal(Selected{i},0);
end

switch Ops.vectorcomponent
    case {'vector (split m,n)'}
        MNK=-1;
    case {'magnitude in plane','m component','n component','normal component','edge normal component','edge tangential component'}
        MNK=1.5;
    case {'vector'}
        if  multiple(K_) && xor(multiple(M_),multiple(N_))
            MNK=1.5;
        end
end
