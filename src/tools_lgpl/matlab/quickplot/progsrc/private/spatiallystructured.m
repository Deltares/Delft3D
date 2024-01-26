function Out=spatiallystructured(In)
%SPATIALLYSTRUCTURED Convert MNK space to xyz equivalent.

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
mnk=[M_ N_ K_];
MNK='MNK';
Out=In;
for i=1:length(In)
    switch MNK(In(i).DimFlag(mnk)~=0)
        case ''
            Out(i).SpatType='';
        case 'MN'
            Out(i).SpatType='xy';
        case 'MNK'
            Out(i).SpatType='xy+z';
        case 'K'
            Out(i).SpatType='z';
        case {'MK','NK'}
            Out(i).SpatType='x+z';
        case {'M','N'}
            Out(i).SpatType='x';
    end
end
