function X=idx2rgb(IDX,MAP)
%IDX2RGB Converts an indexed image into a true color RGB image.
%   Usage: RGBImage=idx2rgb(IndexImage,ColorMap)

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

szX=size(IDX);
if isa(IDX,'uint8')
    IDX=double(IDX)+1;
end
IDX=reshape(IDX,[prod(szX) 1]);
IDX=max(min(IDX,size(MAP,1)),1);

NaNs=isnan(IDX);
MAP=[MAP;NaN NaN NaN];
IDX(NaNs)=size(MAP,1);

X=MAP(IDX,:);
X=reshape(X,[szX 3]);
