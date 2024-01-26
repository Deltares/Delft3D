function EdgeNodeConnect = fnc2enc(FaceNodeConnect)
%FNC2ENC Create edge-nodes connectivity based on face-nodes connectivity
%   This is a UGRID support function.
%
%   ENC = FNC2ENC(FNC)
%   Creates an edge-nodes connectivity array based on a face-nodes
%   connectivity array containing all the edges that make up the faces
%   defined by the face-nodes connectivity array. Any permutation of the
%   edge-nodes connectivity array is a valid version of that array.

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

nc = size(FaceNodeConnect,2);
iConnect = ceil(([0 0:2*nc-2])/2+0.1);
EdgeNodeConnect = FaceNodeConnect(:,iConnect);
ncP = sum(~isnan(FaceNodeConnect),2);
EdgeNodeConnect(:,1) = FaceNodeConnect(sub2ind(size(FaceNodeConnect),(1:size(FaceNodeConnect,1))',ncP));
EdgeNodeConnect = unique(sort(reshape(EdgeNodeConnect',[2 numel(FaceNodeConnect)]),1)','rows');
EdgeNodeConnect(any(isnan(EdgeNodeConnect),2),:) = [];
