function I=qp_icon(Icon,ops)
%QP_ICON QuickPlot icon repository.
%   Generate the requested icon with transparent background
%      Icon=QP_ICON(IconName)
%   or with gray background
%      Icon=QP_ICON(IconName,'gray')

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

persistent IconRepository

if isempty(IconRepository)
   errmsg='Cannot find qp_icons.mat.';
   try
      IconRepository=load('qp_icons.mat');
   catch
      ui_message('error',errmsg,Icon)
      I=[];
   end
end

if ~isempty(IconRepository)
   errmsg='Cannot find icon ''%s''.';
   try
      I=getfield(IconRepository,Icon);
      errmsg='Error processing icon ''%s''.';
      if isstruct(I)
         if nargin==2
            switch ops
               case 'nan'
                  makenan=all(I.cmap==217/255,2);
                  I.cmap(makenan,:)=NaN;
               case 'index'
                  I=I.idx;
                  return;
            end
         end
         I=idx2rgb(I.idx,I.cmap);
      end
   catch
      ui_message('error',errmsg,Icon)
      I=[];
   end
end
