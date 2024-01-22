function hNew=gentext(hOld,Ops,Parent,Str)
%GENTEXT Generic plot routine for a single text.

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

Str = protected(Str);

if ~isempty(hOld) & ishandle(hOld)
    hNew=hOld;
    set(hNew,'string',Str);
else
    hNew=text(0.5,0.5,Str, ...
        Ops.FontParams{:}, ...
        'horizontalalignment','center', ...
        'parent',Parent);
    setaxesprops(Parent,'Text')
end

function S = protected(S)
S = strrep(S,'\','\\');
S = strrep(S,'_','\_');
S = strrep(S,'^','\^');
