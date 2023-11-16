function hNew=gensurface(hOld,Ops,Parent,Val,X,Y,Z)
%GENSURFACE Generic plot routine for surface plot.

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

zcoord=nargin>6;
blank=isnan(Val(:));
%
if ~zcoord
    Z=zeros(size(X));
end
if ~isa(Z,'double')
    Z=double(Z);
end
if ~isa(Val,'double')
    Val=double(Val);
end
if ~isequal(size(X),size(Val))
    [X,Y,vv]=face2surf(X,Y,{Val Z});
    Val = vv{1};
    Z = vv{2};
end
X(isnan(Val))=NaN;
Y(isnan(Val))=NaN;
if ~isempty(hOld) && ishandle(hOld)
    hNew=hOld;
    set(hNew,'xdata',X, ...
        'ydata',Y, ...
        'zdata',Z, ...
        'cdata',Val)
else
    hNew=surface(X,Y,Z, ...
        'cdata',Val, ...
        'parent',Parent, ...
        'edgecolor','none', ...
        'facecolor','interp');
end
