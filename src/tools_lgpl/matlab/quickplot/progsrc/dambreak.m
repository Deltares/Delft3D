function [h,u] = dambreak(xvec,x0,hl,hr,g,tvec)
%DAMBREAK Analytical solution of wet and dry dambreak test.
%   [H,U] = DAMBREAK(XVEC, X0, HL, HR, G, TVEC) returns the water depth H
%   and flow velocity U for the vector of locations XVEC and a vector of
%   times TVEC for the analytical solution of the dry or wet bed dambreak
%   simulation without bed friction. The dam break occurs at T=0 at X0. The
%   initial water depth left of the dam break equals HL, to the right of
%   the dam break HR. The gravitational acceleration used is G.

%----- LGPL --------------------------------------------------------------------
%                                                                               
%   Copyright (C) 2011-2024 Stichting Deltares.                                     
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

if hr>hl
    [h,u] = dambreak(-xvec(end:-1:1),-x0,hr,hl,tvec);
    h = h(end:-1:1);
    u = -u(end:-1:1);
    return
end

xvec = xvec(:);
h = zeros(numel(xvec),numel(tvec));
u = zeros(size(h));

if hl>0 && hr>0
    cl = sqrt(g*hl);
    cr = sqrt(g*hr);
    
    cm = roots([1, 0, -9*cr^2, 16*cl*cr^2, -(cr^2+8*cl^2)*cr^2, 0, cr^6]);
    cm = cm(real(cm)>cr & real(cm)<cl & imag(cm)==0);
    
    for i = 1:length(tvec)
        t = tvec(i);
        
        xa = x0 - cl*t;
        xb = x0 + (2*cl-3*cm)*t;
        xc = x0 + 2*cm^2*(cl-cm)*t/(cm^2-cr^2);
        
        regionA = xvec<xa;
        h(regionA,i) = hl;
        %u(regionA) = 0;
        
        regionAB = xvec>=xa & xvec<xb;
        h(regionAB,i) = 4/(9*g)*(cl - (xvec(regionAB)-x0)/(2*t)).^2;
        u(regionAB,i) = 2/3*((xvec(regionAB)-x0)/t + cl);
        
        regionBC = xvec>=xb & xvec<xc;
        h(regionBC,i) = cm^2/g;
        u(regionBC,i) = 2*(cl-cm);
        
        regionC = xvec>=xc;
        h(regionC,i) = hr;
        %u(regionC,i) = 0;
    end
    
elseif hl>0
    cl = sqrt(g*hl);

    for i = 1:length(tvec)
        t = tvec(i);
        
        xa = x0 - t*cl;
        xb = x0 + 2*t*cl;
        
        regionA = xvec<xa;
        h(regionA,i) = hl;
        %u(regionA,i) = 0;
        
        regionAB = xvec>=xa & xvec<xb;
        h(regionAB,i) = 4/(9*g)*(cl - (xvec(regionAB)-x0)/(2*t)).^2;
        u(regionAB,i) = 2/3*((xvec(regionAB)-x0)/t + cl);
        
        regionB = xvec>=xb;
        h(regionB,i) = 0;
        %u(regionB,i) = 0;
    end
end