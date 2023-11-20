function Thresholds=compthresholds(Ops,lm,LocStartClass)
%COMPTHRESHOLDS Determine automatic threshold levels.

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

Thresholds=Ops.thresholds;
if isempty(Thresholds)
    Thresholds=10;
end
%
if abs(lm(1))>eps(0)*1e6
    lm(1)=lm(1)-eps(lm(1));
end
if abs(lm(end))>eps(0)*1e6
    lm(end)=lm(end)+eps(lm(end));
end
%
if isstruct(Thresholds)
    step = Thresholds.step;
    Thresholds=step*(floor(lm(1)/step):ceil(lm(2)/step));
elseif isequal(size(Thresholds),[1 1]) && ...
        isequal(Thresholds,round(Thresholds)) && ...
        Thresholds>0
    if ~isempty(Ops.colourlimits)
        lm=Ops.colourlimits;
    end
    if Ops.symmetriccolourlimits
        lm=[-1 1]*max(abs(lm));
    end
    switch Ops.thresholddistribution
        case 'logarithmic'
            if all(lm>0)
                Thresholds=lm(1)*10.^((log10(lm(2))-log10(lm(1)))*(LocStartClass:Thresholds)/(Thresholds+1));
            elseif all(lm<0)
                Thresholds=lm(2)*10.^((log10(abs(lm(1)))-log10(abs(lm(2))))*(Thresholds:-1:LocStartClass)/(Thresholds+1));
            else
                ui_message('warning','The colour limits have different sign: switching to linear distribution of thresholds')
                Thresholds=lm(1)+(lm(2)-lm(1))*(LocStartClass:Thresholds)/(Thresholds+1);
            end
        case 'anti-logarithmic'
            ui_message('warning','anti-logarithmic not yet implemented: switching to linear distribution of thresholds')
            Thresholds=lm(1)+(lm(2)-lm(1))*(LocStartClass:Thresholds)/(Thresholds+1);
        otherwise
            Thresholds=lm(1)+(lm(2)-lm(1))*(LocStartClass:Thresholds)/(Thresholds+1);
    end
elseif isequal(size(Thresholds),[1 1])
    Thresholds=[-inf Thresholds];
end
