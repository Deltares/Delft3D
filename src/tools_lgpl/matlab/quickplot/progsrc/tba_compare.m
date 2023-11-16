function h = tba_compare(tba,comp,qnt,varargin)
%TBA_COMPARE Plot computed versus observed tidal analysis data.
%   TBA_COMPARE(TBA,COMPONENT,QUANTITY) plots the tidal component for the
%   specified quantity from the analysed computed data against the observed
%   component.
%
%   H = TBA_COMPARE(...) returns a handle to the created line object.
%
%   TBA_COMPARE(...,Prop1,Value1,Prop2,Value2,...) sets an additional
%   set of optional line property values.
%
%   Example
%      TKL = tekal('open',FILENAME);
%      TBA = tekal2tba(TKL);
%      tba_compare(TBA,'M2','water levels','marker','+');
%
%   See also TBA_PLOTELLIPSES, TEKAL2TBA.

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

ic = strmatch(upper(comp),tba.Components,'exact');
if isempty(ic)
    error('Invalid component specified.')
end
%
iq = ustrcmpi(qnt,tba.Quantities);
if iq<0
    error('Invalid quantity specified.')
end
%
switch tba.Quantities{iq}
    case 'water levels'
        unt = ' (m)';
    case {'east velocities','north velocities'}
        unt = ' (m/s)';
    otherwise
        unt = '';
end
%
qnt = tba.Quantities{iq};
hi = line(squeeze(tba.Info(ic,iq,:,3)),squeeze(tba.Info(ic,iq,:,1)), ...
    'marker','.','linestyle','none',varargin{:});
%
ax = get(hi,'parent');
hh = get(ax,'children');
hh(hh==hi)=[];
delete(hh)
title(ax,sprintf('%s%s, component %s',qnt,unt,tba.Components{ic}))
xlabel(ax,'observed')
ylabel(ax,'computed')
xlim = get(ax,'xlim');
ylim = get(ax,'ylim');
lim = [min(xlim(1),ylim(1)) max(xlim(2),ylim(2))];
line(lim,lim,'color','k','parent',ax)
set(ax,'box','on','xlim',lim,'ylim',lim)
%
if nargout>0
    h = hi;
end
