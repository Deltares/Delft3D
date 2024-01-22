function h = tba_plotellipses(tba,comp,varargin)
%TBA_PLOTELLIPSES Plot tidal ellipses from Delft3D-TRIANA TBA file.
%   TBA_PLOTELLIPSES(TBA,COMPONENT,SOURCE) plots tidal ellipses based on
%   data from the specified TBA file for the specified COMPONENT and source
%   specified as 'computed' or 'observed'. The source parameter may be
%   dropped in which case the default 'computed' is used. The COMPONENT
%   should match one of the components such as 'K1' or 'M2' specified in
%   the TBA file. The TBA structure can be obtained from TEKAL2TBA.
%
%   H = TBA_PLOTELLIPSES(...) returns a handle to the created line object.
%
%   TBA_PLOTELLIPSES(...,Prop1,Value1,Prop2,Value2,...) sets an additional
%   set of optional property values. For supported properties see
%   PLOT_TIDALELLIPSES.
%
%   Example
%      TKL = tekal('open',FILENAME);
%      TBA = tekal2tba(TKL);
%      tba_plotellipses(TBA,'M2');
%      xlabel('x coordinate \rightarrow')
%      ylabel('y coordinate \rightarrow')
%      title('M2')
%
%   See also PLOT_TIDALELLIPSES, TBA_COMPARE, TEKAL2TBA.

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

x = tba.Locations.X;
y = tba.Locations.Y;
%
ic = strmatch(upper(comp),tba.Components,'exact');
if isempty(ic)
    error('Invalid component specified.')
end
%
if round(nargin/2)*2 ~= nargin
    src = varargin{1};
    varargin = varargin(2:end);
    %
    isrc = ustrcmpi(src,{'computed','observed'});
    if isrc<0
        error('Invalid source specified')
    else
        isrc = (isrc-1)*2;
    end
else
    isrc = 0;
end
%
iu = strmatch('east velocities',tba.Quantities,'exact');
iv = strmatch('north velocities',tba.Quantities,'exact');
if isempty(iv) && isempty(iu)
    error('No velocity data available for ellipses.')
elseif isempty(iu)
    error('East velocity component missing for ellipses.')
elseif isempty(iv)
    error('North velocity component missing for ellipses.')
end
%
Au = tba.Info(ic,iu,:,isrc+1);
PHIu = tba.Info(ic,iu,:,isrc+2);
Av = tba.Info(ic,iv,:,isrc+1);
PHIv = tba.Info(ic,iv,:,isrc+2);
%
hi = plot_tidalellipses(x,y,'ap',Au,PHIu,Av,PHIv,varargin{:});
if nargout>0
    h = hi;
end
