function hOut = plot(A,plotstyle,varargin)
%PLOT Plot qp_data object.

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

if A.Dummy
    error('Cannot plot a dummy object.')
elseif nargin<2
    PS = plotstyles(A,[]);
    %
    if isempty(PS)
        error('No plot styles defined for data provided.')
    else
        h = PS(1).Function(gca,A);
    end
elseif nargin==3 && strcmpi(plotstyle,'Parent')
    ax = varargin{1};
    %
    PS = plotstyles(A,ax);
    %
    if isempty(PS)
        error('No plot styles defined for data provided.')
    else
        h = PS(1).Function(ax,A);
    end
else
    Ops = varargin;
    if round(nargin/2)*2~=nargin
        error('Invalid parameter/value pair arguments.')
    end
    OpsPar = Ops(1:2:end);
    if ~all(cellfun('isclass',OpsPar,'char'))
        error('Invalid parameter/value pair arguments.')
    end
    ipar = strmatch('parent',lower(OpsPar),'exact');
    %
    if isempty(ipar)
        ax = [];
    else
        ax = Ops{2*(ipar(end)-1)+2};
        Ops([2*(ipar-1)+1 2*(ipar-1)+2]) = [];
    end
    %
    PS = plotstyles(A,ax);
    %
    PSN = {PS.Name};
    iPS = ustrcmpi(plotstyle,PSN);
    if iPS<0
        error('Plot style ''%s'' not defined for data provided.',plotstyle)
    end
    %
    if isempty(ax)
        ax = gca;
    end
    h = PS(iPS).Function(ax,A,Ops{:});
end
%
if nargout>0
    hOut = h;
end
