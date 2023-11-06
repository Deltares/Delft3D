function Ops = expandPlotOptions(PO,varargin)
%expandPlotOptions Apply plot options.

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

if nargin==1
    Ops = [];
elseif nargin==2
    Ops = varargin{1};
else
    Ops = [];
    PON = {PO.Name};
    nOps = length(varargin)/2;
    if nOps ~= round(nOps)
        error('Invalid parameter/value pair arguments.')
    end
    for i = 1:2:length(varargin)
        Option = varargin{i};
        if ~ischar(Option)
            error('Invalid parameter/value pair arguments: <non char>.')
        end
        iPO = ustrcmpi(Option,PON);
        if iPO<0
            error('Invalid parameter/value pair arguments: %s.',Option)
        end
        Ops.(PON{iPO}) = varargin{i+1};
    end
end
%
for i = 1:length(PO)
    if ~isfield(Ops,PO(i).Name)
        Ops.(PO(i).Name) = PO(i).Default;
    end
end
