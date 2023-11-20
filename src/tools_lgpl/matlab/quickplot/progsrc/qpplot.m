function qpplot(data,varargin)
%QPPLOT Plot data like QUICKPLOT does (beta functionality).
%   QPPLOT(DATA, ..., KEY, VALUE, ...) with DATA the data to be plotted as
%   obtained from QPREAD in the same way as QUICKPLOT can plot that data.
%
%   Example
%      WD = qpread(qpfile,'water depth');
%      qpplot(WD, 'axestype', 'X-Y', ...
%                 'presentationtype', 'continuous shades', ...
%                 'colourlimits', [0 80], ...
%                 'zlevel', 0);
%      colormap(jet)
%   
%   See also QPFOPEN, QPFILE, QPREAD.

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

Ops = struct('version', 1.4, varargin{:});
qp_plot(data, Ops)