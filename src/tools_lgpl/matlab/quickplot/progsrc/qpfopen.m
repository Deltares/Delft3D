function Out = qpfopen(varargin)
%QPFOPEN General routine for open various types of data files.
%   FILE = QPFOPEN('Filename') opens the specified output file and returns
%   a structure containing data used by the QPREAD function.
%
%   For Delwaq/par MAP files the user should also specify a grid file:
%   FILE = QPFOPEN('DataFile','GridFile')
%   If no grid file is specified then the MAP file is treated as a history
%   file with an observation point for each segment.
%
%   When no arguments are passed to the function, the user is asked to
%   specify the file using a standard file selection dialog window.
%
%   See also QPREAD, D3D_QP.

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
%   http://www.deltaressystems.com
%   $HeadURL$
%   $Id$

[Out,FileName,Tp] = qp_fmem('open',varargin{:});
if ~isempty(Tp)
    Out.QP_FileType = Tp;
end