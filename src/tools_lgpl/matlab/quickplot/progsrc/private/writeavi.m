function varargout=writeavi(varargin)
%WRITEAVI MEX interface to Windows AVI functions.
%
%   AVIHandle = WRITEAVI('initialize')
%   AVIHandle = WRITEAVI('open', AVIHandle, FileName)
%   AVIOps = WRITEAVI('getoptions', NBits)
%           NBits = 8 or 24
%   AVIHandle = WRITEAVI('addvideo', AVIHandle, BaseFrameRate, Width,
%               Height, 8, ColorMap, AVIOps)
%   AVIHandle = WRITEAVI('addvideo', AVIHandle, BaseFrameRate, Width,
%               Height, 24, AVIOps)
%   AVIHandle = WRITEAVI('addframe', AVIHandle, Bitmap, FrameNr)
%   AVIHandle = WRITEAVI('close', AVIHandle)
%   Flag = WRITEAVI('finalize', AVIHandle)

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

%   Compile using Visual Studio 6.0:
%   > mex writeavi.cpp vfw32.lib user32.lib
%#mex
error('Missing MEX-file WRITEAVI');
