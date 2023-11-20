% DelftIO toolbox.
% Version <VERSION> (<CREATIONDATE>)
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
%
% DelftIO Routines
%   dio_define           - Create a new DelftIO stream.
%   dio_delete           - Delete/destroy a DelftIO stream.
%
%   dio_startwrite       - Start writing to DelftIO stream.
%   dio_write            - Write to DelftIO stream.
%   dio_endwrite         - End writing to DelftIO stream.
%
%   dio_startread        - Start reading from DelftIO stream.
%   dio_read             - Read from DelftIO stream.
%   dio_endread          - End reading from DelftIO stream.
%
%   dio_getname          - Retrieve the name of the DelftIO stream.
%   dio_getremainingsize - Retrieve the remaining size of the DelftIO stream.
%   dio_getsize          - Retrieve the size of the DelftIO stream.
%
% DelftIO Examples
%   putter               - Example of sending data via DelftIO.
%   getter               - Example of receiving data via DelftIO.
%
% DelftIO PLT Routines
%   dioplt_define        - Create a DelftIO PLT stream.
%   dioplt_write         - Write to a DelftIO PLT stream.
%   dioplt_read          - Read from a DelftIO PLT stream.
%   dioplt_delete        - Delete/destroy a DelftIO PLT stream.

%   @(#)Deltares, DelftIO toolbox, Version <VERSION>, <CREATIONDATE>
%   http://www.deltaressystems.com
%   $HeadURL$
%   $Id$
