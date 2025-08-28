function flag = dio_startwrite(dsh,diopart)
%DIO_STARTWRITE  Start writing to DelftIO stream.
%   Flag = DIO_STARTWRITE(dsh,diopart) enables writing to the specified
%   DelftIO stream where dsh is a DelftIO stream handle obtained from a
%   DIO_DEFINE call. The diopart argument should read 'header' or 'data';
%   it defaults to 'data' if not specified. The function blocks until the
%   data receiving component has finished reading any previous data.
%
%   See also DIO_DEFINE, DIO_WRITE, DIO_ENDWRITE.

%----- LGPL --------------------------------------------------------------------
%                                                                               
%   Copyright (C) 2011-2025 Stichting Deltares.                                 
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
    diopart = 'data';
end
flag = dio_core('startwrite',dsh,diopart);
