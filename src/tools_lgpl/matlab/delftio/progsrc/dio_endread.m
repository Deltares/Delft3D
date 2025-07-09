function dio_endread(dsh,diopart)
%DIO_ENDREAD  End reading from DelftIO stream.
%   DIO_ENDREAD(dsh,diopart) ends the reading from the specified DelftIO
%   stream where dsh is a DelftIO stream handle obtained from a DIO_DEFINE
%   call. The diopart argument should read 'header' or 'data'; it defaults
%   to 'data' if not specified. The stream is released for writing by the
%   data providing component.
%
%   See also DIO_DEFINE, DIO_STARTREAD, DIO_READ.

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
else
    diopart = lower(diopart);
    switch diopart
        case {'data','header'}
        otherwise
            error('The diopart argument should read ''header'' or ''data''.')
    end
end
dio_core('endread',dsh,diopart);
