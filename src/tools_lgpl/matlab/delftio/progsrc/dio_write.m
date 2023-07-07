function dio_write(dsh,varargin)
%DIO_WRITE  Write to DelftIO stream.
%   DIO_WRITE(dsh,data1,data2,data3,...,diopart) writes data to the
%   specified DelftIO stream where dsh is a DelftIO stream handle obtained
%   from a DIO_DEFINE call. The diopart argument should read 'header' or
%   'data'; it defaults to 'data' if not specified. The other input
%   arguments may be arbitrary arrays of classes (u)int8, (u)int16,
%   (u)int32, (u)int64, single (float32), double (float64), logical and
%   char.
%
%   Notes:
%    * Calls to DIO_WRITE should be preceded by a DIO_STARTWRITE call and
%      followed by a DIO_ENDWRITE call.
%    * Writing beyond the buffer limit will produce a warning message and
%      that data will not be send (use DIO_GETREMAININGSIZE to check).
%    * Program language interoperability issues: in MATLAB a logical array
%      uses 1 byte per value, a char array uses 2 bytes per character.
%      Convert logicals and chars to appropriate precision integer arrays
%      before sending data to components written in languages based on
%      other conventions.
%    * Note: if your last data array might equal 'data' or 'header' be sure
%      to include the optional diopart or your data will be misinterpreted
%      for the optional diopart argument.
%
%   Example
%      X = 1:7;
%      dio_write(dsh,int32(X),'data')
%      % writes 7 integers to the data block of the DelftIO stream.
%
%   See also DIO_DEFINE, DIO_STARTWRITE, DIO_GETSIZE, DIO_GETREMAININGSIZE.

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

diopart = 'data';
NData = length(varargin);
if isequal(varargin{end},'header') | isequal(varargin{end},'data')
    diopart = varargin{end};
    NData = NData-1;
end

for i=1:NData
    dio_core('write',dsh,varargin{i},diopart);
end
