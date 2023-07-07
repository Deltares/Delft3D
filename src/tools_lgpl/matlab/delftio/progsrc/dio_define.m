function dsh = dio_define(Name,varargin)
%DIO_DEFINE  Create a new DelftIO stream.
%   dsh = DIO_DEFINE(Name,nBytesHeader,nBytesData) defines a new DelftIO
%   stream with specified name and header and data block sizes. The Name
%   should be a char array and the other two input arguments should be
%   positive integers. Either this or next syntax should be used on the
%   writing side (data providing component).
%
%   Notes:
%    * The DelftIO stream is a one-way stream; for two-way communication
%      between two components one needs two DelftIO streams.
%    * Make sure that the buffers are big enough for the data to be send.
%      If the buffer is too small, data will be lost and zero values will
%      be obtained by the data receiving component (warning message will be
%      produced).
%
%   dsh = DIO_DEFINE(Name,nBytesData) defines a new DelftIO stream with
%   zero header block size. The Name should be a char array and the other
%   argument should be a positive integer. Either this or previous syntax
%   should be used on the writing side (data providing component).
%
%   dsh = DIO_DEFINE(Name) connect to a DelftIO stream from the reading
%   side (data receiving component). The Name should be a char array. The
%   function blocks until the data providing component has defined the
%   DelftIO stream as well.
%
%   Example 1
%      % For the data providing component
%      dsh = dio_define('MyDioStream',12,56);
%      if dio_startwrite(dsh,'header')
%         dio_write(dsh,int32(1:3),'header')
%         dio_endwrite(dsh,'header')
%      end
%      if dio_startwrite(dsh,'data')
%         dio_write(dsh,4:10,'data')
%         dio_endwrite(dsh,'data')
%      end
%      pause(1)
%      dio_delete(dsh)
%
%   Example 2
%      % For the data receiving component
%      dsh = dio_define('MyDioStream');
%      if dio_startread(dsh,'header')
%         A1 = dio_read(dsh,[1 3],'int32','header')
%         dio_endread(dsh,'header')
%      end
%      if dio_startread(dsh,'data')
%         A2 = dio_read(dsh,[1 7],'double','data')
%         dio_endread(dsh,'data')
%      end
%      dio_delete(dsh)
%
%   See also DIOPLT_DEFINE, DIO_GETNAME, DIO_READ, DIO_WRITE, DIO_DELETE.

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

if nargin>=2
    % putter
    dsh = dio_core('newput',varargin{:},Name);
elseif nargin==1
    % getter
    dsh = dio_core('newget',Name);
else
    error('Not enough input arguments.')
end