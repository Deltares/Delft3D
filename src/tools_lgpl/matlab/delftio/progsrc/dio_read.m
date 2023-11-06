function varargout = dio_read(dsh,varargin)
%DIO_READ  Read from DelftIO stream.
%   [Data1,Data2,...,Flag] = DIO_READ(dsh,Size1,Type1,Size2,Type2,...,diopart)
%   reads data from the specified DelftIO stream where dsh is a DelftIO
%   stream handle obtained from a DIO_DEFINE call. The diopart argument
%   should read 'header' or 'data'; it defaults to 'data' if not specified.
%   The size arguments indicate the size vectors of the output arguments
%   and the type arguments indicate the class of the output arguments.
%   Supported data classes are (u)int8, (u)int16, (u)int32, (u)int64,
%   single (float32), double (float64), logical, logical32, char and char8.
%
%   Notes:
%    * Calls to DIO_READ should be preceded by a DIO_STARTREAD call and
%      followed by a DIO_ENDREAD call.
%    * Reading beyond the buffer limit will produce a warning message and
%      return zero values (use DIO_DIO_GETREMAININGSIZE to check).
%    * Program language interoperability issues: in MATLAB a logical array
%      uses 1 byte per value, a char array uses 2 bytes per character. Use
%      type 'logical32' for compatibility with 4 byte logicals and 'char8'
%      for compatibility with 1 byte characters.
%    * If the data send via DelftIO is not of the class desired for further
%      processing, you may request on-the-fly conversion of the data class
%      by using the 'source=>destination' construct as used by FREAD, e.g.
%      'int8=>char' is an expansion of the 'char8' compatibility class
%      mentioned above. Note that contrary to FREAD, DIO_READ does not
%      convert data to 'double' by default.
%
%   Example
%      [A1,A2] = dio_read(dsh,[1 3],'int32',[1 4],'int32=>double')
%      % reads 7 integers from the data block of the DelftIO stream. The
%      % first three are returned as an int32 variable A1 and the last four
%      % are returned as a double variable A2.
%
%   See also DIO_DEFINE, DIO_STARTREAD, DIO_GETSIZE, DIO_GETREMAININGSIZE.

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

% determine number of data items requested
nIn = length(varargin);
if nIn<2
    error('Not enough input arguments.')
end
if round(nIn/2)==nIn/2
    diopart = 'data';
    nDataRequests = nIn/2;
else
    diopart = varargin{end};
    nDataRequests = (nIn-1)/2;
end

% verify number of output arguments
nv = max(nargout,1);
if nv~=nDataRequests & nv~=nDataRequests+1
    error('Incorrect number of output arguments.')
end
varargout = cell(1,nv);

% read data items
allflag = 1;
for i = 1:nDataRequests
    j = 2*(i-1)+1;
    
    % determine number of values per item
    sz = varargin{j};
    if length(sz)<2
        sz(1,2) = 1;
    end
    nel = prod(sz);
    
    % determine item type
    dtype = lower(varargin{j+1});
    rtype = dtype;
    casting = findstr(dtype,'=>');
    if ~isempty(casting)
        rtype = dtype(casting+2:end);
        dtype = dtype(1:casting-1);
    end
    switch dtype
        case {'uint8','int8','uint16','int16','uint32','int32', ...
                'uint64','int64','single','double','logical','char'}
            % MATLAB supported data classes
        case {'float32','float64'}
            % additional data classes synonyms for
            % single,double
        case 'logical32'
            % special compatibility class with automatic conversion
            dtype = 'int32';
            rtype = 'logical';
        case {'char8','uchar'}
            % special compatibility class with automatic conversion
            dtype = 'uint8';
            rtype = 'char';
        otherwise
            error('Requested data type invalid or not yet implemented.')
    end
    switch rtype
        case {'uint8','int8','uint16','int16','uint32','int32', ...
                'uint64','int64','single','double','logical','char'}
            % MATLAB supported data classes
        case 'int'
            rtype = 'int32';
        case 'float32'
            rtype = 'single';
        case 'float64'
            rtype = 'double';
        otherwise
            error('Requested cast type invalid or not yet implemented.')
    end
    
    % read data
    [out,flag] = dio_core('read',dsh,nel,dtype,diopart);
    if ~isequal(dtype,rtype)
        out = cast_v6(out,rtype);
    end
    varargout{i} = reshape(out,sz);
    allflag = allflag & flag;
end
if nv==nDataRequests+1
    varargout{end} = allflag;
end
