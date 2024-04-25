function PLT = dioplt_define(Name,Par,Loc,TypeStr)
%DIOPLT_DEFINE  Create a DelftIO PLT stream.
%   PLT = DIOPLT_DEFINE(Name,Par,Loc,TypeStr) defines a so-called DelftIO
%   Parameter-Location-Time stream. The Name should be a char array, the
%   Par argument should be a cell array of parameter labels, the Loc
%   argument should be a cell array of location labels, the TypeStr
%   argument should be 'int32', 'single', 'double' or 'logical'. This call
%   should be used by the data providing side. The returned PLT structure
%   contains the DelftIO stream handle and the parameter and location
%   labels as well as a data type indicator. Parameter and location strings
%   are limited to 80 and 132 characters respectively.
%
%   PLT = DIOPLT_DEFINE(Name) connects to a DelftIO PLT stream as the data
%   receiving component. The Name should be a char array. The returned PLT
%   structure contains the DelftIO stream handle and the parameter and
%   location labels as well as a data type indicator. The function blocks
%   until the data providing component has performed the matching
%   DIOPLT_DEFINE call.
%
%   PLT = DIOPLT_DEFINE(Name,TypeStr) connects to a DelftIO PLT stream as
%   the data receiving component with additional on-the-fly conversion of
%   the data received to the indicated class. Supported data classes are
%   (u)int8, (u)int16, (u)int32, (u)int64, single (float32), double
%   (float64), logical and char.
%
%   Note: The DelftIO stream name will be reduced to lower case and some
%   characters, namely point, colon, forward and backward slashes, will be
%   replaced by underscores.
%   
%   Example 1
%      % For the data providing component
%      Par = {'Parameter 1','Parameter 2'};
%      Loc = {'Location 1','Location 2','Location 3'};
%      plt = dioplt_define('MyDioPltStream',Par,Loc,'double');
%      for t = 1:10
%         dioplt_write(plt,rand(2,3));
%      end
%      pause(1)
%      dioplt_delete(plt)
%
%   Example 2
%      % For the data receiving component
%      plt = dioplt_define('MyDioPltStream');
%      NPar = length(plt.Par);
%      NLoc = length(plt.Loc);
%      for t = 1:10
%         Data = dioplt_read(plt);
%         fprintf([repmat('%15s ',1,NLoc+1) '\n'],' ',plt.Loc{:})
%         for p = 1:NPar
%            fprintf(['%-15s',repmat(' %15g',1,NLoc) '\n'], ...
%               plt.Par{p},Data(p,:))
%         end
%         fprintf('\n')
%      end
%      dioplt_delete(plt)
%
%   See also DIO_DEFINE, DIOPLT_WRITE, DIOPLT_READ.

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

% The following quantities have been defined in dio-plt-rw.f90
DioMaxParLen = 80;
DioMaxLocLen = 132;

name = lower(Name);
name(name=='.') = '_';
name(name==':') = '_';
name(name=='/') = '_';
name(name=='\') = '_';

if nargin==4
    switch TypeStr
        case 'int32'
            DType = 1;
            nBytes = 4;
        case 'single'
            DType = 2;
            nBytes = 4;
        case 'double'
            DType = 3;
            nBytes = 8;
        case 'logical'
            DType = 4;
            nBytes = 4;
        otherwise
            error(sprintf('DelftIO transfer for data of class %s not implemented.',TypeStr))
    end

    NPar = length(Par);
    NLoc = length(Loc);
    headerSize = 12+NPar*DioMaxParLen+NLoc*DioMaxLocLen;
    dataSize = nBytes*NPar*NLoc;

    dsh = dio_define(name,headerSize,dataSize);

    if dio_startwrite(dsh,'header')

        dio_write(dsh,int32([NPar NLoc]),'header');

        ParArray = strvcat(Par);
        if size(ParArray,2)<DioMaxParLen
            ParArray(1,DioMaxParLen)=' ';
        elseif size(ParArray,2)>DioMaxParLen
            ParArray = ParArray(:,1:DioMaxParLen);
        end
        ParArray = ParArray';

        LocArray = strvcat(Loc);
        if size(LocArray,2)<DioMaxLocLen
            LocArray(1,DioMaxLocLen)=' ';
        elseif size(LocArray,2)>DioMaxLocLen
            LocArray = LocArray(:,1:DioMaxLocLen);
        end
        LocArray = LocArray';

        dio_write(dsh,int8(ParArray),'header');
        dio_write(dsh,int8(LocArray),'header');

        dio_write(dsh,int32(DType),'header');
        dio_endwrite(dsh,'header');
    end
elseif nargin==1 | nargin==2
    if nargin==2
        RType = lower(Par);
        switch RType
            case {'uint8','int8','uint16','int16','uint32','int32', ...
                    'uint64','int64','single','double','logical','char'}
            case 'float32'
                RType = 'single';
            case 'float64'
                RType = 'double';
            otherwise
                error('Requested data type invalid or not yet implemented.')
        end
    end
    dsh = dio_define(name);
    if dio_startread(dsh,'header')

        N = double(dio_read(dsh,2,'int32','header'));
        NPar = N(1);
        NLoc = N(2);

        Par = cellstr(char(dio_read(dsh,[DioMaxParLen NPar],'int8','header'))');
        Loc = cellstr(char(dio_read(dsh,[DioMaxLocLen NLoc],'int8','header'))');

        DType = double(dio_read(dsh,1,'int32','header'));
        dio_endread(dsh,'header');
    end
else
    error('Incorrect number of input arguments.')
end
PLT.dsh = dsh;
PLT.Par = Par;
PLT.Loc = Loc;
PLT.DType = DType;
if nargin==2
    PLT.RType = RType;
end