function dioplt_write(PLT,Data)
%DIOPLT_WRITE  Write to a DelftIO PLT stream.
%   DIOPLT_WRITE(PLT,Data) writes data to the specified DelftIO PLT stream
%   where PLT is a DelftIO PLT structure obtained from a DIOPLT_DEFINE
%   call. The function blocks until the data receiving component has read
%   any previous data. The Data array should be a [length(Par) length(Loc)]
%   array where Par and Loc refer to the dimension label arrays specified
%   in the DIOPLT_DEFINE call. The class of the Data may deviate from the
%   type specified for communication; in such cases it will be
%   automatically converted to the appropriate data type before sending.
%
%   See also DIOPLT_DEFINE, DIOPLT_READ.

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

dsh = PLT.dsh;
NPar = length(PLT.Par);
NLoc = length(PLT.Loc);
if ~isequal(size(Data),[NPar NLoc])
    error(sprintf('Invalid data dimension; size should match [%i %i].',NPar,NLoc))
end
switch PLT.DType
    case 1 % Dio_Plt_Integer
        if ~isa(Data,'int32')
            Data = int32(Data);
        end
        if dio_startwrite(dsh,'data')
            dio_write(dsh,Data,'data');
            dio_endwrite(dsh,'data');
        end
    case 2 % Dio_Plt_Real
        if ~isa(Data,'single')
            Data = single(Data);
        end
        if dio_startwrite(dsh,'data')
            dio_write(dsh,Data,'data');
            dio_endwrite(dsh,'data');
        end
    case 3 % Dio_Plt_Double
        if ~isa(Data,'double')
            Data = double(Data);
        end
        if dio_startwrite(dsh,'data')
            dio_write(dsh,Data,'data');
            dio_endwrite(dsh,'data');
        end
    case 4 % Dio_Plt_Logical
        if ~islogical(Data)
            Data = Data~=0;
        end
        if dio_startwrite(dsh,'data')
            dio_write(dsh,int32(Data),'data');
            dio_endwrite(dsh,'data');
        end
end
