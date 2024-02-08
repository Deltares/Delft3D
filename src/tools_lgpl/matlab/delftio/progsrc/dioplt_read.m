function Data = dioplt_read(PLT)
%DIOPLT_READ  Read from a DelftIO PLT stream.
%   Data = DIOPLT_READ(PLT) reads data from the specified DelftIO PLT
%   stream where PLT is a DelftIO PLT structure obtained from a
%   DIOPLT_DEFINE call. The function blocks until the data providing
%   component has written the data. The returned Data will be an array of
%   size [length(Par) length(Loc)] where Par and Loc refer to the dimension
%   label arrays specified in the DIOPLT_DEFINE call by the data providing
%   component. The class of the Data will match the class specified in the
%   same DIOPLT_DEFINE call.
%
%   See also DIOPLT_DEFINE, DIOPLT_WRITE.

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

dsh = PLT.dsh;
if dio_startread(dsh,'data')
    NPar = length(PLT.Par);
    NLoc = length(PLT.Loc);

    switch PLT.DType
        case 1 % Dio_Plt_Integer
            Data = dio_read(dsh,[NPar NLoc],'int32','data');
        case 2 % Dio_Plt_Real
            Data = dio_read(dsh,[NPar NLoc],'single','data');
        case 3 % Dio_Plt_Double
            Data = dio_read(dsh,[NPar NLoc],'double','data');
        case 4 % Dio_Plt_Logical
            Data = logical(dio_read(dsh,[NPar NLoc],'int32','data'));
    end
    dio_endread(dsh,'data');
end

if isfield(PLT,'RType')
    Data = cast_v6(Data,PLT.RType);
end



