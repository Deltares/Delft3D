function cc=getlds(sds)
%GETLDS Get LDS information from SDS file.
%   C=GETLDS(SDS) reads the LDS distription from the SDS file (if available)
%   and returns the array as a cell string. The input argument SDS is a data
%   structure as obtained from the WAQUA('open',...) command.
%
%   See also WAQUA

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

a4=waqua('read',sds,'','WAQUA_LDS_DLDS');
nc=waqua('read',sds,'','WAQUA_LDS_NUMINT');
cc=cell(length(nc),1);
offset = 0;
for i=1:length(nc)
   nint = nc(i);
   nchar = a4(offset+1);
   cc{i} = deblank(int2char(a4(offset+(2:nint))));
   offset = offset+nint;
end

function c=int2char(a4)
%INT2CHAR Routine to convert integers to characters

c4=mod(a4,256);
a3=(a4-c4)/256;
c3=mod(a3,256);
a2=(a3-c3)/256;
c2=mod(a2,256);
a1=(a2-c2)/256;
c1=mod(a1,256);
c=char([c1 c2 c3 c4])';
c=c(:)';
