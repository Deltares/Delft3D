!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2015-2024.                                
!                                                                               
!  This file is part of Delft3D (D-Flow Flexible Mesh component).               
!                                                                               
!  Delft3D is free software: you can redistribute it and/or modify              
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  Delft3D  is distributed in the hope that it will be useful,                  
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.             
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D",                  
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting 
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!                                                                               
!-------------------------------------------------------------------------------

  select case(var_name)
%for var in variables:
  case("${var['name']}")
     %if var['rank']==0:
         %if var['type']=='char':
     ${var['name']} = ''
     call c_f_pointer(xptr, x_0d_char_ptr, [MAXSTRLEN])
     ${var['name']} = char_array_to_string(x_0d_char_ptr, strlen(x_0d_char_ptr))
         %else:
     call c_f_pointer(xptr, x_${var['rank']}d_${var['type']}_ptr)
         %endif
     %else:
     call c_f_pointer(xptr, x_${var['rank']}d_${var['type']}_ptr, shape(${var['name']}))
     %endif
     ${var['name']}${dimstr(":"*var['rank'])} = x_${var['rank']}d_${var['type']}_ptr
%endfor
  end select
