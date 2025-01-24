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

module m_bmi_set_var_slice

implicit none

private

public :: bmi_set_var_slice

contains

  select case(var_name)
%for var in variables:
  case("${var['name']}")
  %if var['rank']==0:
     ! Does not apply to scalar variables
     continue
  %else:
<%
tempvar = 'x_{rank}d_{type}_ptr'.format(rank=var['rank'], type=var['type'])
size_expr =  "(/" + ", ".join("c_count(%s)" % (i+1,) for i in range(var['rank'])) + "/)"
slice_expr = '(' + ",".join(
    "c_start({i})+1:(c_start({i})+c_count({i}))".format(i=i+1)
    for i in range(var['rank'])
) + ')'
%>
     call c_f_pointer(xptr, ${tempvar}, ${size_expr})
     ${var['name']}${slice_expr} = ${tempvar}
  %endif
%endfor
  end select

end module m_bmi_set_var_slice
