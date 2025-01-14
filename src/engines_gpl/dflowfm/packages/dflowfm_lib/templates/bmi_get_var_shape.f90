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

module m_bmi_get_var_shape

implicit none

private

public :: bmi_get_var_shape

contains

  ! return in c memory order
  select case(var_name)

%for variable in variables:
    case("${variable['altname'] or variable['name']}")
%if variable['rank'] == 0:
       shape(1) = 0
%else:
%for dim in range(variable['rank']):
%if "shape" in variable:
       shape(${variable['rank'] - dim}) = ${variable["shape"][dim]}
%else:
       shape(${variable['rank'] - dim}) = size(${variable['name']}, ${dim+1})
%endif
%endfor
%endif
%endfor
  end select

end module m_bmi_get_var_shape
