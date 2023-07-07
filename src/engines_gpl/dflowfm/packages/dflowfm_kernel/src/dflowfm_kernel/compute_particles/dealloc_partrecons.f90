!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2023.                                
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

! 
! 

!> deallocate flux_coeffs
subroutine dealloc_partrecons()
   use m_partrecons
   implicit none

   if ( allocated(qe) ) deallocate(qe)

   if ( allocated(u0x) ) deallocate(u0x)
   if ( allocated(u0y) ) deallocate(u0y)
   if ( allocated(u0z) ) deallocate(u0z)
   if ( allocated(alpha) ) deallocate(alpha)

   if ( allocated(ireconst) ) deallocate(ireconst)
   if ( allocated(jreconst) ) deallocate(jreconst)
   if ( allocated(Areconst) ) deallocate(Areconst)

   return
end subroutine dealloc_partrecons
