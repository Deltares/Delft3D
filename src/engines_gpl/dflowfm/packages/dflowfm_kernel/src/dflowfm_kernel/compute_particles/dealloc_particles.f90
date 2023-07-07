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

!> deallocate particle data
subroutine dealloc_particles()
   use m_particles
   implicit none

   if ( allocated(xpart)       ) deallocate(xpart)
   if ( allocated(ypart)       ) deallocate(ypart)
   if ( allocated(zpart)       ) deallocate(zpart)
   if ( allocated(dtremaining) ) deallocate(dtremaining)
   if ( allocated(kpart)       ) deallocate(kpart)
   if ( allocated(iglob)       ) deallocate(iglob)

   if ( allocated(numzero)     ) deallocate(numzero)

   Npart = 0

   if ( allocated(trpart)       ) deallocate(trpart)
   if ( allocated(xrpart)       ) deallocate(xrpart)
   if ( allocated(yrpart)       ) deallocate(yrpart)
   if ( allocated(zrpart)       ) deallocate(zrpart)
   Nrpart = 0

   return
end subroutine dealloc_particles
