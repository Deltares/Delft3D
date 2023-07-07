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

!> (re)allocate
subroutine realloc_particles(Nsize, LkeepExisting, ierror)
   use m_particles
   use m_alloc
   use m_missing
   use m_sferic, only: jsferic
   implicit none

   integer, intent(in)  :: Nsize          !< array sizes
   logical, intent(in)  :: LkeepExisting  !< keep existing data (1) or not (0)
   integer, intent(out) :: ierror         !< error (1) or not

   ierror = 1

!  reallocate
   call realloc(xpart, Nsize, keepExisting=LkeepExisting, fill=DMISS)
   call realloc(ypart, Nsize, keepExisting=LkeepExisting, fill=DMISS)
   if ( jsferic.eq.1 ) then
      call realloc(zpart, Nsize, keepExisting=LkeepExisting, fill=DMISS)
   end if
   call realloc(dtremaining, Nsize, keepExisting=LkeepExisting, fill=0d0)
   call realloc(kpart, Nsize, keepExisting=LkeepExisting, fill=0)
   call realloc(iglob, Nsize, keepExisting=LkeepExisting, fill=0)

   call realloc(numzero, Nsize, keepExisting=LkeepExisting, fill=0)
   numzero = 0

   ierror = 0
1234 continue

   return
end subroutine realloc_particles
