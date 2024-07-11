!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2024.                                
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

! =================================================================================================
! =================================================================================================
   subroutine get_netlinks_of_dryarea()
      use network_data   , only: numl, lne
      use fm_external_forcings_data, only: kdryarea, nDryLinks

      implicit none
      integer :: L, k1, k2

      if (allocated(kdryarea) ) deallocate( kdryarea )
      allocate( kdryarea(numl) ) ; kdryarea = 0

      nDryLinks = 0
      do L = 1,numl
         k1 = lne(1,L) ; k2 = lne(2,L)
         if (k1 > 0 .and. k2 > 0) cycle
         if (k1 <= 0 .and. k2 <= 0) cycle
         nDryLinks = nDryLinks + 1
         kdryarea(nDryLinks) = L
      enddo

   end subroutine get_netlinks_of_dryarea
