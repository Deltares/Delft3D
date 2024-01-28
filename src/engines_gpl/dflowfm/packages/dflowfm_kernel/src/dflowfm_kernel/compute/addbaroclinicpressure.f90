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

subroutine addbaroclinicpressure()
use m_flowgeom
use m_flow
use m_flowtimes

use m_transport, only: NUMCONST, ISALT, ITEMP, ISED1, ISEDN, ITRA1, ITRAN, ITRAN0, constituents

implicit none
integer                    :: L,LL,Lb,Lt,n, k, lnxbc

if (jabarocterm==0) return

if (jabarocponbnd == 0) then 
   lnxbc = lnxi
else
   lnxbc = lnx             
endif

if (jabarocterm == 1) then

   !$OMP PARALLEL DO       &
   !$OMP PRIVATE(LL,Lb,Lt)

   do LL = 1,lnxbc
      if (hu(LL) == 0d0) cycle
      call getLbotLtop(LL,Lb,Lt)
      if (Lt < Lb) then
          cycle
      endif
      call addbaroc(LL,Lb,Lt)
   enddo

   !$OMP END PARALLEL DO

else if (jabarocterm == 2 .or. jabarocterm == 3 .or. kmx == 0) then

   !$OMP PARALLEL DO       &
   !$OMP PRIVATE(LL,Lb,Lt)
   do LL = 1,lnxbc
      if (hu(LL) == 0d0) cycle
      call getLbotLtop(LL,Lb,Lt)
      if (Lt < Lb) then
          cycle
      endif
      call addbaroc2(LL,Lb,Lt)
    enddo
    !$OMP END PARALLEL DO

 else

    rvdn = 0d0 ; grn = 0d0

    if (jabaroczlaybed == 0) then       ! org now back for full backward compat.

       !$OMP PARALLEL DO       &
       !$OMP PRIVATE(n)
       do n = 1,ndx
          call addbarocnorg(n)
       enddo
       !$OMP END PARALLEL DO
    
       !$OMP PARALLEL DO       &
       !$OMP PRIVATE(LL,Lb,Lt)
       do LL = 1,lnxbc
         if (hu(LL) == 0d0) cycle
         call getLbotLtop(LL,Lb,Lt)
         if (Lt < Lb) then
             cycle
         endif
         call addbarocLorg(LL,Lb,Lt)
       enddo
       !$OMP END PARALLEL DO

    else                                ! these are the routines we want to keep if all ink is dry

       if (jarhointerfaces == 1) then   

          !$OMP PARALLEL DO       &
          !$OMP PRIVATE(n)
          do n = 1,ndx
             call addbarocnrho_w(n)
          enddo
          !$OMP END PARALLEL DO

          !$OMP PARALLEL DO       &
          !$OMP PRIVATE(LL,Lb,Lt)
          do LL = 1,lnxbc
             if (hu(LL) == 0d0) cycle
             call getLbotLtop(LL,Lb,Lt)
             if (Lt < Lb) then
                 cycle
             endif
             call addbarocLrho_w(LL,Lb,Lt)
          enddo
          !$OMP END PARALLEL DO

       else

          !$OMP PARALLEL DO       &
          !$OMP PRIVATE(n)
          do n = 1,ndx
             call addbarocn(n)
          enddo
          !$OMP END PARALLEL DO
    
          !$OMP PARALLEL DO       &
          !$OMP PRIVATE(LL,Lb,Lt)
          do LL = 1,lnxbc
             if (hu(LL) == 0d0) cycle
             call getLbotLtop(LL,Lb,Lt)
             if (Lt < Lb) then
                cycle
             endif
             call addbarocL(LL,Lb,Lt)
          enddo
          !$OMP END PARALLEL DO
  
       endif

    endif
  
 endif

 end subroutine addbaroclinicpressure
