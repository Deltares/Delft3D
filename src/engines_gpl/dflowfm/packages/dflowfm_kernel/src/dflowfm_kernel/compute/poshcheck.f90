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

 subroutine poshcheck(key)
 use m_flow                                          ! when entering this subroutine, s1=s0, u1=u0, etc
 use m_flowgeom
 use m_flowtimes
 use m_partitioninfo
 use m_timer
 use unstruc_display, only: jaGUI

 implicit none

 integer :: key

 integer :: n, L, LL, LLL, jaHuChanged

 integer, dimension(2) :: idum
 double precision      :: dtrsh=0d-0

 Nodneg = 0 ; key = 0

 if ( jaGUI.eq.1 ) then
      call setcol(221) ! white
 end if

 if ( testdryflood == 1 ) then
    !
    ! In this test implementation the algoritm of Delft3D-FLOW is applied to prevent very thin layers 
    ! 
    dtrsh = max(1d-9, min(epshu, 1d-3))
 endif
 
 if (jposhchk == 0) return

 jaHuChanged = 0
 do n = 1,ndxi                                                  ! check result

    
    if (abs(kfs(n)) /= 0) then ! Also check ghost nodes for posh/setbacks
       if ( s1(n) < bl(n) + dtrsh ) then
           if ( s1(n) < bl(n) + dtrsh - 1d-10 ) then                     ! if ( s1(n) < bl(n) ) then

              nodneg = n ; numnodneg = numnodneg + 1
              if ( jaGUI.eq.1 ) then
                 call rcirc( xz(n), yz(n) )
              end if

              if (jposhchk == -1) then                           ! only detect dry cells and return (for Nested Newton restart)
                 key = 2
              else if (jposhchk == 1) then                       ! only timestep reduction
                 key = 2                                         ! flag redo timestep
                 exit

              else if (jposhchk == 2 .or. jposhchk == 3) then    ! set dry all attached links

                 key = 2                                         ! flag redo setkfs

                 do LL  = 1, nd(n)%lnx
                    L   = iabs(nd(n)%ln(LL))
                    hu(L) = 0d0
                    jaHuChanged = 1
                 enddo

              else if (jposhchk == 4 .or. jposhchk == 5) then    ! reduce links au

                 do LL  = 1, nd(n)%lnx
                    LLL = nd(n)%ln(LL); L = iabs(LLL)
                    if (hu(L) > 0) then
                       au(L) = 0.2d0*au(L)
                       if (au(L) < eps6) then
                          hu(L) = 0d0 ;  key = 2                 ! flag redo setkfs
                          jaHuChanged = 1
                       endif
                    endif
                 enddo

              else if (jposhchk == 6 .or. jposhchk == 7) then    ! only set dry outflowing links

                 do LL  = 1, nd(n)%lnx
                    LLL = nd(n)%ln(LL); L = iabs(LLL)
                    if (LLL < 0 .and. u1(L) > 0 .or. &
                        LLL > 0 .and. u1(L) < 0 ) then
                        hu(L) = 0d0   ; key = 2                  !  flag redo setkfs
                        jaHuChanged = 1
                    endif
                 enddo


              endif


           endif

           if (jamapFlowAnalysis > 0) then
              negativeDepths(n) = negativeDepths(n) + 1
           end if

           s1(n) = bl(n)
        endif
    endif

 enddo

 if ( jampi.eq.1 ) then
!   reduce nodneg and key
    idum = (/ key, nodneg /)

    if ( jatimer.eq.1 ) call starttimer(IMPIREDUCE)
!    call reduce_key(key)
    call reduce_int_max(2,idum)
    if ( jatimer.eq.1 ) call stoptimer(IMPIREDUCE)

    key = idum(1)
    nodneg = idum(2)
 end if

 if (nodneg /= 0 .and. jposhchk /= -1) then
    if (jposhchk == 1 .or. jposhchk == 3   .or. jposhchk == 5 .or. jposhchk == 7) then
        dts = 0.7d0*dts
    endif
    dsetb  = dsetb + 1                               ! total nr of setbacks
    s1     = s0
    vol1 = vol0
    if (dts .lt. dtmin) then
        s1 = max(s1,bl)                              ! above bottom
        call okay(0)
        key = 1                                      ! for easier mouse interrupt
    endif
 endif

 ! If hu is changed, then re-fill array onlyWetLinks
 if (jaHuChanged > 0) then
    call fill_onlyWetLinks()
 end if

 end subroutine poshcheck
