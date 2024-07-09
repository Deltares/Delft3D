!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2022.                                
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

 subroutine step_reduce_hydro(key)                   ! do a flow timestep dts guus, reduce once, then elimin conjugate grad substi
 use m_flow                                          ! when entering this subroutine, s1=s0, u1=u0, etc
 use m_flowgeom
 use Timers
 use m_flowtimes
 use m_sferic
 use m_wind
 use m_reduce
 use m_ship
 use m_partitioninfo
 use m_timer
 use m_xbeach_data
 use MessageHandling
 use m_sobekdfm
 use unstruc_display
 use m_waves
 use m_1d2d_fixedweirs, only : compute_1d2d_fixedweirs, set_discharge_on_1d2d_fixedweirs, compfuru_1d2d_fixedweirs, check_convergence_1d2d_fixedweirs

 implicit none

 integer :: ndraw
 COMMON /DRAWTHIS/  ndraw(50)

 integer            :: key, jposhchk_sav, LL, L, k1,k2, itype
 integer            :: ja, k, ierror, n, kt, num, js1, noddifmaxlevm, nsiz
 character (len=40) :: tex
 logical            :: firstnniteration, last_iteration
 double precision   :: wave_tnow, wave_tstop, t0, t1, dif, difmaxlevm
 double precision   :: hw,tw, uorbi,rkw,ustt,hh,cs,sn,thresh

 character(len=128) :: msg

!-----------------------------------------------------------------------------------------------
 numnodneg = 0
 if (wrwaqon.and.allocated(qsrcwaq)) then
    qsrcwaq0 = qsrcwaq ! store current cumulative qsrc for waq at the beginning of this time step
 end if

 111 continue

 time1    = time0 + dts                               ! try to reach time1
 dti      = 1d0/dts
 nums1it  = 0
 nums1mit = 0
 dnums1it = 0
 firstnniteration = .true.                            !< Flag for first Nested Newton iteration. Only in case of negative depths
                                                      !< firstnniteration is set to .false.

 !call flow_set external forcingsonboundaries(time1) ! set boundary conditions for time that you attempt to reach, every step
                                                     ! should formally be at this position if setbacks occur
                                                     ! this may howver cause a problem for some boundary routines that do not
                                                     ! allow for subsequent calls at decreasing time
                                                     ! In that case put this in initimestep and accept non smooth bndc's



!-----------------------------------------------------------------------------------------------
 hs = max(hs,0d0)
 call furu()                                             ! staat in s0

 if ( itstep.ne.4 ) then                                 ! implicit time-step

   222 if (nonlin == 2 .or. (nonlin ==3 .and. .not. firstnniteration)) then                               ! only for pressurised
       ! Nested newton iteration, start with s1m at bed level.
       s1m = bl !  s1mini
       call volsur()
       difmaxlevm = 0d0 ;  noddifmaxlevm = 0
    endif

333 call s1ini()
    call pack_matrix()

 !-----------------------------------------------------------------------------------------------

444 call s1nod()                                        ! entry point for non-linear continuity
    if (ifixedWeirScheme1d2d == 1) then
       if (last_iteration) then 
          ! Impose previously computed 1d2d discharge on 1d2d fixed weirs to keep mass conservation
          call set_discharge_on_1d2d_fixedweirs()
       else
          call compute_1d2d_fixedweirs()
       endif
    endif

    call solve_matrix(s1, ndx,itsol)                    ! solve s1

    ! if (NDRAW(18) > 1) then
    !    nsiz = ndraw(18)-1
    !    call tekrai(nsiz,ja)
    !    call toemaar()
    ! endif

!    synchronise all water-levels
    if ( jampi == 1 ) then
       if ( jatimer == 1 ) call starttimer(IUPDSALL)
       itype = merge(ITYPE_SALL, ITYPE_Snonoverlap, jaoverlap == 0)
       call update_ghosts(itype, 1, Ndx, s1, ierror)
       if ( jatimer == 1 ) call stoptimer(IUPDSALL)
    end if
    
    if (firstnniteration .and. nonlin1D >=3) then
       ! At first try only check for positive water depths only
       ! Temporarily save the current JPOSCHK value
       jposhchk_sav = jposhchk
       jposhchk = -1
    endif

    call poshcheck(key)                                 ! s1 above local bottom? (return through key only for easier interactive)


    if (firstnniteration .and. nonlin1D >=3) then
       ! reset JPOSCHK to original value
       jposhchk = jposhchk_sav
    endif

    if (key == 1) then
       return                                           ! for easier mouse interrupt
    else if (key == 2 ) then
       if (nonlin1D >= 3 .and. firstnniteration) then   ! jposhcheck==-1
         ! Negative depth(s): retry with restarted Nested Newton
         firstnniteration = .false.
         goto 222
       endif

       if (wrwaqon.and.allocated(qsrcwaq)) then
          qsrcwaq = qsrcwaq0                            ! restore cumulative qsrc for waq from start of this time step to avoid
       end if                                           ! double accumulation and use of incorrect dts in case of time step reduction
       call setkfs()
       if (jposhchk == 2 .or. jposhchk == 4) then       ! redo without timestep reduction, setting hu=0 => 333 s1ini
          if (nonlin >= 2) then
             goto 222
          else
             goto 333
          endif
       else
          if ( jampi == 1 .and. my_rank == 0) call mess(LEVEL_WARN, 'Redo with timestep reduction.')
          goto 111
       endif
    endif

 else
    s1 = s0
 end if

 if (nonlin >=2) then
    ! In case the water levels drop, s1m must be adjusted to the water level. Nested Newton assumes s1(k) >= s1m(k).
    do k = 1,ndx
       if (s1(k) < s1m(k)) then
          s1m(k) = s1(k)
       endif
    enddo
 endif

 call volsur()

 if (nonlin > 0) then

    difmaxlev = 0d0 ; noddifmaxlev = 0

    do k = 1,ndx
       dif = abs(s1(k)-s00(k))

       if (dif  > difmaxlev ) then
          difmaxlev    = dif
           noddifmaxlev = k
       endif
       s00(k) = s1(k)
    enddo

    nums1it   = nums1it + 1

    if (nums1it > maxNonlinearIterations) then
       if (jamapFlowAnalysis > 0) then
          noiterations(noddifmaxlev) = noiterations(noddifmaxlev) + 1
       end if

       write(msgbuf, '(''No convergence in nonlinear solver at time '', g10.5,'' (s), time step is reduced from '', f8.4, '' (s) into '', f8.4, '' (s)'')') time0, dts, 0.5d0*dts
       !if (nonlin1D == 2) then
       !   ! Nested Newton
       !   !call err_flush()
       !else
          call warn_flush()
          dts = 0.5d0*dts
          dsetb  = dsetb + 1                               ! total nr of setbacks
          s1     = s0
          if (dts .lt. dtmin) then
              s1 = max(s1,bl)                              ! above bottom
              call okay(0)
              key = 1                                      ! for easier mouse interrupt
              return
          endif
          call setkfs()
          goto 111                                      ! redo with timestep reduction => 111 furu
       !endif
    endif

    !if (nums1it > 10) then
    !   write(tex,*) difmaxlev
    !   call gtext(tex,xz(noddifmaxlev), yz(noddifmaxlev), nums1it)
    !   call toemaar()
    !endif

    if ( jampi.eq.1 ) then
       if ( jatimer.eq.1 ) call starttimer(IMPIREDUCE)
       call reduce_double_max(difmaxlev)
       if ( jatimer.eq.1 ) call stoptimer (IMPIREDUCE)
    end if

    if ( difmaxlev > epsmaxlev) then
       ccr = ccrsav                                   ! avoid redo s1ini, ccr is altered by solve
       goto 444                                       ! standard non-lin iteration s1 => 444
    else if (ifixedweirscheme1d2d==1 .and. .not. last_iteration) then
       if (check_convergence_1d2d_fixedweirs()) then
          last_iteration = .true. 
       end if
       ccr = ccrsav                                ! avoid redo s1ini, ccr is altered by solve
       goto 444                                    ! when s1 .ne. s1m again do outer loop
       
    endif

    ! beyond or past this point s1 is converged

     if (nonlin >= 2) then
       difmaxlevm = 0d0 ;  noddifmaxlevm = 0
       do k = 1,ndx
          dif = abs(s1m(k)-s1(k))
          if (dif  > difmaxlevm ) then
             difmaxlevm    = dif
             noddifmaxlevm = k
          endif
          s1m(k) = s1(k)                              ! s1m starts at value of converged inner loop s1
       enddo

       if (difmaxlevm > epsmaxlevm ) then
          nums1mit = nums1mit + 1
          call volsur()
          ccr = ccrsav                                ! avoid redo s1ini, ccr is altered by solve
          goto 444                                    ! when s1 .ne. s1m again do outer loop
       endif

    endif

    dnums1it = dnums1it + nums1it
 endif

 end subroutine step_reduce_hydro
