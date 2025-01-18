module update_waves
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU General Public License as published by         
!  the Free Software Foundation version 3.                                      
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU General Public License for more details.                                 
!                                                                               
!  You should have received a copy of the GNU General Public License            
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.        
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"    
!  are registered trademarks of Stichting Deltares, and remain the property of  
!  Stichting Deltares. All rights reserved.                                     
!                                                                               
!-------------------------------------------------------------------------------
!  
!  
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
use precision_basics
! Module parameters
!
    integer :: luniwp = -1  ! Unibest wave conditions input file unit
    !
contains
!
!
!==============================================================================
subroutine update_wavecond(sr,wavetime)
   use swan_input
   use wave_data
   use table_handles
   !
   implicit none
   !
   type(swan_type)         :: sr
   type(wave_time_type)    :: wavetime
   !
   integer                    :: i
   integer, dimension(7)      :: isdir
   integer                    :: namlen
   integer                    :: nwavec
   integer                    :: nwav
   real                       :: hsig
   real                       :: ms
   real                       :: timhr
   real                       :: tpeak
   real, dimension(7)         :: wavcon
   real                       :: wavedir
   real                       :: winddir
   real                       :: windspeed
   real                       :: zeta
   logical                    :: unibest  = .false.
   logical                    :: wavecon  = .false.
   logical                    :: varbound = .false.
   character(256)             :: filnam
   character(256)             :: errorstring
   character(232)             :: model
   !
   data isdir /0, 0, 1, 0, 0, 0, 1/

   ! Test if UNIBEST md-vwac file exists
   !
   filnam      = ' '
   filnam(1:8) = 'md-vwac.'
   namlen      = min(len(sr%casl) , len(filnam)-8)
   filnam(9:)  = sr%casl(1:namlen)
   inquire (file = filnam, exist = unibest)
   if (unibest) then
      if (luniwp < 0) then
         ! Open md-vwac file first time only
         open (newunit = luniwp, file = filnam)
         call skcoma(luniwp    )
         read (luniwp, '(A)') model
         !
         if (model == 'VARBOUND *(MORSYS/UNIBEST)') then
            varbound = .true.
         endif
         !
         call skcoma(luniwp    )
         read (luniwp, *) nwavec
         !
         if (nwavec /= sr%nttide) then
            write (*, '(4A,I4,2A,I4,A)')                 &
              & ' Error : number of steps on ',          &
              & 'md-unib.', sr%casl, ' ( ', nwavec,         &
              & ') not equal to number ',                &
              & 'of tide steps (', sr%nttide, ')'
            call wavestop(1, ' Error : number of steps on md-unib file not equal to number of tide steps')
         endif
         ! Set options to match UNIBEST input
         !
         call iniswn( sr%nnest, sr)
      endif
   endif
   !
   ! Test if WAVECON file exists
   !
   filnam      = ' '
   filnam(1:8) = 'wavecon.'
   namlen      = min(len(sr%casl) , len(filnam)-8)
   filnam(9:)  = sr%casl(1:namlen)
   inquire (file = filnam, exist = wavecon)
   if (wavecon) then
      ! Set options to match WAVECON input
      call iniswn( sr%nnest, sr)
   endif

   if (sr%timedependent) then
      call update_wavecond_mdw(sr,wavetime)
      !
      timhr = wavetime%timmin/60.0
      !
      if (sr%ts_wl(1)>0) then
         call gettabledata(sr%tseriesfile, sr%ts_wl, sr%zeta(1:1), &
                         & timhr, sr%refjulday, errorstring)
      endif
      if (sr%ts_xv(1)>0) then
         call gettabledata(sr%tseriesfile, sr%ts_xv, sr%ux0(1:1), &
                         & timhr, sr%refjulday, errorstring)
      endif
      if (sr%ts_yv(1)>0) then
         call gettabledata(sr%tseriesfile, sr%ts_yv, sr%uy0(1:1), &
                         & timhr, sr%refjulday, errorstring)
      endif
      if (sr%ts_ws(1)>0) then
         call gettabledata(sr%tseriesfile, sr%ts_ws, sr%wvel(1:1), &
                         & timhr, sr%refjulday, errorstring)
      endif
      if (sr%ts_wd(1)>0) then
         call gettabledata(sr%tseriesfile, sr%ts_wd, sr%wdir(1:1), &
                         & timhr, sr%refjulday, errorstring)
      endif
      !
   elseif (unibest) then
      if (varbound) then
        !call skcoma(luniwp    )
        !read (luniwp, *) dist, hsig, tpeak, wavedir, ms, zeta, windspeed, winddir, windfln
        !sr%wvel     = windspeed
        !sr%wdir     = winddir
        !sr%wfil     = windfln
        !sr%zeta     = zeta
         write(*,*) 'Option varbound not implemented yet!'
         call wavestop(1, 'Option varbound not implemented yet!')
        !call setswn_var(sr%nnest, dist, hsig, tpeak, wavedir, ms, sr%swani, sr%swanr, windfln)
      else
         call skcoma(luniwp    )
         read (luniwp, *) hsig, tpeak, wavedir, ms, zeta, windspeed, winddir
         sr%wvel     = windspeed
         sr%wdir     = winddir
         sr%zeta     = zeta
         call setswn(sr%nnest, hsig, tpeak, wavedir, ms, sr)
      endif
   elseif (wavecon) then
      write(*,'(a)') '  Reading conditions from wavecon file'
      nwav   = 7
      call varcon(filnam, wavetime%timmin, wavcon, isdir, nwav)
      write (*, '(8F10.2)') wavetime%timmin, wavcon
      hsig           = wavcon(1)
      tpeak          = wavcon(2)
      wavedir        = wavcon(3)
      ms             = wavcon(4)
      zeta           = wavcon(5)
      windspeed      = wavcon(6)
      winddir        = wavcon(7)
      sr%wvel        = windspeed
      sr%wdir        = winddir
      sr%zeta        = zeta
      call setswn(sr%nnest, hsig, tpeak, wavedir, ms, sr)
   else
   endif
end subroutine update_wavecond
!
!
!==============================================================================
subroutine update_wavecond_mdw(sr,wavetime)
   use swan_input
   use wave_data
   !
   implicit none
   !
   type(swan_type)         :: sr
   type(wave_time_type)    :: wavetime
   !
   integer                    :: i
   integer                    :: n
   real                       :: timhr
   character(256)             :: errorstring
   type(swan_bnd), pointer    :: bnd

   timhr = wavetime%timmin/60.0
   !
   if (sr%ts_wl(1)>0) then
      call gettabledata(sr%tseriesfile, sr%ts_wl, sr%zeta(1:1), &
                      & timhr, sr%refjulday, errorstring)
   endif
   if (sr%ts_xv(1)>0) then
      call gettabledata(sr%tseriesfile, sr%ts_xv, sr%ux0(1:1), &
                      & timhr, sr%refjulday, errorstring)
   endif
   if (sr%ts_yv(1)>0) then
      call gettabledata(sr%tseriesfile, sr%ts_yv, sr%uy0(1:1), &
                      & timhr, sr%refjulday, errorstring)
   endif
   if (sr%ts_ws(1)>0) then
      call gettabledata(sr%tseriesfile, sr%ts_ws, sr%wvel(1:1), &
                      & timhr, sr%refjulday, errorstring)
   endif
   if (sr%ts_wd(1)>0) then
      call gettabledata(sr%tseriesfile, sr%ts_wd, sr%wdir(1:1), &
                      & timhr, sr%refjulday, errorstring)
   endif
   !
   do i=1,sr%nbound
      bnd => sr%bnd(i)
      !
      if (bnd%ts_hs(1)>0) then
         n = bnd%ts_hs(3)
         call gettabledata(sr%tseriesfile, bnd%ts_hs, bnd%waveheight(1:n), &
                         & timhr, sr%refjulday, errorstring)
         if (n==1) bnd%waveheight=bnd%waveheight(1)
      endif
      !
      if (bnd%ts_tp(1)>0) then
         n = bnd%ts_tp(3)
         call gettabledata(sr%tseriesfile, bnd%ts_tp, bnd%period(1:n), &
                         & timhr, sr%refjulday, errorstring)
         if (n==1) bnd%period=bnd%period(1)
      endif
      !
      if (bnd%ts_wd(1)>0) then
         n = bnd%ts_wd(3)
         call gettabledata(sr%tseriesfile, bnd%ts_wd, bnd%direction(1:n), &
                         & timhr, sr%refjulday, errorstring)
         if (n==1) bnd%direction=bnd%direction(1)
      endif
      !
      if (bnd%ts_ds(1)>0) then
         n = bnd%ts_ds(3)
         call gettabledata(sr%tseriesfile, bnd%ts_ds, bnd%dirspread(1:n), &
                         & timhr, sr%refjulday, errorstring)
         if (n==1) bnd%dirspread=bnd%dirspread(1)
      endif
   enddo
end subroutine update_wavecond_mdw
!
!
!==============================================================================
subroutine iniswn(nnest     , sr)
!
! INISWN modifies input from md-wave.case for variabal wave condition
!        computation with respect to Unibest applications
!
!!--declarations----------------------------------------------------------------
!
    use swan_input
    implicit none
!
! Global variables
!
    type(swan_type)                    :: sr
    integer              , intent(in)  :: nnest
!
! Local variables
!
    integer :: i
    integer :: nbound
    integer :: parread
!
!! executable statements -------------------------------------------------------
!
    nbound = sr%nbound
    do i = 1, nbound
       parread = sr%bnd(i)%parread
       if (parread==2) then
          if (sr%bnd(i)%dsprtype/=1) then
             sr%bnd(i)%dsprtype = 1
             write (*, '(A,A)') ' Warning : choice for degrees',                &
                               & ' modified into power'
          endif
       endif
    enddo
end subroutine iniswn
!
!
!==============================================================================
subroutine setswn(nnest     ,hs        ,per       ,dir       ,dd        , &
                & sr        )
!
! SETSWN modifies the variable wave condition from unibest input file
!        at corresponding memory locations for swan application
!
!--declarations----------------------------------------------------------------
!
    use swan_input
    implicit none
!
! Global variables
!
    type(swan_type)                    :: sr
    integer              , intent(in)  :: nnest
    real                 , intent(in)  :: dd
    real                 , intent(in)  :: dir
    real                 , intent(in)  :: hs
    real                 , intent(in)  :: per
!
! Local variables
!
    integer :: convar
    integer :: i
    integer :: nbound
    integer :: nsect
    integer :: parread
    integer :: sec
    type(swan_bnd), pointer :: bnd
!
!! executable statements -------------------------------------------------------
!
    nbound = sr%nbound
    do i = 1, nbound
       bnd => sr%bnd(i)
       nsect   = bnd%nsect
       convar  = bnd%convar
       parread = bnd%parread
       !
       ! Copied from MORSYS:
       ! To be used?
       !
       ! bndtyp  = sr%bndtyp(i)
       if (convar==1) then
          if (parread==2) then
             bnd%waveheight(1) = hs
             bnd%period    (1) = per
             bnd%direction (1) = dir
             bnd%dirspread (1) = dd
          endif
       else
          do sec = 1, nsect
             if (parread==2) then
                bnd%waveheight(sec) = hs
                bnd%period    (sec) = per
                bnd%direction (sec) = dir
                bnd%dirspread (sec) = dd
             endif
          enddo
       endif
    enddo
end subroutine setswn
!
!
!==============================================================================
subroutine varcon(fname     ,timmin    ,result    ,isdir     ,nres )
!
!     VARCON interpolates record of values from time series file
!     TEKAL format; first column time in minutes
!
    implicit none
!
! Local parameters
!
    integer, parameter :: nresmx = 100
!
! Global variables
!
    integer                 , intent(in)  :: nres    ! number of values on record
    integer, dimension(nres), intent(in)  :: isdir   ! mask array, 0 if scalar, 1 if direction (deg)
    real(hp)                , intent(in)  :: timmin  ! time in minutes
    real   , dimension(nres)              :: result  ! array containing interpolated values
    character(*)            , intent(in)  :: fname   ! filename time series file
!
! Local variables
!
    integer                        :: i
    integer                        :: it
    integer                        :: iuntim
    integer                        :: ncol
    integer                        :: nt
    real                           :: a
    real                           :: b
    real                           :: facrad
    real                           :: pi
    real                           :: t1
    real                           :: t2
    real, dimension(nresmx)        :: buff1
    real, dimension(nresmx)        :: buff2
    character(4)                   :: blname
!
!! executable statements -------------------------------------------------------
!
    pi = 4.*atan(1.)
    facrad = pi/180.
    open (newunit = iuntim, file = fname, err = 999)
  100 continue
    read (iuntim, '(A)') blname
    if (blname(1:1)=='*') goto 100
    read (iuntim, *) nt, ncol
    if (ncol<nres + 1) then
       call wavestop(1, 'Not enough columns in file')
    endif
    read (iuntim, *, err = 998) t1, (buff1(i), i = 1, nres)
    !
    do it = 1, nt - 1
       read (iuntim, *, err = 998) t2, (buff2(i), i = 1, nres)
       if (timmin<=t1) then
          !               before start of series; take first value
          do i = 1, nres
             result(i) = buff1(i)
          enddo
          goto 500
       elseif (timmin>=t2) then
          t1 = t2
          do i = 1, nres
             buff1(i) = buff2(i)
          enddo
       else
          if (t2<=t1) then
             call wavestop(1, ' Varcon - time series not monotonous')
          endif
          b = (timmin - t1)/(t2 - t1)
          a = 1.0 - b
          do i = 1, nres
             if (isdir(i) == 1) then
                !
                ! make sure that we rotate via the shortest route between the two angles
                ! do this by increasing or decreasing the initial angle; keep the target
                ! angle between 0 and 360 degrees.
                !
                if (buff2(i)-buff1(i) > 180.0) then
                   buff1(i) = buff1(i) + 360.0
                elseif (buff1(i)-buff2(i) > 180.0) then
                   buff1(i) = buff1(i) - 360.0
                endif
                !
                ! interpolate
                !
                result(i) = buff1(i)*a + buff2(i)*b
                !
                ! reduce angle to range 0 to 360 degrees
                !
                if (result(i) < 0.0) then
                   result(i) = result(i) + 360.0
                elseif (result(i) > 360.0) then
                   result(i) = result(i) - 360.0
                endif
             else
                !
                ! simple interpolation
                !
                result(i) = buff1(i)*a + buff2(i)*b
             endif
          enddo
          goto 500
       endif
    enddo
    !
    !     after end of series or only one record; take last value
    !
    do i = 1, nres
       result(i) = buff1(i)
    enddo
    !
  500 continue
    !
    close (iuntim)
    return
    !
  998 continue
    write (*, *) ' Varcon - Error reading file ', fname
    call wavestop(1, ' Varcon - Error reading file '//trim(fname))
  999 continue
    write (*, *) ' Varcon - Error opening file ', fname
    call wavestop(1, ' Varcon - Error opening file '//trim(fname))
end subroutine varcon



end module update_waves
