subroutine rdwaqpar(lundia, error, kmax, lsed, dt, itcomf, itcomi, itcoml, gdp)
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
!
! Read waq parameters from mdf file
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use properties
    use globaldata
    use system_utils, only: exifil
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer                       , pointer :: aggre
    integer                       , pointer :: itwqff
    integer                       , pointer :: itwqfi
    integer                       , pointer :: itwqfl
    integer                       , pointer :: itstrt
    integer                       , pointer :: itfinish
    integer       , dimension(:)  , pointer :: ilaggr    ! layer aggregation pointer
    real(fp)      , dimension(:,:), pointer :: quwaq     ! Cumulative qxk
    real(fp)      , dimension(:,:), pointer :: qvwaq     ! Cumulative qyk
    real(fp)      , dimension(:,:), pointer :: qwwaq     ! Cumulative qzk
    real(fp)      , dimension(:,:), pointer :: cumsedflx ! Cumulative sedimentation flux
    real(fp)      , dimension(:,:), pointer :: cumresflx ! Cumulative resuspension flux
    real(fp)      , dimension(:)  , pointer :: discumwaq ! Cumulated sources m3/s*nstep 
    logical                       , pointer :: waqfil
    logical                       , pointer :: waqol
    character(256)                , pointer :: flaggr
!
! Global variables
!
    integer   , intent(in)  :: kmax   !  Description and declaration in esm_alloc_int.f90 
    integer   , intent(in)  :: lsed   !!  Number of sediment fractions
    integer                 :: lundia !  Description and declaration in inout.igs
    integer                 :: itcomf !  Description and declaration in inttim.igs
    integer                 :: itcomi !  Description and declaration in inttim.igs
    integer                 :: itcoml !  Description and declaration in inttim.igs
    logical   , intent(out) :: error  !!  Flag=TRUE if an error is encountered
    real(fp)                :: dt     !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer                                             :: i
    integer                                             :: k
    integer                                             :: ilaggrIndex
    integer        ,dimension(:), allocatable           :: ilaggrInput
    integer                                             :: istat
    real(fp)                                            :: ftwqff
    real(fp)                                            :: ftwqfi
    real(fp)                                            :: ftwqfl
    real(fp)       ,dimension(3)                        :: times
    logical                                  , external :: dtn
    character(3000)                                     :: txtput2      ! usage depends on kmax
    character(30)                                       :: txtput1
    character(300)                                      :: message
!
!! executable statements -------------------------------------------------------
!
    aggre      => gdp%gdwaqpar%aggre
    itwqff     => gdp%gdwaqpar%itwqff
    itwqfi     => gdp%gdwaqpar%itwqfi
    itwqfl     => gdp%gdwaqpar%itwqfl
    itstrt     => gdp%gdinttim%itstrt
    itfinish   => gdp%gdinttim%itfinish
    ilaggr     => gdp%gdwaqpar%ilaggr
    quwaq      => gdp%gdwaqpar%quwaq
    qvwaq      => gdp%gdwaqpar%qvwaq
    qwwaq      => gdp%gdwaqpar%qwwaq
    discumwaq  => gdp%gdwaqpar%discumwaq 
    waqfil     => gdp%gdwaqpar%waqfil
    waqol      => gdp%gdwaqpar%waqol
    flaggr     => gdp%gdwaqpar%flaggr
    !
    error = .false.
    !
    ! If keyword Flwq is in the mdf-file, waqfil = true
    !
    txtput1 = ' '
    call prop_get_string(gdp%mdfile_ptr, '*', 'Flwq', txtput1)
    if (txtput1 == ' ') then
       waqfil = .false.
    else
       waqfil = .true.
    endif
    !
    if (.not.waqfil) return
    !
    ! read output times
    ! Flwq  = 1.4400000e+003  6.0000000e+001  2.8800000e+003
    !
    times = 0.0_fp
    call prop_get(gdp%mdfile_ptr, '*', 'Flwq', times, 3)
    !
    ftwqff = times(1)
    ftwqfi = times(2)
    ftwqfl = times(3)
    !
    ! read vertical aggregation array ilaggr
    !
                  allocate(gdp%gdwaqpar%ilaggr     (kmax), stat=istat)
    if (istat==0) allocate(             ilaggrInput(kmax), stat=istat)
    if (istat/=0) then
       call prterr(lundia, 'U021', 'rdwaqpar: memory alloc error')
       call d3stop(1, gdp)
    endif
    !
    ! Update local pointer to point to newly allocated memory
    !
    ilaggr     => gdp%gdwaqpar%ilaggr
    !
    ! Read value of keyword WaqOL in the mdf-file 
    ! When WaqOL = true, FLOW and WAQ communicate online via WAQ input files 
    ! Default WaqOL = false
    !
    call prop_get_logical(gdp%mdfile_ptr, '*', 'WaqOL', waqol)
    !
    ilaggrInput = 0
    call prop_get(gdp%mdfile_ptr, '*', 'ilaggr', ilaggrInput, kmax)
    if (ilaggrInput(1) == 0) then
       ilaggrInput = 1 ! set ilaggrInput to "all ones" for echo to tri-diag file.
       do i = 1, kmax
          ilaggr(i) = i
       enddo
    else
       !
       ! Test whether the sum of ilaggrInput is equal to kmax
       !
       k = 0
       do i=1,kmax
          if (ilaggrInput(i) < 0) then
             write(message, '(a,i0,a)') 'ilaggr element ', i, &
                 & ' is negative.'
             call prterr(lundia, 'P004', trim(message))
             error = .true.
             call d3stop(1, gdp)
          endif
          k = k + ilaggrInput(i)
       enddo
       if (k /= kmax) then
          write(message, '(a,i0,a,i0,a)') 'The sum of all ilaggr elements (', k, &
              & ') is not equal to the total number of layers (',kmax, ').'
          call prterr(lundia, 'P004', trim(message))
          error = .true.
          call d3stop(1, gdp)
       endif
       !
       ! Transform ilaggrInput into ilaggr
       !
       k           = 0
       ilaggrIndex = 1
       do i=1,kmax
          k = k + 1
          do ilaggrIndex = ilaggrIndex, ilaggrIndex + ilaggrInput(i) - 1
             ilaggr(ilaggrIndex) = k
          enddo
       enddo
    endif
    !
    ! calculate integer multiples of DT and test calculated values
    !
    itwqff = nint(ftwqff/dt)
    itwqfi = nint(ftwqfi/dt)
    itwqfl = nint(ftwqfl/dt)
    !
    if (dtn(itwqff, ftwqff, dt)) then
       error = .true.
       call prterr(lundia    ,'U044'    ,'Waq store start time'          )
    endif
    if (dtn(itwqfi, ftwqfi, dt)) then
       error = .true.
       call prterr(lundia    ,'U044'    ,'Waq store time interval'       )
    endif
    if (itwqfi<=0) then
       error = .true.
       call prterr(lundia    ,'U021'    ,'Waq store time interval should be > 0' )
    endif
    if (dtn(itwqfl, ftwqfl, dt)) then
       error = .true.
       call prterr(lundia    ,'U044'    ,'Waq store stop time')
    endif
    if (mod(itwqfl-itwqff,max(itwqfi,1))/=0) then
       error = .true.
       call prterr(lundia    ,'U021'    ,'Waq store period should be multiple of interval' )
    endif
    if (itwqfl > itfinish) then
       itwqfl = itfinish
       call prterr(lundia, 'U190', 'Waq store end time is set to simulation end time')
    endif
    !
    ! read aggregation file
    ! WaqAgg= #example.dwq#
    !
    flaggr = ' '
    call prop_get_string(gdp%mdfile_ptr, '*', 'WaqAgg', flaggr)
    if (flaggr /= ' ' .and. flaggr /= 'active only') then
       if (.not. exifil(flaggr, lundia)) then
          error = .true.
          call d3stop(1, gdp)
       endif
    endif
    !
    ! echo input in diagnose-file
    !
    write (lundia, '(a)') '*** Start  of waq parameters input'
    !
    txtput1 = 'Write waq output'
    txtput2 = '       Yes'
    write (lundia, '(3a)') txtput1, ':  ', trim(txtput2)
    !
    txtput1 = 'Start at time step'
    write (lundia, '(2a,i12)') txtput1, ':', itwqff
    txtput1 = 'Output every N time steps'
    write (lundia, '(2a,i12)') txtput1, ':', itwqfi
    txtput1 = 'Stop at time step'
    write (lundia, '(2a,i12)') txtput1, ':', itwqfl
    txtput1 = 'Aggregation (horizontal)'
    txtput2 = ' '
    message = ' '
    if (flaggr == ' ') then
       write (message, '(a)') ':        none'
    else
       !
       ! flaggr == 'active only' or flaggr == [filename]
       !
       write (message, '(a)') trim(flaggr)
    endif
    txtput2 = trim(message)
    write (lundia, '(3a)') txtput1, ': ', trim(txtput2)
    txtput1 = 'Aggregation (vertical)'
    txtput2 = ' '
    do i=1,kmax
       if (ilaggr(i) /= 0) then
          message = ' '
          if (i == 1) then
             write(message,'(i0)') ilaggrInput(i)
          elseif (ilaggrInput(i)>0) then ! don't show zeroes at end
             write(message,'(a,i0)') ', ', ilaggrInput(i)
          endif
          txtput2 = trim(txtput2)//trim(message)
       endif
    enddo
    write (lundia, '(3a)') txtput1, ': ', trim(txtput2)
    write (lundia, '(a)') '*** End    of waq parameters input'
    !
    ! When no communication file is written:
    ! Force the communication file to be written for one time step
    ! This is needed by WAQ
    !
    if (itcomi == 0) then
       itcomf = itstrt
       itcomi = 1
       itcoml = itstrt + 1
       call prterr(lundia, 'G051', 'The communication file is written during the first time step only. Needed by WAQ.')
    endif
    write (lundia, *)
    !
    ! Writing binary waq file enabled: allocate the required memory
    !
    istat = 0
    if (.not. associated(gdp%gdwaqpar%quwaq)) then
       !
       ! allocate all arrays needed for writing binary waq files
       !
       if (istat==0) allocate (gdp%gdwaqpar%quwaq    (gdp%d%nmlb:gdp%d%nmub,  kmax)     , stat = istat)
       if (istat==0) allocate (gdp%gdwaqpar%qvwaq    (gdp%d%nmlb:gdp%d%nmub,  kmax)     , stat = istat)
       if (istat==0) allocate (gdp%gdwaqpar%qwwaq    (gdp%d%nmlb:gdp%d%nmub,0:kmax)     , stat = istat)
       if (istat==0) allocate (gdp%gdwaqpar%discumwaq(gdp%d%nsrc)                       , stat = istat)
       if (istat==0) allocate (gdp%gdwaqpar%cumsedflx(gdp%d%nmlb:gdp%d%nmub,  lsed)     , stat = istat)
       if (istat==0) allocate (gdp%gdwaqpar%cumresflx(gdp%d%nmlb:gdp%d%nmub,  lsed)     , stat = istat)
       !
       if (istat /= 0) then
          call prterr(lundia, 'U021', 'RDWAQPAR: memory alloc error')
          call d3stop(1, gdp)
       endif
       !
       ! Update local pointers to point to newly allocated memory
       !
       quwaq      => gdp%gdwaqpar%quwaq
       qvwaq      => gdp%gdwaqpar%qvwaq
       qwwaq      => gdp%gdwaqpar%qwwaq
       discumwaq  => gdp%gdwaqpar%discumwaq 
       cumsedflx  => gdp%gdwaqpar%cumsedflx
       cumresflx  => gdp%gdwaqpar%cumresflx
    endif
    !
    ! Initialize arrays allocated
    !
    quwaq     = 0.0_fp
    qvwaq     = 0.0_fp
    qwwaq     = 0.0_fp
    discumwaq = 0.0_fp
    cumsedflx = 0.0_fp
    cumresflx = 0.0_fp
    deallocate(ilaggrInput)
end subroutine rdwaqpar
