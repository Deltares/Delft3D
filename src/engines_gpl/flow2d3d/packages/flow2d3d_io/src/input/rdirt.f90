subroutine rdirt(lunmd     ,lundia    ,error     ,nrrec     ,mdfrec    , &
               & citdat    ,tstart    ,tstop     ,tzone     ,iitdat    , &
               & julday    ,itstrt    ,itfinish  ,dt        ,tunit     , &
               & gdp       )
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
!    Function: - Initialises global time parameters
!              - Reads run time parameters ITDATE, TUNIT, DT,
!                TSTART, TSTOP & TZONE from the MD-file
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use properties
    use time_module
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer , pointer :: itis
    logical , pointer :: dryrun
    character(10), pointer :: tunitstr
!
! Global variables
!
    integer                    :: iitdat   !!  Reference date for the simulation times. Format: YYYYMMDD
    integer                    :: itfinish !  Description and declaration in inttim.igs
    integer                    :: itstrt   !  Description and declaration in inttim.igs
    integer                    :: julday   !  Description and declaration in inttim.igs
    integer                    :: lundia   !  Description and declaration in inout.igs
    integer                    :: lunmd    !  Description and declaration in inout.igs
    integer                    :: nrrec    !!  Pointer to the record number in the MD-file
    logical      , intent(out) :: error    !!  Flag=TRUE if an error is encountered
    real(fp)                   :: dt       !  Description and declaration in esm_alloc_real.f90
    real(fp)     , intent(out) :: tunit    !  Time scale for time parameters (sec)
    real(fp)                   :: tstart   !  Description and declaration in exttim.igs
    real(fp)                   :: tstop    !  Description and declaration in exttim.igs
    real(fp)     , intent(out) :: tzone    !  Description and declaration in exttim.igs
    character(*)               :: mdfrec   !!  Standard rec. length in MD-file (300)
    character(10)              :: citdat   !!  Reference date for the simulation times. Format: "YYYY-MM-DD"
!
! Local variables
!
    integer                :: ibeg   ! Begin position in the RECORD from where the search for data/record is started 
    integer                :: ic
    integer                :: iday   ! Actual day of IITDAT and CITDAT 
    integer                :: idef   ! Help var. containing default va- lue(s) for integer variable 
    integer                :: iend   ! Last position in the RECORD when the searched data/record is finished 
    integer                :: ier    ! =  0 -> end of record encountered =  1 -> real value found = -1 -> length or number of data is larger then specified by the calling routine 
    integer                :: imonth ! Actual month of IITDAT 
    integer                :: iyear  ! Actual year of IITDAT and CITDAT 
    integer                :: lenc   ! Help var. (length of var. cvar to be looked for in the MD-file) 
    integer                :: lkw
    integer                :: lrec   ! Help var. 
    integer                :: nlook  ! Help var.: nr. of data to look for in the MD-file 
    integer                :: ntrec  ! Help. var to keep track of NRREC 
    logical                :: defaul ! Flag set to YES if default value may be applied in case var. read is empty (ier <= 0, or nrread < nlook) 
    logical                :: dtn
    logical                :: found
    logical                :: lerror ! Flag=TRUE if a local error is encountered 
    logical                :: loldd  ! Flag=TRUE if old CITDAT format 
    logical                :: newkw  ! Logical var. specifying whether a new recnam should be read from the MD-file or just new data in the continuation line 
    logical                :: nodef  ! Flag set to YES if default value may NOT be applied in case var. read is empty (ier <= 0, or nrread < nlook) 
    real(fp)               :: rdef   ! Help var. containing default va- lue(s) for real variable 
    real(fp), dimension(1) :: rval   ! Help array (real) where the data, recently read from the MD-file, are stored temporarily 
    character(1)           :: ctunit ! Help var for time scale
    character(10)          :: cdef   ! Default value when CVAR not found 
    character(100)         :: chulp  ! Help var. 
    character(3)           :: maand  ! Actual month of CITDAT 
    character(48)          :: month  ! Array containing the abbreviated month names. It is used to compare the input data specified in MAAND 
    character(6)           :: keyw   ! Name of record to look for in the MD-file (usually KEYWRD or RECNAM) 
    character(100)         :: errmsg ! String containing errror message 
    character(3)           :: sign   ! Sign of local time zone 
    !
    data month/'jan feb mar apr may jun jul aug sep oct nov dec '/
!
!! executable statements -------------------------------------------------------
!
    itis   => gdp%gdrdpara%itis
    dryrun => gdp%gdtmpfil%dryrun
    tunitstr => gdp%gdexttim%tunitstr
    !
    ! initialize local parameters
    !
    nlook  = 1
    idef   = 0
    rdef   = 0.0
    lerror = .false.
    newkw  = .true.
    defaul = .true.
    nodef  = .not.defaul
    !
    iday   = 0
    imonth = 0
    iyear  = 0
    maand  = ' '
    !
    ! initialize parameters that are to be read
    !
    julday = 0
    dt     = 0.0
    tstart = 0.0
    tstop  = 0.0
    tzone  = 0.0
    citdat = ' '
    !
    ! IniTal DATE
    !
    !
    ! locate 'Itdate' record for initial date
    ! default not allowed
    !
    keyw  = 'Itdate'
    ntrec = nrrec
    lenc  = 100
    cdef  = ' '
    chulp = cdef
    call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
              & mdfrec    ,chulp     ,cdef      ,lenc      ,nrrec     , &
              & ntrec     ,lundia    ,gdp       )
    !
    ! reading error?
    !
    if (lerror) then
       error = .true.
       lerror = .false.
    else
       citdat = chulp(1:10)
       !
       ! test if CITDAT is in old format (DD MMM 'YY) then re-define
       !
       loldd = .false.
       call small(chulp, 100)
       ic = ichar(chulp(4:4))
       if (ic>=97 .and. ic<=122) then
          ic = ichar(chulp(5:5))
          if (ic>=97 .and. ic<=122) then
             ic = ichar(chulp(6:6))
             if (ic>=97 .and. ic<=122) loldd = .true.
          endif
       endif
       !
       ! old format
       !
       if (loldd) then
          !
          ! read IDAY from RECORD(IBEG:IEND)
          ! [] and no value not allowed => IER > 0
          !
          ibeg = 1
          iend = 10
          lrec = iend
          call read1i(chulp     ,lrec      ,ibeg      ,iend      ,iday      , &
                    & idef      ,ier       )
          if (ier <= 0) then
             error = .true.
             call prterr(lundia    ,'V007'    ,keyw      )
             citdat = ' '
             iitdat = 0
             goto 100
          endif
          !
          ! read MAAND from RECORD(IBEG:IEND)
          ! string to long and no value not allowed => IER > 0
          !
          ibeg = iend + 1
          lenc = 4
          call read1c(chulp     ,lrec      ,ibeg      ,iend      ,maand     , &
                    & lenc      ,ier       )
          if (ier <= 0) then
             error = .true.
             call prterr(lundia    ,'V007'    ,keyw      )
             citdat = ' '
             iitdat = 0
             goto 100
          endif
          !
          ! read IYEAR from RECORD(IBEG:IEND)
          ! string to long or default value not allowed => IER > 0
          !
          ibeg = iend + 3
          call read1i(chulp     ,lrec      ,ibeg      ,iend      ,iyear     , &
                    & idef      ,ier       )
          if (ier <= 0) then
             error = .true.
             call prterr(lundia    ,'V007'    ,keyw      )
             citdat = ' '
             iitdat = 0
             goto 100
          endif
          !
          ! calculate IMONTH from MAAND and
          ! calculate IDAY, IMONTH and YEAR to ITDATE, with
          ! IYEAR = 1900 + IYEAR
          !
          imonth = (index(month, maand) + 3)/4
          if (imonth == 0) then
             error = .true.
             call prterr(lundia    ,'V007'    ,keyw      )
             citdat = ' '
             iitdat = 0
             goto 100
          endif
          iitdat = (1900 + iyear)*10000 + imonth*100 + iday
          write (citdat, '(i4.4,a1,i2.2,a1,i2.2)') &
              & 1900 + iyear, '-', imonth, '-', iday
       !
       ! new format
       !
       else
          !
          ! define ITDATE in integer value
          ! - replace all - and / by spaces
          ! - read iyear, imonth, iday free formatted
          !
          do
             ic = index(chulp,'-')
             if (ic == 0) ic = index(chulp,'/')
             if (ic == 0) exit
             chulp(ic:ic) = ' '
          enddo
          read (chulp, *, iostat=ic) iyear,imonth,iday
          if (ic /= 0) then
             iitdat = 0
          else
          iitdat = iyear*10000 + imonth*100 + iday
          endif
          write (citdat, '(i4.4,a1,i2.2,a1,i2.2)') &
              & iyear, '-', imonth, '-', iday
          !
          ! test if iitdat is a legal date: JULDAY = 0 not allowed
          !
          julday = ymd2jul(iitdat)
          if (julday == 0) then
             error = .true.
             call prterr(lundia    ,'V007'    ,keyw      )
             citdat = ' '
             iitdat = 0
          endif
       endif
    endif
  100 continue
    !
    ! Time UNIT
    !
    ctunit = ' '
    call prop_get(gdp%mdfile_ptr, '*', 'Tunit', ctunit)
    if (ctunit == ' ') ctunit = 'M'
    if (ctunit == 'S') then
       tunit = 1.0_fp
       tunitstr = 's'
    elseif (ctunit == 'M') then
       tunit = 60.0_fp
       tunitstr = 'min'
    elseif (ctunit == 'H') then
       tunit = 3600.0_fp
       tunitstr = 'h'
    elseif (ctunit == 'D') then
       tunit = 86400.0_fp
       tunitstr = 'day'
    elseif (ctunit == 'W') then
       tunit = 604800.0_fp
       tunitstr = 'week'
    else
       call prterr(lundia, 'V006', ' ')
       tunitstr = 'UNKNOWN'
    endif
    !
    ! Time step, start time, stop time and time step for time-dependent
    ! input data
    !
    !
    ! locate and read 'Dt' record for time step
    ! default value not allowed => nodef
    !
    keyw  = 'Dt    '
    ntrec = nrrec
    call read2r(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
              & mdfrec    ,rval      ,rdef      ,nodef     ,nrrec     , &
              & ntrec     ,lundia    ,gdp       )
    !
    ! reading error?
    !
    if (lerror) then
       error = .true.
       lerror = .false.
       dt = rdef
    else
       dt = rval(1)
    endif
    dt = max(dt, 1.0E-6_fp)
    !
    ! locate and read 'Tstart' record for start time simulation
    ! (0 := ITDATE)
    ! default value not allowed => NODEF
    !
    keyw  = 'Tstart'
    ntrec = nrrec
    call read2r(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
              & mdfrec    ,rval      ,rdef      ,nodef     ,nrrec     , &
              & ntrec     ,lundia    ,gdp       )
    !
    ! reading error?
    !
    if (lerror) then
       error = .true.
       lerror = .false.
       tstart = rdef
    else
       tstart = rval(1)
    endif
    !
    ! locate and read 'Tstop' record for stop time simulation
    ! default value not allowed => NODEF
    !
    keyw  = 'Tstop '
    ntrec = nrrec
    call read2r(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
              & mdfrec    ,rval      ,rdef      ,nodef     ,nrrec     , &
              & ntrec     ,lundia    ,gdp       )
    !
    ! reading error?
    !
    if (lerror) then
       error = .true.
       lerror = .false.
       tstop = rdef
    else
       tstop = rval(1)
    endif
    !
    ! test times for multiple of DT, DT <= 0 then skip tests
    !
    if (dt <= 0.) then
       call prterr(lundia    ,'U007'    ,'value for Simulation time interval'       )
       error = .true.
    else
       itstrt = nint(tstart/dt)
       if (dtn(itstrt, tstart, dt)) then
          call prterr(lundia    ,'U044'    ,'Start time'         )
          error = .true.
       endif
       itfinish = nint(tstop/dt)
       if (dtn(itfinish, tstop, dt)) then
          call prterr(lundia    ,'U044'    ,'Stop time'          )
          error = .true.
       endif
       if ((itfinish - itstrt) <= 0) then
          itfinish = itstrt
          call prterr(lundia    ,'V041'    ,'Stop time must be >='          )
       endif
       if (fp == sp .and. itfinish > 8388607) then
          !
          ! Limited by incrementing timnow by 0.5 every half time step ( 2**23-1 )
          !
          call prterr(lundia    ,'U021'    ,'Stop time too far away from Itdate for ' // &
              & 'a single precision simulation (should be at most 8388607 time steps)')
          error = .true.
       elseif (fp == hp .and. itfinish > 2147483647) then
          !
          ! Limited by output times written as signed integers to NEFIS files ( 2**31-1 )
          ! Double precision incrementing timnow imposes a second limit at
          ! 4503599627370495 ( 2**52-1 )
          !
          call prterr(lundia    ,'U021'    ,'Stop time too far away from Itdate for a ' // &
              & 'double precision simulation (should be at most 2147483647 time steps)')
          error = .true.
       endif
    endif
    !
    ! locate and read 'Tzone' record for time zone of simulation
    ! default value not allowed => NODEF
    !
    keyw  = 'Tzone '
    ntrec = nrrec
    lkw   = 5
    call search(lunmd     ,lerror    ,newkw     ,nrrec     ,found     , &
              & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , &
              & 'NO'      )
    lerror = .false.
    !
    ! not found ?
    !
    if (found) then
       call read2r(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                 & mdfrec    ,rval      ,rdef      ,nodef     ,nrrec     , &
                 & ntrec     ,lundia    ,gdp       )
       !
       ! reading error?
       !
       if (lerror) then
          error = .true.
          lerror = .false.
          tzone = rdef
       else
          tzone = rval(1)
       endif
    endif
    
    if (tzone<0.0) sign = ' - '
    if (tzone>0.0) sign = ' + ' 
    if (abs(tzone)>12.0) then
        write(errmsg, '(a,a,f5.1,a)') 'Expecting local time zone between -12 and 12 hours, but getting', &
          & sign, abs(tzone), ' hours'
        call prterr(lundia    ,'U021'    ,errmsg    )
    elseif (abs(tzone)>0.0) then  
        write (lundia, '(a,a,f4.1,a)') '*** MESSAGE Local time zone is UTC',sign, abs(tzone), ' hours'
    endif
    !
    ! Locate and read keyword 'DryRun', used for running without actual calculations
    !
    dryrun = .false.
    call prop_get(gdp%mdfile_ptr, '*', 'DryRun', dryrun)
    if (dryrun) then
       call prterr(lundia, 'U190', 'DryRun switched on; running without actual calculations')
       write(*,'(a)') '*** WARNING: DryRun switched on; running without actual calculations'
    endif
end subroutine rdirt
