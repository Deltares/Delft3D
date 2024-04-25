subroutine rdiwe(lunmd     ,lundia    ,error     ,nrrec     ,mdfrec    , &
               & dt        ,itiwei    ,tinciw    ,kmxt      ,kmxdt     , &
               & nfreqs    ,npiwe     ,filiwe    ,gdp       )
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
!    Function: - Reads the information of the attribute file
!                dimension for Internal Wave Energy
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use dfparall
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    real(fp) , pointer :: wvlbed
    real(fp) , pointer :: wvlsur
    real(fp) , pointer :: frcbed
    real(fp) , pointer :: frcsur
    real(fp) , pointer :: siglim
    real(fp) , pointer :: clu
    integer  , pointer :: mwriwe
    integer  , pointer :: nwriwe
    logical  , pointer :: iwedia
    integer  , pointer :: mfg
    integer  , pointer :: nfg
!
! Global variables
!
    integer                   :: itiwei !  Description and declaration in inttim.igs
    integer                   :: kmxdt  !  Description and declaration in dimens.igs
    integer                   :: kmxt   !  Description and declaration in dimens.igs
    integer                   :: lundia !  Description and declaration in inout.igs
    integer                   :: lunmd  !  Description and declaration in inout.igs
    integer                   :: nfreqs !  Description and declaration in dimens.igs
    integer                   :: npiwe  !  Description and declaration in dimens.igs
    integer                   :: nrrec  !!  Record counter keeping the track of the last record read
    logical     , intent(out) :: error  !!  Flag=TRUE if an error is encountered
    real(fp)                  :: dt     !  Description and declaration in esm_alloc_real.f90
    real(fp)                  :: tinciw !!  Time in UNIT's to activate the Internal Wave Energy calculation
    character(*)              :: filiwe !!  File name for the IWE parameters
    character(*)              :: mdfrec !!  Record read from the MD-file
!
! Local variables
!
    integer                        :: ibeg
    integer                        :: idef   ! Default value if no input found 
    integer                        :: iend
    integer                        :: ier
    integer                        :: iocond
    integer                        :: it
    integer                        :: k
    integer                        :: lenc   ! Help var. (length of var. cvar to be looked for in the MD-file) 
    integer                        :: lfile  ! Number of non blank characters of file name 
    integer                        :: luntmp ! Unit number of (input par. for) FILIWE 
    integer                        :: newlun
    integer                        :: nlook  ! Help var.: nr. of data to look for in the MD-file 
    integer                        :: ntrec  ! Help. var to keep track of NRREC 
    logical                        :: dtn
    logical                        :: ex
    logical                        :: lerror ! Flag=TRUE if an error is encountered 
    logical                        :: newkw  ! Logical var. specifying whether a new recnam should be read from the MD-file or just new data in the continuation line 
    real(fp)                       :: t
    real(fp)       , dimension(6)  :: rdef   ! Default value for real variable read 
    real(fp)       , dimension(6)  :: rval   ! Help array (real) where the data, recently read from the MD-file, are stored temporarily 
    character(132)                 :: rec132 ! Record read from attribute file 
    character(6)                   :: keyw   ! Name of record to look for in the MD-file (usually KEYWRD or RECNAM) 
    character(80)                  :: cdef   ! Default value when FILIWE not found 
!
!! executable statements -------------------------------------------------------
!
    mfg     => gdp%gdparall%mfg
    nfg     => gdp%gdparall%nfg
    clu     => gdp%gdiwearr%clu
    mwriwe  => gdp%gdiwearr%mwriwe
    nwriwe  => gdp%gdiwearr%nwriwe
    iwedia  => gdp%gdiwearr%iwedia
    wvlbed  => gdp%gdiwepar%wvlbed
    wvlsur  => gdp%gdiwepar%wvlsur
    frcbed  => gdp%gdiwepar%frcbed
    frcsur  => gdp%gdiwepar%frcsur
    siglim  => gdp%gdiwepar%siglim
    !
    lerror = .false.
    newkw = .true.
    nlook = 1
    cdef  = ' '
    !
    ! Initialize global parameters (SOME included in file IWEARR)
    !
    siglim = 0.1
    frcbed = 0.05
    frcsur = 0.05
    wvlbed = 100.
    wvlsur = 100.
    !
    iwedia = .false.
    nfreqs = npiwe
    kmxt = kmxdt
    mwriwe = 0
    nwriwe = 0
    !
    itiwei = 0
    tinciw = 0.0
    !
    ! Locate 'Filiwe' containing parameter values for calculations of
    ! Internal Wave Energy;
    !
    keyw = 'Filiwe'
    ntrec = nrrec
    cdef = ' '
    lenc = len(filiwe)
    call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
              & mdfrec    ,filiwe    ,cdef      ,lenc      ,nrrec     , &
              & ntrec     ,lundia    ,gdp       )
    !
    ! Reading error
    !
    if (lerror) then
       error = .true.
       goto 9999
    endif
    !
    ! Test file existence and if open the file
    ! File is already read in DIMIWE so test is obsolate
    !
    lfile = len(filiwe)
    !
    inquire (file = filiwe(1:lfile), exist = ex)
    if (ex) then
       open (newunit=luntmp, file = filiwe(:lfile), form = 'formatted', status = 'old')
       !
       ! Free formatted file
       ! Read flag for extra writing diagnostic information
       !
       read (luntmp, '(a)', iostat = iocond) rec132
       if (iocond/=0) then
          if (iocond<0) then
             call prterr(lundia    ,'G006'    ,filiwe(1:lfile)      )
          else
             call prterr(lundia    ,'G007'    ,filiwe(1:lfile)      )
          endif
          error = .true.
          goto 1000
       endif
       call small(rec132    ,2         )
       iwedia = .false.
       if (rec132(:2)=='.t') iwedia = .true.
       !
       ! If flag IWEDIA = .true. read M and N coordinates from record
       !
       if (iwedia) then
          idef = 1
          lenc = 132
          ibeg = index(rec132, ' ')
          if (ibeg==0) then
             call prterr(lundia    ,'U180'    ,' '       )
             iwedia = .false.
             goto 100
          endif
          call read1i(rec132    ,lenc      ,ibeg      ,iend      ,mwriwe    , &
                    & idef      ,ier       )
          if (ier<=0) then
             call prterr(lundia    ,'U180'    ,' '       )
             iwedia = .false.
             goto 100
          endif
          ibeg = iend + 1
          call read1i(rec132    ,lenc      ,ibeg      ,iend      ,nwriwe    , &
                    & idef      ,ier       )
          if (ier<=0) then
             call prterr(lundia    ,'U180'    ,' '       )
             iwedia = .false.
          endif
       endif
       !
       ! for parallel runs, determine whether M,N point is inside subdomain
       !
       if ( parll ) then
          mwriwe = mwriwe -mfg +1
          nwriwe = nwriwe -nfg +1
          if ( mwriwe < 1 .or. nwriwe < 1 .or. mwriwe > gdp%d%mmax .or. nwriwe > gdp%d%nmaxus ) then
             mwriwe = 0
             nwriwe = 0
             iwedia = .false.
          endif
       endif
       !
       ! Read 2 parameter values from REC132
       ! default value not allowed incase ier < 0
       !
  100  continue
       read (luntmp, '(a)', iostat = iocond) rec132
       if (iocond/=0) then
          if (iocond<0) then
             call prterr(lundia    ,'G006'    ,filiwe(1:lfile)      )
          else
             call prterr(lundia    ,'G007'    ,filiwe(1:lfile)      )
          endif
          error = .true.
          goto 1000
       endif
       lenc = 132
       iend = 0
       ibeg = iend + 1
       call read1i(rec132    ,lenc      ,ibeg      ,iend      ,kmxt      , &
                 & kmxdt     ,ier       )
       if (ier<=0) kmxt = kmxdt
       ibeg = iend + 1
       call read1i(rec132    ,lenc      ,ibeg      ,iend      ,nfreqs    , &
                 & npiwe     ,ier       )
       if (ier<=0) nfreqs = npiwe
       !
       ! Read 6 parameter values from REC132
       !
       rdef(1) = siglim
       rdef(2) = tinciw
       rdef(3) = wvlbed
       rdef(4) = frcbed
       rdef(5) = wvlsur
       rdef(6) = frcsur
       do k = 1, 6
          read (luntmp, '(a)', iostat = iocond) rec132
          if (iocond/=0) then
             if (iocond<0) then
                call prterr(lundia    ,'G006'    ,filiwe(1:lfile)      )
             else
                call prterr(lundia    ,'G007'    ,filiwe(1:lfile)      )
             endif
             error = .true.
             goto 1000
          endif
          iend = 0
          ibeg = iend + 1
          call read1r(rec132    ,lenc      ,ibeg      ,iend      ,rval(k)   , &
                    & rdef(k)   ,ier       )
          !
          ! End of record or error occurred (IER <= 0)
          !
          if (ier<=0) then
             error = .true.
             call prterr(lundia    ,'G006'    ,filiwe(1:lfile)      )
             !
             goto 1000
          endif
       enddo
       siglim = rval(1)
       tinciw = rval(2)
       wvlbed = rval(3)
       frcbed = rval(4)
       wvlsur = rval(5)
       frcsur = rval(6)
       !
       ! test value WVLBED <> 0. and WVLSUR <> 0.
       !
       if (wvlbed==0.0) then
          error = .true.
          call prterr(lundia    ,'U181'    ,'bed topog.'         )
          goto 1000
       endif
       if (wvlsur==0.0) then
          error = .true.
          call prterr(lundia    ,'U181'    ,'surface' )
          goto 1000
       endif
       !
       ! Test time constistency; TINCIW <> N * DT not allowed
       ! Value 0. has no real meaning, hence not allowed
       !
       itiwei = nint(tinciw/dt)
       if (dtn(itiwei, tinciw, dt)) then
          call prterr(lundia    ,'U044'    ,'Timestep for IWE'   )
          error = .true.
          goto 1000
       endif
       if (itiwei==0) then
          call prterr(lundia    ,'U182'    ,' '       )
          error = .true.
          goto 1000
       endif
       !
       ! Write diagnostic to LUNDIA
       !
       write (lundia, '(a,f12.3,a)') '*** IWE is activated every ', tinciw,     &
                                    & ' minutes'
       if (iwedia) then
          write (lundia, '(a,2i5)') '*** IWE diagnostic for (M,N) = ', mwriwe,  &
                                  & nwriwe
       endif
       !
       ! Stop reading file
       !
       !
       ! Close file
       !
 1000  continue
       close (luntmp)
    !
    ! Test file existence (N) then error
    !
    else
       error = .true.
       call prterr(lundia    ,'G004'    ,filiwe(:lfile)       )
    endif
    !
 9999 continue
end subroutine rdiwe
