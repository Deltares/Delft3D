subroutine rdbct(lunmd     ,lundia    ,error     ,nrrec     ,mdfrec    , &
               & nrver     ,runid     ,filbct    ,eol       ,nambnd    , &
               & typbnd    ,tprofu    ,nto       ,ntot      ,ntof      , &
               & kmax      ,rtbct     ,itstrt    ,itfinish  ,mxbctm    , &
               & nbcttm    ,tampab    ,bubble    ,gdp       )
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
!    Function: - Reads the boundary condition records from the
!                MD-file: FILBCT,TSBCT and AMPAB
!              - The order of reading is sequential for each
!                opening.
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use flow_tables
    use globaldata
    use string_module
    use system_utils, only: exifil
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer                    , pointer :: itis
    integer                    , pointer :: itdate
    real(fp)                   , pointer :: dt
    character*20, dimension(:) , pointer :: keywrd
    character*37, dimension(:) , pointer :: fmtbct
    integer                    , pointer :: julday
    type (handletype)          , pointer :: tseriesfile
!
! Global variables
!
    integer                                        :: itfinish !  Description and declaration in inttim.igs
    integer                                        :: itstrt   !  Description and declaration in inttim.igs
    integer                                        :: kmax     !  Description and declaration in esm_alloc_int.f90
    integer                                        :: lundia   !  Description and declaration in inout.igs
    integer                                        :: lunmd    !  Description and declaration in inout.igs
    integer                          , intent(in)  :: mxbctm   !!  Maximum number of times for which
                                                               !!  time varying boundary conditions are
                                                               !!  allowed in the Md-file
    integer                                        :: nbcttm   !!  Actual number of times for which
                                                               !!  time varying boundary conditions are
                                                               !!  allowed in the Md-file
    integer                                        :: nrrec    !!  Pointer to the record number in the MD-file
    integer                          , intent(in)  :: nrver    !!  Version number (240 - ....)
    integer                                        :: nto      !  Description and declaration in esm_alloc_int.f90
    integer                                        :: ntof     !  Description and declaration in dimens.igs
    integer                                        :: ntot     !  Description and declaration in dimens.igs
    logical, intent(in)                            :: bubble   !  Description and declaration in procs.igs
    logical                                        :: error    !!  Flag=TRUE if an error is encountered
    real(fp), dimension(2, mxbctm, nto)            :: tampab   !!  At most MXBCTM amlitudes for time
                                                               !!  varying boundary openings (end A and end B)
    real(fp), dimension(mxbctm)                    :: rtbct    !!  At most MXBCTM times for time varying boundary openings
    character(*)                                   :: filbct   !!  File name for the time varying boundary conditions file
    character(*)                                   :: mdfrec   !!  Standard rec. length in MD-file (300)
    character(*)                                   :: runid
    character(1)                                   :: eol      !!  ASCII code for End-Of-Line (^J)
    character(1) , dimension(nto)                  :: typbnd   !  Description and declaration in esm_alloc_char.f90
    character(20), dimension(nto)                  :: nambnd   !  Description and declaration in esm_alloc_char.f90
    character(20), dimension(nto)                  :: tprofu   !  Description and declaration in esm_alloc_char.f90
!
! Local variables
!
    integer                     :: ib
    integer                     :: ibct   ! Help var. for times read 
    integer                     :: iocond
    integer                     :: irec
    integer                     :: itold  ! Help var. to store last read time to test accending order 
    integer                     :: ittdep ! Help var. for the time read (now de- fined as multiples of DT, but in fu- ture it may take any value) 
    integer                     :: j      ! Help var. 
    integer                     :: lenc   ! Help var. (length of var. cvar to be looked for in the MD-file) 
    integer                     :: lf     ! Help var. specifying the length of character variables for file names 
    integer                     :: lkw    ! Length (in characters) of keyword 
    integer                     :: lrec   ! Length of direct access records if file already exists 
    integer                     :: lrid   ! Length of character string runid 
    integer                     :: lunout ! Unit number for the transformed file between tdatom and trisim 
    integer                     :: lunrd
    integer                     :: mxlrec
    integer                     :: n      ! Help var. 
    integer                     :: nlook  ! Help var.: nr. of data to look for in the MD-file 
    integer                     :: ntrec  ! Help. var to keep track of NRREC 
    integer                     :: ntyp
    integer, external           :: newlun
    logical                     :: ex     ! Flag to test if file exists 
    logical                     :: found  ! FOUND=TRUE if KEYW in the MD-file was found 
    logical                     :: lerror ! Flag=TRUE if a local error is encountered 
    logical                     :: newkw  ! Logical var. specifying whether a new recnam should be read from the MD-file or just new data in the continuation line 
    logical                     :: nodef  ! Flag set to YES if default value may NOT be applied in case var. read is empty (ier <= 0, or nrread < nlook) 
    logical                     :: noread ! Flag if FILBCT is equal to TMP file and should not be read. 
    real(fp)                    :: rdef   ! Help var. containing default va- lue(s) for real variable 
    real(fp), dimension(2)      :: rval   ! Help array (real) where the data, recently read from the MD-file, are stored temporarily 
    character(1)                :: cdummy ! Character help variable 
    character(1)                :: quote  ! Apostrophe ASCII-character 39 
    character(10), dimension(2) :: parunt ! Unit name fitting the parameter 
    character(10), dimension(6) :: unttyp ! Unit name fitting the parameter depending on value of TYPBND 
    character(12)               :: fildef ! Default file name (usually = blank) 
    character(20), dimension(6) :: namtyp ! Names of the paramaters to write to time dependent files for BCT depending on value of TYPBND 
    character(256)              :: filout ! Help variable for file name 
    character(36), dimension(2) :: parnam ! Names of the paramaters to write to time dependent files for BCT 
    character(40)               :: cntain ! String with comment 
    character(42)               :: tablnm ! Table name specification 
    character(6)                :: typtst ! Data string to test type of boundary 
    character(6)                :: keyw   ! Name of record to look for in the MD-file (usually KEYWRD or RECNAM) 
    character(300)              :: message
    !
    data typtst/'ZCQRTN'/
!
!! executable statements -------------------------------------------------------
!
    fmtbct      => gdp%gdfmtbct%fmtbct
    keywrd      => gdp%gdkeywtd%keywrd
    itdate      => gdp%gdexttim%itdate
    dt          => gdp%gdexttim%dt
    itis        => gdp%gdrdpara%itis
    julday      => gdp%gdinttim%julday
    tseriesfile => gdp%gdinibct%tseriesfile
    !
    ! Initialize local parameters
    !
    namtyp(1) = 'water elevation (z) '
    unttyp(1) = '[   m    ]'
    namtyp(2) = 'current         (c) '
    unttyp(2) = '[  m/s   ]'
    namtyp(3) = 'flux/discharge  (q) '
    unttyp(3) = '[ m**3/s ]'
    namtyp(4) = 'riemann         (r) '
    unttyp(4) = '[  m/s   ]'
    namtyp(5) = 'total discharge (t) '
    unttyp(5) = '[ m**3/s ]'
    namtyp(6) = 'neumann         (n) '
    unttyp(6) = '[  m/m   ]'
    !
    lerror = .false.
    newkw  = .true.
    nodef  = .false.
    found  = .false.
    noread = .false.
    rdef   = 0.0
    nlook  = 1
    fildef = ' '
    filout = ' '
    quote  = char(39)
    cntain = ' # at ends A&B of open boundary segment '
    !
    lunout = 8
    !
    ! Initialize global parameters
    !
    filbct = ' '
    !
    ! Locate 'FilbcT' record for time varying boundary conditions
    ! in extra input file
    !
    keyw = 'FilbcT'
    ntrec = nrrec
    lenc = len(filbct)
    call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
              & mdfrec    ,filbct    ,fildef    ,lenc      ,nrrec     , &
              & ntrec     ,lundia    ,gdp       )
    !
    !
    ! Reading error?
    !
    if (lerror) then
       lerror = .false.
       filbct = fildef
    endif
    !
    ! Read data from external file
    !
    if (filbct /= fildef) then
       !
       ! Define length of RUNID
       ! Check filename "filbct" <> TMP file or
       ! "filbct" = TMP file and access is direct
       ! open output file (ONLY VERSION 2.49 or lower) +
       ! Set name for the parameters, Regenerated locally
       !
       call remove_leading_spaces(runid     ,lrid      )
       filout = 'TMP_' // runid(:lrid) // '.bct'
       !
       ! Check filename and access
       ! For NRVER =< 249 this cannot be the case
       !
       if (filout == filbct) then
          inquire (file = filout(:8 + lrid), exist = ex)
          if (.not.ex) then
             call prterr(lundia    ,'G004'    ,filout    )
             !
             error = .true.
             goto 9999
          endif
          !
          open (newunit=lunout, file = filout(:8 + lrid))
          read (lunout, '(a1,i5)', iostat = iocond) cdummy, lrec
          close (lunout)
          lunout = 8
          !
          ! Not able to read record length for direct access
          !
          if (iocond /= 0) then
             call prterr(lundia    ,'U081'    ,filout    )
             !
             error = .true.
             goto 9999
          endif
          !
          ! Record length read
          !
          noread = .true.
       endif
       !
       ! define length of file name
       !
       call remove_leading_spaces(filbct    ,lf        )
       !
       ! test file existence <YES> -> open file <NO> -> error
       !
       if ( exifil(filbct, lundia) ) then
          if (nrver <= 249) then
             !
             ! Open file only in case NRVER =< 249. Otherwise it will be
             ! opened later as the record length is dependent on the
             ! profile type
             !
             mxlrec = 83
             inquire (file = filout(:8 + lrid), exist = ex)
             if (ex) then
                open (newunit=lunout, file = filout(:8 + lrid))
                close (lunout, status = 'delete')
             endif
             open (newunit=lunout, file = filout(:8 + lrid), form = 'formatted',     &
                 & access = 'direct', status = 'unknown', recl = mxlrec)
             write (lunout, fmtbct(1), rec = 1) '#', mxlrec, eol
             !
             ! Open FILBCT to read data from
             !
             open (newunit=lunrd, file = filbct(:lf), form = 'formatted',            &
                  & status = 'old')
             write (message, '(2a)') 'Reading BC-hydrodynamic file ', filbct(:lf)
             call prterr(lundia, 'G051', trim(message))
             call rdtdt(lundia    ,lunout    ,lunrd     ,error     ,filbct    , &
                      & runid     ,typtst    ,eol       ,itstrt    ,itfinish  , &
                      & nto       ,ntot      ,ntof      ,tprofu    ,nambnd    , &
                      & typbnd    ,namtyp    ,unttyp    ,gdp       )
             !
             close (lunrd)
          elseif (.not.noread) then
             !
             ! Open FILBCT to read data from
             !
             call flw_readtable(tseriesfile, filbct, julday, gdp)
             !
             open (newunit=lunrd, file = filbct(:lf), form = 'formatted',            &
                  & status = 'old')
             write (message, '(2a)') 'Reading BC-hydrodynamic file ', filbct(:lf)
             call prterr(lundia, 'G051', trim(message))
             call rdtdtn(lundia    ,lunout    ,lunrd     ,error     ,filout    , &
                       & filbct    ,runid     ,typtst    ,eol       ,itstrt    , &
                       & itfinish  ,nto       ,ntof      ,ntot      ,kmax      , &
                       & tprofu    ,nambnd    ,typbnd    ,namtyp    ,unttyp    , &
                       & bubble    ,gdp       )
             !
             close (lunrd)
          else
             !
             ! Reading TDD file for BC-hydrodynamic data skipped in
             ! TDATOM
             ! Define "fake" timeframe
             !
             write (message, '(3a)') 'BC-hydrodynamic file ', filbct(:lf), ' will be skipped in TDATOM'
             call prterr(lundia, 'G051', trim(message))
          endif
       else
          error = .true.
       endif
    !
    ! Time varying boundary conditions (flow) in file? <NO>
    ! This part is from now obsolete (all data are read from file)
    ! It is kept here to guarantee downwards compatibility
    !
    elseif (ntot > 0) then
       !
       ! Define length of RUNID
       ! open output file (ONLY VERSION 2.49 or lower) +
       ! Set name for the parameters, Regenerated locally
       !
       call remove_leading_spaces(runid     ,lrid      )
       filout = 'TMP_' // runid(:lrid) // '.bct'
       !
       ! Open file only in case NRVER =< 249, which always the
       ! case for data in MDF file
       !
       mxlrec = 83
       inquire (file = filout(:8 + lrid), exist = ex)
       if (ex) then
          open (newunit=lunout, file = filout(:8 + lrid))
          close (lunout, status = 'delete')
       endif
       open (newunit=lunout, file = filout(:8 + lrid), form = 'formatted',           &
            & access = 'direct', status = 'unknown', recl = mxlrec)
       write (lunout, fmtbct(1), rec = 1) '#', mxlrec, eol
       !
       ! Define parameter name for time column
       !
       parnam(1) = 'Time starting at ITDATE = 0.0 '
       parunt(1) = '[   min  ]'
       !
       ! Test profile definition, because <3d-profile> is not allowed
       ! for old file
       !
       do n = 1, ntot
          if (tprofu(ntof + n)(:10) == '3d-profile') then
             call prterr(lundia    ,'V095'    ,'for v249 or less'   )
             !
             error = .true.
             goto 9999
          endif
       enddo
       !
       ! Time varying boundary conditions contains a group of records
       ! all records part of the group are supposed to lie between two
       ! records with keyword 'TsbcT  '
       ! first set records = first opening => start on top of file
       !
       nbcttm = 0
       !
       keyw = 'TsbcT '
       lkw = 5
       newkw = .false.
       !==>
  150  continue
       call search(lunmd     ,lerror    ,newkw     ,nrrec     ,found     , &
                 & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , &
                 & 'NO'      )
       if (found) then
          nbcttm = nbcttm + 1
          goto 150
       endif
       !
       ! Read time dependent data from MD_file
       !
       ittdep = -1
       itold  = -1
       !
       rewind (lunmd)
       read (lunmd, '(a300)') mdfrec
       nrrec = 1
       ntrec = nrrec
       newkw = .true.
       !
       do ib = 1, nbcttm
          ibct = 1
          !
          ! Locate 'TsbcT ' record and read RVAL
          !
          keyw = 'TsbcT '
          nlook = 1
          call read2r(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                    & mdfrec    ,rval      ,rdef      ,nodef     ,nrrec     , &
                    & ntrec     ,lundia    ,gdp       )
          !
          !
          ! Reading error?
          !
          if (lerror) then
             error = .true.
             lerror = .false.
             exit
          endif
          rtbct(ibct) = rval(1)
          !
          ! Perform some checks with the time read
          !
          call chckit(lundia    ,lerror    ,'MD-file' ,rval(1)   ,dt        , &
                    & ittdep    ,itold     ,itstrt    ,ib        ,gdp       )
          !
          if (lerror) then
             error = .true.
             lerror = .false.
             exit
          endif
          if (ittdep > itold) itold = ittdep
          !
          ! Read hydrodynamic values and write to help array's for
          ! every NTOT
          !
          do n = 1, ntot
             !
             ! locate and read 'Ampab ' record
             ! default value not allowed incase ier < or = 0
             !
             nlook = 2
             keyw = 'Ampab '
             call read2r(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                       & mdfrec    ,rval      ,rdef      ,nodef     ,nrrec     , &
                       & ntrec     ,lundia    ,gdp       )
             !
             !
             ! reading error?
             !
             if (lerror) then
                error = .true.
                lerror = .false.
                goto 500
             endif
             !
             ! copy rval to appropriate array
             !
             do j = 1, 2
                tampab(j, ibct, n) = rval(j)
             enddo
             !
             newkw = .false.
             !
             ! writing to LUNOUT
             !
             irec = (n - 1)*(11 + nbcttm) + 1
             !
             ! Write first 11 records with block information for time
             ! IB=1
             ! Define NAMTYP index dependent on value of TYPBND(NTOF+N)
             !
             if (ib == 1) then
                ntyp = index(typtst, typbnd(ntof + n))
                parnam(2)(:20) = namtyp(ntyp)
                parunt(2) = unttyp(ntyp)
                !
                ! Define table name
                !
                tablnm = 'T-serie BCT ' // nambnd(ntof + n) // ' for run: '
                !
                ! Write first 7 description records to file
                !
                write (lunout, fmtbct(2), rec = irec + 1) &
                    & keywrd(1), quote, tablnm, runid, quote, eol
                write (lunout, fmtbct(3), rec = irec + 2) &
                    & keywrd(2), quote, tprofu(ntof + n), quote, cntain, eol
                write (lunout, fmtbct(4), rec = irec + 3) &
                    & keywrd(3), quote, nambnd(ntof + n), quote, eol
                write (lunout, fmtbct(7), rec = irec + 4) &
                    & keywrd(8), quote, 'non-equidistant', quote, eol
                write (lunout, fmtbct(8), rec = irec + 5) &
                    & keywrd(9), itdate, eol
                write (lunout, fmtbct(9), rec = irec + 6) &
                    & keywrd(10), quote, 'minutes', quote, eol
                write (lunout, fmtbct(10), rec = irec + 7) &
                    & keywrd(12), quote, 'linear', quote, eol
                !
                ! Write parameter name for time to file
                !
                write (lunout, fmtbct(11), rec = irec + 8) &
                    & keywrd(14), quote, parnam(1), quote, keywrd(15)(:10), quote, parunt(1),  &
                    & quote, eol
                !
                ! Write parameter name for TYPBND for end A and end B
                !
                parnam(2)(21:) = ' end A          '
                write (lunout, fmtbct(11), rec = irec + 9) &
                    & keywrd(14), quote, parnam(2), quote, keywrd(15)(:10), quote, parunt(2),  &
                    & quote, eol
                parnam(2)(21:) = ' end B          '
                write (lunout, fmtbct(11), rec = irec + 10) &
                    & keywrd(14), quote, parnam(2), quote, keywrd(15) (:10), quote,  &
                    & parunt(2), quote, eol
                !
                ! Write number of time dependent data to file
                !
                write (lunout, fmtbct(12), rec = irec + 11) &
                    & keywrd(16), nbcttm, eol
             endif
             !
             ! Write time dependent data to block for open boundary N
             ! skipping first 11 records with block info
             !
             irec = irec + 11 + ib
             write (lunout, fmtbct(13), rec = irec) &
                 & rtbct(ibct), (tampab(j, ibct, n), j = 1, 2), eol
          enddo
       enddo
       !
       ! stop reading
       !
       !
       ! define actual number of times for time varying boundary
       ! openings and define maximum time
       !
  500  continue
       if (itold /= -1) then
            if (itold < itfinish) then 
                call prterr(lundia    ,'U042'    ,'Last time for time varying flow boundary conditions <')
                error = .true.
                goto 9999
            endif
       endif
    else
    endif
    !
    ! close files
    !
 9999 continue
    if (lunout/=8) then
       if (error) then
          close (lunout, status = 'delete')
       else
          close (lunout)
       endif
    endif
end subroutine rdbct
