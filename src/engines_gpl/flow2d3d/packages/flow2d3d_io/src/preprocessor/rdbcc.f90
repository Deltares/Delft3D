subroutine rdbcc(lunmd     ,lundia    ,error     ,nrrec     ,mdfrec    , &
               & nrver     ,runid     ,filbcc    ,eol       ,nambnd    , &
               & namcon    ,nto       ,lstsc     ,kmax      ,rtbcc     , &
               & itstrt    ,itfinish  ,mxbctm    ,nbcctm    ,salin     , &
               & temp      ,const     ,lconc     ,sab       ,tab       , &
               & cab       ,zstep     ,tprofc    ,bubble    ,gdp       )
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
!                MD-file: FILBCC,TSBCC, SAB, TAB  and CAB
!              - The order of reading is sequential for each
!                opening.
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
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
    character*38, dimension(:) , pointer :: fmtbcc
!
! Global variables
!
    integer                                                   :: itfinish !  Description and declaration in inttim.igs
    integer                                                   :: itstrt   !  Description and declaration in inttim.igs
    integer                                                   :: kmax     !  Description and declaration in esm_alloc_int.f90
    integer                                     , intent(in)  :: lconc    !  Number of constituents (excl. salinity, temperature, secondary flow,
                                                                          !  turbulence energy dissipation and production)
    integer                                                   :: lstsc    !  Description and declaration in dimens.igs
    integer                                                   :: lundia   !  Description and declaration in inout.igs
    integer                                                   :: lunmd    !  Description and declaration in inout.igs
    integer                                     , intent(in)  :: mxbctm   !  Maximum number of times for which time varying boundary conditions are
                                                                          !  allowed in the Md-file
    integer                                                   :: nbcctm   !  Actual number of times for which time varying data for processes on
                                                                          !  boundaries are allowed in the Md-file
    integer                                                   :: nrrec    !  Pointer to the record number in the MD-file
    integer                                     , intent(in)  :: nrver    !  Version number (240/249)
    integer                                                   :: nto      !  Description and declaration in esm_alloc_int.f90
    logical                                     , intent(in)  :: bubble   !  Description and declaration in procs.igs
    logical                                     , intent(in)  :: const    !  Description and declaration in procs.igs
    logical                                                   :: error    !  Flag=TRUE if an error is encountered
    logical                                     , intent(in)  :: salin    !  Description and declaration in procs.igs
    logical                                     , intent(in)  :: temp     !  Description and declaration in procs.igs
    real(fp)     , dimension(4, 5, mxbctm, nto)               :: cab      !  At most MXBCTM time varying concentrations on boundaries (end A and end B)
    real(fp)     , dimension(4, mxbctm, nto)                  :: sab      !  At most MXBCTM time varying salinities on boundaries (end A and end B)
    real(fp)     , dimension(4, mxbctm, nto)                  :: tab      !  At most MXBCTM time varying temperatures on boundaries (end A and end B)
    real(fp)     , dimension(mxbctm)                          :: rtbcc    !  At most MXBCTM times for time varying data on boundaries for processes
    real(fp)     , dimension(mxbctm, nto, lstsc), intent(out) :: zstep    !  Description and declaration in esm_alloc_real.f90
    character(*)                                              :: filbcc   !  File name for the time varying data on boundaries for processes file
    character(*)                                              :: mdfrec   !  Standard rec. length in MD-file (300)
    character(*)                                              :: runid
    character(1)                                              :: eol      !  ASCII code for End-Of-Line (^J)
    character(10), dimension(nto, lstsc)                      :: tprofc   !  Description and declaration in esm_alloc_char.f90
    character(20), dimension(lstsc)                           :: namcon   !  Description and declaration in esm_alloc_char.f90
    character(20), dimension(nto)                             :: nambnd   !  Description and declaration in esm_alloc_char.f90
!
! Local variables
!
    integer                        :: ib
    integer                        :: ibcc    ! Help var. for times read 
    integer                        :: inprof  ! Index number of first character in PROFIL string of TPROFC definition 
    integer                        :: iocond  ! IO status for reading 
    integer                        :: irec
    integer                        :: itold   ! Help var. to store last read time to test accending order 
    integer                        :: ittdep  ! Help var. for the time read (now de- fined as multiples of DT, but in fu- ture it may take any value) 
    integer                        :: j       ! Help var. 
    integer                        :: l       ! Help var. for constituents loops 
    integer                        :: lenc    ! Help var. (length of var. cvar to be looked for in the MD-file) 
    integer                        :: lf      ! Help var. specifying the length of character variables for file names 
    integer                        :: lkw     ! Length (in characters) of keyword 
    integer                        :: ll      ! Help var. for constituents 
    integer                        :: lrec    ! Length of direct access records if file already exists 
    integer                        :: lrid    ! Length of character string runid 
    integer                        :: lunout  ! Unit number for the transformed file between tdatom and trisim 
    integer                        :: lunrd
    integer                        :: mxlrec
    integer, external              :: newlun
    integer                        :: nlook   ! Help var.: nr. of data to look for in the MD-file 
    integer                        :: nn
    integer                        :: ntrec   ! Help. var to keep track of NRREC 
    integer, dimension(0:7)        :: ntpara  ! Total number of parameter records 
    integer, dimension(7)          :: npara   ! Number of parameter records 
    logical                        :: ex      ! Flag to test if file exists 
    logical                        :: found   ! FOUND=TRUE if KEYW in the MD-file was found 
    logical                        :: lerror  ! Flag=TRUE if a local error is encountered 
    logical                        :: newkw   ! Logical var. specifying whether a new recnam should be read from the MD-file or just new data in the continuation line 
    logical                        :: nodef   ! Flag set to YES if default value may NOT be applied in case var. read is empty (ier <= 0, or nrread < nlook) 
    logical                        :: noread  ! Flag if FILBCC is equal to TMP file and should not be read. 
    real(fp)                       :: rdef    ! Help var. containing default va- lue(s) for real variable 
    real(fp), dimension(4, 7)      :: rwbval  ! Help array (real) where the data, recently read from the MD-file, are stored temporarily 
    real(fp), dimension(7)         :: rval    ! Help array to read the data from MD-file 
    real(fp), dimension(7)         :: rwdep   ! Help array (real) where the data, recently read from the MD-file, are stored temporarily 
    character(1)                   :: cdummy  ! Character help variable 
    character(1)                   :: quote   ! Apostrophe ASCII-character 39 
    character(10)                  :: cdef
    character(10)                  :: chulp
    character(10), dimension(2)    :: parunt  ! Unit name fitting the parameter 
    character(12)                  :: fildef  ! Default file name (usually = blank) 
    character(256)                 :: filout  ! Help variable for file name 
    character(36), dimension(2)    :: parnam  ! Names of the paramaters to write to time dependent files for BCC 
    character(40)                  :: cntain
    character(40)                  :: profil  ! Total string of possible profiles 
    character(6)                   :: keyw    ! Name of record to look for in the MD-file (usually KEYWRD or RECNAM) 
    character(63)                  :: tablnm  ! Table name specification
    character(300)                 :: message
    !
    data profil/'uniform   linear    step      3d-profile'/
!
!! executable statements -------------------------------------------------------
!
    fmtbcc  => gdp%gdfmtbcc%fmtbcc
    keywrd  => gdp%gdkeywtd%keywrd
    itdate  => gdp%gdexttim%itdate
    dt      => gdp%gdexttim%dt
    itis    => gdp%gdrdpara%itis
    !
    lerror = .false.
    newkw  = .true.
    nodef  = .false.
    found  = .false.
    noread = .false.
    rdef   = 0.0
    cdef   = 'uniform   '
    nlook  = 1
    fildef = ' '
    filout = ' '
    chulp  = ' '
    quote  = char(39)
    cntain = ' # at ends A&B of open boundary segment '
    !
    lunout = 8
    !
    ! Initialize global parameters
    !
    filbcc = fildef
    !
    ! Locate 'FilbcC' record for time varying process data on boundaries
    ! in extra input file
    !
    keyw = 'FilbcC'
    ntrec = nrrec
    lenc = len(filbcc)
    call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
              & mdfrec    ,filbcc    ,fildef    ,lenc      ,nrrec     , &
              & ntrec     ,lundia    ,gdp       )
    !
    !
    ! Reading error?
    !
    if (lerror) then
       lerror = .false.
       filbcc = fildef
    endif
    !
    ! Read data from external file
    !
    if (filbcc/=fildef) then
       !
       ! Define length of RUNID
       ! Check filename "filbcc" <> TMP file or
       ! "filbcc" = TMP and access is direct
       ! open output file (ONLY VERSION 2.49 or lower) +
       ! Set name for the constituents, Regenerated locally
       !
       call remove_leading_spaces(runid     ,lrid      )
       filout = 'TMP_' // runid(:lrid) // '.bcc'
       !
       ! Check filename and access
       ! For NRVER =< 249 this cannot be the case
       !
       if (filout==filbcc) then
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
          if (iocond/=0) then
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
       call remove_leading_spaces(filbcc    ,lf        )
       !
       ! Test file existence <YES> -> open file <NO> -> error
       !
       if (exifil(filbcc, lundia)) then
          if (nrver<=249) then
             !
             ! Open file only in case NRVER =< 249. Otherwise it will be
             ! opened later as the record length is dependent on the
             ! profile type Maximum number of values to write is 5 (for
             ! profile = <step>)
             ! Total lenght from strings formats FMTBCC(2) = 89
             ! from Profile <step> = 83
             !
             mxlrec = 89
             inquire (file = filout(:8 + lrid), exist = ex)
             if (ex) then
                open (newunit=lunout, file = filout(:8 + lrid))
                close (lunout, status = 'delete')
             endif
             open (newunit=lunout, file = filout(:8 + lrid), form = 'formatted',     &
                 & access = 'direct', status = 'unknown', recl = mxlrec)
             write (lunout, fmtbcc(1), rec = 1) '#', mxlrec, eol
             !
             ! Open FILBCC to read data from
             !
             open (newunit=lunrd, file = filbcc(:lf), form = 'formatted',            &
                  & status = 'old')
             write (message, '(2a)') 'Reading BC-transport file ', filbcc(:lf)
             call prterr(lundia, 'G051', trim(message))
             call rdtdc(lundia    ,lunout    ,lunrd     ,error     ,filbcc    , &
                      & runid     ,profil    ,eol       ,itstrt    ,itfinish  , &
                      & nto       ,lstsc     ,nambnd    ,namcon    ,gdp       )
             !
             close (lunrd)
          elseif (.not.noread) then
             !
             ! Open FILBCC to read data from
             !
             open (newunit=lunrd, file = filbcc(:lf), form = 'formatted',            &
                  & status = 'old')
             write (message, '(2a)') 'Reading BC-transport file ', filbcc(:lf)
             call prterr(lundia, 'G051', trim(message))
             call rdtdcn(lundia    ,lunout    ,lunrd     ,error     ,filout    , &
                       & filbcc    ,runid     ,profil    ,eol       ,itstrt    , &
                       & itfinish  ,nto       ,lstsc     ,kmax       ,nambnd    , &
                       & namcon    ,bubble    ,gdp       )
             !
             close (lunrd)
          else
             !
             ! Reading TDD file for open boundary transport data skipped
             ! in TDATOM
             ! Define "fake" timeframe
             !
             write (message, '(3a)') 'BC-transport file ', filbcc(:lf), ' will be skipped in TDATOM'
             call prterr(lundia, 'G051', trim(message))
          endif
       else
          error = .true.
       endif
    !
    ! Time varying data for processes on boundaries in file? <NO>
    ! This part is from now obsolete (all data are read from file)
    ! It is kept here to guarantee downwards compatibility
    !
    elseif (nto>0) then
       !
       ! Define length of RUNID
       ! open output file (ONLY VERSION 2.49 or lower) +
       ! Set name for the constituents, Regenerated locally
       !
       call remove_leading_spaces(runid     ,lrid      )
       filout = 'TMP_' // runid(:lrid) // '.bcc'
       !
       ! Open file only in case NRVER =< 249, which always the
       ! case for data in MDF file
       !
       mxlrec = 89
       inquire (file = filout(:8 + lrid), exist = ex)
       if (ex) then
          open (newunit=lunout, file = filout(:8 + lrid))
          close (lunout, status = 'delete')
       endif
       open (newunit=lunout, file = filout(:8 + lrid), form = 'formatted',           &
            & access = 'direct', status = 'unknown', recl = mxlrec)
       write (lunout, fmtbcc(1), rec = 1) '#', mxlrec, eol
       !
       ! Define parameter name for time column
       !
       ntpara(0) = 0
       parnam(1) = 'Time starting at ITDATE = 0.0       '
       parunt(1) = '[   min  ]'
       !
       ! Time varying data for processes on boundaries contains a group
       ! of records. all records part of the group are supposed to lie
       ! between two records with keyword 'TsbcC  '
       ! first set records = first opening => start on top of file
       ! Count for number of times specified
       !
       keyw = 'TsbcC '
       lkw = 5
       newkw = .false.
       ntrec = nrrec
       !
       nbcctm = 0
       !==>
  150  continue
       call search(lunmd     ,lerror    ,newkw     ,nrrec     ,found     , &
                 & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , &
                 & 'NO'      )
       if (found) then
          nbcctm = nbcctm + 1
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
       lenc = 10
       !
       do ib = 1, nbcctm
          ibcc = 1
          !
          ! Locate 'TsbcC ' record and read RVAL
          !
          keyw = 'TsbcC '
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
          !
          rtbcc(ibcc) = rval(1)
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
          !
          if (ittdep>itold) itold = ittdep
          !
          ! Read process values and write to help array's for every NTO
          !
          do nn = 1, nto
             ll = 0
             !
             ! Salinity
             !
             if (salin) then
                ll = ll + 1
                !
                ! Locate and read 'Sab   ' record
                ! default value not allowed (IER < or = 0)
                !
                nlook = 4
                keyw = 'Sab   '
                !
                rval(1) = -1.0
                rval(2) = -1.0
                rval(3) = -1.0
                rval(4) = -1.0
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
                   goto 700
                endif
                !
                ! Copy RVAL to appropriate array
                !
                do j = 1, 4
                   sab(j, ibcc, nn) = rval(j)
                   rwbval(j, ll) = rval(j)
                   !
                   ! Check for negative values
                   !
                   if (sab(j, ibcc, nn)<0.0) then
                      call prterr(lundia    ,'V061'    ,'Salinity at open boundary'     )
                      !
                      error = .true.
                      goto 700
                   endif
                   !
                   ! If an NaN is read -> error
                   !
                   if ( isnan(cab(j, l, ibcc, nn)) ) then 
                       write(message,'(a,a)') 'Salinity containing NaN in ',filbcc
                       call prterr(lundia    ,'P004'    ,message      )
                       error = .true.
                       goto 700
                   endif
                enddo
                !
                ! Locate and read 'ProfC ' record for version 2.40-2.49
                ! Read ZSTEP from record
                !
                nlook = 1
                keyw = 'ProfC '
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
                   goto 700
                endif
                !
                zstep(ibcc, nn, ll) = rval(1)
                rwdep(ll) = rval(1)
                !
                ! Read TPROFC from record in CHULP
                !
                nlook = 0
                call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                          & mdfrec    ,chulp     ,cdef      ,lenc      ,nrrec     , &
                          & ntrec     ,lundia    ,gdp       )
                !
                !
                ! Reading error?
                !
                if (lerror) then
                   error = .true.
                   lerror = .false.
                   goto 700
                endif
                !
                ! Redefine name in small characters, save original
                ! name in case of an error (first entry)
                !
                if (ib==1) then
                   tprofc(nn, ll) = chulp
                   call small(tprofc(nn, ll)       ,lenc      )
                   !
                   ! Check for undefined profile definition
                   !
                   inprof = index(profil, tprofc(nn, ll))
                   if (inprof==0) then
                      call prterr(lundia    ,'U066'    ,chulp     )
                      !
                      tprofc(nn, ll) = cdef
                   endif
                   !
                   ! Check for not allowed profile (3d-profile)
                   !
                   if (tprofc(nn, ll)=='3d-profile') then
                      call prterr(lundia    ,'V095'    ,'for v249 or less'   )
                      !
                      error = .true.
                      goto 700
                   endif
                else
                   !
                   ! Check for changing of profile definition in time
                   ! all but first entry
                   !
                   call small(chulp     ,lenc      )
                   if (chulp/=tprofc(nn, ll)) then
                      error = .true.
                      call prterr(lundia    ,'U066'    ,chulp     )
                      !
                      goto 700
                   endif
                endif
             endif
             !
             ! Temperature
             !
             if (temp) then
                ll = ll + 1
                !
                ! Locate and read 'Tab   ' record
                ! default value not allowed (IER < or = 0)
                !
                nlook = 4
                keyw = 'Tab   '
                !
                rval(1) = -1.0
                rval(2) = -1.0
                rval(3) = -1.0
                rval(4) = -1.0
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
                   goto 700
                endif
                !
                ! Copy RVAL to appropriate array
                !
                do j = 1, 4
                   tab(j, ibcc, nn) = rval(j)
                   rwbval(j, ll) = rval(j)
                   !
                   ! Check for negative values
                   !
                   if (tab(j, ibcc, nn)<0.0) then
                      call prterr(lundia    ,'V061'    ,'Temperature at open boundary'  )
                      !
                      error = .true.
                      goto 700
                   endif
                   !
                   ! If an NaN is read -> error
                   !
                   if ( isnan(cab(j, l, ibcc, nn)) ) then 
                       write(message,'(a,a)') 'Temperature containing NaN in ',filbcc
                       call prterr(lundia    ,'P004'    ,message      )
                       error = .true.
                       goto 700
                   endif
                enddo
                !
                ! Locate and read 'ProfC ' record for version 2.40-2.49
                ! Read ZSTEP from record
                !
                nlook = 1
                keyw = 'ProfC '
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
                   goto 700
                endif
                !
                zstep(ibcc, nn, ll) = rval(1)
                rwdep(ll) = rval(1)
                !
                ! Read TPROFC from record in CHULP
                !
                nlook = 0
                call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                          & mdfrec    ,chulp     ,cdef      ,lenc      ,nrrec     , &
                          & ntrec     ,lundia    ,gdp       )
                !
                !
                ! Reading error?
                !
                if (lerror) then
                   error = .true.
                   lerror = .false.
                   goto 700
                endif
                !
                ! Redefine name in small characters, save original
                ! name in case of an error (first entry)
                !
                if (ib==1) then
                   tprofc(nn, ll) = chulp
                   call small(tprofc(nn, ll)       ,lenc      )
                   !
                   ! Check for undefined profile definition
                   !
                   inprof = index(profil, tprofc(nn, ll))
                   if (inprof==0) then
                      call prterr(lundia    ,'U066'    ,chulp     )
                      !
                      tprofc(nn, ll) = cdef
                   endif
                   !
                   ! Check for not allowed profile (3d-profile)
                   !
                   if (tprofc(nn, ll)=='3d-profile') then
                      call prterr(lundia    ,'V095'    ,'for v249 or less'   )
                      !
                      error = .true.
                      goto 700
                   endif
                else
                   !
                   ! Check for changing of profile definition in time
                   ! all but first entry
                   !
                   call small(chulp     ,lenc      )
                   if (chulp/=tprofc(nn, ll)) then
                      error = .true.
                      call prterr(lundia    ,'U066'    ,chulp     )
                      !
                      goto 700
                   endif
                endif
             endif
             !
             ! Other constituents
             !
             if (const) then
                do l = 1, lconc
                   ll = ll + 1
                   !
                   ! Locate and read 'Cab   ' record
                   ! default value not allowed (IER < or = 0)
                   !
                   nlook = 4
                   keyw = 'Cab   '
                   write (keyw(4:4), '(i1)') l
                   !
                   rval(1) = -1.0
                   rval(2) = -1.0
                   rval(3) = -1.0
                   rval(4) = -1.0
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
                      goto 700
                   endif
                   !
                   ! Copy RVAL to appropriate array
                   !
                   do j = 1, 4
                      cab(j, l, ibcc, nn) = rval(j)
                      rwbval(j, ll) = rval(j)
                      !
                      ! Check for negative values
                      !
                      if (cab(j, l, ibcc, nn)<0.0) then
                         call prterr(lundia    ,'V061'    ,'Concentration at open boundary')
                         !
                         error = .true.
                         goto 700
                      endif
                      !
                      ! If an NaN is read -> error
                      !
                      if ( isnan(cab(j, l, ibcc, nn)) ) then 
                          write(message,'(a,a)') 'Concentration containing NaN in ',filbcc
                          call prterr(lundia    ,'P004'    ,message      )
                          error = .true.
                          goto 700
                      endif
                   enddo
                   !
                   ! Locate and read 'ProfC ' record version 2.40-2.49
                   ! Read ZSTEP from record
                   !
                   nlook = 1
                   keyw = 'ProfC '
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
                      goto 700
                   endif
                   !
                   zstep(ibcc, nn, ll) = rval(1)
                   rwdep(ll) = rval(1)
                   !
                   ! Read TPROFC from record in CHULP
                   !
                   nlook = 0
                   call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                             & mdfrec    ,chulp     ,cdef      ,lenc      ,nrrec     , &
                             & ntrec     ,lundia    ,gdp       )
                   !
                   !
                   ! Reading error?
                   !
                   if (lerror) then
                      error = .true.
                      lerror = .false.
                      goto 700
                   endif
                   !
                   ! Redefine name in small characters, save original
                   ! name in case of an error (first entry)
                   !
                   if (ib==1) then
                      tprofc(nn, ll) = chulp
                      call small(tprofc(nn, ll)       ,lenc      )
                      !
                      ! Check for undefined profile definition
                      !
                      inprof = index(profil, tprofc(nn, ll))
                      if (inprof==0) then
                         call prterr(lundia    ,'U066'    ,chulp     )
                         !
                         tprofc(nn, ll) = cdef
                      endif
                      !
                      ! Check for not allowed profile (3d-profile)
                      !
                      if (tprofc(nn, ll)=='3d-profile') then
                         call prterr(lundia    ,'V095'    ,'for v249 or less'   )
                         !
                         error = .true.
                         goto 700
                      endif
                   else
                      !
                      ! Check for changing of profile definition in
                      ! time all but first entry
                      !
                      call small(chulp     ,lenc      )
                      if (chulp/=tprofc(nn, ll)) then
                         error = .true.
                         call prterr(lundia    ,'U066'    ,chulp     )
                         !
                         goto 700
                      endif
                   endif
                enddo
             endif
             !
             ! Write constituent data to LUNOUT
             !
             do l = 1, ll
                !
                ! Define number of parameter records
                !
                if (ib==1) then
                   if (tprofc(nn, l)(:7)=='uniform') then
                      ntpara(l) = ntpara(l - 1) + 3
                   elseif (tprofc(nn, l)(:6)=='linear') then
                      ntpara(l) = ntpara(l - 1) + 5
                   elseif (tprofc(nn, l)(:4)=='step') then
                      ntpara(l) = ntpara(l - 1) + 6
                   else
                   endif
                endif
                !
                ! Define record
                !
                irec = (nn - 1)*(ll*(nbcctm + 8) + ntpara(ll)) + (l - 1)     &
                     & *(nbcctm + 8) + ntpara(l - 1) + 1
                !
                ! Write first 8+NPARA(L) records with block
                ! information for time IB=1
                !
                if (ib==1) then
                   !
                   ! Define table name
                   !
                   tablnm = 'T-serie BCC ' // nambnd(nn) // '-' // namcon(l) &
                          & // ' for run: '
                   !
                   ! Write first 7 description records to file
                   !
                   write (lunout, fmtbcc(2), rec = irec + 1) &
                       & keywrd(1), quote, tablnm, runid, quote, eol
                   write (lunout, fmtbcc(3), rec = irec + 2) &
                       & keywrd(2), quote, tprofc(nn, l), quote, cntain, eol
                   write (lunout, fmtbcc(4), rec = irec + 3) &
                       & keywrd(3), quote, nambnd(nn), quote, eol
                   write (lunout, fmtbcc(7), rec = irec + 4) &
                       & keywrd(8), quote, 'non-equidistant' , quote, eol
                   write (lunout, fmtbcc(8), rec = irec + 5) &
                       & keywrd(9), itdate, eol
                   write (lunout, fmtbcc(9), rec = irec + 6) &
                       & keywrd(10), quote, 'minutes', quote, eol
                   write (lunout, fmtbcc(10), rec = irec + 7) &
                       & keywrd(12), quote, 'linear', quote, eol
                   !
                   ! Write parameter name for time to file
                   !
                   npara(l) = 1
                   write (lunout, fmtbcc(11), rec = irec + 7 + npara(l) &
                       & ) keywrd (14), quote, parnam (1), quote, keywrd (15)  &
                       & (:10), quote, parunt (1), quote, eol
                   !
                   ! Define parameter name for constituent L
                   !
                   parnam(2)(:20) = namcon(l)
                   parunt(2) = '[    -   ]'
                   if (parnam(2)(:8)=='salinity') parunt(2) = '[   ppt  ]'
                   if (parnam(2) &
                    & (:11)=='temperature') parunt(2) = '[   deg  ]'
                   !
                   ! Write parameter names for profile <uniform>
                   !
                   if (tprofc(nn, l)(:7)=='uniform') then
                      npara(l) = npara(l) + 1
                      parnam(2)(21:) = ' end A uniform  '
                      write (lunout, fmtbcc(11), rec = irec + 7 + npara(l) &
                          & ) keywrd (14), quote, parnam (2), quote, keywrd (15)  &
                          & (:10), quote, parunt (2), quote, eol
                      npara(l) = npara(l) + 1
                      parnam(2)(21:) = ' end B uniform  '
                      write (lunout, fmtbcc(11), rec = irec + 7 + npara(l) &
                          & ) keywrd (14), quote, parnam (2), quote, keywrd (15)  &
                          & (:10), quote, parunt (2), quote, eol
                   !
                   ! Write parameter names for profile <linear>
                   !
                   elseif (tprofc(nn, l)(:6)=='linear') then
                      npara(l) = npara(l) + 1
                      parnam(2)(21:) = ' end A surface  '
                      write (lunout, fmtbcc(11), rec = irec + 7 + npara(l) &
                          & ) keywrd (14), quote, parnam (2), quote, keywrd (15)  &
                          & (:10), quote, parunt (2), quote, eol
                      npara(l) = npara(l) + 1
                      parnam(2)(21:) = ' end A bed      '
                      write (lunout, fmtbcc(11), rec = irec + 7 + npara(l) &
                          & ) keywrd (14), quote, parnam (2), quote, keywrd (15)  &
                          & (:10), quote, parunt (2), quote, eol
                      !
                      npara(l) = npara(l) + 1
                      parnam(2)(21:) = ' end B surface  '
                      write (lunout, fmtbcc(11), rec = irec + 7 + npara(l) &
                          & ) keywrd (14), quote, parnam (2), quote, keywrd (15)  &
                          & (:10), quote, parunt (2), quote, eol
                      npara(l) = npara(l) + 1
                      parnam(2)(21:) = ' end B bed      '
                      write (lunout, fmtbcc(11), rec = irec + 7 + npara(l) &
                          & ) keywrd (14), quote, parnam (2), quote, keywrd (15)  &
                          & (:10), quote, parunt (2), quote, eol
                   !
                   ! Write parameter names for profile <step>
                   !
                   elseif (tprofc(nn, l)(:4)=='step') then
                      npara(l) = npara(l) + 1
                      parnam(2)(21:) = ' end A surface  '
                      write (lunout, fmtbcc(11), rec = irec + 7 + npara(l) &
                          & ) keywrd (14), quote, parnam (2), quote, keywrd (15)  &
                          & (:10), quote, parunt (2), quote, eol
                      npara(l) = npara(l) + 1
                      parnam(2)(21:) = ' end A bed      '
                      write (lunout, fmtbcc(11), rec = irec + 7 + npara(l) &
                          & ) keywrd (14), quote, parnam (2), quote, keywrd (15)  &
                          & (:10), quote, parunt (2), quote, eol
                      !
                      npara(l) = npara(l) + 1
                      parnam(2)(21:) = ' end B surface  '
                      write (lunout, fmtbcc(11), rec = irec + 7 + npara(l) &
                          & ) keywrd (14), quote, parnam (2), quote, keywrd (15)  &
                          & (:10), quote, parunt (2), quote, eol
                      npara(l) = npara(l) + 1
                      parnam(2)(21:) = ' end B bed      '
                      write (lunout, fmtbcc(11), rec = irec + 7 + npara(l) &
                          & ) keywrd (14), quote, parnam (2), quote, keywrd (15)  &
                          & (:10), quote, parunt (2), quote, eol
                      !
                      npara(l) = npara(l) + 1
                      parnam(2) = 'discontinuity'
                      parunt(2) = '[    m   ]'
                      write (lunout, fmtbcc(11), rec = irec + 7 + npara(l) &
                          & ) keywrd (14), quote, parnam (2), quote, keywrd (15)  &
                          & (:10), quote, parunt (2), quote, eol
                   else
                   endif
                   !
                   ! Write number of time dependent data to file
                   !
                   write (lunout, fmtbcc(12), rec = irec + 8 + npara(l) &
                       & ) keywrd (16), nbcctm , eol
                endif
                !
                ! Write time dependent data to block for constituent
                ! L skipping first 8+NPARA(L) records with block info
                !
                irec = irec + ib + 8 + npara(l)
                if (tprofc(nn, l)(:7)=='uniform') then
                   fmtbcc(13)(10:10) = '2'
                   write (lunout, fmtbcc(13), rec = irec) &
                       & rtbcc(ibcc), rwbval(1, l), rwbval(3, l), eol
                elseif (tprofc(nn, l)(:6)=='linear') then
                   fmtbcc(13)(10:10) = '4'
                   write (lunout, fmtbcc(13), rec = irec) &
                       & rtbcc(ibcc), (rwbval(j, l), j = 1, 4), eol
                elseif (tprofc(nn, l)(:4)=='step') then
                   fmtbcc(13)(10:10) = '5'
                   write (lunout, fmtbcc(13), rec = irec) &
                       & rtbcc(ibcc), (rwbval(j, l), j = 1, 4), rwdep(l) , eol
                else
                endif
             enddo
          enddo
       enddo
       !
       ! Stop reading
       !
       !
       ! Define actual number of times for time varying data for
       ! processes on boundaries and maximum time
       !
  700  continue
       if (itold/= - 1) then
            if (itold < itfinish) then 
                call prterr(lundia    ,'U042'    ,'Last time for time varying constituents boundary conditions <')
                error = .true.
                goto 9999
            endif
       endif
       nbcctm = ibcc - 1
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
end subroutine rdbcc
