subroutine rddis(lunmd     ,lundia    ,error     ,nrrec     ,mdfrec    , &
               & nrver     ,runid     ,fildis    ,eol       ,namsrc    , &
               & disint    ,namcon    ,nsrc      ,rtdis     ,itstrt    , &
               & itfinish  ,mxnsrc    ,mxdist    ,ndistm    ,lstsc     , &
               & salin     ,temp      ,const     ,lconc     ,disch     , &
               & cqs       ,cqt       ,cqc       ,bubble    ,gdp       )
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
!    Function: - Reads the time dependent boundary data for the
!                discharges directly from the MD-file or indi-
!                rectly from the attribute file Fildis.
!              - Tests the file or data consistency.
!              - Checks whether the file exists or the required
!                data is not empty.
!              - An essential assumption is that the data has to
!                be specified sequentially in time. This imply
!                that NT times NSRC of Flow and Conc(L=1,LCON)
!                records should exist in the file (NT unrestric-
!                ted).
!              - Writes the data to an unformatted file
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
    character*39, dimension(:) , pointer :: fmtdis
!
! Global variables
!
    integer                                               :: itfinish !  Description and declaration in inttim.igs
    integer                                               :: itstrt   !  Description and declaration in inttim.igs
    integer                                 , intent(in)  :: lconc    !  Number of Constituents (excl. salinity, temperature, secondary flow,
                                                                      !  turbulence energy dissipation and production)
    integer                                               :: lstsc    !  Description and declaration in dimens.igs
    integer                                               :: lundia   !  Description and declaration in inout.igs
    integer                                               :: lunmd    !  Description and declaration in inout.igs
    integer                                 , intent(in)  :: mxdist   !  Maximum number of times for which time varying data at discharge points is allowed in the Md-file
    integer                                 , intent(in)  :: mxnsrc   !  Maximum number of discharge points
    integer                                               :: ndistm   !  Actual number of times for which time varying data at discharge points is allowed in the Md-file
    integer                                               :: nrrec    !  Pointer to the record number in the MD-file
    integer                                 , intent(in)  :: nrver    !  Version number (240/249)
    integer                                               :: nsrc     !  Description and declaration in esm_alloc_int.f90
    logical                                 , intent(in)  :: bubble   !  Description and declaration in procs.igs
    logical                                 , intent(in)  :: const    !  Description and declaration in procs.igs
    logical                                               :: error    !  Flag=TRUE if an error is encountered
    logical                                 , intent(in)  :: salin    !  Description and declaration in procs.igs
    logical                                 , intent(in)  :: temp     !  Description and declaration in procs.igs
    real(fp)     , dimension(5, mxdist, mxnsrc)           :: cqc      !  At most MXDIST time varying concen-trations at discharge points
    real(fp)     , dimension(mxdist)                      :: rtdis    !  At most MXDIST times for time varying data at discharge points
    real(fp)     , dimension(mxdist, mxnsrc)              :: cqs      !  At most MXDIST time varying salinities at discharge points
    real(fp)     , dimension(mxdist, mxnsrc)              :: cqt      !  At most MXDIST time varying temperatures at discharge points
    real(fp)     , dimension(mxdist, mxnsrc), intent(out) :: disch    !  Description and declaration in esm_alloc_real.f90
    character(*)                                          :: fildis   !  File name for the time varying data at discharge points
    character(*)                                          :: mdfrec   !  Standard rec. length in MD-file (300)
    character(*)                                          :: runid
    character(1)                                          :: eol      !  ASCII code for End-Of-Line (^J)
    character(1) , dimension(nsrc)                        :: disint   !  Description and declaration in esm_alloc_char.f90
    character(20), dimension(lstsc)         , intent(in)  :: namcon   !  Description and declaration in esm_alloc_char.f90
    character(20), dimension(nsrc)                        :: namsrc   !  Description and declaration in esm_alloc_char.f90
!
! Local variables
!
    integer                                  :: id
    integer                                  :: idis    ! Help var. for times read 
    integer                                  :: iocond  ! IO status for reading 
    integer                                  :: irec
    integer                                  :: istat
    integer                                  :: itold   ! Help var. to store last read time to test accending order 
    integer                                  :: ittdep  ! Help var. for the time read (now de- fined as multiples of DT, but in fu- ture it may take any value) 
    integer                                  :: l       ! Help loop var. 
    integer                                  :: lenc    ! Help var. (length of var. cvar to be looked for in the MD-file) 
    integer                                  :: lf      ! Help var. specifying the length of character variables for file names 
    integer                                  :: lkw     ! Length (in characters) of keyword 
    integer                                  :: ll      ! Help var. 
    integer                                  :: lnconc
    integer                                  :: lrec    ! Length of direct access records if file already exists 
    integer                                  :: lrid    ! Length of character string runid 
    integer                                  :: lunout  ! Unit number for transformed file 
    integer                                  :: lunrd
    integer                                  :: maxval  ! Maximum number of values 1+LSTSC+2 
    integer                                  :: mxlrec
    integer                                  :: n       ! Help var. 
    integer, external                        :: newlun
    integer                                  :: nlook   ! Help var.: nr. of data to look for in the MD-file 
    integer                                  :: nrval   ! Number of values to read from file 
    integer                                  :: ntrec   ! Help. var to keep track of NRREC 
    logical                                  :: ex      ! Flag to test if file exists 
    logical                                  :: found   ! FOUND=TRUE if KEYW in the MD-file was found 
    logical                                  :: lerror  ! Flag=TRUE if a local error is encountered 
    logical                                  :: newkw   ! Logical var. specifying whether a new recnam should be read from the MD-file or just new data in the continuation line 
    logical                                  :: nodef   ! Flag set to YES if default value may NOT be applied in case var. read is empty (ier <= 0, or nrread < nlook) 
    logical                                  :: noread  ! Flag if FILDIS is equal to TMP file and should not be read. 
    real(fp)                                 :: rdef    ! Help var. containing default va- lue(s) for real variable 
    real(fp)     , dimension(:), allocatable :: rval    ! Help array to read the data from MD-file 
    real(fp)     , dimension(:), allocatable :: rwdis
    character(1)                             :: cdummy  ! Character help variable 
    character(1)                             :: quote   ! Apostrophe ASCII-character 39 
    character(10), dimension(:), allocatable :: parunt  ! Unit name fitting the parameter 
    character(12)                            :: fildef  ! Default file name (usually = blank) 
    character(20)                            :: interp  ! Character string containing inter- polation option 
    character(256)                           :: filout  ! Help variable for file name 
    character(36), dimension(:), allocatable :: parnam  ! Names of the paramaters to write to time dependent file DIS 
    character(40)                            :: cntain
    character(42)                            :: tablnm  ! Table name specification 
    character(6)                             :: keyw    ! Name of record to look for in the MD-file (usually KEYWRD or RECNAM) 
    character(300)                           :: message
!
!! executable statements -------------------------------------------------------
!
    fmtdis  => gdp%gdfmtdis%fmtdis
    keywrd  => gdp%gdkeywtd%keywrd
    itdate  => gdp%gdexttim%itdate
    dt      => gdp%gdexttim%dt
    itis    => gdp%gdrdpara%itis
    !
                    allocate(rval  (4+lstsc), stat = istat)
    if (istat == 0) allocate(rwdis (4+lstsc), stat = istat)
    if (istat == 0) allocate(parunt(4+lstsc), stat = istat)
    if (istat == 0) allocate(parnam(4+lstsc), stat = istat)
    if (istat /= 0) then
       call prterr(lundia, 'U021', 'rddis: memory alloc error')
       call d3stop(1, gdp)
    endif
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
    cntain = 'regular    # discharge without momentum '
    !
    lunout = 8
    !
    ! Initialize global parameters
    !
    fildis = fildef
    !
    ! Define parameter names,
    ! Discharge + LSTSC constituens names from NAMCON and to
    ! extra for flow magnitude / directory for discharge
    !
    lnconc = 1
    parnam(lnconc) = 'Time starting at ITDATE = 0.0 '
    parunt(lnconc) = '[   min  ]'
    lnconc = lnconc + 1
    parnam(lnconc) = 'flux/discharge rate'
    parunt(lnconc) = '[ m**3/s ]'
    do l = 1, lstsc
       lnconc = lnconc + 1
       parnam(lnconc) = namcon(l)
       parunt(lnconc) = '[    -   ]'
       if (parnam(lnconc)(:8)=='salinity') parunt(lnconc) = '[   ppt  ]'
       if (parnam(lnconc)(:11)=='temperature') parunt(lnconc) = '[   deg  ]'
       if (parnam(lnconc)(:8)=='sediment') parunt(lnconc) = '[ kg/m3  ]'
    enddo
    lnconc = lnconc + 1
    parnam(lnconc) = 'flow magnitude'
    parunt(lnconc) = '[   m/s  ]'
    lnconc = lnconc + 1
    parnam(lnconc) = 'flow direction'
    parunt(lnconc) = '[   deg  ]'
    !
    ! locate 'Fildis' record for time varying process data at
    ! discharge points in extra input file
    !
    keyw = 'Fildis'
    ntrec = nrrec
    lenc = 12
    call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
              & mdfrec    ,fildis    ,fildef    ,lenc      ,nrrec     , &
              & ntrec     ,lundia    ,gdp       )
    !
    !
    ! reading error?
    !
    if (lerror) then
       lerror = .false.
       fildis = fildef
    endif
    !
    ! read data from external file
    !
    if (fildis/=fildef) then
       !
       ! Check filename "fildis" <> TMP file or
       ! "fildis" = TMP file and access is direct
       ! Define length of RUNID
       ! open output file (ONLY VERSION 2.49 or lower) +
       ! Set name for the constituents, Regenerated locally
       !
       call remove_leading_spaces(runid     ,lrid      )
       filout = 'TMP_' // runid(:lrid) // '.dis'
       !
       ! Check filename and access
       ! For NRVER =< 249 this cannot be the case
       !
       if (filout==fildis) then
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
       call remove_leading_spaces(fildis    ,lf        )
       !
       ! test file existence <YES> -> open file <NO> -> error
       !
       if (exifil(fildis, lundia)) then
          if (nrver<=249) then
             !
             ! Open file only in case NRVER =< 249. Otherwise it will be
             ! opened later as the record length is dependent on the
             ! profile type
             ! Total lenght from strings formats (FMTDIS(3) or (11))=63
             ! from discharge = 13+(1+lstsc)*14 (MAX = 125)
             !
             mxlrec = 125
             inquire (file = filout(:8 + lrid), exist = ex)
             if (ex) then
                open (newunit=lunout, file = filout(:8 + lrid))
                close (lunout, status = 'delete')
             endif
             open (newunit=lunout, file = filout(:8 + lrid), form = 'formatted',     &
                 & access = 'direct', status = 'unknown', recl = mxlrec)
             write (lunout, fmtdis(1), rec = 1) '#', mxlrec, eol
             !
             ! Open FILDIS to read data from
             !
             open (newunit=lunrd, file = fildis(:lf), form = 'formatted',            &
                  & status = 'old')
             write (message, '(2a)') 'Reading Discharge file ', fildis(:lf)
             call prterr(lundia, 'G051', trim(message))
             nrval = 1 + lstsc
             call rdtdd(lundia    ,lunout    ,lunrd     ,error     ,fildis    , &
                      & runid     ,cntain    ,eol       ,itstrt    ,itfinish  , &
                      & nsrc      ,lstsc     ,nrval     ,rval      ,namsrc    , &
                      & disint    ,parnam    ,parunt    ,gdp       )
             !
             close (lunrd)
          elseif (.not.noread) then
             !
             ! Open FILDIS to read data from
             !
             open (newunit=lunrd, file = fildis(:lf), form = 'formatted',            &
                  & status = 'old')
             write (message, '(2a)') 'Reading Discharge file ', fildis(:lf)
             call prterr(lundia, 'G051', trim(message))
             maxval = 1 + lstsc + 2
             call rdtddn(lundia    ,lunout    ,lunrd     ,error     ,filout    , &
                       & fildis    ,runid     ,eol       ,itstrt    ,itfinish  , &
                       & nsrc      ,lstsc     ,rval      ,maxval    ,namsrc    , &
                       & disint    ,parnam    ,parunt    ,bubble    ,gdp       )
             !
             close (lunrd)
          else
             !
             ! Reading TDD file for discharges skipped in TDATOM
             ! Define "fake" timeframe
             !
             write (message, '(3a)') 'Discharge file ', fildis(:lf), ' will be skipped in TDATOM'
             call prterr(lundia, 'G051', trim(message))
          endif
       else
          error = .true.
       endif
    !
    ! Time varying data at discharge points in file? <NO>
    ! This part is from now obsolete (all data are read from file)
    ! It is kept here to guarantee downwards compatibility
    !
    elseif (nsrc>0) then
       !
       ! Define length of RUNID
       ! open output file (ONLY VERSION 2.49 or lower) +
       ! Set name for the parameters, Regenerated locally
       !
       call remove_leading_spaces(runid     ,lrid      )
       filout = 'TMP_' // runid(:lrid) // '.dis'
       !
       ! Open file only in case NRVER =< 249, which always the
       ! case for data in MDF file
       !
       mxlrec = 125
       inquire (file = filout(:8 + lrid), exist = ex)
       if (ex) then
          open (newunit=lunout, file = filout(:8 + lrid))
          close (lunout, status = 'delete')
       endif
       open (newunit=lunout, file = filout(:8 + lrid), form = 'formatted',           &
            & access = 'direct', status = 'unknown', recl = mxlrec)
       write (lunout, fmtdis(1), rec = 1) '#', mxlrec, eol
       !
       ! Time varying data at discharge points contains a group
       ! of records. all records part of the group are supposed to lie
       ! between two records with keyword 'Tsdis  '
       ! first set records = first opening => start on top of file
       ! Count for number of times specified
       !
       write (fmtdis(13)(8:9), '(i2.2)') lstsc + 1
       !
       keyw = 'Tsdis '
       lkw = 5
       ndistm = 0
       newkw = .false.
       !==>
  150  continue
       call search(lunmd     ,lerror    ,newkw     ,nrrec     ,found     , &
                 & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , &
                 & 'NO'      )
       if (found) then
          ndistm = ndistm + 1
          goto 150
       endif
       !
       ndistm = ndistm/nsrc
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
       nlook = 2
       !
       do id = 1, ndistm
          idis = 1
          !
          ! Read process values and write to help array's for every NSRC
          !
          do n = 1, nsrc
             !
             ! locate 'Tsdis ' record
             ! NEWKW = .true. only for the first record
             !
             keyw = 'Tsdis '
             nlook = 2
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
             ! Perform some checks with the time read
             !
             if (n==1) then
                rtdis(idis) = rval(1)
                call chckit(lundia    ,lerror    ,'MD-file' ,rval(1)   ,dt        , &
                          & ittdep    ,itold     ,itstrt    ,id        ,gdp       )
                !
                if (lerror) then
                   error = .true.
                   lerror = .false.
                   goto 700
                endif
                if (ittdep>itold) itold = ittdep
             endif
             !
             ll = 1
             disch(idis, n) = rval(2)
             rwdis(ll) = rval(2)
             !
             ! locate and read 'Cqs   ' record
             ! default value not allowed incase IER < or = 0
             !
             if (salin) then
                nlook = 1
                keyw = 'Cqs   '
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
                   goto 700
                endif
                !
                ! copy RVAL to appropriate array
                !
                ll = ll + 1
                cqs(idis, n) = rval(1)
                rwdis(ll) = rval(1)
                !
                ! check for negative values
                !
                if (cqs(idis, n)<0.0) then
                   call prterr(lundia    ,'V061'    ,'Salinity at discharge point'   )
                   !
                   error = .true.
                   goto 700
                endif
             endif
             !
             ! locate and read 'Cqt   ' record
             ! default value not allowed incase IER < or = 0
             !
             if (temp) then
                nlook = 1
                keyw = 'Cqt   '
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
                   goto 700
                endif
                !
                ! copy RVAL to appropriate array
                !
                ll = ll + 1
                cqt(idis, n) = rval(1)
                rwdis(ll) = rval(1)
                !
                ! check for negative values
                !
                if (cqt(idis, n)<0.0) then
                   call prterr(lundia    ,'V061'    ,'Temperature at discharge point')
                   !
                   error = .true.
                   goto 700
                endif
             endif
             !
             ! locate and read 'CqcN  ' record
             ! default value not allowed incase IER < or = 0
             !
             if (const) then
                nlook = lconc
                keyw = 'CqcN  '
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
                   goto 700
                endif
                !
                ! copy RVAL to appropriate array
                !
                do l = 1, lconc
                   ll = ll + 1
                   cqc(l, idis, n) = rval(l)
                   rwdis(ll) = rval(l)
                   !
                   ! check for negative values
                   !
                   if (cqc(l, idis, n)<0.0) then
                      call prterr(lundia    ,'V061'    ,'Concentration at discharge point'         )
                      !
                      error = .true.
                      goto 700
                   endif
                enddo
             endif
             !
             ! Write discharge data to LUNOUT
             !
             irec = (n - 1)*(ndistm + 9 + ll) + 1
             !
             ! Write first 9+LL records with block information
             ! for time ID=1
             !
             if (id==1) then
                interp = 'linear    '
                if (disint(n)=='N') interp = 'block    '
                !
                ! Define table name
                !
                tablnm = 'T-serie DIS ' // namsrc(n) // ' for run: '
                !
                ! Write first 7 description records to file
                !
                write (lunout, fmtdis(2), rec = irec + 1) &
                    & keywrd(1), quote, tablnm, runid, quote, eol
                write (lunout, fmtdis(3), rec = irec + 2) &
                    & keywrd(2), quote, cntain(:10), quote, cntain(11:40), eol
                write (lunout, fmtdis(4), rec = irec + 3) &
                    & keywrd(3), quote, namsrc(n), quote, eol
                write (lunout, fmtdis(7), rec = irec + 4) &
                    & keywrd(8), quote, 'non-equidistant', quote, eol
                write (lunout, fmtdis(8), rec = irec + 5) &
                    & keywrd(9), itdate, eol
                write (lunout, fmtdis(9), rec = irec + 6) &
                    & keywrd(10), quote, 'minutes', quote, eol
                write (lunout, fmtdis(10), rec = irec + 7) &
                    & keywrd(12), quote, interp, quote, eol
                !
                ! Write parameter name for time to file
                !
                write (lunout, fmtdis(11), rec = irec + 8) &
                    & keywrd(14), quote, parnam(1), quote, keywrd(15)(:10), quote, parunt(1),  &
                    & quote, eol
                !
                ! Write parameter names for discharges and concentrations
                !
                do l = 1, ll
                   write (lunout, fmtdis(11), rec = irec + 8 + l) &
                       & keywrd(14), quote, parnam (1 + l), quote, keywrd(15)  &
                       & (:10), quote, parunt (1 + l), quote, eol
                enddo
                !
                ! Write number of time dependent data to file
                !
                write (lunout, fmtdis(12), rec = irec + 9 + ll) &
                    & keywrd(16), ndistm, eol
             endif
             !
             ! Write time dependent data to block for constituent
             ! L skipping first 9+LL records with block info
             !
             write (lunout, fmtdis(13), rec = irec + 9 + ll + id) &
                 & rtdis(idis) , (rwdis(l), l = 1, ll), eol
          enddo
          newkw = .false.
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
                call prterr(lundia    ,'U042'    ,'Last time for time varying discharge rates <')
                error = .true.
                goto 9999
            endif
       endif
       ndistm = idis - 1
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
    !
    deallocate(rval  )
    deallocate(rwdis )
    deallocate(parunt)
    deallocate(parnam)
end subroutine rddis
