subroutine rdtdcn(lundia    ,lunout    ,lunrd     ,error     ,filout    , &
                & filbcc    ,runid     ,profil    ,eol       ,itstrt    , &
                & itfinish  ,nto       ,lstsc     ,kmax      ,nambnd    , &
                & namcon    ,bubble    ,gdp       )
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
!    Function: Reads time dependent constituent data from new
!              bc-file for the constituents
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
    use globaldata
    use string_module
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    include 'pardef.igd'
    integer                    , pointer :: itdate
    real(fp)                   , pointer :: dt
    real(fp)                   , pointer :: tunit
    character*20, dimension(:) , pointer :: keywrd
    character*38, dimension(:) , pointer :: fmtbcc
!
! Global variables
!
    integer                        , intent(in)  :: itfinish !  Description and declaration in inttim.igs
    integer                                      :: itstrt   !  Description and declaration in inttim.igs
    integer                                      :: kmax     !  Description and declaration in esm_alloc_int.f90
    integer                        , intent(in)  :: lstsc    !  Description and declaration in dimens.igs
    integer                                      :: lundia   !  Description and declaration in inout.igs
    integer                                      :: lunout   !  Unit number for direct access Delft3D-FLOW help file between TDATOM and TRISIM
    integer                                      :: lunrd    !  Unit number of the attribute file containing the time series
    integer                        , intent(in)  :: nto      !  Description and declaration in esm_alloc_int.f90
    logical                        , intent(in)  :: bubble   !  Description and declaration in procs.igs
    logical                                      :: error    !  Flag=TRUE if an error is encountered
    character(*)                                 :: filbcc   !  Name of the specified data file
    character(*)                                 :: filout   !  Name of the output file to be opened
    character(*)                   , intent(in)  :: runid    !  Run identification code for the current simulation (used to determine
                                                             !  the names of the in- /output files used by the system)
    character(1)                   , intent(in)  :: eol      !  ASCII code for End-Of-Line (^J)
    character(20), dimension(lstsc), intent(in)  :: namcon   !  Description and declaration in esm_alloc_char.f90
    character(20), dimension(nto)                :: nambnd   !  Description and declaration in esm_alloc_char.f90
    character(40)                  , intent(in)  :: profil   !  Total string of possible profiles
!
! Local variables
!
    integer                                   :: ibeg   ! Begin position in the RECORD from where the search for data/record is started 
    integer                                   :: iend   ! Last position in the RECORD when the searched data/record is finished 
    integer                                   :: ier    ! =  0 -> end of record encountered =  1 -> real value found = -1 -> length or number of data is larger then specified by the calling routine 
    integer                                   :: iocond ! IO status for reading 
    integer                                   :: irec
    integer                                   :: irecrd ! Counter of records if input file is a direct access file 
    integer                                   :: itold  ! Help var. to store last read time to test accending order 
    integer                                   :: ittdep ! Help var. for the time read (now de- fined as multiples of DT, but in fu- ture it may take any value) 
    integer                                   :: ix
    integer                                   :: k
    integer                                   :: l
    integer                                   :: lflbcc ! Actual length of FILBCC 
    integer                                   :: lflout ! Actual length of FILOUT 
    integer                                   :: lrecrd ! Length of record read 
    integer                                   :: mxlrec ! Actual maximum record length (:= KMAX*24*2 + 24) 
    integer                                   :: n      ! Help var. for the nr. of data to be read (see LENDAT) 
    integer                                   :: nb
    integer                                   :: newlun
    integer                                   :: nlook  ! Nr. of data to look for in RECORD 
    integer                                   :: np
    integer                                   :: npara  ! Number of parameter records 
    integer                                   :: nparrd ! NR. of parameter records actual read 
    integer                                   :: npconc ! NR. of parameter records actual read for constituents only 
    integer                                   :: nr
    integer                                   :: ntimrd
    logical                                   :: access ! Flag to read file as direct access or sequential 
    logical                                   :: ex     ! Flag to test if file exists 
    real(fp)                                  :: rdfaul ! Default value when RARRAY(N) not found 
    real(fp)                                  :: sdt    ! dt*tunit/60
    real(fp)                                  :: timrd  ! Time in minutes read 
    real(fp)                                  :: timscl
    real(fp), dimension(5)                    :: rarray ! Help array to read 5 values from RECORD 
    real(fp), dimension(:,:,:,:),allocatable  :: rwbval ! 7 Array for the time dependent data:
                                                        !    1,1,N,L = value at surface, at A
                                                        !    1,2,N,L = value at bottom , at A
                                                        !    2,1,N,L = value at surface, at B
                                                        !    2,2,N,L = value at bottom , at B
                                                        !   or
                                                        !    1,K,N,L = value at layer K, at A
                                                        !    2,K,N,L = value at layer K, at B 
    real(fp), dimension(:,:),allocatable      :: zstep  ! Time varying location of disconti- nuity for the 3D BC for constituents 
    character(1)                              :: interp
    character(1)                              :: quote  ! Apostrophe ASCII-character 39 
    character(10), dimension(2)               :: parunt ! Unit name fitting the parameter 
    character(10), dimension(:,:),allocatable :: tprofc ! Vertical profile for constituent - uniform - linear - step - 3d-profile 
    character(36), dimension(1 + 2*kmax)      :: parrd  ! Parameter names read 
    character(36), dimension(2)               :: parnam ! Names of the paramaters to write to time dependent files for BCC 
    character(40)                             :: cntain
    character(400)                            :: errmsg ! Character var. containing the error message to be written to file. The message depends on the error. 
    character(kmax*24*2+48)                   :: record ! Standard rec. length in an attribute file (maximum kmax*24*2 + 48) 
    character(63)                             :: tablnm ! Table name specification 
    !
    data rarray/5*0.0/
!
!! executable statements -------------------------------------------------------
!
    ! Initialisation
    ! >>>>>for now reading the direct access file not possible
    !
    fmtbcc  => gdp%gdfmtbcc%fmtbcc
    keywrd  => gdp%gdkeywtd%keywrd
    itdate  => gdp%gdexttim%itdate
    dt      => gdp%gdexttim%dt
    tunit   => gdp%gdexttim%tunit
    !
    allocate(rwbval(kmax, 2, mxnto, lstsc))
    allocate(zstep(mxnto, lstsc))
    allocate(tprofc(mxnto, lstsc))
    !
    access = .false.
    irecrd = 0
    quote  = char(39)
    cntain = ' # at ends A&B of open boundary segment '
    interp = 'Y'
    !
    call remove_leading_spaces(filout    ,lflout    )
    call remove_leading_spaces(filout    ,lflbcc    )
    !
    ! Define parameter name for time column
    !
    parnam(1) = 'Time starting at ITDATE = 0.0       '
    parunt(1) = '[   min  ]'
    !
    ! Initialize RWBVAL, ZSTEP and TPROFC array
    !
    do n = 1, nto
       do l = 1, lstsc
          do k = 1, kmax
             rwbval(k, 1, n, l) = 0.0
             rwbval(k, 2, n, l) = 0.0
          enddo
          zstep (n, l) = 0.0
          tprofc(n, l) = ' '
       enddo
    enddo
    !
    ! Start reading the time dependent data.
    !
    do n = 1, nto
       do l = 1, lstsc
          ittdep = -1
          itold  = -1
          !
          read (lunrd, '(a)', iostat = iocond) record
          !
          ! Premature EOR encountered or error (IOCOND <> 0)
          ! Test last time read
          ! Error messages not completely correct
          !
          if (iocond/=0) then
             error = .true.
             call prterr(lundia    ,'G007'    ,filbcc    )
             if (iocond<0) then
                if (itold < itfinish) then
                   write(errmsg,'(a,a,a)') 'Last time in file ', trim(filbcc), ' <' 
                   call prterr(lundia    ,'U042'    ,errmsg)
                   error = .true.
                   goto 9999
                elseif (n<nto .or. l<lstsc) then
                   write(errmsg,'(a,a)') 'not for all open boundaries BCC data defined in ', trim(filbcc) 
                   call prterr(lundia    ,'U021'    ,errmsg)
                   error = .true.
                   goto 9999
                else
                endif
             endif
          endif
          !
          ! Check for EOL in file and find maximum number of characters
          ! to read if EOL is located.
          ! Only for UNIX systems where EOL = ^J
          !
          lrecrd = len(record)
          if (eol/=' ' .and. index(record, eol)/=0) lrecrd = index(record, eol)
          !
          ! Read all keywords prior to time dependent data records
          !
          npara = 1 + 2*kmax
          call flhnew(lunrd     ,lundia    ,error     ,record(:lrecrd)      ,access    , &
                    & irecrd    ,nambnd(n) ,tprofc(n, l)         ,interp    ,itdate    , &
                    & timscl    ,ntimrd    ,parrd     ,npara     ,nparrd    , &
                    & bubble    ,gdp       )
          if (error) goto 9999
          !
          ! Test profile definition
          !
          if (index(profil, tprofc(n, l))==0) then
             call prterr(lundia    ,'V096'    ,tprofc(n, l)         )
             error = .true.
             goto 9999
          endif
          !
          ! Test number of parameters read incombination with defined
          !
          if (tprofc(n, l)(:7)=='uniform') then
             if (nparrd/=3) then
                call prterr(lundia    ,'V097'    ,' '       )
                error = .true.
                goto 9999
             endif
          elseif (tprofc(n, l)(:6)=='linear') then
             if (nparrd/=5) then
                call prterr(lundia    ,'V097'    ,' '       )
                error = .true.
                goto 9999
             endif
          elseif (tprofc(n, l)(:4)=='step') then
             if (nparrd/=6) then
                call prterr(lundia    ,'V097'    ,' '       )
                error = .true.
                goto 9999
             endif
          elseif (tprofc(n, l)=='3d-profile') then
             if (nparrd/=1 + 2*kmax) then
                call prterr(lundia    ,'V097'    ,' '       )
                error = .true.
                goto 9999
             endif
          else
          endif
          !
          ! Test name of constituent conform read parameter name
          ! Only for the constituent names, so for step last parameter
          ! should be tested for 'dicontinuity'
          !
          npconc = nparrd
          if (tprofc(n, l)(:4)=='step') npconc = nparrd - 1
          do np = 2, npconc
             if (parrd(np)(:20)/=namcon(l)) then
                call prterr(lundia    ,'V096'    ,parrd(np) )
                error = .true.
                goto 9999
             endif
          enddo
          !
          if (tprofc(n, l)(:4)=='step') then
             if (parrd(nparrd)(:13)/='discontinuity') then
                call prterr(lundia    ,'V096'    ,parrd(nparrd)        )
                error = .true.
                goto 9999
             endif
          endif
          !
          ! Determine the record length for direct access file and the
          ! corresponding format
          !
          if (n==1 .and. l==1) then
             mxlrec = max(89,16 + kmax*2*14 + 1)
             if (mxlrec > 89) then           
                do nb = 1, mxfmtc
                   ix = index(fmtbcc(nb), 't89')
                   if (ix/=0) then
                      write (fmtbcc(nb)(ix + 1:ix + 5), '(i5.5)') mxlrec
                   endif
                enddo
             endif
             !
             ! Open output file
             ! and write length direct access file to file first
             !
             inquire (file = filout(:lflout), exist = ex)
             if (ex) then
                open (newunit=lunout, file = filout(:lflout))
                close (lunout, status = 'delete')
             endif
             open (newunit=lunout, file = filout(:lflout), form = 'formatted',          &
                  & access = 'direct', status = 'unknown', recl = mxlrec)
             irec = 1
             write (lunout, fmtbcc(1), rec = irec) '#', mxlrec, eol
          endif
          !
          ! Define table name
          !
          tablnm = 'T-serie BCC ' // nambnd(n) // '-' // namcon(l)              &
                  & // ' for run: '
          !
          ! Write first 7 description records to file
          !
          write (lunout, fmtbcc(2), rec = irec + 1) &
              & keywrd(1), quote, tablnm, runid, quote, eol
          write (lunout, fmtbcc(3), rec = irec + 2) &
              & keywrd(2), quote, tprofc(n, l), quote, cntain, eol
          write (lunout, fmtbcc(4), rec = irec + 3) &
              & keywrd(3), quote, nambnd(n), quote, eol
          write (lunout, fmtbcc(7), rec = irec + 4) &
              & keywrd(8), quote, 'non-equidistant', quote, eol
          write (lunout, fmtbcc(8), rec = irec + 5) keywrd(9), itdate, eol
          write (lunout, fmtbcc(9), rec = irec + 6) &
              & keywrd(10), quote, 'minutes', quote, eol
          write (lunout, fmtbcc(10), rec = irec + 7) &
              & keywrd(12), quote, 'linear', quote, eol
          !
          ! Write parameter name for time to file
          !
          npara = 1
          write (lunout, fmtbcc(11), rec = irec + 7 + npara) &
              & keywrd(14), quote, parnam(1), quote, keywrd(15)(:10), quote, parunt(1),  &
              & quote, eol
          !
          ! Re-define parameter name for constituent L
          !
          parnam(2)(:20) = namcon(l)
          parunt(2) = '[    -   ]'
          if (parnam(2)(:8)=='salinity') parunt(2) = '[   ppt  ]'
          if (parnam(2)(:11)=='temperature') parunt(2) = '[   deg  ]'
          !
          ! Write parameter names for profile <uniform>
          !
          if (tprofc(n, l)(:7)=='uniform') then
             npara = npara + 1
             parnam(2)(21:) = ' end A uniform  '
             write (lunout, fmtbcc(11), rec = irec + 7 + npara) &
                 & keywrd(14), quote, parnam(2) , quote, keywrd(15)(:10), quote, parunt(2)  &
                 & , quote, eol
             npara = npara + 1
             parnam(2)(21:) = ' end B uniform  '
             write (lunout, fmtbcc(11), rec = irec + 7 + npara) &
                 & keywrd(14), quote, parnam(2) , quote, keywrd(15)(:10), quote, parunt(2)  &
                 & , quote, eol
          !
          ! Write parameter names for profile <linear>
          !
          elseif (tprofc(n, l)(:6)=='linear') then
             npara = npara + 1
             parnam(2)(21:) = ' end A surface  '
             write (lunout, fmtbcc(11), rec = irec + 7 + npara) &
                 & keywrd(14), quote, parnam(2) , quote, keywrd(15)(:10), quote, parunt(2)  &
                 & , quote, eol
             npara = npara + 1
             parnam(2)(21:) = ' end A bed      '
             write (lunout, fmtbcc(11), rec = irec + 7 + npara) &
                 & keywrd(14), quote, parnam(2) , quote, keywrd(15)(:10), quote, parunt(2)  &
                 & , quote, eol
             npara = npara + 1
             parnam(2)(21:) = ' end B surface  '
             write (lunout, fmtbcc(11), rec = irec + 7 + npara) &
                 & keywrd(14), quote, parnam(2) , quote, keywrd(15)(:10), quote, parunt(2)  &
                 & , quote, eol
             npara = npara + 1
             parnam(2)(21:) = ' end B bed      '
             write (lunout, fmtbcc(11), rec = irec + 7 + npara) &
                 & keywrd(14), quote, parnam(2) , quote, keywrd(15)(:10), quote, parunt(2)  &
                 & , quote, eol
          !
          ! Write parameter names for profile <step>
          !
          elseif (tprofc(n, l)(:4)=='step') then
             npara = npara + 1
             parnam(2)(21:) = ' end A surface  '
             write (lunout, fmtbcc(11), rec = irec + 7 + npara) &
                 & keywrd(14), quote, parnam(2) , quote, keywrd(15)(:10), quote, parunt(2)  &
                 & , quote, eol
             npara = npara + 1
             parnam(2)(21:) = ' end A bed      '
             write (lunout, fmtbcc(11), rec = irec + 7 + npara) &
                 & keywrd(14), quote, parnam(2) , quote, keywrd(15)(:10), quote, parunt(2)  &
                 & , quote, eol
             npara = npara + 1
             parnam(2)(21:) = ' end B surface  '
             write (lunout, fmtbcc(11), rec = irec + 7 + npara) &
                 & keywrd(14), quote, parnam(2) , quote, keywrd(15)(:10), quote, parunt(2)  &
                 & , quote, eol
             npara = npara + 1
             parnam(2)(21:) = ' end B bed      '
             write (lunout, fmtbcc(11), rec = irec + 7 + npara) &
                 & keywrd(14), quote, parnam(2) , quote, keywrd(15)(:10), quote, parunt(2)  &
                 & , quote, eol
             npara = npara + 1
             parnam(2) = 'discontinuity'
             parunt(2) = '[    m   ]'
             write (lunout, fmtbcc(11), rec = irec + 7 + npara) &
                 & keywrd(14), quote, parnam(2) , quote, keywrd(15)(:10), quote, parunt(2)  &
                 & , quote, eol
          !
          ! Write parameter names for profile <3d-profile>
          !
          elseif (tprofc(n, l)=='3d-profile') then
             do k = 1, kmax
                npara = npara + 1
                parnam(2)(21:) = ' end A layer    '
                write (parnam(2)(34:36), '(i3)') k
                write (lunout, fmtbcc(11), rec = irec + 7 + npara) &
                    & keywrd(14), quote, parnam(2), quote, keywrd(15) (:10), quote,  &
                    & parunt(2), quote, eol
             enddo
             do k = 1, kmax
                npara = npara + 1
                parnam(2)(21:) = ' end B layer    '
                write (parnam(2)(34:36), '(i3)') k
                write (lunout, fmtbcc(11), rec = irec + 7 + npara) &
                    & keywrd(14), quote, parnam(2), quote, keywrd(15) (:10), quote,  &
                    & parunt(2), quote, eol
             enddo
          else
          endif
          !
          ! Write number of time dependent data to file
          !
          write (lunout, fmtbcc(12), rec = irec + 8 + npara) &
              & keywrd(16), ntimrd, eol
          !
          ! Re-define IREC 8+NPARA description records forward
          !
          irec = irec + 8 + npara
          !
          ! Read step values (optional) & concentrations for all open
          ! boundaries and all constituents depending on TPROFC value
          !
          do nr = 1, ntimrd
             !
             ! Read time.
             !
             ibeg = 1
             call read1r(record    ,lrecrd    ,ibeg      ,iend      ,timrd     , &
                       & rdfaul    ,ier       )
             !
             ! Premature EOR (IER = 0) or empty value (IER = -1)
             !
             if (ier<=0) then
                error = .true.
                call prterr(lundia    ,'G007'    ,filbcc    )
                !
                write (lundia, '(a,a)') 'RECORD: ', record(:72)
                goto 9999
             endif
             !
             ! Re-defined time read (TIMRD ) in minutes
             !
             timrd = timrd*timscl
             !
             ! Perform some checks on time value read
             !
             sdt = dt*(tunit/60.0)
             call chckit(lundia    ,error     ,filbcc    ,timrd     ,sdt       , &
                       & ittdep    ,itold     ,itstrt    ,nr        ,gdp       )
             if (error) goto 9999
             !
             ! Read time dependent data depending on value of TPROFC
             !
             if (tprofc(n, l)=='3d-profile') then
                if (tprofc(n, l)/=tprofc(max(1, n - 1), l)) then
                   errmsg = '3d-profile must precede other type'
                   call prterr(lundia    ,'U021'    ,errmsg    )
                   error = .true.
                   goto 9999
                endif
                !
                ! KMAX values for 3d-profile open boundary point A
                !
                ibeg = iend + 1
                call readnr(record    ,lrecrd    ,ibeg      ,iend      ,kmax      , &
                          & rwbval(1, 1, n, l)   ,rdfaul    ,ier       )
                !
                ! Premature EOR (IER = 0) or empty value (IER = -1)
                !
                if (ier<=0) then
                   error = .true.
                   call prterr(lundia    ,'G007'    ,filbcc    )
                   write (lundia, '(a,a)') 'RECORD: ', record(:72)
                   goto 9999
                endif
                !
                ! KMAX values for 3d-profile open boundary point B
                !
                ibeg = iend + 1
                call readnr(record    ,lrecrd    ,ibeg      ,iend      ,kmax      , &
                          & rwbval(1, 2, n, l)   ,rdfaul    ,ier       )
                !
                ! Premature EOR (IER = 0) or empty value (IER = -1)
                !
                if (ier<=0) then
                   error = .true.
                   call prterr(lundia    ,'G007'    ,filbcc    )
                   write (lundia, '(a,a)') 'RECORD: ', record(:72)
                   goto 9999
                endif
             else
                !
                ! Other profiles for open boundary point A and B
                !
                nlook = 5
                if (tprofc(n, l)=='linear    ') nlook = 4
                if (tprofc(n, l)=='uniform   ') nlook = 2
                ibeg = iend + 1
                call readnr(record    ,lrecrd    ,ibeg      ,iend      ,nlook     , &
                          & rarray    ,rdfaul    ,ier       )
                !
                ! Premature EOR or empty value (IER = -1)
                !
                if (ier<=0) then
                   error = .true.
                   call prterr(lundia    ,'G007'    ,filbcc    )
                   write (lundia, '(a,a)') 'RECORD: ', record(:72)
                   goto 9999
                endif
                !
                ! Define NLOOK values to respective array's
                ! timscl introduced to make input of times more flexible
                !
                rwbval(1, 1, n, l) = rarray(1)
                if (tprofc(n, l)=='uniform   ') then
                   rwbval(1, 2, n, l) = rarray(2)
                else
                   rwbval(kmax, 1, n, l) = rarray(2)
                   rwbval(1, 2, n, l) = rarray(3)
                   rwbval(kmax, 2, n, l) = rarray(4)
                   if (tprofc(n, l)=='step      ') then
                      zstep(n, l) = rarray(5)
                   endif
                endif
             endif
             !
             ! Check for negative values
             !
             do k = 1, kmax
                if (rwbval(k, 1, n, l)<0.0) error = .true.
                if (rwbval(k, 2, n, l)<0.0) error = .true.
             enddo
             if (error) then
                write(errmsg,'(a,a)') 'Concentration at open boundary in ', trim(filbcc)
                call prterr(lundia    ,'V061'    ,errmsg)
             endif
             !
             ! If an NaN is read -> error
             !
             do k = 1, kmax
                if (isnan(rwbval(k, 1, n, l))) error = .true.
                if (isnan(rwbval(k, 2, n, l))) error = .true.
             enddo
             if (error) then
                write(errmsg,'(a,a)') 'Concentration at open boundary contains NaN in ', trim(filbcc)
                call prterr(lundia    ,'P004'    ,errmsg      )
             endif
             !
             ! Write time dependent data depending on value of TPROFU
             !
             if (tprofc(n, l)(:7)=='uniform') then
                fmtbcc(13)(8:10) = '  2'
                write (lunout, fmtbcc(13), rec = irec + nr) &
                    & timrd, rwbval(1, 1, n, l), rwbval(1, 2, n, l), eol
             elseif (tprofc(n, l)(:6)=='linear') then
                fmtbcc(13)(8:10) = '  4'
                write (lunout, fmtbcc(13), rec = irec + nr) &
                    & timrd, rwbval(1, 1, n, l), rwbval(kmax, 1, n, l), rwbval(1, 2, n, l),  &
                    & rwbval(kmax, 2, n, l), eol
             elseif (tprofc(n, l)(:4)=='step') then
                fmtbcc(13)(8:10) = '  5'
                write (lunout, fmtbcc(13), rec = irec + nr) &
                    & timrd, rwbval(1, 1, n, l), rwbval(kmax, 1, n, l), rwbval(1, 2, n, l),  &
                    & rwbval(kmax, 2, n, l), zstep(n, l), eol
             else
                write (fmtbcc(13)(8:10), '(i3)') 2*kmax
                write (lunout, fmtbcc(13), rec = irec + nr) &
                    & timrd, (rwbval(k, 1, n, l), k = 1, kmax), (rwbval(k, 2, n, l),  k = 1, kmax), eol
             endif
             !
             ! Re-define ITOLD and
             ! As long as NR < NTIMRD read new RECORD
             !
             itold = ittdep
             if (nr<=ntimrd - 1) read (lunrd, '(a)') record(:lrecrd)
          enddo
          !
          ! Re-define IREC NTIMRD records forward
          !
          irec = irec + ntimrd
          !
          ! Define maximum time
          !
          if (itold/= - 1) then
             if (itold < itfinish) then
                write(errmsg,'(a,a,a,a,a)') 'Last time in file ', trim(filbcc), ' for section #',trim(nambnd(n)),'# <' 
                call prterr(lundia    ,'U042'    ,errmsg)
                error = .true.
                goto 9999
             endif
          endif
       enddo
    enddo
    !
    ! stop reading file
    !
 9999 continue
    deallocate(rwbval)
    deallocate(zstep)
    deallocate(tprofc)
end subroutine rdtdcn
