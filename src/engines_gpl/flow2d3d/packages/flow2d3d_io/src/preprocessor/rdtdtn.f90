subroutine rdtdtn(lundia    ,lunout    ,lunrd     ,error     ,filout    , &
                & filbct    ,runid     ,typtst    ,eol       ,itstrt    , &
                & itfinish  ,nto       ,ntof      ,ntot      ,kmax      , &
                & tprofu    ,nambnd    ,typbnd    ,namtyp    ,unttyp    , &
                & bubble    ,gdp       )
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
!              bc-file for flow
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
    character*37, dimension(:) , pointer :: fmtbct
!
! Global variables
!
    integer                      , intent(in)  :: itfinish !  Description and declaration in inttim.igs
    integer                                    :: itstrt   !  Description and declaration in inttim.igs
    integer                                    :: kmax     !  Description and declaration in esm_alloc_int.f90
    integer                                    :: lundia   !  Description and declaration in inout.igs
    integer                                    :: lunout   !  Unit number for unformatted FLOW help file between tdatom and trisim
    integer                                    :: lunrd    !  Unit number of the attribute file containing the time series
    integer                      , intent(in)  :: nto      !  Description and declaration in esm_alloc_int.f90
    integer                      , intent(in)  :: ntof     !  Description and declaration in dimens.igs
    integer                      , intent(in)  :: ntot     !  Description and declaration in dimens.igs
    logical                      , intent(in)  :: bubble   !  Description and declaration in procs.igs
    logical                                    :: error    !  Flag=TRUE if an error is encountered
    character(*)                               :: filbct   !  File name for the time varying boundary conditions file
    character(*)                               :: filout   !  Name of the output file to be opened
    character(*)                 , intent(in)  :: runid    !  Run identification code for the current simulation (used to determine
                                                           !  the names of the in- /output files used by the system)
    character(1)                 , intent(in)  :: eol      !  ASCII code for End-Of-Line (^J)
    character(1), dimension(nto) , intent(in)  :: typbnd   !  Description and declaration in esm_alloc_char.f90
    character(10), dimension(6)  , intent(in)  :: unttyp   !  Unit name fitting the parameter depending on value of TYPBND
    character(20), dimension(6)  , intent(in)  :: namtyp   !  Names of the paramaters to write to time dependent files for BCT depending on value of TYPBND
    character(20), dimension(nto)              :: nambnd   !  Description and declaration in esm_alloc_char.f90
    character(20), dimension(nto), intent(in)  :: tprofu   !  Description and declaration in esm_alloc_char.f90
    character(6)                 , intent(in)  :: typtst   !  Data string to test type of boundary
!
! Local variables
!
    integer                                :: ibeg   ! Begin position in the RECORD from where the search for data/record is started 
    integer                                :: iend   ! Last position in the RECORD when the searched data/record is finished 
    integer                                :: ier    ! =  0 -> end of record encountered =  1 -> real value found = -1 -> length or number of data is larger then specified by the calling routine 
    integer                                :: iocond ! IO status for reading 
    integer                                :: irec
    integer                                :: irecrd ! Counter of records if input file is a direct access file 
    integer                                :: itold  ! Help var. to store last read time to test accending order 
    integer                                :: ittdep ! Help var. for the time read (now de- fined as multiples of DT, but in fu- ture it may take any value) 
    integer                                :: i
    integer                                :: ix
    integer                                :: k
    integer                                :: kend
    integer                                :: lfbct  ! Actual length of FILBCT 
    integer                                :: lflout ! Actual length of FILOUT 
    integer                                :: lrecrd ! Length of record read 
    integer                                :: mxlrec ! Actual maximum record length (:= KMAX*24*2 + 24) 
    integer                                :: n      ! Help var. for the nr. of data to be read (see LENDAT) 
    integer                                :: nb
    integer                                :: nn
    integer                                :: np
    integer                                :: npara
    integer                                :: nparrd
    integer                                :: nr     ! Sequence nr of the time read 
    integer                                :: ntimrd
    integer                                :: ntyp
    integer, external                      :: newlun
    logical                                :: access ! Flag to read file as direct access or sequential 
    logical                                :: ex     ! Flag to test if file exists 
    real(fp)                               :: rdfaul
    real(fp)                               :: sdt    ! dt*tunit/60
    real(fp)                               :: timrd  ! Time in minutes read 
    real(fp)                               :: timscl
    real(fp), dimension(kmax, 2)           :: rwbval ! Array for the time dependent data 1,1 = value at at A 1,2 = value at at B K,1 = value at layer K, at A K,2 = value at layer K, at B 
    character(1)                           :: interp
    character(1)                           :: quote  ! Apostrophe ASCII-character 39 
    character(10), dimension(2)            :: parunt ! Unit name fitting the parameter 
    character(20)                          :: cntent
    character(36), dimension(1 + 2*kmax)   :: parrd  ! Parameter names read 
    character(36), dimension(2)            :: parnam ! Names of the paramaters to write to time dependent files for BCT 
    character(40)                          :: cntain
    character(400)                         :: errmsg ! Character var. containing the error message to be written to file. The message depend on the error. 
    character(42)                          :: tablnm ! Table name specification 
    character(kmax*24*2 + 48)              :: record ! Standard rec. length in an attribute file (maximum kmax*24*2 + 48) 
!
!
!! executable statements -------------------------------------------------------
!
    fmtbct  => gdp%gdfmtbct%fmtbct
    keywrd  => gdp%gdkeywtd%keywrd
    itdate  => gdp%gdexttim%itdate
    dt      => gdp%gdexttim%dt
    tunit   => gdp%gdexttim%tunit
    !
    access = .false.
    irecrd = 0
    quote  = char(39)
    cntain = ' # at ends A&B of open boundary segment '
    interp = 'Y'
    irec   = 0
    !
    call remove_leading_spaces(filout    ,lflout    )
    call remove_leading_spaces(filbct    ,lfbct     )
    !
    ! Define parameter name for time column
    !
    parnam(1) = 'Time starting at ITDATE = 0.0       '
    parunt(1) = '[   min  ]'
    !
    ! Start reading the time dependent data.
    !
    do nn = 1, ntot
       n = ntof + nn
       ittdep = -999
       itold  = -999
       !
       read (lunrd, '(a)', iostat = iocond) record
       !
       ! Check for EOL in file and find maximum number of characters
       ! to read if EOL is located. Only for UNIX systems where EOL = ^J
       !
       lrecrd = len(record)
       if (eol/=' ' .and. index(record, eol)/=0) lrecrd = index(record, eol)
       !
       ! Premature EOR encountered or error (IOCOND <> 0)
       ! Test last time read
       ! >>>>>>>error messages not completely correct
       !
       if (iocond /= 0) then
          error = .true.
          call prterr(lundia    ,'G007'    ,filbct    )
          if (iocond < 0) then
             if (itold < itfinish) then
                write(errmsg,'(a,a,a)') 'Last time in file ', trim(filbct), ' <' 
                call prterr(lundia    ,'U042'    ,errmsg)
             elseif (nn < ntot) then
                write(errmsg,'(a,a)') 'not for all open boundaries BCT data defined in ', trim(filbct) 
                call prterr(lundia    ,'U021'    ,errmsg        )
             else
             endif
          endif
       endif
       !
       ! Define NTYP index dependent on value of TYPBND(NTOF+N)
       !
       ntyp = index(typtst, typbnd(n))
       !
       ! Read all keywords prior to time dependent data records
       ! Define index number for parameter name
       ! TYPTST = <ZCQRTN>
       ! PARNAM = <water-elevation (z) >
       !          <current         (c) >
       !          <flux/discharge  (q) >
       !          <riemann         (r) >
       !          <total discharge (t) >
       !          <neumann         (n) >
       !
       npara = 1 + 2*kmax
       call flhnew(lunrd     ,lundia    ,error     ,record(:lrecrd)      ,access    , &
                 & irecrd    ,nambnd(n) ,cntent    ,interp    ,itdate    , &
                 & timscl    ,ntimrd    ,parrd     ,npara     ,nparrd    , &
                 & bubble    ,gdp       )
       if (error) then
          exit
       endif
       !
       ! Test profile definition, should be as defined in open
       ! boundary definition attribute files (tested in RDBNDD)
       !
       if (cntent /= tprofu(n)) then
          call prterr(lundia    ,'V096'    ,cntent    )
          error = .true.
          exit
       endif
       !
       ! Test number of parameters read in combination with defined
       !
       if (tprofu(n) == '3d-profile') then
          if (nparrd /= 1+2*kmax) then
             call prterr(lundia    ,'V097'    ,' '       )
             error = .true.
             exit
          endif
       elseif (nparrd /= 3) then
          call prterr(lundia    ,'V097'    ,' '       )
          error = .true.
          exit
       else
       endif
       !
       ! Test name of constituent conform read parameter name
       !
       do np = 2, nparrd
          if (parrd(np)(:20) /= namtyp(ntyp)) then
             call prterr(lundia    ,'V096'    ,parrd(np) )
             error = .true.
             goto 9999
          endif
       enddo
       !
       ! Determine the record length for direct access file and the
       ! corresponding format
       !
       if (nn == 1) then
          mxlrec = max(83, 16 + kmax*2*14 + 1)
          if (mxlrec>83) then
             do nb = 1, mxfmtt
                ix = index(fmtbct(nb), 't83')
                if (ix /= 0) then
                   write (fmtbct(nb)(ix + 1:ix + 4), '(i4.4)') mxlrec
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
          open (newunit=lunout, file = filout(:lflout), form = 'formatted',             &
               & access = 'direct', status = 'unknown', recl = mxlrec)
          irec = 1
          write (lunout, fmtbct(1), rec = irec) '#', mxlrec, eol
       endif
       !
       ! Define table name
       !
       tablnm = 'T-serie BCT ' // nambnd(n) // ' for run: '
       !
       ! Write first 7 description records to file
       !
       write (lunout, fmtbct(2), rec = irec + 1) &
           & keywrd(1), quote, tablnm, runid, quote, eol
       write (lunout, fmtbct(3), rec = irec + 2) &
           & keywrd(2), quote, tprofu(n), quote, cntain, eol
       write (lunout, fmtbct(4), rec = irec + 3) &
           & keywrd(3), quote, nambnd(n), quote, eol
       write (lunout, fmtbct(7), rec = irec + 4) &
           & keywrd(8), quote, 'non-equidistant', quote, eol
       write (lunout, fmtbct(8), rec = irec + 5) keywrd(9), itdate, eol
       write (lunout, fmtbct(9), rec = irec + 6) &
           & keywrd(10), quote, 'minutes', quote, eol
       write (lunout, fmtbct(10), rec = irec + 7) &
           & keywrd(12), quote, 'linear', quote, eol
       !
       ! Write parameter name for time to file
       !
       npara = 1
       write (lunout, fmtbct(11), rec = irec + 7 + npara) &
           & keywrd(14), quote, parnam(1), quote, keywrd(15)(:10), quote, parunt(1),  &
           & quote, eol
       !
       ! Define NAMTYP index dependent on value of TYPBND(NTOF+N)
       !
       parnam(2)(:20) = namtyp(ntyp)
       parunt(2)      = unttyp(ntyp)
       !
       ! Write parameter name for TYPBND for end A and end B
       ! For 3d-profile 2 * KMAX records
       !
       if (tprofu(n)(:10) == '3d-profile') then
          parnam(2)(21:) = ' end A layer    '
          do k = 1, kmax
             npara = npara + 1
             write (parnam(2)(34:36), '(i3)') k
             write (lunout, fmtbct(11), rec = irec + 7 + npara) &
                 & keywrd(14), quote, parnam(2) , quote, keywrd(15)(:10), quote, parunt(2)  &
                 & , quote, eol
          enddo
          parnam(2)(21:) = ' end B layer    '
          do k = 1, kmax
             npara = npara + 1
             write (parnam(2)(34:36), '(i3)') k
             write (lunout, fmtbct(11), rec = irec + 7 + npara) &
                 & keywrd(14), quote, parnam(2) , quote, keywrd(15)(:10), quote, parunt(2)  &
                 & , quote, eol
          enddo
       else
          npara = npara + 1
          parnam(2)(21:) = ' end A          '
          write (lunout, fmtbct(11), rec = irec + 7 + npara) &
              & keywrd(14), quote, parnam(2), quote, keywrd(15)(:10), quote, parunt(2),  &
              & quote, eol
          npara = npara + 1
          parnam(2)(21:) = ' end B          '
          write (lunout, fmtbct(11), rec = irec + 7 + npara) &
              & keywrd(14), quote, parnam(2), quote, keywrd(15)(:10), quote, parunt(2),  &
              & quote, eol
       endif
       !
       ! Write number of time dependent data to file
       !
       write (lunout, fmtbct(12), rec = irec + 8 + npara) &
           & keywrd(16), ntimrd, eol
       !
       ! Re-define IREC 8+NPARA description records forward
       !
       irec = irec + 8 + npara
       !
       ! Read time dependent data for open boundary N (NTOF+NN)
       ! for all times NTIMRD on file
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
          if (ier <= 0) then
             error = .true.
             call prterr(lundia    ,'G007'    ,filbct    )
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
          call chckit(lundia    ,error     ,filbct    ,timrd     ,sdt       , &
                    & ittdep    ,itold     ,itstrt    ,nr        ,gdp       )
          if (error) goto 9999
          !
          ! Read time dependent data depending on value of TPROFU
          !
          if (tprofu(n)(:2) == '3d') then
             if (tprofu(n) /= tprofu(max(ntof+1,n-1))) then
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
                       & rwbval(1, 1)         ,rdfaul    ,ier       )
             !
             ! Premature EOR (IER = 0) or empty value (IER = -1)
             !
             if (ier <= 0) then
                error = .true.
                call prterr(lundia    ,'G007'    ,filbct    )
                write (lundia, '(a,a)') 'RECORD: ', record(:72)
                goto 9999
             endif
             !
             ! KMAX values for 3d-profile open boundary point B
             !
             ibeg = iend + 1
             call readnr(record    ,lrecrd    ,ibeg      ,iend      ,kmax      , &
                       & rwbval(1, 2)         ,rdfaul    ,ier       )
             !
             ! Premature EOR (IER = 0) or empty value (IER = -1)
             !
             if (ier <= 0) then
                error = .true.
                call prterr(lundia    ,'G007'    ,filbct    )
                write (lundia, '(a,a)') 'RECORD: ', record(:72)
                goto 9999
             endif
          else
             !
             ! 1 values for open boundary point A
             !
             ibeg = iend + 1
             call read1r(record    ,lrecrd    ,ibeg      ,iend      ,rwbval(1, 1)         , &
                       & rdfaul    ,ier       )
             !
             ! Premature EOR (IER = 0) or empty value (IER = -1)
             !
             if (ier <= 0) then
                error = .true.
                call prterr(lundia    ,'G007'    ,filbct    )
                write (lundia, '(a,a)') 'RECORD: ', record(:72)
                goto 9999
             endif
             !
             ! 1 values for open boundary point B
             !
             ibeg = iend + 1
             call read1r(record    ,lrecrd    ,ibeg      ,iend      ,rwbval(1, 2)         , &
                       & rdfaul    ,ier       )
             !
             ! Premature EOR (IER = 0) or empty value (IER = -1)
             !
             if (ier <= 0) then
                error = .true.
                call prterr(lundia    ,'G007'    ,filbct    )
                write (lundia, '(a,a)') 'RECORD: ', record(:72)
                goto 9999
             endif
          endif
          ! 
          ! If an NaN is read -> error
          !
          kend = 1
          if (tprofu(n)(:10) == '3d-profile') kend = kmax
          do k = 1, kend
             do i = 1, 2
                if ( isnan(rwbval(k,i)) ) then 
                    write(errmsg,'(a,a)') 'NaN in ',filbct(1:lfbct)
                    call prterr(lundia    ,'P004'    ,errmsg      )
                    error = .true.
                    goto 9999
                endif
             enddo
          enddo
          !
          ! Write time dependent data depending on value of TPROFU
          !
          if (tprofu(n)(:10) == '3d-profile') then
             write (fmtbct(13)(8:10), '(i3)') kmax*2
             write (lunout, fmtbct(13), rec = irec + nr) &
                 & timrd, (rwbval(k, 1), k = 1, kmax), (rwbval(k, 2), k = 1, kmax), eol
          else
             fmtbct(13)(8:10) = '  2'
             write (lunout, fmtbct(13), rec = irec + nr) &
                 & timrd, rwbval(1, 1), rwbval(1, 2), eol
          endif
          !
          ! Re-define ITOLD and
          ! As long as NR < NTIMRD read new RECORD
          !
          itold = ittdep
          if (nr <= ntimrd-1) read (lunrd, '(a)') record(:lrecrd)
       enddo
       !
       ! Re-define IREC NTIMRD records forward
       !
       irec = irec + ntimrd
       !
       ! Define maximum time
       !
       if (itold /= -1) then
            if (itold < itfinish) then
                write(errmsg,'(a,a,a,a,a)') 'Last time in file ', trim(filbct), ' for section #',trim(nambnd(n)),'# <' 
                call prterr(lundia    ,'U042'    ,errmsg)
                error = .true.
                goto 9999
            endif
       endif
    enddo
    !
    ! Stop reading file
    !
 9999 continue
end subroutine rdtdtn
