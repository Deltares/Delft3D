subroutine rdtddn(lundia    ,lunout    ,lunrd     ,error     ,filout    , &
                & fildis    ,runid     ,eol       ,itstrt    ,itfinish  , &
                & nsrc      ,lstsc     ,rval      ,maxval    ,namsrc    , &
                & disint    ,parnam    ,parunt    ,bubble    ,gdp       )
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
!    Function: This general purpose routine reads the time depen-
!              dent data from file (new)
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
    integer                    , pointer :: itdate
    real(fp)                   , pointer :: dt
    real(fp)                   , pointer :: tunit
    character*20, dimension(:) , pointer :: keywrd
    character*39, dimension(:) , pointer :: fmtdis
!
! Global variables
!
    integer                            , intent(in)  :: itfinish !  Description and declaration in inttim.igs
    integer                                          :: itstrt   !  Description and declaration in inttim.igs
    integer                            , intent(in)  :: lstsc    !  Description and declaration in dimens.igs
    integer                                          :: lundia   !  Description and declaration in inout.igs
    integer                                          :: lunout   !  Unit number for the transformed ouput file
    integer                                          :: lunrd    !  Unit number of the attribute file containing the time series
    integer                            , intent(in)  :: maxval   !  Maximum number of values 1+LSTSC+2
    integer                            , intent(in)  :: nsrc     !  Description and declaration in esm_alloc_int.f90
    logical                            , intent(in)  :: bubble   !  Description and declaration in procs.igs
    logical                                          :: error    !  Flag=TRUE if an error is encountered
    real(fp)     , dimension(maxval)                 :: rval     !  Array for the time dependent data
    character(*)                                     :: fildis   !  Name of time dependent input file
    character(*)                                     :: filout   !  Name of the output file to be opened
    character(*)                       , intent(in)  :: runid
    character(1)                       , intent(in)  :: eol      !  ASCII code for End-Of-Line (^J)
    character(1) , dimension(nsrc)                   :: disint   !  Description and declaration in esm_alloc_char.f90
    character(10), dimension(lstsc + 4), intent(in)  :: parunt   !  Unit name fitting the parameter
    character(20), dimension(nsrc)                   :: namsrc   !  Description and declaration in esm_alloc_char.f90
    character(36), dimension(lstsc + 4), intent(in)  :: parnam   !  Names of theparamaters to write to time dependent file DIS
!
! Local variables
!
    integer                      :: ibeg    ! Begin position in the RECORD from where the search for data/record is started 
    integer                      :: iend    ! Last position in the RECORD when the searched data/record is finished 
    integer                      :: ier     ! =  0 -> end of record encountered =  1 -> real value found = -1 -> length or number of data is larger then specified by the calling routine 
    integer                      :: iocond  ! IO status for reading 
    integer                      :: irec
    integer                      :: irecrd  ! Counter of records if input file is a direct access file 
    integer                      :: itold   ! Help var. to store last read time to test accending order 
    integer                      :: ittdep  ! Help var. for the time read (now de- fined as multiples of DT, but in fu- ture it may take any value) 
    integer                      :: ix
    integer                      :: j
    integer                      :: l
    integer                      :: lflout  ! Actual length of FILOUT 
    integer                      :: lrecrd  ! Length of record read 
    integer                      :: mxlrec  ! Actual maximum record length 
    integer                      :: n       ! Help var. for the nr. of data to be read (see LENDAT) 
    integer                      :: nb
    integer                      :: newlun
    integer                      :: np
    integer                      :: npara   ! NR. of parameter records equal to number of data values in a record to be read 
    integer                      :: nparrd  ! NR. of parameter records actual read 
    integer                      :: nr
    integer                      :: nrval
    integer                      :: ntimrd
    logical                      :: access  ! Flag to read file as direct access or sequential 
    logical                      :: ex
    real(fp)                     :: dtmin   ! Time step dt in minutes
    real(fp)                     :: rdfaul
    real(fp)                     :: timrd   ! Time in minutes read 
    real(fp)                     :: timscl  ! Multiple factor to create minutes from read times 
    character(1)                 :: quote   ! Apostrophe ASCII-character 39 
    character(10)                :: disflg
    character(10)                :: interp  ! Character string containing inter- polation option 
    character((4+lstsc)*16)      :: record  ! Record for data to be read. Maximum length is 11 * 24 + 1 
    character(36), dimension(:), allocatable :: parrd   ! Parameter names read 
    character(40)                :: cntain
    character(42)                :: tablnm  ! Table name specification 
    character(300)               :: errmsg
!
!! executable statements -------------------------------------------------------
!
    fmtdis  => gdp%gdfmtdis%fmtdis
    keywrd  => gdp%gdkeywtd%keywrd
    itdate  => gdp%gdexttim%itdate
    dt      => gdp%gdexttim%dt
    tunit   => gdp%gdexttim%tunit
    !
    access = .false.
    irecrd = 0
    !
    quote  = char(39)
    !
    npara = 2 + lstsc + 2
    allocate(parrd(npara))
    !
    call remove_leading_spaces(filout    ,lflout    )
    !
    ! Determine the record length for direct access file and the
    ! corresponding format
    !
    mxlrec = max(125, 16 + (lstsc + 3)*14 + 1)
    if (mxlrec>125) then
       do nb = 1, mxfmtd
          ix = index(fmtdis(nb), 't125')
          if (ix/=0) write (fmtdis(nb)(ix + 1:ix + 4), '(i4)') mxlrec
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
    open (newunit=lunout, file = filout(:lflout), form = 'formatted', access = 'direct',&
        & status = 'unknown', recl = mxlrec)
    irec = 1
    write (lunout, fmtdis(1), rec = irec) '#', mxlrec, eol
    !
    ! Start reading the time dependent data.
    !
    do n = 1, nsrc
       ittdep = -1
       itold  = -1
       read (lunrd, '(a)', iostat = iocond) record
       !
       ! Premature EOR encountered or error (IOCOND <> 0)
       ! Test last time read
       ! Part of error message not fully correct
       !
       if (iocond/=0) then
          error = .true.
          call prterr(lundia    ,'G007'    ,fildis    )
          !
          if (iocond<0) then
             if (itold < itfinish) then
                write(errmsg,'(a,a,a)') 'Last time in file ', trim(fildis), ' <' 
                call prterr(lundia    ,'U042'    ,errmsg    )
             elseif (n<nsrc) then
                call prterr(lundia    ,'U021'    ,'not for all discharge locations DIS data defined'    )
             else
             endif
          endif
       endif
       !
       ! Check for EOL in file and find maximum number of characters
       ! to read if EOL is located. Only for UNIX systems where EOL = ^J
       !
       lrecrd = len(record)
       if (eol/=' ' .and. index(record, eol)/=0) lrecrd = index(record, eol)
       !
       ! Read all keywords prior to time dependent data records
       ! Define number for data values (parameter names) to be read
       ! Input NPARA = 2 + LSTSC + 2, output can be -2 depending on
       ! contents of DISFLG
       !
       npara = 2 + lstsc + 2
       call flhnew(lunrd     ,lundia    ,error     ,record(:lrecrd)      ,access    , &
                 & irecrd    ,namsrc(n) ,disflg    ,disint(n) ,itdate    , &
                 & timscl    ,ntimrd    ,parrd     ,npara     ,nparrd    , &
                 & bubble    ,gdp       )
       if (error) then
          exit
       endif
       !
       ! Define NPARA
       !
       npara = nparrd
       nrval = npara - 1
       !
       ! Test string DISFLG
       !
       if (disflg=='regular   ') then
          if (nparrd/=2 + lstsc) then
             call prterr(lundia    ,'V097'    ,' '       )
             error = .true.
             exit
          endif
          cntain = 'regular    # discharge without momentum '
       elseif (disflg=='momentum  ') then
          if (nparrd/=4 + lstsc) then
             call prterr(lundia    ,'V097'    ,' '       )
             error = .true.
             exit
          endif
          cntain = 'momentum   # discharge with momentum'
       elseif (disflg=='walking   ') then
          if (nparrd/=2 + lstsc) then
             call prterr(lundia    ,'V097'    ,' '       )
             error = .true.
             exit
          endif
          cntain = 'walking   # walking discharge      '
       elseif (disflg=='inoutlet  ') then
          if (nparrd/=2 + lstsc) then
             call prterr(lundia    ,'V097'    ,' '       )
             error = .true.
             exit
          endif
          cntain = 'inoutlet  # power station / culvert'
       elseif (disflg=='power     ') then
          if (nparrd/=2 + lstsc) then
             call prterr(lundia    ,'V097'    ,' '       )
             error = .true.
             exit
          endif
          cntain = 'power     # power station in/out   '
       elseif (disflg=='culvert   ') then
          if (nparrd/=2 + lstsc) then
             call prterr(lundia    ,'V097'    ,' '       )
             error = .true.
             exit
          endif
          cntain = 'culvert   # culvert in/out         '
       else
          call prterr(lundia    ,'V096'    ,disflg    )
          error = .true.
          exit
       endif
       !
       ! Test name of constituent conform read parameter name
       ! Only first 20 characters are of significance
       !
       do np = 2, nparrd
          if (parrd(np)(:20)/=parnam(np)(:20)) then
             call prterr(lundia    ,'V096'    ,parrd(np)(:20)       )
             !
             error = .true.
             goto 9999
          endif
       enddo
       !
       ! Define INTERP
       !
       interp = 'linear    '
       if (disint(n)=='N') interp = 'block    '
       !
       ! Define format for number of values NRVAL
       !
       write (fmtdis(13)(8:10), '(i3)') nrval
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
       write (lunout, fmtdis(8), rec = irec + 5) keywrd(9), itdate, eol
       write (lunout, fmtdis(9), rec = irec + 6) &
           & keywrd(10), quote, 'minutes', quote, eol
       write (lunout, fmtdis(10), rec = irec + 7) &
           & keywrd(12), quote, interp, quote, eol
       !
       ! Write NPARA parameter names
       !
       do l = 1, npara
          write (lunout, fmtdis(11), rec = irec + 7 + l) &
              & keywrd(14), quote, parnam(l), quote, keywrd(15)(:10), quote, parunt(l), quote, eol
       enddo
       !
       ! Write number of time dependent data to file
       !
       write (lunout, fmtdis(12), rec = irec + 8 + npara) &
           & keywrd(16), ntimrd, eol
       !
       ! Re-define IREC 8+NPARA description records forward
       !
       irec = irec + 8 + npara
       !
       ! Read time dependent data for discharge location (N)
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
          if (ier<=0) then
             error = .true.
             call prterr(lundia    ,'G007'    ,fildis    )
             write (lundia, '(a,a)') 'RECORD: ', record(:72)
             goto 9999
          endif
          !
          ! Re-defined time read (TIMRD ) in minutes
          !
          timrd = timrd*timscl
          dtmin = dt * tunit / 60.0_fp
          !
          ! Perform some checks on time value read
          !
          call chckit(lundia    ,error     ,fildis    ,timrd     ,dtmin     , &
                    & ittdep    ,itold     ,itstrt    ,nr        ,gdp       )
          if (error) goto 9999
          !
          ! Read discharge value, concentrations for LSTSC and optional
          ! magnitude and direction
          !
          ibeg = iend + 1
          call readnr(record    ,lrecrd    ,ibeg      ,iend      ,nrval     , &
                    & rval      ,rdfaul    ,ier       )
          !
          ! Premature EOR or empty value (IER = -1)
          !
          if (ier<=0) then
             error = .true.
             call prterr(lundia    ,'G007'    ,fildis    )
             write (lundia, '(a,a)') 'RECORD: ', record(:72)
             goto 9999
          endif
          !
          ! If an NaN is read -> error
          !
          do j = 2, nrval
              if ( isnan(rval(j)) ) then
                 write(errmsg,'(a,a)') 'NaN in ', trim(fildis)
                 call prterr(lundia    ,'P004'    ,errmsg      )
                 error = .true.
                 goto 9999
              endif
          enddo
          !
          ! Check for negative values, when not a power station/culvert
          !
          if (disflg/='inoutlet  ' .and. disflg/='power     ' .and.             &
             & disflg/='culvert   ') then
             do j = 2, lstsc + 1
                if (rval(j)<0.0) then
                   errmsg = 'Concentration at discharge ' // namsrc(n)
                   call prterr(lundia    ,'V061'    ,errmsg    )
                   error = .true.
                   goto 9999
                endif
             enddo
          endif
          !
          ! Check for negative magnitude
          !
          if (nrval>lstsc + 1) then
             if (rval(lstsc + 2)<0.0) then
                errmsg = 'Flow magnitude at discharge ' // namsrc(n)
                call prterr(lundia    ,'V061'    ,errmsg    )
                error = .true.
                goto 9999
             endif
          endif
          !
          ! Write NRVAL time dependent data
          !
          write (lunout, fmtdis(13), rec = irec + nr) &
              & timrd, (rval(l), l = 1, nrval), eol
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
             write(errmsg,'(a,a,a,a,a)') 'Last time in file ', trim(fildis), ' for discharge #',trim(namsrc(n)),'# <' 
             call prterr(lundia    ,'U042'    ,errmsg)
             error = .true.
             goto 9999
          endif
       endif
    enddo
    !
    ! stop reading file
    !
 9999 continue
    deallocate(parrd)
end subroutine rdtddn
