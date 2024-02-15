subroutine dimbub(error, gdp)
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
!    Function: Reads the dimension for discharge definition,
!              sources and sinks, from the MD-file
! Method used:
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
    real(fp)         , dimension(:)   , pointer :: cpdis
    real(fp)         , dimension(:)   , pointer :: hsink
    real(fp)         , dimension(:)   , pointer :: hsour
    real(fp)         , dimension(:)   , pointer :: xlbub
    real(fp)         , dimension(:)   , pointer :: zvelo
    logical          , dimension(:)   , pointer :: flbub
    type (gd_bubble)                  , pointer :: gdbubble
    integer                           , pointer :: kmax
    integer                           , pointer :: lsts
    integer                           , pointer :: lstsc
    integer                           , pointer :: lstsci
    integer                           , pointer :: nbub
    integer                           , pointer :: nxbub
    integer                           , pointer :: nsrc
    integer                           , pointer :: nsrcd
    integer                           , pointer :: lundia
!
! Global variables
!
    logical , intent(out) :: error  !!  Flag=TRUE if an error is encountered
!
! Local variables
!
    integer                       :: ibeg   ! Begin position in the RECORD from where the search for data/record is started 
    integer                       :: idef   ! Help var. containing default value(s) for integer variable 
    integer                       :: iend   ! Last position in the RECORD when the searched data/record is finished 
    integer                       :: ier    ! =  0 -> end of record encountered =  1 -> real value found = -1 -> length or number of data is larger then specified by the calling routine 
    integer                       :: iocond ! Reading condition, should be 0 
    integer                       :: istat
    integer        , dimension(4) :: ival   ! Help array (integer) where the data, recently read from the MD-file, are stored temporarily 
    integer                       :: lenc   ! Number of char. to read in string 
    integer                       :: lfile  ! Number of non blank characters of file name 
    integer                       :: lkw    ! Length of keyword (:= 6) 
    integer                       :: luntmp ! Unit number of FILTMP 
    integer, external             :: newlun
    integer                       :: mcount ! Nr. of bubble screens in M-direction
    integer                       :: ncount ! Nr. of bubble screens in N-direction
    integer                       :: nlook  ! Nr. of values to look for in a record 
    integer                       :: ntrec  ! Current record counter. It's value is changed to detect if all records in the MD-file have been read 
    logical                       :: found  ! Flag is true if KEYWRD is found 
    logical                       :: newkw  ! Flag to specify if the keyword to look for is a new keyword 
    character(11)                 :: fmttmp ! Format of FILTMP (UN/FRee formatted) 
    character(12)                 :: fildef ! Default file name = blank 
    character(20)                 :: cdef   ! Default value for chulp 
    character(20)                 :: chulp  ! Help variable to read character from MD-file 
    character(132)                :: rec132 ! Standard rec. length in an attribute file (132) 
    character(256)                :: filtmp ! Attribute file name 
    character(300)                :: mdfrec ! Record read from the MD-file 300 = 256 + a bit (field, =, ##, etc.) 
    character(6)                  :: keyw   ! Keyword to look for in the MD-file 
    character(40)                 :: errmsg ! Text string error messages 
!
!! executable statements -------------------------------------------------------
!
    cpdis     => gdp%gdbubble%cpdis
    hsink     => gdp%gdbubble%hsink
    hsour     => gdp%gdbubble%hsour
    xlbub     => gdp%gdbubble%xlbub
    zvelo     => gdp%gdbubble%zvelo
    flbub     => gdp%gdbubble%flbub
    gdbubble  => gdp%gdbubble
    kmax      => gdp%d%kmax
    lsts      => gdp%d%lsts
    lstsc     => gdp%d%lstsc
    lstsci    => gdp%d%lstsci
    nbub      => gdp%d%nbub
    nxbub     => gdp%d%nxbub
    nsrc      => gdp%d%nsrc
    nsrcd     => gdp%d%nsrcd
    lundia    => gdp%gdinout%lundia
    !
    ! initialize local parameters
    !
    mdfrec = ' '
    fmttmp = ' '
    fildef = ' '
    filtmp = fildef
    lfile  = 12
    lkw    = 6
    nlook  = 1
    nbub   = 0
    nxbub  = 0
    cdef   = ' '
    chulp  = cdef
    newkw  = .true.
    found  = .true.
    errmsg = 'one of the bubble screens'
    !
    ! locate 'Filbub' record in case discharges definitions (source and sink) are written in an attribute file
    ! for old files 'Filbub' is not found, which leads to fifsrd = ' '
    !
    filtmp = fildef
    call prop_get_string(gdp%mdfile_ptr, '*', 'Filbub', filtmp)
    if (filtmp /= fildef) then
       !
       ! test file existence
       !
       if (exifil(filtmp, lundia)) then
          !
          ! open input file
          !
          open (newunit=luntmp, file = trim(filtmp), status = 'old')
          if (fmttmp(1:2) == 'un') then
             !
             ! unformatted file
             ! read record and add 1 to NXBUB till end of file
             !
             ! -->
  110        continue
             read (luntmp, iostat = iocond) rec132
             !
             ! error while reading IOCOND>0, End-Of-File IOCOND<0
             !
             if (iocond /= 0) then
                if (iocond < 0) goto 310
                call prterr(lundia, 'G007', filtmp(:lfile))
                error = .true.
                goto 999
             endif
             !
             ! discharge definition found (non-blanks in record part)
             !
             chulp = rec132(:20)
             if (chulp /= cdef) then
                nxbub = nxbub + 1
                lenc  = 1
                ibeg  = 21
                nlook = 4
                call readni(rec132    ,132       ,ibeg      ,iend      ,nlook     , &
                          & ival      ,idef      ,ier       )
                if (ier <= 0) then
                   error = .true.
                   call prterr(lundia    ,'G007'    ,filtmp(1:lfile)      )
                   goto 310
                endif
                mcount = abs(ival(3) - ival(1)) + 1
                ncount = abs(ival(4) - ival(2)) + 1
                if (mcount>1 .and. ncount>1 .and. mcount/=ncount) then
                   error = .true.
                   goto 999
                endif 
                nbub = nbub + max(mcount,ncount)*kmax
                goto 110
             ! <--
             endif
          else
             !
             ! freeformatted file
             !
             !
             ! skip lines starting with a '*'
             !
             call skipstarlines(luntmp)
             !
             ! read record and add 1 to NXBUB till end of file
             !
             ! -->
  210        continue
             read (luntmp, '(a)', iostat = iocond) rec132
             !
             ! error while reading IOCOND>0, End-Of-File IOCOND<0
             !
             if (iocond /= 0) then
                if (iocond < 0) goto 310
                call prterr(lundia, 'G007', filtmp(:lfile))
                error = .true.
                goto 999
             endif
             !
             ! discharge definition found (non-blanks in record part)
             !
             chulp = rec132(:20)
             if (chulp /= cdef) then
                nxbub = nxbub + 1
                lenc  = 1
                ibeg  = 21
                nlook = 4
                call readni(rec132    ,132       ,ibeg      ,iend      ,nlook     , &
                          & ival      ,idef      ,ier       )
                if (ier <= 0) then
                   error = .true.
                   call prterr(lundia, 'G007', filtmp(1:lfile))
                   goto 310
                endif
                mcount = abs(ival(3) - ival(1)) + 1
                ncount = abs(ival(4) - ival(2)) + 1
                if (mcount>1 .and. ncount>1 .and. mcount/=ncount) then
                   error = .true.
                   call prterr(lundia , 'V231', errmsg(:32))
                   goto 999
                endif 
                nbub = nbub + max(mcount,ncount)*kmax
                goto 210
             ! <--
             endif
          endif
          !
          ! close file
          !
  310     continue
          close (luntmp)
       else
          !
          ! file does not exist !!
          !
          error = .true.
       endif
       !
       ! calculate NSRC (= NSRCD + NBUB)
       ! this line is also in dimrd (needed here for allocation)
       !
       nsrc = nsrcd + nbub - nxbub
       !
       ! deallocate dynamic memory
       ! This code is visited during tdatom phase AND during trisim phase
       !
       if (associated(gdp%gdbubble%cpdis)) deallocate(gdp%gdbubble%cpdis, stat=istat)
       if (associated(gdp%gdbubble%hsink)) deallocate(gdp%gdbubble%hsink, stat=istat)
       if (associated(gdp%gdbubble%hsour)) deallocate(gdp%gdbubble%hsour, stat=istat)
       if (associated(gdp%gdbubble%xlbub)) deallocate(gdp%gdbubble%xlbub, stat=istat)
       if (associated(gdp%gdbubble%zbubl)) deallocate(gdp%gdbubble%zbubl, stat=istat)
       if (associated(gdp%gdbubble%zvelo)) deallocate(gdp%gdbubble%zvelo, stat=istat)
       if (associated(gdp%gdbubble%flbub)) deallocate(gdp%gdbubble%flbub, stat=istat)
       !
       ! allocate dynamic memory
       ! Allocate using the gdp structure itself instead of the local pointers
       !
                     allocate (gdp%gdbubble%cpdis(nsrcd) , stat=istat)
       if (istat==0) allocate (gdp%gdbubble%hsink(lstsci), stat=istat)
       if (istat==0) allocate (gdp%gdbubble%hsour(lstsci), stat=istat)
       if (istat==0) allocate (gdp%gdbubble%xlbub(nsrcd) , stat=istat)
       if (istat==0) allocate (gdp%gdbubble%zbubl(nsrc)  , stat=istat)
       if (istat==0) allocate (gdp%gdbubble%zvelo(kmax)  , stat=istat)
       if (istat==0) allocate (gdp%gdbubble%flbub(nsrcd) , stat=istat)
       if (istat/=0) then
          call prterr(lundia, 'U021', 'Dimbub: memory alloc error')
          call d3stop(1, gdp)
       endif
    endif
  999 continue
end subroutine dimbub
