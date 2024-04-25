subroutine bndfil(lundia    ,error     ,kmax      ,lnto      ,lntof     , &
                & lntoq     ,mxdnto    ,mxnto     ,filbnd    ,fmttmp    , &
                & profil    ,nambnd    ,typbnd    ,datbnd    ,mnbnd     , &
                & alpha     ,tprofu    ,statns    ,gdp       )
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
!    Function: Reads the following boundary definitions from the
!              attribute file: FILBND, FMTBND, TYPBND, DATBND,
!                              MNBND , ALPHA , TPROFU and STATNS
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
!
! Global variables
!
    integer                            , intent(in)  :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                          :: lnto   !!  Local number of boundary openings read in attribute file
                                                               !!  Test with input NTO
    integer                                          :: lntof  !!  Local number of QH openings read in attribute file
                                                               !!  Test with input NTO
                                                               !!  Local number of harmonic boundary openings read in attribute file
                                                               !!  Test with input NTO
    integer                                          :: lntoq
    integer                                          :: lundia !  Description and declaration in inout.igs
    integer                            , intent(in)  :: mxdnto !!  Array dimension for opening boundary
                                                               !!  definition arrays, which are dynamic
                                                               !!  declared (NTO in call from READMD
                                                               !!  and MXNTO in call from TDATOM)
    integer                            , intent(in)  :: mxnto  !!  Max. number of open boundaries
    integer     , dimension(7, mxdnto)               :: mnbnd  !  Description and declaration in esm_alloc_int.f90
    logical                            , intent(out) :: error  !!  Flag=TRUE if an error is encountered
    real(fp)    , dimension(mxdnto)                  :: alpha  !  Description and declaration in esm_alloc_real.f90
    character(*)                                     :: filbnd !!  File name for the boundary definition file
    character(1), dimension(mxdnto)                  :: typbnd !  Description and declaration in esm_alloc_char.f90
    character(1), dimension(mxnto)                   :: datbnd !!  Type of open boundary:
                                                               !!     -'H'(armonic/Tide)
                                                               !!     -'A'(stronomic/Tide)
                                                               !!     -'Q'(QH-relation)
                                                               !!     -'T'(ime series/time dependent)
    character(11)                      , intent(in)  :: fmttmp !!  Help variable for file format
    character(12), dimension(mxnto, 2)               :: statns !!  References to tidal stations at boundary support points
    character(20), dimension(mxdnto)                 :: nambnd !  Description and declaration in esm_alloc_char.f90
    character(20), dimension(mxdnto)                 :: tprofu !  Description and declaration in esm_alloc_char.f90
    character(60)                      , intent(in)  :: profil !!  Total string of possible profiles
!
! Local variables
!
    integer           :: ibeg   ! Begin position in the RECORD from where the search for data/record is started 
    integer           :: idef   ! Help var. containing default va- lue(s) for integer variable 
    integer           :: iend   ! Last position in the RECORD when the searched data/record is finished 
    integer           :: ier    ! =  0 -> end of record encountered 
                                ! =  1 -> real value found 
                                ! = -1 -> length or number of data is larger then specified by the calling routine 
    integer           :: inprof ! Index number of first character in PROFIL string of TPROFC definition 
    integer           :: iocond ! IO status for reading 
    integer           :: lenc   ! Help var. (length of var. cvar to be looked for in the MD-file) 
    integer           :: lfile  ! Help var. specifying the length of character variables for file names 
    integer           :: lquote ! Index in REC132 of a quote 
    integer           :: lr132  ! Standard length of a record in the attribute file = 132 
    integer           :: luntmp ! Help var. for a unit number of an attribute file 
    integer           :: n      ! Help var. 
    integer, external :: newlun
    logical           :: ltest  ! Flag for testing 
    real(fp)          :: rdef   ! Help var. containing default va- lue(s) for real variable 
    character(1)      :: cdefd  ! Default value when DATBND not found 
    character(1)      :: cdeft  ! Default value when TYPBND not found 
    character(12)     :: cdefl
    character(132)    :: rec132 ! Standard rec. length in an attribute file (132) 
    character(132)    :: recold
    character(20)     :: cdefn  ! Default value when NAMBND not found 
    character(20)     :: cdefp  ! Default value when PROFU not found 
    character(20)     :: chulp  ! Help var. 
!
!! executable statements -------------------------------------------------------
!
    !
    ! initialize local parameters
    !
    lnto   = 0
    lntof  = 0
    lntoq  = 0
    !
    idef   = 1
    rdef   = 0.0
    cdefn  = 'Boundary Opening    '
    cdefd  = 'H'
    cdeft  = 'Z'
    cdefp  = 'uniform             '
    cdefl  = '            '
    lr132  = 132
    ltest  = .true.
    recold = 'no records read yet'
    !
    ! test if file exists; and if so, read.
    !
    call remove_leading_spaces(filbnd    ,lfile     )
    !
    if (exifil(filbnd, lundia)) then
       !
       open (newunit=luntmp, file = filbnd(1:lfile), form = fmttmp, status = 'old')
       !
       ! unformatted file
       ! read per NTO: NAMBND, TYPBND, DATBND, MNBND(6) and ALPHA
       ! NOTE: starting with version 2.40 the unformatted file is
       !       obsolete hence the reading of tprofu and statns can not
       !       be together with an unformatted file.
       !
       if (fmttmp(1:2) == 'un') then
          !
          ! WARNING: mnbnd(5,n) and mnbnd(6,n) are NOT read for unformatted files
          !
          do n = 1, mxnto
             read (luntmp, iostat = iocond) &
                & nambnd(n), typbnd(n), datbnd(n), mnbnd(1, n), mnbnd(2, n), mnbnd(3, n), mnbnd(4, n), alpha(n)
             if (iocond /= 0) then
                if (iocond > 0) then
                   error = .true.
                   call prterr(lundia, 'G007', filbnd(1:lfile))
                   write (lundia,'(a,i3)') 'Last record read for open boundary nr :', n - 1
                endif
                goto 200
             endif
             !
             ! define semi global number of boundary opening definitions
             !
             lnto = lnto + 1
             if (datbnd(n)=='Q' .or. datbnd(n)=='q') then
                lntoq = lntoq + 1
             elseif (datbnd(n)/='T' .and. datbnd(n)/='t') then
                lntof = lntof + 1
             else
             endif
             tprofu(n)    = cdefp
             statns(n, 1) = cdefl
             statns(n, 2) = cdefl
          enddo
          !
          ! test if there are more than MXNTO opening definitions
          ! this will never appear while MXDNTO = NTO
          !
          read (luntmp, iostat = iocond)
          if (iocond == 0) then
             error = .true.
             call prterr(lundia, 'U122', filbnd(1:lfile))
          endif
       else
          !
          ! freeformatted file, skip lines starting with a '*'
          !
          call skipstarlines(luntmp)
          !
          ! freeformatted file (free including quotes to seperate
          ! character variables .or. free as Deepak defined it)
          ! use first non-comment line to find out
          !
          read (luntmp, '(a)', iostat = iocond) rec132
          rewind(luntmp)
          call skipstarlines(luntmp)
          lquote = index(rec132, '''')
          if (lquote == 0) then
             !
             ! read per NTO a REC132 containing:
             ! NAMBND, TYPBND, DATBND, MNBND(4), ALPHA, TPROFU & STATNS
             !
             do n = 1, mxnto
                read (luntmp, '(a)', iostat = iocond) rec132
                if (iocond /= 0) then
                   if (iocond > 0) then
                      call prterr(lundia, 'G007', filbnd(1:lfile))
                      write (lundia, '(a)') 'Last correct record:'
                      write (lundia, '(2a)') recold(:72), '...'
                      error = .true.
                   endif
                   goto 200
                endif
                !
                ! read NAMBND from record, default value allowed
                !
                nambnd(n) = rec132(:20)
                if (nambnd(n) == ' ') then
                   write (cdefn(18:20), '(i3)') n
                   call prterr(lundia, 'U150', filbnd(1:lfile))
                   nambnd(n) = cdefn
                endif
                !
                ! read TYPBND from record, default value not allowed
                !
                lenc = 1
                ibeg = 21
                call read1c(rec132    ,lr132     ,ibeg      ,iend      ,chulp     , &
                          & lenc      ,ier       )
                if (ier <= 0) then
                   call prterr(lundia, 'G007', filbnd(1:lfile))
                   write (lundia, '(a)') 'Last correct record:'
                   write (lundia, '(2a)') recold(:72), '...'
                   typbnd(n) = cdeft
                   error = .true.
                   goto 200
                else
                   typbnd(n) = chulp(:1)
                endif
                !
                ! read DATBND from record, empty or long string not allowed
                !
                ibeg = iend + 1
                call read1c(rec132    ,lr132     ,ibeg      ,iend      ,chulp     , &
                          & lenc      ,ier       )
                if (ier <= 0) then
                   call prterr(lundia, 'G007', filbnd(1:lfile))
                   write (lundia, '(a)') 'Last correct record:'
                   write (lundia, '(2a)') recold(:72), '...'
                   datbnd(n) = cdefd
                   error = .true.
                   goto 200
                else
                   datbnd(n) = chulp(:1)
                endif
                !
                ! read MNBND(4) from record, default value not allowed
                !
                ibeg = iend + 1
                call readni(rec132    ,lr132     ,ibeg      ,iend      ,4         , &
                          & mnbnd(1, n)          ,idef      ,ier       )
                if (ier <= 0) then
                   call prterr(lundia, 'G007', filbnd(1:lfile))
                   write (lundia, '(a)') 'Last correct record:'
                   write (lundia, '(2a)') recold(:72), '...'
                   mnbnd(1, n) = idef
                   mnbnd(2, n) = idef
                   mnbnd(3, n) = idef
                   mnbnd(4, n) = idef
                   error = .true.
                   goto 200
                endif
                !
                ! read ALPHA from record, default value not allowed
                !
                ibeg = iend + 1
                call read1r(rec132    ,lr132     ,ibeg      ,iend      ,alpha(n)  , &
                          & rdef      ,ier       )
                if (ier<=0) then
                   call prterr(lundia, 'G007', filbnd(1:lfile))
                   write (lundia, '(a)') 'Last correct record:'
                   write (lundia, '(2a)') recold(:72), '...'
                   alpha(n) = rdef
                   error = .true.
                   goto 200
                endif
                !
                ! read profile type from record, only if TYPBND = C/Q/R/T and kmax > 1
                ! default value allowed => default
                !
                tprofu(n) = cdefp
                ltest = ((kmax>1) .and. (typbnd(n)=='C' .or. typbnd(n)=='Q' &
                         & .or. typbnd(n)=='R' .or. typbnd(n)=='T'))
                if (ltest) then
                   !
                   ! read TPROFU from record, empty or long string "allowed" (old version)
                   !
                   lenc = 20
                   ibeg = iend + 1
                   call read1c(rec132    ,lr132     ,ibeg      ,iend      ,chulp     , &
                             & lenc      ,ier       )
                   if (ier <= 0) then
                      call prterr(lundia, 'G007', filbnd(1:lfile))
                      write (lundia, '(a)') 'Last correct record:'
                      write (lundia, '(2a)') recold(:72), '...'
                      tprofu(n) = cdefp
                      error = .true.
                   else
                      tprofu(n) = chulp
                   endif
                endif
                call small(tprofu(n), lenc)
                !
                ! Check for undefined profile definition
                !
                inprof = index(profil, tprofu(n))
                if (inprof == 0) then
                   call prterr(lundia, 'U066', chulp)
                   write (lundia, '(a)') 'Last correct record:'
                   write (lundia, '(2a)') recold(:72), '...'
                   tprofu(n) = cdefp
                   error = .true.
                endif
                !
                ! TPROFU may be "3d-profile" only for Time series (DATBND(N) = 'T')
                !
                if (datbnd(n)/='T' .and. tprofu(n)(:10)=='3d-profile') then
                   call prterr(lundia, 'U021', '<3D-profile> not allowed for H/A/Q open boundary definitions')
                   write (lundia, '(a)') 'Last correct record:'
                   write (lundia, '(2a)') recold(:72), '...'
                   tprofu(n) = cdefp
                   error = .true.
                endif
                !
                ! Two stations specified after velocity profile for
                ! DATBND(N) is "astronomical" (=> new version)
                !
                if (datbnd(n) == 'A') then
                   lenc = 12
                   ibeg = iend + 1
                   call read1c(rec132    ,lr132     ,ibeg      ,iend      ,chulp     , &
                             & lenc      ,ier       )
                   if (ier <= 0) then
                      statns(n, 1) = cdefl
                      call prterr(lundia, 'G007', filbnd(1:lfile))
                      write (lundia, '(a)') 'Last correct record:'
                      write (lundia, '(2a)') recold(:72), '...'
                      error = .true.
                      goto 200
                   else
                      statns(n, 1) = chulp(:12)
                   endif
                   !
                   ! Test if value of STATNS(N,1) not "uniform" or
                   ! "logarithmic" (values for TPROFU) if so read again
                   !
                   chulp = statns(n, 1)
                   call small(chulp, lenc)
                   !
                   ! Check for undefined profile definition
                   !
                   inprof = index(profil, chulp(:12))
                   if (inprof /= 0) then
                      ibeg = iend + 1
                      call read1c(rec132    ,lr132     ,ibeg      ,iend      ,chulp     , &
                                & lenc      ,ier       )
                      if (ier <= 0) then
                         statns(n, 1) = cdefl
                         call prterr(lundia, 'G007', filbnd(1:lfile))
                         write (lundia, '(a)') 'Last correct record:'
                         write (lundia, '(2a)') recold(:72), '...'
                         error = .true.
                         goto 200
                      else
                         statns(n, 1) = chulp(:12)
                      endif
                   endif
                   !
                   ! Second station
                   !
                   ibeg = iend + 1
                   call read1c(rec132    ,lr132     ,ibeg      ,iend      ,chulp     , &
                             & lenc      ,ier       )
                   if (ier <= 0) then
                      statns(n, 2) = cdefl
                      call prterr(lundia, 'G007', filbnd(1:lfile))
                      write (lundia, '(a)') 'Last correct record:'
                      write (lundia, '(2a)') recold(:72), '...'
                      error = .true.
                      goto 200
                   else
                      statns(n, 2) = chulp(:12)
                   endif
                else
                   statns(n, 1) = cdefl
                   statns(n, 2) = cdefl
                   !
                   ! read MNBND(5,n) and MNBND(6,n) from record, default values allowed
                   !
                   idef = 0
                   ibeg = iend + 1
                   call readni(rec132      , lr132, ibeg, iend, 2, &
                             & mnbnd(5:6,n), idef , ier)
                endif
                !
                ! define semi global number of boundary opening definitions
                !
                lnto = lnto + 1
                if (datbnd(n)=='Q' .or. datbnd(n)=='q') then
                   lntoq = lntoq + 1
                elseif (datbnd(n)/='T' .and. datbnd(n)/='t') then
                   lntof = lntof + 1
                else
                endif
                !
                ! Copy last record read correctly to RECOLD definitions
                !
                recold = rec132
             enddo
          else
             !
             ! freeformatted file including quotes to seperate character
             ! variables, not (yet) implemented
             !
             call prterr(lundia, 'U161', filbnd(1:lfile))
             error = .true.
          endif
          !
          ! test if there are more then MXNTO opening definitions
          ! this will never appear while MXDNTO = NTO
          !
          read (luntmp, *, iostat = iocond)
          if (iocond == 0) then
             error = .true.
             call prterr(lundia, 'U122', filbnd(1:lfile))
          endif
          !
          ! stop reading file
          !
       endif
       !
       ! close file
       !
  200  continue
       close (luntmp)
    else
       !
       ! file does not exist
       !
       error = .true.
    endif
end subroutine bndfil
