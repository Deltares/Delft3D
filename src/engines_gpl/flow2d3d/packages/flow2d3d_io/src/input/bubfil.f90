subroutine bubfil(lundia    ,filbub    ,error     ,mmax      ,nmax      , &
                & nmaxus    ,kmax      ,nsrc      ,nxbub     ,nbub      , &
                & mnksrc    ,namsrc    ,gdp       )
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
!    Function: Reads the bubble screen location definitions and
!              coefficients from the attribute file
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    use string_module
    use dfparall 
    use system_utils, only: exifil
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    real(fp), dimension(:)  , pointer :: zbubl
    integer, pointer       :: mmaxgl
    integer, pointer       :: nmaxgl
    integer, pointer       :: mfg 
    integer, pointer       :: nfg 
    integer, pointer       :: mlg 
    integer, pointer       :: nlg 
!
! Global variables
!
    integer                          , intent(in)  :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                        :: lundia !  Description and declaration in inout.igs
    integer                          , intent(in)  :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                                        :: nmax   !  Description and declaration in esm_alloc_int.f90
    integer                          , intent(in)  :: nmaxus !  Description and declaration in esm_alloc_int.f90
    integer                                        :: nbub   !  Description and declaration in dimens.igs
    integer                                        :: nsrc   !  Description and declaration in dimens.igs
    integer                          , intent(in)  :: nxbub  !  Description and declaration in dimens.igs
    integer      , dimension(7, nsrc), intent(out) :: mnksrc !  Description and declaration in esm_alloc_int.f90
    logical                                        :: error  !!  Flag=TRUE if an error is encountered
    character(*)                                   :: filbub !!  File name for Bubble screen
    character(20), dimension(nsrc)   ,intent(out)  :: namsrc !  Description and declaration in esm_alloc_char.f90
!
! Local variables
!
    integer                                     :: i
    integer                                     :: ibub    ! Loop variable 
    integer                                     :: ibeg    ! Begin position in the RECORD from where the search for data/record is started 
    integer                                     :: icount  ! Loop variable 
    integer                                     :: idef    ! Help var. containing default va- lue(s) for integer variable 
    integer                                     :: iend    ! Last position in the RECORD when the searched data/record is finished 
    integer                                     :: ier     ! =  0 -> end of record encountered =  1 -> real value found = -1 -> length or number of data is larger then specified by the calling routine 
    integer                                     :: ii      ! Loop variable 
    integer                                     :: incx    ! Increment between M1,M2 
    integer                                     :: incy    ! Increment between N1,N2 
    integer                                     :: iocond  ! IO status for reading
    integer                                     :: j
    integer                                     :: k       ! Layer variable 
    integer                                     :: lenc    ! Number of char. to read in string 
    integer                                     :: lfile   ! Number of non blank characters of file name 
    integer                                     :: luntmp  ! Temporary file unit 
    integer                                     :: m       ! loop counter 
    integer                                     :: m1      ! First m-index for bubble screen 
    integer                                     :: m2      ! Last m-index for bubble screen 
    integer                                     :: mint    ! Last m-index for bubble screen 
    integer                                     :: maxinc  ! Maximum of (increment between M1,M2 & increment between N1,N2) 
    integer                                     :: mcount  ! Nr. of bubble screens in M-direction
    integer                                     :: n
    integer                                     :: n1      ! First n-index for bubble screen 
    integer                                     :: n2      ! Last n-index for bubble screen 
    integer                                     :: ncount  ! Nr. of bubble screens in N-direction
    integer                                     :: nint    ! Last m-index for bubble screen 
    integer, external                           :: newlun
    integer                                     :: nlook   ! Nr. of values to look for in a record 
    integer, dimension(4)                       :: ival    ! Help array (integer) where the data, recently read from the MD-file, are stored temporarily 
    integer, dimension(:,:)       , allocatable :: itemp   ! work array to store mnksrc temporarily 
    integer, dimension(nsrc)                    :: nsd     ! integer array to store sequence number of arrays mnksrc and namsrc in own subdomain 
    real(fp)                                    :: rdef    ! Help var. containing default va- lue(s) for real variable 
    real(fp)                                    :: zbub    ! Help var. containing default va- lue(s) for real variable 
    real(fp)     , dimension(:)   , allocatable :: zbubltemp
    character(1)                                :: direct  ! Help string for reading direction 
    character(132)                              :: rec132  ! Standard rec. length in an attribute file (132) 
    character(20)                               :: cdefn   ! Default value when CVAR not found 
    character(20)                               :: chulp   ! Help charatcter string
    character(20), dimension(:)   , allocatable :: ctmp2      ! work array to store namsrc temporarily 
    character(40)                               :: errmsg  ! Text string error messages
    character(300)                              :: message
!
!! executable statements -------------------------------------------------------
!
    zbubl   => gdp%gdbubble%zbubl
    mmaxgl  => gdp%gdparall%mmaxgl
    nmaxgl  => gdp%gdparall%nmaxgl
    mfg     => gdp%gdparall%mfg 
    mlg     => gdp%gdparall%mlg 
    nfg     => gdp%gdparall%nfg 
    nlg     => gdp%gdparall%nlg 
    !
    cdefn  = ' '
    direct = ' '
    idef   = 0
    rdef   = 0.0_fp
    lenc   = 1
    if (kmax < 3) then
       write (errmsg,'(a)') 'Not enough layers for bubble screen. Increase the number of layers.'
       call prterr(lundia, 'P004', trim(errmsg))
       call d3stop(1, gdp)
    endif
    errmsg = 'bubble screen '
    write (message, '(2a)') 'Reading Bubble screen file ', trim(filbub)
    call prterr(lundia, 'G051', trim(message))
    !
    ! test file existence
    !
    call remove_leading_spaces(filbub, lfile)
    error = .not.exifil(filbub, lundia)
    if (error) goto 9999
    !
    ! open formatted file, if not formatted IOCOND <> 0
    !
    open (newunit=luntmp, file = filbub(:lfile), status = 'old', iostat = iocond)
    if (iocond /= 0) then
       error = .true.
       call prterr(lundia, 'G007', filbub(:lfile))
       goto 9999
    endif
    !
    ! freeformatted file, skip lines starting with a '*'
    !
    call skipstarlines(luntmp)
    !
    ! freeformatted file, read input and test iocond
    !
    icount = nsrc - nbub
    do ibub = 1, nxbub
    ! --->
  100  continue
       read (luntmp, '(a)', iostat = iocond) rec132
       if (iocond /= 0) then
          if (iocond < 0) then
             call prterr(lundia, 'G006', filbub(:lfile))
          else
             call prterr(lundia, 'G007', filbub(:lfile))
          endif
          error = .true.
          goto 300
       endif
       !
       icount = icount + 1
       !
       !  Ignore empty lines
       !
       if (rec132 == ' ') goto 100
       ! <---
       !
       ! define bubble screen location name
       !
       namsrc(icount) = rec132(:20)
       call small(namsrc(icount), 20)
       !
       ! there must be a name defined !!
       !
       if (namsrc(icount) == cdefn) then
          errmsg(12:) = ': no name defined'
          error = .true.
          call prterr(lundia, 'U021', errmsg)
          goto 300
       endif
       !
       ! Read IVAL  and define local variables
       !
       lenc  = 1
       ibeg  = 21
       nlook = 4
       call readni(rec132    ,132       ,ibeg      ,iend      ,nlook     , &
                 & ival      ,idef      ,ier       )
       if (ier <= 0) then
          error = .true.
          call prterr(lundia, 'G007', filbub(1:lfile))
          goto 300
       endif
       !
       ! bubble screen location is defined in M,N coordinates
       !
       m1 = ival(1)
       n1 = ival(2)
       m2 = ival(3)
       n2 = ival(4)
       !
       ! read ZUBUBL from record, default value not allowed
       !
       zbub = 0.0
       ibeg = iend + 1
       call read1r(rec132    ,132       ,ibeg      ,iend      ,zbub        , &
                 & rdef      ,ier       )
       if (ier <= 0) then
          call prterr(lundia, 'G007', filbub(1:lfile))
          error = .true.
          goto 300
       endif
       !
       ! Test contents of read coordinate pairs
       !
       call increm(m1        ,n1        ,m2        ,n2        ,incx      , &
                 & incy      ,maxinc    ,error     )
       !
       ! If error then no multiple of 45 degrees
       !
       if (error) then
          errmsg(9:) = namsrc(icount)
          call prterr(lundia, 'V231', errmsg(:32))
          error = .true.
          goto 300
       endif
       !
       ! Inside (global) computational domain ?
       !
       if (m1>mmaxgl .or. m1<1 .or. m2>mmaxgl .or. m2<1) then
          call prterr(lundia, 'V249', errmsg(:22))
          error = .true.
          goto 300
       endif
       if (n1>nmaxgl .or. n1<1 .or. n2>nmaxgl .or. n2<1) then
          call prterr(lundia, 'V249', errmsg(:22))
          error = .true.
          goto 300
       endif
       mcount           = abs(ival(3)-ival(1)) + 1
       ncount           = abs(ival(4)-ival(2)) + 1
       mnksrc(1,icount) = m1
       mnksrc(2,icount) = n1
       mnksrc(3,icount) = 1
       chulp            = rec132(:20)
       namsrc(icount)   = chulp
       zbubl (icount)   = zbub
       !
       mnksrc(4,icount) = mnksrc(1,icount)
       mnksrc(5,icount) = mnksrc(2,icount)
       mnksrc(6,icount) = mnksrc(3,icount)
       !
       ! also for the other layers
       !
       do k = 2, kmax
          icount           = icount + 1
          mnksrc(1,icount) = m1
          mnksrc(2,icount) = n1
          mnksrc(3,icount) = k
          chulp            = rec132(:20)
          namsrc(icount)   = chulp
          zbubl (icount)   = zbub
          !
          mnksrc(4,icount) = mnksrc(1,icount)
          mnksrc(5,icount) = mnksrc(2,icount)
          mnksrc(6,icount) = mnksrc(3,icount)
       enddo
       !
       ! also for the other bubble screen points at this line segment
       !
       do m = 1, maxinc
          do k = 1, kmax
             icount           = icount + 1
             mnksrc(1,icount) = m1 + (m-1) * incx
             mnksrc(2,icount) = n1 + (m-1) * incy
             mnksrc(3,icount) = k
             chulp            = rec132(:20)
             namsrc(icount)   = chulp
             zbubl (icount)   = zbub
             !
             mnksrc(4,icount) = mnksrc(1,icount)
             mnksrc(5,icount) = mnksrc(2,icount)
             mnksrc(6,icount) = mnksrc(3,icount)
          enddo
       enddo
    enddo
    !
    ! close file
    !
  300 continue
    close (luntmp)
    ! 
    ! for parallel runs, determine which discharge points are outside this partition and disable them
    ! 
    if (parll) then
       do n = nsrc-nbub+1, nsrc
          m1 = mnksrc(1,n) - mfg + 1 
          n1 = mnksrc(2,n) - nfg + 1 
          mnksrc(1,n) = m1 
          mnksrc(2,n) = n1 
          mnksrc(4,n) = m1 
          mnksrc(5,n) = n1 
          ! 
          ! check if bubble points is inside or outside subdomain 
          ! 
          if ( m1 < 1 .or. n1 < 1 .or. m1 > mmax .or. n1 > nmaxus ) then
             !
             ! Disable this bubble point by setting k to -1
             !
             mnksrc(3,n) = -1
             mnksrc(6,n) = -1
             write(message,'(a,3(i0,a))') 'Bubble screen related discharge point (m,n,k)=(', mnksrc(1, n), ',', mnksrc(2, n), ',', mnksrc(3, n), ') is disabled: not in this subdomain'
             call prterr( lundia, 'U190', trim(message))
          endif 
       enddo 
    endif 
 9999 continue
end subroutine bubfil
