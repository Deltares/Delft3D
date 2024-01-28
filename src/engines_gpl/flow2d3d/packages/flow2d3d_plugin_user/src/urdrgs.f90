subroutine urdrgs(lundia    ,error     ,filusr    ,nmax      ,mmax      , &
                & kmax      ,nmaxus    ,kcs       ,kspu      ,kspv      , &
                & ubrlsu    ,ubrlsv    ,gdp       )
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
!    Function: - Test existence of user defined file for
!                rigis sheets
!              - Read of input from user defined files and
!                initialize UBRLSU & UBRLSV
!              - Define mask array's for drying / flooding
!                upwind if point is speical point
!
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
    use globaldata
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
    integer                                                             , intent(in)  :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                           :: lundia !  Description and declaration in inout.igs
    integer                                                             , intent(in)  :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                           :: nmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                             , intent(in)  :: nmaxus !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)        , intent(in)  :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 0:kmax), intent(out) :: kspu   !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 0:kmax), intent(out) :: kspv   !  Description and declaration in esm_alloc_int.f90
    logical                                                                           :: error  !!  Flag=TRUE if an error is encountered
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax) , intent(out) :: ubrlsu !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax) , intent(out) :: ubrlsv !  Description and declaration in esm_alloc_real.f90
    character(256)                                                                    :: filusr !  Description and declaration in usrpar.igs
!
! Local variables
!
    integer               :: ibeg    ! Begin position in the RECORD from where the search for data/record is started 
    integer               :: idef    ! Help var. containing default va- lue(s) for integer variable 
    integer               :: iend    ! Last position in the RECORD when the searched data/record is finished 
    integer               :: ier     ! =  0 -> end of record encountered =  1 -> real value found = -1 -> length or number of data is larger then specified by the calling routine 
    integer               :: inc
    integer               :: incx    ! Increment between M1,M2 
    integer               :: incy    ! Increment between N1,N2 
    integer               :: iocond  ! IO status output: > 0 error < 0 end-of-file = 0 ok 
    integer               :: k       ! Loop variable 
    integer               :: k1
    integer               :: k2
    integer               :: kkmax
    integer               :: kkmin
    integer               :: lenc    ! Help var. (length of var. cvar to be looked for in the MD-file) 
    integer               :: lfile   ! Length of file name 
    integer               :: luntmp  ! Unit number for attribute file 
    integer               :: m
    integer               :: m1      ! First m-index for rigid sheets 
    integer               :: m2      ! Last m-index for rigid sheets 
    integer               :: maxinc  ! Maximum of (increment between M1,M2 & increment between N1,N2) 
    integer               :: n       ! Loop variable 
    integer               :: n1      ! First n-index for rigid sheets 
    integer               :: n2      ! Last n-index for rigid sheets 
    integer               :: newlun
    integer               :: nlook   ! Help var.: nr. of data to look for in the MD-file 
    integer               :: nrigid  ! Help var. (counter) 
    integer, dimension(6) :: ival    ! Help array (int.) where the data, recently read from file, are stored temporarily 
    logical               :: ex      ! Logical flag for file existence 
    real(fp)              :: brlosc  ! Constant value for wall roughness 
    real(fp)              :: rdef    ! Help var. containing default va- lue(s) for real variable 
    character(1)          :: direct  ! Direction (U/V) of rigid sheet 
    character(132)        :: rec132  ! Standard rec. length in an attribute file (132) 
    character(4)          :: errtxt  ! Text string error messages 
!
!! executable statements -------------------------------------------------------
!
    !-----Initialize local parameters
    !
    lenc = 1
    idef = 0
    rdef = 0.0
    !
    !-----Test file existence, if so read
    !
    lfile = index(filusr, ' ')
    if (lfile==0) lfile = len(filusr) + 1
    lfile = lfile - 1
    !
    inquire (file = filusr(1:lfile), exist = ex)
    if (ex) then
       !
       !--------File exists
       !
       nrigid = 0
       open (newunit=luntmp, file = filusr(1:lfile), form = 'formatted', status = 'old')
       !
       !-->     Read input till end-of-file
       !
 1000  continue
       read (luntmp, '(a)', iostat = iocond) rec132
       if (iocond/=0) then
          !
          !--------------Reading error?
          !
          if (iocond>0) then
             call prterr(lundia    ,'G007'    ,filusr(1:lfile)      )
             !
             error = .true.
          endif
          goto 1100
       endif
       !
       !-----------Read direct from record
       !           string to long and no value not allowed => IER > 0
       !
       ibeg = 1
       iend = 132
       call read1c(rec132    ,132       ,ibeg      ,iend      ,direct    , &
                 & lenc      ,ier       )
       if (ier<=0) then
          error = .true.
          call prterr(lundia    ,'G007'    ,filusr(1:lfile)      )
          !
          goto 1100
       endif
       !
       !-----------Read IVAL  and define local variables
       !           number of values to read (NLOOK) to many and
       !           no value not allowed => IER > 0
       !
       ibeg = iend + 1
       nlook = 6
       call readni(rec132    ,132       ,ibeg      ,iend      ,nlook     , &
                 & ival      ,idef      ,ier       )
       if (ier<=0) then
          error = .true.
          call prterr(lundia    ,'G007'    ,filusr(1:lfile)      )
          !
          goto 1100
       endif
       m1 = ival(1)
       n1 = ival(2)
       m2 = ival(3)
       n2 = ival(4)
       k1 = ival(5)
       k2 = ival(6)
       !
       !-----------Read BRLOSC from record, default value not allowed
       !
       ibeg = iend + 1
       call read1r(rec132    ,132       ,ibeg      ,iend      ,brlosc    , &
                 & rdef      ,ier       )
       if (ier<=0) then
          call prterr(lundia    ,'G007'    ,filusr(1:lfile)      )
          !
          error = .true.
          goto 1100
       endif
       !
       !-----------Define rigid sheet sequence number
       !
       nrigid = nrigid + 1
       write (errtxt, '(i4)') nrigid
       !
       !-----------Test contents of read coordinate pairs
       !
       call increm(m1        ,n1        ,m2        ,n2        ,incx      , &
                 & incy      ,maxinc    ,error     )
       !
       !-----------If error then no multiple of 45 degrees
       !
       if (error) then
          call prterr(lundia    ,'V231'    ,' '       )
          !
          goto 1100
       endif
       !
       !-----------Inside computational domain ?
       !
       if (m1>mmax .or. m1<1 .or. m2>mmax .or. m2<1) then
          call prterr(lundia    ,'V232'    ,errtxt    )
          !
          error = .true.
          goto 1100
       endif
       !
       if (n1>nmaxus .or. n1<1 .or. n2>nmaxus .or. n2<1) then
          call prterr(lundia    ,'V232'    ,errtxt    )
          !
          error = .true.
          goto 1100
       endif
       !
       kkmin = min(k1, k2)
       kkmax = max(k1, k2)
       if (kkmin<1 .or. kkmax>kmax) then
          call prterr(lundia    ,'V232'    ,errtxt    )
          !
          error = .true.
          goto 1100
       endif
       !
       !-----------Value for wall roughness permitted ?
       !
       if (brlosc<=0.0) then
          call prterr(lundia    ,'V234'    ,errtxt    )
          !
          error = .true.
          goto 1100
       endif
       !
       !-----------Write input BRLOSC to array for <M1,N1,K1> to <M2,N2,K2>
       !            belonging to the given direction in DIRECT
       !
       call small(direct    ,1         )
       m = m1 - incx
       n = n1 - incy
       !
       !-----------First U-direction
       !
       if (direct=='u') then
          do inc = 1, maxinc + 1
             m = m + incx
             n = n + incy
             if (kcs(n, m)/=1) then
                call prterr(lundia    ,'V232'    ,errtxt    )
                !
                error = .true.
                goto 1100
             endif
             kspu(n, m, 0) = 5
             do k = kkmin, kkmax
                ubrlsu(n, m, k) = brlosc
                kspu(n, m, k) = 1
             enddo
          enddo
       elseif (direct=='v') then
          do inc = 1, maxinc + 1
             m = m + incx
             n = n + incy
             kspv(n, m, 0) = 5
             if (kcs(n, m)/=1) then
                call prterr(lundia    ,'V232'    ,errtxt    )
                !
                error = .true.
                goto 1100
             endif
             do k = kkmin, kkmax
                kspv(n, m, k) = 1
                ubrlsv(n, m, k) = brlosc
             enddo
          enddo
       else
          call prterr(lundia    ,'V233'    ,direct    )
          !
          error = .true.
          goto 1100
       endif
       !
       goto 1000
       !
       ! <--    Next boundary number
       !
       !
       !--------Close file
       !
 1100  continue
       close (luntmp)
    else
       !
       !--------File not exist
       !
       call prterr(lundia    ,'G004'    ,filusr(1:lfile)      )
       !
       error = .true.
    endif
end subroutine urdrgs
