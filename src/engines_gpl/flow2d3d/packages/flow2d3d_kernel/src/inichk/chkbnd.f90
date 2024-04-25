subroutine chkbnd(lundia    ,error     ,nmax      ,mmax      ,nrob      , &
                & nlcest    ,noroco    ,norow     ,nocol     ,nob       , &
                & irocol    ,icom      ,idupl     ,mnbnd     ,gdp       )
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
!    Function: - Checks whether open boundaries lie on in- or
!                outside the active points
!              - Checks whether non-water elevation type of open
!                boundaries have been described on a vertex of
!                the comp. grid enclosure
!              - Fills the IROCOL table
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
    use globaldata
    use dfparall
    !
    implicit none
!
! Constants
!
    !
    ! icom values can become negative and, when having very much boundaries, very big
    ! be sure to use a value that differs from normal values
    !
    integer, parameter :: parll_int_flag = -999999999
!
! GDP
!
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer, pointer :: nmaxus
!
! Global variables
!
    integer                                                          :: lundia !  Description and declaration in inout.igs
    integer                                            , intent(in)  :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer, dimension(7, gdp%d%nto)                                 :: mnbnd  !  Description and declaration in esm_alloc_int.f90
    integer                                            , intent(in)  :: nlcest !  Description and declaration in dimens.igs
    integer                                            , intent(in)  :: nmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                          :: nocol  !  Description and declaration in esm_alloc_int.f90
    integer                                                          :: noroco !  Description and declaration in esm_alloc_int.f90
    integer                                                          :: norow  !  Description and declaration in esm_alloc_int.f90
    integer                                            , intent(in)  :: nrob   !  Description and declaration in esm_alloc_int.f90
    integer, dimension(5, nlcest)                                    :: irocol !  Description and declaration in esm_alloc_int.f90
    integer, dimension(8, nrob)                                      :: nob    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%mlb:gdp%d%mub, gdp%d%nlb:gdp%d%nub)     :: icom   !!  Temporary work array containing:
                                                                               !!      0 = if a point is not active
                                                                               !!      1 = if a point is active
    integer, dimension(gdp%d%mlb:gdp%d%mub, gdp%d%nlb:gdp%d%nub)     :: idupl
    logical                                                          :: error  !!  Flag=TRUE if an error is encountered
!
! Local variables
!
    integer       :: i        ! Help var. 
    integer       :: ibt1     ! Type of boundary at the beginning of the comp. ROW/COLUMN 
    integer       :: ibt2     ! Type of boundary at the end       of the comp. ROW/COLUMN 
    integer       :: iflag1   ! Help var. 
    integer       :: iflag2   ! Help var. 
    integer       :: itypbn   ! Help var. for the boundary type 
    integer       :: ix       ! X-coord. of the open boundary point in concern 
    integer       :: ixd      ! Max. (1   ,IX-1) 
    integer       :: ixu      ! Min. (MMAX,IX+1) 
    integer       :: iy       ! Y-coord. of the open boundary point in concern 
    integer       :: iyd      ! Max. (1   ,IY-1) 
    integer       :: iyu      ! Min. (NMAX,IY+1) 
    integer       :: m        ! Current M-index of the active point in the current computational ROW 
    integer       :: mf       ! First M-index of the active point in the current computational ROW 
    integer       :: ml
    integer       :: mlu      ! One after last M-index of the active point in the current computational ROW 
    integer       :: n        ! Current N-index of the active point in the current computational COLUMN 
    integer       :: nf       ! First N-index of the active point in the current computational COLUMN 
    integer       :: nl
    integer       :: nlu      ! One after last N-index of the active point in the current computational COLUMN 
    integer       :: norocx   ! Min. (NOROCO,NLCEST) 
    integer       :: norowx   ! Min. (NOROW ,NLCEST) 
    integer       :: nr       ! Sequence number of open boundary points 
    integer       :: commBotBoundary
    integer       :: commTopBoundary
    logical       :: addbndpnt
    character(11) :: errmsg   ! Character var. containing the errormessage to be written to file. The message depend on the error. 
!
!! executable statements -------------------------------------------------------
!
    ! initialisation
    !
    nmaxus => gdp%d%nmaxus
    !
    norow  = 0
    nocol  = 0
    noroco = 0
    !
    ! initialisation of local parameters not required
    ! (lnto > nto is tested in rdbndd)
    !
    ! fill in boundary type characteristics, check against value of icom
    ! (boundary may not lie on points with icom(m,n)= 1 -> active pnt
    !                                 or   icom(m,n)= 0 -> inactive pnt)
    ! points with icom(m,n) > 1 or < 0 are however allowed)
    !
    do nr = 1, nrob
       ix = nob(1, nr)
       iy = nob(2, nr)
       write (errmsg, '(''('',i4,'','',i4,'')'')') ix, iy
       if (icom(ix, iy)==0) then
          error = .true.
          call prterr(lundia    ,'V031'    ,errmsg    )
       elseif (icom(ix, iy)==1) then
          error = .true.
          call prterr(lundia    ,'V030'    ,errmsg    )
       else
       endif
    enddo
    if (error) goto 9999
    !
    ! change grid polygons code to 0, if not a computational point
    !
    do m = 1, mmax
       do n = 1, nmax
          if (icom(m,n) /= 1) then
             icom(m, n) = 0
          endif
       enddo
    enddo
    !
    ! boundary point may not lie on a comp. grid vertex
    !
    do nr = 1, nrob
       ix     = nob(1, nr)
       iy     = nob(2, nr)
       itypbn = nob(3, nr)
       ixd    = max(ix - 1, 1)
       iyd    = max(iy - 1, 1)
       ixu    = min(mmax, ix + 1)
       iyu    = min(nmax, iy + 1)
       write (errmsg, '(''('',i4,'','',i4,'')'')') ix, iy
       if (itypbn /= 2) then
          if (ix/=1 .and. ix/=mmax .and. iy/=1 .and. iy/=nmax) then
             if (icom(ixd, iy)==0 .and. icom(ix, iyd)==0 .and. icom(ixu, iy)==1     &
               & .and. icom(ix, iyu)==1) then
                error = .true.
                call prterr(lundia    ,'V032'    ,errmsg    )
             elseif (icom(ixu, iy)==0 .and. icom(ix, iyd)==0 .and. icom(ixd, iy)==1 &
                   & .and. icom(ix, iyu)==1) then
                error = .true.
                call prterr(lundia    ,'V032'    ,errmsg    )
             elseif (icom(ixu, iy)==0 .and. icom(ix, iyu)==0 .and. icom(ixd, iy)==1 &
                   & .and. icom(ix, iyd)==1) then
                error = .true.
                call prterr(lundia    ,'V032'    ,errmsg    )
             elseif (icom(ixd, iy)==0 .and. icom(ix, iyu)==0 .and. icom(ixu, iy)==1 &
                   & .and. icom(ix, iyd)==1) then
                error = .true.
                call prterr(lundia    ,'V032'    ,errmsg    )
             else
             endif
          endif
       endif
       if (icom(ixd, iy)==0 .and. icom(ix, iyd)==0 .and. icom(ixu, iy)==0 &
         & .and. icom(ix, iyu)==0) then
          error = .true.
          call prterr(lundia    ,'V032'    ,errmsg    )
       endif
    enddo
    if (error) goto 9999
    !
    ! for parallel Delft3D-FLOW, indicate coupling boundaries as such
    ! these coupling points are usually computational ones, i.e. icom=1
    ! by setting them to a ghost value, they are regarded as boundary points
    !
    if (parll) then
       if (idir == 1) then
          do m = 1, mmax
             if ( icom(m,     1) == 1 ) icom(m,     1) = parll_int_flag
             if ( icom(m,nmaxus) == 1 ) icom(m,nmaxus) = parll_int_flag
          enddo
       endif
       if (idir == 2) then
          do n = 1, nmax
             if ( icom(   1,n) == 1 ) icom(   1,n) = parll_int_flag
             if ( icom(mmax,n) == 1 ) icom(mmax,n) = parll_int_flag
          enddo
       endif
    endif
    !
    ! fill irocol table when all boundary are filled
    !
    ! The complex addbndpnt tests for parallel uses data from array icom outside the active area
    ! This means that data is used from the halo
    ! This data is necessary! Replacing "n-1" by "max(1,n-1)" will cause an abort when running parallel
    ! When running sequential, "n-1" will cause an abort: the halo is not available.
    ! Therefore the addbndpnt conditions MUST be different for parallel and sequential
    !
    do n = 1, nmax
       do m = 2, mmax - 1
          if (parll) then
             addbndpnt =      ((icom(m-1,n)==   0 .or.  icom(m-1,n)==parll_int_flag) .and. icom(m,n)==1) &
                       & .or. ( icom(m-1,n)==   0 .and. icom(m  ,n)==parll_int_flag)                     &
                       & .or. ( icom(m-1,n)==parll_int_flag .and. icom(m  ,n)==parll_int_flag                      &
                       &        .and. (icom(m-1,n-1)==parll_int_flag .or. icom(m-1,n+1)==parll_int_flag))          &
                       & .or. ( icom(m-1,n)==parll_int_flag .and. icom(m  ,n)==   0  .and. idir==2)      &
                       & .or. ( icom(m+1,n)==parll_int_flag .and. icom(m  ,n)==   0  .and. idir==2)
          else
             addbndpnt = icom(m-1,n)==0 .and. icom(m,n)==1
          endif
          if (addbndpnt) then
             norow = norow + 1
             norowx = min(norow, nlcest)
             irocol(1, norowx) = n
             irocol(2, norowx) = m
             irocol(4, norowx) = 1
          endif
          if (parll) then
             addbndpnt =      ((icom(m+1,n)==   0 .or.  icom(m+1,n)==parll_int_flag) .and. icom(m,n)==1) &
                       & .or. ( icom(m+1,n)==   0 .and. icom(m  ,n)==parll_int_flag)                     &
                       & .or. ( icom(m+1,n)==parll_int_flag .and. icom(m  ,n)==parll_int_flag                      &
                       &        .and. (icom(m+1,n-1)==parll_int_flag .or. icom(m+1,n+1)==parll_int_flag))          &
                       & .or. ( icom(m-1,n)==parll_int_flag .and. icom(m  ,n)==   0  .and. idir==2)      &
                       & .or. ( icom(m+1,n)==parll_int_flag .and. icom(m  ,n)==   0  .and. idir==2)
          else
             addbndpnt = icom(m+1,n)==0 .and. icom(m,n)==1
          endif
          if (addbndpnt) then
             irocol(3, norowx) = m
             irocol(5, norowx) = 1
          endif
       enddo
    enddo
    !
    do m = 1, mmax
       do n = 2, nmax - 1
          if (parll) then
             addbndpnt =      ((icom(m,n-1)==   0 .or.  icom(m,n-1)==parll_int_flag) .and. icom(m,n)==1) &
                       & .or. ( icom(m,n-1)==   0 .and. icom(m,n  )==parll_int_flag)                     &
                       & .or. ( icom(m,n-1)==parll_int_flag .and. icom(m,n  )==parll_int_flag                      &
                       &       .and. (icom(m-1,n-1)==parll_int_flag .or. icom(m+1,n-1)==parll_int_flag))           & 
                       & .or. ( icom(m,n-1)==parll_int_flag .and. icom(m,n  )==   0  .and. idir==1)      &
                       & .or. ( icom(m,n+1)==parll_int_flag .and. icom(m,n  )==   0  .and. idir==1)
          else
             addbndpnt = icom(m,n-1)==0 .and. icom(m,n)==1
          endif
          if (addbndpnt) then
             nocol = nocol + 1
             noroco = norow + nocol
             norocx = min(noroco, nlcest)
             irocol(1, norocx) = m
             irocol(2, norocx) = n
             irocol(4, norocx) = 1
          endif
          if (parll) then
             addbndpnt =       ((icom(m,n+1)==   0 .or.  icom(m,n+1)==parll_int_flag) .and. icom(m,n)==1) &
                       & .or.  ( icom(m,n+1)==   0 .and. icom(m,n  )==parll_int_flag)                     &
                       & .or.  ( icom(m,n+1)==parll_int_flag .and. icom(m,n  )==parll_int_flag                      &
                       &         .and. (icom(m-1,n+1)==parll_int_flag .or. icom(m+1,n+1)==parll_int_flag))          &
                       & .or.  ( icom(m,n-1)==parll_int_flag .and. icom(m,n  )==   0  .and. idir==1)      &
                       & .or.  ( icom(m,n+1)==parll_int_flag .and. icom(m,n  )==   0  .and. idir==1)
          else
             addbndpnt = icom(m,n+1)==0 .and. icom(m,n)==1
          endif
          if (addbndpnt) then
             irocol(3, norocx) = n
             irocol(5, norocx) = 1
          endif
       enddo
    enddo
    !
    ! fill in boundary type characteristics
    ! if a boundary point is already defined (icom (ix,iy) is < 0)
    ! then the relevant POINT is encountered twice.  Flag idupl(ix,iy)
    ! is subsequently set to nr
    !
    do nr = 1, nrob
       ix = nob(1, nr)
       iy = nob(2, nr)
       if (icom(ix, iy)<0 .and. icom(ix, iy)/=parll_int_flag) then
          idupl(ix, iy) = nr
       elseif (icom(ix, iy)/=parll_int_flag) then
          icom(ix, iy) = -nr
       endif
    enddo
    !
    ! check array boundary of irocol table (noroco .gt. nlcest)
    ! if noroco .gt. nlcest then contents of irocol is rubbish
    !
    if (noroco > nlcest) then
       error = .true.
       call prterr(lundia, 'P004', 'Insufficient memory allocated for boundary data. Increase space by adding the following line to the mdf-file:')
       write(lundia,'(a,i0)') "nlcest = ", noroco
       goto 9999
    endif
    !
    ! Fill in rows with boundary type (index 4 and 5 of irocol table)
    ! Check whether idupl has values different from zero
    !
    ! The commBotBoundary and commTopBoundary are introduced to check if
    ! the boundary is a communication boundary. In that case the irocol table 
    ! is set to 10 for compute points which lie in the halo area.
    !
    commBotBoundary=0
    commTopBoundary=0
    do i = 1, norow
       n      = irocol(1, i)
       mf     = irocol(2, i) - 1
       mlu    = irocol(3, i) + 1
       ibt1   = icom(mf, n)
       ibt2   = icom(mlu, n)
       iflag1 = idupl(mf, n)
       iflag2 = idupl(mlu, n)
       if (i>1) then
          if (irocol(4, i-1) == 10) then
             commBotBoundary = 1
          endif
       endif
       if (i>1) then
          if (irocol(5,i-1) == 10) then
             commTopBoundary = 1
          endif
       endif
       if (ibt1 > 0) then
          !
          ! stop 'error in grid boundaries occured'
          !
          error = .true.
          goto 9999
       elseif (ibt1 == 0) then
          irocol(4, i) = 1
       elseif (ibt1==parll_int_flag) then
          irocol(4, i) = 10
          !
          ! add case norow==1 not accounted for in tests lines 320-321 otherwise
          ! (as i>1 never satisfied)
          !
          if (parll.and.norow==1) commBotBoundary=1
       else
          irocol(4, i)  = nob(3, -ibt1)
          nob(4, -ibt1) = 1
          nob(5, -ibt1) = i
       endif
       !
       ! set identical code for points that were encountered twice
       !
       if (iflag1 /= 0) then
          nob(4, iflag1) = 1
          nob(5, iflag1) = i
       endif
       if (ibt2 > 0) then
          !
          ! stop 'error in grid boundaries occured'
          !
          error = .true.
          goto 9999
       elseif (ibt2 == 0) then
          irocol(5, i) = 1
       elseif (ibt2==parll_int_flag) then
          irocol(5, i) = 10
          !
          ! add case norow==1 not accounted for in tests lines 320-321 otherwise
          ! (as i>1 never satisfied)
          !
          if (parll.and.norow==1) commTopBoundary=1
       else
          irocol(5, i)  = nob(3, -ibt2)
          nob(4, -ibt2) = 2
          nob(5, -ibt2) = i
          !
          ! set identical code for points that were encountered twice
          !
          if (iflag2 /= 0) then
             nob(4, iflag2) = 2
             nob(5, iflag2) = i
          endif
       endif
    enddo
    !
    ! for parallel code check communication boundaries again for 
    ! defined points in the halo area and adjust start and end irocol table
    !
    do i = 1, norow
       n    = irocol(1, i)
       m    = irocol(2, i)
       mf   = irocol(2, i) - 1
       ml   = irocol(3, i) 
       ibt1 = icom(mf, n)
       ibt2 = icom(ml+1, n)
       if ( (commBotBoundary==1) .and. (mf.lt.ihalom) ) then
          irocol(4, i) = 10
          !
          ! adjust boundaries irocol(2,i) and irocol(3,i) within halo area
          !
          if (icom(1,n)==0) then
             if (icom(2,n)/=0) then
                irocol(2,i) = 2
             endif
          else 
             irocol(2,i) = 1
          endif
          if (ml.lt.ihalom) then
             if (icom(1,n)/=0) then
                if (icom(2,n)==0) then
                   irocol(3,i) = 1
                endif
             endif
          endif
       endif
       if ( (commTopBoundary==1) .and. (ml.gt.(mmax-ihalom)) ) then
          irocol(5, i) = 10
          !
          ! adjust boundaries irocol(2,i) and irocol(3,i) within halo area
          !
          if (icom(mmax,n)==0) then
             if (icom(mmax-1,n)/=0) then
                irocol(3,i) = mmax-1
             endif
          else 
             irocol(3,i) = mmax
          endif
          if (m.gt.(mmax-ihalom)) then
             if (icom(mmax,n)/=0) then
                if (icom(mmax-1,n)==0) then
                   irocol(2,i) = mmax
                endif
             endif
          endif
       endif
    enddo
    !
    ! fill in columns with boundary type (4 and 5 of irocol table)
    !
    commBotBoundary=0
    commTopBoundary=0
    do i = norow + 1, noroco
       m      = irocol(1, i)
       nf     = irocol(2, i) - 1
       nlu    = irocol(3, i) + 1
       ibt1   = icom(m, nf)
       ibt2   = icom(m, nlu)
       if ( (i.gt.(norow + 1)) .and. irocol(4, i-1)==10 ) then
          commBotBoundary=1
       endif
       if ( (i.gt.(norow + 1)) .and. irocol(5, i-1)==10 ) then
          commTopBoundary=1
       endif
       !
       ! in case that nmax /= nmaxus !!!
       !
       if ( icom(m, nlu-1) == parll_int_flag  .and. icom(m+1, nlu-1) == parll_int_flag ) then
          ibt2 = parll_int_flag
       endif
       iflag1 = idupl(m, nf)
       iflag2 = idupl(m, nlu)
       if (ibt1 > 0) then
          !
          ! stop 'error in grid boundaries occured'
          !
          error = .true.
          exit
       elseif (ibt1 == 0) then
          irocol(4, i) = 1
       elseif (ibt1==parll_int_flag) then
          irocol(4, i) = 10
       else
          irocol(4, i) = nob(3, -ibt1)
          nob(6, -ibt1) = 1
          nob(7, -ibt1) = i
          !
          ! set identical code for points that were encountered twice
          !
          if (iflag1 /= 0) then
             nob(6, iflag1) = 1
             nob(7, iflag1) = i
          endif
       endif
       if (ibt2 > 0) then
          !
          ! stop 'error in grid boundaries occured'
          !
          error = .true.
          exit
       elseif (ibt2 == 0) then
          irocol(5, i) = 1
       elseif (ibt2==parll_int_flag) then
          irocol(5, i) = 10
       else
          irocol(5, i)  = nob(3, -ibt2)
          nob(6, -ibt2) = 2
          nob(7, -ibt2) = i
          !
          ! set identical code for points that were encountered twice
          !
          if (iflag2 /= 0) then
             nob(6, iflag2) = 2
             nob(7, iflag2) = i
          endif
       endif
    enddo
    !
    ! set codes for mnbnd(7,n1), denoting the position of the openboundary, related to the complete grid: 
    !
    !                4 
    !            __________
    !           |++++++++++|
    !           |++++++++++|
    !         1 |++++++++++| 3
    !           |__________|
    !                      
    !                2
    !
    !
    do i = 1, nrob
       nr = nob(8, i)
       if (nob(4,i) == 1) mnbnd(7,nr) = 1 
       if (nob(4,i) == 2) mnbnd(7,nr) = 3 
       if (nob(6,i) == 1) mnbnd(7,nr) = 2 
       if (nob(6,i) == 2) mnbnd(7,nr) = 4 
    enddo
    !
    ! for parallel code check communication boundaries again for 
    ! defined points in the halo area which have kcs=0 on the boundary
    !
    do i = norow + 1, noroco
       m    = irocol(1, i)
       nf   = irocol(2, i) - 1
       nlu  = irocol(3, i) + 1
       nl   = irocol(3, i)
       ibt1 = icom(m, nf)
       ibt2 = icom(m, nlu)
       if ( (commBotBoundary==1) .and. (nf.lt.ihalon) ) then
          irocol(4, i) = 10
          ! 
          ! adjust boundaries irocol(2,i) and irocol(3,i) within halo area
          !
          if (icom(m,1)==0) then
             if (icom(m,2)/=0) then
                irocol(2,i) = 2
             endif
          else 
             irocol(2,i) = 1
          endif
          if (nl.lt.ihalon) then
             if (icom(m,1)/=0) then
                if (icom(m,2)==0) then
                   irocol(3,i) = 1
                endif
             endif
          endif
       endif
       !
       if ( (commTopBoundary==1) .and. (nl.gt.(nmaxus-ihalon)) ) then
          irocol(5, i) = 10
          ! 
          ! adjust boundaries irocol(2,i) and irocol(3,i) within halo area
          !
          if (icom(m,nmaxus)==0) then
             if (icom(m,nmaxus-1)/=0) then
                irocol(3,i) = nmaxus-1
             endif
          else 
             irocol(3,i) = nmaxus
          endif
          if (nl.gt.(nmaxus-ihalon)) then
             if (icom(m,nmaxus)/=0) then
                if (icom(m,nmaxus-1)==0) then
                   irocol(2,i) = nmaxus
                endif
             endif
          endif
       endif
    enddo
 9999 continue
end subroutine chkbnd
