subroutine ipon(xpoly     ,ypoly     ,n         ,xp        ,yp        , &
              & inout     ,lunlog)
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
!--description----------------------------------------------------------------
!
! Detect whether point (xp,yp) lies inside polygon (x,y) of n points.
! Point n+1 is made equal to point 1.
! inout = -1 :  Outside polygon
! inout =  0 :  On boundary of polygon
! inout =  1 :  Inside polygon
!
!--pseudo code and references-------------------------------------------------
!
! Author: J.A. Roelvink
! Date  : 22 Dec 1988
!
! Method used:
! - Draw a vertical line through (xp,yp)
! - Detect the number of crossings with the polygon below yp: nonder
! - If nonder is even, the point is outside the polygon, else inside
! - The boundary is handled seperately
!
!--declarations---------------------------------------------------------------
    use precision_kubint
!
! Global variables
!
    integer           , intent(out) :: inout
    integer           , intent(in)  :: lunlog
    integer           , intent(in)  :: n
    real(hp)          , intent(in)  :: xp
    real(hp)          , intent(in)  :: yp
    real(hp), dimension(*), intent(in)  :: xpoly
    real(hp), dimension(*), intent(in)  :: ypoly
!
! Local variables
!
    integer :: i
    integer :: istat
    integer :: nonder
    real(hp):: ysn
    real(hp), dimension(:), pointer  :: x
    real(hp), dimension(:), pointer  :: y
!
! executable statements ------------------------------------------------------
!
    !
    if (n > maxpolpoint) then
       !
       ! ipon is a small routine that may be called for all points of a grid.
       ! It should not be loaded with a allocate/deallocate each time it is called.
       ! Therefore the allocated memory in the previous call is used, unless this
       ! space is too small.
       !
       if (maxpolpoint > 0) then
          deallocate (x, stat = istat)
          deallocate (y, stat = istat)
       endif
       maxpolpoint = n
       !
       ! allocate the arrays x and y with one more than the number of points in the
       ! polygon: the first point is copied in position n+1.
       !
                     allocate (x(maxpolpoint+1), stat = istat)
       if (istat==0) allocate (y(maxpolpoint+1), stat = istat)
       if (istat/=0) then
          write(lunlog, '(''Error: Memory allocation error in routine IPON'')')
       endif
    endif
    do i = 1, n
       x(i) = xpoly(i) - xp
       y(i) = ypoly(i) - yp
    enddo
    x(n + 1) = x(1)
    y(n + 1) = y(1)
    nonder   = 0
    do i = 1, n
       if ((x(i    )<0._hp .and. x(i + 1)>=0._hp).or. &
           (x(i + 1)<0._hp .and. x(i    )>=0._hp)     ) then
          if (y(i)<0._hp .and. y(i + 1)<0._hp) then
             nonder = nonder + 1
          elseif ((y(i    )<=0._hp .and. y(i + 1)>=0._hp) .or. &
                & (y(i + 1)<=0._hp .and. y(i    )>=0._hp)) then
             ysn = (y(i)*x(i + 1) - x(i)*y(i + 1))/(x(i + 1) - x(i))
             if (ysn<0._hp) then
                nonder = nonder + 1
             elseif (ysn<=0._hp) then
                !
                ! boundary
                !
                inout = 0
                goto 100
             else
             endif
          else
          endif
       elseif (abs(x(i))<1.0D-8 .and. abs(x(i + 1))<1.0D-8) then
          if ((y(i    )<=0._hp .and. y(i + 1)>=0._hp) .or. &
              (y(i + 1)<=0._hp .and. y(i    )>=0._hp)      ) then
             !
             ! boundary
             !
             inout = 0
             goto 100
          endif
       else
       endif
    enddo
    if (mod(nonder, 2)==0) then
       !
       ! outside
       !
       inout = -1
    else
       !
       ! inside
       !
       inout = 1
    endif
  100 continue
end subroutine ipon
