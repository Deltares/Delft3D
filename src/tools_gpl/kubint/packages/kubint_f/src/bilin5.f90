subroutine bilin5(xa        ,ya        ,x0        ,y0        ,w         , &
                & ier       )
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
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision_kubint
    implicit none
!
! Global variables
!
    integer, intent(out)           :: ier
    real(hp), intent(in)               :: x0
    real(hp), intent(in)               :: y0
    real(hp), dimension(4), intent(out) :: w
    real(hp), dimension(4), intent(in) :: xa
    real(hp), dimension(4), intent(in) :: ya
!
!
! Local variables
!
    real(hp)                           :: a
    real(hp)                           :: a21
    real(hp)                           :: a22
    real(hp)                           :: a31
    real(hp)                           :: a32
    real(hp)                           :: a41
    real(hp)                           :: a42
    real(hp)                           :: b
    real(hp)                           :: c
    real(hp)                           :: det
    real(hp)                           :: discr
    real(hp)                           :: eta
    real(hp)                           :: x
    real(hp)                           :: x1
    real(hp)                           :: x2
    real(hp)                           :: x3
    real(hp)                           :: x3t
    real(hp)                           :: x4
    real(hp)                           :: xi
    real(hp)                           :: xt
    real(hp)                           :: y
    real(hp)                           :: y1
    real(hp)                           :: y2
    real(hp)                           :: y3
    real(hp)                           :: y3t
    real(hp)                           :: y4
    real(hp)                           :: yt
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    ! Author: H. Petit
    !
    !
    !     read(12,*)x1,y1,f1
    x1 = xa(1)
    y1 = ya(1)
    !     read(12,*)x2,y2,f2
    x2 = xa(2)
    y2 = ya(2)
    !     read(12,*)x3,y3,f3
    x3 = xa(3)
    y3 = ya(3)
    !     read(12,*)x4,y4,f4
    x4 = xa(4)
    y4 = ya(4)
    x = x0
    y = y0
    ! The bilinear interpolation problem is first transformed
    ! to the quadrangle with nodes
    ! (0,0),(1,0),(x3t,y3t),(0,1)
    ! and required location (xt,yt)
    a21 = x2 - x1
    a22 = y2 - y1
    a31 = x3 - x1
    a32 = y3 - y1
    a41 = x4 - x1
    a42 = y4 - y1
    det = a21*a42 - a22*a41
    if (abs(det)<1D-20) then
       ! write (*, *) 'surface is zero'
       ier = 1
       goto 99999
    endif
    x3t = (a42*a31 - a41*a32)/det
    y3t = ( - a22*a31 + a21*a32)/det
    xt = (a42*(x - x1) - a41*(y - y1))/det
    yt = ( - a22*(x - x1) + a21*(y - y1))/det
    if ((x3t<.0_hp) .or. (y3t<.0_hp)) then
       ! write (*, *) 'distorted quadrangle'
       ier = 1
       goto 99999
    endif
    if (abs(x3t - 1.0)<1.0D-7) then
       xi = xt
       if (abs(y3t - 1.0)<1.0D-7) then
          eta = yt
       elseif (abs(1.0_hp + (y3t - 1.0_hp)*xt)<1.0D-6) then
          ! write (*, *) 'extrapolation over too large a distance'
          ier = 1
          goto 99999
       else
          eta = yt/(1.0_hp + (y3t - 1.0_hp)*xt)
       endif
    elseif (abs(y3t - 1.0_hp)<1.0D-6) then
       eta = yt
       if (abs(1.0_hp + (x3t - 1.0_hp)*yt)<1.D-6) then
          ! write (*, *) 'extrapolation over too large a distance'
          ier = 1
          goto 99999
       else
          xi = xt/(1.0_hp + (x3t - 1.0_hp)*yt)
       endif
    else
       a = y3t - 1._hp
       b = 1.0_hp + (x3t - 1.0_hp)*yt - (y3t - 1.0_hp)*xt
       c = -xt
       discr = b*b - 4.0_hp*a*c
       if (discr<1.0D-6) then
          ! write (*, *) 'extrapolation over too large a distance'
          ier = 1
          goto 99999
       endif
       xi = ( - b + sqrt(discr))/(2.0_hp*a)
       eta = ((y3t - 1.0_hp)*(xi - xt) + (x3t - 1.0_hp)*yt)/(x3t - 1.0_hp)
    endif
    w(1) = (1._hp - xi)*(1._hp - eta)
    w(2) = xi*(1._hp - eta)
    w(3) = xi*eta
    w(4) = eta*(1._hp - xi)
    return
99999 continue
end subroutine bilin5
