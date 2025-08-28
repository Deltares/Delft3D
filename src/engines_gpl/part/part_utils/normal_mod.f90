!!  Copyright (C)  Stichting Deltares, 2012-2025.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.

module normal_mod
    use random_generator

    implicit none

    integer, save :: nb                 !  a common number of boxes
    real(8), save, allocatable :: xbox(:), ybox(:)   !  the box x and y values

contains

    !     Generate the look-up table

    subroutine norm_init ()

        real(8)  pi     !  pi
        real(8)  a      !  the ziggurat basket surface size
        real(8)  t      !  help variable to determine the tail
        integer  ibox   !  number of lookup boxes

        nb = 256
        allocate (xbox(0:nb), ybox(0:nb))
        pi = 4.0d+00 * atan(1.0d+00)
        xbox(1) = 3.6484759268d+00
        ybox(1) = exp(-xbox(1) * xbox(1) / 2.0d+00)
        t = 1.0d+00 / (1.0d+00 + 0.3275911d+00 * xbox(1) / sqrt(2.0d+00))
        a = sqrt(pi) / 2.0d+00 * (0.254829592d+00 * t - 0.284496736d+00 * t * t + &
                &                           1.421413741d+00 * t * t * t - 1.453152027d+00 * t * t * t * t + &
                &                           1.061405429d+00 * t * t * t * t * t) * ybox(1)
        a = a + ybox(1) * xbox(1)
        xbox(0) = a / ybox(1)
        ybox(0) = 0.0d+00
        do ibox = 2, nb
            ybox(ibox) = ybox(ibox - 1) + a / xbox(ibox - 1)
            if (ybox(ibox) > 1.0d+00) exit
            xbox(ibox) = sqrt(- 2.0d+00 * log(ybox(ibox)))
        enddo
        xbox(nb) = 0.0

    end subroutine norm_init

    !     draw from the normal distribution function

    real function normal (rseed)

        real(4)  s, a, x, y
        real(8)  rseed
        integer  i

        10 a = rnd(rseed)
        if (a >= 0.5) then                      !  half is negative
            s = 1.0
        else
            s = -1.0
        endif
        a = 2.0d+00 * abs(a - 0.5d+00)
        i = min (int(rnd(rseed) * nb), nb - 1)      !  select the box
        x = a * xbox(i)                               !  in most cases direct hit
        if (x >= xbox(i + 1)) then                !  in some cases refinement
            if (i > 0) then
                y = ybox(i) + rnd(rseed) * (ybox(i + 1) - ybox(i))
                if (y >= exp(-x * x / 2.0)) goto 10
            else                                     !  special treatment of the
                x = -log(rnd(rseed)) / xbox(1)          !  tail (box zero)
                y = -log(rnd(rseed))
                if (2.0 * y <= x * x) goto 10
                x = x + xbox(1)
            endif
        endif

        normal = s * x                                  !  half is negative
        return

    end function normal

end module
