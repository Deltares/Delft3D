!!  Copyright (C)  Stichting Deltares, 2012-2024.
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
module m_srotg
    use m_waq_precision

    implicit none

contains


    subroutine srotg (sa, sb, c, s)
        !
        !     construct givens plane rotation.
        !     jack dongarra, linpack, 3/11/78.
        !                    modified 9/27/86.
        !
        use timers

        real(kind = dp) :: sa, sb, c, s, roe, scale, r, z, one
        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("srotg", ithandl)
        !
        roe = sb
        if(abs(sa) > abs(sb)) roe = sa
        scale = abs(sa) + abs(sb)
        if(scale /= 0.0) go to 10
        c = 1.0
        s = 0.0
        r = 0.0
        go to 20
        10 r = scale * sqrt((sa / scale)**2 + (sb / scale)**2)
        one = 1.0
        r = sign(one, roe) * r
        c = sa / r
        s = sb / r
        20 z = s
        if(abs(c) > 0.0 .and. abs(c) <= s) z = 1.0 / c
        sa = r
        sb = z
        if (timon) call timstop (ithandl)
        return
    end

end module m_srotg
