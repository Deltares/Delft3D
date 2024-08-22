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

module pinpok_mod
    !
    !  module declarations
    !
    !  data definition module(s)
    !
    use m_waq_precision      ! single/double precision
    use timers
    !
    implicit none
    !
contains
    subroutine pinpok(xl, yl, n, x, y, inside)
        !
        !     programmer : frank kleissen
        !     function   : checks whether point (xx,yy) lies inside
        !                  the polygone
        !     date       : may 2004
        !
        real(sp), dimension(:) :: x, y
        !
        !     local scalars
        !
        integer :: i, i1, i2, inside, n, np
        real :: amiss, rechts, rl, rm
        real :: x1, x2, xl, y1, y2, yl
        integer(4) ithndl              ! handle to time this subroutine
        data       ithndl / 0 /
        if (timon) call timstrt("pinpok", ithndl)
        !
        amiss = 999.999
        if (n  <=  2) then
            inside = 1
        else
            np = 0
            5    continue
            np = np + 1
            if (np  <=  n) then
                if (x(np)  /=  amiss) goto 5
            endif
            np = np - 1
            inside = 0
            rechts = 0
            i = 0
            10    continue
            i1 = mod(i, np) + 1
            i2 = mod(i1, np) + 1
            x1 = x(i1)
            x2 = x(i2)
            y1 = y(i1)
            y2 = y(i2)
            if (xl  >=  min(x1, x2) .and. xl  <=  max(x1, x2)) then
                !           tussen of op lijnstuk
                if (xl == x1 .and. yl == y1 .or.   &
                        !               op punt 1
                        (x1 == x2 .and.  &
                                yl  >=  min(y1, y2) .and. yl  <=  max(y1, y2)) .or.  &
                        !              op verticale lijn
                        (yl == y1 .and. y1 == y2)) then
                    !              op horizontale lijn
                    inside = 1
                    goto 99999
                else if (x1  /=  x2) then
                    !              scheve lijn
                    rl = (xl - x1) / (x2 - x1)
                    rm = (y1 - yl) + rl * (y2 - y1)
                    if (rm == 0) then
                        !                 op scheve lijn
                        inside = 1
                        goto 99999
                    else if (rm  >  0.0) then
                        !                 onder scheve lijn
                        if (xl == x1 .or. xl == x2) then
                            if (x1  >  xl .or. x2  >  xl) then
                                rechts = rechts + 1.0
                            endif
                        endif
                        inside = 1 - inside
                    endif
                endif
            endif
            i = i + 1
            if (i  < np) goto 10
            if (mod(rechts, 2.0)  /=  0) inside = 1 - inside
        endif
        99999 if (timon) call timstop (ithndl)
        return
    end subroutine pinpok
end module
