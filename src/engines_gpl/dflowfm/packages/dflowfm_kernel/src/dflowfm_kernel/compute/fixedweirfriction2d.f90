!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2024.
!
!  This file is part of Delft3D (D-Flow Flexible Mesh component).
!
!  Delft3D is free software: you can redistribute it and/or modify
!  it under the terms of the GNU Affero General Public License as
!  published by the Free Software Foundation version 3.
!
!  Delft3D  is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU Affero General Public License for more details.
!
!  You should have received a copy of the GNU Affero General Public License
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D",
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------

!
!

 subroutine fixedweirfriction2D(L, k1, k2, frL) ! frL of fixed weir
    use m_flowgeom
    use m_flow
    use m_missing

    implicit none

    integer :: L
    double precision :: frL

    integer :: k1, k2
    double precision :: umod, uin, frLk1, frLk2, ucxk, ucyk, Cz, weirheight, weirlength, flatlength, a, ff

    if (frcu(L) == 0 .or. hu(L) < epshu) then
       frL = 0d0; return
    end if

    if (fixedweirtopfrictcoef /= dmiss) then ! standard friction on weirtop only
       call getcz(hu(L), fixedweirtopfrictcoef, ifrcutp(L), Cz, L)
    else
       call getcz(hu(L), frcu(L), ifrcutp(L), Cz, L)
    end if

    umod = sqrt(u1(L) * u1(L) + v(L) * v(L))
    frL = umod * ag / (Cz * Cz * hu(L)) ! on top of weir
    frLk1 = frL ! on side 1
    frLk2 = frL ! on side 2

    weirheight = max(0d0, 0.5d0 * (bob(1, L) + bob(2, L)) - 0.5d0 * (bl(k1) + bl(k2)))
    weirlength = fixedweirtopwidth
    flatlength = max(weirlength, dx(L) - (weirlength + 2d0 * weirheight * fixedweirtalud))
    a = weirlength / (weirlength + flatlength)

    if (ifxedweirfrictscheme == 1) then ! simple bedlevel&velocity
       ! assumption + direct linearisation
       if (hs(k1) > 0d0) then
          ff = min(1d0, hu(L) / hs(k1))
          umod = sqrt(u1(L) * u1(L) * ff * ff + v(L) * v(L))
          call getcz(hs(k1), frcu(L), ifrcutp(L), Cz, L)
          frLk1 = umod * ff * ag / (Cz * Cz * hs(k1))
       end if

       if (hs(k2) > 0d0) then
          ff = min(1d0, hu(L) / hs(k2))
          umod = sqrt(u1(L) * u1(L) * ff * ff + v(L) * v(L))
          call getcz(hs(k2), frcu(L), ifrcutp(L), Cz, L)
          frLk2 = umod * ff * ag / (Cz * Cz * hs(k2))
       end if

       frL = a * frL + (1d0 - a) * ((frLk1 + frLk2) * 0.5d0)

    else if (ifxedweirfrictscheme == 2) then ! Without weir like WAQUA

       if (hs(k1) > 0d0) then
          ff = min(1d0, hu(L) / hs(k1))
          umod = sqrt(u1(L) * u1(L) * ff * ff + v(L) * v(L))
          call getcz(hs(k1), frcu(L), ifrcutp(L), Cz, L)
          frLk1 = umod * ff * ag / (Cz * Cz * hs(k1))
       end if

       if (hs(k2) > 0d0) then
          ff = min(1d0, hu(L) / hs(k2))
          umod = sqrt(u1(L) * u1(L) * ff * ff + v(L) * v(L))
          call getcz(hs(k2), frcu(L), ifrcutp(L), Cz, L)
          frLk2 = umod * ff * ag / (Cz * Cz * hs(k2))
       end if

       frL = (frLk1 + frLk2) * 0.5d0

    else if (ifxedweirfrictscheme == 3) then ! full undisturbed velocity reconstruction

       if (abs(u1(L)) > 0.1d0) then

          if (hs(k1) > 0d0) then
             call getucxucynoweirs(k1, ucxk, ucyk)
             umod = sqrt(ucxk * ucxk + ucyk * ucyk)
             uin = abs(ucxk * csu(L) + ucyk * snu(L))
             call getcz(hs(k1), frcu(L), ifrcutp(L), Cz, L)
             frLk1 = umod * uin * ag / (Cz * Cz * hs(k1) * u1(L))
          end if

          if (hs(k2) > 0d0) then
             call getucxucynoweirs(k2, ucxk, ucyk)
             umod = sqrt(ucxk * ucxk + ucyk * ucyk)
             uin = abs(ucxk * csu(L) + ucyk * snu(L))
             call getcz(hs(k2), frcu(L), ifrcutp(L), Cz, L)
             frLk2 = umod * uin * ag / (Cz * Cz * hs(k2) * u1(L))
          end if

       end if

       frL = a * frL + (1d0 - a) * ((frLk1 + frLk2) * 0.5d0)

    else if (ifxedweirfrictscheme == 4) then ! full undisturbed velocity reconstruction

       flatlength = max(weirlength, dx(L) - (weirlength + weirheight * fixedweirtalud))
       a = weirlength / (weirlength + flatlength)

       if (hs(k1) > 0d0) then
          ff = min(1d0, hu(L) / hs(k1))
          umod = sqrt(u1(L) * u1(L) * ff * ff + v(L) * v(L))
          call getcz(hs(k1), frcu(L), ifrcutp(L), Cz, L)
          frLk1 = umod * ff * ag / (Cz * Cz * hs(k1))
       end if

       if (hs(k2) > 0d0) then
          ff = min(1d0, hu(L) / hs(k2))
          umod = sqrt(u1(L) * u1(L) * ff * ff + v(L) * v(L))
          call getcz(hs(k2), frcu(L), ifrcutp(L), Cz, L)
          frLk2 = umod * ff * ag / (Cz * Cz * hs(k2))
       end if

       frL = a * frL + (1d0 - a) * ((frLk1 + frLk2) * 0.5d0)

    end if

 end subroutine fixedweirfriction2D
