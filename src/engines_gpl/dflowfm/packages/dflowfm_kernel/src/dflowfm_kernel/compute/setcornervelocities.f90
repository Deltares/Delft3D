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

 subroutine setcornervelocities() ! set corner related velocity x- and y components

    use m_flow
    use m_netw
    use m_flowgeom
    use m_sferic
    use m_get_Lbot_Ltop

    implicit none

    integer :: L, k1, k2, k3, k4, k, kk, LL, Lb, Lt, kw
    integer :: m, n
    double precision :: uLx, uLy, csk, snk, sg

    double precision, external :: nod2linx, nod2liny, lin2corx, lin2cory

    ucnx = 0; ucny = 0

    if (kmx == 0) then

       if (jacomp <= 1) then

          do L = lnx1D + 1, lnx
             k1 = ln(1, L); k2 = ln(2, L)
             k3 = lncn(1, L); k4 = lncn(2, L)
             if (jasfer3D == 0) then
                uLx = 0.5d0 * (ucx(k1) + ucx(k2))
                uLy = 0.5d0 * (ucy(k1) + ucy(k2))
             else
                uLx = 0.5d0 * (nod2linx(L, 1, ucx(k1), ucy(k1)) + nod2linx(L, 2, ucx(k2), ucy(k2)))
                uLy = 0.5d0 * (nod2liny(L, 1, ucx(k1), ucy(k1)) + nod2liny(L, 2, ucx(k2), ucy(k2)))
             end if

             ucnx(k3) = ucnx(k3) + uLx * wcnx3(L)
             ucny(k3) = ucny(k3) + uLy * wcny3(L)
             ucnx(k4) = ucnx(k4) + uLx * wcnx4(L)
             ucny(k4) = ucny(k4) + uLy * wcny4(L)
          end do

       else ! use banf instead

          do m = 1, mxban ! bz based on netnodes area
             k = nban(1, m)
             n = nban(2, m)
             ucnx(k) = ucnx(k) + banf(m) * ucx(n)
             ucny(k) = ucny(k) + banf(m) * ucy(n)
          end do
          ucnx = ucnx / ban
          ucny = ucny / ban

       end if

       do kw = 1, nrcnw ! cornervelocities aligned with closed walls
          csk = cscnw(kw)
          snk = sncnw(kw)
          k = kcnw(kw)
          sg = csk * ucnx(k) + snk * ucny(k)
          ucnx(k) = sg * csk
          ucny(k) = sg * snk
       end do

    else

       if (jased > 0 .and. jased < 4) then
          ustbc = 0d0
       end if

       if (jacomp == jacomp) then ! for now in 3D use org method

          do LL = lnx1D + 1, lnx
             if (abs(kcu(LL)) == 2) then
                call getLbotLtop(LL, Lb, Lt)
                do L = Lb, Lt
                   k1 = ln(1, L); k2 = ln(2, L)
                   k3 = lncn(1, L); k4 = lncn(2, L)
                   if (jasfer3D == 0) then
                      uLx = 0.5d0 * (ucx(k1) + ucx(k2))
                      uLy = 0.5d0 * (ucy(k1) + ucy(k2))
                   else
                      uLx = 0.5d0 * (nod2linx(LL, 1, ucx(k1), ucy(k1)) + nod2linx(LL, 2, ucx(k2), ucy(k2)))
                      uLy = 0.5d0 * (nod2liny(LL, 1, ucx(k1), ucy(k1)) + nod2liny(LL, 2, ucx(k2), ucy(k2)))
                   end if

                   ucnx(k3) = ucnx(k3) + uLx * wcnx3(LL)
                   ucny(k3) = ucny(k3) + uLy * wcny3(LL)
                   ucnx(k4) = ucnx(k4) + uLx * wcnx4(LL)
                   ucny(k4) = ucny(k4) + uLy * wcny4(LL)
                   if (L == Lb) then
                      k3 = lncn(1, LL); k4 = lncn(2, LL)
                      ucnx(k3) = ucnx(k3) + uLx * wcnx3(LL)
                      ucny(k3) = ucny(k3) + uLy * wcny3(LL)
                      ucnx(k4) = ucnx(k4) + uLx * wcnx4(LL)
                      ucny(k4) = ucny(k4) + uLy * wcny4(LL)
                      if (jased > 0 .and. jased < 4) then
                         ustbc(k3) = ustbc(k3) + ustb(LL) * wcLn(1, LL)
                         ustbc(k4) = ustbc(k4) + ustb(LL) * wcLn(2, LL)
                      end if
                   end if
                end do
             end if
          end do

       else

       end if

       do kw = 1, nrcnw ! cornervelocities aligned with closed walls
          kk = kcnw(kw)
          csk = cscnw(kw)
          snk = sncnw(kw)
          do k = kbotc(kk), kbotc(kk) + kmxc(kk) - 1
             sg = csk * ucnx(k) + snk * ucny(k)
             ucnx(k) = sg * csk
             ucny(k) = sg * snk
          end do
          sg = csk * ucnx(kk) + snk * ucny(kk)
          ucnx(kk) = sg * csk
          ucny(kk) = sg * snk
       end do

    end if

 end subroutine setcornervelocities

