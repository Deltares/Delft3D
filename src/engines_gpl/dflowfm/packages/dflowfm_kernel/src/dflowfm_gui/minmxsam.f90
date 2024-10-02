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

module m_minmxsam

implicit none

contains

 subroutine minmxsam()

    use m_samples, only: ns, xs, ys, zs
    use m_missing, only: dmiss
    use m_isoscaleunit
    use m_depmax2, only: vmax=>vmax2, vmin=>vmin2, dv=>dv2, val=>val2, nv=>nv2, jaauto=>jaauto2
    use m_paramtext
    use m_inview

    double precision :: rmin, rmax
    character(len=256) :: buffer
    integer :: k, i

    if (jaauto > 0) then
       rmin = 1d30
       rmax = -1d30

       do k = 1, ns
          if (zs(k) == DMISS) cycle
          if (inview(xs(k), ys(k))) then
             if (zs(k) < rmin) then
                rmin = zs(k)
             end if
             if (zs(k) > rmax) then
                rmax = zs(k)
             end if
          end if
       end do
       vmax = rmax
       vmin = rmin
       dv = vmax - vmin
       do i = 1, nv
          val(i) = vmin + (i - 1) * dv / (nv - 1)
       end do
    end if

    !Samples have the same unit of the displayed values
    write (buffer, '(a,a)') 'Samples                              ', UNIT(1)
    call PARAMTEXT(buffer, 2)

 end subroutine minmxsam

 subroutine minmxarc()

    use m_arcinfo
    use m_missing
    use m_isoscaleunit
    use m_depmax2, only: vmax=>vmax2, vmin=>vmin2, dv=>dv2, val=>val2, nv=>nv2, jaauto=>jaauto2
    use m_paramtext
    use m_inview

    implicit none

    double precision :: rmin, rmax, x, y, z
    character(len=256) :: buffer
    integer :: m, n, i

    if (jaauto > 0) then
       rmin = 1d30
       rmax = -1d30

       do n = 1, nca
          do m = 1, mca

             x = x0 + dxa * (m - 1)
             y = y0 + dya * (n - 1)
             z = dble(d(m, n))
             if (inview(x, y) .and. z /= dmiss) then
                if (z < rmin) then
                   rmin = z
                end if
                if (z > rmax) then
                   rmax = z
                end if
             end if
          end do
       end do
       vmax = rmax
       vmin = rmin
       dv = vmax - vmin
       do i = 1, nv
          val(i) = vmin + (i - 1) * dv / (nv - 1)
       end do
    end if

    !Samples have the same unit of the displayed values
    write (buffer, '(a,a)') 'Samples                              ', UNIT(1)
    call PARAMTEXT(buffer, 2)

 end subroutine minmxarc

end module m_minmxsam
