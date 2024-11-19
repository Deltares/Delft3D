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

module m_minmxnds

implicit none

contains

 subroutine minmxnds()
    use unstruc_display_data ! bepaal minimum en maximum van znod in viewing area
    use m_flowgeom, only: ndx, xz, yz
    use m_flow, only: ndmin, ndmax, hs
    use m_missing, only: dmiss
    use m_depmax
    use m_drawthis
    use m_inview
    use m_znod
    
    integer :: i
    double precision :: rmin, rmax
    double precision :: zn
    integer :: n, ja2

    if (jaauto > 0) then
       rmin = 1d30; ndmin = 0
       rmax = -1d30; ndmax = 0

       do n = 1, ndx
          ja2 = 1
          if (wetplot > 0d0) then
             if (hs(n) < wetplot) then
                ja2 = 0
             end if
          end if
          if (ja2 == 1 .or. ndraw(28) == 3) then ! crash
             if (inview(xz(n), yz(n))) then
                zn = znod(n)
                if (zn == DMISS) cycle
                if (zn < rmin) then
                   rmin = zn; ndmin = n
                end if
                if (zn > rmax) then
                   rmax = zn; ndmax = n
                end if
             end if
          end if
       end do
       vmax = rmax
       vmin = rmin
    end if

    dv = vmax - vmin
    do i = 1, nv
       val(i) = vmin + (i - 1) * dv / (nv - 1)
    end do

 end subroutine minmxnds

end module m_minmxnds
