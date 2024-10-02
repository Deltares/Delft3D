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

 subroutine tekflownodes(ja)
    use m_isosmoothflownode
    use m_isocol
    use m_halt
    use m_drcirc
    use m_dhtext
    use m_dhitext
    use m_copyznodtornod
    use unstruc_display
    use m_flowgeom
    use m_flow
    use m_missing
    use m_transport
    use m_drawthis
    use m_pfiller
    use m_inview
    implicit none
    integer :: nodemode, nodewhat
    integer :: k, ja, ja2, nn, ncol
    double precision :: znod, zn

    nodemode = ndraw(19)
    nodewhat = ndraw(28)
    ja = 0

    if (nodemode == 3) then ! interpolate rnod on netnodes based upon znod on flownodes
       call copyznodtornod()
    end if
    do k = 1, ndxi
       if (mod(k, 200) == 0) then
          call halt(ja)
          if (ja == 1) then
             return
          end if
       end if
       if (nodewhat >= 2) then
          ja2 = 1
          if (wetplot > 0d0) then
             if (hs(k) < wetplot) then
                ja2 = 0
             end if
          end if
          if (ja2 == 1 .or. nodewhat == 3) then ! nodewhat==3: always show bottom
             if (inview(xz(k), yz(k))) then
                zn = znod(k)
                if (zn == DMISS) cycle
                if (nodemode == 3 .or. nodemode == 3 + 3) then ! isolines within cell
                   if (k <= ndx2d) then
                      call ISOSMOOTHflownode(k)
                   else
                      call isocol(zn, ncol)
                      nn = size(nd(k)%x)
                      call PFILLER(nd(k)%x, nd(k)%y, nn, NCOL, NCol)
                   end if
                else if (nodemode >= 4 .or. nodemode == 4 + 3) then ! isofil= cellfill
                   call isocol(zn, ncol)
                   if (nodemode == 5 .or. nodemode == 5 + 3) then
                      call drcirc(xz(k), yz(k), zn)
                   else
                      nn = size(nd(k)%x)
                      call PFILLER(nd(k)%x, nd(k)%y, nn, NCOL, NCol)
                   end if
                end if
                ! draw text values:
                if (nodemode == 2 .or. nodemode >= 6) then
                   call isocol(zn, ncol)
                   if (nodewhat == 15 .or. nodewhat == 16 .or. nodewhat == 17 .or. nodewhat == 25) then
                      call DHITEXT(int(zn), xz(k), yz(k))
                   else
                      call dhtext(zn, xz(k), yz(k), bl(k))
                   end if
                end if
             end if
          end if
       end if
    end do
 end subroutine tekflownodes
