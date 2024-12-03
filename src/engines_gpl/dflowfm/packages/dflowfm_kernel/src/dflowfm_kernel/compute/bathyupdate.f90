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

module m_bathyupdate

implicit none

private

public :: bathyupdate

contains

 subroutine bathyupdate()
    use m_flowgeom
    use m_flow
    use m_netw, only: zk, zk1, numk
    use m_sediment !, only : jamorf

    implicit none
    integer :: L, k, kk, kkk, k1, k2, n, nn, ierr, ja, k3, k4
    double precision :: znn, bobm, zki

    if (jamorf == 0) return
    if (stm_included) return ! Done in fm_bott3d

    if (.not. (ibedlevtyp == 1 .or. ibedlevtyp == 6) .and. jaceneqtr == 1 .and. .not. allocated(zn2rn)) then ! netnode depth + netcell fluxes                                                        !

       if (allocated(zk1)) deallocate (zk1)
       allocate (zk1(numk), stat=ierr)
       call aerr('zk1(numk)', ierr, numk); zk1 = 0d0

       ja = 0
       if (.not. allocated(zn2rn)) then
          ja = 1
       else if (size(zn2rn) < numk) then
          deallocate (zn2rn); ja = 1
       end if
       if (ja == 1) then
          allocate (zn2rn(numk), stat=ierr)
          call aerr('zn2rn(numk)', ierr, numk); zn2rn = 0d0
          do n = 1, ndx2d
             nn = size(nd(n)%x)
             do kk = 1, nn
                kkk = nd(n)%nod(kk)
                zn2rn(kkk) = zn2rn(kkk) + ba(n)
             end do
          end do
       end if

    end if

    if (ibedlevtyp == 1 .or. ibedlevtyp == 6) then ! tiledepth types

       bl = bl + blinc

       do L = lnx1D + 1, lnx
          bob(1, L) = max(bl(ln(1, L)), bl(ln(2, L)))
          bob(2, L) = bob(1, L)
       end do

    else ! netnode types

       if (jaceneqtr == 1) then
          zk1 = 0d0
          do n = 1, ndx2d
             znn = blinc(n)
             if (znn /= 0d0) then
                nn = size(nd(n)%x)
                do kk = 1, nn
                   kkk = nd(n)%nod(kk)
                   zk1(kkk) = zk1(kkk) + znn * ba(n)
                end do
             end if
          end do

          do k = 1, numk
             if (zk1(k) /= 0d0) then
                if (zn2rn(k) > 0) then
                   zki = zk1(k) / zn2rn(k) ! increment
                   zk(k) = zk(k) + zki ! new bathy
                end if
             end if
          end do
       else ! update already done in subroutine  transport

       end if

       ! TODO: Herman: should we skip the step below if optional ibedlevmode==BLMODE_D3D?
       bl(1:ndxi) = 1d9

       do L = lnx1D + 1, lnxi
          k3 = lncn(1, L)
          k4 = lncn(2, L)
          bob(1, L) = zk(k3)
          bob(2, L) = zk(k4)
          bobm = min(bob(1, L), bob(2, L))
          k1 = ln(1, L); k2 = ln(2, L)
          ! TODO: Herman: should we skip the step below if optional ibedlevmode==BLMODE_D3D?
          bl(k1) = min(bl(k1), bobm) ! here minimise based on connected lowest linklevels
          bl(k2) = min(bl(k2), bobm)
       end do

    end if

    ! call setaifu() ! or, do this every so many steps

    do k = 1, ndxi
       if (s1(k) < bl(k)) then
          s0(k) = bl(k) + 1d-9
          s1(k) = bl(k) + 1d-9
       end if
    end do

    bob0(:, lnx1D + 1:lnxi) = bob(:, lnx1D + 1:lnxi)

 end subroutine bathyupdate

end module m_bathyupdate
