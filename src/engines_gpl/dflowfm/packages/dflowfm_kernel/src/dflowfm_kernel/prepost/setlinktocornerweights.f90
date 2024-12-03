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

 subroutine setlinktocornerweights() ! set corner related link x- and y weights
    use precision, only: dp

    use m_flow
    use m_netw
    use m_flowgeom
    use geometry_module, only: normalin
    use m_sferic, only: jsferic, jasfer3D
    use m_missing, only: dmiss, dxymis
    use gridoperations

    implicit none

    real(kind=dp) :: ax, ay, wuL, wud, csa, sna
    integer :: k, L, ierr, nx
    integer :: k1, k2, k3, k4
    integer :: ka, kb, LL

    real(kind=dp), allocatable :: wcnxy(:, :) ! corner weight factors (2,numk) , only for normalising
    integer, dimension(:), allocatable :: jacorner ! corner node (1) or not (0), dim(numk)

    real(kind=dp), external :: lin2corx, lin2cory

    if (allocated(wcnx3)) deallocate (wcnx3, wcny3, wcnx4, wcny4)
    if (allocated(wcnxy)) deallocate (wcnxy)
    allocate (wcnx3(lnx), stat=ierr); wcnx3 = 0
    call aerr('wcnx3(lnx) ', ierr, lnx)
    allocate (wcny3(lnx), stat=ierr); wcny3 = 0
    call aerr('wcny3(lnx) ', ierr, lnx)
    allocate (wcnx4(lnx), stat=ierr); wcnx4 = 0
    call aerr('wcnx4(lnx) ', ierr, lnx)
    allocate (wcny4(lnx), stat=ierr); wcny4 = 0
    call aerr('wcny4(lnx) ', ierr, lnx)

    !if (kmx > 0 .and. jased > 0 .and. jased < 4) then
    if (allocated(wcLn)) deallocate (wcLn)
    allocate (wcLn(2, lnx), stat=ierr); wcLn = 0
    call aerr('wcLn(2,lnx)', ierr, lnx)
    !endif

    nx = 0
    do L = lnx1D + 1, lnx
       k3 = lncn(1, L); k4 = lncn(2, L)
       nx = max(nx, k3, k4)
    end do
    allocate (wcnxy(3, numk), stat=ierr); wcnxy = 0
    call aerr('wcnxy(3,numk)', ierr, 3 * numk)

    allocate (jacorner(numk), stat=ierr)
    jacorner = 0
    call aerr('jacorner(numk)', ierr, numk)

    do L = lnx1D + 1, lnx
       if (abs(kcu(L)) == 1) then
          cycle
       end if

       wud = wu(L) * dx(L)
       k3 = lncn(1, L); k4 = lncn(2, L)
       wcnxy(3, k3) = wcnxy(3, k3) + wud
       wcnxy(3, k4) = wcnxy(3, k4) + wud

       wcLn(1, L) = wud
       wcLn(2, L) = wud

!    csa  = max( 1d-6,abs(csu(L)) )
!    sna  = max( 1d-6,abs(snu(L)) )
       csa = max(1d-6, abs(lin2corx(L, 1, csu(L), snu(L))))
       sna = max(1d-6, abs(lin2cory(L, 1, csu(L), snu(L))))

       wuL = acn(1, L) * wud
       if (jacomp == 1) then
          ax = csa * wuL
          ay = sna * wuL
       else
          ax = 0.5d0 * wuL
          ay = ax
       end if
       wcnx3(L) = ax
       wcny3(L) = ay

       wcnxy(1, k3) = wcnxy(1, k3) + ax
       wcnxy(2, k3) = wcnxy(2, k3) + ay
!    wcnxy(1,k3) = wcnxy(1,k3) + lin2corx(L,1,ax,ay)
!    wcnxy(2,k3) = wcnxy(2,k3) + lin2cory(L,1,ax,ay)

!    csa  = max( 1d-6,abs(csu(L)) )
!    sna  = max( 1d-6,abs(snu(L)) )
       csa = max(1d-6, abs(lin2corx(L, 2, csu(L), snu(L))))
       sna = max(1d-6, abs(lin2cory(L, 2, csu(L), snu(L))))

       wuL = acn(2, L) * wud
       if (jacomp == 1) then
          ax = csa * wuL
          ay = sna * wuL
       else
          ax = 0.5d0 * wuL
          ay = ax
       end if
       wcnx4(L) = ax
       wcny4(L) = ay
       wcnxy(1, k4) = wcnxy(1, k4) + ax
       wcnxy(2, k4) = wcnxy(2, k4) + ay
!    wcnxy(1,k4) = wcnxy(1,k4) + lin2corx(L,2,ax,ay)
!    wcnxy(2,k4) = wcnxy(2,k4) + lin2cory(L,2,ax,ay)
    end do

! count number of attached and closed boundary links, and store it temporarily in jacorner
    jacorner = 0
    do L = 1, numL
       if ((kn(3, L) == 2 .and. lnn(L) == 1 .and. lne2ln(L) <= 0)) then
          k1 = kn(1, L)
          k2 = kn(2, L)
          jacorner(k1) = jacorner(k1) + 1
          jacorner(k2) = jacorner(k2) + 1
       end if
    end do

! post-process corner indicator: use ALL boundary nodes, and project on closed boundary later
!   used to be: nmk(k) - int(wcnxy (3,k)) == 2
    do k = 1, numk
       if (jacorner(k) >= 1) then
          jacorner(k) = 1
       else
          jacorner(k) = 0
       end if
    end do

    ! exclude all nodes with a disabled netlink attached from the projection
    do L = 1, numL
       if (kn(3, L) == 0) then
          k1 = kn(1, L)
          k2 = kn(2, L)
          jacorner(k1) = 0
          jacorner(k2) = 0
       end if
    end do

    do L = lnx1D + 1, lnx
       if (abs(kcu(L)) == 1) cycle
       k3 = lncn(1, L); k4 = lncn(2, L)
       if (wcnxy(1, k3) /= 0) wcnx3(L) = wcnx3(L) / wcnxy(1, k3)
       if (wcnxy(2, k3) /= 0) wcny3(L) = wcny3(L) / wcnxy(2, k3)
       if (wcnxy(1, k4) /= 0) wcnx4(L) = wcnx4(L) / wcnxy(1, k4)
       if (wcnxy(2, k4) /= 0) wcny4(L) = wcny4(L) / wcnxy(2, k4)

       if (wcnxy(3, k3) /= 0) wcLn(1, L) = wcLn(1, L) / wcnxy(3, k3)
       if (wcnxy(3, k4) /= 0) wcLn(2, L) = wcLn(2, L) / wcnxy(3, k4)

       if (irov == 2) then ! zero cornervelocities for no-slip

          if (int(wcnxy(3, k3)) /= nmk(k3)) then
             wcnx3(L) = 0d0; wcny3(L) = 0d0
          end if

          if (int(wcnxy(3, k4)) /= nmk(k4)) then
             wcnx4(L) = 0d0; wcny4(L) = 0d0
          end if

       end if

    end do

    nrcnw = 0
    do k = 1, numk ! set up admin for corner velocity alignment at closed walls

!    if ( nmk(k) - int(wcnxy (3,k)) == 2 ) then ! two more netlinks than flowlinks to this corner
       if (jacorner(k) == 1) then
          nrcnw = nrcnw + 1 ! cnw = cornerwall point (netnode)
       end if
    end do

    if (allocated(cscnw)) deallocate (cscnw, sncnw, kcnw, nwalcnw, sfcnw)
    allocate (cscnw(nrcnw), stat=ierr); cscnw = 0
    call aerr('cscnw(nrcnw)', ierr, nrcnw)
    allocate (sncnw(nrcnw), stat=ierr); sncnw = 0
    call aerr('sncnw(nrcnw)', ierr, nrcnw)
    allocate (kcnw(nrcnw), stat=ierr); kcnw = 0
    call aerr(' kcnw(nrcnw)', ierr, nrcnw)
    allocate (nwalcnw(2, nrcnw), stat=ierr); nwalcnw = 0
    call aerr(' nwalcnw(2,nrcnw)', ierr, 2 * nrcnw)
    allocate (sfcnw(nrcnw), stat=ierr); sfcnw = 0
    call aerr(' sfcnw(nrcnw)', ierr, nrcnw)

    nrcnw = 0
    do k = 1, numk ! set up admin for corner velocity alignment at closed walls

!    if ( nmk(k) - int(wcnxy (3,k)) == 2 ) then ! two more netlinks than flowlinks to this corner
       if (jacorner(k) == 1) then
          nrcnw = nrcnw + 1 ! cnw = cornerwall point (netnode)
          kcnw(nrcnw) = k
          ka = 0; kb = 0
          do LL = 1, nmk(k)
             L = nod(k)%lin(LL) ! netstuff
             if (lnn(L) == 1) then
                if (ka == 0) then
                   if (lne2ln(L) <= 0 .and. kn(3, L) /= 0) then ! SPvdP: closed boundaries used in determination of normal vector only
                      call othernode(k, L, ka) ! use other node on closed boundary
                   else
                      ka = k ! use own node on open boundary
                   end if
                else if (kb == 0 .and. kn(3, L) /= 0) then
                   if (lne2ln(L) <= 0) then ! SPvdP: closed boundaries used in determination of normal vector only
                      call othernode(k, L, kb) ! use other node on closed boundary
                   else
                      kb = k ! use own node on closed boundary
                   end if
                end if
             end if
          end do
          if (ka /= 0 .and. kb /= 0 .and. ka /= kb) then ! only for 2D netnodes
             call normalin(xk(ka), yk(ka), xk(kb), yk(kb), csa, sna, xk(k), yk(k), jsferic, jasfer3D, dxymis)
             cscnw(nrcnw) = csa
             sncnw(nrcnw) = sna
          end if

       end if

    end do

    deallocate (wcnxy, acn, jacorner)

 end subroutine setlinktocornerweights
