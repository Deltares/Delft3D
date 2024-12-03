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
 subroutine setlinktocenterweights() ! set center related linkxy weights
    use precision, only: dp

    use m_flow
    use m_netw
    use m_flowgeom
    use m_sferic
    use m_longculverts
    implicit none

    real(kind=dp) :: wud, wuL1, wuL2, cs, sn
    integer :: L, ierr, n, kk, n12, lnxmax
    integer :: k1, k2, LL
    integer :: ilongc, L1dlink

    real(kind=dp) :: aa1, wcw, alf
    real(kind=dp), allocatable :: wwL(:)

    real(kind=dp), allocatable :: wcxy(:, :) ! center weight factors (2,ndx) , only for normalising
    real(kind=dp), allocatable :: wc(:) ! center weight factors (ndx)   , only for normalising

    real(kind=dp), external :: lin2nodx, lin2nody

    wcx1 = 0
    wcy1 = 0
    wcx2 = 0
    wcy2 = 0
    wcL = 0

    if (allocated(wcxy)) deallocate (wcxy)
    allocate (wcxy(2, ndx), stat=ierr); wcxy = 0
    call aerr('wcxy (2,ndx)', ierr, 2 * ndx)
    allocate (wc(ndx), stat=ierr); wc = 0
    call aerr('wc     (ndx)', ierr, ndx)

    do L = 1, lnx

       if (kcu(L) == 3) cycle ! no contribution from 1D2D internal links

       k1 = ln(1, L); k2 = ln(2, L) !left and right node
       wud = wu(L) * dx(L) !flow surface area
!    cs   = csu(L)
!    sn   = snu(L)

       wuL1 = acl(L) * wud ! 2d center factor
       wcL(1, L) = wuL1
       wc(k1) = wc(k1) + wuL1

       wuL2 = (1d0 - acl(L)) * wud
       wcL(2, L) = wuL2
       wc(k2) = wc(k2) + wuL2

       cs = lin2nodx(L, 1, csu(L), snu(L))
       sn = lin2nody(L, 1, csu(L), snu(L))
       wcx1(L) = cs * wuL1
       wcy1(L) = sn * wuL1

       cs = lin2nodx(L, 2, csu(L), snu(L))
       sn = lin2nody(L, 2, csu(L), snu(L))
       wcx2(L) = cs * wuL2
       wcy2(L) = sn * wuL2

       wcxy(1, k1) = wcxy(1, k1) + abs(wcx1(L))
       wcxy(2, k1) = wcxy(2, k1) + abs(wcy1(L))

       wcxy(1, k2) = wcxy(1, k2) + abs(wcx2(L))
       wcxy(2, k2) = wcxy(2, k2) + abs(wcy2(L))
    end do

    if (newculverts) then
       do ilongc = 1, nlongculverts
          L = abs(longculverts(ilongc)%flowlinks(1))
          L1Dlink = abs(longculverts(ilongc)%flowlinks(2))
          if (L > 0 .and. L1Dlink > 0) then
             k1 = ln(1, L); k2 = ln(2, L) !left and right node
             wud = wu(L) * dx(L) !flow surface area
             wuL1 = acl(L) * wud ! 2d center factor
             wcL(1, L) = wuL1
             wuL2 = (1d0 - acl(L)) * wud
             wcL(2, L) = wuL2

             !replace last addition of wcx1 etc.
             wcxy(1, k1) = wcxy(1, k1) - abs(wcx1(L))
             wcxy(2, k1) = wcxy(2, k1) - abs(wcy1(L))

             wcxy(1, k2) = wcxy(1, k2) - abs(wcx2(L))
             wcxy(2, k2) = wcxy(2, k2) - abs(wcy2(L))

             cs = lin2nodx(L1Dlink, 1, csu(L1Dlink), snu(L1Dlink)) !L van buur 1D linkje
             sn = lin2nody(L1Dlink, 1, csu(L1Dlink), snu(L1Dlink)) ! idem
             wcx1(L) = cs * wuL1
             wcy1(L) = sn * wuL1

             cs = lin2nodx(L1Dlink, 2, csu(L1Dlink), snu(L1Dlink)) !L van buur 1D linkje
             sn = lin2nody(L1Dlink, 2, csu(L1Dlink), snu(L1Dlink)) ! idem
             wcx2(L) = cs * wuL2
             wcy2(L) = sn * wuL2

             wcxy(1, k1) = wcxy(1, k1) + abs(wcx1(L))
             wcxy(2, k1) = wcxy(2, k1) + abs(wcy1(L))

             wcxy(1, k2) = wcxy(1, k2) + abs(wcx2(L))
             wcxy(2, k2) = wcxy(2, k2) + abs(wcy2(L))
          end if

          L = abs(longculverts(ilongc)%flowlinks(longculverts(ilongc)%numlinks))
          L1Dlink = abs(longculverts(ilongc)%flowlinks(longculverts(ilongc)%numlinks - 1))
          if (L > 0 .and. L1Dlink > 0) then
             k1 = ln(1, L); k2 = ln(2, L) !left and right node
             wud = wu(L) * dx(L) !flow surface area
             wuL1 = acl(L) * wud ! 2d center factor
             wcL(1, L) = wuL1
             wuL2 = (1d0 - acl(L)) * wud
             wcL(2, L) = wuL2

             !replace last addition of wcx1 etc.
             wcxy(1, k1) = wcxy(1, k1) - abs(wcx1(L))
             wcxy(2, k1) = wcxy(2, k1) - abs(wcy1(L))

             wcxy(1, k2) = wcxy(1, k2) - abs(wcx2(L))
             wcxy(2, k2) = wcxy(2, k2) - abs(wcy2(L))

             cs = lin2nodx(L1Dlink, 1, csu(L1Dlink), snu(L1Dlink)) !L van buur 1D linkje
             sn = lin2nody(L1Dlink, 1, csu(L1Dlink), snu(L1Dlink)) ! idem
             wcx1(L) = cs * wuL1
             wcy1(L) = sn * wuL1

             cs = lin2nodx(L1Dlink, 2, csu(L1Dlink), snu(L1Dlink)) !L van buur 1D linkje
             sn = lin2nody(L1Dlink, 2, csu(L1Dlink), snu(L1Dlink)) ! idem
             wcx2(L) = cs * wuL2
             wcy2(L) = sn * wuL2

             wcxy(1, k1) = wcxy(1, k1) + abs(wcx1(L))
             wcxy(2, k1) = wcxy(2, k1) + abs(wcy1(L))

             wcxy(1, k2) = wcxy(1, k2) + abs(wcx2(L))
             wcxy(2, k2) = wcxy(2, k2) + abs(wcy2(L))
          end if
       end do
    end if

    lnxmax = 0
    do n = 1, mxwalls ! wall contribution to scalar linktocenterweights
       k1 = walls(1, n)
       aa1 = 2d0 * walls(17, n)
       wcw = 0d0
       lnxmax = max(lnxmax, nd(k1)%lnx)
       call realloc(wwL, lnxmax, keepExisting=.false.)
       do kk = 1, size(nd(k1)%ln)
          LL = abs(nd(k1)%ln(kk))
          n12 = 1; alf = acL(LL)
          if (k1 /= ln(1, LL)) then
             n12 = 2; alf = 1d0 - acL(LL)
          end if
          wuL1 = alf * dx(LL) * wu(LL)
          cs = walls(8, n) ! outward positive
          sn = -walls(7, n)
          wwL(kk) = abs(cs * csu(LL) + sn * snu(LL))
          wwL(kk) = wwL(kk) * wuL1
          wcw = wcw + wwL(kk)
       end do
       if (wcw > 0d0) then
          wc(k1) = wc(k1) + aa1
          do kk = 1, size(nd(k1)%ln)
             LL = abs(nd(k1)%ln(kk))
             n12 = 1; alf = acL(LL)
             if (k1 /= ln(1, LL)) then
                n12 = 2; alf = 1d0 - acL(LL)
             end if
             wcL(n12, LL) = wcL(n12, LL) + wwL(kk) * aa1 / wcw
          end do
       end if
    end do

    do L = 1, lnx
       k1 = ln(1, L); k2 = ln(2, L)
       if (abs(kcu(L)) == 2 .or. abs(kcu(L)) == 4) then ! 2D links and 1D2D lateral links
          if (kfs(K1) == 0) then ! kfs temporarily used as cutcell flag, set in cutcelwu
             wcx1(L) = wcx1(L) * bai(k1)
             wcy1(L) = wcy1(L) * bai(k1)
          else
             if (wcxy(1, k1) /= 0) wcx1(L) = wcx1(L) / wcxy(1, k1)
             if (wcxy(2, k1) /= 0) wcy1(L) = wcy1(L) / wcxy(2, k1)
          end if

          if (kfs(K2) == 0) then
             wcx2(L) = wcx2(L) * bai(k2)
             wcy2(L) = wcy2(L) * bai(k2)
          else
             if (wcxy(1, k2) /= 0) wcx2(L) = wcx2(L) / wcxy(1, k2)
             if (wcxy(2, k2) /= 0) wcy2(L) = wcy2(L) / wcxy(2, k2)
          end if
       else
          wcx1(L) = wcx1(L) * bai(k1) !if (wcxy(2,k1) .ne. 0) /wcxy(2,k1)
          wcy1(L) = wcy1(L) * bai(k1) !if (wcxy(1,k1) .ne. 0) /wcxy(1,k1)
          wcx2(L) = wcx2(L) * bai(k2) !if (wcxy(2,k2) .ne. 0) /wcxy(2,k2)
          wcy2(L) = wcy2(L) * bai(k2) !if (wcxy(1,k2) .ne. 0) /wcxy(1,k2)
       end if
       if (wc(k1) > 0d0) wcL(1, L) = wcL(1, L) / wc(k1)
       if (wc(k2) > 0d0) wcL(2, L) = wcL(2, L) / wc(k2)

    end do

    deallocate (wcxy, wc)
    if (allocated(wwL)) deallocate (wwL)

    kfs = 0

 end subroutine setlinktocenterweights
