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

    !> Renumber flow nodes in an early stage.
    !! Called only from withing flow_geominit, it operates on lne, kn and netcell
    !! data. The actual construction of all flow_geom data remains exactly the
    !! same in flow_geominit. Two steps are taken:
    !!    * renumber (=reorder) netcell by RCM reordering with lne links as input.
    !!    * reorder lne links based on the new netcell numbers.
    !!      The kn netlinks are reorder exactly the same as lne. They have to
    !!      be ordered identically.
    !!    * Also update the (net-)link numbers in %lin
    !! Note: Only 2D cells/links are renumbered (so blocks 1:numl1D, and
    !! numl1D+1:numl remain intact). Also: boundary links are ignored.
    subroutine renumberFlowNodes()
       use network_data
       use m_flowgeom
       use unstruc_messages
       use m_alloc
       use m_partitioninfo, only: idomain, iglobal_s, my_rank
       use m_readyy
       use m_rcm
       
       implicit none

       integer, allocatable :: adj_row(:)
       integer, allocatable :: adj(:)
       integer, allocatable :: perm(:), perm_inv(:), perm_lnk(:), perm_inv_lnk(:), idomain1(:), iglobal_s1(:)
       integer, allocatable :: i1(:)
       double precision, allocatable :: xz1(:), yz1(:)
       type(tface), allocatable :: tface1(:)

       integer, allocatable :: adj_tmp(:, :), adj_tmp2(:)

       integer :: numltot, ii, jj, i, j, indx, isgn, k, kk, k1, k2, km, L, LL, p, p1, bw, bwrn, sumdiff, sumdiffrn

       call readyy('Renumber flow nodes', 0d0)

       jaFlowNetChanged = 1
       numltot = 2 * numl ! Undirected links: 2 adjacency elements per link.

       allocate (adj_row(NUMP + 1), adj(numltot), &
                 perm(NUMP), perm_inv(NUMP), perm_lnk(numl), perm_inv_lnk(numl), &
                 i1(NUML), xz1(NUMP), yz1(NUMP), tface1(NUMP))
       allocate (adj_tmp(20, NUMP))
       allocate (adj_tmp2(NUMP))

       ! Build adjacency list by hand: fill 2D array adj_tmp(20, numk) and
       ! flatten it afterwards to adj(numltot).
       ! Built-in adj_set_ij is VERY slow, don't use it.
       adj_tmp2 = 0
       do L = 1, NUML
          k1 = abs(lne(1, L))
          k2 = abs(lne(2, L))
          if (k1 > nump .or. k2 > nump .or. k1 == 0 .or. k2 == 0) then
             cycle ! Don't use 1D links for now.
          end if
          km = adj_tmp2(k1)
          ! For node k1, store its neighbours in sorted order.
          do k = 1, km
             if (adj_tmp(k, k1) > k2) then
                exit
             end if
          end do
          adj_tmp(k + 1:km + 1, k1) = adj_tmp(k:km, k1)
          adj_tmp(k, k1) = k2
          adj_tmp2(k1) = km + 1

          ! Same for node k2
          km = adj_tmp2(k2)
          do k = 1, km
             if (adj_tmp(k, k2) > k1) then
                exit
             end if
          end do
          adj_tmp(k + 1:km + 1, k2) = adj_tmp(k:km, k2)
          adj_tmp(k, k2) = k1
          adj_tmp2(k2) = km + 1
       end do
       call readyy('Renumber flow nodes', .15d0)

       ! Flatten the adjacency list.
       j = 1
       do k = 1, NUMP
          adj_row(k) = j
          do kk = 1, adj_tmp2(k)
             adj(j) = adj_tmp(kk, k)
             j = j + 1
          end do
       end do
       adj_row(NUMP + 1) = j

       call readyy('Renumber flow nodes', .18d0)

       bw = adj_bandwidth(NUMP, numltot, adj_row, adj)

       ! Find a renumbering (permutation)
       call genrcm(NUMP, numltot, adj_row, adj, perm)
       call perm_inverse3(NUMP, perm, perm_inv)
       call readyy('Renumber flow nodes', .35d0)

       ! Now apply the permutation to relevant net cell numbers
       ! (This will propagate automatically to all flow node related arrays in flow_geominit)
       ! Only xz and yz are already available and should be permuted now.
       tface1(1:NUMP) = netcell(1:NUMP)
       xz1(1:NUMP) = xz(1:NUMP)
       yz1(1:NUMP) = yz(1:NUMP)
       do k = 1, NUMP
          netcell(k) = tface1(perm(k))
          xz(k) = xz1(perm(k))
          yz(k) = yz1(perm(k))
       end do

       xz1(1:NUMP) = xzw(1:NUMP)
       yz1(1:NUMP) = yzw(1:NUMP)
       do k = 1, NUMP
          xzw(k) = xz1(perm(k))
          yzw(k) = yz1(perm(k))
       end do

       xz1(1:NUMP) = ba(1:NUMP)
       do k = 1, NUMP
          ba(k) = xz1(perm(k))
       end do

!   see if we can update idomain
       if (allocated(idomain)) then
          if (ubound(idomain, 1) >= nump) then
             call realloc(idomain1, nump, keepExisting=.false.)
             idomain1(1:nump) = idomain(1:nump)

             do k = 1, nump
                idomain(k) = idomain1(perm(k))
             end do

             deallocate (idomain1)
          end if
       end if

!   see if we can update iglobal
       if (allocated(iglobal_s)) then
          if (ubound(iglobal_s, 1) >= nump) then
             call realloc(iglobal_s1, nump, keepExisting=.false., fill=0)
             iglobal_s1(1:nump) = iglobal_s(1:nump)

             do k = 1, nump
                iglobal_s(k) = iglobal_s1(perm(k))
             end do

             deallocate (iglobal_s1)
          end if
       end if

       !i1(1:NUMP)   = lc(1:NUMP)
       !do k=1,NUMP
       !    LC(k) = i1( perm(k) )
       !end do

       sumdiff = 0
       do L = 1, NUML
          sumdiff = sumdiff + abs(abs(lne(1, L)) - abs(lne(2, L)))
       end do

       i1 = lne(1, 1:NUML)
       do L = 1, NUML
          if (abs(i1(L)) > nump .or. i1(L) == 0) cycle
          lne(1, L) = sign(1, i1(L)) * perm_inv(abs(i1(L)))
       end do
       i1 = lne(2, 1:NUML)
       do L = 1, NUML
          if (abs(i1(L)) > nump .or. i1(L) == 0) cycle
          lne(2, L) = sign(1, i1(L)) * perm_inv(abs(i1(L)))
       end do

       ! Finish with some stats
       sumdiffrn = 0
       do L = 1, NUML
          sumdiffrn = sumdiffrn + abs(abs(lne(1, L)) - abs(lne(2, L)))
       end do

       call readyy('Renumber flow nodes', 0.5d0)
       bwrn = adj_perm_bandwidth(NUMP, numltot, adj_row, adj, perm, perm_inv)

       write (msgbuf, *) 'Renumber flow nodes...'
       call dbg_flush()
       write (msgbuf, *) 'Sumdiff:', sumdiff, '  Sumdiff renum: ', sumdiffrn
       call dbg_flush()
       write (msgbuf, *) 'Bandwidth:', bw, '  Bandwidth renum: ', bwrn
       call dbg_flush()

!    return

       ! STEP 2: Reorder flow links (to follow the renumbered flow nodes, when possible)
       ! Only for: NUML1D+1:NUML
       indx = 0

       ! Make identity permutation first.
       do L = 1, NUML
          perm_lnk(L) = L
       end do

       ! Now build the permutation list, based on sort order of lne.
       ! Only after this 'postponed sorting', the lne list itself will be permuted.
       do
          if (numl - numl1D <= 1) exit
          call sort_heap_external(numl - numl1D, indx, i, j, isgn)
          ii = i + numl1D
          jj = j + numl1D
          if (indx > 0) then
             ! * interchange items I and J;
             ! * call again.
             p1 = perm_lnk(ii)
             perm_lnk(ii) = perm_lnk(jj)
             perm_lnk(jj) = p1
          else if (indx < 0) then
             ! * compare items I and J;
             ! * set ISGN = -1 if I < J, ISGN = +1 if J < I;
             ! * call again.
             if (abs(lne(1, perm_lnk(ii))) < abs(lne(1, perm_lnk(jj))) .or. &
                 abs(lne(1, perm_lnk(ii))) == abs(lne(1, perm_lnk(jj))) .and. &
                 abs(lne(2, perm_lnk(ii))) < abs(lne(2, perm_lnk(jj)))) then
                isgn = -1
             else
                isgn = 1
             end if
          else
             ! * equal to 0, the sorting is done.
             exit
          end if
       end do

       ! Also determine inverse permutation (needed for renumbering %lin)
       do L = 1, numl
          perm_inv_lnk(perm_lnk(L)) = L
       end do

       call readyy('Renumber flow nodes', 0.7d0)

       ! Now perm_lnk contains the desired reordering of link numbers.
       ! Permute kn and lne
       i1 = kn(1, 1:NUML)
       do L = numl1d + 1, numl
          kn(1, L) = i1(perm_lnk(L))
       end do
       i1 = kn(2, 1:NUML)
       do L = numl1d + 1, numl
          kn(2, L) = i1(perm_lnk(L))
       end do
       i1 = kn(3, 1:NUML)
       do L = numl1d + 1, numl
          kn(3, L) = i1(perm_lnk(L))
       end do

       !i1 = LC(1:NUML)
       !do L=1,NUML                ! hk: we need LC because of small piece of 1D2D admin in LC
       !    LC(L) = i1(perm_lnk(L))
       !end do

       i1 = lne(1, 1:NUML)
       do L = numl1d + 1, numl
          lne(1, L) = i1(perm_lnk(L))
       end do
       i1 = lne(2, 1:NUML)
       do L = numl1d + 1, numl
          lne(2, L) = i1(perm_lnk(L))
       end do
       i1 = lnn(1:NUML)
       do L = numl1d + 1, numl
          lnn(L) = i1(perm_lnk(L))
       end do

       ! Renumber in the %lin
       do k = 1, numk
          do LL = 1, NMK(K)
             NOD(K)%LIN(LL) = perm_inv_lnk(NOD(K)%LIN(LL))
          end do
       end do

       do p = 1, nump
          do L = 1, netcell(p)%n
             netcell(p)%lin(L) = perm_inv_lnk(netcell(p)%lin(L))
          end do
       end do

       call renumber_cutcellmasks(perm_lnk)

!   permute cutcell related arrays
!    call permute_cutcellmasks(iperm_lnk)

       call readyy('Renumber flow nodes', 1d0)

       deallocate (adj_row)
       deallocate (adj)
       deallocate (perm)
       deallocate (perm_inv)
       deallocate (perm_lnk)
       deallocate (perm_inv_lnk)
       deallocate (i1)
       deallocate (xz1)
       deallocate (yz1)
       deallocate (tface1)
       deallocate (adj_tmp)
       deallocate (adj_tmp2)

       call readyy('Renumber flow nodes', -1d0)

!   DO NOT CALL FINDCELLS FROM NOW ON, IT WILL DESTROY THE RENUMBERING
       netstat = NETSTAT_OK

    end subroutine renumberFlowNodes
