!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2026.
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

module m_cutcelwu
   use m_darean, only: darean
   use m_store_cutcellmasks, only: store_cutcellmasks
   use m_split_pol, only: split_pol

   implicit none

   private

   public :: cutcelwu

contains

   subroutine CUTCELWU(n12, jamasks, ipoly)
      use precision, only: dp
      use m_crosslinkpoly, only: crosslinkpoly
      use m_find_crossed_links_kdtree2, only: find_crossed_links_kdtree2, ITYPE_NETLINK, BOUNDARY_ALL
      use m_tekpolygon
      use m_netw
      use m_flowgeom
      use kdtree2Factory
      use m_missing, only: dmiss, jins
      use m_cutcells
      use messagehandling, only: LEVEL_INFO, LEVEL_ERROR, mess
      use m_polygon, only: NPL, xpl, ypl, zpl
      use geometry_module, only: dbpinpol, dbdistance
      use m_sferic, only: jsferic, jasfer3D
      use m_flow, only: numlimdt, numlimdt_baorg, baorgfracmin
      use m_flowparameters, only: immersed_boundary
      use precision_basics, only: comparereal
      use m_readyy

      integer, intent(in) :: N12 ! 3: only mask nodes, 4: preparation for cut cells (set kfs), 5: actual cut cells (change wu, nd), 6: dry cells
      integer, intent(in) :: jamasks ! do not use masks (0), store masks (1), use stored masks (2)
      integer, intent(in) :: ipoly ! polygon number for masks

      integer :: ja, KMOD
      integer :: K, K1, K2, L, LL, N, NN, LF, IC, LLU, IN, KL, ninside
      integer, allocatable :: KNP(:)
      integer, allocatable :: ibm_link_cross_count(:)

      real(kind=dp) :: XM, YM, XXC(8), YYC(8), DAREA, DLENGTH, DLENMX
      real(kind=dp), allocatable :: ibm_link_cross_x(:), ibm_link_cross_y(:)

      real(kind=dp), dimension(:), allocatable :: xmL, ymL ! intersection coordinates
      integer, dimension(:), allocatable :: Lmask ! link mask

      real(kind=dp) :: cx, cy, R2search, Area, cof0
      integer :: i, ip1, num, k_start, k_end, numsam, ierror
      integer :: jadelete
      integer :: jakdtree
      integer :: jasplitpol, numpolies, ip

      type(tpoly), dimension(:), allocatable :: pli_loc

      real(kind=dp), parameter :: dtol = 1.0e-8_dp
      real(kind=dp) :: xmin, xmax, ymin, ymax, cell_tol

      jakdtree = 1

      call READYY('CUTCELWU', 0.0_dp)

      IN = -1

!  write(6,"('cutcelwu:', I4)") 1

      if (jamasks == 0 .or. jamasks == 1) then
!    generate mask "kc"

         jasplitpol = 0
         numpolies = 1
         if (NPL > 100) then
            call mess(LEVEL_INFO, 'splitting polygons...')
            call split_pol(2, 2, 100, 100)
            call mess(LEVEL_INFO, 'done')
            jasplitpol = 1
            call pol_to_tpoly(numpolies, pli_loc, keepExisting=.false.)
         end if

         if (n12 >= 4) then ! n12=3: mask nodes
            KC = 0
         end if

         do ip = 1, numpolies
            if (jasplitpol == 1) then
               NPL = 0
               call tpoly_to_pol(pli_loc, iselect=ip)
               in = -1
            end if

            if (jakdtree == 1) then
               !
               ! gravity point of polygon
               !
               Area = 0.0_dp
               cx = 0.0_dp
               cy = 0.0_dp
               num = 0
               do i = 1, NPL
                  ip1 = i + 1
                  if (ip1 > NPL) then
                     ip1 = ip1 - NPL
                  end if

                  if (xpl(ip1) == DMISS) then
                     cycle
                  end if
                  cof0 = xpl(i) * ypl(ip1) - xpl(ip1) * ypl(i)
                  Area = Area + cof0
                  cx = cx + (xpl(i) + xpl(ip1)) * cof0
                  cy = cy + (ypl(i) + ypl(ip1)) * cof0
                  num = num + 1
               end do
               area = area * 0.5_dp

               if (area == 0.0_dp) then
                  cycle
               end if

               cx = cx / area / 6.0_dp
               cy = cy / area / 6.0_dp
               !
               ! find the circumcircle
               !
               R2search = 0.0_dp
               do i = 1, npl - 1
                  R2search = max(R2search, dbdistance(xpl(i), ypl(i), cx, cy, jsferic, jasfer3D, dmiss)**2)
               end do

!           write(6,"('cutcelwu:', I4)") 2

               call make_queryvector_kdtree(treeglob, cx, cy, jsferic)
               numsam = kdtree2_r_count(treeglob%tree, treeglob%qv, R2search)

!           write(6,"('cutcelwu:', I4)") 3

               k_start = 1
               k_end = numsam
               if (numsam > 0) then
                  call realloc_results_kdtree(treeglob, numsam)
                  call kdtree2_n_nearest(treeglob%tree, treeglob%qv, numsam, treeglob%results)
                  call tekpolygon()
!              call qnerror(' ', ' ', ' ')
               end if

!           write(6,"('cutcelwu:', I4)") 4

               do k = k_start, k_end ! LOKAAL BINNEN BUITEN POLYGON, IN REKENGEBIED = 0
                  k1 = treeglob%results(k)%idx
                  if (kc(k1) /= 1) then
                     call DBPINPOL(xk(k1), yk(k1), IN, dmiss, jins, NPL, xpl, ypl, zpl)
                     KC(K1) = IN
                  end if
               end do

!           write(6,"('cutcelwu:', I4)") 5

        !!!
            else
               do K = 1, NUMK ! LOKAAL BINNEN BUITEN POLYGON, IN REKENGEBIED = 0
                  call DBPINPOL(XK(K), YK(K), IN, dmiss, jins, NPL, xpl, ypl, zpl)
                  KC(K) = IN
               end do
            end if

         end do

         if (jasplitpol == 1) then
            call restorepol()
            call dealloc_tpoly(pli_loc)
         end if

      else
!    use stored masks
         kc = 0
         do i = ik(ipoly), ik(ipoly + 1) - 1
            kc(jk(i)) = 1
         end do
      end if

      if (n12 >= 4) then ! 4, 5, or 6
         if (immersed_boundary == 1) then
            allocate (ibm_link_cross_count(numL), ibm_link_cross_x(numL), ibm_link_cross_y(numL))
            call ibm_find_crossed_links(ibm_link_cross_count, ibm_link_cross_x, ibm_link_cross_y, ierror)
            if (ierror /= 0) then
               call mess(LEVEL_ERROR, 'ImmersedBoundary=1 could not find polygon/link intersections.')
               deallocate (ibm_link_cross_count, ibm_link_cross_x, ibm_link_cross_y)
               call READYY('CUTCELWU', -1.0_dp)
               return
            end if
         end if

         allocate (KNP(NUMP))
         KNP = 0

         do N = 1, NUMP
            NN = netcell(N)%N
            !     IF ( NN == 4 ) THEN
            do K = 1, NN
               K1 = NETCELL(N)%NOD(K)
               if (KC(K1) == 1) then
                  KNP(N) = 1
               end if
            end do
            !     ENDIF
         end do

         if (immersed_boundary == 1) then
            do N = 1, NUMP
               if (KNP(N) == 1) then
                  cycle
               end if
               NN = netcell(N)%N
               do LL = 1, NN
                  L = netcell(N)%LIN(LL)
                  if (ibm_link_cross_count(L) > 0) then
                     KNP(N) = 1
                     exit
                  end if
               end do
               if (KNP(N) == 1) then
                  cycle
               end if

               xmin = huge(1.0_dp)
               xmax = -huge(1.0_dp)
               ymin = huge(1.0_dp)
               ymax = -huge(1.0_dp)
               do LL = 1, NN
                  K1 = netcell(N)%nod(LL)
                  xmin = min(xmin, xk(K1))
                  xmax = max(xmax, xk(K1))
                  ymin = min(ymin, yk(K1))
                  ymax = max(ymax, yk(K1))
               end do
               cell_tol = 1.0e-8_dp * min(xmax - xmin, ymax - ymin)
               do i = 1, NPL
                  if (comparereal(xpl(i), dmiss) == 0) then
                     cycle
                  end if
                  if (xpl(i) > xmin + cell_tol .and. xpl(i) < xmax - cell_tol .and. &
                      ypl(i) > ymin + cell_tol .and. ypl(i) < ymax - cell_tol) then
                     KNP(N) = 1
                     exit
                  end if
               end do
            end do
         end if

         KMOD = max(1, NUMP / 100)

         !  write(6,"('cutcelwu:', I4)") 6

         !  if ( jakdtree_cross.eq.1 ) then
         !     call find_intersecting_polysections()
         !  end if

         !  write(6,"('cutcelwu:', I4)") 7

         num = 0 !< number of netlink-polygon intersections

         if (jamasks == 1) then
            allocate (Lmask(numL))
            Lmask = 0
            allocate (xmL(numL))
            allocate (ymL(numL))
         end if

         do N = 1, NUMP

            if (mod(n, KMOD) == 0) then
               call READYY('CUTCELWU', real(n, kind=dp) / real(nump, kind=dp))
            end if

            if (KNP(N) == 1) then ! AT LEAST 1 POINT INSIDE POLYGON, SO CHECK CUTC

               NN = netcell(N)%N
               if (immersed_boundary == 1 .and. n12 == 5) then
                  ninside = 0
                  do LL = 1, NN
                     K1 = netcell(N)%nod(LL)
                     if (KC(K1) == 1) then
                        ninside = ninside + 1
                     end if
                  end do
                  if (ninside == 0) then
                     call mess(LEVEL_ERROR, &
                               'ImmersedBoundary=1: a cut cell has no active grid node; refine the grid or simplify the polygon.')
                     cycle
                  end if
               end if

               IC = 0
               do LL = 1, NN

                  L = netcell(N)%LIN(LL)

                  !IF (LNN (L) <= 1) THEN
                  !   CYCLE
                  !ELSE
                  !   IF (N12 == 5) LF  = LNE2LN(L)
                  !ENDIF

                  if (n12 /= 6) then ! 6: netgeom only
                     !             SPvdP: cell next to net boundary may be cut, and not necessarily at the boundary. So need to include boundary link too
                     Lf = lne2ln(L)
                  else
                     jadelete = 0
                  end if

                  LLU = LL + 1
                  if (LLU > NN) then
                     LLU = 1
                  end if
                  K1 = NETCELL(N)%NOD(LL)
                  K2 = NETCELL(N)%NOD(LLU)

                  if (immersed_boundary == 1) then
                     if (ibm_link_cross_count(L) > 1) then
                        call mess(LEVEL_ERROR, 'ImmersedBoundary=1: multiple polygon crossings on net link ', L)
                        ja = 0
                     else if (jamasks == 0 .or. jamasks == 1) then
                        if (ibm_link_cross_count(L) == 1) then
                           call CROSSLINKPOLY(L, 0, 0, [0], [0], XM, YM, JA)
                           if (JA /= 1) then
                              call mess(LEVEL_ERROR, &
                                        'ImmersedBoundary=1: polygon/link crossing search returned inconsistent results.')
                           end if
                        else
                           ja = 0
                        end if
                     else
                        ja = 0
                        do i = idxL(L), idxL(L + 1) - 1
                           if (pdxL(i) == ipoly) then
                              ja = 1
                              xm = xdxL(i)
                              ym = ydxL(i)
                              exit
                           end if
                        end do
                     end if
                  else if (jamasks == 0 .or. jamasks == 1) then
                     if (kc(kn(1, L)) == 1 .or. kc(kn(2, L)) == 1) then
                        call CROSSLINKPOLY(L, 0, 0, [0], [0], XM, YM, JA)
                     else
                        ja = 0
                     end if

                     !             if ( kc(kn(1,L)).ne.kc(kn(2,L)) .and. ja.eq.0 ) then
                     !                call qnerror('cutcelwu: error', ' ', ' ')
                     !             end if
                  else
                     !             use stored intersections
                     ja = 0
                     do i = idxL(L), idxL(L + 1) - 1
                        if (pdxL(i) == ipoly) then
                           ja = 1
                           xm = xdxL(i)
                           ym = ydxL(i)
                           exit
                        end if
                     end do
                  end if

                  if (JA == 1) then

                     if (jamasks == 1) then
                        !                store intersections with polygon
                        Lmask(L) = 1
                        xmL(L) = xm
                        ymL(L) = ym
                     end if

                     if (N12 == 5) then ! OP DEZE MANIER UITSTEL AANPASSING TOT NA DE WEGINGEN VAN LINK CENTER/CORNER WEIGHTS

                        if (KC(K1) == 1 .and. kc(k2) /= 1) then ! 1 OUTSIDE
                           IC = IC + 1
                           XXC(IC) = XM
                           YYC(IC) = YM
                           IC = IC + 1
                           XXC(IC) = XK(K2)
                           YYC(IC) = YK(K2)
                           if (Lf > 0) then
                              if (wu(LF) /= 0.0_dp) then
                                 WU(LF) = DBDISTANCE(XM, YM, XK(K2), YK(K2), jsferic, jasfer3D, dmiss)
                              end if
                           end if
                        else if (kc(k1) /= 1 .and. kc(k2) == 1) then
                           if (IC == 0) then
                              IC = IC + 1
                              XXC(IC) = XK(K1)
                              YYC(IC) = YK(K1)
                           end if
                           IC = IC + 1
                           XXC(IC) = XM
                           YYC(IC) = YM
                           if (Lf > 0) then
                              if (wu(LF) /= 0.0_dp) then
                                 WU(LF) = DBDISTANCE(XM, YM, XK(K1), YK(K1), jsferic, jasfer3D, dmiss)
                              end if
                           end if
                        else if (kc(k1) == 1 .and. kc(k2) == 1 .and. Lf > 0) then
                           wu(Lf) = 0.0_dp
                        end if
                     else if (N12 == 4) then
                        kfs(n) = 1 ! temporary cutcell flag, TO CHANGE LINKTOCENTER AND LINKTOCORNERSWEIGHTING FOR CUTCELLS
                     else if (n12 == 6) then
                        jadelete = 0
                        if (KC(K1) == 1 .and. kc(k2) /= 1) then ! 1 OUTSIDE
                           if (DBDISTANCE(XM, YM, XK(K2), YK(K2), jsferic, jasfer3D, dmiss) <= dtol) then
                              jadelete = 1
                           end if
                        else if (kc(k1) /= 1 .and. kc(k2) == 1) then
                           if (DBDISTANCE(XM, YM, XK(K1), YK(K1), jsferic, jasfer3D, dmiss) <= dtol) then
                              jadelete = 1
                           end if
                        else if (kc(k1) == 1 .and. kc(k2) == 1) then
                           jadelete = 1
                        end if
                        if (jadelete == 1) then
                           lnn(L) = -abs(lnn(L))
                        end if
                     end if

                  else
                     if (KC(K1) == 0 .and. KC(K2) == 0) then
                        if (N12 == 5) then
                           if (IC == 0) then
                              IC = IC + 1
                              XXC(IC) = XK(K1)
                              YYC(IC) = YK(K1)
                           end if
                           IC = IC + 1
                           XXC(IC) = XK(K2)
                           YYC(IC) = YK(K2)
                        end if
                     else if (N12 == 4) then
                        LNN(L) = 0
                     else if (n12 == 6) then
                        lnn(L) = -abs(lnn(L))
                     end if
                  end if

               end do

               if (N12 == 5 .and. IC > 0) then

                  call dAREAN(XXC, YYC, IC, DAREA, DLENGTH, DLENMX) ! AREA AND LENGTH OF POLYGON
                  if (numlimdt(n) <= numlimdt_baorg) then
                     BA(N) = max(DAREA, Baorgfracmin * ba(n))
                  end if
                  BA(N) = max(BA(N), BAMIN) ! ; BAI(N) = 1D0/BA(N)    ! BAI ZIT IN ADVECTIEWEGING
                  if (ic > 2) then
                     deallocate (ND(N)%X, ND(N)%Y)
                     allocate (ND(N)%X(IC), ND(N)%Y(IC))
                     ND(N)%X(1:IC) = XXC(1:IC)
                     ND(N)%Y(1:IC) = YYC(1:IC)
                  end if

               end if

               if (N12 == -5) then

                  if (IC < 3) then

                     do KL = 1, nd(n)%lnx
                        L = abs(nd(n)%ln(KL))
                        wu(L) = 0.0_dp
                     end do
                     ba(n) = 0.0_dp

                  else

                     call dAREAN(XXC, YYC, IC, DAREA, DLENGTH, DLENMX) ! AREA AND LENGTH OF POLYGON

                     if (DAREA / BA(n) < 0.05_dp) then
                        do KL = 1, nd(n)%lnx
                           L = abs(nd(n)%ln(KL))
                           wu(L) = 0.0_dp
                        end do
                        ba(n) = 0.0_dp
                     else
                        BA(N) = max(DAREA, BAMIN) ! ; BAI(N) = 1D0/BA(N)    ! BAI ZIT IN ADVECTIEWEGING
                        deallocate (ND(N)%X, ND(N)%Y)
                        allocate (ND(N)%X(IC), ND(N)%Y(IC))
                        ND(N)%X(1:IC) = XXC(1:IC)
                        ND(N)%Y(1:IC) = YYC(1:IC)
                     end if

                  end if

               end if
            end if

         end do
      end if

!  write(6,"('cutcelwu:', I4)") 8

!  if ( n12.eq.5 ) then
!!    SPvdP: disable flow-links that are associated to disabled net-links
!     do Lf=1,Lnx
!        L = abs(ln2lne(Lf))
!        if ( L.gt.0 ) then
!           if ( lnn(L).eq.0 ) then
!              wu(Lf) = 0d0
!           end if
!        end if
!     end do
!  end if

      if (jamasks == 1) then
!    store mask
         call store_cutcellmasks(numk, kc, numL, Lmask, xmL, ymL)
      end if

!  write(6,"('cutcelwu:', I4)") 9

      if (allocated(knp)) then
         deallocate (knp)
      end if
      if (allocated(ibm_link_cross_count)) then
         deallocate (ibm_link_cross_count, ibm_link_cross_x, ibm_link_cross_y)
      end if

      if (jamasks == 1) then
         if (allocated(Lmask)) then
            deallocate (Lmask)
         end if
         if (allocated(xmL)) then
            deallocate (xmL)
         end if
         if (allocated(ymL)) then
            deallocate (ymL)
         end if
      end if

      call READYY('CUTCELWU', -1.0_dp)

   contains

      subroutine ibm_find_crossed_links(crossing_count, crossing_x, crossing_y, ierror_out)
         integer, dimension(:), intent(out) :: crossing_count !< Distinct polygon intersections per net link.
         real(kind=dp), dimension(:), intent(out) :: crossing_x !< First polygon intersection x coordinate per link.
         real(kind=dp), dimension(:), intent(out) :: crossing_y !< First polygon intersection y coordinate per link.
         integer, intent(out) :: ierror_out !< Non-zero if the intersection search failed.

         integer, allocatable :: crossed_links(:), polygon_nodes(:)
         real(kind=dp), allocatable :: segment_weights(:), polygon_x(:), polygon_y(:)
         integer :: intersection_count, i, link, polygon_node, capacity
         real(kind=dp) :: intersection_x, intersection_y
         type(kdtree_instance) :: crossing_tree

         crossing_count = 0
         crossing_x = 0.0_dp
         crossing_y = 0.0_dp
         ierror_out = 0
         if (NPL < 2) then
            return
         end if

         capacity = max(1, numL)
         allocate (crossed_links(capacity), polygon_nodes(capacity), segment_weights(capacity))
         allocate (polygon_x(NPL + 1), polygon_y(NPL + 1))
         polygon_x(1:NPL) = xpl(1:NPL)
         polygon_y(1:NPL) = ypl(1:NPL)
         polygon_x(NPL + 1) = xpl(1)
         polygon_y(NPL + 1) = ypl(1)
         call find_crossed_links_kdtree2(crossing_tree, NPL + 1, polygon_x, polygon_y, ITYPE_NETLINK, capacity, &
                                         BOUNDARY_ALL, intersection_count, crossed_links, polygon_nodes, segment_weights, ierror_out)
         if (ierror_out == 0) then
            do i = 1, min(intersection_count, capacity)
               link = crossed_links(i)
               polygon_node = polygon_nodes(i)
               if (link < 1 .or. link > size(crossing_count) .or. polygon_node < 1 .or. polygon_node > NPL) then
                  cycle
               end if
               intersection_x = polygon_x(polygon_node) + segment_weights(i) * &
                                (polygon_x(polygon_node + 1) - polygon_x(polygon_node))
               intersection_y = polygon_y(polygon_node) + segment_weights(i) * &
                                (polygon_y(polygon_node + 1) - polygon_y(polygon_node))
               if (crossing_count(link) == 0) then
                  crossing_count(link) = 1
                  crossing_x(link) = intersection_x
                  crossing_y(link) = intersection_y
               else if (dbdistance(intersection_x, intersection_y, crossing_x(link), crossing_y(link), &
                                   jsferic, jasfer3D, dmiss) > dtol) then
                  crossing_count(link) = crossing_count(link) + 1
               end if
            end do
         end if
      end subroutine ibm_find_crossed_links

   end subroutine CUTCELwu

end module m_cutcelwu
