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

   integer, parameter :: ibm_union_disconnected = -1

   public :: cutcelwu, cutcelwu_ibm_union

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
      use m_immersed_boundary, only: ibm_state, ibm_wet_area_from_polygon
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
      real(kind=dp) :: cell_corner_x(4), cell_corner_y(4)
      real(kind=dp), allocatable :: ibm_link_cross_x(:), ibm_link_cross_y(:)
      real(kind=dp) :: snap_tol, solid_area, expected_wet_area, full_area, area_tolerance
      logical :: ibm_area_valid

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
         if (NPL > 100 .and. immersed_boundary /= 1) then
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
            snap_tol = 1.0e-8_dp
            if (ibm_state%grid%dx > 0.0_dp .and. ibm_state%grid%dy > 0.0_dp) then
               snap_tol = 1.0e-8_dp * min(ibm_state%grid%dx, ibm_state%grid%dy)
            end if
            call ibm_snap_nodes_to_polygon(snap_tol)
            allocate (ibm_link_cross_count(numL), ibm_link_cross_x(numL), ibm_link_cross_y(numL))
            call ibm_find_crossed_links(ibm_link_cross_count, ibm_link_cross_x, ibm_link_cross_y, snap_tol, ierror)
            if (ierror /= 0) then
               call mess(LEVEL_ERROR, 'ImmersedBoundary=1 could not find polygon/link intersections.')
               deallocate (ibm_link_cross_count, ibm_link_cross_x, ibm_link_cross_y)
               call READYY('CUTCELWU', -1.0_dp)
               return
            end if
         end if

         allocate (KNP(NUMP))
         KNP = 0

         if (immersed_boundary == 1) then
            do N = 1, NUMP
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
               if (KNP(N) == 0 .and. n12 == 5) then
                  ninside = 0
                  do LL = 1, NN
                     K1 = netcell(N)%nod(LL)
                     if (KC(K1) == 1) then
                        ninside = ninside + 1
                     end if
                  end do
                  if (ninside == NN) then
                     KNP(N) = 1
                  end if
               end if
            end do
         else
            do N = 1, NUMP
               NN = netcell(N)%N
               do K = 1, NN
                  K1 = NETCELL(N)%NOD(K)
                  if (KC(K1) == 1) then
                     KNP(N) = 1
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
                               'ImmersedBoundary=1: polygon is enclosed by flow cell ', N)
                     call READYY('CUTCELWU', -1.0_dp)
                     return
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
                     else
                        ja = 0
                        if (ibm_link_cross_count(L) == 1) then
                           ja = 1
                           xm = ibm_link_cross_x(L)
                           ym = ibm_link_cross_y(L)
                        end if
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

               if (N12 == 5 .and. immersed_boundary == 1) then
                  if (NN /= 4 .or. .not. allocated(ibm_state%ba_full) .or. N > size(ibm_state%ba_full)) then
                     call mess(LEVEL_ERROR, 'ImmersedBoundary=1: full Cartesian cell geometry is unavailable for cell ', N)
                     call READYY('CUTCELWU', -1.0_dp)
                     return
                  end if
                  do LL = 1, 4
                     K1 = netcell(N)%nod(LL)
                     cell_corner_x(LL) = xk(K1)
                     cell_corner_y(LL) = yk(K1)
                  end do
                  call ibm_wet_area_from_polygon(cell_corner_x, cell_corner_y, xpl(1:NPL), ypl(1:NPL), &
                                                 snap_tol, solid_area, ibm_area_valid)
                  if (.not. ibm_area_valid) then
                     call mess(LEVEL_ERROR, 'ImmersedBoundary=1: cannot independently clip polygon in flow cell ', N)
                     call READYY('CUTCELWU', -1.0_dp)
                     return
                  end if
                  full_area = ibm_state%ba_full(N)
                  area_tolerance = 1.0e-10_dp * full_area
                  if (solid_area > full_area + area_tolerance) then
                     call mess(LEVEL_ERROR, 'ImmersedBoundary=1: polygon area exceeds full area of flow cell ', N)
                     call READYY('CUTCELWU', -1.0_dp)
                     return
                  end if
                  expected_wet_area = max(0.0_dp, min(full_area, full_area - solid_area))
                  if (expected_wet_area <= area_tolerance) then
                     BA(N) = 0.0_dp
                     BAI(N) = 0.0_dp
                     do KL = 1, NN
                        L = abs(netcell(N)%lin(KL))
                        Lf = lne2ln(L)
                        if (Lf > 0) then
                           WU(Lf) = 0.0_dp
                        end if
                     end do
                     if (allocated(ND(N)%X)) then
                        deallocate (ND(N)%X)
                     end if
                     if (allocated(ND(N)%Y)) then
                        deallocate (ND(N)%Y)
                     end if
                     allocate (ND(N)%X(0), ND(N)%Y(0))
                     cycle
                  end if
                  if (expected_wet_area >= full_area - area_tolerance) then
                     cycle
                  end if
                  if (IC < 3) then
                     call mess(LEVEL_ERROR, 'ImmersedBoundary=1: unsupported disconnected cut geometry in flow cell ', N)
                     call READYY('CUTCELWU', -1.0_dp)
                     return
                  end if

                  call dAREAN(XXC, YYC, IC, DAREA, DLENGTH, DLENMX)
                  if (abs(DAREA - expected_wet_area) > area_tolerance) then
                     call mess(LEVEL_ERROR, 'ImmersedBoundary=1: cutcelwu area disagrees with polygon clipping in flow cell ', N)
                     call READYY('CUTCELWU', -1.0_dp)
                     return
                  end if
                  BA(N) = max(DAREA, Baorgfracmin * full_area)
                  BA(N) = max(BA(N), BAMIN)
                  deallocate (ND(N)%X, ND(N)%Y)
                  allocate (ND(N)%X(IC), ND(N)%Y(IC))
                  ND(N)%X(1:IC) = XXC(1:IC)
                  ND(N)%Y(1:IC) = YYC(1:IC)
               else if (N12 == 5 .and. IC > 0) then

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

      subroutine ibm_snap_nodes_to_polygon(tolerance)
         real(kind=dp), intent(in) :: tolerance

         integer :: node, first, last, segment
         real(kind=dp) :: x1, y1, x2, y2, fraction, denominator, distance

         do node = 1, numk
            if (kc(node) == 1) then
               cycle
            end if
            first = 1
            do while (first <= NPL)
               do while (first <= NPL)
                  if (comparereal(xpl(first), dmiss) /= 0 .and. comparereal(ypl(first), dmiss) /= 0) then
                     exit
                  end if
                  first = first + 1
               end do
               if (first > NPL) then
                  exit
               end if

               last = first
               do while (last < NPL)
                  if (comparereal(xpl(last + 1), dmiss) == 0 .or. comparereal(ypl(last + 1), dmiss) == 0) then
                     exit
                  end if
                  last = last + 1
               end do

               if (last - first + 1 >= 2) then
                  do segment = first, last
                     x1 = xpl(segment)
                     y1 = ypl(segment)
                     if (segment == last) then
                        x2 = xpl(first)
                        y2 = ypl(first)
                     else
                        x2 = xpl(segment + 1)
                        y2 = ypl(segment + 1)
                     end if
                     denominator = (x2 - x1)**2 + (y2 - y1)**2
                     if (denominator <= tiny(1.0_dp)) then
                        fraction = 0.0_dp
                     else
                        fraction = max(0.0_dp, min(1.0_dp, &
                                                   ((xk(node) - x1) * (x2 - x1) + &
                                                    (yk(node) - y1) * (y2 - y1)) / denominator))
                     end if
                     distance = sqrt((xk(node) - (x1 + fraction * (x2 - x1)))**2 + &
                                     (yk(node) - (y1 + fraction * (y2 - y1)))**2)
                     if (distance <= tolerance) then
                        kc(node) = 1
                        exit
                     end if
                  end do
               end if
               if (kc(node) == 1) then
                  exit
               end if
               first = last + 1
            end do
         end do
      end subroutine ibm_snap_nodes_to_polygon

      subroutine ibm_find_crossed_links(crossing_count, crossing_x, crossing_y, tolerance, ierror_out)
         integer, dimension(:), intent(out) :: crossing_count !< Distinct polygon intersections per net link.
         real(kind=dp), dimension(:), intent(out) :: crossing_x !< First polygon intersection x coordinate per link.
         real(kind=dp), dimension(:), intent(out) :: crossing_y !< First polygon intersection y coordinate per link.
         real(kind=dp), intent(in) :: tolerance !< Coordinate tolerance for endpoint contacts and duplicate crossings.
         integer, intent(out) :: ierror_out !< Non-zero if the intersection search failed.

         integer, allocatable :: crossed_links(:), polygon_nodes(:)
         real(kind=dp), allocatable :: segment_weights(:), polygon_x(:), polygon_y(:)
         integer :: intersection_count, i, link, polygon_node, capacity, first, last, npolygon, endpoint_node
         real(kind=dp) :: intersection_x, intersection_y, endpoint_distance
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
         ierror_out = 0
         first = 1
         do while (first <= NPL)
            do while (first <= NPL)
               if (comparereal(xpl(first), dmiss) /= 0 .and. comparereal(ypl(first), dmiss) /= 0) then
                  exit
               end if
               first = first + 1
            end do
            if (first > NPL) then
               exit
            end if
            last = first
            do while (last < NPL)
               if (comparereal(xpl(last + 1), dmiss) == 0 .or. comparereal(ypl(last + 1), dmiss) == 0) then
                  exit
               end if
               last = last + 1
            end do

            npolygon = last - first + 1
            if (npolygon > 3) then
               if (dbdistance(xpl(first), ypl(first), xpl(last), ypl(last), jsferic, jasfer3D, dmiss) <= tolerance) then
                  npolygon = npolygon - 1
               end if
            end if
            if (npolygon >= 3) then
               polygon_x(1:npolygon) = xpl(first:first + npolygon - 1)
               polygon_y(1:npolygon) = ypl(first:first + npolygon - 1)
               polygon_x(npolygon + 1) = polygon_x(1)
               polygon_y(npolygon + 1) = polygon_y(1)
               call find_crossed_links_kdtree2(crossing_tree, npolygon + 1, polygon_x(1:npolygon + 1), &
                                               polygon_y(1:npolygon + 1), ITYPE_NETLINK, capacity, BOUNDARY_ALL, &
                                               intersection_count, crossed_links, polygon_nodes, segment_weights, ierror_out)
               if (ierror_out /= 0) then
                  return
               end if
               do i = 1, min(intersection_count, capacity)
                  link = crossed_links(i)
                  polygon_node = polygon_nodes(i)
                  if (link < 1 .or. link > size(crossing_count) .or. &
                      polygon_node < 1 .or. polygon_node > npolygon) then
                     cycle
                  end if
                  intersection_x = polygon_x(polygon_node) + segment_weights(i) * &
                                   (polygon_x(polygon_node + 1) - polygon_x(polygon_node))
                  intersection_y = polygon_y(polygon_node) + segment_weights(i) * &
                                   (polygon_y(polygon_node + 1) - polygon_y(polygon_node))
                  endpoint_distance = huge(1.0_dp)
                  do endpoint_node = 1, 2
                     if (kn(endpoint_node, link) < 1 .or. kn(endpoint_node, link) > numk) then
                        cycle
                     end if
                     endpoint_distance = min(endpoint_distance, &
                                             dbdistance(intersection_x, intersection_y, &
                                                        xk(kn(endpoint_node, link)), yk(kn(endpoint_node, link)), &
                                                        jsferic, jasfer3D, dmiss))
                  end do
                  if (endpoint_distance <= tolerance) then
                     cycle
                  end if
                  if (crossing_count(link) == 0) then
                     crossing_count(link) = 1
                     crossing_x(link) = intersection_x
                     crossing_y(link) = intersection_y
                  else if (dbdistance(intersection_x, intersection_y, crossing_x(link), crossing_y(link), &
                                      jsferic, jasfer3D, dmiss) > tolerance) then
                     crossing_count(link) = crossing_count(link) + 1
                  end if
               end do
            end if
            first = last + 1
         end do

         do link = 1, size(crossing_count)
            if (crossing_count(link) /= 1) then
               cycle
            end if
            if (kn(1, link) < 1 .or. kn(1, link) > numk .or. kn(2, link) < 1 .or. kn(2, link) > numk) then
               crossing_count(link) = 0
            else if (kc(kn(1, link)) == kc(kn(2, link))) then
               crossing_count(link) = 0
            end if
         end do
      end subroutine ibm_find_crossed_links

   end subroutine CUTCELwu

   !> Clip every Cartesian flow cell against the union of its Cutcelllist solids.
   subroutine cutcelwu_ibm_union(polygons)
      use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
      use precision, only: dp
      use m_flow, only: baorgfracmin
      use m_flowgeom
      use m_netw
      use m_cutcells
      use m_tpoly, only: tpoly, isize_tpoly
      use m_immersed_boundary, only: ibm_state
      use m_flowparameters, only: immersed_boundary_merge_area_frac
      use messagehandling, only: LEVEL_ERROR, mess

      type(tpoly), dimension(:), allocatable, intent(in) :: polygons !< All solid components from Cutcelllist.

      real(kind=dp), allocatable :: wet_x(:), wet_y(:), next_x(:), next_y(:)
      real(kind=dp) :: cell_x(4), cell_y(4), full_area, wet_area, area_tol, snap_tol
      real(kind=dp) :: side_dx, side_dy, side_length
      real(kind=dp) :: edge_dx, edge_dy, edge_length, projection_1, projection_2
      real(kind=dp) :: cross_1, cross_2, face_width, x1, y1, x2, y2
      integer :: n, p, n_polygons, n_wet, n_next, n_area_vertices, ll, ll_next, i, j, l, lf, ierror
      logical :: polygon_bounds_overlap
      character(len=256) :: error_text

      if (.not. allocated(polygons)) then
         call mess(LEVEL_ERROR, 'ImmersedBoundary=1: union clipping requires Cutcelllist polygons.')
         return
      end if
      if (.not. allocated(ibm_state%ba_full)) then
         call mess(LEVEL_ERROR, 'ImmersedBoundary=1: union clipping requires full-cell areas.')
         return
      end if
      if (.not. allocated(ibm_state%wu_full)) then
         call mess(LEVEL_ERROR, 'ImmersedBoundary=1: union clipping requires full-link widths.')
         return
      end if
      if (.not. ieee_is_finite(ibm_state%grid%dx) .or. .not. ieee_is_finite(ibm_state%grid%dy) .or. &
          ibm_state%grid%dx <= 0.0_dp .or. ibm_state%grid%dy <= 0.0_dp) then
         call mess(LEVEL_ERROR, 'ImmersedBoundary=1: union clipping requires positive Cartesian grid spacing.')
         return
      end if

      n_polygons = isize_tpoly(polygons)
      if (n_polygons < 1 .or. size(ibm_state%ba_full) < nump .or. size(ibm_state%wu_full) < lnx .or. &
          size(wu) < lnx) then
         call mess(LEVEL_ERROR, 'ImmersedBoundary=1: union clipping has no polygons or incomplete full-cell metrics.')
         return
      end if

      snap_tol = 1.0e-8_dp * min(ibm_state%grid%dx, ibm_state%grid%dy)
      do n = 1, nump
         if (netcell(n)%n /= 4) then
            call mess(LEVEL_ERROR, 'ImmersedBoundary=1: union clipping requires four cell corners for flow cell ', n)
            return
         end if
         do ll = 1, 4
            l = abs(netcell(n)%lin(ll))
            lf = lne2ln(l)
            if (lf > 0) then
               if (lf > size(ibm_state%wu_full)) then
                  call mess(LEVEL_ERROR, 'ImmersedBoundary=1: invalid full-link width for flow link ', lf)
                  return
               end if
               wu(lf) = ibm_state%wu_full(lf)
            end if
         end do
      end do

      do n = 1, nump
         if (netcell(n)%n /= 4) then
            call mess(LEVEL_ERROR, 'ImmersedBoundary=1: union clipping requires four cell corners for flow cell ', n)
            return
         end if
         do ll = 1, 4
            l = netcell(n)%nod(ll)
            cell_x(ll) = xk(l)
            cell_y(ll) = yk(l)
         end do
         full_area = ibm_state%ba_full(n)
         if (full_area <= 0.0_dp .or. .not. ieee_is_finite(full_area)) then
            call mess(LEVEL_ERROR, 'ImmersedBoundary=1: invalid full-cell area for flow cell ', n)
            return
         end if

         allocate (wet_x(4), wet_y(4))
         wet_x = cell_x
         wet_y = cell_y
         n_wet = 4
         do p = 1, n_polygons
            if (.not. allocated(polygons(p)%x) .or. .not. allocated(polygons(p)%y)) then
               call mess(LEVEL_ERROR, 'ImmersedBoundary=1: invalid solid polygon in Cutcelllist.')
               return
            end if
            if (polygons(p)%len < 3 .or. polygons(p)%len > size(polygons(p)%x) .or. &
                polygons(p)%len > size(polygons(p)%y)) then
               call mess(LEVEL_ERROR, 'ImmersedBoundary=1: invalid solid polygon in Cutcelllist.')
               return
            end if
            if (n_wet == 0) then
               exit
            end if

            polygon_bounds_overlap = polygons(p)%xmax >= minval(cell_x) - snap_tol .and. &
                                     polygons(p)%xmin <= maxval(cell_x) + snap_tol .and. &
                                     polygons(p)%ymax >= minval(cell_y) - snap_tol .and. &
                                     polygons(p)%ymin <= maxval(cell_y) + snap_tol
            if (.not. polygon_bounds_overlap) then
               cycle
            end if

            call ibm_subtract_solid_polygon(polygons(p), wet_x, wet_y, n_wet, snap_tol, next_x, next_y, &
                                            n_next, ierror)
            if (ierror /= 0) then
               if (ierror == ibm_union_disconnected) then
                  write (error_text, '(a,i0)') &
                     'ImmersedBoundary=1: union clipping creates disconnected wet geometry in flow cell ', n
               else
                  write (error_text, '(a,i0,a,i0,a,i0,a)') &
                     'ImmersedBoundary=1: polygon subtraction failed in flow cell ', n, &
                     ' for Cutcelllist polygon ', p, ' (Polypack error ', ierror, ')'
               end if
               call mess(LEVEL_ERROR, trim(error_text))
               return
            end if
            call move_alloc(next_x, wet_x)
            call move_alloc(next_y, wet_y)
            n_wet = n_next
         end do

         if (n_wet == 0) then
            ba(n) = 0.0_dp
            bai(n) = 0.0_dp
            kfs(n) = 0
            do ll = 1, 4
               l = abs(netcell(n)%lin(ll))
               lf = lne2ln(l)
               if (lf > 0) then
                  wu(lf) = 0.0_dp
               end if
            end do
            if (allocated(nd(n)%x)) then
               deallocate (nd(n)%x)
            end if
            if (allocated(nd(n)%y)) then
               deallocate (nd(n)%y)
            end if
            allocate (nd(n)%x(0), nd(n)%y(0))
            deallocate (wet_x, wet_y)
            cycle
         end if

         n_area_vertices = n_wet
         if (n_area_vertices > 3) then
            if (sqrt((wet_x(n_area_vertices) - wet_x(1))**2 + (wet_y(n_area_vertices) - wet_y(1))**2) <= snap_tol) then
               n_area_vertices = n_area_vertices - 1
            end if
         end if
         wet_area = 0.0_dp
         if (n_area_vertices >= 3) then
            do i = 1, n_area_vertices
               j = i + 1
               if (j > n_area_vertices) then
                  j = 1
               end if
               wet_area = wet_area + wet_x(i) * wet_y(j) - wet_x(j) * wet_y(i)
            end do
            ! A signed-area sum rejects degenerate remnants that Polypack can return when a cell is fully covered.
            wet_area = 0.5_dp * abs(wet_area)
         end if
         area_tol = 1.0e-10_dp * full_area
         if (.not. ieee_is_finite(wet_area) .or. wet_area < -area_tol .or. wet_area > full_area + area_tol) then
            call mess(LEVEL_ERROR, 'ImmersedBoundary=1: union-clipped area is invalid in flow cell ', n)
            return
         end if
         wet_area = max(0.0_dp, min(full_area, wet_area))
         if (wet_area <= area_tol) then
            ba(n) = 0.0_dp
            bai(n) = 0.0_dp
            kfs(n) = 0
            do ll = 1, 4
               l = abs(netcell(n)%lin(ll))
               lf = lne2ln(l)
               if (lf > 0) then
                  wu(lf) = 0.0_dp
               end if
            end do
            if (allocated(nd(n)%x)) then
               deallocate (nd(n)%x)
            end if
            if (allocated(nd(n)%y)) then
               deallocate (nd(n)%y)
            end if
            allocate (nd(n)%x(0), nd(n)%y(0))
            deallocate (wet_x, wet_y)
            cycle
         end if
         if (wet_area >= full_area - area_tol) then
            kfs(n) = 0
            deallocate (wet_x, wet_y)
            cycle
         end if

         kfs(n) = 1
         if (allocated(ibm_state%ba_wet)) then
            ibm_state%ba_wet(n) = wet_area
         end if
         ba(n) = max(wet_area, baorgfracmin * full_area)
         ba(n) = max(ba(n), bamin)
         if (allocated(nd(n)%x)) then
            deallocate (nd(n)%x)
         end if
         if (allocated(nd(n)%y)) then
            deallocate (nd(n)%y)
         end if
         allocate (nd(n)%x(n_wet), nd(n)%y(n_wet))
         nd(n)%x = wet_x
         nd(n)%y = wet_y

         do ll = 1, 4
            ll_next = ll + 1
            if (ll_next > 4) then
               ll_next = 1
            end if
            side_dx = cell_x(ll_next) - cell_x(ll)
            side_dy = cell_y(ll_next) - cell_y(ll)
            side_length = sqrt(side_dx * side_dx + side_dy * side_dy)
            if (side_length <= tiny(1.0_dp)) then
               call mess(LEVEL_ERROR, 'ImmersedBoundary=1: zero-length cell face in flow cell ', n)
               return
            end if
            face_width = 0.0_dp
            do i = 1, n_wet
               j = i + 1
               if (j > n_wet) then
                  j = 1
               end if
               x1 = wet_x(i)
               y1 = wet_y(i)
               x2 = wet_x(j)
               y2 = wet_y(j)
               edge_dx = x2 - x1
               edge_dy = y2 - y1
               edge_length = sqrt(edge_dx * edge_dx + edge_dy * edge_dy)
               cross_1 = abs((x1 - cell_x(ll)) * side_dy - (y1 - cell_y(ll)) * side_dx)
               cross_2 = abs((x2 - cell_x(ll)) * side_dy - (y2 - cell_y(ll)) * side_dx)
               projection_1 = ((x1 - cell_x(ll)) * side_dx + (y1 - cell_y(ll)) * side_dy) / side_length
               projection_2 = ((x2 - cell_x(ll)) * side_dx + (y2 - cell_y(ll)) * side_dy) / side_length
               if (cross_1 <= snap_tol * side_length .and. cross_2 <= snap_tol * side_length .and. &
                   min(projection_1, projection_2) >= -snap_tol .and. &
                   max(projection_1, projection_2) <= side_length + snap_tol) then
                  face_width = face_width + edge_length
               end if
            end do
            l = abs(netcell(n)%lin(ll))
            lf = lne2ln(l)
            if (lf > 0) then
               if (lf <= size(ibm_state%wu_full)) then
                  face_width = min(face_width, ibm_state%wu_full(lf))
               end if
               ! Intersect apertures from both adjacent cells so a later wet cell cannot reopen a solid face.
               wu(lf) = min(wu(lf), max(0.0_dp, face_width))
            end if
         end do

         deallocate (wet_x, wet_y)
      end do

      if (immersed_boundary_merge_area_frac > 0.0_dp) then
         call cutcelwu_merge_small_cells(polygons, snap_tol)
      end if
   end subroutine cutcelwu_ibm_union

   !> Subtract one solid from a wet polygon using Polypack's difference operation.
   subroutine ibm_subtract_solid_polygon(solid, subject_x, subject_y, subject_count, perturbation_tol, wet_x, wet_y, &
                                         wet_count, error_code)
      use precision, only: dp
      use m_polygon, only: npl, savepol, restorepol
      use m_tpoly, only: tpoly, pol_to_tpoly, dealloc_tpoly
      use m_addtopol, only: addtopol

      type(tpoly), intent(in) :: solid !< Solid polygon to remove from the wet subject.
      real(kind=dp), dimension(:), intent(in) :: subject_x !< Current wet-polygon x coordinates.
      real(kind=dp), dimension(:), intent(in) :: subject_y !< Current wet-polygon y coordinates.
      integer, intent(in) :: subject_count !< Number of current wet-polygon vertices.
      real(kind=dp), intent(in) :: perturbation_tol !< Scale for retrying coincident-edge clipping.
      real(kind=dp), dimension(:), allocatable, intent(out) :: wet_x !< Resulting wet-polygon x coordinates.
      real(kind=dp), dimension(:), allocatable, intent(out) :: wet_y !< Resulting wet-polygon y coordinates.
      integer, intent(out) :: wet_count !< Result vertex count, or -1 for disconnected geometry.
      integer, intent(out) :: error_code !< Non-zero if Polypack fails or the result is disconnected.

      integer, parameter :: max_workspace = 1000000
      integer :: workspace_size, ierr, component_count, polygon_length, perturb_attempt
      integer, allocatable :: iwrk(:)
      real(kind=dp), allocatable :: rwrk(:), shifted_x(:), shifted_y(:)
      real(kind=dp) :: subject_offset_x, subject_offset_y
      real(kind=dp), parameter :: offset_x_factor(9) = &
                                  [0.0_dp, 1.0_dp, 0.0_dp, -1.0_dp, 0.0_dp, 1.0_dp, 1.0_dp, -1.0_dp, -1.0_dp]
      real(kind=dp), parameter :: offset_y_factor(9) = &
                                  [0.0_dp, 0.0_dp, 1.0_dp, 0.0_dp, -1.0_dp, 1.0_dp, -1.0_dp, 1.0_dp, -1.0_dp]
      type(tpoly), dimension(:), allocatable :: components

      wet_count = -1
      error_code = 1
      if (.not. allocated(solid%x) .or. .not. allocated(solid%y)) then
         return
      end if
      if (solid%len < 3 .or. solid%len > size(solid%x) .or. solid%len > size(solid%y) .or. &
          subject_count < 3 .or. size(subject_x) < subject_count .or. size(subject_y) < subject_count) then
         return
      end if

      workspace_size = min(max_workspace, max(128, 16 * (solid%len + subject_count)))
      allocate (rwrk(workspace_size), iwrk(workspace_size))
      allocate (shifted_x(subject_count), shifted_y(subject_count))
      call savepol()
      ! Polypack can fail on exactly shared edges; translate only the subject for clipping and undo that translation in the result.
      do perturb_attempt = 1, size(offset_x_factor)
         subject_offset_x = offset_x_factor(perturb_attempt) * perturbation_tol
         subject_offset_y = offset_y_factor(perturb_attempt) * perturbation_tol
         shifted_x = subject_x(1:subject_count) + subject_offset_x
         shifted_y = subject_y(1:subject_count) + subject_offset_y
         do
            npl = 0
            call PPDIPO(solid%x, solid%y, solid%len, shifted_x, shifted_y, subject_count, rwrk, iwrk, &
                        workspace_size, addtopol, ierr)
            if (ierr == 0 .or. ierr > 3 .or. workspace_size >= max_workspace) then
               exit
            end if
            deallocate (rwrk, iwrk)
            workspace_size = min(max_workspace, 2 * workspace_size)
            allocate (rwrk(workspace_size), iwrk(workspace_size))
         end do
         if (ierr == 0 .or. ierr <= 3) then
            exit
         end if
      end do

      if (ierr == 0) then
         if (npl > 0) then
            allocate (components(1))
            call pol_to_tpoly(component_count, components, keepExisting=.false.)
         else
            component_count = 0
         end if
         if (component_count == 0) then
            allocate (wet_x(0), wet_y(0))
            wet_count = 0
            error_code = 0
         else if (component_count == 1) then
            polygon_length = components(1)%len
            allocate (wet_x(polygon_length), wet_y(polygon_length))
            wet_x = components(1)%x(1:polygon_length) - subject_offset_x
            wet_y = components(1)%y(1:polygon_length) - subject_offset_y
            wet_count = polygon_length
            error_code = 0
         else
            error_code = ibm_union_disconnected
         end if
      end if

      call restorepol()
      if (allocated(components)) then
         call dealloc_tpoly(components)
      end if
      deallocate (rwrk, iwrk, shifted_x, shifted_y)
   end subroutine ibm_subtract_solid_polygon

   !> Merge each selected small wet cell into its target: rewire its remaining links to the target and make it solid.
   !!
   !! The shared link between the two cells is kept in the link list with zero width.
   !! The removed wet area is not added to the target, because the target polygon is not updated.
   subroutine cutcelwu_apply_small_cell_merges(merge_target, merge_link)
      use m_flowgeom, only: nd, ln, wu, kfs, ba, bai, tnode
      use m_immersed_boundary, only: ibm_state

      integer, dimension(:), intent(in) :: merge_target !< Target cell per source cell, zero otherwise.
      integer, dimension(:), intent(in) :: merge_link !< Shared link per source cell, zero otherwise.

      integer, dimension(:), allocatable :: moved
      integer :: icell, itgt, l0, i, lk, v, k

      ibm_state%nmerged = 0
      do icell = 1, size(merge_target)
         itgt = merge_target(icell)
         if (itgt <= 0) then
            cycle
         end if
         l0 = merge_link(icell)

         allocate (moved(nd(icell)%lnx))
         k = 0
         do i = 1, nd(icell)%lnx
            v = nd(icell)%ln(i)
            lk = abs(v)
            if (lk == l0) then
               cycle
            end if
            if (ln(1, lk) == icell) then
               ln(1, lk) = itgt
            else
               ln(2, lk) = itgt
            end if
            k = k + 1
            moved(k) = sign(lk, v)
         end do
         call ibm_node_replace_links(nd(itgt), l0, moved(1:k))
         deallocate (moved)

         wu(l0) = 0.0_dp
         ba(icell) = 0.0_dp
         bai(icell) = 0.0_dp
         kfs(icell) = 0
         nd(icell)%lnx = 0
         if (allocated(nd(icell)%ln)) then
            deallocate (nd(icell)%ln)
         end if
         allocate (nd(icell)%ln(0))
         if (allocated(nd(icell)%x)) then
            deallocate (nd(icell)%x)
         end if
         if (allocated(nd(icell)%y)) then
            deallocate (nd(icell)%y)
         end if
         allocate (nd(icell)%x(0), nd(icell)%y(0))
         ibm_state%nmerged = ibm_state%nmerged + 1
      end do
   end subroutine cutcelwu_apply_small_cell_merges

   !> Select small-cell merges for the current IBM cut-cell state and apply them. Opt-in via ImmersedBoundaryMergeAreaFrac.
   !!
   !! Each target wet polygon becomes the wet region of its own cell plus all cells merged into it, so the
   !! rewired links lie on the boundary of their target. The region is the bounding box of the group, minus the
   !! other cells inside that box, minus the solids.
   subroutine cutcelwu_merge_small_cells(polygons, snap_tol)
      use precision, only: dp
      use m_flowgeom
      use m_netw
      use m_flow, only: baorgfracmin
      use m_flowparameters, only: immersed_boundary_merge_area_frac
      use m_partitioninfo, only: jampi
      use m_tpoly, only: tpoly
      use m_immersed_boundary, only: ibm_state, ibm_select_small_cell_merges
      use messagehandling, only: LEVEL_ERROR, mess

      type(tpoly), dimension(:), intent(in) :: polygons !< All solid components from Cutcelllist.
      real(kind=dp), intent(in) :: snap_tol !< Snapping tolerance for polygon clipping.

      integer, allocatable :: merge_target(:), merge_link(:)
      logical, allocatable :: in_group(:), done(:)
      real(kind=dp), allocatable :: region_x(:), region_y(:), wet_x(:), wet_y(:), next_x(:), next_y(:)
      real(kind=dp) :: xmin, xmax, ymin, ymax, cx, cy, area, cell_x(4), cell_y(4)
      integer :: s, t, m, k, k2, ll, l, n_region, n_wet, n_next, ierror
      type(tpoly) :: cell_solid

      if (jampi > 0) then
         call mess(LEVEL_ERROR, 'ImmersedBoundaryMergeAreaFrac > 0 is not supported with MPI partitioning.')
         return
      end if

      allocate (merge_target(size(kfs)), merge_link(size(kfs)), in_group(size(kfs)), done(size(kfs)))
      call ibm_select_small_cell_merges(kcu, ln, wu, kfs, ibm_state%ba_wet, ibm_state%ba_full, &
                                        immersed_boundary_merge_area_frac, merge_target, merge_link)

      done = .false.
      do s = 1, size(merge_target)
         t = merge_target(s)
         if (t <= 0) then
            cycle
         end if
         if (done(t)) then
            cycle
         end if
         done(t) = .true.

         in_group = .false.
         in_group(t) = .true.
         do m = 1, size(merge_target)
            if (merge_target(m) == t) then
               in_group(m) = .true.
            end if
         end do

         xmin = huge(1.0_dp)
         ymin = huge(1.0_dp)
         xmax = -huge(1.0_dp)
         ymax = -huge(1.0_dp)
         do m = 1, nump
            if (.not. in_group(m)) then
               cycle
            end if
            do ll = 1, 4
               l = netcell(m)%nod(ll)
               xmin = min(xmin, xk(l))
               xmax = max(xmax, xk(l))
               ymin = min(ymin, yk(l))
               ymax = max(ymax, yk(l))
            end do
         end do

         allocate (region_x(4), region_y(4))
         region_x = [xmin, xmax, xmax, xmin]
         region_y = [ymin, ymin, ymax, ymax]
         n_region = 4

         ! Remove flow cells inside the bounding box that do not belong to this group.
         do m = 1, nump
            if (in_group(m)) then
               cycle
            end if
            do ll = 1, 4
               l = netcell(m)%nod(ll)
               cell_x(ll) = xk(l)
               cell_y(ll) = yk(l)
            end do
            cx = sum(cell_x) / 4.0_dp
            cy = sum(cell_y) / 4.0_dp
            if (cx <= xmin .or. cx >= xmax .or. cy <= ymin .or. cy >= ymax) then
               cycle
            end if
            cell_solid%len = 4
            cell_solid%x = cell_x
            cell_solid%y = cell_y
            cell_solid%xmin = minval(cell_x)
            cell_solid%xmax = maxval(cell_x)
            cell_solid%ymin = minval(cell_y)
            cell_solid%ymax = maxval(cell_y)
            call ibm_subtract_solid_polygon(cell_solid, region_x, region_y, n_region, snap_tol, next_x, next_y, &
                                            n_next, ierror)
            if (ierror /= 0) then
               call mess(LEVEL_ERROR, 'ImmersedBoundaryMergeAreaFrac: merged region of flow cell could not be clipped, cell ', t)
               return
            end if
            call move_alloc(next_x, region_x)
            call move_alloc(next_y, region_y)
            n_region = n_next
         end do

         call ibm_clip_region_by_solids(region_x, region_y, n_region, polygons, snap_tol, wet_x, wet_y, n_wet, ierror)
         if (ierror /= 0) then
            call mess(LEVEL_ERROR, 'ImmersedBoundaryMergeAreaFrac: merged wet region is invalid or disconnected for flow cell ', t)
            return
         end if

         area = 0.0_dp
         do k = 1, n_wet
            k2 = k + 1
            if (k2 > n_wet) then
               k2 = 1
            end if
            area = area + wet_x(k) * wet_y(k2) - wet_x(k2) * wet_y(k)
         end do
         area = 0.5_dp * abs(area)
         if (n_wet < 3 .or. area <= 0.0_dp) then
            call mess(LEVEL_ERROR, 'ImmersedBoundaryMergeAreaFrac: merged wet region has no area for flow cell ', t)
            return
         end if

         ibm_state%ba_wet(t) = area
         ba(t) = max(area, baorgfracmin * ibm_state%ba_full(t))
         ba(t) = max(ba(t), bamin)
         if (allocated(nd(t)%x)) then
            deallocate (nd(t)%x)
         end if
         if (allocated(nd(t)%y)) then
            deallocate (nd(t)%y)
         end if
         allocate (nd(t)%x(n_wet), nd(t)%y(n_wet))
         nd(t)%x = wet_x
         nd(t)%y = wet_y

         deallocate (region_x, region_y, wet_x, wet_y)
      end do

      call cutcelwu_apply_small_cell_merges(merge_target, merge_link)
      deallocate (merge_target, merge_link, in_group, done)
   end subroutine cutcelwu_merge_small_cells

   !> Subtract every solid from a region polygon. Returns the remaining wet polygon, or a non-zero error code.
   subroutine ibm_clip_region_by_solids(region_x, region_y, region_count, polygons, snap_tol, wet_x, wet_y, wet_count, &
                                        error_code)
      use precision, only: dp
      use m_tpoly, only: tpoly

      real(kind=dp), dimension(:), intent(in) :: region_x !< Region x coordinates.
      real(kind=dp), dimension(:), intent(in) :: region_y !< Region y coordinates.
      integer, intent(in) :: region_count !< Number of region vertices.
      type(tpoly), dimension(:), intent(in) :: polygons !< All solid components from Cutcelllist.
      real(kind=dp), intent(in) :: snap_tol !< Snapping tolerance for polygon clipping.
      real(kind=dp), dimension(:), allocatable, intent(out) :: wet_x !< Wet-polygon x coordinates.
      real(kind=dp), dimension(:), allocatable, intent(out) :: wet_y !< Wet-polygon y coordinates.
      integer, intent(out) :: wet_count !< Number of wet-polygon vertices.
      integer, intent(out) :: error_code !< Non-zero if clipping fails.

      real(kind=dp), dimension(:), allocatable :: next_x, next_y
      real(kind=dp) :: xmin, xmax, ymin, ymax
      integer :: p, n_next

      allocate (wet_x(region_count), wet_y(region_count))
      wet_x = region_x(1:region_count)
      wet_y = region_y(1:region_count)
      wet_count = region_count
      error_code = 0
      xmin = minval(wet_x)
      xmax = maxval(wet_x)
      ymin = minval(wet_y)
      ymax = maxval(wet_y)

      do p = 1, size(polygons)
         if (wet_count < 3) then
            exit
         end if
         if (polygons(p)%xmax < xmin - snap_tol .or. polygons(p)%xmin > xmax + snap_tol .or. &
             polygons(p)%ymax < ymin - snap_tol .or. polygons(p)%ymin > ymax + snap_tol) then
            cycle
         end if
         call ibm_subtract_solid_polygon(polygons(p), wet_x, wet_y, wet_count, snap_tol, next_x, next_y, n_next, &
                                         error_code)
         if (error_code /= 0) then
            return
         end if
         call move_alloc(next_x, wet_x)
         call move_alloc(next_y, wet_y)
         wet_count = n_next
      end do
   end subroutine ibm_clip_region_by_solids

   !> Remove link drop from a node's link list and append the links in add.
   subroutine ibm_node_replace_links(node, drop, add)
      use m_flowgeom, only: tnode

      type(tnode), intent(inout) :: node !< Node whose link list is updated.
      integer, intent(in) :: drop !< Link number to remove.
      integer, dimension(:), intent(in) :: add !< Signed link numbers to append.

      integer, dimension(:), allocatable :: kept
      integer :: i, n

      allocate (kept(node%lnx + size(add)))
      n = 0
      do i = 1, node%lnx
         if (abs(node%ln(i)) == drop) then
            cycle
         end if
         n = n + 1
         kept(n) = node%ln(i)
      end do
      kept(n + 1:n + size(add)) = add
      n = n + size(add)
      node%ln = kept(1:n)
      node%lnx = n
   end subroutine ibm_node_replace_links

end module m_cutcelwu
