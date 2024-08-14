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

subroutine CUTCELWU(n12, jamasks, ipoly)
   use m_netw
   use m_flowgeom
   use kdtree2Factory
   use m_missing, only: dmiss, jins
   use m_cutcells
   use unstruc_messages
   use m_polygon, only: NPL, xpl, ypl, zpl
   use geometry_module, only: dbpinpol, dbdistance
   use m_sferic, only: jsferic, jasfer3D
   use m_flow, only: numlimdt, numlimdt_baorg, baorgfracmin

   implicit none
   integer, intent(in) :: N12 ! 3: only mask nodes, 4: preparation for cut cells (set kfs), 5: actual cut cells (change wu, nd), 6: dry cells
   integer, intent(in) :: jamasks ! do not use masks (0), store masks (1), use stored masks (2)
   integer, intent(in) :: ipoly ! polygon number for masks

   integer :: ja, KMOD
   integer :: K, K1, K2, L, LL, N, NN, LF, IC, LLU, IN, KL
   integer, allocatable :: KNP(:)

   double precision :: XM, YM, XXC(8), YYC(8), DAREA, DLENGTH, DLENMX

   double precision, dimension(:), allocatable :: xmL, ymL ! intersection coordinates
   integer, dimension(:), allocatable :: Lmask ! link mask

   double precision :: cx, cy, R2search, Area, cof0
   integer :: i, ip1, num, k_start, k_end, numsam
   integer :: jadelete
   integer :: jakdtree
   integer :: jasplitpol, numpolies, ip

   type(tpoly), dimension(:), allocatable :: pli_loc

   double precision, parameter :: dtol = 1d-8

   jakdtree = 1

   call READYY('CUTCELWU', 0d0)

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
            Area = 0d0
            cx = 0d0
            cy = 0d0
            num = 0
            do i = 1, NPL
               ip1 = i + 1; if (ip1 > NPL) ip1 = ip1 - NPL

               if (xpl(ip1) == DMISS) cycle
               cof0 = xpl(i) * ypl(ip1) - xpl(ip1) * ypl(i)
               Area = Area + cof0
               cx = cx + (xpl(i) + xpl(ip1)) * cof0
               cy = cy + (ypl(i) + ypl(ip1)) * cof0
               num = num + 1
            end do
            area = area * 0.5d0

            if (area == 0d0) cycle

            cx = cx / area / 6.0d0
            cy = cy / area / 6.0d0
            !
            ! find the circumcircle
            !
            R2search = 0d0
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
      allocate (KNP(NUMP)); KNP = 0

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

         if (mod(n, KMOD) == 0) call READYY('CUTCELWU', dble(n) / dble(nump))

         if (KNP(N) == 1) then ! AT LEAST 1 POINT INSIDE POLYGON, SO CHECK CUTC

            NN = netcell(N)%N

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

               LLU = LL + 1; if (LLU > NN) LLU = 1
               K1 = NETCELL(N)%NOD(LL)
               K2 = NETCELL(N)%NOD(LLU)

               if (jamasks == 0 .or. jamasks == 1) then
                  if (kc(kn(1, L)) == 1 .or. kc(kn(2, L)) == 1) then
                     call CROSSLINKPOLY(L, 0, 0, (/0/), (/0/), XM, YM, JA)
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
                        IC = IC + 1; XXC(IC) = XM; YYC(IC) = YM
                        IC = IC + 1; XXC(IC) = XK(K2); YYC(IC) = YK(K2)
                        if (Lf > 0) then
                           if (wu(LF) /= 0d0) WU(LF) = DBDISTANCE(XM, YM, XK(K2), YK(K2), jsferic, jasfer3D, dmiss)
                        end if
                     else if (kc(k1) /= 1 .and. kc(k2) == 1) then
                        if (IC == 0) then
                           IC = IC + 1; XXC(IC) = XK(K1); YYC(IC) = YK(K1)
                        end if
                        IC = IC + 1; XXC(IC) = XM; YYC(IC) = YM
                        if (Lf > 0) then
                           if (wu(LF) /= 0d0) WU(LF) = DBDISTANCE(XM, YM, XK(K1), YK(K1), jsferic, jasfer3D, dmiss)
                        end if
                     else if (kc(k1) == 1 .and. kc(k2) == 1 .and. Lf > 0) then
                        wu(Lf) = 0d0
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
                           IC = IC + 1; XXC(IC) = XK(K1); YYC(IC) = YK(K1)
                        end if
                        IC = IC + 1; XXC(IC) = XK(K2); YYC(IC) = YK(K2)
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
                     L = abs(nd(n)%ln(KL)); wu(L) = 0d0
                  end do
                  ba(n) = 0d0

               else

                  call dAREAN(XXC, YYC, IC, DAREA, DLENGTH, DLENMX) ! AREA AND LENGTH OF POLYGON

                  if (DAREA / BA(n) < 0.05d0) then
                     do KL = 1, nd(n)%lnx
                        L = abs(nd(n)%ln(KL)); wu(L) = 0d0
                     end do
                     ba(n) = 0d0
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

   if (allocated(knp)) deallocate (KNP)

   if (jamasks == 1) then
      if (allocated(Lmask)) deallocate (Lmask)
      if (allocated(xmL)) deallocate (xmL)
      if (allocated(ymL)) deallocate (ymL)
   end if

   call READYY('CUTCELWU', -1d0)

end subroutine CUTCELwu
