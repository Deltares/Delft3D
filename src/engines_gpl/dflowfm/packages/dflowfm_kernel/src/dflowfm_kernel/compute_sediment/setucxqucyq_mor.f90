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

   subroutine setucxqucyq_mor(u1, ucxq, ucyq)
      use m_fm_erosed, only: ucxq_mor, ucyq_mor, hs_mor, link1, link1sign, ndx_mor
      use m_flowgeom, only: ndx, lnx, lnxi, ln, nd, wcx1, wcx2, wcy1, wcy2, csu, snu, bl, lnx1D, kcs
      use m_flow, only: hs, hu, zws, kmx, au, q1, lnkx, ndkx
      use m_flowparameters, only: jacstbnd, epshs, eps10, flow_solver, FLOW_SOLVER_FM
      use m_sediment, only: stmpar
      use m_turbulence, only: ln0
      use m_CrossSections, only: GetCSParsFlow
      use unstruc_channel_flow, only: network
      use m_f1dimp, only: f1dimppar
      use m_get_kbot_ktop
      use m_get_Lbot_Ltop

      implicit none
      double precision, dimension(lnkx), intent(in) :: u1
      double precision, dimension(ndkx), intent(in) :: ucxq
      double precision, dimension(ndkx), intent(in) :: ucyq
      integer :: L, LL, k, k1, k2, Lt, Lb, kk, kb, kt, idx_sre, kd
      double precision :: wcxu, wcyu, cs, sn, uin, huL
      logical, pointer :: maximumwaterdepth
      double precision, pointer :: maximumwaterdepthfrac
      double precision, dimension(:), allocatable :: area
      integer :: qsign
      double precision :: area_L
      double precision :: width_L
      double precision :: perim_L
      integer :: nstruc

      maximumwaterdepth => stmpar%morpar%mornum%maximumwaterdepth
      maximumwaterdepthfrac => stmpar%morpar%mornum%maximumwaterdepthfrac

      allocate (area(ndx))

      do k = 1, ndx
         hs_mor(k) = hs(k)
         if (kcs(k) == 1) then
            ucxq_mor(k) = 0d0
            ucyq_mor(k) = 0d0
            area(k) = 0d0
         else
            ucxq_mor(k) = ucxq(k)
            ucyq_mor(k) = ucyq(k)
         end if
      end do

      !------------------------------------------
      ! FM solver
      !------------------------------------------

      if (flow_solver == FLOW_SOLVER_FM) then !FM solver

         ! we define the node as the begin/end point of the first link connected to it
         if (stmpar%morpar%mornum%pure1d) then
            do L = 1, lnx1D
               k1 = ln(1, L)
               k2 = ln(2, L)
               nstruc = network%adm%lin2str(L)
               if (kcs(k1) == 1) then ! link pointing away from the node
                  if (link1(k1) == L) then
                     qsign = 1
                  else
                     qsign = link1sign(k1)
                  end if
                  ! call getprof_1D(L, hu(L), au(L), widu, japerim, calcConv, perim)
                  ucxq_mor(k1) = ucxq_mor(k1) + qsign * q1(L)
                  if (.true.) then !nstruc > 0) then
                     ! link has structure
                     ! see getprof_1D: if this is not a boundary link, we can use L to index line2cross.
                     call GetCSParsFlow(network%adm%line2cross(L, 2), network%crs%cross, hs(k1), area_L, perim_L, width_L)
                  else
                     ! no structure on link
                     area_L = au(L)
                  end if
                  area(k1) = area(k1) + area_L
               end if
               if (kcs(k2) == 1) then ! link pointing towards the node
                  if (link1(k2) == L) then
                     qsign = 1
                  else
                     qsign = -link1sign(k2)
                  end if
                  ucxq_mor(k2) = ucxq_mor(k2) + qsign * q1(L)
                  if (.true.) then !nstruc > 0) then ! link has structure?
                     ! link has structure
                     ! see getprof_1D: if this is not a boundary link, we can use L to index line2cross.
                     call GetCSParsFlow(network%adm%line2cross(L, 2), network%crs%cross, hs(k2), area_L, perim_L, width_L)
                  else
                     ! no structure on link
                     area_L = au(L)
                  end if
                  area(k2) = area(k2) + area_L
               end if
            end do

            do k = 1, ndx
               if (kcs(k) == 1) then
                  ucxq_mor(k) = ucxq_mor(k) / area(k)
                  ucyq_mor(k) = 0d0
               else
                  ucxq_mor(k) = ucxq(k)
                  ucyq_mor(k) = ucyq(k)
               end if
            end do

            deallocate (area)

         else ! 2D/3D
            ucxq_mor = 0d0; ucyq_mor = 0d0; 
            if (.not. maximumwaterdepth) then
               if (kmx == 0) then
                  do k = 1, ndx
                     ucxq_mor(k) = ucxq(k)
                     ucyq_mor(k) = ucyq(k)
                  end do
               else
                  do k = 1, ndx
                     call getkbotktop(k, kb, kt)
                     do kk = kb, kt
                        ucxq_mor(kk) = ucxq(kk)
                        ucyq_mor(kk) = ucyq(kk)
                     end do
                  end do
               end if
               return
            end if

            if (kmx == 0) then
               do L = 1, lnx
                  if (u1(L) == 0d0) cycle
                  k1 = ln(1, L); k2 = ln(2, L)
                  wcxu = wcx1(L) * u1(L)
                  ucxq_mor(k1) = ucxq_mor(k1) + wcxu * hu(L)
                  wcyu = wcy1(L) * u1(L)
                  ucyq_mor(k1) = ucyq_mor(k1) + wcyu * hu(L)
                  wcxu = wcx2(L) * u1(L)
                  ucxq_mor(k2) = ucxq_mor(k2) + wcxu * hu(L)
                  wcyu = wcy2(L) * u1(L)
                  ucyq_mor(k2) = ucyq_mor(k2) + wcyu * hu(L)
               end do

               do L = lnxi + 1, lnx
                  k1 = ln(1, L); k2 = ln(2, L)
                  if (jacstbnd == 0) then
                     cs = csu(L); sn = snu(L)
                     uin = ucxq_mor(k2) * cs + ucyq_mor(k2) * sn
                     ucxq_mor(k1) = uin * cs
                     ucyq_mor(k1) = uin * sn
                     bl(k2) = bl(k1)
                  else
                     ucxq_mor(k1) = ucxq_mor(k2)
                     ucyq_mor(k1) = ucyq_mor(k2)
                  end if
               end do

               do k = 1, ndx
                  do L = 1, nd(k)%lnx
                     LL = abs(nd(k)%ln(L))
                     hs_mor(k) = max(hs_mor(k), maximumwaterdepthfrac * hu(LL))
                  end do
               end do

               do k = 1, ndx
                  if (hs_mor(k) > epshs) then
                     ucxq_mor(k) = ucxq_mor(k) / hs_mor(k)
                     ucyq_mor(k) = ucyq_mor(k) / hs_mor(k)
                  else
                     ucxq_mor(k) = 0d0
                     ucyq_mor(k) = 0d0
                  end if
               end do
            else ! 3D, max waterdepth
               do LL = 1, lnx
                  if (abs(u1(LL)) < eps10) cycle
                  call getLbotLtop(LL, Lb, Lt)
                  if (Lt < Lb) cycle
                  do L = Lb, Lt
                     k1 = ln0(1, L); k2 = ln0(2, L)
                     huL = hu(L)
                     huL = huL - hu(L - 1)
                     ucxq_mor(k1) = ucxq_mor(k1) + wcx1(LL) * u1(L) * huL
                     ucyq_mor(k1) = ucyq_mor(k1) + wcy1(LL) * u1(L) * huL
                     ucxq_mor(k2) = ucxq_mor(k2) + wcx2(LL) * u1(L) * huL
                     ucyq_mor(k2) = ucyq_mor(k2) + wcy2(LL) * u1(L) * huL
                  end do
               end do

               do L = lnxi + 1, lnx
                  cs = csu(L); sn = snu(L)
                  call getLbotLtop(L, Lb, Lt)
                  do LL = Lb, Lt
                     k1 = ln0(1, LL); k2 = ln0(2, LL)
                     if (jacstbnd == 0) then
                        uin = ucxq_mor(k2) * cs + ucyq_mor(k2) * sn
                        ucxq_mor(k1) = uin * cs
                        ucyq_mor(k1) = uin * sn
                     else
                        ucxq_mor(k1) = ucxq_mor(k2)
                        ucyq_mor(k1) = ucyq_mor(k2)
                     end if
                  end do
               end do
               !
               do k = 1, ndx
                  do L = 1, nd(k)%lnx
                     LL = abs(nd(k)%ln(L))
                     hs_mor(k) = max(hs_mor(k), hu(LL))
                  end do
                  !
                  ! fill hs_mor with layer thickness
                  call getkbotktop(k, kb, kt)
                  do kk = kb, kt
                     hs_mor(kk) = max(zws(kk) - zws(kk - 1), epshs)
                  end do
               end do
               !
               ! Replace hs_mor with largest link value
               ! This is potentially not consistent for Z/mixed sigma approach, to check JRE
               do LL = 1, lnx
                  call getLbotLtop(LL, Lb, Lt)
                  if (Lt < Lb) cycle
                  do L = Lb, Lt
                     k1 = ln0(1, L); k2 = ln0(2, L)
                     huL = hu(L) - hu(L - 1)
                     hs_mor(k1) = max(hs_mor(k1), huL)
                     hs_mor(k2) = max(hs_mor(k2), huL)
                  end do
               end do
               !
               !
               do k = 1, ndx
                  call getkbotktop(k, kb, kt)
                  do kk = kb, kt
                     ucxq_mor(kk) = ucxq_mor(kk) / hs_mor(kk)
                     ucyq_mor(kk) = ucyq_mor(kk) / hs_mor(kk)
                  end do
               end do
            end if
         end if

         !------------------------------------------
         ! SRE solver
         !------------------------------------------

      else !SRE solver
         do kd = 1, ndx_mor !loop on FM nodes

            idx_sre = f1dimppar%grd_fm_sre(kd)

            ucxq_mor(kd) = f1dimppar%qpack(idx_sre, 3) / f1dimppar%waoft(idx_sre, 3)
            ucyq_mor(kd) = 0
            hs_mor(kd) = f1dimppar%hpack(idx_sre, 3) - f1dimppar%bedlevel(idx_sre)

         end do !kd

      end if !flow_solver

   end subroutine setucxqucyq_mor
