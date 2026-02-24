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

module m_setumod
   use precision, only: dp
   implicit none

   private

   public :: setumod, compute_tangential_velocity_spherical

   real(kind=dp), dimension(:), allocatable :: hmin_

contains

   !> set cell center Perot velocities at nodes
   !! set Perot based friction velocities umod at u point
   !! set tangential velocities at u point
   !! set velocity gradient at u point
   !! set corner based Perot velocities
   subroutine setumod(jazws0)
      use precision, only: dp
      use m_setucxucyucxuucyunew, only: setucxucyucxuucyunew
      use m_setucxucyucxuucyu, only: setucxucyucxuucyu
      use m_setcornervelocities, only: setcornervelocities
      use m_prefetch, only: prefetch_node_velocities, prefetch_corner_velocities, allocate_prefetch_arrays
      use timers, only: timstrt, timstop
      use m_flow
      use m_flowgeom
      use m_flowtimes, only: handle_umod, ja_timestep_auto_visc, dti
      use m_sferic
      use m_xbeach_data, only: roller, swave, nuhfac
      use unstruc_model, only: md_restartfile
      use m_setucxcuy_leastsquare, only: reconst2nd
      use MessageHandling
      use m_drawthis
      use m_get_kbot_ktop
      use m_get_Lbot_Ltop
      use m_get_chezy, only: get_chezy
      use m_horvic
      use m_horvic3
      use m_cor2linx, only: cor2linx
      use m_cor2liny, only: cor2liny
      use m_lin2nodx, only: lin2nodx
      use m_lin2nody, only: lin2nody
      use m_nod2linx, only: nod2linx
      use m_nod2liny, only: nod2liny
      use m_nod2wallx, only: nod2wallx
      use m_nod2wally, only: nod2wally
      use m_wall2linx, only: wall2linx
      use m_wall2liny, only: wall2liny
      use m_waveconst

      implicit none(external)

      integer, intent(in) :: jazws0
      ! locals
      integer :: L, LL, k, k1, k2, k3, k4, kb, n, n1, n2 !
      real(kind=dp) :: duxdn, duydn, duxdt, duydt ! normal and tangential global ux,uy gradients
      real(kind=dp) :: vicl, c11, c12, c22
      real(kind=dp) :: sf, ac1, ac2, csl, snl, wuw, ustar, suxw, suyw, suxL, suyL
      real(kind=dp) :: cs, sn
      real(kind=dp) :: hmin, hs1, hs2
      real(kind=dp) :: fcor, vcor, fcor1, fcor2, fvcor
      real(kind=dp) :: dundn, dutdn, dundt, dutdt, shearvar, delty
      real(kind=dp) :: huv
      real(kind=dp), allocatable :: u1_tmp(:), vluban(:)
      integer :: nw, L1, L2, kt, Lb, Lt, Lb1, Lt1, Lb2, Lt2, kb1, kb2, ntmp, m
      real(kind=dp) :: dxiAu, vicc, vlban, fcLL
      integer :: number_limited_links
      real(kind=dp) :: viscocity_max_limit

      call timstrt('Umod', handle_umod)
      if (jazws0 == 1 .and. len_trim(md_restartfile) > 0) then
         ! This is the moment after the restart file is read and before the first output of the inital info.
         ! At this moment, u0 is used to compute the cell-center velocities. And hs has been computed in flow_initimestep, using s0.
         ntmp = size(u1)
         allocate (u1_tmp(ntmp))
         u1_tmp = u1
         u1 = u0
         hs = s0 - bl
         if (Perot_type == NOT_DEFINED) then
            call reconst2nd()
         end if
         if (newcorio == 1) then
            call setucxucyucxuucyunew() !reconstruct cell-center velocities
         else
            call setucxucyucxuucyu() !reconstruct cell-center velocities
         end if
         u1 = u1_tmp
         deallocate (u1_tmp)
      else
         if (Perot_type == NOT_DEFINED) then
            call reconst2nd()
         end if
         if (newcorio == 1) then
            call setucxucyucxuucyunew()
         else
            call setucxucyucxuucyu()
         end if
      end if

      call allocate_prefetch_arrays()
      call prefetch_node_velocities(ucx, ucy, ucxq, ucyq)
      if (kmx == 0) then
         ! pre compute hmin as it is reused a lot
         if (.not. allocated(hmin_)) then
            allocate (hmin_(lnkx))
         end if
         do L = lnx1D + 1, lnx
            k1 = ln(1, L)
            k2 = ln(2, L)
            hmin_(L) = min(hs(k1), hs(k2))
         end do
      end if

      if (kmx == 0 .and. newcorio == 1 .and. Perot_type /= NOT_DEFINED) then
         call set_V_2D_default(jasfer3D)
      else
         !$OMP PARALLEL DO                           &
         !$OMP PRIVATE(L,LL,Lb,Lt,k1,k2,cs,sn,hmin,fcor,vcor)
         do LL = lnx1D + 1, lnx
            if (newcorio == 0) then
               hmin = min(hs(ln(1, LL)), hs(ln(2, LL)))
            elseif (newcorio == 1) then
               if (hu(LL) == 0) then
                  cycle
               end if
            end if

            call getLbotLtop(LL, Lb, Lt)
            cs = csu(LL)
            sn = snu(LL)
            v(LL) = 0.0_dp
            do L = Lb, Lt
               k1 = ln(1, L)
               k2 = ln(2, L)

               if (Perot_type /= NOT_DEFINED) then
                  if (jasfer3D == 1) then
                     v(L) = acL(LL) * (-sn * nod2linx(LL, 1, ucx(k1), ucy(k1)) + cs * nod2liny(LL, 1, ucx(k1), ucy(k1))) + &
                            (1.0_dp - acL(LL)) * (-sn * nod2linx(LL, 2, ucx(k2), ucy(k2)) + cs * nod2liny(LL, 2, ucx(k2), ucy(k2)))
                  else
                     v(L) = acl(LL) * (-sn * ucx(k1) + cs * ucy(k1)) + &
                            (1.0_dp - acl(LL)) * (-sn * ucx(k2) + cs * ucy(k2))
                  end if
               end if
               if (kmx > 0) then
                  v(LL) = v(LL) + v(L) * Au(L) ! hk: activate when needed
               end if

               if (newcorio == 0 .and. icorio > 0) then
                  ! set u tangential
                  if (icorio == 4) then
                     vcor = v(L)
                  else
                     if (jasfer3D == 1) then
                        vcor = acL(LL) * (-sn * nod2linx(LL, 1, ucxq(k1), ucyq(k1)) + cs * nod2liny(LL, 1, ucxq(k1), ucyq(k1))) + &
                               (1.0_dp - acL(LL)) * (-sn * nod2linx(LL, 2, ucxq(k2), ucyq(k2)) + cs * nod2liny(LL, 2, ucxq(k2), ucyq(k2)))
                     else
                        vcor = acl(LL) * (-sn * ucxq(k1) + cs * ucyq(k1)) + & ! continuity weighted best sofar plus depth limiting
                               (1.0_dp - acl(LL)) * (-sn * ucxq(k2) + cs * ucyq(k2))
                     end if
                  end if

                  if (jsferic == 1) then
                     fcor = fcori(LL)
                  else
                     fcor = fcorio
                  end if
                  if (fcor /= 0.0_dp) then
                     if (trshcorio > 0) then
                        if (hmin < trshcorio) then
                           fcor = fcor * hmin / trshcorio
                        end if
                     end if
                     adve(L) = adve(L) - fcor * vcor
                  end if
               end if
            end do
            if (kmx > 0) then
               if (Au(LL) > 0.0_dp) then ! hk: activate if needed
                  v(LL) = v(LL) / Au(LL)
               end if
            end if
         end do
         !$OMP END PARALLEL DO
      end if
      if (newcorio == 1 .and. icorio > 0) then
         if (icorio <= 20 .and. kmx == 0) then

            call compute_coriolis_correction_2D_default(icorio, jsferic, jasfer3D, jacorioconstant, fcorio, trshcorio, Corioadamsbashfordfac)

         else if (icorio < 40) then
            fcor1 = fcorio
            fcor2 = fcorio
            !x$OMP PARALLEL DO                           &
            !x$OMP PRIVATE(L,LL,Lb,Lt,k1,k2,cs,sn,hs1,hs2,fcor,fcor1,fcor2,fvcor,vcor,volu,hmin)
            do LL = lnx1D + 1, lnx
               if (hu(LL) == 0) then
                  cycle
               end if
               n1 = ln(1, LL)
               n2 = ln(2, LL)
               hmin = min(hs(n1), hs(n2))

               call getLbotLtop(LL, Lb, Lt)
               cs = csu(LL)
               sn = snu(LL)

               if (jsferic > 0 .or. jacorioconstant > 0) then
                  if (icorio >= 4 .and. icorio <= 6) then
                     fcor1 = fcori(LL)
                     fcor2 = fcor1 ! defined at u-point
                  else
                     fcor1 = fcori(n1)
                     fcor2 = fcori(n2) ! defined at zeta-points
                  end if
               end if

               do L = Lb, Lt
                  k1 = ln(1, L)
                  k2 = ln(2, L)

                  if (icorio > 0) then
                     fvcor = 0.0_dp
                     ! set u tangential
                     if (icorio <= 20) then ! Olga types
                        if (jasfer3D == 1) then
                           fvcor = acL(LL) * (-sn * nod2linx(LL, 1, ucxq(k1), ucyq(k1)) + cs * nod2liny(LL, 1, ucxq(k1), ucyq(k1))) * fcor1 + &
                                   (1.0_dp - acL(LL)) * (-sn * nod2linx(LL, 2, ucxq(k2), ucyq(k2)) + cs * nod2liny(LL, 2, ucxq(k2), ucyq(k2))) * fcor2
                        else
                           fvcor = acl(LL) * (-sn * ucxq(k1) + cs * ucyq(k1)) * fcor1 + &
                                   (1.0_dp - acl(LL)) * (-sn * ucxq(k2) + cs * ucyq(k2)) * fcor2
                        end if
                     else ! David types
                        if (icorio <= 26) then ! hs/hu
                           hs1 = hs(n1)
                           hs2 = hs(n2)
                           if (kmx > 0) then
                              if (mod(icorio, 2) /= 0) then ! odd nrs get local k-weighting
                                 hs1 = zws(k1) - zws(k1 - 1)
                                 hs2 = zws(k2) - zws(k2 - 1)
                              end if
                           end if
                           huv = hu(L)
                        else if (icorio <= 28) then ! ahus/ahu
                           hs1 = hus(n1)
                           hs2 = hus(n2)
                           if (kmx > 0) then
                              if (mod(icorio, 2) /= 0) then ! odd nrs get local k-weighting
                                 hs1 = hus(k1)
                                 hs2 = hus(k2)
                              end if
                           end if
                           huv = acl(LL) * hs1 + (1.0_dp - acl(LL)) * hs2
                        else if (icorio <= 30) then ! like advec33
                           if (mod(icorio, 2) /= 0) then ! odd nrs get local k-weighting
                              hs1 = vol1(k1)
                              hs2 = vol1(k2)
                           else
                              hs1 = vol1(n1)
                              hs2 = vol1(n2)
                           end if
                           huv = acl(LL) * hs1 + (1.0_dp - acl(LL)) * hs2
                        end if

                        if (huv > 0) then
                           if (jasfer3D == 1) then
                              fvcor = acL(LL) * (-sn * nod2linx(LL, 1, ucxq(k1), ucyq(k1)) + cs * nod2liny(LL, 1, ucxq(k1), ucyq(k1))) * fcor1 * hs1 + &
                                      (1.0_dp - acL(LL)) * (-sn * nod2linx(LL, 2, ucxq(k2), ucyq(k2)) + cs * nod2liny(LL, 2, ucxq(k2), ucyq(k2))) * fcor2 * hs2
                           else
                              fvcor = acl(LL) * (-sn * ucxq(k1) + cs * ucyq(k1)) * fcor1 * hs1 + &
                                      (1.0_dp - acl(LL)) * (-sn * ucxq(k2) + cs * ucyq(k2)) * fcor2 * hs2
                           end if
                           fvcor = fvcor / huv
                        end if
                     end if

                     if (trshcorio > 0) then
                        if (hmin < trshcorio) then
                           fvcor = fvcor * hmin / trshcorio
                        end if
                     end if
                     adve(L) = adve(L) - fvcor

                     if (Corioadamsbashfordfac > 0.0_dp) then
                        if (fvcoro(L) /= 0.0_dp) then
                           adve(L) = adve(L) - Corioadamsbashfordfac * (fvcor - fvcoro(L))
                        end if
                        fvcoro(L) = fvcor
                     end if
                  end if
               end do
               if (icorio > 0 .and. Corioadamsbashfordfac > 0.0_dp) then
                  fvcoro(Lt + 1:Lb + kmxL(LL) - 1) = 0.0_dp
               end if
            end do

         else if (icorio >= 65) then

            if (.not. allocated(vluban)) then
               allocate (vluban(Lnkx))
            end if
            vluban = 0
            do m = 1, mxban ! bz based on netnodes area
               k = nban(1, m)
               n = nban(2, m)
               L1 = nban(3, m)
               L2 = nban(4, m)
               if (L1 > 0 .and. L2 > 0) then
                  vlban = banf(m) * hs(k)
                  if (icorio == 66) then
                     hs1 = acl(L1) * hs(ln(1, L1)) + (1.0_dp - acl(L1)) * hs(ln(2, L1))
                     hs2 = acl(L2) * hs(ln(1, L2)) + (1.0_dp - acl(L2)) * hs(ln(2, L2))
                     vlban = 0.5_dp * (hs1 + hs2)
                  end if
                  fcLL = vlban * fcorio * (-csu(L1) * snu(L2) + snu(L1) * csu(L2)) ! tangential L1 L2
                  adve(L1) = adve(L1) + u1(L2) * fcLL
                  adve(L2) = adve(L2) - u1(L1) * fcLL
                  vluban(L1) = vluban(L1) + vlban
                  vluban(L2) = vluban(L2) + vlban
               end if
            end do
            do L = 1, Lnx
               adve(L) = adve(L) / vluban(L)
            end do
         end if
      end if

      !updvertp

      ihorvic = 0
      if (vicouv > 0 .or. javiusp == 1 .or. Smagorinsky > 0 .or. Elder > 0 .or. kmx > 0) then
         ihorvic = 1
      end if
      if (ihorvic > 0 .or. jaconveyance2D >= 3 .or. ndraw(29) == 37) then
         call setcornervelocities() ! must be called after ucx, ucy have been set
         call prefetch_corner_velocities(ucnx, ucny)
      end if
      if (vicouv < 0.0_dp) then
         ihorvic = 0
      end if

      if (ihorvic > 0 .or. NDRAW(29) == 37) then
         number_limited_links = 0
         dvxc = 0
         dvyc = 0
         suu = 0
         if (kmx == 0) then

            if (istresstyp == 2 .or. istresstyp == 3) then ! first set stressvector in cell centers
               call compute_viscosity_and_stress_vectorized(Smagorinsky, Elder, vicouv, javiusp, nshiptxy, vicuship, ja_timestep_auto_visc, jawave, swave, roller, nuhfac, rhomean, jsferic, jasfer3D)
            else
               !$OMP PARALLEL DO                                  &
               !$OMP PRIVATE(L,k1,k2)
               do L = lnx1D + 1, lnx
                  if (hu(L) > 0) then ! link will flow
                     k1 = ln(1, L)
                     k2 = ln(2, L)
                     if (istresstyp == 4 .or. istresstyp == 5) then ! set stresscomponent in links right away
                        suu(L) = acl(L) * horvic(1, L) + (1.0_dp - acl(L)) * horvic(2, L)
                     else if (istresstyp == 6) then
                        suu(L) = acl(L) * horvic3(1, L) + (1.0_dp - acl(L)) * horvic3(2, L)
                     end if
                  end if
               end do
               !$OMP END PARALLEL DO
            end if
         else if (kmx > 0) then
            if (istresstyp == 2 .or. istresstyp == 3) then ! first set stressvector in cell centers
               do LL = lnx1D + 1, lnx
                  if (abs(kcu(LL)) /= 2) then
                     cycle
                  end if
                  call getLbotLtop(LL, Lb, Lt)
                  cs = csu(LL)
                  sn = snu(LL)

                  if (javiusp == 1) then ! user specified part
                     vicc = viusp(LL)
                  else
                     vicc = vicouv
                  end if
                  do L = Lb, Lt
                     vicL = 0.0_dp
                     k1 = ln(1, L)
                     k2 = ln(2, L)
                     k3 = lncn(1, L)
                     k4 = lncn(2, L)
                     if (jasfer3D == 1) then
                        duxdn = (nod2linx(LL, 2, ucx(k2), ucy(k2)) - nod2linx(LL, 1, ucx(k1), ucy(k1))) * dxi(LL)
                        duydn = (nod2liny(LL, 2, ucx(k2), ucy(k2)) - nod2liny(LL, 1, ucx(k1), ucy(k1))) * dxi(LL)
                        duxdt = (cor2linx(LL, 2, ucnx(k4), ucny(k4)) - cor2linx(LL, 1, ucnx(k3), ucny(k3))) * wui(LL)
                        duydt = (cor2liny(LL, 2, ucnx(k4), ucny(k4)) - cor2liny(LL, 1, ucnx(k3), ucny(k3))) * wui(LL)
                     else
                        duxdn = (ucx(k2) - ucx(k1)) * dxi(LL)
                        duydn = (ucy(k2) - ucy(k1)) * dxi(LL)
                        duxdt = (ucnx(k4) - ucnx(k3)) * wui(LL)
                        duydt = (ucny(k4) - ucny(k3)) * wui(LL)
                     end if
                     if (Smagorinsky > 0 .or. NDRAW(29) == 37) then ! add Smagorinsky
                        dundn = cs * duxdn + sn * duydn
                        dutdn = -sn * duxdn + cs * duydn
                        dundt = cs * duxdt + sn * duydt
                        dutdt = -sn * duxdt + cs * duydt
                        if (NDRAW(29) == 37 .and. L - Lb + 1 == kplot) then ! plot curl
                           plotlin(LL) = (dutdn - dundt)
                        end if
                        if (Smagorinsky > 0) then
                           shearvar = 2.0_dp * (dundn * dundn + dutdt * dutdt + dundt * dutdn) + dundt * dundt + dutdn * dutdn
                           vicL = vicL + Smagorinsky * Smagorinsky * sqrt(shearvar) / (dxi(LL) * wui(LL))
                        end if
                     end if
                     vicL = vicL + vicc
                     if (javiuplus3D > 0) then
                        vicL = vicL + vicwwu(L)
                     end if
                     if (nshiptxy > 0) then
                        if (vicuship /= 0.0_dp) then
                           vicL = vicL + vicushp(LL)
                        end if
                     end if
                     if (ja_timestep_auto_visc == 0) then
                        dxiAu = dxi(LL) * Au(L)
                        if (dxiAu > 0.0_dp) then
                           viscocity_max_limit = 0.2_dp * dti * min(vol1(k1), vol1(k2)) / dxiAu
                           if (vicL > viscocity_max_limit) then
                              vicL = viscocity_max_limit ! see Tech Ref.: Limitation of Viscosity Coefficient
                              number_limited_links = number_limited_links + 1
                           end if
                        end if
                     end if
                     vicLu(L) = vicL ! horizontal eddy viscosity applied in mom eq.
                     viu(L) = max(0.0_dp, vicL - vicc) ! modeled turbulent part

                     c11 = cs * cs
                     c12 = cs * sn
                     c22 = sn * sn
                     suxL = duxdn + c11 * duxdn + c12 * (duydn - duxdt) - c22 * duydt
                     suyL = duydn + c11 * duxdt + c12 * (duxdn + duydt) + c22 * duydn
                     suxL = suxL * vicL / wui(LL)
                     suyL = suyL * vicL / wui(LL)
                     if (istresstyp == 3) then
                        hmin = min(zws(k1) - zws(k1 - 1), zws(k2) - zws(k2 - 1))
                        suxL = hmin * suxL
                        suyL = hmin * suyL
                     end if
                     if (jsferic == 1 .and. jasfer3D == 1) then
                        dvxc(k1) = dvxc(k1) + lin2nodx(LL, 1, suxL, suyL)
                        dvyc(k1) = dvyc(k1) + lin2nody(LL, 1, suxL, suyL)
                        dvxc(k2) = dvxc(k2) - lin2nodx(LL, 2, suxL, suyL)
                        dvyc(k2) = dvyc(k2) - lin2nody(LL, 2, suxL, suyL)
                     else
                        dvxc(k1) = dvxc(k1) + suxL
                        dvyc(k1) = dvyc(k1) + suyL
                        dvxc(k2) = dvxc(k2) - suxL
                        dvyc(k2) = dvyc(k2) - suyL
                     end if
                  end do
               end do
            end if
         end if

         if (number_limited_links > 0 .and. number_steps_limited_visc_flux_links <= MAX_PRINTS_LIMITED_VISC_FLUX_LINKS) then
            number_steps_limited_visc_flux_links = number_steps_limited_visc_flux_links + 1
            write (msgbuf, '(a,i0,a)') 'Viscosity coefficient was limited for ', number_limited_links, ' links.'
            call mess(LEVEL_WARN, msgbuf)
         end if

      end if

      if (ihorvic > 0) then

         if (istresstyp == 2 .or. istresstyp == 3) then

            if (kmx == 0) then
               call interpolate_stress_to_links_2D()
            else
               !$OMP PARALLEL DO                       &
               !$OMP PRIVATE(LL,kb1,kb2,Lb,Lt,L,k1,k2,huv)
               do LL = lnx1D + 1, lnx
                  if (hu(LL) > 0.0_dp) then
                     kb1 = ln(1, LL)
                     kb2 = ln(2, LL)
                     call getLbotLtop(LL, Lb, Lt)
                     do L = Lb, Lt
                        k1 = ln(1, L)
                        k2 = ln(2, L)
                        huv = 0.5_dp * ((zws(k1) - zws(k1 - 1)) + (zws(k2) - zws(k2 - 1)))
                        if (huv > epshu) then
                           if (jasfer3D == 1) then
                              suu(L) = acl(LL) * bai(kb1) * (csu(LL) * nod2linx(LL, 1, dvxc(k1), dvyc(k1)) + snu(LL) * nod2liny(LL, 1, dvxc(k1), dvyc(k1))) + &
                                       (1.0_dp - acl(LL)) * bai(kb2) * (csu(LL) * nod2linx(LL, 2, dvxc(k2), dvyc(k2)) + snu(LL) * nod2liny(LL, 2, dvxc(k2), dvyc(k2)))
                           else
                              suu(L) = acl(LL) * bai(kb1) * (csu(LL) * dvxc(k1) + snu(LL) * dvyc(k1)) + &
                                       (1.0_dp - acl(LL)) * bai(kb2) * (csu(LL) * dvxc(k2) + snu(LL) * dvyc(k2))
                           end if

                           if (istresstyp == 3) then
                              suu(L) = suu(L) / huv
                           end if
                        end if
                     end do
                  end if
               end do
               !$OMP END PARALLEL DO
            end if
         end if

         do nw = 1, mxwalls
            k1 = walls(1, nw) ! waterlevel point on the inside
            k3 = walls(2, nw) ! first corner
            k4 = walls(3, nw) ! second corner
            L1 = walls(4, nw) ! link attached to first corner
            L2 = walls(5, nw) ! link attached to second corner
            sf = walls(6, nw) ! ustarfactor, ustar=sf*us
            cs = walls(7, nw) ! sux = -cs*ustar
            sn = walls(8, nw) ! suy = -sn*ustar
            wuw = walls(9, nw) ! width of wall

            if (irov == 1) then ! partial slip
               if (kmx == 0) then
                  if (jasfer3D == 1) then
                     ustar = (cs * nod2wallx(nw, ucx(k1), ucy(k1)) + sn * nod2wally(nw, ucx(k1), ucy(k1))) * sf
                  else
                     ustar = (cs * ucx(k1) + sn * ucy(k1)) * sf
                  end if
                  walls(16, nw) = ustar
                  suxw = -cs * ustar * abs(ustar) * wuw * bai(k1)
                  suyw = -sn * ustar * abs(ustar) * wuw * bai(k1)
                  if (L1 /= 0) then
                     csl = csu(L1)
                     snl = snu(L1)
                     ac1 = walls(10, nw)
                     if (jasfer3D == 1) then
                        suu(L1) = suu(L1) + (csl * wall2linx(nw, 1, suxw, suyw) + snl * wall2liny(nw, 1, suxw, suyw)) * ac1
                     else
                        suu(L1) = suu(L1) + (csl * suxw + snl * suyw) * ac1
                     end if
                  end if
                  if (L2 /= 0) then
                     csl = csu(L2)
                     snl = snu(L2)
                     ac2 = walls(11, nw)
                     if (jasfer3D == 1) then
                        suu(L2) = suu(L2) + (csl * wall2linx(nw, 2, suxw, suyw) + snl * wall2liny(nw, 2, suxw, suyw)) * ac2
                     else
                        suu(L2) = suu(L2) + (csl * suxw + snl * suyw) * ac2
                     end if
                  end if
               else
                  call getkbotktop(k1, kb, kt)
                  if (L1 /= 0) then
                     call getLbotLtop(L1, Lb1, Lt1)
                  end if
                  if (L2 /= 0) then
                     call getLbotLtop(L2, Lb2, Lt2)
                  end if
                  do k = kb, kt
                     if (jasfer3D == 1) then
                        ustar = (cs * nod2wallx(nw, ucx(k), ucy(k)) + sn * nod2wally(nw, ucx(k), ucy(k))) * sf
                     else
                        ustar = (cs * ucx(k) + sn * ucy(k)) * sf
                     end if
                     walls(16, nw) = ustar
                     suxw = -cs * ustar * abs(ustar) * wuw * bai(k1)
                     suyw = -sn * ustar * abs(ustar) * wuw * bai(k1)
                     if (L1 /= 0) then
                        csl = csu(L1)
                        snl = snu(L1)
                        ac1 = walls(10, nw)
                        if (jasfer3D == 1) then
                           suu(Lb1 + k - kb) = suu(Lb1 + k - kb) + (csl * wall2linx(nw, 1, suxw, suyw) + snl * wall2liny(nw, 1, suxw, suyw)) * ac1
                        else
                           suu(Lb1 + k - kb) = suu(Lb1 + k - kb) + (csl * suxw + snl * suyw) * ac1
                        end if
                     end if
                     if (L2 /= 0) then
                        csl = csu(L2)
                        snl = snu(L2)
                        ac2 = walls(11, nw)
                        if (jasfer3D == 1) then
                           suu(Lb2 + k - kb) = suu(Lb2 + k - kb) + (csl * wall2linx(nw, 2, suxw, suyw) + snl * wall2liny(nw, 2, suxw, suyw)) * ac2
                        else
                           suu(Lb2 + k - kb) = suu(Lb2 + k - kb) + (csl * suxw + snl * suyw) * ac2
                        end if
                     end if
                  end do
               end if
            else if (irov == 2) then ! no slip
               if (jasfer3D == 1) then
                  ustar = (cs * nod2wallx(nw, ucx(k1), ucy(k1)) + sn * nod2wally(nw, ucx(k1), ucy(k1)))
               else
                  ustar = (cs * ucx(k1) + sn * ucy(k1)) ! component parallel to wall
               end if

               walls(16, nw) = 0.0_dp

               if (javiusp == 1) then
                  vicl = viusp(L)
               else
                  vicl = vicouv
               end if

               delty = ba(k1) / wuw ! cell area / wall width is distance between internal point and mirror point
               delty = 0.5_dp * delty
               suxw = -(cs * ustar * vicl / delty) * wuw * bai(k1)
               suyw = -(sn * ustar * vicl / delty) * wuw * bai(k1)
               if (L1 /= 0) then
                  csl = csu(L1)
                  snl = snu(L1)
                  ac1 = walls(10, nw)
                  if (jasfer3D == 1) then
                     suu(L1) = suu(L1) + (csl * wall2linx(nw, 1, suxw, suyw) + snl * wall2liny(nw, 1, suxw, suyw)) * ac1
                  else
                     suu(L1) = suu(L1) + (csl * suxw + snl * suyw) * ac1
                  end if
               end if
               if (L2 /= 0) then
                  csl = csu(L2)
                  snl = snu(L2)
                  ac2 = walls(11, nw)
                  if (jasfer3D == 1) then
                     suu(L2) = suu(L2) + (csl * wall2linx(nw, 2, suxw, suyw) + snl * wall2liny(nw, 2, suxw, suyw)) * ac2
                  else
                     suu(L2) = suu(L2) + (csl * suxw + snl * suyw) * ac2
                  end if
               end if
            else if (irov == 0) then ! free slip
               if (jasfer3D == 1) then
                  walls(16, nw) = (cs * nod2wallx(nw, ucx(k1), ucy(k1)) + sn * nod2wally(nw, ucx(k1), ucy(k1)))
                  walls(16, nw) = cs * ucx(k1) + sn * ucy(k1)
               end if
            end if
         end do
         if (izbndpos == 0) then
            do L = lnxi + 1, lnx ! quick fix for open boundaries
               suu(L) = 2.0_dp * suu(L)
            end do
         end if
         adve = adve - suu
      end if
      call timstop(handle_umod)

   end subroutine setumod

   !> Compute tangential velocity component for Coriolis force
   !! Transforms velocity from global frame to link frame, then projects to tangential direction
   elemental function compute_tangential_velocity_spherical(ux_node, uy_node, csb_node, snb_node, csu_link, snu_link) result(tangential)
      real(dp), intent(in) :: ux_node !< x-velocity at node (global frame)
      real(dp), intent(in) :: uy_node !< y-velocity at node (global frame)
      real(dp), intent(in) :: csb_node !< cosine of node-to-link transformation
      real(dp), intent(in) :: snb_node !< sine of node-to-link transformation
      real(dp), intent(in) :: csu_link !< cosine of link direction (for tangential projection)
      real(dp), intent(in) :: snu_link !< sine of link direction (for tangential projection)
      real(dp) :: tangential

      real(dp) :: ux_link, uy_link ! Velocity in link-local frame

      ! Step 1: Transform from global to link-local frame
      ux_link = csb_node * ux_node + snb_node * uy_node ! link-x (normal)
      uy_link = -snb_node * ux_node + csb_node * uy_node ! link-y (tangential in link frame)

      ! Step 2: Project to tangential direction in global frame
      tangential = -snu_link * ux_link + csu_link * uy_link

   end function compute_tangential_velocity_spherical

   !> Compute viscosity and stress for 2D links (vectorized proof of concept)
   !! Computes for ALL links, caller zeros dry links afterward
   subroutine compute_viscosity_and_stress_vectorized(Smagorinsky, Elder, vicouv, javiusp, nshiptxy, vicuship, ja_timestep_auto_visc, jawave, swave, roller, nuhfac, rhomean, jsferic, jasfer3D)
      use precision, only: dp
      use m_flowgeom, only: lnx, lnx1d, ln, acl, wui, dx, csu, snu, wu
      use m_physcoef, only: vonkar, sag
      use m_flow, only: vol1, hu, u1, v, frcu, ifrcutp, vicushp, viclu, viu, viusp, dvxc, dvyc
      use m_get_chezy, only: get_chezy
      use m_prefetch, only: csbn_1, snbn_1, csbn_2, snbn_2, csb_1, csb_2, snb_1, snb_2, uxcorner_1, uycorner_1, uxcorner_2, uycorner_2
      use m_prefetch, only: ucx_1, ucy_1, ucx_2, ucy_2
      use m_xbeach_data, only: DR
      use m_waveconst, only: WAVE_SURFBEAT
      use m_flowtimes, only: dti

      real(dp), intent(in) :: Smagorinsky, Elder, vicouv, vicuship, nuhfac, rhomean
      integer, intent(in) :: javiusp, nshiptxy, ja_timestep_auto_visc
      integer, intent(in) :: jawave, swave, roller, jsferic, jasfer3D

      real(kind=dp) :: vksag6

      integer :: L, L1, L2, k1, k2
      real(kind=dp) :: vicc, vicl, dxiAu, viscosity_max_limit
      real(kind=dp) :: nuhroller
      real(kind=dp) :: shearvar
      real(kind=dp) :: suxL, suyL
      real(kind=dp) :: dundn, dutdn, dundt, dutdt
      real(kind=dp) :: duxdn, duydn, duxdt, duydt
      real(kind=dp) :: c11, c12, c22
      real(kind=dp), dimension(:), allocatable, save :: dvx1, dvy1, dvx2, dvy2, volmin, drl, chezy_elder
      real(kind=dp) :: wuiL, wuL, dxiL, dxL
      real(kind=dp) :: ucnx_link_1, ucny_link_1, ucnx_link_2, ucny_link_2
      real(kind=dp) :: ucx_link_1, ucy_link_1, ucx_link_2, ucy_link_2
      real(kind=dp) :: dvx1_, dvy1_, dvx2_, dvy2_

      L1 = lnx1D + 1
      L2 = lnx
      vksag6 = vonkar * sag / 6.0_dp

      if (.not. allocated(dvx1)) then
         allocate (dvx1(lnx))
         allocate (dvy1(lnx))
         allocate (dvx2(lnx))
         allocate (dvy2(lnx))
         allocate (volmin(lnx))
      end if

      !precompute indirect volumes and conditionals (visc_limit)
      if (ja_timestep_auto_visc == 0) then
         do L = L1, L2
            k1 = ln(1, L)
            k2 = ln(2, L)
            volmin(L) = min(vol1(k1), vol1(k2))
         end do
      end if
      if ((jawave == WAVE_SURFBEAT) .and. (swave == 1) .and. (roller == 1)) then
         do L = L1, L2
            k1 = ln(1, L)
            k2 = ln(2, L)
            drl(L) = acL(L) * DR(k1) + (1 - acL(L)) * DR(k2)
         end do
      end if
      if (Elder > 0.0_dp) then !  add Elder
         chezy_elder = get_chezy(hu, frcu, u1, v, ifrcutp)
         chezy_elder = Elder * (vksag6 / chezy_elder) * (hu) * sqrt(u1**2 + v**2)
      end if

      !$OMP SIMD
      do L = L1, L2
         vicL = 0.0_dp
         wuiL = wui(L)
         wuL = 1.0_dp / wuiL
         dxL = dx(L)
         dxiL = 1.0_dp / dxL
         if (Elder > 0.0_dp) then !  add Elder
            vicL = vicL + chezy_elder(L)
         end if

         if (jsferic == 1 .and. jasfer3D == 1) then
            ucx_link_1 = +csb_1(L) * ucx_1(L) + snb_1(L) * ucy_1(L)
            ucy_link_1 = -snb_1(L) * ucx_1(L) + csb_1(L) * ucy_1(L)
            ucx_link_2 = +csb_2(L) * ucx_2(L) + snb_2(L) * ucy_2(L)
            ucy_link_2 = -snb_2(L) * ucx_2(L) + csb_2(L) * ucy_2(L)
         else
            ucx_link_1 = ucx_1(L)
            ucy_link_1 = ucy_1(L)
            ucx_link_2 = ucx_2(L)
            ucy_link_2 = ucy_2(L)
         end if
         duxdn = (ucx_link_2 - ucx_link_1) * dxiL
         duydn = (ucy_link_2 - ucy_link_1) * dxiL

         if (jsferic == 1 .and. jasfer3D == 1) then
            ucnx_link_1 = +csbn_1(L) * uxcorner_1(L) + snbn_1(L) * uycorner_1(L)
            ucny_link_1 = -snbn_1(L) * uxcorner_1(L) + csbn_1(L) * uycorner_1(L)
            ucnx_link_2 = +csbn_2(L) * uxcorner_2(L) + snbn_2(L) * uycorner_2(L)
            ucny_link_2 = -snbn_2(L) * uxcorner_2(L) + csbn_2(L) * uycorner_2(L)
         else
            ucnx_link_1 = uxcorner_1(L)
            ucny_link_1 = uycorner_1(L)
            ucnx_link_2 = uxcorner_2(L)
            ucny_link_2 = uycorner_2(L)
         end if
         duxdt = (ucnx_link_2 - ucnx_link_1) * wuiL
         duydt = (ucny_link_2 - ucny_link_1) * wuiL

         if (Smagorinsky > 0) then
            dundn = +csu(L) * duxdn + snu(L) * duydn
            dutdn = -snu(L) * duxdn + csu(L) * duydn
            dundt = +csu(L) * duxdt + snu(L) * duydt
            dutdt = -snu(L) * duxdt + csu(L) * duydt

            shearvar = 2.0_dp * (dundn**2 + dutdt**2 + dundt * dutdn) + dundt**2 + dutdn**2
            vicL = vicL + merge(Smagorinsky**2 * sqrt(shearvar) * dxL * wuL, 0.0_dp, shearvar > 1.0e-15_dp)
         end if

         if (nshiptxy > 0 .and. vicuship /= 0.0_dp) then
            vicL = vicL + vicushp(L)
         end if

         ! JRE: add roller induced viscosity
         if ((jawave == WAVE_SURFBEAT) .and. (swave == 1) .and. (roller == 1)) then
            nuhroller = nuhfac * hu(L) * (drl(L) / rhomean)**(1.0_dp / 3.0_dp)
            vicL = max(nuhroller, vicL)
         end if
         vicc = 0.0_dp
         if (javiusp == 1) then
            vicc = vicc + viusp(L)
         else
            vicc = vicc + vicouv
         end if
         vicL = vicL + vicc

         if (ja_timestep_auto_visc == 0) then
            dxiAu = dxiL * hu(L) * wu(L)
            if (dxiAu > 0.0_dp) then
               viscosity_max_limit = 0.2_dp * dti * volmin(L) / dxiAu
               if (vicL > viscosity_max_limit) then
                  vicL = viscosity_max_limit ! see Tech Ref.: Limitation of Viscosity Coefficient
               end if
            end if
         end if

         c11 = csu(L)**2
         c12 = csu(L) * snu(L)
         c22 = snu(L)**2

         suxL = (duxdn + c11 * duxdn + c12 * (duydn - duxdt) - c22 * duydt) * vicL * hmin_(L) * wuL !> hmin multiplication if istresstyp = 3 (always!)
         suyL = (duydn + c11 * duxdt + c12 * (duxdn + duydt) + c22 * duydn) * vicL * hmin_(L) * wuL
         if (jsferic == 1 .and. jasfer3D == 1) then
            dvx1_ = +(csb_1(L) * suxL - snb_1(L) * suyL)
            dvy1_ = +(snb_1(L) * suxL + csb_1(L) * suyL)
            dvx2_ = -(csb_2(L) * suxL - snb_2(L) * suyL)
            dvy2_ = -(snb_2(L) * suxL + csb_2(L) * suyL)
         else
            dvx1_ = suxL
            dvy1_ = suyL
            dvx2_ = -suxL
            dvy2_ = -suyL
         end if
         if (hu(L) > 0) then
            vicLU(L) = vicL ! Total viscosity
            viu(L) = max(0.0_dp, vicL - vicc) ! Modeled turbulent part only
         end if
         dvx1(L) = merge(dvx1_, 0.0_dp, hu(L) > 0)
         dvy1(L) = merge(dvy1_, 0.0_dp, hu(L) > 0)
         dvx2(L) = merge(dvx2_, 0.0_dp, hu(L) > 0)
         dvy2(L) = merge(dvy2_, 0.0_dp, hu(L) > 0)

      end do
      dvxc = 0.0_dp
      dvyc = 0.0_dp
      do L = L1, L2 !> accumulate link stress at nodes
         k1 = ln(1, L)
         k2 = ln(2, L)

         dvxc(k1) = dvxc(k1) + dvx1(L)
         dvyc(k1) = dvyc(k1) + dvy1(L)
         dvxc(k2) = dvxc(k2) + dvx2(L)
         dvyc(k2) = dvyc(k2) + dvy2(L)
      end do
   end subroutine compute_viscosity_and_stress_vectorized

   !> Set tangential velocity v(L) for newcorio=1, kmx=0 case
   subroutine set_V_2D_default(jasfer3D)
      use precision, only: dp
      use m_flow, only: hu, v
      use m_flowgeom, only: lnx, lnx1D, csu, snu, acL
      use m_prefetch, only: csb_1, snb_1, csb_2, snb_2, ucx_1, ucy_1, ucx_2, ucy_2

      integer, intent(in) :: jasfer3D !> jasfer3D flag for coordinate transformation, should match m_sferic jasfer3D

      integer :: L
      real(kind=dp) :: cs, sn, acL_LL, acL_iv
      real(kind=dp) :: ucx_link_1, ucy_link_1, ucx_link_2, ucy_link_2
      real(kind=dp) :: v_

      !$OMP SIMD
      do L = lnx1D + 1, lnx
         if (jasfer3D) then
            ucx_link_1 = +csb_1(L) * ucx_1(L) + snb_1(L) * ucy_1(L)
            ucy_link_1 = -snb_1(L) * ucx_1(L) + csb_1(L) * ucy_1(L)
            ucx_link_2 = +csb_2(L) * ucx_2(L) + snb_2(L) * ucy_2(L)
            ucy_link_2 = -snb_2(L) * ucx_2(L) + csb_2(L) * ucy_2(L)
         else
            ucx_link_1 = ucx_1(L)
            ucy_link_1 = ucy_1(L)
            ucx_link_2 = ucx_2(L)
            ucy_link_2 = ucy_2(L)
         end if
         acL_LL = acL(L)
         acL_iv = 1.0_dp - acL_LL
         cs = csu(L)
         sn = snu(L)
         v_ = acL_LL * (-sn * ucx_link_1 + cs * ucy_link_1) + &
              acL_iv * (-sn * ucx_link_2 + cs * ucy_link_2)
         if (hu(L) > 0.0_dp) then
            v(L) = v_
         end if
      end do
   end subroutine set_V_2D_default

   !> Interpolate accumulated node stresses back to link centers for 2D case
   !! Projects dvxc/dvyc (stress at nodes) to suu (stress at links)
   subroutine interpolate_stress_to_links_2D()
      use precision, only: dp
      use m_flow, only: hu, hs, suu, dvxc, dvyc
      use m_flowgeom, only: lnx, lnx1D, ln, csu, snu, acl
      use m_flowparameters, only: epshu
      use m_sferic, only: jasfer3D
      use m_prefetch, only: csb_1, snb_1, csb_2, snb_2, bai_1, bai_2

      integer :: L, k1, k2
      real(kind=dp) :: huv, acl_L, acl_iv
      real(kind=dp) :: dvx_link_1, dvy_link_1, dvx_link_2, dvy_link_2
      real(kind=dp) :: suu_1, suu_2

      ! no pre-gather for vectorisation 8 loads & stores & loads is not worth it!
      do L = lnx1D + 1, lnx
         if (hu(L) > 0) then
            k1 = ln(1, L)
            k2 = ln(2, L)
            huv = 0.5_dp * (hs(k1) + hs(k2))
            if (huv > epshu) then
               acl_L = acl(L)
               acl_iv = 1.0_dp - acl_L

               if (jasfer3D == 1) then
                  dvx_link_1 = csb_1(L) * dvxc(k1) - snb_1(L) * dvyc(k1)
                  dvy_link_1 = snb_1(L) * dvxc(k1) + csb_1(L) * dvyc(k1)
                  dvx_link_2 = csb_2(L) * dvxc(k2) - snb_2(L) * dvyc(k2)
                  dvy_link_2 = snb_2(L) * dvxc(k2) + csb_2(L) * dvyc(k2)

                  suu_1 = csu(L) * dvx_link_1 + snu(L) * dvy_link_1
                  suu_2 = csu(L) * dvx_link_2 + snu(L) * dvy_link_2
               else
                  suu_1 = csu(L) * dvxc(k1) + snu(L) * dvyc(k1)
                  suu_2 = csu(L) * dvxc(k2) + snu(L) * dvyc(k2)
               end if

               suu(L) = acl_L * bai_1(L) * suu_1 + acl_iv * bai_2(L) * suu_2

               !if (istresstyp == 3) then
               suu(L) = suu(L) / huv
               !end if
            end if
         end if
      end do

   end subroutine interpolate_stress_to_links_2D

!> Compute Coriolis correction for 2D links in the default case (icorio <= 20, kmx=0)
!! Applies Coriolis force to adve(:) and updates fvcoro(:)
!! Module arrays accessed (read-only):
!!   - hu, acL, csu, snu, ln, fcori
!!   - ucxq_1, ucyq_1, ucxq_2, ucyq_2, csb_1, snb_1, csb_2, snb_2
!!   - hmin_ (module-level cache)
!!
!! Module arrays modified:
!!   - adve, fvcoro
   subroutine compute_coriolis_correction_2D_default(icorio, jsferic, jasfer3D, jacorioconstant, fcorio, trshcorio, Corioadamsbashfordfac)

      use precision, only: dp
      use m_flowgeom, only: ln, acL, csu, snu, lnx, lnx1D
      use m_flow, only: hu, adve, fvcoro, fcori
      use m_prefetch, only: ucxq_1, ucyq_1, ucxq_2, ucyq_2, csb_1, snb_1, csb_2, snb_2

      !> intent in conditionals so that compiler can assume constants and optimize accordingly
      integer, intent(in) :: icorio !< coriolis type, should match m_flowparameters icorio
      integer, intent(in) :: jsferic !< sferic type, should match m_sferic jsferic
      integer, intent(in) :: jasfer3D !< 3D sferic coordinates flag, should match m_sferic jasfer3D
      integer, intent(in) :: jacorioconstant !< is coriolis constnant in space? should match m_flow jacorioconstant
      real(dp), intent(in) :: fcorio !< Coriolis parameter, should match m_sferic fcorio
      real(dp), intent(in) :: trshcorio !< Coriolis threshold, should match m_flowparameters trshcorio
      real(dp), intent(in) :: Corioadamsbashfordfac !< Coriolis Adams-Bashford factor, should match m_flowparameters Corioadamsbashfordfac

      integer :: L, k1, k2
      logical :: spatial_coriolis
      real(dp) :: fvcor

      real(dp), allocatable, dimension(:), save :: fcor1_, fcor2_
      real(dp) :: fcor1, fcor2

      spatial_coriolis = (jsferic > 0 .or. jacorioconstant > 0)
      if (.not. spatial_coriolis .and. fcorio == 0.0_dp) then
         return ! early exit if Coriolis force is zero
      end if

      if (spatial_coriolis .and. (icorio /= 5)) then
         ! Node-based fcor (zeta-based)
         if (.not. allocated(fcor1_)) then
            allocate (fcor1_(lnx), fcor2_(lnx))
            do L = lnx1D + 1, lnx
               k1 = ln(1, L)
               k2 = ln(2, L)
               fcor1_(L) = fcori(k1)
               fcor2_(L) = fcori(k2)
            end do
         end if
      end if

      !$OMP SIMD
      do L = lnx1D + 1, lnx
         if (spatial_coriolis .and. icorio /= 5) then
            fcor1 = fcor1_(L)
            fcor2 = fcor2_(L)
         else if (spatial_coriolis .and. icorio == 5) then
            fcor1 = fcori(L)
            fcor2 = fcori(L)
         else
            fcor1 = fcorio
            fcor2 = fcorio
         end if
         if (jasfer3D == 1) then
            fvcor = calculate_coriolis_force_spherical_3D(fcor1, fcor2, acL(L), csu(L), snu(L), ucxq_1(L), ucyq_1(L), ucxq_2(L), ucyq_2(L), csb_1(L), snb_1(L), csb_2(L), snb_2(L), hmin_(L), trshcorio)
         else
            fvcor = calculate_coriolis_force(fcor1, fcor2, acL(L), csu(L), snu(L), ucxq_1(L), ucyq_1(L), ucxq_2(L), ucyq_2(L), hmin_(L), trshcorio)
         end if
         if (hu(L) > 0.0_dp) then
            adve(L) = adve(L) - fvcor

            if (Corioadamsbashfordfac > 0.0_dp) then
               if (fvcoro(L) /= 0.0_dp) then
                  adve(L) = adve(L) - Corioadamsbashfordfac * (fvcor - fvcoro(L))
               end if
               fvcoro(L) = fvcor
            end if
         end if
      end do

   end subroutine compute_coriolis_correction_2D_default

!> Apply Coriolis correction for a single link, using "spherical 3D" mode for the tangential velocity calculation.
!! This elemental function will be inlined to allow vectorization by the compiler
   elemental function calculate_coriolis_force_spherical_3D(fcor_1, fcor_2, &
                                                         acL_L, csu_L, snu_L, &
                                                         ucxq_1_L, ucyq_1_L, ucxq_2_L, ucyq_2_L, &
                                                         csb_1_L, snb_1_L, csb_2_L, snb_2_L, &
                                                         hmin_L, trshcorio) result(fvcor)

      real(dp), intent(in) :: fcor_1 !< Coriolis parameter at node 1
      real(dp), intent(in) :: fcor_2 !< Coriolis parameter at node 2
      real(dp), intent(in) :: acL_L !< Link interpolation weight for node 1
      real(dp), intent(in) :: csu_L !< Cosine of link orientation
      real(dp), intent(in) :: snu_L !< Sine of link orientation
      real(dp), intent(in) :: ucxq_1_L !< x-velocity at node 1 (global frame)
      real(dp), intent(in) :: ucyq_1_L !< y-velocity at node 1 (global frame)
      real(dp), intent(in) :: ucxq_2_L !< x-velocity at node 2 (global frame)
      real(dp), intent(in) :: ucyq_2_L !< y-velocity at node 2 (global frame)
      real(dp), intent(in) :: csb_1_L !< Cosine of node 1 to link transformation
      real(dp), intent(in) :: snb_1_L !< Sine of node 1 to link transformation
      real(dp), intent(in) :: csb_2_L !< Cosine of node 2 to link transformation
      real(dp), intent(in) :: snb_2_L !< Sine of node 2 to link transformation
      real(dp), intent(in) :: hmin_L !< Minimum water depth at link endpoints
      real(dp), intent(in) :: trshcorio !< Depth threshold for Coriolis scaling

      real(dp) :: fvcor
      real(dp) :: tangential_1, tangential_2

      tangential_1 = compute_tangential_velocity_spherical(ucxq_1_L, ucyq_1_L, csb_1_L, snb_1_L, csu_L, snu_L)
      tangential_2 = compute_tangential_velocity_spherical(ucxq_2_L, ucyq_2_L, csb_2_L, snb_2_L, csu_L, snu_L)

      ! Compute Coriolis force
      fvcor = acL_L * tangential_1 * fcor_1 + (1.0_dp - acL_L) * tangential_2 * fcor_2

      ! Apply depth threshold
      if (trshcorio > 0.0_dp .and. hmin_L < trshcorio) then
         fvcor = fvcor * hmin_L / trshcorio
      end if

   end function calculate_coriolis_force_spherical_3D
!> Apply Coriolis correction for a single link
!! This elemental function will be inlined to allow vectorization by the compiler
   elemental function calculate_coriolis_force(fcor_1, fcor_2, &
                                               acL_L, csu_L, snu_L, &
                                               ucxq_1_L, ucyq_1_L, ucxq_2_L, ucyq_2_L, &
                                               hmin_L, trshcorio) result(fvcor)

      real(dp), intent(in) :: fcor_1 !< Coriolis parameter at node 1
      real(dp), intent(in) :: fcor_2 !< Coriolis parameter at node 2
      real(dp), intent(in) :: acL_L !< Link interpolation weight for node 1
      real(dp), intent(in) :: csu_L !< Cosine of link orientation
      real(dp), intent(in) :: snu_L !< Sine of link orientation
      real(dp), intent(in) :: ucxq_1_L !< x-velocity at node 1 (global frame)
      real(dp), intent(in) :: ucyq_1_L !< y-velocity at node 1 (global frame)
      real(dp), intent(in) :: ucxq_2_L !< x-velocity at node 2 (global frame)
      real(dp), intent(in) :: ucyq_2_L !< y-velocity at node 2 (global frame)
      real(dp), intent(in) :: hmin_L !< Minimum water depth at link endpoints
      real(dp), intent(in) :: trshcorio !< Depth threshold for Coriolis scaling

      real(dp) :: fvcor
      real(dp) :: tangential_1, tangential_2

      tangential_1 = -snu_L * ucxq_1_L + csu_L * ucyq_1_L
      tangential_2 = -snu_L * ucxq_2_L + csu_L * ucyq_2_L

      ! Compute Coriolis force
      fvcor = acL_L * tangential_1 * fcor_1 + (1.0_dp - acL_L) * tangential_2 * fcor_2

      ! Apply depth threshold
      if (trshcorio > 0.0_dp .and. hmin_L < trshcorio) then
         fvcor = fvcor * hmin_L / trshcorio
      end if

   end function calculate_coriolis_force

end module m_setumod
