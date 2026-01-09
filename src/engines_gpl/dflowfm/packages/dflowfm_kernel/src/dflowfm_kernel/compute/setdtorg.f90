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

module m_setdtorg

   implicit none

   private

   public :: setdtorg

contains

   subroutine setdtorg(jareduced) ! set computational timestep dts
      use precision, only: dp
      use m_flowgeom, only: ndx, lnx1d, iadv, iadv_general_structure, ln, ndxi, lnxi, lnx, dxi, wu, ba, kcu, lncn
      use m_flow, only: jamapdtcell, plotlin, kkcflmx, kcflmx, itstep, squcor, squ, q1, qin, eps10, hs, epshu, vol1, jamapflowanalysis, flowcourantnumber, cflmx, sqwave, sqi, squ2d, rho, ag, hu, au, ihorvic, kmx, istresstyp, viclu, zws, limitingtimestepestimation
      use m_flowtimes, only: dtcell, autotimestep, AUTO_TIMESTEP_OFF, AUTO_TIMESTEP_2D_OUT, AUTO_TIMESTEP_2D_INOUT, AUTO_TIMESTEP_3D_HOR_OUT, AUTO_TIMESTEP_3D_HOR_INOUT, & 
         AUTO_TIMESTEP_3D_INOUT, AUTO_TIMESTEP_3D_HOR_OUT_TOTAL_IN, AUTO_TIMESTEP_3D_INOUT_BAROCLINE, AUTO_TIMESTEP_3D_OUT_NOTOP, AUTO_TIMESTEP_3D_HOR_OUT_TOTAL_IN_NOTOP, &
         dt_max, dts, ja_timestep_nostruct, ja_timestep_noqout, dtsc, ja_timestep_auto_visc
      use m_partitioninfo, only: jampi, idomain, my_rank
      use m_drawthis, only: ndraw
      use m_get_kbot_ktop, only: getkbotktop
      use m_get_Lbot_Ltop, only: getlbotltop

      integer, intent(out) :: jareduced ! maximum time-step is already globally reduced (1) or not (0)

      ! locals
      integer :: L, LL, k, n1, n2, kk, kb, kt, k1, k2, k3, k4, Lb, Lt
      integer :: kk1, kk2
      real(kind=dp) :: rhomin, rhomax, cbaroc, drho
      real(kind=dp) :: dtsc2D

      real(kind=dp) :: dxiAu !
      real(kind=dp) :: huv
      real(kind=dp) :: dtsc1, dtsc2
      real(kind=dp) :: squloc

      jareduced = 0
      if (jamapdtcell > 0) then
         dtcell = 0.0_dp
      end if

      ! Compute dt when automic timestepping is on
      if (autotimestep /= AUTO_TIMESTEP_OFF) then

         if (NDRAW(28) == 30 .or. NDRAW(29) == 38) then
            plotlin = dt_max
         end if

         dts = 1.0e9_dp
         kkcflmx = 0
         kcflmx = 0

         ! Supported parameters 
         !----------------------

         ! 2D outflow only
         if (autotimestep == AUTO_TIMESTEP_2D_OUT) then

            if (itstep /= 4) then ! Non-explicit time-step
               if (ja_timestep_nostruct > 0) then ! Exclude (structure) links without advection from the time step limitation
                  squcor(1:ndx) = squ(1:ndx) ! Initialize corrected outgoing flux with already computed squ
                  do L = 1, lnx1d ! Go through all flow links
                     if (iadv(L) /= 0 .and. iadv(L) /= IADV_GENERAL_STRUCTURE) then
                        cycle ! Do not exclude this link
                     end if

                     ! Get indices of the two linked cells
                     k1 = ln(1, L)
                     k2 = ln(2, L)
                     
                     ! Undo some of the added q1 contributions in squ (as produced by u1q1()).
                     if (q1(L) > 0) then
                        squcor(k1) = squcor(k1) - q1(L)
                     else if (q1(L) < 0) then
                        squcor(k2) = squcor(k2) + q1(L)
                     end if
                  end do
               end if

               do k = 1, ndxi ! Go through all internal flow cells
                  if (jampi == 1) then 
                     if (idomain(k) /= my_rank) then
                        cycle ! Do not include ghost cells
                     end if
                  end if

                  if (ja_timestep_nostruct > 0) then ! Exclude (structure) links without advection from the time step limitation
                     squloc = squcor(k)
                  else
                     squloc = squ(k)
                  end if

                  if (ja_timestep_noqout > 0) then ! Do not include negative qin in timestep
                     squloc = squloc - max(-qin(k), 0.0_dp)
                  end if

                  if (squloc > eps10) then ! Check if outgoing flux is present
                     if (hs(k) > epshu .and. vol1(k) > 0.0) then ! Check if cell is wet
                        if (jamapFlowAnalysis > 0) then
                           ! The flowCourantNumber will be multiplied by dt on a later stage
                           flowCourantNumber(k) = squloc / vol1(k)
                        end if

                        dtsc = cflmx * vol1(k) / squloc ! Compute maximum cell timestep based on outgoing flux (2D)

                        if (jamapdtcell > 0) then ! Store cell timestep if required
                           dtcell(k) = dtsc
                        end if

                        if (dtsc < dts) then ! Check if timestep of cell is globally limiting and store cell index
                           dts = dtsc
                           kkcflmx = k
                        end if

                        if (dtsc == 0.0_dp) then ! Set cell to globally limiting if timestep is equal to zero
                           kkcflmx = k
                        end if
                     end if
                  end if
               end do
               continue

            else ! Explicit time-step
               do k = 1, ndxi ! Go through all internal flow cells
                  if (jampi == 1) then 
                     if (idomain(k) /= my_rank) then
                        cycle ! Do not include ghost cells
                     end if
                  end if
                  if (sqwave(k) > eps10) then ! Check if flux is outgoing
                     if (hs(k) > epshu) then ! Check if cell is wet
                        dtsc = cflmx * vol1(k) / sqwave(k) ! Compute maximum cell timestep
                        
                        if (jamapdtcell > 0) then ! Store cell timestep if required
                           dtcell(k) = dtsc
                        end if

                        if (dtsc < dts) then ! Check if timestep of cell is globally limiting and store cell index
                           dts = dtsc
                           kkcflmx = k
                        end if
                     end if
                  end if
               end do

            end if

         ! 3D horizontal outflow or inflow+outflow
         else if (autotimestep == AUTO_TIMESTEP_3D_HOR_OUT .or. autotimestep == AUTO_TIMESTEP_3D_HOR_INOUT) then

            do kk = 1, ndxi ! Go through all 2D internal flow cells
               if (jampi == 1) then
                  if (idomain(kk) /= my_rank) then
                     cycle ! Do not include ghost cells
                  end if
               end if

               if (squ2D(kk) > eps10) then ! Check if horizontal flux is present
                  if (hs(kk) > epshu) then ! Check if cell is wet
                     call getkbotktop(kk, kb, kt) ! Get bottom and top layer index of cell
                     do k = kb, kt ! Go through all vertical layers
                        if (squ2d(k) > eps10) then ! Check if local horizontal flux is present
                           dtsc = cflmx * vol1(k) / squ2d(k) ! Compute maximum cell timestep based on horizontal flux (3D)

                           if (jamapdtcell > 0) then ! Store cell timestep if required
                              dtcell(k) = dtsc
                           end if

                           if (dtsc < dts) then ! Check if timestep of cell is globally limiting and store cell index
                              dts = dtsc
                              kkcflmx = kk
                              kcflmx = k
                           end if
                        end if
                     end do
                  end if
               end if
            end do

         ! 3D inflow or outflow
         else if (autotimestep == AUTO_TIMESTEP_3D_INOUT) then

            do kk = 1, ndxi ! Go through all 2D internal flow cells
               if (jampi == 1) then
                  if (idomain(kk) /= my_rank) then
                     cycle ! do not include ghost cells
                  end if
               end if

               if (hs(kk) > epshu) then ! Check if cell is wet
                  call getkbotktop(kk, kb, kt) ! Get bottom and top layer index of cell
                  do k = kb, kt ! Go through all vertical layers
                     if (squ(k) > eps10 .or. sqi(k) > eps10) then ! Check if there is an incoming or outgoing flux present
                        dtsc = cflmx * vol1(k) / max(squ(k), sqi(k)) ! Compute maximum cell timestep based on largest flux

                        if (jamapdtcell > 0) then ! Store cell timestep if required
                           dtcell(k) = dtsc
                        end if
                        if (dtsc < dts) then
                           dts = dtsc
                           kkcflmx = kk
                           kcflmx = k
                        end if
                     end if
                  end do
               end if
            end do


         ! Research parameters
         !---------------------

         ! 2D in+outflow
         else if (autotimestep == AUTO_TIMESTEP_2D_INOUT) then

            do k = 1, ndxi ! Go through all internal flow cells
               if (jampi == 1) then 
                  if (idomain(k) /= my_rank) then
                     cycle ! do not include ghost cells
                  end if
               end if

               if (squ(k) + sqi(k) > eps10) then ! Check if the total flux is positive
                  if (hs(k) > epshu .and. vol1(k) > 0.0) then ! Check if cell is wet
                     dtsc = cflmx * vol1(k) / (squ(k) + sqi(k)) ! Compute maximum cell timestep based on total flux (2D)

                     if (jamapdtcell > 0) then ! Store cell timestep if required
                        dtcell(k) = dtsc
                     end if

                     if (dtsc < dts) then ! Check if timestep of cell is globally limiting and store cell index
                        dts = dtsc
                        kkcflmx = k
                     end if
                  end if
               end if
            end do

         ! 3D horizontal outflow + total inflow
         else if (autotimestep == AUTO_TIMESTEP_3D_HOR_OUT_TOTAL_IN) then

            do kk = 1, ndxi ! Go through all 2D internal flow cells
               if (jampi == 1) then
                  if (idomain(kk) /= my_rank) then
                     cycle ! do not include ghost cells
                  end if
               end if

               if (hs(kk) > epshu) then ! Check if cell is wet
                  dtsc2D = dt_max

                  if (squ(kk) > eps10) then ! Check if flux is outgoing
                     dtsc2D = cflmx * vol1(kk) / squ(kk) ! Compute maximum cell timestep based on horizontal outflow (2D)
                  end if

                  call getkbotktop(kk, kb, kt) ! Get bottom and top layer index of cell
                  do k = kb, kt ! Go through all vertical layers
                     if (sqi(k) > eps10) then ! Check if flux is incoming
                        dtsc = cflmx * vol1(k) / sqi(k) ! Compute maximum cell timestep based on inflow (3D)
                        dtsc = min(dtsc, dtsc2D) ! Take the most limiting timestep (2D/3D)

                        if (jamapdtcell > 0) then ! Store cell timestep if required
                           dtcell(k) = dtsc
                        end if

                        if (dtsc < dts) then ! Check if timestep of cell is globally limiting and store cell index
                           dts = dtsc
                           kkcflmx = kk
                        end if
                     end if
                  end do
               end if
            end do

         ! 3D inflow+outflow plus barocline effects
         else if (autotimestep == AUTO_TIMESTEP_3D_INOUT_BAROCLINE) then
            ! First compute baroclinic contributions to fluxes, second compute timestep based on total fluxes

            do LL = 1, lnxi ! Go through all flow links
               ! Get indices of the two linked cells
               n1 = ln(1, LL)
               n2 = ln(2, LL) 

               call getLbotLtop(LL, Lb, Lt) ! Get bottom and top layer index of link

               ! Define minimum and maximum density
               rhomin = 2.0e3_dp
               rhomax = -1.0_dp

               do L = Lb, Lt ! Go through all vertical layers of link
                  ! Get indices of the two linked cells
                  k1 = ln(1, L)
                  k2 = ln(2, L)

                  ! Check for new minimum and maximum density
                  rhomin = min(rhomin, rho(k1), rho(k2))
                  rhomax = max(rhomax, rho(k1), rho(k2))
               end do

               drho = rhomax - rhomin ! Compute density difference
               cbaroc = sqrt(0.25_dp * ag * hu(LL) * drho * 0.001_dp) ! Compute baroclinic wave speed (m/s)

               ! Add baroclinic contribution to fluxes in both cells of the link
               do L = Lb, Lt
                  squ(n1) = squ(n1) + au(L) * cbaroc
                  squ(n2) = squ(n2) + au(L) * cbaroc
                  sqi(n1) = sqi(n1) + au(L) * cbaroc
                  sqi(n2) = sqi(n2) + au(L) * cbaroc
               end do
            end do

            do kk = 1, ndxi ! Go through all 2D internal flow cells
               if (jampi == 1) then
                  if (idomain(kk) /= my_rank) then
                     cycle ! do not include ghost cells
                  end if
               end if

               if (hs(kk) > epshu) then ! Check if cell is wet
                  call getkbotktop(kk, kb, kt) ! Get bottom and top layer index of cell
                  do k = kb, kt ! Go through all vertical layers
                     if (squ(k) > eps10) then ! Check if flux is outgoing
                        dtsc = cflmx * vol1(k) / (squ(k) + sqi(k)) ! Compute maximum cell timestep based on total flux (3D)

                        if (jamapdtcell > 0) then ! Store cell timestep if required
                           dtcell(k) = dtsc
                        end if

                        if (dtsc < dts) then ! Check if timestep of cell is globally limiting and store cell index
                           dts = dtsc
                           kkcflmx = kk
                        end if
                     end if
                  end do
               end if
            end do

         ! 3D outflow excluding top layer
         else if (autotimestep == AUTO_TIMESTEP_3D_OUT_NOTOP) then

            do kk = 1, ndxi ! Go through all 2D internal flow cells
               if (jampi == 1) then
                  if (idomain(kk) /= my_rank) then
                     cycle ! do not include ghost cells
                  end if
               end if

               if (hs(kk) > epshu) then ! Check if cell is wet
                  call getkbotktop(kk, kb, kt) ! Get bottom and top layer index of cell
                  do k = kb, max(kb, kt - 1) ! Go through all vertical layers except the top layer
                     if (squ(k) > eps10) then ! Check if there is an outgoing flux present
                        dtsc = cflmx * vol1(k) / squ(k) ! Compute maximum cell timestep based on outflow (3D)
                        
                        if (jamapdtcell > 0) then ! Store cell timestep if required
                           dtcell(k) = dtsc
                        end if
                        
                        if (dtsc < dts) then ! Check if timestep of cell is globally limiting and store cell index
                           dts = dtsc
                           kkcflmx = kk
                        end if
                     end if
                  end do
               end if
            end do

         ! 3D horizontal outflow + total inflow, excluding top layer
         else if (autotimestep == AUTO_TIMESTEP_3D_HOR_OUT_TOTAL_IN_NOTOP) then

            do kk = 1, ndxi ! Go through all 2D internal flow cells
               if (jampi == 1) then
                  if (idomain(kk) /= my_rank) then
                     cycle ! do not include ghost cells
                  end if
               end if

               if (hs(kk) > epshu) then ! Check if cell is wet
                  dtsc = 9.0e9_dp

                  if (squ(kk) > eps10) then ! Check if flux is outgoing
                     dtsc2D = cflmx * vol1(kk) / squ(kk) ! Compute maximum cell timestep based on horizontal outflow (2D)
                  end if

                  call getkbotktop(kk, kb, kt) ! Get bottom and top layer index of cell
                  do k = kb, kt - 1 ! Go through all vertical layers except the top layer
                     if (sqi(k) > eps10) then ! Check if flux is incoming
                        dtsc = cflmx * vol1(k) / sqi(k) ! Compute maximum cell timestep based on inflow (3D)
                        dtsc = min(dtsc2D, dtsc) ! Take the minimum of horizontal outflow and total inflow timesteps

                        if (jamapdtcell > 0) then ! Store cell timestep if required
                           dtcell(k) = dtsc
                        end if

                        if (dtsc < dts) then ! Check if timestep of cell is globally limiting and store cell index
                           dts = dtsc
                           kkcflmx = kk
                        end if
                     end if
                  end do
               end if
            end do
         end if

         ! Explicit time step restriction on viscosity term.
         if (ja_timestep_auto_visc == 1 .and. ihorvic > 0) then
            if (kmx == 0) then
               if (istresstyp == 2 .or. istresstyp == 3) then ! first set stressvector in cell centers
                  do L = lnx1D + 1, lnx
                     if (hu(L) > 0) then ! link will flow
                        k1 = ln(1, L)
                        k2 = ln(2, L)

                        if (jampi == 1) then
                           if (idomain(k1) /= my_rank .and. idomain(k2) /= my_rank) then
                              cycle ! do not include ghost cells
                           end if
                        end if

                        dxiAu = dxi(L) * wu(L)
                        if (istresstyp == 3) then
                           dxiAu = min(hs(k1), hs(k2)) * dxiAu
                        end if

                        huv = 0.5_dp * (hs(k1) + hs(k2))

                        if (dxiAu > 0.0_dp .and. vicLu(L) > 0.0_dp .and. huv > epshu) then ! see "setumod" for huv
                           dtsc = 0.2_dp / (dxiAu * vicLu(L))
                           if (istresstyp == 3) then
                              dtsc = dtsc * huv
                           end if

                           dtsc1 = dtsc * ba(k1)
                           if (dtsc1 < dts) then
                              dts = dtsc1
                              kkcflmx = k1
                           end if

                           dtsc2 = dtsc * ba(k2)
                           if (dtsc2 < dts) then
                              dts = dtsc2
                              kkcflmx = k2
                           end if

                           if (jamapdtcell > 0) then
                              dtcell(k1) = min(dtcell(k1), dtsc1)
                              dtcell(k2) = min(dtcell(k2), dtsc2)
                           end if
                        end if
                     end if
                  end do
               end if
            else if (kmx > 0) then
               if (istresstyp == 2 .or. istresstyp == 3) then ! first set stressvector in cell centers
                  do LL = lnx1D + 1, lnx
                     if (abs(kcu(LL)) /= 2) then
                        cycle
                     end if

                     kk1 = ln(1, LL)
                     kk2 = ln(2, LL)

                     if (jampi == 1) then
                        if (idomain(kk1) /= my_rank .and. idomain(kk2) /= my_rank) then
                           cycle ! do not include ghost cells
                        end if
                     end if

                     call getLbotLtop(LL, Lb, Lt)
                     do L = Lb, Lt
                        k1 = ln(1, L)
                        k2 = ln(2, L)
                        k3 = lncn(1, L)
                        k4 = lncn(2, L)

                        dxiAu = dxi(LL) * wu(LL)
                        if (istresstyp == 3) then
                           dxiAu = min(zws(k1) - zws(k1 - 1), zws(k2) - zws(k2 - 1)) * dxiAu
                        end if

                        huv = 0.5_dp * ((zws(k1) - zws(k1 - 1)) + (zws(k2) - zws(k2 - 1)))

                        if (dxiAu > 0.0_dp .and. vicLu(L) > 0.0_dp .and. huv > epshu) then
                           dtsc = 0.2_dp / (dxiAu * vicLu(L))
                           if (istresstyp == 3) then
                              dtsc = dtsc * huv
                           end if

                           dtsc1 = dtsc * ba(kk1)
                           if (dtsc1 < dts) then
                              dts = dtsc1
                              kkcflmx = kk1
                           end if

                           dtsc2 = dtsc * ba(kk2)
                           if (dtsc2 < dts) then
                              dts = dtsc2
                              kkcflmx = kk2
                           end if

                           if (jamapdtcell > 0) then
                              dtcell(k1) = min(dtcell(k1), dtsc1)
                              dtcell(k2) = min(dtcell(k2), dtsc2)
                           end if
                        end if
                     end do
                  end do
               end if
            end if
         end if

         if (dts > dt_max) then
            dts = dt_max
            kkcflmx = 0
            dtsc = 0
         else
            dtsc = dts ! Courant-driven timestep
         end if

!    dtsc = dts ! Courant-driven timestep

!    if (kkcflmx > 0) then
!        numlimdt(kkcflmx) = numlimdt(kkcflmx) + 1
!    endif

!    if (dts > dt_fac_max*dtprev) then
!        dts = dt_fac_max*dtprev
!        nsteps = ceiling((time_user-time0) / dts)
!        ! New timestep dts would be rounded down to same dtprev (undesired, so use nsteps-1)
!        if (nsteps == ceiling((time_user-time0) / dtprev)) then
!            nsteps = max(1,nsteps - 1)
!        end if
!        dts = (time_user-time0) / dble(nsteps)
!
!        ! dtmax is always leading.
!        if (dts > dt_max .or. dts > dtsc) then ! Fall back to smaller step anyway.
!           dts    = (time_user-time0) / dble(nsteps+1)
!        end if
!    else
!        ! dts = min (dts, dt_max) ! hk: commented out, already done this 15 lines above
!
!        ! Fit timestep dts so that we will exactly reach time_user in future steps.
!        nsteps = max(1,ceiling((time_user-time0) / dts ) )
!        dts = ( time_user-time0 ) / dble(nsteps)
!    endif
      else
         dts = dt_max
         dtsc = 0.0_dp ! SPvdP: safety, was undefined but could be used later
         kkcflmx = 0 ! SPvdP: safety, was undefined but could be used later
      end if

! if (ja_time_step_analysis == 1) then
!    if (mout == 0) then
!       call newfil(mout, trim(md_ident)//'.steps')
!       write(mout, '(A)')  'time0/60, dts, dtsc, kkcflmx, kcflmx-kbot(kkcflmx)+1, vol1(kcflmx), squ2D(kcflmx), squ(kcflmx), sqi(kcflmx) '
!    endif
!    if (kkcflmx > 0) then
!       if (kcflmx == 0) kcflmx = kkcflmx
!       if (autotimestep == AUTO_TIMESTEP_3D_HOR_OUT .or. autotimestep == AUTO_TIMESTEP_3D_HOR_INOUT ) then
!          write(mout, '(3F14.4,2I8,4F14.4)')  time0/60d0, dts, dtsc, kkcflmx, kcflmx-kbot(kkcflmx)+1, vol1(kcflmx), squ2D(kcflmx), squ(kcflmx), sqi(kcflmx)
!       else
!          write(mout, '(3F14.4,2I8,4F14.4)')  time0/60d0, dts, dtsc, kkcflmx, kcflmx-kbot(kkcflmx)+1, vol1(kcflmx), squ  (kcflmx), squ(kcflmx), sqi(kcflmx)
!       endif
!    else
!       write(mout, '(3F14.4, I8)')         time0/60d0, dts, dtsc, kkcflmx
!    endif
! endif
      if (kkcflmx > 0 .and. jamapFlowAnalysis > 0) then
         limitingTimestepEstimation(kkcflmx) = limitingTimestepEstimation(kkcflmx) + 1
      end if
   end subroutine setdtorg

end module m_setdtorg
