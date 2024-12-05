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

 subroutine setdtorg(jareduced) ! set computational timestep dts
    use precision, only: dp
    use m_flowgeom
    use m_flow
    use m_wind
    use m_flowtimes
    use m_partitioninfo
    use m_missing
    use m_drawthis
    use m_get_kbot_ktop
    use m_get_Lbot_Ltop
    implicit none

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
    if (jamapdtcell > 0) dtcell = 0d0

    if (ja_timestep_auto >= 1) then

       if (NDRAW(28) == 30 .or. NDRAW(29) == 38) then
          plotlin = dt_max
       end if

       dts = 1d9; kkcflmx = 0; kcflmx = 0

       if (ja_timestep_auto == 1) then ! depth averaged timestep
          if (itstep /= 4) then ! non-explicit time-step
             if (ja_timestep_nostruct > 0) then !< Exclude (structure) links without advection from the time step limitation
                squcor(1:ndx) = squ(1:ndx) ! Start with already computed squ.
                do L = 1, lnx1d
                   if (iadv(L) /= 0 .and. iadv(L) /= 22) then
                      cycle ! Do NOT exclude this link
                   end if
                   k1 = ln(1, L); k2 = ln(2, L)
                   ! Undo some of the added q1 contributions in squ (as produced by u1q1()).
                   if (q1(L) > 0) then
                      squcor(k1) = squcor(k1) - q1(L)
                   else if (q1(L) < 0) then
                      squcor(k2) = squcor(k2) + q1(L)
                   end if
                end do
             end if

             do k = 1, ndxi
                if (jampi == 1) then
!               do not include ghost cells
                   if (idomain(k) /= my_rank) cycle
                end if

                if (ja_timestep_nostruct > 0) then !< Exclude (structure) links without advection from the time step limitation
                   squloc = squcor(k)
                else
                   squloc = squ(k)
                end if

                ! DO not include negative qin in timestep
                if (ja_timestep_noqout > 0) then
                   squloc = squloc - max(-qin(k), 0d0)
                end if

                if (squloc > eps10) then ! outflow only
                   if (hs(k) > epshu .and. vol1(k) > 0.0 .and. squloc > 0.0) then
                      if (jamapFlowAnalysis > 0) then
                         ! The flowCourantNumber will be multiplied by dt on a later stage
                         flowCourantNumber(k) = squloc / vol1(k)
                      end if
                      dtsc = cflmx * vol1(k) / squloc

                      if (jamapdtcell > 0) then
                         dtcell(k) = dtsc
                      end if
                      if (dtsc < dts) then
                         dts = dtsc; kkcflmx = k
                      end if
                      if (dtsc == 0d0) then
                         kkcflmx = k
                      end if
                   end if
                end if
             end do
             ! UNST-3844: DEBUG code:
             !if (kkcflmx > 0) then
             !   write (*,'(2(a,i8),3(a,e16.5))') '#dt: ', int(dnt), ', kkcflmx = ', kkcflmx, ', dts = ', dts, ', qextreal(kkcflmx) = ', qextreal(kkcflmx), ', squcor(kkcflmx) = ', squcor(kkcflmx)
             !   write (*,'(4(a,e16.5))') 'vol1(kkcflmx) = ', vol1(kkcflmx), ', qin(kkcflmx) = ', qin(kkcflmx), ', q1(i) = ', q1(abs(nd(kkcflmx)%ln(1))), ', q1(ii) = ', q1(abs(nd(kkcflmx)%ln(min(nd(kkcflmx)%lnx,2))))
             !   flush(6)
             !end if
             continue

          else ! explicit time-step
             do k = 1, ndxi
                if (jampi == 1) then
!               do not include ghost cells
                   if (idomain(k) /= my_rank) cycle
                end if
                if (sqwave(k) > eps10) then ! outflow only
                   if (hs(k) > epshu) then
                      dtsc = cflmx * vol1(k) / sqwave(k)
                      if (jamapdtcell > 0) then
                         dtcell(k) = dtsc
                      end if
                      if (dtsc < dts) then
                         dts = dtsc; kkcflmx = k
                      end if
                   end if
                end if
             end do

          end if

       else if (ja_timestep_auto == 2) then ! depth averaged timestep

          do k = 1, ndxi
             if (jampi == 1) then
!            do not include ghost cells
                if (idomain(k) /= my_rank) cycle
             end if
             if (squ(k) + sqi(k) > eps10) then ! outflow+inflow
                if (hs(k) > epshu .and. vol1(k) > 0.0) then
                   dtsc = cflmx * vol1(k) / (squ(k) + sqi(k))
                   if (jamapdtcell > 0) then
                      dtcell(k) = dtsc
                   end if
                   if (dtsc < dts) then
                      dts = dtsc; kkcflmx = k
                   end if

                end if
             end if
          end do

       else if (ja_timestep_auto == 3 .or. ja_timestep_auto == 4) then ! 3 = 2D out over layers, 4=2D in+out all layers

          do kk = 1, ndxi
             if (jampi == 1) then
!            do not include ghost cells
                if (idomain(kk) /= my_rank) cycle
             end if
             if (squ2D(kk) > eps10 .and. hs(kk) > epshu) then
                call getkbotktop(kk, kb, kt)
                do k = kb, kt
                   if (squ2d(k) > eps10) then
                      dtsc = cflmx * vol1(k) / squ2d(k) ! outflow or outflow+inflow
                      if (jamapdtcell > 0) then
                         dtcell(k) = dtsc
                      end if
                      if (dtsc < dts) then
                         dts = dtsc; kkcflmx = kk; kcflmx = k
                      end if
                   end if
                end do
             end if
          end do

       else if (ja_timestep_auto == 5) then ! full 3D

          do kk = 1, Ndxi
             if (jampi == 1) then
!            do not include ghost cells
                if (idomain(kk) /= my_rank) cycle
             end if
             if (hs(kk) > epshu) then
                call getkbotktop(kk, kb, kt)
                do k = kb, kt
                   if (squ(k) > eps10 .or. sqi(k) > eps10) then
!                      dtsc = cflmx*vol1(k)/squ(k)
                      dtsc = cflmx * vol1(k) / max(squ(k), sqi(k))
                      if (jamapdtcell > 0) then
                         dtcell(k) = dtsc
                      end if
                      if (dtsc < dts) then
                         dts = dtsc; kkcflmx = kk
                      end if
                   end if
                end do
             end if
          end do
          !      end if

       else if (ja_timestep_auto == 6) then
          do kk = 1, Ndxi
             if (jampi == 1) then
!            do not include ghost cells
                if (idomain(kk) /= my_rank) cycle
             end if
             if (hs(kk) > epshu) then
                dtsc2D = dt_max
                if (squ(kk) > eps10) then
                   dtsc2D = cflmx * vol1(kk) / squ(kk)
                end if
                call getkbotktop(kk, kb, kt)
                do k = kb, kt
                   if (sqi(k) > eps10) then
                      dtsc = cflmx * vol1(k) / sqi(k)
                      dtsc = min(dtsc, dtsc2D)
                      if (jamapdtcell > 0) then
                         dtcell(k) = dtsc
                      end if
                      if (dtsc < dts) then
                         dts = dtsc; kkcflmx = kk
                      end if
                   end if
                end do
             end if
          end do

       else if (ja_timestep_auto == 7) then ! full 3D plus barocline

          do LL = 1, Lnxi
             n1 = ln(1, LL); n2 = ln(2, LL)
             call getLbotLtop(LL, Lb, Lt)
             rhomin = 2d3; rhomax = -1d0
             do L = Lb, Lt
                k1 = ln(1, L); k2 = ln(2, L)
                rhomin = min(rhomin, rho(k1), rho(k2))
                rhomax = max(rhomax, rho(k1), rho(k2))
             end do
             drho = rhomax - rhomin
             cbaroc = sqrt(0.25d0 * ag * hu(LL) * drho * 0.001d0) ! rhomax-rhomin
             do L = Lb, Lt
                squ(n1) = squ(n1) + au(L) * cbaroc
                squ(n2) = squ(n2) + au(L) * cbaroc
                sqi(n1) = sqi(n1) + au(L) * cbaroc
                sqi(n2) = sqi(n2) + au(L) * cbaroc
             end do
          end do

          do kk = 1, Ndxi
             if (jampi == 1) then
                if (idomain(kk) /= my_rank) cycle !            do not include ghost cells
             end if
             if (hs(kk) > epshu) then
                call getkbotktop(kk, kb, kt)
                do k = kb, kt
                   if (squ(k) > eps10) then
                      dtsc = cflmx * vol1(k) / (squ(k) + sqi(k))
                      if (jamapdtcell > 0) then
                         dtcell(k) = dtsc
                      end if
                      if (dtsc < dts) then
                         dts = dtsc; kkcflmx = kk
                      end if
                   end if
                end do
             end if
          end do

       else if (ja_timestep_auto == 8) then ! full 3D except top layer

          do kk = 1, Ndxi
             if (jampi == 1) then
!            do not include ghost cells
                if (idomain(kk) /= my_rank) cycle
             end if
             if (hs(kk) > epshu) then
                call getkbotktop(kk, kb, kt)
                do k = kb, max(kb, kt - 1)
                   if (squ(k) > eps10) then
                      dtsc = cflmx * vol1(k) / squ(k)
                      if (jamapdtcell > 0) then
                         dtcell(k) = dtsc
                      end if
                      if (dtsc < dts) then
                         dts = dtsc; kkcflmx = kk
                      end if
                   end if
                end do
             end if
          end do

       else if (ja_timestep_auto == 9) then ! 2D outgoing and 3D incoming fluxes

          do kk = 1, Ndxi
             if (jampi == 1) then
!            do not include ghost cells
                if (idomain(kk) /= my_rank) cycle
             end if
             if (hs(kk) > epshu) then
                dtsc = 9d9
                if (squ(kk) > eps10) then
                   dtsc = cflmx * vol1(kk) / squ(kk)
                end if
                call getkbotktop(kk, kb, kt)
                do k = kb, kt
                   if (sqi(k) > eps10) then
                      dtsc = min(dtsc, cflmx * vol1(k) / sqi(k))
                      if (jamapdtcell > 0) then
                         dtcell(k) = dtsc
                      end if
                      if (dtsc < dts) then
                         dts = dtsc; kkcflmx = kk
                      end if
                   end if
                end do
             end if
          end do

       else if (ja_timestep_auto == 10) then ! 2D outgoing and 3D incoming fluxes

          do kk = 1, Ndxi
             if (jampi == 1) then
!            do not include ghost cells
                if (idomain(kk) /= my_rank) cycle
             end if
             if (hs(kk) > epshu) then
                dtsc = 9d9
                if (squ(kk) > eps10) then
                   dtsc = cflmx * vol1(kk) / squ(kk)
                end if
                call getkbotktop(kk, kb, kt)
                do k = kb, kt - 1
                   if (sqi(k) > eps10) then
                      dtsc = min(dtsc, cflmx * vol1(k) / sqi(k))
                      if (jamapdtcell > 0) then
                         dtcell(k) = dtsc
                      end if
                      if (dtsc < dts) then
                         dts = dtsc; kkcflmx = kk
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
                      k1 = ln(1, L); k2 = ln(2, L)

                      if (jampi == 1) then
                         if (idomain(k1) /= my_rank .and. idomain(k2) /= my_rank) cycle ! do not include ghost cells
                      end if

                      dxiAu = dxi(L) * wu(L)
                      if (istresstyp == 3) then
                         dxiAu = min(hs(k1), hs(k2)) * dxiAu
                      end if

                      huv = 0.5d0 * (hs(k1) + hs(k2))

                      if (dxiAu > 0d0 .and. vicLu(L) > 0d0 .and. huv > epshu) then ! see "setumod" for huv
                         dtsc = 0.2d0 / (dxiAu * vicLu(L))
                         if (istresstyp == 3) then
                            dtsc = dtsc * huv
                         end if

                         dtsc1 = dtsc * ba(k1)
                         if (dtsc1 < dts) then
                            dts = dtsc1; kkcflmx = k1
                         end if

                         dtsc2 = dtsc * ba(k2)
                         if (dtsc2 < dts) then
                            dts = dtsc2; kkcflmx = k2
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
                   if (abs(kcu(LL)) /= 2) cycle

                   kk1 = ln(1, LL); kk2 = ln(2, LL)

                   if (jampi == 1) then
                      if (idomain(kk1) /= my_rank .and. idomain(kk2) /= my_rank) cycle ! do not include ghost cells
                   end if

                   call getLbotLtop(LL, Lb, Lt)
                   do L = Lb, Lt
                      k1 = ln(1, L); k2 = ln(2, L)
                      k3 = lncn(1, L); k4 = lncn(2, L)

                      dxiAu = dxi(LL) * wu(LL)
                      if (istresstyp == 3) then
                         dxiAu = min(zws(k1) - zws(k1 - 1), zws(k2) - zws(k2 - 1)) * dxiAu
                      end if

                      huv = 0.5d0 * ((zws(k1) - zws(k1 - 1)) + (zws(k2) - zws(k2 - 1)))

                      if (dxiAu > 0d0 .and. vicLu(L) > 0d0 .and. huv > epshu) then
                         dtsc = 0.2d0 / (dxiAu * vicLu(L))
                         if (istresstyp == 3) then
                            dtsc = dtsc * huv
                         end if

                         dtsc1 = dtsc * ba(kk1)
                         if (dtsc1 < dts) then
                            dts = dtsc1; kkcflmx = kk1
                         end if

                         dtsc2 = dtsc * ba(kk2)
                         if (dtsc2 < dts) then
                            dts = dtsc2; kkcflmx = kk2
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
          dts = dt_max; kkcflmx = 0; dtsc = 0
       else
          dtsc = dts ! Courant-driven timestep
       end if

!    dtsc = dts ! Courant-driven timestep

!    if (kkcflmx > 0) then
!        numlimdt(kkcflmx) = numlimdt(kkcflmx) + 1
!    endif

!    if (dts > dtfacmax*dtprev) then
!        dts = dtfacmax*dtprev
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
       dtsc = 0d0 ! SPvdP: safety, was undefined but could be used later
       kkcflmx = 0 ! SPvdP: safety, was undefined but could be used later
    end if

! if (jatimestepanalysis == 1) then
!    if (mout == 0) then
!       call newfil(mout, trim(md_ident)//'.steps')
!       write(mout, '(A)')  'time0/60, dts, dtsc, kkcflmx, kcflmx-kbot(kkcflmx)+1, vol1(kcflmx), squ2D(kcflmx), squ(kcflmx), sqi(kcflmx) '
!    endif
!    if (kkcflmx > 0) then
!       if (kcflmx == 0) kcflmx = kkcflmx
!       if (ja_timestep_auto == 3 .or. ja_timestep_auto == 4 ) then
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
