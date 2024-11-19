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
!> This subroutine transports an array of scalars.
!> In light of future vectorization, the aim is to:
!>   -use as few module variables as possible,
!>   -do not use if statements within do-loops.
!> updates sedl with
!>     d(h sedl)/dt = -div (q1 sedl) + div (diag(NU) grad sedl) + h ( -sink sedl + source )
!>   solves for each column {k | 1<=k<k=top} an equation of the form
!>     aaj(k) sedj(k-1) + bbj(k) sedj(k) + ccj(k) sedj(k+1) = ddj(k)
module m_update_constituents

implicit none

private

public :: update_constituents

contains

subroutine update_constituents(jarhoonly)
   use m_solve_vertical, only: solve_vertical
   use m_solve_2d, only: solve_2d
   use m_get_ndeltasteps, only: get_ndeltasteps
   use m_get_jaupdatehorflux, only: get_jaupdatehorflux
   use m_get_jaupdate, only: get_jaupdate
   use m_get_dtmax, only: get_dtmax
   use m_fill_rho, only: fill_rho
   use m_fill_constituents, only: fill_constituents
   use m_extract_rho, only: extract_rho
   use m_extract_constituents, only: extract_constituents
   use m_diffusionimplicit2d, only: diffusionimplicit2d
   use m_decaytracers, only: decaytracers
   use m_comp_sumhorflux, only: comp_sumhorflux
   use m_comp_sinktot, only: comp_sinktot
   use m_comp_horfluxtot, only: comp_horfluxtot
   use m_comp_fluxver, only: comp_fluxver
   use m_comp_fluxhor3d, only: comp_fluxhor3d
   use m_comp_dxiau, only: comp_dxiau
   use m_flowgeom, only: Ndx, Ndxi, Lnx ! static mesh information
   use m_flow, only: Ndkx, Lnkx, u1, q1, au, qw, zws, sqi, vol1, kbot, ktop, Lbot, Ltop, kmxn, kmxL, kmx, viu, vicwws, wsf, jadecaytracers
   use m_flowtimes, only: dts
   use m_turbulence, only: sigdifi
   use m_transport
   use m_mass_balance_areas
   use m_flowparameters, only: limtypsa, limtyptm, limtypsed, flowwithoutwaves
   use m_alloc
   use m_partitioninfo
   use m_timer
   use unstruc_messages
   use m_sediment, only: jatranspvel, jased, stmpar, stm_included
   use m_waves
   use timers
   use mass_balance_areas_routines, only: comp_horfluxmba
   use m_get_Lbot_Ltop

   implicit none

   integer, intent(in) :: jarhoonly

   integer :: ierror

   integer :: limtyp !< limiter type (>0), or first-order upwind (0)
   double precision :: dts_store

   integer :: LL, L, j, numconst_store, Lb, Lt
   integer :: istep
   integer :: numstepssync

   integer(4) :: ithndl =  0
   
   if (NUMCONST == 0) return ! nothing to do
   if (timon) call timstrt("update_constituents", ithndl)

   ierror = 1

   limtyp = max(limtypsa, limtyptm, limtypsed)

   if (jarhoonly == 1) then
      call fill_rho(); numconst_store = numconst
   else if (jarhoonly == 2) then
      ! call fill_ucxucy() ; numconst_store = numconst
   else
      call fill_constituents(1)
   end if

!  compute areas of horizontal diffusive fluxes divided by Dx
   call comp_dxiAu()

!  get maximum transport time step
   call get_dtmax()

   call get_ndeltasteps()

!  store dts
   dts_store = dts

!  set dts to smallest timestep
   dts = dts / nsubsteps

   if (jampi /= 0) then
!     determine at which sub timesteps to update
      if (limtyp == 0) then
         numstepssync = max(numlay_cellbased - 1, 1) ! one level used in first-order upwind order reconstruction
      else
         numstepssync = max(int(numlay_cellbased / 2), 1) ! two levels used in higher-order upwind reconstruction
      end if
   end if

   jaupdate = 1

   fluxhor = 0d0 ! not necessary
   sumhorflux = 0d0

   if (stm_included) then
      fluxhortot = 0d0
      sinksetot = 0d0
      sinkftot = 0d0
      u1sed = 0d0
      q1sed = 0d0
   end if

   do istep = 0, nsubsteps - 1
      if (kmx > 0) then
         fluxver = 0d0
      end if

!     determine which fluxes need to be updated
      if (nsubsteps > 1) then
         call get_jaupdatehorflux(nsubsteps, limtyp, jaupdate, jaupdatehorflux)
      end if

!     compute horizontal fluxes, explicit part
      if ((.not. stm_included) .or. flowwithoutwaves) then ! just do the normal stuff
         call comp_fluxhor3D(NUMCONST, limtyp, Ndkx, Lnkx, u1, q1, sqi, vol1, kbot, Lbot, Ltop, kmxn, kmxL, constituents, difsedu, sigdifi, viu, nsubsteps, jaupdatehorflux, ndeltasteps, jaupdateconst, fluxhor, dsedx, dsedy, jalimitdiff, dxiAu)
      else
         if (jatranspvel == 0 .or. jatranspvel == 1) then ! Lagrangian approach
            ! only add velocity asymmetry
            do LL = 1, Lnx
               call getLbotLtop(LL, Lb, Lt) ! prefer this, as Ltop gets messed around with in hk specials
               do L = Lb, Lt
                  u1sed(L) = u1(L) !+mtd%uau(LL)                    ! JRE to do, discuss with Dano
                  q1sed(L) = q1(L) !+mtd%uau(LL)*Au(L)
               end do
            end do
         else if (jatranspvel == 2) then ! Eulerian approach
!           stokes+asymmetry
            do LL = 1, Lnx
               call getLbotLtop(LL, Lb, Lt)
               do L = Lb, Lt
                  u1sed(L) = u1(L) - ustokes(L) !+mtd%uau(LL)
                  q1sed(L) = q1(L) - ustokes(L) * Au(L) !+mtd%uau(LL)*Au(L)
               end do
            end do
         end if
         call comp_fluxhor3D(NUMCONST, limtyp, Ndkx, Lnkx, u1sed, q1sed, sqi, vol1, kbot, Lbot, Ltop, kmxn, kmxL, constituents, difsedu, sigdifi, viu, nsubsteps, jaupdatehorflux, ndeltasteps, noupdateconst, fluxhor, dsedx, dsedy, jalimitdiff, dxiAu)
!        water advection velocity
         call comp_fluxhor3D(NUMCONST, limtyp, Ndkx, Lnkx, u1, q1, sqi, vol1, kbot, Lbot, Ltop, kmxn, kmxL, constituents, difsedu, sigdifi, viu, nsubsteps, jaupdatehorflux, ndeltasteps, jaupdateconst, fluxhor, dsedx, dsedy, jalimitdiff, dxiAu)
      end if

      call starttimer(IDEBUG)
      call comp_sumhorflux(NUMCONST, kmx, Lnkx, Ndkx, Lbot, Ltop, fluxhor, sumhorflux)
      call stoptimer(IDEBUG)

      if (jased == 4 .and. stmpar%lsedsus > 0) then ! at moment, this function is only required by suspended sediment. Can be extended to other fluxes if necessary
         call comp_horfluxtot()
      end if

      if (jamba > 0) then ! at moment, this function is only required for the mass balance areas
         call comp_horfluxmba()
      end if

!     determine which cells need to be updated
      if (nsubsteps > 1) then
         call get_jaupdate(istep, Ndxi, Ndx, ndeltasteps, jaupdate)
      end if

      if (kmx < 1) then ! 2D, call to 3D as well for now
         call solve_2D(NUMCONST, Ndkx, vol1, kbot, ktop, sumhorflux, fluxver, const_sour, const_sink, nsubsteps, jaupdate, ndeltasteps, constituents, rhs)
      else
         call comp_fluxver(NUMCONST, limtyp, thetavert, Ndkx, zws, qw, kbot, ktop, constituents, nsubsteps, jaupdate, ndeltasteps, fluxver, wsf)

         call solve_vertical(NUMCONST, ISED1, ISEDN, thetavert, Ndkx, kmx, &
                             zws, qw, vol1, kbot, ktop, &
                             sumhorflux, fluxver, const_sour, const_sink, &
                             difsedw, sigdifi, vicwws, nsubsteps, jaupdate, ndeltasteps, constituents, &
                             a, b, c, d, e, sol, rhs)
      end if

      if (jampi > 0) then
!        communicate every numstepssync'th or last subtimestep
         if (mod(istep + 1, numstepssync) == 0 .or. istep + 1 == nsubsteps) then
            if (jatimer == 1) call starttimer(IUPDSALL)
            if (kmx < 1) then ! 2D
               call update_ghosts(ITYPE_Sall, NUMCONST, Ndx, constituents, ierror)
            else ! 3D
               call update_ghosts(ITYPE_Sall3D, NUMCONST, Ndkx, constituents, ierror)
            end if
            if (jatimer == 1) call stoptimer(IUPDSALL)
         end if
      end if

      call comp_sinktot()

   end do
   if (jalimitdiff == 3 .and. kmx == 0) then
      call diffusionimplicit2D()
   end if

   if (jased == 4 .and. stmpar%lsedsus > 0) then
      do j = ISED1, ISEDN
         fluxhortot(j, :) = fluxhortot(j, :) / dts_store
         sinksetot(j, :) = sinksetot(j, :) / dts_store
         sinkftot(j, :) = sinkftot(j, :) / dts_store
      end do
   end if

!  Move here, needed in two following subroutines
!  restore dts
   dts = dts_store

   if (jarhoonly == 1) then
      call extract_rho(); numconst = numconst_store
   else
      if (jadecaytracers > 0) then ! because tracerdecay is normally not done in DFM we do it here so as not to cause overhead elsewhere
         call decaytracers()
      end if
      call extract_constituents()
   end if

   ierror = 0
1234 continue

   if (timon) call timstop(ithndl)
   return
end subroutine update_constituents

end module m_update_constituents
