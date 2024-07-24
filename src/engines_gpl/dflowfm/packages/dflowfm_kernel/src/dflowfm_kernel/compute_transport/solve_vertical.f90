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

!> solve equations implicitly in vertical direction
subroutine solve_vertical(NUMCONST, ISED1, ISEDN, limtyp, thetavert, Ndkx, Lnkx, kmx, &
                          zws, qw, vol1, kbot, ktop, Lbot, Ltop, &
                          sumhorflux, fluxver, source, sink, &
                          difsed, sigdifi, vicwws, &
                          nsubsteps, jaupdate, ndeltasteps, sed, &
                          a, b, c, d, e, sol, rhs)
   use m_flowgeom, only: Ndxi, Ndx, Lnx, ba, kfs ! static mesh information
   use m_flowtimes, only: dts
   use m_flow, only: kmxn, xlozmidov, rhomean, rho, ag, a1, wsf, jaimplicitfallvelocity ! do not use m_flow, please put this in the argument list
   use m_flowparameters, only: epshu, testdryflood
   use m_sediment, only: mtd, jased
   use m_fm_erosed, only: tpsnumber
   use sediment_basics_module
   use timers

   implicit none

   integer, intent(in) :: NUMCONST !< number of transported quantities
   integer, intent(in) :: ISED1 !< index of first sediment fraction in constituents array
   integer, intent(in) :: ISEDN !< index of last  sediment fraction in constituents array
   integer, intent(in) :: limtyp !< limiter type
   double precision, dimension(NUMCONST), intent(in) :: thetavert !< vertical advection explicit (0) or implicit (1)
   integer, intent(in) :: Ndkx !< total number of flownodes (dynamically changing)
   integer, intent(in) :: Lnkx !< total number of flowlinks (dynamically changing)
   integer, intent(in) :: kmx !< maximum number of layers
   double precision, dimension(Ndkx), intent(in) :: zws !< vertical coordinate of layers at interface/center locations
   double precision, dimension(Ndkx), intent(in) :: qw !< flow-field vertical discharges
!   double precision, dimension(Ndkx),          intent(in)    :: sq          !< flux balance (inward positive)
   double precision, dimension(Ndkx), intent(in) :: vol1 !< volumes
   integer, dimension(Ndx), intent(in) :: kbot !< flow-node based layer administration
   integer, dimension(Ndx), intent(in) :: ktop !< flow-node based layer administration
   integer, dimension(Lnx), intent(in) :: Lbot !< flow-link based layer administration
   integer, dimension(Lnx), intent(in) :: Ltop !< flow-link based layer administration
   double precision, dimension(NUMCONST, Ndkx), intent(inout) :: sumhorflux !< sum of horizontal fluxes
   double precision, dimension(NUMCONST, Ndkx), intent(in) :: fluxver !< vertical fluxes
   double precision, dimension(NUMCONST, Ndkx), intent(in) :: source !< sources
   double precision, dimension(NUMCONST, Ndkx), intent(in) :: sink !< sinks
   double precision, dimension(NUMCONST), intent(in) :: difsed !< scalar-specific diffusion coefficent (dicoww+difmod)
   double precision, dimension(Ndkx), intent(in) :: vicwws !< vertical eddy viscosity, NOTE: real, not double
   double precision, dimension(NUMCONST), intent(in) :: sigdifi !< 1/(Prandtl number) for heat, 1/(Schmidt number) for mass
   integer, intent(in) :: nsubsteps !< number of substeps
   integer, dimension(Ndx), intent(in) :: jaupdate !< update cell (1) or not (0)
   integer, dimension(Ndx), intent(in) :: ndeltasteps !< number of substeps
   double precision, dimension(NUMCONST, Ndkx), intent(inout) :: sed !< transported quantities
   double precision, dimension(kmx, NUMCONST) :: a, b, c, d ! work array: aj(i,j)*sed(j,k-1) + bj(i,j)*sed(j,k) + c(i,j)*sed(j,k+1) = d(i), i=k-kb+1
   double precision, dimension(kmx) :: sol, e ! work array: solution and dummy array in tridag, respectively
   double precision, dimension(NUMCONST, Ndkx) :: rhs ! work array: right-hand side, dim(NUMCONST,Ndkx)

   double precision :: fluxfac, dvol1i, dvol2i
   double precision :: dtbazi, dtba, ozmid, bruns

   integer :: kk, k, kb, kt, ktx
   integer :: j, n

   double precision :: dt_loc
   double precision :: qw_loc

   double precision, parameter :: dtol = 1d-8

   integer(4) ithndl / 0 /
   if (timon) call timstrt("solve_vertical", ithndl)

   dt_loc = dts

   rhs = 0d0

   call make_rhs(NUMCONST, thetavert, Ndkx, Lnkx, kmx, vol1, kbot, ktop, Lbot, Ltop, sumhorflux, fluxver, source, sed, nsubsteps, jaupdate, ndeltasteps, rhs)

   ! construct and solve system
   !$OMP PARALLEL DO                                                 &
   !$OMP PRIVATE(kk,kb,ktx,kt,a,b,c,sol,j,d,k,n,dvol1i,dvol2i,fluxfac,e,dtbazi,dtba,ozmid,bruns,qw_loc) &
   !$OMP FIRSTPRIVATE(dt_loc)
   do kk = 1, Ndxi
      if (nsubsteps > 1) then
         if (jaupdate(kk) == 0) then
            cycle
         else
            dt_loc = dts * ndeltasteps(kk)
         end if
      else
         dt_loc = dts
      end if

      kb = kbot(kk)
      kt = ktop(kk)
      if (kfs(kk) <= 0) cycle

      ktx = kb + kmxn(kk) - 1
      a = 0d0
      c = 0d0
      sol = 0d0

!     add linearized sinks to diagonal
      do k = kb, kt
         n = k - kb + 1 ! layer number
         do j = 1, NUMCONST
            b(n, j) = 1d0 + dt_loc * sink(j, k)
         end do
      end do

      do j = 1, NUMCONST
         d(1, j) = rhs(j, kb)
         do k = kb, kt ! assume zero-fluxes at boundary and top
            n = k - kb + 1 ! layer number
            d(n, j) = rhs(j, k)
         end do
      end do

      ! if ( s1(kk)-bl(kk) > epshsdif ) then
      ! if ( s1(kk)-zws(kb-1) > epshsdif ) then

      do k = kb, kt - 1 ! assume zero-fluxes at boundary and top
         n = k - kb + 1 ! layer number
         dvol1i = 1d0 / max(vol1(k), dtol) ! dtol: safety
         if (testdryflood == 2) dvol1i = 1d0 / max(vol1(k), epshu * ba(kk) / max(kt - kb + 1, 1))
         dvol2i = 1d0 / max(vol1(k + 1), dtol) ! dtol: safety
         if (testdryflood == 2) dvol2i = 1d0 / max(vol1(k + 1), epshu * ba(kk) / max(kt - kb + 1, 1))
         dtba = dt_loc * ba(kk)
         dtbazi = dtba / max(1d-4, 0.5d0 * (zws(k + 1) - zws(k - 1))) ! another safety check

         ozmid = 0d0
         if (xlozmidov > 0d0) then
            if (rho(k) < rho(k - 1)) then
               bruns = (rho(k - 1) - rho(k)) / (0.5d0 * (zws(k + 1) - zws(k - 1))) ! = -drhodz
               bruns = sqrt(bruns * ag / rhomean)
               ozmid = 0.2d0 * xlozmidov * xlozmidov * bruns
            end if
         end if

         do j = 1, NUMCONST

!           ! diffusion
            if (jased > 3 .and. j >= ISED1 .and. j <= ISEDN) then ! sediment d3d
               fluxfac = (ozmid + mtd%seddif(j - ISED1 + 1, k) / tpsnumber(j - ISED1 + 1) + difsed(j)) * dtbazi
               ! i.w.  + vicwws/van rijn                              + background (dicoww)
            else
               fluxfac = (sigdifi(j) * vicwws(k) + difsed(j) + ozmid) * dtbazi
            end if

!           BEGIN DEBUG
!            fluxfac = dt_loc * (difsed(j)) *ba(kk) / ( 0.5d0*(zws(k+1) - zws(k-1)) )  ! m3
!           END DEBUG

            b(n, j) = b(n, j) + fluxfac * dvol1i
            c(n, j) = c(n, j) - fluxfac * dvol1i

            b(n + 1, j) = b(n + 1, j) + fluxfac * dvol2i
            a(n + 1, j) = a(n + 1, j) - fluxfac * dvol2i

!           advection
            if (thetavert(j) > 0d0) then ! semi-implicit, use central scheme
               ! BEGIN DEBUG
               ! if ( .false. .and. thetavert(j).gt.0d0 ) then ! semi-implicit, use central scheme
               ! END DEBUG

               if (jased > 0 .and. jaimplicitfallvelocity == 0) then ! explicit fallvelocity
                  if (jased < 4) then
                     qw_loc = qw(k) - wsf(j) * a1(kk)
                  else if (j >= ISED1 .and. j <= ISEDN) then
                     qw_loc = qw(k) - mtd%ws(k, j - ISED1 + 1) * a1(kk)
                  end if
               else
                  qw_loc = qw(k)
               end if

               fluxfac = qw_loc * 0.5d0 * thetavert(j) * dt_loc

               a(n + 1, j) = a(n + 1, j) - fluxfac * dvol2i
               b(n + 1, j) = b(n + 1, j) - fluxfac * dvol2i

               b(n, j) = b(n, j) + fluxfac * dvol1i
               c(n, j) = c(n, j) + fluxfac * dvol1i
            end if

            if (jased > 0 .and. jaimplicitfallvelocity == 1) then
               fluxfac = 0d0
               if (jased > 3) then
                  if (j >= ISED1 .and. j <= ISEDN) then
                     fluxfac = mtd%ws(k, j - ISED1 + 1) * a1(kk) * dt_loc
                  else
                     fluxfac = wsf(j) * a1(kk) * dt_loc
                  end if
               else
                  fluxfac = wsf(j) * a1(kk) * dt_loc
               end if

               if (fluxfac > 0d0) then
                  c(n, j) = c(n, j) - fluxfac * dvol1i
                  b(n + 1, j) = b(n + 1, j) + fluxfac * dvol2i
               else if (fluxfac < 0d0) then
                  b(n, j) = b(n, j) - fluxfac * dvol1i
                  a(n + 1, j) = a(n + 1, j) + fluxfac * dvol2i
               end if

            end if

         end do
      end do

!     solve system(s)
      do j = 1, NUMCONST
!         if ( kk.eq.2 .and. j.eq.1 ) then
!            do k=kb,kt
!               n = k-kb+1
!               write(6,*) n, a(n,j), b(n,j), c(n,j), d(n,j)
!            end do
!         end if

         call tridag(a(1, j), b(1, j), c(1, j), d(1, j), e, sol, kt - kb + 1)

         sed(j, kb:kt) = sol(1:kt - kb + 1)
         sed(j, kt + 1:ktx) = sed(j, kt)

!        BEGIN DEBUG
!         do k=kb,kt
!            if ( j.eq.1 .and. ( sed(j,k).gt.30.0001 .or. sed(j,k).lt.-0.0001 ) ) then
!               continue
!               write(6,*) 'kk=', kk, 'lay=', k-kb+1
!               write(6,*) 'rhs=', rhs(j,k)
!               write(6,*) 'sed=', sed(j,k)
!               call qnerror(' ', ' ', ' ')
!            end if
!         end do
!        END DEBUG

      end do

   end do

   !$OMP END PARALLEL DO

   if (timon) call timstop(ithndl)
   return
end subroutine solve_vertical
