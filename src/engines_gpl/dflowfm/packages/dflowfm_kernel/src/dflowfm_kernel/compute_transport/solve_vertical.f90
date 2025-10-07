!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2025.
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
module m_solve_vertical
   use m_tridag, only: tridag

   implicit none

   private

   public :: solve_vertical

contains

   subroutine solve_vertical(NUMCONST, ISED1, ISEDN, thetavert, Ndkx, kmx, &
                             zws, qw, vol1, kbot, ktop, &
                             sumhorflux, fluxver, source, sink, &
                             sigdifi, vicwws, &
                             nsubsteps, jaupdate, ndeltasteps, sed, &
                             a, b, c, d, e, sol, rhs)
      use precision, only: dp
      use m_make_rhs, only: make_rhs
      use m_flowgeom, only: Ndxi, kfs, ba, ndx ! static mesh information
      use m_flowtimes, only: dts
      use m_flow, only: kmxn, xlozmidov, rhomean, rho, ag, a1, wsf, jaimplicitfallvelocity
      use m_turbulence, only: difwws
      use m_flowparameters, only: epshu, testdryflood
      use m_sediment, only: mtd, jased
      use m_fm_erosed, only: tpsnumber
      use timers, only: timon, timstrt, timstop
      use m_transport, only: isalt

      implicit none

      integer, intent(in) :: NUMCONST !< number of transported quantities
      integer, intent(in) :: ISED1 !< index of first sediment fraction in constituents array
      integer, intent(in) :: ISEDN !< index of last  sediment fraction in constituents array
      real(kind=dp), dimension(NUMCONST), intent(in) :: thetavert !< vertical advection explicit (0) or implicit (1)
      integer, intent(in) :: Ndkx !< total number of flownodes (dynamically changing)
      integer, intent(in) :: kmx !< maximum number of layers
      real(kind=dp), dimension(Ndkx), intent(in) :: zws !< vertical coordinate of layers at interface/center locations
      real(kind=dp), dimension(Ndkx), intent(in) :: qw !< flow-field vertical discharges
!   real(kind=dp), dimension(Ndkx),          intent(in)    :: sq          !< flux balance (inward positive)
      real(kind=dp), dimension(Ndkx), intent(in) :: vol1 !< volumes
      integer, dimension(Ndx), intent(in) :: kbot !< flow-node based layer administration
      integer, dimension(Ndx), intent(in) :: ktop !< flow-node based layer administration
      real(kind=dp), dimension(NUMCONST, Ndkx), intent(inout) :: sumhorflux !< sum of horizontal fluxes
      real(kind=dp), dimension(NUMCONST, Ndkx), intent(in) :: fluxver !< vertical fluxes
      real(kind=dp), dimension(NUMCONST, Ndkx), intent(in) :: source !< sources
      real(kind=dp), dimension(NUMCONST, Ndkx), intent(in) :: sink !< sinks
      real(kind=dp), dimension(Ndkx), intent(in) :: vicwws !< vertical eddy viscosity, NOTE: real, not double
      real(kind=dp), dimension(NUMCONST), intent(in) :: sigdifi !< 1/(Prandtl number) for heat, 1/(Schmidt number) for mass
      integer, intent(in) :: nsubsteps !< number of substeps
      integer, dimension(Ndx), intent(in) :: jaupdate !< update cell (1) or not (0)
      integer, dimension(Ndx), intent(in) :: ndeltasteps !< number of substeps
      real(kind=dp), dimension(NUMCONST, Ndkx), intent(inout) :: sed !< transported quantities

      real(kind=dp), dimension(kmx, NUMCONST) :: a, b, c, d ! work array: aj(i,j)*sed(j,k-1) + bj(i,j)*sed(j,k) + c(i,j)*sed(j,k+1) = d(i), i=k-kb+1
      real(kind=dp), dimension(kmx) :: ac, bc, cc, dc, sol, e ! work array: solution and dummy array in tridag, respectively
      real(kind=dp), dimension(NUMCONST, Ndkx) :: rhs ! work array: right-hand side, dim(NUMCONST,Ndkx)
      real(kind=dp) :: fluxfac, dvol1i, dvol2i
      real(kind=dp) :: dtbazi, dtba, ozmid, bruns
      integer :: kk, k, kb, kt, ktx, nel
      integer :: j, n
      real(kind=dp) :: dt_loc
      real(kind=dp) :: qw_loc
      real(kind=dp), parameter :: dtol = 1.0e-8_dp
      integer(4) :: ithndl = 0

      if (timon) call timstrt("solve_vertical", ithndl)

      dt_loc = dts
      rhs = 0.0_dp
      ac = 0.0_dp
      bc = 0.0_dp
      cc = 0.0_dp
      dc = 0.0_dp

      call make_rhs(NUMCONST, thetavert, Ndkx, kmx, vol1, kbot, ktop, sumhorflux, fluxver, source, sed, nsubsteps, jaupdate, ndeltasteps, rhs)

      ! construct and solve system
      !$OMP PARALLEL DO                                                 &
      !$OMP PRIVATE(kk,kb,ktx,kt,a,b,c,sol,j,d,k,n,dvol1i,dvol2i,fluxfac,e,dtbazi,dtba,ozmid,bruns,qw_loc,ac,bc,cc,dc,nel) &
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
         a = 0.0_dp
         c = 0.0_dp
         sol = 0.0_dp

!     add linearized sinks to diagonal
         do k = kb, kt
            n = k - kb + 1 ! layer number
            do j = 1, NUMCONST
               b(n, j) = 1.0_dp + dt_loc * sink(j, k)
            end do
         end do

         do j = 1, NUMCONST
            d(1, j) = rhs(j, kb)
            do k = kb, kt ! assume zero-fluxes at boundary and top
               n = k - kb + 1 ! layer number
               d(n, j) = rhs(j, k)
            end do
         end do

         do k = kb, kt - 1 ! assume zero-fluxes at boundary and top
            n = k - kb + 1 ! layer number
            dvol1i = 1.0_dp / max(vol1(k), dtol) ! dtol: safety
            if (testdryflood == 2) dvol1i = 1.0_dp / max(vol1(k), epshu * ba(kk) / max(kt - kb + 1, 1))
            dvol2i = 1.0_dp / max(vol1(k + 1), dtol) ! dtol: safety
            if (testdryflood == 2) dvol2i = 1.0_dp / max(vol1(k + 1), epshu * ba(kk) / max(kt - kb + 1, 1))
            dtba = dt_loc * ba(kk)
            dtbazi = dtba / max(1.0e-4_dp, 0.5_dp * (zws(k + 1) - zws(k - 1))) ! another safety check
            ozmid = 0.0_dp
            if (xlozmidov > 0.0_dp) then
               if (rho(k) < rho(k - 1)) then
                  bruns = (rho(k - 1) - rho(k)) / (0.5_dp * (zws(k + 1) - zws(k - 1))) ! = -drhodz
                  bruns = sqrt(bruns * ag / rhomean)
                  ozmid = 0.2_dp * xlozmidov * xlozmidov * bruns
               end if
            end if

            do j = 1, NUMCONST
               ! diffusion
               if (jased == 4 .and. j >= ISED1 .and. j <= ISEDN) then ! sediment d3d
                  fluxfac = (ozmid + mtd%seddif(j - ISED1 + 1, k) / tpsnumber(j - ISED1 + 1) + get_difsedw(kk, j)) * dtbazi
               else
                  fluxfac = (sigdifi(j) * vicwws(k) + get_difsedw(kk, j) + ozmid) * dtbazi
                  if (j == ISALT) then
                     difwws(k) = (sigdifi(j) * vicwws(k) + get_difsedw(kk, j) + ozmid)
                  end if
               end if

               b(n, j) = b(n, j) + fluxfac * dvol1i
               c(n, j) = c(n, j) - fluxfac * dvol1i

               b(n + 1, j) = b(n + 1, j) + fluxfac * dvol2i
               a(n + 1, j) = a(n + 1, j) - fluxfac * dvol2i

               ! advection
               if (thetavert(j) > 0.0_dp) then ! semi-implicit, use central scheme
                  if (jased > 0 .and. jaimplicitfallvelocity == 0) then ! explicit fallvelocity
                     if (jased < 4) then
                        qw_loc = qw(k) - wsf(j) * a1(kk)
                     else if (j >= ISED1 .and. j <= ISEDN) then
                        qw_loc = qw(k) - mtd%ws(k, j - ISED1 + 1) * a1(kk)
                     end if
                  else
                     qw_loc = qw(k)
                  end if
                  fluxfac = qw_loc * 0.5_dp * thetavert(j) * dt_loc

                  a(n + 1, j) = a(n + 1, j) - fluxfac * dvol2i
                  b(n + 1, j) = b(n + 1, j) - fluxfac * dvol2i

                  b(n, j) = b(n, j) + fluxfac * dvol1i
                  c(n, j) = c(n, j) + fluxfac * dvol1i
               end if

               if (jased > 0 .and. jaimplicitfallvelocity == 1) then
                  fluxfac = 0.0_dp
                  if (jased == 4) then
                     if (j >= ISED1 .and. j <= ISEDN) then
                        fluxfac = mtd%ws(k, j - ISED1 + 1) * a1(kk) * dt_loc
                     else
                        ! tracers
                        fluxfac = wsf(j) * a1(kk) * dt_loc
                     end if
                  else
                     fluxfac = wsf(j) * a1(kk) * dt_loc
                  end if
                  if (fluxfac > 0.0_dp) then
                     c(n, j) = c(n, j) - fluxfac * dvol1i
                     b(n + 1, j) = b(n + 1, j) + fluxfac * dvol2i
                  else if (fluxfac < 0.0_dp) then
                     b(n, j) = b(n, j) - fluxfac * dvol1i
                     a(n + 1, j) = a(n + 1, j) + fluxfac * dvol2i
                  end if
               end if
            end do
         end do

!     solve system(s)
         do j = 1, NUMCONST
            ! make this compiler safe, ie don't pass first element and assume memory contiguity
            nel = kt - kb + 1
            ac(1:nel) = a(1:nel, j)
            bc(1:nel) = b(1:nel, j)
            cc(1:nel) = c(1:nel, j)
            dc(1:nel) = d(1:nel, j)
            call tridag(ac, bc, cc, dc, e, sol, nel)
            sed(j, kb:kt) = sol(1:nel)
            sed(j, kt + 1:ktx) = sed(j, kt)
         end do
      end do
      !$OMP END PARALLEL DO
      if (timon) call timstop(ithndl)
   end subroutine solve_vertical

   !> Get vertical diffusivity at a given node and add constituent specific molecular diffusivity.
   pure function get_difsedw(node_index, constituent_index) result(val)
      use m_physcoef, only: dicoww
      use m_transport, only: molecular_diffusion_coeff
      use precision, only: dp
      integer, intent(in) :: node_index !< base node index (1:Ndxi)
      integer, intent(in) :: constituent_index !< constituent index (1:NUMCONST)
      real(kind=dp) :: val ! return value

      !total diffusivity is user specified background diffusivity plus molecular diffusivity depending on constituent
      val = dicoww%get(node_index) + molecular_diffusion_coeff(constituent_index)
   end function get_difsedw

end module m_solve_vertical
