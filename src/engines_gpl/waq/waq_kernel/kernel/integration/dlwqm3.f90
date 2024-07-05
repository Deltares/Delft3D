!!  Copyright (C)  Stichting Deltares, 2012-2024.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.
module m_dlwqm3
    use m_waq_precision

    implicit none

contains

    !> Fills in the rhs and the initial guess for the adjusting theta algorithm
    subroutine dlwqm3(idt, isys, nosys, notot, noseg, &
                      conc, deriv, volold, nobnd, bound, &
                      noq, ipoint, flowtot, disptot, theta, &
                      diag, iscale, rhs, sol)

        use timers

        implicit none

        integer(kind=int_wp), intent(in   ) :: idt                 !< Time step
        integer(kind=int_wp), intent(in   ) :: isys                !< Current active substance
        integer(kind=int_wp), intent(in   ) :: nosys               !< Number of active substances
        integer(kind=int_wp), intent(in   ) :: notot               !< Total number of substances
        integer(kind=int_wp), intent(in   ) :: noseg               !< Number of cells or segments
        real(kind=real_wp),   intent(in   ) :: conc(notot, noseg)  !< Concentrations
        real(kind=real_wp),   intent(in   ) :: deriv(notot, noseg) !< Processes and discharges (divided by the time step idt)
        real(kind=real_wp),   intent(in   ) :: volold(noseg)       !< Segment volumes at the previous time
        integer(kind=int_wp), intent(in   ) :: nobnd               !< Number of boundary segments
        real(kind=real_wp),   intent(in   ) :: bound(nosys, nobnd) !< Boundary concentrions
        integer(kind=int_wp), intent(in   ) :: noq                 !< Number of exchanges
        integer(kind=int_wp), intent(in   ) :: ipoint(4, noq)      !< Exchange pointers (dim: 4 x noq)
        real(kind=real_wp),   intent(in   ) :: flowtot(noq)        !< Flows plus additional velos. (dim: noq)
        real(kind=real_wp),   intent(in   ) :: disptot(noq)        !< Dispersion plus additional dipers. (dim: noq)
        real(kind=real_wp),   intent(in   ) :: theta(noq)          !< Variable theta coefficients
        real(kind=dp),        intent(in   ) :: diag(noseg + nobnd) !< Diagonal of the matrix (lhs)
        integer(kind=int_wp), intent(in   ) :: iscale              !< 0: no diagonal scaling
                                                                   !< 1: diagonal scaling
        real(kind=dp),        intent(  out) :: rhs(noseg + nobnd)  !< Right hand side
        real(kind=dp),        intent(  out) :: sol(noseg + nobnd)  !< Initial guess
       
        ! Local variables
        integer(kind=int_wp) :: ifrom  !< Index cell from
        integer(kind=int_wp) :: ito    !< Index cell to
        real(kind=real_wp)   :: ci     !< Concentration from
        real(kind=real_wp)   :: cj     !< Concentration to
        real(kind=real_wp)   :: fluxij !< Flux from cell i to cell j
        integer(kind=int_wp) :: iseg   !< Index current cell
        integer(kind=int_wp) :: ibnd   !< Index current boundary cell
        integer(kind=int_wp) :: iq     !< Index current edge

        integer(kind=int_wp) :: ithandl = 0

        if (timon) call timstrt("dlwqm3", ithandl)

        ! volumes, processes, and discharges
        do iseg = 1, noseg
            rhs(iseg) = volold(iseg)*conc(isys, iseg)/real(idt) + deriv(isys, iseg)
        end do

        do ibnd = 1, nobnd
            rhs(noseg + ibnd) = bound(isys, ibnd)
        end do

        ! flow and diffusion
        do iq = 1, noq
            ifrom = ipoint(1, iq)
            ito = ipoint(2, iq)
            if (ifrom == 0 .or. ito == 0) cycle

            ! compute from- and to concentrations
            if (ifrom > 0) then
                ci = conc(isys, ifrom)
            else
                ci = bound(isys, -ifrom)
            end if
            if (ito > 0) then
                cj = conc(isys, ito)
            else
                cj = bound(isys, -ito)
            end if

            ! compute flux from i to j
            if (flowtot(iq) > 0) then         ! flow from i to j
                fluxij = flowtot(iq)*ci - disptot(iq)*(cj - ci)
            else                                   ! flow from j to i
                fluxij = flowtot(iq)*cj - disptot(iq)*(cj - ci)
            end if

            ! add flux to both neighbours
            if (ifrom > 0) rhs(ifrom) = rhs(ifrom) - (1 - theta(iq))*fluxij
            if (ito > 0) rhs(ito) = rhs(ito) + (1 - theta(iq))*fluxij
        end do

        ! scale rhs (diagonal scaling to improve convergence of gmres)
        if (iscale == 1) then
            do iseg = 1, noseg + nobnd
                rhs(iseg) = rhs(iseg)/diag(iseg)
            end do
        end if

        ! zero initial guess, try previous concentration for water volumes
        ! ( alternatively take zero vector ). Zero initial guess for boundaries.
        sol = 0.0
        do iseg = 1, noseg
            sol(iseg) = conc(isys, iseg) + 0.01
        end do

        if (timon) call timstop(ithandl)
    end subroutine dlwqm3
end module m_dlwqm3
