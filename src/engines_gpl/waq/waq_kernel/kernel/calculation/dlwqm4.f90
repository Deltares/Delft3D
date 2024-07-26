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
module m_dlwqm4
    use m_waq_precision

    implicit none

contains

    !> Adjust mass balance for adjusting theta algorithm
    subroutine dlwqm4(isys, num_substances_transported, num_substances_total, num_cells, conc, &
                      concvt, num_boundary_conditions, bound, num_exchanges, ipoint, &
                      theta, flowtot, disptot, amass2, ndmpq, &
                      iqdmp, dmpq, idt)

        use timers

        implicit none

        integer(kind=int_wp), intent(in   ) :: isys                  !< Current active substance
        integer(kind=int_wp), intent(in   ) :: num_substances_transported                 !< Number of active substances
        integer(kind=int_wp), intent(in   ) :: num_substances_total                 !< Total number of substances
        integer(kind=int_wp), intent(in   ) :: num_cells                 !< Number of segments
        real(kind=real_wp),   intent(in   ) :: conc(num_substances_total, num_cells)    !< Old concentrations
        real(kind=dp),        intent(in   ) :: concvt(num_cells)         !< First solution estimation by means of local theta method
        integer(kind=int_wp), intent(in   ) :: num_boundary_conditions                 !< Number of boundary segments
        real(kind=real_wp),   intent(in   ) :: bound(num_substances_transported, num_boundary_conditions)   !< Boundary concentrations
        integer(kind=int_wp), intent(in   ) :: num_exchanges                   !< Number of exchanges
        integer(kind=int_wp), intent(in   ) :: ipoint(4, num_exchanges)        !< Exchange pointers
        real(kind=real_wp),   intent(in   ) :: theta(num_exchanges)            !< Local theta coefficients
        real(kind=real_wp),   intent(in   ) :: flowtot(num_exchanges)          !< Flows plus additional velos.
        real(kind=real_wp),   intent(in   ) :: disptot(num_exchanges)          !< Dispersion plus additional dipers.
        real(kind=real_wp),   intent(inout) :: amass2(num_substances_total, 5)      !< amass2(*,1) masses
                                                                     !< amass2(*,2) processes
                                                                     !< amass2(*,3) discharges
                                                                     !< amass2(*,4) incoming boundary transport
                                                                     !< amass2(*,5) outgoing boundary transport
        integer(kind=int_wp), intent(in   ) :: ndmpq                 !< Number of dumped exchanges
        integer(kind=int_wp), intent(in   ) :: iqdmp(num_exchanges)            !< Indeces dumped exchages
        real(kind=real_wp),   intent(inout) :: dmpq(num_substances_transported, ndmpq, 2) !< dmpq(*,*,1) incoming transport
                                                                     !< dmpq(*,*,2) outgoing transport
        integer(kind=int_wp), intent(in   ) :: idt                   !< Time step

        ! Local variables
        real(kind=real_wp)   :: cio    !< Old from concentration
        real(kind=real_wp)   :: cjo    !< Old to concentration
        real(kind=real_wp)   :: cin    !< New from concentration
        real(kind=real_wp)   :: cjn    !< New to concentration
        real(kind=real_wp)   :: fluxij !< Flux from i to j
        integer(kind=int_wp) :: ifrom  !< Index from cell
        integer(kind=int_wp) :: ito    !< Index to cell
        integer(kind=int_wp) :: iq     !< Current edge
        integer(kind=int_wp) :: ithandl = 0

        if (timon) call timstrt("dlwqm4", ithandl)

        ! flow and diffusion
        do iq = 1, num_exchanges
            ifrom = ipoint(1, iq)
            ito = ipoint(2, iq)

            ! only compute where needed
            if (ifrom > 0 .and. ito > 0 .and. iqdmp(iq) == 0) cycle
            if (ifrom == 0 .or. ito == 0) cycle

            if (ifrom > 0) then
                cio = conc(isys, ifrom)
                cin = concvt(ifrom)
            else
                cio = bound(isys, -ifrom)
                cin = bound(isys, -ifrom)
            end if

            if (ito > 0) then
                cjo = conc(isys, ito)
                cjn = concvt(ito)
            else
                cjo = bound(isys, -ito)
                cjn = bound(isys, -ito)
            end if

            if (flowtot(iq) > 0) then ! flow from i to j
                fluxij = theta(iq)*(flowtot(iq)*cin - disptot(iq)*(cjn - cin)) &
                         + (1 - theta(iq))*(flowtot(iq)*cio - disptot(iq)*(cjo - cio))
            else                      ! flow from j to i
                fluxij = theta(iq)*(flowtot(iq)*cjn - disptot(iq)*(cjn - cin)) &
                         + (1 - theta(iq))*(flowtot(iq)*cjo - disptot(iq)*(cjo - cio))
            end if
            if (ifrom < 0) then
                if (fluxij > 0) then
                    amass2(isys, 4) = amass2(isys, 4) + real(idt)*fluxij
                else
                    amass2(isys, 5) = amass2(isys, 5) - real(idt)*fluxij
                end if
            end if
            if (ito < 0) then
                if (fluxij > 0) then
                    amass2(isys, 5) = amass2(isys, 5) + real(idt)*fluxij
                else
                    amass2(isys, 4) = amass2(isys, 4) - real(idt)*fluxij
                end if
            end if
            if (iqdmp(iq) > 0) then
                if (fluxij > 0) then
                    dmpq(isys, iqdmp(iq), 1) = dmpq(isys, iqdmp(iq), 1) + real(idt)*fluxij
                else
                    dmpq(isys, iqdmp(iq), 2) = dmpq(isys, iqdmp(iq), 2) - real(idt)*fluxij
                end if
            end if
        end do
        if (timon) call timstop(ithandl)
    end subroutine dlwqm4
end module m_dlwqm4
