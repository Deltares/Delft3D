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
module m_dlwqm5
    use m_waq_precision

    implicit none

contains


    !> Zalezac flux correction procedure
    !! Procedure:
    !! - all wanted corrections are summed per computational volume
    !! - all room for change of concentrations without generating
    !!   new maxima or minima are evaluted per volume
    !! - then those corrections are applied pro-rato
    !! - flux correction accross open boundaries is removed !
    subroutine dlwqm5(idt, isys, num_substances_transported, num_substances_total, num_cells, &
            conc, concvt, volnew, num_boundary_conditions, bound, &
            num_exchanges, num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, ipoint, &
            iknmrk, area, aleng, theta, flowtot, &
            disptot, amass2, ndmpq, iqdmp, &
            dmpq, flux, lim, maxi, mini, &
            l1, l2, m1, m2, n1, &
            n2)

        use timers

        integer(kind = int_wp), intent(in   ) :: idt                   !< Time step
        integer(kind = int_wp), intent(in   ) :: isys                  !< Current active substance
        integer(kind = int_wp), intent(in   ) :: num_substances_transported                 !< Number of active substances
        integer(kind = int_wp), intent(in   ) :: num_substances_total                 !< Total number of substances
        integer(kind = int_wp), intent(in   ) :: num_cells                 !< Number of segments
        real(kind = real_wp),   intent(inout) :: conc(num_substances_total, num_cells)    !< Concentrations
        real(kind = dp),        intent(inout) :: concvt(num_cells)         !< First solution estimation by means of local theta method
        real(kind = real_wp),   intent(in   ) :: volnew(num_cells)         !< Segment volumes at the new time
        integer(kind = int_wp), intent(in   ) :: num_boundary_conditions                 !< Number of boundary segments
        real(kind = real_wp),   intent(in   ) :: bound(num_substances_transported, num_boundary_conditions)   !< Boundary concentrations
        integer(kind = int_wp), intent(in   ) :: num_exchanges                   !< Number of exchanges
        integer(kind = int_wp), intent(in   ) :: num_exchanges_u_dir                  !< Number of exchanges in the first direction
        integer(kind = int_wp), intent(in   ) :: num_exchanges_v_dir                  !< Number of exchanges in the second direction
        integer(kind = int_wp), intent(in   ) :: num_exchanges_z_dir                  !< Number of exchanges in the third direction
        integer(kind = int_wp), intent(in   ) :: ipoint(4, num_exchanges)        !< Exchange pointers
        integer(kind = int_wp), intent(in   ) :: iknmrk(num_cells)         !< Feature array
        real(kind = real_wp),   intent(in   ) :: area(num_exchanges)             !< Surface areas
        real(kind = real_wp),   intent(in   ) :: aleng(2, num_exchanges)         !< From- and to lengths (dim: 2*num_exchanges)
        real(kind = real_wp),   intent(in   ) :: theta(num_exchanges)            !< Local theta coefficients
        real(kind = real_wp),   intent(in   ) :: flowtot(num_exchanges)          !< Flows plus additional velos.
        real(kind = real_wp),   intent(in   ) :: disptot(num_exchanges)          !< Dispersion plus additional dipers.
        real(kind = real_wp),   intent(inout) :: amass2(num_substances_total, 5)      !< Areawide mass balance arrays
        integer(kind = int_wp), intent(in   ) :: ndmpq                 !< Number of dumped discharges
        integer(kind = int_wp), intent(in   ) :: iqdmp(num_exchanges)            !< Pointer dumped exchages
        real(kind = real_wp),   intent(inout) :: dmpq(num_substances_transported, ndmpq, 2) !< Mass balance array per monitoring area
        real(kind = real_wp),   intent(inout) :: flux(num_exchanges)             !< Flux corrections
        real(kind = real_wp),   intent(inout) :: lim(num_exchanges)              !< Limiter
        real(kind = real_wp),   intent(inout) :: maxi(num_cells)           !< Workspace
        real(kind = real_wp),   intent(inout) :: mini(num_cells)           !< Workspace
        real(kind = real_wp),   intent(inout) :: l1  (num_cells)           !< Workspace
        real(kind = real_wp),   intent(inout) :: l2  (num_cells)           !< Workspace
        real(kind = real_wp),   intent(inout) :: m1  (num_cells)           !< Workspace
        real(kind = real_wp),   intent(inout) :: m2  (num_cells)           !< Workspace
        real(kind = real_wp),   intent(inout) :: n1  (num_cells)           !< Workspace
        real(kind = real_wp),   intent(inout) :: n2  (num_cells)           !< Workspace

        ! Local variables
        real(kind = real_wp)   :: length
        real(kind = real_wp)   :: cio   !< Old concentration from cell
        real(kind = real_wp)   :: cjo   !< Old concentration to cell
        real(kind = real_wp)   :: cin   !< New (local theta) concentration from cell
        real(kind = real_wp)   :: cjn   !< New (local theta) concentration to cell
        integer(kind = int_wp) :: ifrom !< From- and to cell indices
        integer(kind = int_wp) :: ito   !< From- and to cell indices
        integer(kind = int_wp) :: iseg  !< Current segment
        integer(kind = int_wp) :: iq    !< Current edge

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("dlwqm5", ithandl)

        ! initialisation
        do iq = 1, num_exchanges
            flux(iq) = 0.0
            lim (iq) = 1.0
        end do

        do iseg = 1, num_cells
            maxi(iseg) = concvt(iseg)
            mini(iseg) = concvt(iseg)
            l1(iseg) = 0.0
            l2(iseg) = 0.0
            m1(iseg) = 0.0
            m2(iseg) = 0.0
            n1(iseg) = 0.0
            n2(iseg) = 0.0
        end do

        ! compute flux corrections
        do iq = 1, num_exchanges
            ifrom = ipoint(1, iq)
            ito = ipoint(2, iq)

            if (ifrom == 0 .or.  ito == 0) cycle
            if (ifrom > 0) then
                if (.not. btest(iknmrk(ifrom), 0)) cycle       ! identified dry at start and end of timestep
            end if                                              ! aggregated time step can be wet in between
            if (ito   > 0) then                           ! start and end, that is why a check on 1 cm3/s
                if (.not. btest(iknmrk(ito), 0)) cycle       ! life is not easy
            end if

            if (ifrom > 0) then
                cio = conc  (isys, ifrom)
                cin = concvt(ifrom)
            else
                cio = bound (isys, -ifrom)
                cin = bound (isys, -ifrom)
            end if

            if (ito   > 0) then
                cjo = conc  (isys, ito)
                cjn = concvt(ito)
            else
                cjo = bound (isys, -ito)
                cjn = bound (isys, -ito)
            end if

            if (theta(iq) < 1.0E-25) then ! Lax-Wendroff flux correction at `explicit' edges (theta = 0)
                length = aleng(1, iq) + aleng(2, iq)
                if (length > 1.0E-25) then
                    if (flowtot(iq) > 0) then          ! flow from i to j
                        flux(iq) = (aleng(1, iq) / length - (flowtot(iq) * real(idt)) / (2 * area(iq) * length)) * flowtot(iq) * (cjo - cio)
                    else                                    ! flow from j to i
                        flux(iq) = (-aleng(2, iq) / length - (flowtot(iq) * real(idt)) / (2 * area(iq) * length)) * flowtot(iq) * (cjo - cio)
                    end if
                end if
            else                          ! central flux correction at implicit edges (theta > 0)
                if (flowtot(iq) > 0) then ! flow from i to j
                    flux(iq) = (1.0 - theta(iq)) * (flowtot(iq) * (cio + cjo) / 2.0 - flowtot(iq) * cio) &
                            + theta(iq) * (flowtot(iq) * (cin + cjn) / 2.0 - flowtot(iq) * cin)
                else ! flow from j to i
                    flux(iq) = (1.0 - theta(iq)) * (flowtot(iq) * (cio + cjo) / 2.0 - flowtot(iq) * cjo) &
                            + theta(iq) * (flowtot(iq) * (cin + cjn) / 2.0 - flowtot(iq) * cjn)
                end if
            end if
            if (flux(iq) * (cin - cjn)>0)  flux(iq) = 0.0 ! antidiffusion should not behave as diffusion.
        end do

        ! compute limiter following Zalesak
        do iq = 1, num_exchanges
            ifrom = ipoint(1, iq)
            ito = ipoint(2, iq)

            if (ifrom == 0 .or.  ito == 0) cycle
            if (ifrom > 0) then
                if (.not. btest(iknmrk(ifrom), 0)) cycle       ! identified dry at start and end of timestep
            end if                                              ! aggregated time step can be wet in between
            if (ito   > 0) then                           ! start and end, that is why a check on 1 cm3/s
                if (.not. btest(iknmrk(ito), 0)) cycle       ! life is not easy
            end if

            if (ifrom > 0) then
                if (ito  > 0) then
                    maxi(ifrom) = max(maxi(ifrom), concvt(ito))
                    mini(ifrom) = min(mini(ifrom), concvt(ito))
                else
                    maxi(ifrom) = max(maxi(ifrom), bound(isys, -ito))
                    mini(ifrom) = min(mini(ifrom), bound(isys, -ito))
                end if

                l1(ifrom) = l1(ifrom) + real(idt) * max(0.0, -flux(iq))
                l2(ifrom) = l2(ifrom) + real(idt) * max(0.0, flux(iq))
            end if

            if (ito   > 0) then
                if (ifrom > 0) then
                    maxi(ito) = max(maxi(ito), concvt(ifrom))
                    mini(ito) = min(mini(ito), concvt(ifrom))
                else
                    maxi(ito) = max(maxi(ito), bound(isys, -ifrom))
                    mini(ito) = min(mini(ito), bound(isys, -ifrom))
                end if

                l1(ito) = l1(ito) + real(idt) * max(0.0, flux(iq))
                l2(ito) = l2(ito) + real(idt) * max(0.0, -flux(iq))
            end if
        end do

        do iseg = 1, num_cells
            m1(iseg) = volnew(iseg) * (maxi(iseg) - concvt(iseg))
            m2(iseg) = volnew(iseg) * (concvt(iseg) - mini(iseg))
            if (l1(iseg) > 1.0E-25) n1(iseg) = min(1.0, m1(iseg) / l1(iseg))
            if (l2(iseg) > 1.0E-25) n2(iseg) = min(1.0, m2(iseg) / l2(iseg))
        end do

        do iq = 1, num_exchanges
            ifrom = ipoint(1, iq)
            ito = ipoint(2, iq)
            if (ifrom > 0 .and.  ito > 0) then
                if (flux(iq) < 0) then
                    lim(iq) = min(n1(ifrom), n2(ito))
                else
                    lim(iq) = min(n1(ito), n2(ifrom))
                end if
            end if
        end do

        ! store the result
        do iseg = 1, num_cells
            if (btest(iknmrk(iseg), 0)) then
                conc(isys, iseg) = concvt(iseg)
            else
                conc(isys, iseg) = 0.0
            end if
        end do

        ! solution estimation 2: local theta FCT solution estimation
        ! apply limited flux correction
        do iq = 1, num_exchanges
            ifrom = ipoint(1, iq)
            ito = ipoint(2, iq)
            if (ifrom <= 0 .and.  ito <= 0) cycle
            if (ifrom > 0) then
                if (.not. btest(iknmrk(ifrom), 0)) cycle       ! identified dry at start and end of timestep
            end if                                              ! aggregated time step can be wet in between
            if (ito   > 0) then                           ! start and end. Life is not easy
                if (.not. btest(iknmrk(ito), 0)) cycle
            end if
            if (ifrom > 0 .and.  ito > 0) then
                if (volnew(ifrom) > 1.0e-25 .and. volnew(ito) > 1.0e-25) then
                    conc(isys, ifrom) = conc(isys, ifrom) - lim(iq) * real(idt) * flux(iq) / volnew(ifrom)
                    conc(isys, ito) = conc(isys, ito) + lim(iq) * real(idt) * flux(iq) / volnew(ito)
                    if (iqdmp(iq) > 0) then
                        if (flux(iq) > 0) then
                            dmpq(isys, iqdmp(iq), 1) = dmpq(isys, iqdmp(iq), 1) + real(idt) * lim(iq) * flux(iq)
                        else
                            dmpq(isys, iqdmp(iq), 2) = dmpq(isys, iqdmp(iq), 2) - real(idt) * lim(iq) * flux(iq)
                        end if
                    end if
                end if
            end if
        end do
        if (timon) call timstop (ithandl)
    end subroutine dlwqm5
end module m_dlwqm5
