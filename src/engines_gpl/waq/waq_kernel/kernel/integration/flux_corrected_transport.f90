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
module m_flux_corrected_transport_fct
    use m_waq_precision
    use timers

    implicit none

    private
    public :: first_step_fct, apply_fct_salezac, apply_fct_boris_book_5_12_14, apply_fct_boris_book_scheme_21_22

contains


    !> Calculates first step of Flux Corrected Transport (FCT) scheme
    !! Makes derivatives, upwind in space, advection only, for first step of FCT
    !! First step of FCT consists of first order, upwind, monotonous, advection
    !! step, with numerical diffusion. In the correction step an anti diffusion
    !! is computed, to arrive at 2nd order Lax Wendroff, if no artificial minima
    !! and maxima are generated, otherwise the flux limiter will become active.
    !! The desired diffusion is subtracted from the anti diffusion in the correction
    !! step if a positive diffusion remains, then no correction takes place, if a
    !! negative diffusion remains, it is applied to the degree possible.
    subroutine first_step_fct(num_substances_transported, num_substances_total, num_cells, num_exchanges, &
            num_velocity_arrays, velo, area, flow, ipoint, ivpnt, &
            conc, bound, idt, deriv, iaflag, &
            amass2)

        integer(kind = int_wp), intent(in) :: num_substances_transported     !< number of transported substances
        integer(kind = int_wp), intent(in) :: num_substances_total           !< total number of substances
        integer(kind = int_wp), intent(in) :: num_cells                      !< number of computational volumes
        integer(kind = int_wp), intent(in) :: num_exchanges                  !< total number of interfaces
        integer(kind = int_wp), intent(in) :: num_velocity_arrays            !< number additional velocities
        real(kind = real_wp), intent(in) :: velo(num_velocity_arrays, num_exchanges)!< array with additional velocities
        real(kind = real_wp), intent(in) :: area(num_exchanges)            !< exchange areas in m2
        real(kind = real_wp), intent(in) :: flow(num_exchanges)            !< flows through the exchange areas in m3/s
        integer(kind = int_wp), intent(in) :: ipoint(4, num_exchanges)       !< from, to, from-1, to+1 volume numbers
        integer(kind = int_wp), intent(in) :: ivpnt(num_substances_transported) !< additional velocity number per substance
        real(kind = real_wp), intent(in) :: conc(num_substances_total, num_cells)!< concentrations at previous time level
        real(kind = real_wp), intent(in) :: bound(num_substances_transported, *)      !< open boundary concentrations
        integer(kind = int_wp), intent(in) :: idt                  !< time step in seconds
        real(kind = real_wp), intent(inout) :: deriv(num_substances_total, num_cells)!< derivatives of the concentraions
        integer(kind = int_wp), intent(in) :: iaflag               !< if 1 then accumulate mass in report array
        real(kind = real_wp), intent(inout) :: amass2(num_substances_total, 5)     !< report array for monitoring file

        ! Local variables     :
        integer(kind = int_wp) :: iq              ! loop counter exchanges
        integer(kind = int_wp) :: substance_i            ! loop counter substance
        integer(kind = int_wp) :: ifrom, ito      ! from and to volume numbers
        real(kind = real_wp) :: a               ! this area
        real(kind = real_wp) :: q               ! flow for this exchange
        real(kind = real_wp) :: v               ! flow for this substance
        real(kind = real_wp) :: dq              ! total flux from and to

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("dlwq50", ithandl)

        ! loop along number of exchanges
        do iq = 1, num_exchanges
            ! initialisations , check for transport anyhow
            ifrom = ipoint(1, iq)
            ito = ipoint(2, iq)
            if (ifrom == 0 .or.  ito == 0) cycle
            if (ifrom <= 0 .and. ito <= 0) cycle

            a = area(iq)
            q = flow(iq)
            if (ifrom < 0) goto 20
            if (ito   < 0) goto 40

            ! the regular case
            do substance_i = 1, num_substances_transported
                v = q
                if (ivpnt(substance_i) > 0) v = v + velo(ivpnt(substance_i), iq) * a
                if (v > 0.0) then
                    dq = v * conc(substance_i, ifrom)
                else
                    dq = v * conc(substance_i, ito)
                endif
                deriv(substance_i, ifrom) = deriv(substance_i, ifrom) - dq
                deriv(substance_i, ito) = deriv(substance_i, ito) + dq
            end do
            cycle

            ! The 'from' element was a boundary.
            20    do substance_i = 1, num_substances_transported
                v = q
                if (ivpnt(substance_i) > 0) v = v + velo(ivpnt(substance_i), iq) * a
                if (v > 0.0) then
                    dq = v * bound(substance_i, -ifrom)
                    if (iaflag == 1) amass2(substance_i, 4) = amass2(substance_i, 4) + dq * idt
                else
                    dq = v * conc (substance_i, ito)
                    if (iaflag == 1) amass2(substance_i, 5) = amass2(substance_i, 5) - dq * idt
                endif
                deriv(substance_i, ito) = deriv(substance_i, ito) + dq
            end do
            cycle

            ! The 'to' element was a boundary.
            40    do substance_i = 1, num_substances_transported
                v = q
                if (ivpnt(substance_i) > 0) v = v + velo(ivpnt(substance_i), iq) * a
                if (v > 0.0) then
                    dq = v * conc (substance_i, ifrom)
                    if (iaflag == 1) amass2(substance_i, 5) = amass2(substance_i, 5) + dq * idt
                else
                    dq = v * bound(substance_i, -ito)
                    if (iaflag == 1) amass2(substance_i, 4) = amass2(substance_i, 4) - dq * idt
                endif
                deriv(substance_i, ifrom) = deriv(substance_i, ifrom) - dq
            end do

            ! end of the loop over exchanges
        end do

        if (timon) call timstop (ithandl)
    end subroutine first_step_fct

    !> Zalezac flux correction procedure
    !! Procedure:
    !! - all wanted corrections are summed per computational volume
    !! - all room for change of concentrations without generating
    !!   new maxima or minima are evaluted per volume
    !! - then those corrections are applied pro-rato
    !! - flux correction accross open boundaries is removed !
    subroutine apply_fct_salezac(idt, substance_i, num_substances_transported, &
            num_substances_total, num_cells, &
            conc, concvt, volnew, num_boundary_conditions, bound, &
            num_exchanges, num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, ipoint, &
            iknmrk, area, aleng, theta, flowtot, &
            disptot, amass2, ndmpq, iqdmp, &
            dmpq, flux, lim, maxi, mini, &
            l1, l2, m1, m2, n1, &
            n2)

        integer(kind = int_wp), intent(in) :: idt                   !< Time step
        integer(kind = int_wp), intent(in) :: substance_i                  !< Current active substance
        integer(kind = int_wp), intent(in) :: num_substances_transported                 !< Number of active substances
        integer(kind = int_wp), intent(in) :: num_substances_total                 !< Total number of substances
        integer(kind = int_wp), intent(in) :: num_cells                 !< Number of segments
        real(kind = real_wp), intent(inout) :: conc(num_substances_total, num_cells)    !< Concentrations
        real(kind = dp), intent(inout) :: concvt(num_cells)         !< First solution estimation by means of local theta method
        real(kind = real_wp), intent(in) :: volnew(num_cells)         !< Segment volumes at the new time
        integer(kind = int_wp), intent(in) :: num_boundary_conditions                 !< Number of boundary segments
        real(kind = real_wp), intent(in) :: bound(num_substances_transported, num_boundary_conditions)   !< Boundary concentrations
        integer(kind = int_wp), intent(in) :: num_exchanges               !< Number of exchanges
        integer(kind = int_wp), intent(in) :: num_exchanges_u_dir         !< Number of exchanges in the first direction
        integer(kind = int_wp), intent(in) :: num_exchanges_v_dir        !< Number of exchanges in the second direction
        integer(kind = int_wp), intent(in) :: num_exchanges_z_dir         !< Number of exchanges in the third direction
        integer(kind = int_wp), intent(in) :: ipoint(4, num_exchanges)    !< Exchange pointers
        integer(kind = int_wp), intent(in) :: iknmrk(num_cells)           !< Feature array
        real(kind = real_wp), intent(in) :: area(num_exchanges)             !< Surface areas
        real(kind = real_wp), intent(in) :: aleng(2, num_exchanges)      !< From- and to lengths (dim: 2*num_exchanges)
        real(kind = real_wp), intent(in) :: theta(num_exchanges)            !< Local theta coefficients
        real(kind = real_wp), intent(in) :: flowtot(num_exchanges)          !< Flows plus additional velos.
        real(kind = real_wp), intent(in) :: disptot(num_exchanges)          !< Dispersion plus additional dipers.
        real(kind = real_wp), intent(inout) :: amass2(num_substances_total, 5)      !< Areawide mass balance arrays
        integer(kind = int_wp), intent(in) :: ndmpq                 !< Number of dumped discharges
        integer(kind = int_wp), intent(in) :: iqdmp(num_exchanges)            !< Pointer dumped exchages
        real(kind = real_wp), intent(inout) :: dmpq(num_substances_transported, ndmpq, 2) !< Mass balance array per monitoring area
        real(kind = real_wp), intent(inout) :: flux(num_exchanges)             !< Flux corrections
        real(kind = real_wp), intent(inout) :: lim(num_exchanges)              !< Limiter
        real(kind = real_wp), intent(inout) :: maxi(num_cells)           !< Workspace
        real(kind = real_wp), intent(inout) :: mini(num_cells)           !< Workspace
        real(kind = real_wp), intent(inout) :: l1  (num_cells)           !< Workspace
        real(kind = real_wp), intent(inout) :: l2  (num_cells)           !< Workspace
        real(kind = real_wp), intent(inout) :: m1  (num_cells)           !< Workspace
        real(kind = real_wp), intent(inout) :: m2  (num_cells)           !< Workspace
        real(kind = real_wp), intent(inout) :: n1  (num_cells)           !< Workspace
        real(kind = real_wp), intent(inout) :: n2  (num_cells)           !< Workspace

        ! Local variables
        real(kind = real_wp) :: length
        real(kind = real_wp) :: cio   !< Old concentration from cell
        real(kind = real_wp) :: cjo   !< Old concentration to cell
        real(kind = real_wp) :: cin   !< New (local theta) concentration from cell
        real(kind = real_wp) :: cjn   !< New (local theta) concentration to cell
        integer(kind = int_wp) :: ifrom !< From- and to cell indices
        integer(kind = int_wp) :: ito   !< From- and to cell indices
        integer(kind = int_wp) :: cell_i  !< Current segment
        integer(kind = int_wp) :: iq    !< Current edge
        real(kind = real_wp) :: tmp_real_wp
        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("apply_fct_salezac", ithandl)

        ! initialisation
        do iq = 1, num_exchanges
            flux(iq) = 0.0
            lim (iq) = 1.0
        end do

        do cell_i = 1, num_cells
            maxi(cell_i) = concvt(cell_i)
            mini(cell_i) = concvt(cell_i)
            l1(cell_i) = 0.0
            l2(cell_i) = 0.0
            m1(cell_i) = 0.0
            m2(cell_i) = 0.0
            n1(cell_i) = 0.0
            n2(cell_i) = 0.0
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
                cio = conc  (substance_i, ifrom)
                cin = concvt(ifrom)
            else
                cio = bound (substance_i, -ifrom)
                cin = bound (substance_i, -ifrom)
            end if

            if (ito   > 0) then
                cjo = conc  (substance_i, ito)
                cjn = concvt(ito)
            else
                cjo = bound (substance_i, -ito)
                cjn = bound (substance_i, -ito)
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
                    tmp_real_wp = concvt(ito)
                    maxi(ifrom) = max(maxi(ifrom), tmp_real_wp)
                    mini(ifrom) = min(mini(ifrom), tmp_real_wp)
                else
                    tmp_real_wp = bound(substance_i, -ito)
                    maxi(ifrom) = max(maxi(ifrom), tmp_real_wp)
                    mini(ifrom) = min(mini(ifrom), tmp_real_wp)
                end if

                l1(ifrom) = l1(ifrom) + real(idt) * max(0.0, -flux(iq))
                l2(ifrom) = l2(ifrom) + real(idt) * max(0.0, flux(iq))
            end if

            if (ito   > 0) then
                if (ifrom > 0) then
                    tmp_real_wp = concvt(ifrom)
                    maxi(ito) = max(maxi(ito), tmp_real_wp)
                    mini(ito) = min(mini(ito), tmp_real_wp)
                else
                    tmp_real_wp = bound(substance_i, -ifrom)
                    maxi(ito) = max(maxi(ito), tmp_real_wp)
                    mini(ito) = min(mini(ito), tmp_real_wp)
                end if

                l1(ito) = l1(ito) + real(idt) * max(0.0, flux(iq))
                l2(ito) = l2(ito) + real(idt) * max(0.0, -flux(iq))
            end if
        end do

        do cell_i = 1, num_cells
            m1(cell_i) = volnew(cell_i) * (maxi(cell_i) - concvt(cell_i))
            m2(cell_i) = volnew(cell_i) * (concvt(cell_i) - mini(cell_i))
            if (l1(cell_i) > 1.0E-25) n1(cell_i) = min(1.0, m1(cell_i) / l1(cell_i))
            if (l2(cell_i) > 1.0E-25) n2(cell_i) = min(1.0, m2(cell_i) / l2(cell_i))
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
        do cell_i = 1, num_cells
            if (btest(iknmrk(cell_i), 0)) then
                conc(substance_i, cell_i) = concvt(cell_i)
            else
                conc(substance_i, cell_i) = 0.0
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
                    conc(substance_i, ifrom) = conc(substance_i, ifrom) - lim(iq) * real(idt) * flux(iq) / volnew(ifrom)
                    conc(substance_i, ito) = conc(substance_i, ito) + lim(iq) * real(idt) * flux(iq) / volnew(ito)
                    if (iqdmp(iq) > 0) then
                        if (flux(iq) > 0) then
                            dmpq(substance_i, iqdmp(iq), 1) = dmpq(substance_i, iqdmp(iq), 1) + real(idt) * lim(iq) * flux(iq)
                        else
                            dmpq(substance_i, iqdmp(iq), 2) = dmpq(substance_i, iqdmp(iq), 2) - real(idt) * lim(iq) * flux(iq)
                        end if
                    end if
                end if
            end if
        end do
        if (timon) call timstop (ithandl)
    end subroutine apply_fct_salezac

    !> Performs flux correction in the direction of flow according to Boris and Book.
    !! This routine makes for the num_substances_transported transported substaces the flux correction term after
    !! the upwind advection step is set by dlwq50/18 producing CONC2(num_substances_total,num_cells) array.
    !! The numerical diffusion of the upwind method amounts to 0.5*v*dx - 0.5*v*v*dt. The
    !! numerical diffusion is subtracted from the prescribed diffusion. The resulting flux
    !! correction term, if negative, is applied as negative diffusion. This negative diffusion
    !! term is limited to the value that new maxima and minima would result.
    !! the criterion against new maxima and minima is measured compared to the old level
    !! concentration field in CONC(num_substances_total,num_cells).
    !! This method of Boris and Book looks in the flow direction only to limit the flux
    !! correction term. Salezac's method looks around a cell, but that has disadvantages in
    !! stratified systems.
    !! The application is subject to the following switches:
    !! - 1) no dispersion accross open boundaries (bit 1 of integration_id is 1)
    !! - 2) first order processing accross open boundaries (bit 2 of integration_id is 1)
    !! Because the routine corrects the CONC2 array, the flux correction (as mass/timestep)
    !! is divided by the new volume of the cell to get concentratiosn again.
    !! This routine also accumulates on the fly the mass balance information for the whole area in
    !! the AMASS2(num_substances_total,5) array. This array is printed as header for every time step in the monitoring
    !! file.
    !! Furthermore the fluxes in and out of monitoring areas for detail balances are accumulated on
    !! the fly. IQDMP(num_exchanges) indicates which flux needs to be accumulated in what balance.
    subroutine apply_fct_boris_book_5_12_14(num_substances_transported, num_substances_total, num_cells, num_exchanges_u_dir, &
            num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges, num_dispersion_arrays, num_velocity_arrays, &
            disp, disper, velo, volume, area, flow, &
            aleng, ipoint, iknmrk, idpnt, ivpnt, &
            conc, conc2, bound, integration_id, ilflag, &
            idt, iaflag, amass2, ndmpq, iqdmp, &
            dmpq)

        integer(kind = int_wp), intent(in) :: num_substances_transported                 !< number of transported substances
        integer(kind = int_wp), intent(in) :: num_substances_total                 !< total number of substances
        integer(kind = int_wp), intent(in) :: num_cells                 !< number of computational volumes
        integer(kind = int_wp), intent(in) :: num_exchanges_u_dir                  !< number of interfaces in direction 1
        integer(kind = int_wp), intent(in) :: num_exchanges_v_dir                  !< number of interfaces in direction 2
        integer(kind = int_wp), intent(in) :: num_exchanges_z_dir                  !< number of interfaces in direction 3
        integer(kind = int_wp), intent(in) :: num_exchanges                   !< total number of interfaces
        integer(kind = int_wp), intent(in) :: num_dispersion_arrays                !< number additional dispersions
        integer(kind = int_wp), intent(in) :: num_velocity_arrays                !< number additional velocities
        real(kind = real_wp), intent(in) :: disp(3)               !< fixed dispersions in the 3 directions
        real(kind = real_wp), intent(in) :: disper(num_dispersion_arrays, num_exchanges)   !< array with additional dispersions
        real(kind = real_wp), intent(in) :: velo  (num_velocity_arrays, num_exchanges)   !< array with additional velocities
        real(kind = real_wp), intent(in) :: volume(num_cells)         !< volumes at end of time step
        real(kind = real_wp), intent(in) :: area(num_exchanges)             !< exchange areas in m2
        real(kind = real_wp), intent(in) :: flow(num_exchanges)             !< flows through the exchange areas in m3/s
        real(kind = real_wp), intent(in) :: aleng(2, num_exchanges)         !< mixing length to and from the exchange area
        integer(kind = int_wp), intent(in) :: ipoint(4, num_exchanges)        !< from, to, from-1, to+1 volume numbers
        integer(kind = int_wp), intent(in) :: iknmrk(num_cells)         !< feature array
        integer(kind = int_wp), intent(in) :: idpnt(num_substances_transported)          !< additional dispersion number per substance
        integer(kind = int_wp), intent(in) :: ivpnt(num_substances_transported)          !< additional velocity number per substance
        real(kind = real_wp), intent(in) :: conc (num_substances_total, num_cells)   !< concentrations at previous time level
        real(kind = real_wp), intent(inout) :: conc2(num_substances_total, num_cells)   !< first estimate to be flux corrected
        real(kind = real_wp), intent(in) :: bound(num_substances_transported, *)       !< open boundary concentrations
        integer(kind = int_wp), intent(in) :: integration_id        !< bit 0: 1 if no dispersion at zero flow
        !< bit 1: 1 if no dispersion across boundaries
        !< bit 2: 1 if lower order across boundaries
        !< bit 3: 1 if mass balance output
        integer(kind = int_wp), intent(in) :: ilflag                !< if 0 then only 3 constant lenght values
        integer(kind = int_wp), intent(in) :: idt                   !< time step in seconds
        integer(kind = int_wp), intent(in) :: iaflag                !< if 1 then accumulate mass in report array
        real(kind = real_wp), intent(inout) :: amass2(num_substances_total, 5)      !< report array for monitoring file
        integer(kind = int_wp), intent(in) :: ndmpq                 !< number of dumped exchanges for mass balances
        integer(kind = int_wp), intent(in) :: iqdmp(num_exchanges)            !< pointer from echange to dump location
        real(kind = real_wp), intent(inout) :: dmpq(num_substances_transported, ndmpq, 2) !< array with mass balance information

        ! Local variables
        integer(kind = int_wp) :: iq             !< loop counter exchanges
        integer(kind = int_wp) :: substance_i           !< loop counter substance
        integer(kind = int_wp) :: noq12          !< number of horizontal exchanges
        integer(kind = int_wp) :: ifrom, ito     !< from   and to   volume numbers
        real(kind = real_wp) :: vfrom, vto       !< from   and to   volumes
        integer(kind = int_wp) :: ifrom_1, ito_1 !< from-1 and to+1 volume numbers
        real(kind = real_wp) :: cfrm_1, cto_1    !< from-1 and to+1 concentration values
        real(kind = real_wp) :: e1, e2, e3       !< limiter help variable
        real(kind = real_wp) :: s                !< limiter sign variable
        real(kind = real_wp) :: a                !< this area
        real(kind = real_wp) :: q                !< flow for this exchange
        real(kind = real_wp) :: e                !< dispersion for this exchange
        real(kind = real_wp) :: al               !< this length
        real(kind = real_wp) :: dl               !< area / length
        real(kind = real_wp) :: d                !< dispersion for this substance
        real(kind = real_wp) :: v                !< flow for this substance
        real(kind = real_wp) :: dq               !< total flux from and to
        integer(kind = int_wp) :: ipb            !< pointer in the mass balance dump array
        real(kind = real_wp) :: f1, f2           !< correction factors central differences
        real(kind = real_wp) :: dqtr, dqtot      !< balances help variables

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("apply_fct_boris_book_5_12_14", ithandl)

        ! loop accross the number of exchanges
        noq12 = num_exchanges_u_dir + num_exchanges_v_dir
        do iq = 1, num_exchanges
            ! initialisations , check for transport anyhow
            ifrom = ipoint(1, iq)
            ito = ipoint(2, iq)
            ifrom_1 = ipoint(3, iq)
            ito_1 = ipoint(4, iq)
            if (ifrom   == 0 .or. ito   == 0) cycle
            if (ifrom <= 0 .and. ito <= 0) cycle

            if (ifrom > 0) then
                if (.not. btest(iknmrk(ifrom), 0)) cycle   ! identified dry at start and end of timestep
            end if
            if (ito   > 0) then
                if (.not. btest(iknmrk(ito), 0)) cycle
            end if

            a = area(iq)
            q = flow(iq)
            if (abs(q) < 10.0e-25 .and. iq <= noq12 .and. btest(integration_id, 0))  cycle
            ! thin dam option, no dispersion at zero flow
            ! Check if exchange is dump exchange, set IPB

            ipb = 0
            if (btest(integration_id, 3)) then
                if (iqdmp(iq) > 0) ipb = iqdmp(iq)
            end if

            ! initialize uniform values
            if (iq <= num_exchanges_u_dir) then
                e = disp (1)
                al = aleng(1, 1)
            elseif (iq <= noq12) then
                e = disp (2)
                al = aleng(2, 1)
            else
                e = disp (3)
                al = aleng(1, 2)
            end if
            if (iq > noq12 + num_exchanges_z_dir) e = 0.0     ! in the bed

            if (ilflag == 1) al = aleng(1, iq) + aleng(2, iq)
            if (al < 1.0e-25) cycle
            if (ilflag == 1) then
                f1 = aleng(1, iq) / al
            else
                f1 = 0.5
            end if
            dl = a / al
            e = e * dl                              ! in m3/s
            !
            if (ifrom < 0) goto 20
            if (ito   < 0) goto 40

            ! correction step
            vfrom = volume(ifrom)
            vto = volume(ito)
            do substance_i = 1, num_substances_transported
                v = q
                if (ivpnt(substance_i) > 0) v = v + velo  (ivpnt(substance_i), iq) * a
                f2 = f1
                if (v < 0.0) f2 = f2 - 1.0
                d = -f2 * v + e + 0.5 * v * v * idt / a / al
                d = min(d, e)
                if (idpnt(substance_i) > 0) d = d + disper(idpnt(substance_i), iq) * dl
                dq = (conc(substance_i, ifrom) - conc(substance_i, ito)) * d * idt
                if (d < 0.0) then
                    e2 = dq
                    s = sign (1.0, e2)
                    select case (ifrom_1)
                    case (1:)
                        cfrm_1 = conc2(substance_i, ifrom_1)
                    case (0)
                        if (s > 0) then
                            cfrm_1 = 0.0
                        else
                            cfrm_1 = 2.0 * conc2(substance_i, ifrom)
                        end if
                    case (:-1)
                        cfrm_1 = bound(substance_i, -ifrom_1)
                    end select
                    select case (ito_1)
                    case (1:)
                        cto_1 = conc2(substance_i, ito_1)
                    case (0)
                        if (s > 0) then
                            cto_1 = 2.0 * conc2(substance_i, ito)
                        else
                            cto_1 = 0.0
                        end if
                    case (:-1)
                        cto_1 = bound(substance_i, -ito_1)
                    end select

                    e1 = (conc2(substance_i, ifrom) - cfrm_1) * vfrom
                    e3 = (cto_1 - conc2(substance_i, ito)) * vto
                    dq = s * max(0.0, min(s * e1, s * e2, s * e3))
                end if

                conc2(substance_i, ifrom) = conc2(substance_i, ifrom) - dq / vfrom
                conc2(substance_i, ito) = conc2(substance_i, ito) + dq / vto

                if (ipb > 0) then                          ! recalculate
                    if (v > 0.0) then                       ! transport
                        dqtr = v * conc(substance_i, ifrom) * idt         ! for the mass
                    else                                         ! balance
                        dqtr = v * conc(substance_i, ito) * idt
                    end if
                    dqtot = dq + dqtr
                    if (dqtot > 0.0) then
                        dmpq(substance_i, ipb, 1) = dmpq(substance_i, ipb, 1) + dqtot
                    else
                        dmpq(substance_i, ipb, 2) = dmpq(substance_i, ipb, 2) - dqtot
                    end if
                end if
            end do
            cycle

            ! The 'from' element was a boundary. Note the 2 options.
            20    vto = volume(ito)
            do substance_i = 1, num_substances_transported
                v = q
                if (ivpnt(substance_i) > 0) v = v + velo  (ivpnt(substance_i), iq) * a
                d = 0.0
                if (.not. btest(integration_id, 1)) then
                    d = e
                    if (idpnt(substance_i) > 0) d = d + disper(idpnt(substance_i), iq) * dl
                end if
                if (.not. btest(integration_id, 2)) then
                    f2 = f1
                    if (v < 0.0) f2 = f2 - 1.0
                    d = d - f2 * v + 0.5 * v * v * idt / a / al
                    d = min(d, e)
                end if
                dq = d * (bound(substance_i, -ifrom) - conc(substance_i, ito))
                conc2(substance_i, ito) = conc2(substance_i, ito) + dq * idt / vto

                if (iaflag == 1) then
                    if (dq > 0) then
                        amass2(substance_i, 4) = amass2(substance_i, 4) + dq * idt
                    else
                        amass2(substance_i, 5) = amass2(substance_i, 5) - dq * idt
                    end if
                end if

                if (ipb > 0) then                          ! recalculate
                    if (v > 0.0) then                       ! transport
                        dqtr = v * bound(substance_i, -ifrom)             ! for the mass
                    else                                         ! balance
                        dqtr = v * conc (substance_i, ito)
                    end if
                    dqtot = dq + dqtr
                    if (dqtot > 0.0) then
                        dmpq(substance_i, ipb, 1) = dmpq(substance_i, ipb, 1) + dqtot * idt
                    else
                        dmpq(substance_i, ipb, 2) = dmpq(substance_i, ipb, 2) - dqtot * idt
                    end if
                end if
            end do
            cycle

            ! The 'to' element was a boundary.
            40    vfrom = volume(ifrom)
            do substance_i = 1, num_substances_transported
                v = q
                if (ivpnt(substance_i) > 0) v = v + velo  (ivpnt(substance_i), iq) * a
                d = 0.0
                if (.not. btest(integration_id, 1)) then
                    d = e
                    if (idpnt(substance_i) > 0) d = d + disper(idpnt(substance_i), iq) * dl
                end if
                if (.not. btest(integration_id, 2)) then
                    f2 = f1
                    if (v < 0) f2 = f2 - 1.0
                    d = -f2 * v + d + 0.5 * v * v * idt / a / al
                    d = min(d, e)
                end if
                dq = d * (conc(substance_i, ifrom) - bound(substance_i, -ito))
                conc2(substance_i, ifrom) = conc2(substance_i, ifrom) - dq * idt / vfrom

                if (iaflag == 1) then
                    if (dq > 0) then
                        amass2(substance_i, 5) = amass2(substance_i, 5) + dq * idt
                    else
                        amass2(substance_i, 4) = amass2(substance_i, 4) - dq * idt
                    end if
                end if

                if (ipb > 0) then                          ! recalculate
                    if (v > 0.0) then                       ! transport
                        dqtr = v * conc (substance_i, ifrom)             ! for the mass
                    else                                         ! balance
                        dqtr = v * bound(substance_i, -ito)
                    end if
                    dqtot = dq + dqtr
                    if (dqtot > 0.0) then
                        dmpq(substance_i, ipb, 1) = dmpq(substance_i, ipb, 1) + dqtot * idt
                    else
                        dmpq(substance_i, ipb, 2) = dmpq(substance_i, ipb, 2) - dqtot * idt
                    end if
                end if
            end do
            ! end of the loop over exchanges
        end do

        if (timon) call timstop (ithandl)
    end subroutine apply_fct_boris_book_5_12_14

    !> Flux correction according to Boris and Book
    subroutine apply_fct_boris_book_scheme_21_22(idt, substance_i, num_substances_transported, num_substances_total, num_cells, &
            conc, concvt, volnew, num_boundary_conditions, bound, &
            num_exchanges, iknmrk, ipoint, area, aleng, &
            theta, flowtot, integration_id, amass2, ndmpq, &
            iqdmp, dmpq)

        integer(kind = int_wp), intent(in) :: idt                   !< Time step
        integer(kind = int_wp), intent(in) :: substance_i                  !< Index of current transported substance
        integer(kind = int_wp), intent(in) :: num_substances_transported      !< Number of transported substances
        integer(kind = int_wp), intent(in) :: num_substances_total                 !< Total number of substances
        integer(kind = int_wp), intent(in) :: num_cells                 !< Number of cells or segments
        real(kind = real_wp), intent(inout) :: conc(num_substances_total, num_cells)    !< Concentrations
        real(kind = dp), intent(inout) :: concvt(num_cells) !< Estimation of first solution by means of local theta method
        real(kind = real_wp), intent(in) :: volnew(num_cells)         !< Cell volumes at the new time
        integer(kind = int_wp), intent(in) :: num_boundary_conditions                 !< Number of boundary cells
        real(kind = real_wp), intent(in) :: bound(num_substances_transported, num_boundary_conditions)   !< Boundary concentrations
        integer(kind = int_wp), intent(in) :: num_exchanges                   !< Number of exchanges
        integer(kind = int_wp), intent(in) :: iknmrk(num_cells)         !< Feature array
        integer(kind = int_wp), intent(in) :: ipoint(4, num_exchanges)        !< Exchange indeces
        real(kind = real_wp), intent(in) :: area(num_exchanges)             !< Surface areas
        real(kind = real_wp), intent(in) :: aleng(2, num_exchanges)         !< From- and to lengths
        real(kind = real_wp), intent(in) :: theta(num_exchanges)            !< Local theta coefficients
        real(kind = real_wp), intent(in) :: flowtot(num_exchanges)          !< Flow plus additional velocities
        integer(kind = int_wp), intent(in) :: integration_id        !< Integration option
        real(kind = real_wp), intent(inout) :: amass2(num_substances_total, 5)      !< Area-wide mass balance array
        integer(kind = int_wp), intent(in) :: ndmpq                 !< Number of dumped discharges
        integer(kind = int_wp), intent(in) :: iqdmp(num_exchanges)            !< Indeces dumped exchages
        real(kind = real_wp), intent(inout) :: dmpq(num_substances_transported, ndmpq, 2) !< Mass balance array for monitoring areas

        ! Local variables
        real(kind = real_wp) :: length   !< Length between midpoints of cells
        real(kind = real_wp) :: cio    !< Old concentration from cell
        real(kind = real_wp) :: cjo    !< Old concentration to cell
        real(kind = real_wp) :: cin    !< New (local theta) concentration from cell
        real(kind = real_wp) :: cjn    !< New (local theta) concentration to cell
        integer(kind = int_wp) :: ifrom  !< Index from-cell
        integer(kind = int_wp) :: ito    !< Index to-cell
        integer(kind = int_wp) :: ifrom1 !< Index from-1 cell
        integer(kind = int_wp) :: itopl1 !< Index to+1 cell
        integer(kind = int_wp) :: cell_i   !< Index current cell
        integer(kind = int_wp) :: iq     !< Index current edge
        real(kind = real_wp) :: aflux    !< Corrective flux
        real(kind = real_wp) :: vfrom    !< 'from' new volume
        real(kind = real_wp) :: vto      !< 'to' new volume
        real(kind = real_wp) :: dq       !< Auxiliary variable for the limiter
        real(kind = real_wp) :: e1       !< Auxiliary variable for the limiter
        real(kind = real_wp) :: e3       !< Auxiliary variable for the limiter
        real(kind = real_wp) :: s        !< Auxiliary variable for the limiter
        real(kind = real_wp) :: cfrm1    !< Concentration of from-1 cell
        real(kind = real_wp) :: ctop1    !< Concentration of to+1 cell

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("apply_fct_boris_book_scheme_21_22", ithandl)

        ! loop accross the number of exchanges
        do iq = 1, num_exchanges
            ! initialisations , check for transport anyhow
            ifrom = ipoint(1, iq)
            ito = ipoint(2, iq)
            ifrom1 = ipoint(3, iq)
            itopl1 = ipoint(4, iq)
            if (ifrom  == 0 .or.  ito    == 0) cycle
            if (ifrom  <= 0 .and. ito    <= 0) cycle
            if (ifrom1 == 0 .or.  itopl1 == 0) cycle   ! no flux correction with closed edges
            if ((ifrom  < 0 .or.  ito    < 0) .and. btest(integration_id, 2)) cycle
            if (ifrom > 0) then
                if (.not. btest(iknmrk(ifrom), 0)) cycle       ! identified dry at start and end of timestep
            endif                                              ! aggregated time step can be wet in between
            if (ito   > 0) then                           ! start and end, that is why a check on 1 cm3/s
                if (.not. btest(iknmrk(ito), 0)) cycle       ! life is not easy
            endif

            ! Compute the difference flux towards 2nd order
            if (ifrom > 0) then
                cio = conc  (substance_i, ifrom)
                cin = concvt(ifrom)
            else
                cio = bound (substance_i, -ifrom)
                cin = bound (substance_i, -ifrom)
            endif

            if (ito   > 0) then
                cjo = conc  (substance_i, ito)
                cjn = concvt(ito)
            else
                cjo = bound (substance_i, -ito)
                cjn = bound (substance_i, -ito)
            endif

            if (theta(iq) < 1.0E-25) then ! Lax-Wendroff flux correction at `explicit' edges (theta = 0)
                length = aleng(1, iq) + aleng(2, iq)
                if (length > 1.0e-25) then
                    if (flowtot(iq) > 0) then          ! flow from i to j
                        aflux = (aleng(1, iq) / length - (flowtot(iq) * real(idt)) / (2 * area(iq) * length)) * flowtot(iq) * (cjo - cio)
                    else                                ! flow from j to i
                        aflux = (-aleng(2, iq) / length - (flowtot(iq) * real(idt)) / (2 * area(iq) * length)) * flowtot(iq) * (cjo - cio)
                    endif
                else
                    aflux = 0.0
                endif
            else                          ! central flux correction at implicit edges (theta > 0)
                if (flowtot(iq) > 0) then ! flow from i to j
                    aflux = (1.0 - theta(iq)) * (flowtot(iq) * (cio + cjo) / 2.0 - flowtot(iq) * cio) &
                            + theta(iq) * (flowtot(iq) * (cin + cjn) / 2.0 - flowtot(iq) * cin)
                else                      ! flow from j to i
                    aflux = (1.0 - theta(iq)) * (flowtot(iq) * (cio + cjo) / 2.0 - flowtot(iq) * cjo) &
                            + theta(iq) * (flowtot(iq) * (cin + cjn) / 2.0 - flowtot(iq) * cjn)
                endif
            endif

            if (aflux * (cin - cjn) > 0) aflux = 0.0 ! antidiffusion should not behave as diffusion.
            dq = aflux * real(idt)

            ! Flux correction at the open boundaries
            if (ifrom < 0) then
                if (itopl1 <= 0) cycle
                vto = volnew(ito)
                s = sign (1.0, dq)
                e3 = (concvt(itopl1) - concvt(ito)) * vto
                dq = s * max(0.0, min(s * dq, s * e3))
                concvt(ito) = concvt(ito) + dq / vto
                if (dq > 0) then
                    amass2(substance_i, 4) = amass2(substance_i, 4) + dq
                else
                    amass2(substance_i, 5) = amass2(substance_i, 5) - dq
                endif
                if (btest(integration_id, 3)) then ! balances active
                    if (iqdmp(iq) > 0) then        ! balances to be updated
                        if (dq > 0.0) then
                            dmpq(substance_i, iqdmp(iq), 1) = dmpq(substance_i, iqdmp(iq), 1) + dq
                        else
                            dmpq(substance_i, iqdmp(iq), 2) = dmpq(substance_i, iqdmp(iq), 2) - dq
                        endif
                    endif
                endif
                cycle
            endif
            if (ito   < 0) then
                if (ifrom1 <= 0) cycle
                vfrom = volnew(ifrom)
                s = sign (1.0, dq)
                e1 = (concvt(ifrom) - concvt(ifrom1)) * vfrom
                dq = s * max(0.0, min(s * e1, s * dq))
                concvt(ifrom) = concvt(ifrom) - dq / vfrom
                if (dq > 0) then
                    amass2(substance_i, 5) = amass2(substance_i, 5) + dq
                else
                    amass2(substance_i, 4) = amass2(substance_i, 4) - dq
                endif
                if (btest(integration_id, 3)) then ! balances active
                    if (iqdmp(iq) > 0) then        ! balances to be updated
                        if (dq > 0.0) then
                            dmpq(substance_i, iqdmp(iq), 1) = dmpq(substance_i, iqdmp(iq), 1) + dq
                        else
                            dmpq(substance_i, iqdmp(iq), 2) = dmpq(substance_i, iqdmp(iq), 2) - dq
                        endif
                    endif
                endif
                cycle
            endif

            ! Boris and Book for the inner area
            vfrom = volnew(ifrom)
            vto = volnew(ito)
            if (vfrom > 1.0e-25 .and. vto > 1.0e-25) then
                s = sign (1.0, dq)
                if      (ifrom1 > 0) then
                    cfrm1 = concvt(ifrom1)
                else if (ifrom1 == 0) then
                    if (s > 0) then
                        cfrm1 = 0.0
                    else
                        cfrm1 = 2.0 * concvt(ifrom)
                    endif
                else if (ifrom1 < 0) then
                    cfrm1 = bound(substance_i, -ifrom1)
                endif
                if      (itopl1 > 0) then
                    ctop1 = concvt(itopl1)
                else if (itopl1 == 0) then
                    if (s > 0) then
                        ctop1 = 2.0 * concvt(ito)
                    else
                        ctop1 = 0.0
                    endif
                else if (itopl1 < 0) then
                    ctop1 = bound(substance_i, -itopl1)
                endif

                e1 = (concvt(ifrom) - cfrm1) * vfrom
                e3 = (ctop1 - concvt(ito)) * vto
                dq = s * max(0.0, min(s * e1, s * dq, s * e3))

                concvt(ifrom) = concvt(ifrom) - dq / vfrom
                concvt(ito) = concvt(ito) + dq / vto

                if (btest(integration_id, 3)) then ! balances active
                    if (iqdmp(iq) > 0) then        ! balances to be updated
                        if (dq > 0.0) then
                            dmpq(substance_i, iqdmp(iq), 1) = dmpq(substance_i, iqdmp(iq), 1) + dq
                        else
                            dmpq(substance_i, iqdmp(iq), 2) = dmpq(substance_i, iqdmp(iq), 2) - dq
                        endif
                    endif
                endif
            endif
        end do

        do cell_i = 1, num_cells
            if (btest(iknmrk(cell_i), 0)) then
                conc(substance_i, cell_i) = concvt(cell_i)
            else
                conc(substance_i, cell_i) = 0.0
            endif
        enddo

        if (timon) call timstop (ithandl)
    end subroutine apply_fct_boris_book_scheme_21_22

end module m_flux_corrected_transport_fct
