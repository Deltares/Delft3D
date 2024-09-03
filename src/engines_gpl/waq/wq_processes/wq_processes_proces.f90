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
module m_wq_processes_proces
    use m_waq_precision
    use m_wq_processes_velocities

    implicit none

    private
    public :: wq_processes_proces
contains


    subroutine wq_processes_proces (num_substances_total, num_cells, conc, volume, time, &
            dts, deriv, ndmpar, num_processes_activated, noflux, &
            process_space_int, prvnio, promnr, iflux, increm, &
            flux, flxdmp, stochi, ibflag, bloom_status_ind, &
            bloom_ind, amass, num_substances_transported, isfact, itfact, &
            iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, &
            num_exchanges_bottom_dir, area, num_dispersion_arrays_new, idpnew, dispnw, &
            num_dispersion_arrays_extra, dspx, dsto, num_velocity_arrays_new, ivpnew, &
            velonw, num_velocity_arrays_extra, velx, vsto, isdmp, &
            defaul, prondt, prvvar, prvtyp, vararr, &
            varidx, arrpoi, arrknd, arrdm1, arrdm2, &
            num_vars, a, num_monitoring_cells, pronam, prvpnt, &
            num_defaults, surfac, flux_int)

        !     Deltares Software Centre

        !>\File
        !>         This subroutine procesfm is a 'light' variant of PROCES, intended to be called from FM
        !>
        !>         Control routine of PROCES system. Process sub-system of DELWAQ waterquality modelling system.

        use m_wq_processes_integrate_velocities
        use m_wq_processes_integrate_fluxes
        use m_wq_processes_derivatives
        use processes_pointers, only : dll_opb
        use process_registration
        use timers

        implicit none


        !     Arguments           :

        !     Kind         Function         Name                          Description

        integer(kind = int_wp), intent(in) :: num_substances_total                        !< Total number of substances
        integer(kind = int_wp), intent(in) :: num_cells                        !< Nr. of computational volumes
        integer(kind = int_wp), intent(in) :: num_defaults                        !< Number of values in the deafult array
        integer(kind = int_wp), intent(in) :: num_vars                        !<
        real(kind = real_wp), intent(inout) :: conc  (num_substances_total, num_cells)          !< Model concentrations
        real(kind = dp), intent(in) :: volume(num_cells)          !< Segment volumes
        real(kind = dp), intent(in) :: time                         !< Time in system clock units
        real(kind = dp), intent(in) :: dts                          !< Time step system clock units
        real(kind = dp), intent(inout) :: deriv (num_cells, num_substances_total)          !< Model derivatives
        integer(kind = int_wp), intent(in) :: ndmpar                       !< Number of dump areas
        integer(kind = int_wp), intent(in) :: num_processes_activated                        !< Number of processes
        integer(kind = int_wp), intent(in) :: noflux                       !< Number of fluxes
        integer(kind = int_wp), intent(in) :: process_space_int (:)                    !< Direct pointer in DELWAQ arrays
        integer(kind = int_wp), intent(in) :: prvnio(num_processes_activated)                !< Nr. of state variables per proces
        integer(kind = int_wp), intent(in) :: promnr(num_processes_activated)                !< Proces module number per proces
        integer(kind = int_wp), intent(in) :: iflux (num_processes_activated)                !< Offset in flux array per process
        integer(kind = int_wp), intent(in) :: increm(:)                    !< Direct increment in DELWAQ arrays
        real(kind = real_wp) :: flux  (noflux, num_cells)         !< Proces fluxes
        real(kind = dp), intent(inout) :: flxdmp(2, noflux, num_monitoring_cells)         !< Fluxes at dump segments
        real(kind = real_wp), intent(in) :: stochi(num_substances_total, noflux)        !< Proces stochiometry
        integer(kind = int_wp), intent(in) :: ibflag                       !< if 1 then mass balance output
        integer(kind = int_wp), intent(in) :: bloom_status_ind                       !< Number of Bloom module  (if >0)
        integer(kind = int_wp), intent(in) :: bloom_ind                       !< Offset in process_space_int for Bloom
        real(kind = dp), intent(inout) :: amass (num_substances_total, num_cells)          !< mass array to be updated
        integer(kind = int_wp), intent(in) :: num_substances_transported                        !< number of active substances
        integer(kind = int_wp), intent(in) :: isfact                       !< system clock in seconds
        integer(kind = int_wp), intent(in) :: itfact                       !< time scale factor processes
        integer(kind = int_wp), intent(in) :: iexpnt(:)                  !< Exchange pointer
        integer(kind = int_wp), intent(in) :: iknmrk(num_cells)                !< Integration suboptions
        integer(kind = int_wp), intent(in) :: num_exchanges_u_dir                         !< Number of exchanges first direction
        integer(kind = int_wp), intent(in) :: num_exchanges_v_dir                         !< Number of exchanges second direction
        integer(kind = int_wp), intent(in) :: num_exchanges_z_dir                         !< Number of exchanges vertical
        integer(kind = int_wp), intent(in) :: num_exchanges_bottom_dir                         !< Number of exchanges in the bed
        real(kind = real_wp), intent(in) :: area  (*)                    !< exchange areas
        integer(kind = int_wp), intent(in) :: num_dispersion_arrays_new
        integer(kind = int_wp), intent(in) :: idpnew(num_substances_transported)               !< Pointer to new disp array
        real(kind = real_wp), intent(inout) :: dispnw(num_dispersion_arrays_new, *)             !< New dispersion array
        integer(kind = int_wp), intent(in) :: num_dispersion_arrays_extra                        !< Nr. of calculated dispersions
        real(kind = real_wp) :: dspx  (num_dispersion_arrays_extra, *)             !< Calculated dispersions
        real(kind = real_wp), intent(in) :: dsto  (num_substances_transported, num_dispersion_arrays_extra)          !< Factor for calc. dispersions
        integer(kind = int_wp), intent(in) :: num_velocity_arrays_new
        integer(kind = int_wp), intent(in) :: ivpnew(num_substances_transported)               !< Pointer to new velo array
        real(kind = real_wp), intent(out) :: velonw(num_velocity_arrays_new, *)                    !< New velocity array
        integer(kind = int_wp), intent(in) :: num_velocity_arrays_extra          !< Nr. of calculated velocities
        real(kind = real_wp) :: velx  (num_velocity_arrays_extra, *)             !< Calculated velocities
        real(kind = real_wp), intent(in) :: vsto  (num_substances_transported, num_velocity_arrays_extra)          !< Factor for velocitie
        integer(kind = int_wp), intent(in) :: isdmp (num_cells)                !< pointer dumped segments
        real(kind = real_wp), intent(inout) :: defaul(num_defaults)                !< Default proces parameters
        integer(kind = int_wp), intent(inout) :: prondt(num_processes_activated)                !<
        integer(kind = int_wp), intent(in) :: prvvar(*)                    !<
        integer(kind = int_wp), intent(in) :: prvtyp(*)                    !<
        integer(kind = int_wp), intent(in) :: vararr(num_vars)                !<
        integer(kind = int_wp), intent(in) :: varidx(num_vars)                !<
        integer(kind = int_wp), intent(in) :: arrpoi(78)                   !<
        integer(kind = int_wp), intent(in) :: arrknd(78)                   !<
        integer(kind = int_wp), intent(in) :: arrdm1(78)                   !<
        integer(kind = int_wp), intent(in) :: arrdm2(78)                   !<
        real(kind = real_wp), intent(in) :: a     (:)                    !<
        integer(kind = int_wp), intent(in) :: num_monitoring_cells
        character(10) :: pronam(num_processes_activated)               !< Name of called module
        integer(kind = int_wp), intent(in) :: prvpnt(num_processes_activated)                !< entry in process io pointers (cummulative of prvnio)
        real(kind = real_wp), intent(in) :: surfac(num_cells)                !< horizontal surface
        integer(kind = int_wp), intent(in) :: flux_int                     !< Switch for integration of process fluxes by Delwaq (or not)
        integer(kind = int_wp) :: lunrep                       !< Logical unit number of report-file

        !     Local declarations

        integer(kind = int_wp) :: ivar, iarr, iv_idx, ip_arr           !  help variables
        integer(kind = int_wp) :: ipndt, ndtblo                           !  help variables
        integer(kind = int_wp) :: nfluxp, ifracs, iproc    !  help variables
        real(kind = dp) :: dtspro     ! fractional step dts
        integer(kind = int_wp) :: ipp_dts     ! pointer in default array to process specific dts
        integer(kind = int_wp) :: ipp_delt    ! pointer in default array to process specific delt
        INTEGER(kind = int_wp) :: ISTEP, num_exchanges
        integer(kind = int_wp) :: open_shared_library
        integer(kind = int_wp), save :: ifirst = 1
        logical :: lfound
        integer(kind = int_wp) :: idummy
        real(kind = real_wp) :: rdummy
        integer(kind = int_wp) :: ierror
        integer(kind = int_wp) :: ierr2
        logical :: l_stop
        integer(kind = int_wp) :: iflx                             ! Loop counter over fluxes
        integer(kind = int_wp) :: iseg                             ! Loop counter over segments
        integer(kind = int_wp) :: nflux1                           ! Help variable for fluxes
        integer(kind = int_wp) :: ips                              ! Help variable for dump segments
        real(kind = dp) :: vol                              ! Help variable volume
        real(kind = dp) :: ndt                              ! Help variable time step multiplier
        real(kind = dp) :: atfac                            ! Help variable

        save    istep
        data    istep  / 0 /

        integer(4) :: ithndl =  0
        
        if (timon) call timstrt ("wq_processes_proces", ithndl)

        IFRACS = 1

        IF (num_processes_activated == 0) goto 9999

        !     Count calls of this module
        !
        istep = istep + 1
        !
        num_exchanges = num_exchanges_u_dir + num_exchanges_v_dir + num_exchanges_z_dir + num_exchanges_bottom_dir

        !     BLOOM fractional step (derivs assumed zero at entry)

        if (bloom_status_ind > 0) then         !     Check presence of BLOOM module for this run
            ivar = prvvar(bloom_ind)
            iarr = vararr(ivar)
            iv_idx = varidx(ivar)
            ip_arr = arrpoi(iarr)
            ipndt = ip_arr + iv_idx - 1
            ndtblo = nint(a(ipndt))  ! This picks up TimMultBl from BLOOM (without checking the name!)
            prondt(bloom_status_ind) = ndtblo

            !        This timestep fractional step ?

            if (mod(istep - 1, ndtblo) == 0) then
                flux = 0.0

                !           set dts and delt, bloom itself will multiply with prondt
                dtspro = prondt(bloom_status_ind) * dts
                ipp_dts = num_defaults - 2 * num_processes_activated + bloom_status_ind
                ipp_delt = num_defaults - num_processes_activated + bloom_status_ind
                defaul(ipp_dts) = dts
                defaul(ipp_delt) = dts / real(itfact)

                call calculate_single_process (bloom_status_ind, bloom_ind, prvnio, prvtyp, prvvar, vararr, &
                        varidx, arrknd, arrpoi, arrdm1, arrdm2, &
                        num_cells, a, process_space_int, increm, &
                        noflux, iflux, promnr, flux, iexpnt, &
                        iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir, &
                        pronam, dll_opb)

                if (bloom_status_ind /= num_processes_activated) then
                    nfluxp = iflux(bloom_status_ind + 1) - iflux(bloom_status_ind)
                else
                    nfluxp = noflux - iflux(bloom_status_ind) + 1
                endif
                if (nfluxp > 0) then
                    !              Construct derivatives for these fluxes on this grid
                    call wq_processes_derivatives (deriv, num_substances_total, noflux, stochi, iflux (bloom_status_ind), &
                            nfluxp, flux, num_cells, volume, prondt(bloom_status_ind))

                    !              For balances store FLXDMP
                    if (ibflag > 0) then
                        ndt = prondt(bloom_status_ind) * dts / 86400.0
                        do iseg = 1, num_cells
                            if (isdmp(iseg) > 0) then
                                nflux1 = iflux (bloom_status_ind)
                                vol = volume(iseg)
                                ips = isdmp(iseg)
                                if(ips<1) cycle
                                do iflx = nflux1, nflux1 + nfluxp - 1
                                    if(flux(iflx, iseg)>0) then
                                        flxdmp(1, iflx, ips) = flxdmp(1, iflx, ips) + flux(iflx, iseg) * vol * ndt
                                    else
                                        flxdmp(2, iflx, ips) = flxdmp(2, iflx, ips) - flux(iflx, iseg) * vol * ndt
                                    endif
                                enddo
                            endif
                        enddo
                    endif

                endif

                if (istep == 1) then
                    deriv(:, :) = 0.0d0
                    if (ibflag > 0) flxdmp = 0.0d0
                else
                    !              Scale fluxes and update "processes" accumulation arrays
                    atfac = 1.0 / real(itfact, 8)
                    do iseg = 1, num_cells
                        deriv (iseg, :) = deriv(iseg, :) * atfac
                    enddo

                    if (flux_int == 1) then
                        !                 let WAQ integrate the process fluxes
                        call wq_processes_integrate_fluxes (conc, amass, deriv, volume, dts, &
                                num_substances_transported, num_substances_total, num_cells, surfac)
                    endif
                endif
            endif
        endif

        !     The processes fractional step
        flux = 0.0

        do iproc = 1, num_processes_activated
            !        NOT bloom
            if (iproc /= bloom_status_ind) then
                !           Check fractional step
                if (mod(istep - 1, prondt(iproc)) == 0) then

                    ! set dts and delt for this process in the default array
                    ipp_dts = num_defaults - 2 * num_processes_activated + iproc
                    ipp_delt = num_defaults - num_processes_activated + iproc
                    dtspro = prondt(iproc) * dts
                    defaul(ipp_dts) = dtspro
                    defaul(ipp_delt) = dtspro / real(itfact)

                    call calculate_single_process (iproc, prvpnt(iproc), prvnio, prvtyp, prvvar, vararr, &
                            varidx, arrknd, arrpoi, arrdm1, arrdm2, &
                            num_cells, a, process_space_int, increm, &
                            noflux, iflux, promnr, flux, iexpnt, &
                            iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir, &
                            pronam, dll_opb)
                endif
            endif
        enddo

        !     Now update the derivatives and the dumps of the fluxes from
        !     all processes together outside of the parallel region
        call update_derivaties_and_dump_fluxes (num_processes_activated, noflux, num_cells, &
                num_substances_total, num_monitoring_cells, dts, iflux, &
                volume, deriv, stochi, flux, &
                prondt, ibflag, isdmp, flxdmp, bloom_status_ind, istep)

        !     Calculate new velocities
        if (flux_int == 2) then
            if (num_velocity_arrays_new  > 0 .and. flux_int /= 1) then
                call wq_processes_velocities (velonw, num_velocity_arrays_new, ivpnew, &
                        velx, num_velocity_arrays_extra, vsto, num_substances_transported, &
                        num_exchanges)
            endif
        endif

        !     Set fractional step
        if (noflux > 0 .and. ifracs == 1) then

            ! no fluxes at first step of fractional step

            if (istep == 1) then
                deriv(:, :) = 0.0d0
                if (ibflag > 0) flxdmp = 0.0d0
            else

                !           Scale fluxes and update "processes" accumulation arrays
                atfac = 1.0 / real(itfact, 8)
                do iseg = 1, num_cells
                    deriv (iseg, :) = deriv(iseg, :) * atfac
                enddo

                if (flux_int == 1) then
                    !              let WAQ integrate the process fluxes
                    if (num_velocity_arrays_new  > 0) then
                        !                 Add effect of additional flow velocities
                        call wq_processes_integrate_velocities (num_substances_transported, num_substances_total, num_cells, num_exchanges, num_velocity_arrays_new, &
                                velx, area, volume, iexpnt, iknmrk, &
                                ivpnew, conc, dts, deriv)
                    end if

                    !              Integration (derivs are zeroed)
                    call wq_processes_integrate_fluxes (conc, amass, deriv, volume, dts, &
                            num_substances_transported, num_substances_total, num_cells, surfac)
                endif
            endif
        endif

        9999 continue
        if (timon) call timstop(ithndl)
        return
    end

    subroutine calculate_single_process (iproc, k, prvnio, prvtyp, prvvar, vararr, &
            varidx, arrknd, arrpoi, arrdm1, arrdm2, num_cells, &
            a, process_space_int, increm, noflux, iflux, promnr, &
            flux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, &
            num_exchanges_bottom_dir, pronam, dll_opb)

        use timers
        use process_registration
        use processes_pointers, only : num_grids

        integer(kind = int_wp) :: iproc, k, num_cells, noflux, num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir
        integer             prvnio(*), prvtyp(*), &
                prvvar(*), vararr(*), &
                varidx(*), arrknd(*), &
                arrpoi(*), arrdm1(*), &
                arrdm2(*), &
                process_space_int (:), increm(:), &
                iflux (:), promnr(:), &
                iexpnt(:), iknmrk(:)
        real                a(:), flux(*)
        character(len=10)        pronam(*)
        integer(c_intptr_t), intent(in) :: dll_opb     ! open proces library dll handle
        !
        !     Local
        !
        integer(kind = int_wp) :: ityp
        integer(kind = int_wp) :: ivario
        integer(kind = int_wp) :: ivar
        integer(kind = int_wp) :: iarr
        integer(kind = int_wp) :: iv_idx
        integer(kind = int_wp) :: iarknd
        integer(kind = int_wp) :: ip_arr
        integer(kind = int_wp) :: idim1
        integer(kind = int_wp) :: idim2
        integer(kind = int_wp) :: ipflux

        integer(4) :: ithndl =  0
        
        if (timon) call timstrt ("calculate_single_process", ithndl)

        !     Set the variables
        do ivario = 1, prvnio(iproc)
            ityp = prvtyp(k + ivario - 1)
            ivar = prvvar(k + ivario - 1)
            iarr = vararr(ivar)
            iv_idx = varidx(ivar)
            iarknd = arrknd(iarr)
            ip_arr = arrpoi(iarr)
            idim1 = arrdm1(iarr)
            idim2 = arrdm2(iarr)

            !        Set pointer structure
            if (iarknd == 1) then
                process_space_int (k + ivario - 1) = ip_arr + iv_idx - 1
                increm(k + ivario - 1) = 0
            elseif (iarknd == 2) then
                process_space_int (k + ivario - 1) = ip_arr + iv_idx - 1
                increm(k + ivario - 1) = idim1
            elseif (iarknd == 3) then
                process_space_int (k + ivario - 1) = ip_arr + (iv_idx - 1) * idim1
                increm(k + ivario - 1) = 1
            else
                write(*, *) 'Processes: type = ', iarknd, ivario, ' - ', pronam(iproc)
            endif
            !
        enddo

        !     compute fluxes
        ipflux = iflux(iproc)
        call procal (a, promnr(iproc), flux(ipflux:(noflux*num_cells*num_grids)), process_space_int(k:), increm(k:), &
                num_cells, noflux, iexpnt, iknmrk(1:), num_exchanges_u_dir, &
                num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir, pronam(iproc), &
                iproc, dll_opb)

        if (timon) call timstop(ithndl)
        return
    end

    subroutine update_derivaties_and_dump_fluxes (num_processes_activated, noflux, num_cells, &
            num_substances_total, num_monitoring_cells, dts, iflux, &
            volume, deriv, stochi, flux, &
            prondt, ibflag, isdmp, flxdmp, bloom_status_ind, istep)

        use m_wq_processes_derivatives
        use timers

        implicit none

        !     Arguments           :

        !     Kind        Function         Name   Dimensions                 Description
        integer(kind = int_wp), intent(in) :: num_processes_activated                            ! Total number of processes
        integer(kind = int_wp), intent(in) :: noflux                           ! Total number of fluxes
        integer(kind = int_wp), intent(in) :: num_cells
        integer(kind = int_wp), intent(in) :: num_substances_total
        integer(kind = int_wp), intent(in) :: num_monitoring_cells                 ! Total number of mass balance areas
        real(kind = dp), intent(in) :: dts
        integer(kind = int_wp), intent(in) :: iflux (num_processes_activated)                   ! Offset in the flux array per process
        real(kind = dp), intent(in) :: volume(num_cells)                   ! Computational volumes
        real(kind = dp), intent(inout) :: deriv (num_cells, num_substances_total)           ! Array with derivatives
        real(kind = real_wp), intent(in) :: stochi(num_substances_total, noflux)          ! Stoichiometric factors per flux
        real(kind = real_wp), intent(in) :: flux  (noflux, num_cells)           ! Process fluxes
        integer(kind = int_wp), intent(in) :: prondt(num_processes_activated)                   ! Time step size of the process
        integer(kind = int_wp), intent(in) :: ibflag                           ! If > 0 then balances are required
        integer(kind = int_wp), intent(in) :: isdmp (num_cells)                   ! Segment to dumped segment pointer
        real(kind = dp), intent(inout) :: flxdmp(2, noflux, num_monitoring_cells)        ! Dumped fluxes
        integer(kind = int_wp), intent(in) :: bloom_status_ind                           ! The BLOOM  process if any
        integer(kind = int_wp), intent(in) :: istep                            ! Time step nr.

        !     Local
        integer(kind = int_wp) :: iproc                            ! Loop counter over processes
        integer(kind = int_wp) :: iflx                             ! Loop counter over fluxes
        integer(kind = int_wp) :: iseg                             ! Loop counter over segments
        integer(kind = int_wp) :: nflux1                           ! Help variable for fluxes
        integer(kind = int_wp) :: ips                              ! Help variable for dump segments
        integer(kind = int_wp) :: nfluxp                           ! Number of fluxes in this process
        real(kind = dp) :: vol                              ! Help variable volume
        real(kind = dp) :: ndt                              ! Help variable time step multiplier

        integer(4) :: ithndl =  0
        
        if (timon) call timstrt ("update_derivaties_and_dump_fluxes", ithndl)

        do iproc = 1, num_processes_activated
            if (iproc == bloom_status_ind) cycle
            if (mod(istep - 1, prondt(iproc)) /= 0) cycle

            !        See if this process produces fluxes
            if (iproc /= num_processes_activated) then
                nfluxp = iflux(iproc + 1) - iflux(iproc)
            else
                nfluxp = noflux - iflux(iproc) + 1
            endif
            if (nfluxp == 0) cycle

            !        Construct derivatives from these fluxes on this grid
            call wq_processes_derivatives(deriv, num_substances_total, noflux, stochi, iflux (iproc), &
                    nfluxp, flux, num_cells, volume, prondt(iproc))

            !        For the use in balances, store fluxes in 'flxdmp' using aggregation pointer 'isdmp'
            ndt = prondt(iproc) * real(dts) / 86400.0
            if (ibflag > 0) then
                do iseg = 1, num_cells
                    if (isdmp(iseg) > 0) then
                        nflux1 = iflux (iproc)
                        vol = volume(iseg)
                        ips = isdmp(iseg)
                        if(ips<1) cycle
                        do iflx = nflux1, nflux1 + nfluxp - 1
                            if(flux(iflx, iseg)>0) then
                                flxdmp(1, iflx, ips) = flxdmp(1, iflx, ips) + flux(iflx, iseg) * vol * ndt
                            else
                                flxdmp(2, iflx, ips) = flxdmp(2, iflx, ips) - flux(iflx, iseg) * vol * ndt
                            endif
                        enddo
                    endif
                enddo
            endif

        enddo

        if (timon) call timstop(ithndl)
        return
    end

end module m_wq_processes_proces
