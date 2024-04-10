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

contains


    subroutine wq_processes_proces (notot, noseg, conc, volume, time, &
            dts, deriv, ndmpar, nproc, noflux, &
            ipmsa, prvnio, promnr, iflux, increm, &
            flux, flxdmp, stochi, ibflag, ipbloo, &
            ioffbl, amass, nosys, isfact, itfact, &
            iexpnt, iknmrk, noq1, noq2, noq3, &
            noq4, area, ndspn, idpnew, dispnw, &
            ndspx, dspx, dsto, nveln, ivpnew, &
            velonw, nvelx, velx, vsto, isdmp, &
            defaul, prondt, prvvar, prvtyp, vararr, &
            varidx, arrpoi, arrknd, arrdm1, arrdm2, &
            novar, a, ndmps, pronam, prvpnt, &
            nodef, surfac, flux_int)

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

        integer(kind = int_wp), intent(in) :: notot                        !< Total number of substances
        integer(kind = int_wp), intent(in) :: noseg                        !< Nr. of computational volumes
        integer(kind = int_wp), intent(in) :: nodef                        !< Number of values in the deafult array
        integer(kind = int_wp), intent(in) :: novar                        !<
        real(kind = real_wp), intent(inout) :: conc  (notot, noseg)          !< Model concentrations
        real(kind = dp), intent(in) :: volume(noseg)          !< Segment volumes
        real(kind = dp), intent(in) :: time                         !< Time in system clock units
        real(kind = dp), intent(in) :: dts                          !< Time step system clock units
        real(kind = dp), intent(inout) :: deriv (noseg, notot)          !< Model derivatives
        integer(kind = int_wp), intent(in) :: ndmpar                       !< Number of dump areas
        integer(kind = int_wp), intent(in) :: nproc                        !< Number of processes
        integer(kind = int_wp), intent(in) :: noflux                       !< Number of fluxes
        integer(kind = int_wp), intent(in) :: ipmsa (:)                    !< Direct pointer in DELWAQ arrays
        integer(kind = int_wp), intent(in) :: prvnio(nproc)                !< Nr. of state variables per proces
        integer(kind = int_wp), intent(in) :: promnr(nproc)                !< Proces module number per proces
        integer(kind = int_wp), intent(in) :: iflux (nproc)                !< Offset in flux array per process
        integer(kind = int_wp), intent(in) :: increm(:)                    !< Direct increment in DELWAQ arrays
        real(kind = real_wp) :: flux  (noflux, noseg)         !< Proces fluxes
        real(kind = dp), intent(inout) :: flxdmp(2, noflux, ndmps)         !< Fluxes at dump segments
        real(kind = real_wp), intent(in) :: stochi(notot, noflux)        !< Proces stochiometry
        integer(kind = int_wp), intent(in) :: ibflag                       !< if 1 then mass balance output
        integer(kind = int_wp), intent(in) :: ipbloo                       !< Number of Bloom module  (if >0)
        integer(kind = int_wp), intent(in) :: ioffbl                       !< Offset in IPMSA for Bloom
        real(kind = dp), intent(inout) :: amass (notot, noseg)          !< mass array to be updated
        integer(kind = int_wp), intent(in) :: nosys                        !< number of active substances
        integer(kind = int_wp), intent(in) :: isfact                       !< system clock in seconds
        integer(kind = int_wp), intent(in) :: itfact                       !< time scale factor processes
        integer(kind = int_wp), intent(in) :: iexpnt(:)                  !< Exchange pointer
        integer(kind = int_wp), intent(in) :: iknmrk(noseg)                !< Integration suboptions
        integer(kind = int_wp), intent(in) :: noq1                         !< Number of exchanges first direction
        integer(kind = int_wp), intent(in) :: noq2                         !< Number of exchanges second direction
        integer(kind = int_wp), intent(in) :: noq3                         !< Number of exchanges vertical
        integer(kind = int_wp), intent(in) :: noq4                         !< Number of exchanges in the bed
        real(kind = real_wp), intent(in) :: area  (*)                    !< exchange areas
        integer(kind = int_wp), intent(in) :: ndspn                        !< Number of new dispersion arrays
        integer(kind = int_wp), intent(in) :: idpnew(nosys)               !< Pointer to new disp array
        real(kind = real_wp), intent(inout) :: dispnw(ndspn, *)             !< New dispersion array
        integer(kind = int_wp), intent(in) :: ndspx                        !< Nr. of calculated dispersions
        real(kind = real_wp) :: dspx  (ndspx, *)             !< Calculated dispersions
        real(kind = real_wp), intent(in) :: dsto  (nosys, ndspx)          !< Factor for calc. dispersions
        integer(kind = int_wp), intent(in) :: nveln                        !< Nr. of new velocity array's
        integer(kind = int_wp), intent(in) :: ivpnew(nosys)               !< Pointer to new velo array
        real(kind = real_wp), intent(out) :: velonw(nveln, *)             !< New velocity array
        integer(kind = int_wp), intent(in) :: nvelx                        !< Nr. of calculated velocities
        real(kind = real_wp) :: velx  (nvelx, *)             !< Calculated velocities
        real(kind = real_wp), intent(in) :: vsto  (nosys, nvelx)          !< Factor for velocitie
        integer(kind = int_wp), intent(in) :: isdmp (noseg)                !< pointer dumped segments
        real(kind = real_wp), intent(inout) :: defaul(nodef)                !< Default proces parameters
        integer(kind = int_wp), intent(inout) :: prondt(nproc)                !<
        integer(kind = int_wp), intent(in) :: prvvar(*)                    !<
        integer(kind = int_wp), intent(in) :: prvtyp(*)                    !<
        integer(kind = int_wp), intent(in) :: vararr(novar)                !<
        integer(kind = int_wp), intent(in) :: varidx(novar)                !<
        integer(kind = int_wp), intent(in) :: arrpoi(78)                   !<
        integer(kind = int_wp), intent(in) :: arrknd(78)                   !<
        integer(kind = int_wp), intent(in) :: arrdm1(78)                   !<
        integer(kind = int_wp), intent(in) :: arrdm2(78)                   !<
        real(kind = real_wp), intent(in) :: a     (:)                    !<
        integer(kind = int_wp), intent(in) :: ndmps                        !<
        character(10) :: pronam(nproc)               !< Name of called module
        integer(kind = int_wp), intent(in) :: prvpnt(nproc)                !< entry in process io pointers (cummulative of prvnio)
        real(kind = real_wp), intent(in) :: surfac(noseg)                !< horizontal surface
        integer(kind = int_wp), intent(in) :: flux_int                     !< Switch for integration of process fluxes by Delwaq (or not)
        integer(kind = int_wp) :: lunrep                       !< Logical unit number of report-file

        !     Local declarations

        integer(kind = int_wp) :: ivar, iarr, iv_idx, ip_arr           !  help variables
        integer(kind = int_wp) :: ipndt, ndtblo                           !  help variables
        integer(kind = int_wp) :: nfluxp, ifracs, iproc    !  help variables
        real(kind = dp) :: dtspro     ! fractional step dts
        integer(kind = int_wp) :: ipp_dts     ! pointer in default array to process specific dts
        integer(kind = int_wp) :: ipp_delt    ! pointer in default array to process specific delt
        INTEGER(kind = int_wp) :: ISTEP, NOQ
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

        integer(4) ithndl /0/
        if (timon) call timstrt ("wq_processes_proces", ithndl)

        IFRACS = 1

        IF (nproc == 0) goto 9999

        !     Count calls of this module
        !
        istep = istep + 1
        !
        noq = noq1 + noq2 + noq3 + noq4

        !     BLOOM fractional step (derivs assumed zero at entry)

        if (ipbloo > 0) then         !     Check presence of BLOOM module for this run
            ivar = prvvar(ioffbl)
            iarr = vararr(ivar)
            iv_idx = varidx(ivar)
            ip_arr = arrpoi(iarr)
            ipndt = ip_arr + iv_idx - 1
            ndtblo = nint(a(ipndt))  ! This picks up TimMultBl from BLOOM (without checking the name!)
            prondt(ipbloo) = ndtblo

            !        This timestep fractional step ?

            if (mod(istep - 1, ndtblo) == 0) then
                flux = 0.0

                !           set dts and delt, bloom itself will multiply with prondt
                dtspro = prondt(ipbloo) * dts
                ipp_dts = nodef - 2 * nproc + ipbloo
                ipp_delt = nodef - nproc + ipbloo
                defaul(ipp_dts) = dts
                defaul(ipp_delt) = dts / float(itfact)

                call onepro_wqp (ipbloo, ioffbl, prvnio, prvtyp, prvvar, vararr, &
                        varidx, arrknd, arrpoi, arrdm1, arrdm2, &
                        noseg, a, ipmsa, increm, &
                        noflux, iflux, promnr, flux, iexpnt, &
                        iknmrk, noq1, noq2, noq3, noq4, &
                        pronam, dll_opb)

                if (ipbloo /= nproc) then
                    nfluxp = iflux(ipbloo + 1) - iflux(ipbloo)
                else
                    nfluxp = noflux - iflux(ipbloo) + 1
                endif
                if (nfluxp > 0) then
                    !              Construct derivatives for these fluxes on this grid
                    call wq_processes_derivatives (deriv, notot, noflux, stochi, iflux (ipbloo), &
                            nfluxp, flux, noseg, volume, prondt(ipbloo))

                    !              For balances store FLXDMP
                    if (ibflag > 0) then
                        ndt = prondt(ipbloo) * dts / 86400.0
                        do iseg = 1, noseg
                            if (isdmp(iseg) > 0) then
                                nflux1 = iflux (ipbloo)
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
                    do iseg = 1, noseg
                        deriv (iseg, :) = deriv(iseg, :) * atfac
                    enddo

                    if (flux_int == 1) then
                        !                 let WAQ integrate the process fluxes
                        call wq_processes_integrate_fluxes (conc, amass, deriv, volume, dts, &
                                nosys, notot, noseg, surfac)
                    endif
                endif
            endif
        endif

        !     The processes fractional step
        flux = 0.0

        do iproc = 1, nproc
            !        NOT bloom
            if (iproc /= ipbloo) then
                !           Check fractional step
                if (mod(istep - 1, prondt(iproc)) == 0) then

                    ! set dts and delt for this process in the default array
                    ipp_dts = nodef - 2 * nproc + iproc
                    ipp_delt = nodef - nproc + iproc
                    dtspro = prondt(iproc) * dts
                    defaul(ipp_dts) = dtspro
                    defaul(ipp_delt) = dtspro / float(itfact)

                    call onepro_wqp (iproc, prvpnt(iproc), prvnio, prvtyp, prvvar, vararr, &
                            varidx, arrknd, arrpoi, arrdm1, arrdm2, &
                            noseg, a, ipmsa, increm, &
                            noflux, iflux, promnr, flux, iexpnt, &
                            iknmrk, noq1, noq2, noq3, noq4, &
                            pronam, dll_opb)
                endif
            endif
        enddo

        !     Now update the derivatives and the dumps of the fluxes from
        !     all processes together outside of the parallel region
        call twopro_wqm (nproc, noflux, noseg, &
                notot, ndmps, dts, iflux, &
                volume, deriv, stochi, flux, &
                prondt, ibflag, isdmp, flxdmp, ipbloo, istep)

        !     Calculate new velocities
        if (flux_int == 2) then
            if (nveln  > 0 .and. flux_int /= 1) then
                call wq_processes_velocities (velonw, nveln, ivpnew, &
                        velx, nvelx, vsto, nosys, &
                        noq)
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
                do iseg = 1, noseg
                    deriv (iseg, :) = deriv(iseg, :) * atfac
                enddo

                if (flux_int == 1) then
                    !              let WAQ integrate the process fluxes
                    if (nveln  > 0) then
                        !                 Add effect of additional flow velocities
                        call wq_processes_integrate_velocities (nosys, notot, noseg, noq, nveln, &
                                velx, area, volume, iexpnt, iknmrk, &
                                ivpnew, conc, dts, deriv)
                    end if

                    !              Integration (derivs are zeroed)
                    call wq_processes_integrate_fluxes (conc, amass, deriv, volume, dts, &
                            nosys, notot, noseg, surfac)
                endif
            endif
        endif

        9999 continue
        if (timon) call timstop(ithndl)
        return
    end

    subroutine onepro_wqp (iproc, k, prvnio, prvtyp, prvvar, vararr, &
            varidx, arrknd, arrpoi, arrdm1, arrdm2, noseg, &
            a, ipmsa, increm, noflux, iflux, promnr, &
            flux, iexpnt, iknmrk, noq1, noq2, noq3, &
            noq4, pronam, dll_opb)

        use timers
        use process_registration
        use processes_pointers, only : nogrid

        integer(kind = int_wp) :: iproc, k, noseg, noflux, noq1, noq2, noq3, noq4
        integer             prvnio(*), prvtyp(*), &
                prvvar(*), vararr(*), &
                varidx(*), arrknd(*), &
                arrpoi(*), arrdm1(*), &
                arrdm2(*), &
                ipmsa (:), increm(:), &
                iflux (:), promnr(:), &
                iexpnt(:), iknmrk(:)
        real                a(:), flux(*)
        character*10        pronam(*)
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

        integer(4) ithndl /0/
        if (timon) call timstrt ("onepro_wqp", ithndl)

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
                ipmsa (k + ivario - 1) = ip_arr + iv_idx - 1
                increm(k + ivario - 1) = 0
            elseif (iarknd == 2) then
                ipmsa (k + ivario - 1) = ip_arr + iv_idx - 1
                increm(k + ivario - 1) = idim1
            elseif (iarknd == 3) then
                ipmsa (k + ivario - 1) = ip_arr + (iv_idx - 1) * idim1
                increm(k + ivario - 1) = 1
            else
                write(*, *) 'Processes: type = ', iarknd, ivario, ' - ', pronam(iproc)
            endif
            !
        enddo

        !     compute fluxes
        ipflux = iflux(iproc)
        call procal (a, promnr(iproc), flux(ipflux:(noflux*noseg*nogrid)), ipmsa(k:), increm(k:), &
                noseg, noflux, iexpnt, iknmrk(1:), noq1, &
                noq2, noq3, noq4, pronam(iproc), &
                iproc, dll_opb)

        if (timon) call timstop(ithndl)
        return
    end

    subroutine twopro_wqm (nproc, noflux, noseg, &
            notot, ndmps, dts, iflux, &
            volume, deriv, stochi, flux, &
            prondt, ibflag, isdmp, flxdmp, ipbloo, istep)

        use m_wq_processes_derivatives
        use timers

        implicit none

        !     Arguments           :

        !     Kind        Function         Name   Dimensions                 Description
        integer(kind = int_wp), intent(in) :: nproc                            ! Total number of processes
        integer(kind = int_wp), intent(in) :: noflux                           ! Total number of fluxes
        integer(kind = int_wp), intent(in) :: noseg                            ! Total number of computational volumes
        integer(kind = int_wp), intent(in) :: notot                            ! Total number of substances
        integer(kind = int_wp), intent(in) :: ndmps                            ! Total number of mass balance areas
        real(kind = dp), intent(in) :: dts
        integer(kind = int_wp), intent(in) :: iflux (nproc)                   ! Offset in the flux array per process
        real(kind = dp), intent(in) :: volume(noseg)                   ! Computational volumes
        real(kind = dp), intent(inout) :: deriv (noseg, notot)           ! Array with derivatives
        real(kind = real_wp), intent(in) :: stochi(notot, noflux)          ! Stoichiometric factors per flux
        real(kind = real_wp), intent(in) :: flux  (noflux, noseg)           ! Process fluxes
        integer(kind = int_wp), intent(in) :: prondt(nproc)                   ! Time step size of the process
        integer(kind = int_wp), intent(in) :: ibflag                           ! If > 0 then balances are required
        integer(kind = int_wp), intent(in) :: isdmp (noseg)                   ! Segment to dumped segment pointer
        real(kind = dp), intent(inout) :: flxdmp(2, noflux, ndmps)        ! Dumped fluxes
        integer(kind = int_wp), intent(in) :: ipbloo                           ! The BLOOM  process if any
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

        integer(4) ithndl /0/
        if (timon) call timstrt ("twopro_wqm", ithndl)

        do iproc = 1, nproc
            if (iproc == ipbloo) cycle
            if (mod(istep - 1, prondt(iproc)) /= 0) cycle

            !        See if this process produces fluxes
            if (iproc /= nproc) then
                nfluxp = iflux(iproc + 1) - iflux(iproc)
            else
                nfluxp = noflux - iflux(iproc) + 1
            endif
            if (nfluxp == 0) cycle

            !        Construct derivatives from these fluxes on this grid
            call wq_processes_derivatives(deriv, notot, noflux, stochi, iflux (iproc), &
                    nfluxp, flux, noseg, volume, prondt(iproc))

            !        For the use in balances, store fluxes in 'flxdmp' using aggregation pointer 'isdmp'
            ndt = prondt(iproc) * real(dts) / 86400.0
            if (ibflag > 0) then
                do iseg = 1, noseg
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
