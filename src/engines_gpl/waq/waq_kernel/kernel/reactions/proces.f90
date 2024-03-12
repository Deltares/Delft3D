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
module m_proces
    use m_waq_precision
    use m_provel
    use m_proint
    use m_profld
    use m_prodr2

    implicit none

contains


    subroutine proces (notot, noseg, conc, volume, itime, &
            idt, deriv, ndmpar, nproc, noflux, &
            ipmsa, prvnio, promnr, iflux, increm, &
            flux, flxdmp, stochi, ibflag, ipbloo, &
            ioffbl, amass, nosys, &
            itfact, amass2, iaflag, intopt, flxint, &
            iexpnt, iknmrk, noq1, noq2, noq3, &
            noq4, ndspn, idpnew, dispnw, nodisp, &
            idpnt, disper, ndspx, dspx, dsto, &
            nveln, ivpnew, velonw, novelo, ivpnt, &
            velo, nvelx, velx, vsto, dmps, &
            isdmp, ipdmp, ntdmpq, defaul, prondt, &
            progrd, prvvar, prvtyp, vararr, varidx, &
            vartda, vardag, vartag, varagg, arrpoi, &
            arrknd, arrdm1, arrdm2, vgrset, grdnos, &
            grdseg, novar, a, nogrid, ndmps, &
            pronam, intsrt, &
            prvpnt, done, nrref, proref, nodef, &
            surfac, lunrep)

        !     Deltares Software Centre

        !>\File
        !>         Control routine of PROCES system. Process sub-system of DELWAQ waterquality modelling system.
        !>
        !>         Routine deals with:
        !>         - processes that act on different spatial grids (important application is layered bed)
        !>         - processes that act with coarser time steps (notably the Bloom algal growth model and the
        !>         - Paralellism of the different processes on shared memory multi core machines.

        !     Created:            : november 1992 by Jos van Gils and Jan van Beek

        !     Subroutines called  : PROCAL, Compute fluxes with call to a module
        !                           PRODER, Make derivatives, store fluxes
        !                           resample_v2, De-aggregation of a variable
        !                           DLWQ14, set deriv array
        !                           DLWQP0, set a step
        !                           PROINT, integrate fluxes at dump segments
        !                           PROVEL, calculate new velocities/dispersions
        !                           aggregate_extended, aggrgation of a variable
        !                           GETMLU, get unit number monitor file
        !                           SRSTOP, stops execution with an error indication

        !     Files               : Monitoring file if needed for messages

        use m_dlwqp0
        use m_dlwq14
        use m_srstop
        use m_monsys
        use m_cli_utils, only : retrieve_command_argument
        use aggregation, only : aggregate, aggregate_extended, resample, aggregate_attributes
        use m_dhgvar
        use m_array_manipulation, only : set_array_parameters
        use timers
        use process_registration

        implicit none

        !$    include "omp_lib.h"

        !     Arguments           :

        !     Kind         Function         Name                          Description

        integer(kind = int_wp), intent(in) :: nogrid                      !< Number of computational grids
        integer(kind = int_wp), intent(in) :: notot                       !< Total number of substances
        integer(kind = int_wp), intent(in) :: noseg                       !< Nr. of computational volumes
        integer(kind = int_wp), intent(in) :: nodef                       !< Number of values in the deafult array
        integer(kind = int_wp), intent(in) :: novar                       !<
        real(kind = real_wp), intent(inout) :: conc  (notot, noseg, nogrid)  !< Model concentrations
        real(kind = real_wp), intent(inout) :: volume(noseg, nogrid)  !< Segment volumes
        integer(kind = int_wp), intent(in) :: itime                       !< Time in system clock units
        integer(kind = int_wp), intent(in) :: idt                         !< Time step system clock units
        real(kind = real_wp), intent(out) :: deriv (notot, noseg, nogrid)  !< Model derivatives
        integer(kind = int_wp), intent(in) :: ndmpar                      !< Number of dump areas
        integer(kind = int_wp), intent(in) :: nproc                       !< Number of processes
        integer(kind = int_wp), intent(in) :: noflux                      !< Number of fluxes
        integer(kind = int_wp), intent(in) :: ipmsa (*)                   !< Direct pointer in DELWAQ arrays
        integer(kind = int_wp), intent(in) :: prvnio(nproc)               !< Nr. of state variables per proces
        integer(kind = int_wp), intent(in) :: promnr(nproc)               !< Proces module number per proces
        integer(kind = int_wp), intent(in) :: iflux (nproc)               !< Offset in flux array per process
        integer(kind = int_wp), intent(in) :: increm(*)                   !< Direct increment in DELWAQ arrays
        real(kind = real_wp) :: flux  (noflux, noseg, nogrid) !< Proces fluxes
        real(kind = real_wp) :: flxdmp(ndmps, noflux)       !< Fluxes at dump segments
        real(kind = real_wp), intent(in) :: stochi(notot, noflux)       !< Proces stochiometry
        integer(kind = int_wp), intent(in) :: ibflag                      !< if 1 then mass balance output
        integer(kind = int_wp), intent(in) :: ipbloo                      !< Number of Bloom module  (if >0)
        integer(kind = int_wp), intent(in) :: ioffbl                      !< Offset in IPMSA for Bloom
        real(kind = real_wp), intent(inout) :: amass (notot, noseg, nogrid)  !< mass array to be updated
        integer(kind = int_wp), intent(in) :: nosys                       !< number of active substances
        integer(kind = int_wp), intent(in) :: itfact                      !< time scale factor processes
        real(kind = real_wp), intent(inout) :: amass2(notot, 5)         !< mass balance array
        integer(kind = int_wp), intent(in) :: iaflag                      !< if 1 then accumulation
        integer(kind = int_wp), intent(in) :: intopt                      !< Integration suboptions
        real(kind = real_wp), intent(inout) :: flxint(ndmpar, noflux)       !< Integrated fluxes at dump areas
        integer(kind = int_wp), intent(in) :: iexpnt(4, *)                 !< Exchange pointer
        integer(kind = int_wp), intent(inout) :: iknmrk(noseg, nogrid)        !< Integration suboptions
        integer(kind = int_wp), intent(in) :: noq1                        !< Number of exchanges first direction
        integer(kind = int_wp), intent(in) :: noq2                        !< Number of exchanges second direction
        integer(kind = int_wp), intent(in) :: noq3                        !< Number of exchanges vertical
        integer(kind = int_wp), intent(in) :: noq4                        !< Number of exchanges in the bed
        integer(kind = int_wp), intent(in) :: ndspn                       !< Number of new dispersion arrays
        integer(kind = int_wp), intent(in) :: idpnew(nosys)              !< Pointer to new disp array
        real(kind = real_wp), intent(out) :: dispnw(ndspn, *)            !< New dispersion array
        integer(kind = int_wp), intent(in) :: nodisp                      !< Nr. of original dispersions
        integer(kind = int_wp), intent(in) :: idpnt (nosys)              !< Pointer to original dispersion
        real(kind = real_wp), intent(in) :: disper(nodisp, *)            !< Original dispersions
        integer(kind = int_wp), intent(in) :: ndspx                       !< Nr. of calculated dispersions
        real(kind = real_wp) :: dspx  (ndspx, *)            !< Calculated dispersions
        real(kind = real_wp), intent(in) :: dsto  (nosys, ndspx)         !< Factor for calc. dispersions
        integer(kind = int_wp), intent(in) :: nveln                       !< Nr. of new velocity array's
        integer(kind = int_wp), intent(in) :: ivpnew(nosys)              !< Pointer to new velo array
        real(kind = real_wp), intent(out) :: velonw(nveln, *)            !< New velocity array
        integer(kind = int_wp), intent(in) :: novelo                      !< Nr. of original velocities
        integer(kind = int_wp), intent(in) :: ivpnt (nosys)              !< pointer to original velo
        real(kind = real_wp), intent(in) :: velo  (novelo, *)            !< Original velocities
        integer(kind = int_wp), intent(in) :: nvelx                       !< Nr. of calculated velocities
        real(kind = real_wp) :: velx  (nvelx, *)            !< Calculated velocities
        real(kind = real_wp), intent(in) :: vsto  (nosys, nvelx)         !< Factor for velocitie
        real(kind = real_wp), intent(inout) :: dmps  (notot, ndmps)         !< dumped segment fluxes
        integer(kind = int_wp), intent(in) :: isdmp (noseg)               !< pointer dumped segments
        integer(kind = int_wp), intent(in) :: ipdmp (*)                   !< pointer structure dump area's
        integer(kind = int_wp), intent(in) :: ntdmpq                      !< total number exchanges in dump area
        real(kind = real_wp), intent(inout) :: defaul(nodef)               !< Default proces parameters
        integer(kind = int_wp), intent(inout) :: prondt(nproc)               !<
        integer(kind = int_wp), intent(in) :: progrd(nproc)               !< Grid per process
        integer(kind = int_wp), intent(in) :: prvvar(*)                   !<
        integer(kind = int_wp), intent(in) :: prvtyp(*)                   !<
        integer(kind = int_wp), intent(in) :: vararr(novar)               !<
        integer(kind = int_wp), intent(in) :: varidx(novar)               !<
        integer(kind = int_wp), intent(in) :: vartda(novar)               !<
        integer(kind = int_wp), intent(in) :: vardag(novar)               !<
        integer(kind = int_wp), intent(in) :: vartag(novar)               !<
        integer(kind = int_wp), intent(in) :: varagg(novar)               !<
        integer(kind = int_wp), intent(in) :: arrpoi(*)                   !<
        integer(kind = int_wp), intent(in) :: arrknd(*)                   !<
        integer(kind = int_wp), intent(in) :: arrdm1(*)                   !<
        integer(kind = int_wp), intent(in) :: arrdm2(*)                   !<
        integer(kind = int_wp) :: vgrset(novar, nogrid)        !< Local flag for variables and grid
        integer(kind = int_wp), intent(in) :: grdnos(nogrid)              !< Number of segments per grid
        integer(kind = int_wp), intent(in) :: grdseg(noseg, nogrid)        !< Aggregation pointer per grid
        real(kind = real_wp), intent(in) :: a     (*)                   !<
        integer(kind = int_wp), intent(in) :: ndmps                       !<
        character(20) :: pronam(*)                   !< Name of called module
        integer(kind = int_wp), intent(in) :: intsrt                      !< Number of integration routine used
        integer(kind = int_wp), intent(in) :: prvpnt(nproc)               !< entry in process pointers OMP
        integer(kind = int_wp) :: done  (nproc)               !< flag whether a process has ran
        integer(kind = int_wp), intent(in) :: nrref                       !< maximum nr of back references
        integer(kind = int_wp), intent(in) :: proref(nrref, nproc)         !< the back references
        real(kind = real_wp), intent(in) :: surfac(noseg)               !< horizontal surface
        integer(kind = int_wp), intent(in) :: lunrep                      !< Logical unit number of report-file

        !     Local declarations

        integer(kind = int_wp) :: maxgrid    ! Highest grid number in progrd array
        integer(kind = int_wp) :: iiknmr     ! Pointer somewhere into the array tree
        integer(kind = int_wp) :: ix_hlp, ia_hlp, iv_hlp, ik_hlp, ip_hlp, & !  array pointers
                id1hlp, id2hlp                          !
        integer(kind = int_wp) :: ivar, iarr, iv_idx, ip_arr          !  help variables
        integer(kind = int_wp) :: ix_cnc, ia_cnc, iv_cnc, ip_arh          !  help variables
        integer(kind = int_wp) :: ipndt, ndtblo, igrblo, ndtcha, igrcha  !  help variables
        integer(kind = int_wp) :: isys, igrid, isysh, nototh, igrd    !  help variables
        integer(kind = int_wp) :: noseg2, nfluxp, iswcum, ifracs, iproc   !  help variables
        integer(kind = int_wp) :: k
        integer(kind = int_wp) :: actually_done
        integer(kind = int_wp) :: idtpro    ! fractional step idt
        integer(kind = int_wp) :: ipp_idt    ! pointer in default array to process specific idt
        integer(kind = int_wp) :: ipp_delt   ! pointer in default array to process specific delt
        INTEGER(kind = int_wp) :: ISTEP, NOQ, IERR
        integer(kind = int_wp), allocatable, save :: velndt(:) ! fractional step per velocity
        integer(kind = int_wp), allocatable, save :: dspndt(:) ! fractional step per dispersion
        integer(kind = int_wp) :: open_shared_library
        integer(kind = int_wp) :: perf_function
        integer(kind = int_wp), save :: ifirst = 1
        integer(c_intptr_t), save :: dll_opb     ! open proces library dll handle
        character(len = 256) :: shared_dll
        logical :: lfound
        integer(kind = int_wp) :: idummy
        real(kind = real_wp) :: rdummy
        integer(kind = int_wp) :: ierror
        integer(kind = int_wp) :: ierr2
        logical :: l_stop


        !     LOGICAL PROFLG
        SAVE    ISTEP
        DATA    ISTEP  / 0 /
        !     DATA    PROFLG / .TRUE. /
        !
        !jvb  Store fractional step flag in common CFRACS
        !
        COMMON /CFRACS/ IFRACS
        logical                 run              ! lp for OMP
        integer(kind = int_wp) :: aproc            ! lp for OMP

        logical timon_old
        integer(kind = int_wp) :: ithandl = 0
        integer(kind = int_wp) :: ithand2 = 0
        if (timon) call timstrt ("proces", ithandl)
        !jvb
        !
        !     If no processes, get out of here
        !
        IF (NPROC == 0) goto 9999

        ! open openpb dll

        if (ifirst == 1) then
            call getmlu(lunrep)
            call retrieve_command_argument ('-openpb', 3, lfound, idummy, rdummy, shared_dll, ierr2)
            if (lfound) then
                if (ierr2== 0) then
                    write(lunrep, *) ' -openpb command line argument found'
                    write(lunrep, *) ' using dll : ', trim(shared_dll)
                else
                    shared_dll = 'd3dwaq_openpb.dll'
                    write(lunrep, *) ' WARNING : -openpb command line argument without filename'
                    write(lunrep, *) ' using default dll : ', trim(shared_dll)
                endif
                l_stop = .true.
            else
                shared_dll = 'd3dwaq_openpb.dll'
                l_stop = .false.
                write(lunrep, *) ' using default dll : ', trim(shared_dll)
            endif
            dll_opb = 0 ! in C this one could be 4 or 8 bytes, so make sure the last bytes are zero
            ierror = open_shared_library(dll_opb, shared_dll)
            if (ierror /= 0 .and. l_stop) then
                write(*, *) 'ERROR : opening process library DLL'
                write(*, *) 'DLL   : ', trim(shared_dll)
                write(*, *) 'dll handle: ', dll_opb
                write(lunrep, *) 'ERROR : opening process library DLL'
                write(lunrep, *) 'DLL   : ', trim(shared_dll)
                write(lunrep, *) 'dll handle: ', dll_opb
                call srstop(1)
            endif
            ifirst = 0
        endif
        !
        !     Count calls of this module
        !
        ISTEP = ISTEP + 1
        done = 0                                           ! this zeros the whole array
        !
        !     Start timings
        !
        !     allocate velndt, dspndt
        !
        if (.not. allocated(velndt)) then
            allocate(velndt(nvelx))
            velndt = 1
        endif
        if (.not. allocated(dspndt)) then
            allocate(dspndt(ndspx))
            dspndt = 1
        endif

        !
        !JVB
        !     TEMPORARY HERE ?
        !
        !     aggregate kenmerk array
        !
        !grd  only if there is a process on a higher grid
        maxgrid = maxval(progrd(1:nproc))
        iiknmr = 78 + 30                 !   = iasize + 30
        if (maxgrid > 1) then
            call aggregate_attributes (noseg, arrdm2(iiknmr), nogrid, iknmrk, grdnos, &
                    grdseg)
        endif

        !     Get the general local work array, first index of LOCAL array

        ix_hlp = 1
        ia_hlp = 33
        call dhgvar(ia_hlp, ix_hlp, iv_hlp)
        ik_hlp = arrknd(ia_hlp)
        ip_hlp = arrpoi(ia_hlp)
        id1hlp = arrdm1(ia_hlp)
        id2hlp = arrdm2(ia_hlp)

        !     Fill some specific variables absolute in the real array

        defaul(2) = float(itime)
        noq = noq1 + noq2 + noq3 + noq4

        !     BLOOM fractional step (derivs assumed zero at entry)

        if (ipbloo > 0) then         !     Check presence of BLOOM module for this run
            ivar = prvvar(ioffbl)
            iarr = vararr(ivar)
            iv_idx = varidx(ivar)
            ip_arr = arrpoi(iarr)
            ipndt = ip_arr + iv_idx - 1
            ndtblo = nint(a(ipndt))
            prondt(ipbloo) = ndtblo

            !        This timestep fractional step ?

            if (mod(istep - 1, ndtblo) == 0) then

                !           Set CONC on the Bloom grid if that is not the first grid

                igrblo = progrd(ipbloo)
                if (igrblo > 1) then
                    noseg2 = grdnos(igrblo)
                    ix_cnc = 1
                    ia_cnc = 6
                    call dhgvar(ia_cnc, ix_cnc, iv_cnc)
                    call set_array_parameters(iv_hlp, ia_hlp, ik_hlp, ix_hlp, id1hlp, &
                            id2hlp, ip_hlp, igrblo, isysh, nototh, &
                            ip_arh)

                    !              actives and inactives if applicable

                    call aggregate(noseg, noseg2, notot, 1, nototh, &
                            notot, 1, 1, isysh, 1, &
                            nosys, grdseg(1, igrblo), 3, conc, volume, &
                            a(ip_arh), conc(1, 1, igrblo))
                    if (notot - nosys > 0)      & !   inactives
                            call aggregate(noseg, noseg2, notot, 1, nototh, &
                                    notot, nosys + 1, 1, isysh, nosys + 1, &
                                    notot - nosys, grdseg(1, igrblo), 3, conc, surfac, &
                                    a(ip_arh), conc(1, 1, igrblo))
                    do isys = 1, notot
                        ivar = iv_cnc + isys - 1
                        vgrset(ivar, igrblo) = 1
                    enddo
                endif

                flux = 0.0
                if (ibflag > 0) flxdmp = 0

                !           set idt and delt, bloom itself will multiply with prondt
                idtpro = prondt(ipbloo) * idt
                ipp_idt = nodef - 2 * nproc + ipbloo
                ipp_delt = nodef - nproc + ipbloo
                defaul(ipp_idt) = float(idt)
                defaul(ipp_delt) = float(idt) / float(itfact)
                if (timon) call timstrt ("onepro", ithand2)
                call onepro (ipbloo, ioffbl, idt, itfact, progrd, &
                        grdnos, prvnio, prvtyp, prvvar, vararr, &
                        varidx, arrknd, arrpoi, arrdm1, arrdm2, &
                        vgrset, nogrid, vartda, vardag, noseg, &
                        grdseg, a, varagg, ipmsa, increm, &
                        noflux, iflux, promnr, flux, iexpnt, &
                        iknmrk, noq1, noq2, noq3, noq4, &
                        nproc, notot, deriv, stochi, volume, &
                        prondt, ibflag, isdmp, flxdmp, novar, &
                        vartag, iiknmr, pronam, &
                        dspndt, velndt, dll_opb)
                done(ipbloo) = 1
                if (timon) call timstop (ithand2)
                igrid = progrd(ipbloo)
                noseg2 = grdnos(igrid)
                if (ipbloo /= nproc) then
                    nfluxp = iflux(ipbloo + 1) - iflux(ipbloo)
                else
                    nfluxp = noflux - iflux(ipbloo) + 1
                endif
                if (nfluxp > 0) then

                    !              If necessary set volume for this grid. Volume is always variable 1

                    if (vgrset(1, igrid) /= 1) then
                        call aggregate_extended(noseg, noseg2, 1, 1, 1, &
                                1, 1, 1, 1, 1, &
                                grdseg(1, igrid), 1, volume, volume, volume, &
                                volume(1, igrid))
                        vgrset(1, igrid) = 1
                    endif

                    !              Construct derivatives for these fluxes on this grid

                    call prodr2 (deriv(1, 1, igrid), notot, noflux, stochi, iflux (ipbloo), &
                            nfluxp, flux(1, 1, igrid), noseg2, volume(1, igrid), prondt(ipbloo))

                    !              For balances store FLXDMP

                    if (ibflag > 0) then
                        call profld (noflux, iflux (ipbloo), nfluxp, igrid, noseg2, &
                                noseg, prondt(ipbloo), isdmp, grdseg, flux(1, 1, igrid), &
                                volume, flxdmp)
                    endif
                endif

                !           If processes on other grid convert derivs to base grid

                igrblo = progrd(ipbloo)
                if (noflux > 0 .and. igrblo > 1) then
                    iswcum = 1
                    noseg2 = grdnos(igrblo)
                    call resample(noseg, noseg2, notot, notot, notot, &
                            notot, 1, 1, 1, 1, &
                            notot, grdseg(1, igrblo), 2, deriv(1, 1, igrblo), amass, &
                            iswcum, amass (1, 1, igrblo), deriv)
                    deriv(:, :, igrblo) = 0.0   !     Zero derivs higher grids
                endif

                !           Scale fluxes and update "processes" accumulation arrays

                call dlwq14 (deriv, notot, noseg, itfact, amass2, &
                        idt, iaflag, dmps, intopt, isdmp)

                !           Integration (derivs are zeroed)

                call dlwqp0 (conc, amass, deriv, volume, idt, &
                        nosys, notot, noseg, 0, 0, &
                        surfac)

                !           Integrate the fluxes at dump segments

                if (ibflag > 0) then
                    call proint (noflux, ndmpar, idt, itfact, flxdmp, &
                            flxint, isdmp, ipdmp, ntdmpq)
                    flxdmp = 0.0
                endif

                !           Set CONC not actual for higer grids

                ix_cnc = 1
                ia_cnc = 6
                call dhgvar(ia_cnc, ix_cnc, iv_cnc)
                do igrid = 2, nogrid
                    do isys = 1, notot
                        ivar = iv_cnc + isys - 1
                        vgrset(ivar, igrid) = 0
                    enddo
                enddo
            else
                done (ipbloo) = 1
            endif
        endif


        !     See if converting CONC in one step speeds up. Only in case of no fractional step

        if (ifracs == 0 .and. maxgrid > 1) then
            ix_cnc = 1
            ia_cnc = 6
            call dhgvar(ia_cnc, ix_cnc, iv_cnc)
            do igrid = 2, nogrid
                noseg2 = grdnos(igrid)
                call set_array_parameters(iv_hlp, ia_hlp, ik_hlp, ix_hlp, id1hlp, &
                        id2hlp, ip_hlp, igrid, isysh, nototh, &
                        ip_arh)

                !           actives and inactives if applicable

                call aggregate(noseg, noseg2, notot, 1, nototh, &
                        notot, 1, 1, isysh, 1, &
                        nosys, grdseg(1, igrid), 3, conc, volume, &
                        a(ip_arh), conc(1, 1, igrid))
                if (notot - nosys > 0)      & !   inactives
                        call aggregate(noseg, noseg2, notot, 1, nototh, &
                                notot, nosys + 1, 1, isysh, nosys + 1, &
                                notot - nosys, grdseg(1, igrid), 3, conc, surfac, &
                                a(ip_arh), conc(1, 1, igrid))
                do isys = 1, notot
                    ivar = iv_cnc + isys - 1
                    vgrset(ivar, igrid) = 1
                enddo
            enddo
        endif

        !     The processes fractional step

        flux = 0.0
        if (ibflag > 0) flxdmp = 0

        if (timon) call timstrt ("onepro", ithand2)
        timon_old = timon
        if (OMP_GET_MAX_THREADS() > 1) timon = .false.
        !$OMP PARALLEL
        !$OMP DO  PRIVATE(run,idtpro,k,nfluxp,ipp_idt,ipp_delt)  SCHEDULE(DYNAMIC)
        do iproc = 1, nproc

            !        NOT bloom

            if (iproc /= ipbloo) then

                !           Check fractional step

                if (mod(istep - 1, prondt(iproc)) == 0) then
                    run = .false.                             ! to get the loop running
                    do while (.not. run)                    ! wait untill all input is resolved
                        run = .true.                           ! we are optimistic
                        do k = 1, nrref                        ! maximum number of references / proc
                            if (proref(k, iproc) == 0) exit        ! no references left

                            !
                            ! Flush the array done:
                            ! The Intel Fortran compiler seems to optimise this
                            ! loop too aggressively under Linux, if we use the array done()
                            ! directly.
                            !

                            !$omp flush(done)

                            if (done(proref(k, iproc)) == 0) then  ! an unresolved one found
                                run = .false.                          ! so no run yet
                                exit
                            endif                                     ! everything is resolved
                        enddo                                        ! for this processs
                    enddo

                    ! set idt and delt for this process in the default array
                    ipp_idt = nodef - 2 * nproc + iproc
                    ipp_delt = nodef - nproc + iproc
                    IDTPRO = PRONDT(IPROC) * IDT
                    DEFAUL(ipp_idt) = FLOAT(IDTPRO)
                    DEFAUL(ipp_delt) = FLOAT(IDTPRO) / FLOAT(ITFACT)

                    call onepro (iproc, prvpnt(iproc), idt, itfact, progrd, &
                            grdnos, prvnio, prvtyp, prvvar, vararr, &
                            varidx, arrknd, arrpoi, arrdm1, arrdm2, &
                            vgrset, nogrid, vartda, vardag, noseg, &
                            grdseg, a, varagg, ipmsa, increm, &
                            noflux, iflux, promnr, flux, iexpnt, &
                            iknmrk, noq1, noq2, noq3, noq4, &
                            nproc, notot, deriv, stochi, volume, &
                            prondt, ibflag, isdmp, flxdmp, novar, &
                            vartag, iiknmr, pronam, &
                            dspndt, velndt, dll_opb)

                    done(iproc) = 1                           ! this process has resolved its output
                    !$omp flush(done)

                endif
            endif
        enddo
        !$OMP END DO
        !$OMP END PARALLEL

        timon = timon_old
        if (timon) call timstop (ithand2)

        !           Now update the derivatives and the dumps of the fluxes from
        !              all processes together outside of the parallel region

        call twopro (nproc, nogrid, noflux, novar, noseg, &
                notot, progrd, grdnos, iflux, vgrset, &
                grdseg, volume, deriv, stochi, flux, &
                prondt, ibflag, isdmp, flxdmp, &
                ipbloo, istep)

        !     Store fluxes and elaborate mass balances set fractional step
        !     Vraag , doen we nu altijd fractional step? of moeten we als we geen
        !     processen hebben met een grotere tijdstap de integratie samen met het
        !     transport doen.

        if (noflux > 0 .and. maxgrid > 1) then
            do igrd = 2, nogrid

                iswcum = 1
                noseg2 = grdnos(igrd)
                call resample(noseg, noseg2, notot, notot, notot, &
                        notot, 1, 1, 1, 1, &
                        notot, grdseg(1, igrd), 2, deriv(1, 1, igrd), amass, &
                        iswcum, amass (1, 1, igrd), deriv)
            enddo

            !        Zero derivs higher grids

            deriv(:, :, 2:nogrid) = 0.0
        endif

        !     Set fractional step

        if (noflux > 0 .and. ifracs == 1) then

            ! no fluxes at first step of fractional step

            if (istep == 1) then
                deriv(:, :, 1) = 0.0
                if (ibflag > 0) flxdmp = 0.0
            else

                !           Scale fluxes and update "processes" accumulation arrays

                call dlwq14 (deriv, notot, noseg, itfact, amass2, &
                        idt, iaflag, dmps, intopt, isdmp)

                !           Integration (derivs are zeroed)

                call dlwqp0 (conc, amass, deriv, volume, idt, &
                        nosys, notot, noseg, 0, 0, &
                        surfac)

                !           Integrate the fluxes at dump segments

                if (ibflag > 0) then
                    call proint (noflux, ndmpar, idt, itfact, flxdmp, &
                            flxint, isdmp, ipdmp, ntdmpq)
                    flxdmp = 0.0
                endif
            endif
        endif

        !     Calculate new dispersions

        if (ndspn  > 0) then
            call provel (dispnw, ndspn, idpnew, disper, nodisp, &
                    idpnt, dspx, ndspx, dsto, nosys, &
                    noq, dspndt, istep)
        endif

        !     Calculate new velocities

        if (nveln  > 0) then
            call provel (velonw, nveln, ivpnew, velo, novelo, &
                    ivpnt, velx, nvelx, vsto, nosys, &
                    noq, velndt, istep)
        endif

        9999 if (timon) call timstop (ithandl)
        return
    end

    SUBROUTINE ONEPRO (IPROC, K, IDT, ITFACT, PROGRD, &
            GRDNOS, PRVNIO, PRVTYP, PRVVAR, VARARR, &
            VARIDX, ARRKND, ARRPOI, ARRDM1, ARRDM2, &
            VGRSET, NOGRID, VARTDA, VARDAG, NOSEG, &
            GRDSEG, A, VARAGG, IPMSA, INCREM, &
            NOFLUX, IFLUX, PROMNR, FLUX, IEXPNT, &
            IKNMRK, NOQ1, NOQ2, NOQ3, NOQ4, &
            NPROC, NOTOT, DERIV, STOCHI, VOLUME, &
            PRONDT, IBFLAG, ISDMP, FLXDMP, NOVAR, &
            VARTAG, IIKNMR, PRONAM, &
            DSPNDT, VELNDT, dll_opb)
        !
        use timers
        use process_registration
        use aggregation, only : aggregate_extended, resample, resample_v2
        use m_array_manipulation, only : set_array_parameters
        use m_dhgvar
        !
        INTEGER(kind = int_wp) :: IPROC, K, IDT, ITFACT, NOGRID, &
                NOSEG, NOFLUX, NOQ1, NOQ2, NOQ3, &
                NOQ4, NPROC, NOTOT, IBFLAG, NOVAR, &
                IIKNMR
        INTEGER(kind = int_wp) :: PROGRD(*), GRDNOS(*), &
                PRVNIO(*), PRVTYP(*), &
                PRVVAR(*), VARARR(*), &
                VARIDX(*), ARRKND(*), &
                ARRPOI(*), ARRDM1(*), &
                ARRDM2(*), VGRSET(NOVAR, *), &
                VARTDA(*), VARDAG(*), &
                GRDSEG(NOSEG, *), VARAGG(*), &
                IPMSA (*), INCREM(*), &
                IFLUX (*), PROMNR(*), &
                IEXPNT(*), IKNMRK(*), &
                PRONDT(*), ISDMP (*), &
                VARTAG(*), &
                DSPNDT(*), VELNDT(*)
        REAL(kind = real_wp) :: A(*), FLUX(*), &
                DERIV(*), STOCHI(*), &
                VOLUME(*), FLXDMP(*)
        CHARACTER*10        PRONAM(*)
        integer(c_intptr_t), intent(in) :: dll_opb     ! open proces library dll handle
        !
        !     Local
        !
        INTEGER(kind = int_wp) :: IDTPRO, ITYP, IX_HLP, IA_HLP, IV_HLP, IK_HLP, IP_HLP, ID1HLP, ID2HLP
        integer(kind = int_wp) :: NOSEG2, IVARIO, IGRID, IGR3, NOSEG3, ISYSI, NOTOTI
        integer(kind = int_wp) :: IVAR, IARR, IV_IDX, IARKND, IP_ARR, IDIM1, IDIM2
        integer(kind = int_wp) :: IV_AG, IA_AG, IX_AG, IK_AG, IP_AG, ID1_AG, ID2_AG
        integer(kind = int_wp) :: IP_ARI, NOTOTO, ISYSO, IP_ARO, IDATYP, IV_DA, IA_DA, IK_DA
        integer(kind = int_wp) :: IX_DA, IP_DA, ID1_DA, ID2_DA, NOTOTW, ISYSW, IP_ARW
        integer(kind = int_wp) :: NOTOTH, ISYSH, IP_ARH, ISWCUM, IAGTYP, IPFLUX, IPKNMR, IGR2


        !
        !     get the general local work array, first index of LOCAL array
        !
        IX_HLP = 1
        IA_HLP = 33
        CALL DHGVAR(IA_HLP, IX_HLP, IV_HLP)
        IK_HLP = ARRKND(IA_HLP)
        IP_HLP = ARRPOI(IA_HLP)
        ID1HLP = ARRDM1(IA_HLP)
        ID2HLP = ARRDM2(IA_HLP)
        !
        !     Which grid
        !
        IGRID = PROGRD(IPROC)
        NOSEG2 = GRDNOS(IGRID)
        !
        !     Set the variable for this grid
        !

        DO IVARIO = 1, PRVNIO(IPROC)
            ITYP = PRVTYP(K + IVARIO - 1)
            IVAR = PRVVAR(K + IVARIO - 1)
            IARR = VARARR(IVAR)
            IV_IDX = VARIDX(IVAR)
            IARKND = ARRKND(IARR)
            IP_ARR = ARRPOI(IARR)
            IDIM1 = ARRDM1(IARR)
            IDIM2 = ARRDM2(IARR)
            IF (ITYP == 1) THEN
                !
                !           Only for space varying array's
                !
                IF (IARKND >= 2) THEN
                    !
                    !              Only if variable isn't actual set for this grid
                    !
                    IF (VGRSET(IVAR, IGRID) == 0) THEN
                        !
                        !                 Set variable for base grid
                        !
                        IF (VGRSET(IVAR, 1) == 0) THEN
                            DO IGR3 = 2, NOGRID
                                IF (VGRSET(IVAR, IGR3) == 1) THEN
                                    NOSEG3 = GRDNOS(IGR3)
                                    !
                                    !                          Determine characteristics of variable
                                    !
                                    CALL set_array_parameters(IVAR, IARR, &
                                            IARKND, IV_IDX, &
                                            IDIM1, IDIM2, &
                                            IP_ARR, IGR3, &
                                            ISYSI, NOTOTI, &
                                            IP_ARI)
                                    CALL set_array_parameters(IVAR, IARR, &
                                            IARKND, IV_IDX, &
                                            IDIM1, IDIM2, &
                                            IP_ARR, 1, &
                                            ISYSO, NOTOTO, &
                                            IP_ARO)
                                    !
                                    !                          Determine characteristics of WEIGHT variable
                                    !                          ( Don't mind if this one is actual ? )
                                    !
                                    IDATYP = VARTDA(IVAR)
                                    IF (IDATYP == 2) THEN
                                        IV_DA = VARDAG(IVAR)
                                        IA_DA = VARARR(IV_DA)
                                        IK_DA = ARRKND(IA_DA)
                                        IF (IK_DA == 1) THEN
                                            !
                                            !                                Not variable in space use help var
                                            !
                                            IDATYP = 3
                                            IV_DA = IV_HLP
                                            IA_DA = VARARR(IV_DA)
                                            IK_DA = ARRKND(IA_DA)
                                        ENDIF
                                        IX_DA = VARIDX(IV_DA)
                                        IP_DA = ARRPOI(IA_DA)
                                        ID1_DA = ARRDM1(IA_DA)
                                        ID2_DA = ARRDM2(IA_DA)
                                        CALL set_array_parameters(IV_DA, IA_DA, &
                                                IK_DA, IX_DA, &
                                                ID1_DA, ID2_DA, &
                                                IP_DA, 1, &
                                                ISYSW, NOTOTW, &
                                                IP_ARW)
                                        CALL set_array_parameters(IV_HLP, IA_HLP, &
                                                IK_HLP, IX_HLP, &
                                                ID1HLP, ID2HLP, &
                                                IP_HLP, IGR3, &
                                                ISYSH, NOTOTH, &
                                                IP_ARH)
                                    ELSEIF (IDATYP == 3) THEN
                                        IV_DA = IV_HLP
                                        IA_DA = VARARR(IV_DA)
                                        IK_DA = ARRKND(IA_DA)
                                        IX_DA = VARIDX(IV_DA)
                                        IP_DA = ARRPOI(IA_DA)
                                        ID1_DA = ARRDM1(IA_DA)
                                        ID2_DA = ARRDM2(IA_DA)
                                        CALL set_array_parameters(IV_DA, IA_DA, &
                                                IK_DA, IX_DA, &
                                                ID1_DA, ID2_DA, &
                                                IP_DA, 1, &
                                                ISYSW, NOTOTW, &
                                                IP_ARW)
                                        CALL set_array_parameters(IV_HLP, IA_HLP, &
                                                IK_HLP, IX_HLP, &
                                                ID1HLP, ID2HLP, &
                                                IP_HLP, IGR3, &
                                                ISYSH, NOTOTH, &
                                                IP_ARH)
                                    ELSE
                                        !
                                        !                             Weight and help array's dummy's
                                        !                             so set to the variable itself
                                        !
                                        ISYSW = ISYSO
                                        ISYSH = ISYSI
                                        NOTOTW = NOTOTO
                                        NOTOTH = NOTOTI
                                        IP_ARW = IP_ARO
                                        IP_ARH = IP_ARI
                                        !
                                    ENDIF
                                    !
                                    ISWCUM = 0
                                    CALL resample_v2(NOSEG, NOSEG3, &
                                            NOTOTI, NOTOTW, &
                                            NOTOTH, NOTOTO, &
                                            ISYSI, ISYSW, &
                                            ISYSH, ISYSO, &
                                            GRDSEG(1, IGR3), IDATYP, &
                                            A(IP_ARI), A(IP_ARW), &
                                            ISWCUM, A(IP_ARH), &
                                            A(IP_ARO))
                                    VGRSET(IVAR, 1) = 1
                                ENDIF
                            ENDDO
                        ENDIF
                        !
                        !                 Set the variable for this grid
                        !
                        IF (IGRID /= 1) THEN
                            !
                            !                    Determine characteristics of variable
                            !
                            CALL set_array_parameters(IVAR, IARR, &
                                    IARKND, IV_IDX, &
                                    IDIM1, IDIM2, &
                                    IP_ARR, 1, &
                                    ISYSI, NOTOTI, &
                                    IP_ARI)
                            CALL set_array_parameters(IVAR, IARR, &
                                    IARKND, IV_IDX, &
                                    IDIM1, IDIM2, &
                                    IP_ARR, IGRID, &
                                    ISYSO, NOTOTO, &
                                    IP_ARO)
                            !
                            !                    Determine characteristics of WEIGHT variable
                            !
                            IAGTYP = VARTAG(IVAR)
                            IF (IAGTYP == 2 .OR. IAGTYP == 3) THEN
                                IV_AG = VARAGG(IVAR)
                                IA_AG = VARARR(IV_AG)
                                IX_AG = VARIDX(IV_AG)
                                IK_AG = ARRKND(IA_AG)
                                IP_AG = ARRPOI(IA_AG)
                                ID1_AG = ARRDM1(IA_AG)
                                ID2_AG = ARRDM2(IA_AG)
                                CALL set_array_parameters(IV_AG, IA_AG, &
                                        IK_AG, IX_AG, &
                                        ID1_AG, ID2_AG, &
                                        IP_AG, 1, &
                                        ISYSW, NOTOTW, &
                                        IP_ARW)
                                CALL set_array_parameters(IV_HLP, IA_HLP, &
                                        IK_HLP, IX_HLP, &
                                        ID1HLP, ID2HLP, &
                                        IP_HLP, IGRID, &
                                        ISYSH, NOTOTH, &
                                        IP_ARH)
                            ELSE
                                !
                                !                       Weight and help array's dummy's
                                !                       so set to the variable itself
                                !
                                ISYSW = ISYSO
                                ISYSH = ISYSI
                                NOTOTW = NOTOTO
                                NOTOTH = NOTOTI
                                IP_ARW = IP_ARO
                                IP_ARH = IP_ARI
                                !
                            ENDIF
                            !
                            CALL aggregate_extended(NOSEG, NOSEG2, &
                                    NOTOTI, NOTOTW, &
                                    NOTOTH, NOTOTO, &
                                    ISYSI, ISYSW, &
                                    ISYSH, ISYSO, &
                                    GRDSEG(1, IGRID), IAGTYP, &
                                    A(IP_ARI), A(IP_ARW), &
                                    A(IP_ARH), A(IP_ARO))
                            VGRSET(IVAR, IGRID) = 1
                        ENDIF
                        !
                    ENDIF
                ENDIF
            ENDIF
            !
            !        Zet pointer structuur voor procesmodule, dit hoeft eigenlijk maar 1 keer
            !
            IF (IARKND == 1) THEN
                IPMSA (K + IVARIO - 1) = IP_ARR + IV_IDX - 1
                INCREM(K + IVARIO - 1) = 0
            ELSEIF (IARKND == 2) THEN
                IPMSA (K + IVARIO - 1) = IP_ARR + (IGRID - 1) * IDIM1 * IDIM2 + &
                        IV_IDX - 1
                INCREM(K + IVARIO - 1) = IDIM1
            ELSEIF (IARKND == 3) THEN
                IPMSA (K + IVARIO - 1) = IP_ARR + (IGRID - 1) * IDIM1 * IDIM2 + &
                        (IV_IDX - 1) * IDIM1
                INCREM(K + IVARIO - 1) = 1
            ENDIF
            !
        ENDDO
        !
        !     compute fluxes
        !
        IPFLUX = (IGRID - 1) * NOFLUX * NOSEG + IFLUX(IPROC)
        IPKNMR = (IGRID - 1) * ARRDM1(IIKNMR) * ARRDM2(IIKNMR) + 1
        CALL PROCAL (A, PROMNR(IPROC), FLUX(IPFLUX), IPMSA(K), INCREM(K), &
                NOSEG2, NOFLUX, IEXPNT, IKNMRK(IPKNMR), NOQ1, &
                NOQ2, NOQ3, NOQ4, PRONAM(IPROC), PRVNIO(IPROC), &
                PRVTYP(K), iproc, dll_opb)
        !
        !     the used grid is now the only actual value for the output
        !
        DO IVARIO = 1, PRVNIO(IPROC)
            ITYP = PRVTYP(K + IVARIO - 1)
            IF (ITYP == 3 .OR. ITYP == 4 .OR. ITYP == 5) THEN
                IVAR = PRVVAR(K + IVARIO - 1)
                IARR = VARARR(IVAR)
                IARKND = ARRKND(IARR)
                !
                !           Only for space varying array's
                !
                IF (IARKND >= 2) THEN
                    DO IGR2 = 1, NOGRID
                        VGRSET(IVAR, IGR2) = 0
                    ENDDO
                    VGRSET(IVAR, IGRID) = 1
                ENDIF
            ENDIF

            ! set fractional step array for dispersion and velocities from the processes

            IF (ITYP == 4) THEN
                IARR = VARARR(IVAR)
                IF (IARR == 40) THEN
                    IV_IDX = VARIDX(IVAR)
                    IF (IV_IDX > 0) THEN
                        DSPNDT(IV_IDX) = PRONDT(IPROC)
                    ENDIF
                ENDIF
                IF (IARR == 41) THEN
                    IV_IDX = VARIDX(IVAR)
                    IF (IV_IDX > 0) THEN
                        VELNDT(IV_IDX) = PRONDT(IPROC)
                    ENDIF
                ENDIF
            ENDIF

        ENDDO
        !
        !     Scale fluxes with fractional step
        !
        !
        !     Dis-aggregate fluxes to base grid
        !     Dit is niet volgens ontwerp, flux zou op stof moeten werken
        !     En de stof daarna herverdeeld over volgens de oude verdeling.
        !     dus is deze herverdeling voor twee stoffen anders dus kunnen
        !     we niet eerst de flux op het basis nivo brengen
        !
        !     Oplossing zou zijn de flux voor iedere stof te disaggregeren
        !     met als gewicht de massa van die stof en dan meteen de deriv
        !     voor die stof met de fractional step te vullen. Vervolgens
        !     voor de volgende stof hetzelfde geintje te herhalen. Dus een
        !     loop over de stochi's toe te voegen. Op deze manier loop je voor een
        !     stof met veel fluxen wel eindeloss te aggregeren natuurlijk.
        !
        !     Dus zou je ook eerst deriv op geaggregeerd grid kunnen cummuleren en
        !     daarna eenmaal naar het basis grid. Dit betekend dat ook DERIV
        !     voor alle grids gedefinieerd moet zijn. We moeten er nu wel voor zorgen
        !     dat alle derivs voor een stof op hetzelfde grid komt of dat we alle
        !     derivs bij disaggregatie mee moeten nemen. Als we dan een vlaggetje
        !     per stof per grid meenemen of de deriv gevuld is doen we geen extra werk.
        !     Kunnen we de disaggregatie zo maken dat deze cummuleert in de variable op
        !     het target grid. PRODER vervalt hiermee, wat doen we met met de FLXDMP
        !     functionaliteit van PRODER hier is theoretisch nog een probleem .
        !     verder is nodig STOCHI, DTSTEP, VOLUME
        !
        !     NOG checken of VOLUME OP HET GRID ACTUEEL IS !!! PROBLEEM we
        !     weten het variable nummer van vol niet.
        !
        !

        RETURN
    END

    subroutine twopro (nproc, nogrid, noflux, novar, noseg, &
            notot, progrd, grdnos, iflux, vgrset, &
            grdseg, volume, deriv, stochi, flux, &
            prondt, ibflag, isdmp, flxdmp, &
            ipbloo, istep)

        !     Deltares - Delft Software Department

        !     Created   : Dec. 2009 by Leo Postma

        !     Function  : This routine has been split off from the 'onepro' routine and in that sense
        !                 Jan van Beek is the author of this code since somewhere 1992.
        !                 Onepro is used in a parallel setting in such a way that previous processes
        !                 always have completed the generation of input for the following processes.
        !                 Conflicts nevertheless arose because more parallel instances of 'onepro'
        !                 could together want to update the same derivative array. This is prevented
        !                 by isolation of the update of the derivative array for all processes together
        !                 in this separate routine outside of the parallel region of 'onepro'.

        !     Modified  :

        !     Subroutines called :  aggregate_extended - fills a variable on a specific grid from its values on another grid
        !                           prodr2 - updates the derivatives from the fluxes
        !                           profld - fills the dump array for fluxes used in a mass balance

        use timers
        use aggregation, only : aggregate_extended, resample

        implicit none

        !     Arguments           :

        !     Kind        Function         Name   Dimensions                 Description

        integer(kind = int_wp), intent(in) :: nproc                           ! Total number of processes
        integer(kind = int_wp), intent(in) :: nogrid                          ! Total number of grids
        integer(kind = int_wp), intent(in) :: noflux                          ! Total number of fluxes
        integer(kind = int_wp), intent(in) :: novar                           ! Total number of variables
        integer(kind = int_wp), intent(in) :: noseg                           ! Total number of computational volumes
        integer(kind = int_wp), intent(in) :: notot                           ! Total number of substances
        integer(kind = int_wp), intent(in) :: progrd(nproc)                  ! The grid number of each process
        integer(kind = int_wp), intent(in) :: grdnos(nogrid)                  ! The nummber of volumes in each grid
        integer(kind = int_wp), intent(in) :: iflux (nproc)                  ! Offset in the flux array per process
        integer(kind = int_wp), intent(inout) :: vgrset(novar, nogrid)  ! Indicates whether a variable for a grid is set
        integer(kind = int_wp), intent(in) :: grdseg(noseg, nogrid)  ! Probably the aggregation pointer of the grids
        real(kind = real_wp), intent(inout) :: volume(noseg, nogrid)  ! Computational volumes
        real(kind = real_wp), intent(inout) :: deriv (notot, noseg, nogrid)  ! Array with derivatives
        real(kind = real_wp), intent(in) :: stochi(notot, noflux)          ! Stoichiometric factors per flux
        real(kind = real_wp), intent(in) :: flux  (noflux, noseg, nogrid)  ! Process fluxes
        integer(kind = int_wp), intent(in) :: prondt(nproc)                  ! Time step size of the process
        integer(kind = int_wp), intent(in) :: ibflag                          ! If > 0 then balances are required
        integer(kind = int_wp), intent(in) :: isdmp (noseg)                  ! Segment to dumped segment pointer
        real(kind = real_wp), intent(inout) :: flxdmp(noflux, *)          ! Dumped fluxes
        integer(kind = int_wp), intent(in) :: ipbloo                          ! The BLOOM  process if any
        integer(kind = int_wp), intent(in) :: istep                           ! Time step nr.

        !     Local

        integer(kind = int_wp) :: iproc                           ! Loop counter over processes
        integer(kind = int_wp) :: igrid                           ! Grid nr of this process
        integer(kind = int_wp) :: noseg2                          ! Number of computational volumes in this grid
        integer(kind = int_wp) :: nfluxp                          ! Number of fluxes in this process
        integer(kind = int_wp), save :: ithandl = 0
        if (timon) call timstrt ("twopro", ithandl)

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

            !        If necessary set volume for this grid.

            igrid = progrd(iproc)
            noseg2 = grdnos(igrid)
            if (vgrset(1, igrid) /= 1) then  !
                call aggregate_extended(noseg, noseg2, 1, 1, 1, &
                        1, 1, 1, 1, 1, &
                        grdseg(1, igrid), 1, volume, volume, volume, &
                        volume(1, igrid))
                vgrset(1, igrid) = 1              !  Volume is always variable 1
            endif

            !        Construct derivatives from these fluxes on this grid

            call prodr2 (deriv(1, 1, igrid), notot, noflux, stochi, iflux (iproc), &
                    nfluxp, flux(1, 1, igrid), noseg2, volume(1, igrid), prondt(iproc))

            !        For the use in balances, store fluxes in 'flxdmp' using aggregation pointer 'isdmp'

            if (ibflag > 0) then
                call profld (noflux, iflux (iproc), nfluxp, igrid, noseg2, &
                        noseg, prondt(iproc), isdmp, grdseg, flux(1, 1, igrid), &
                        volume, flxdmp)
            endif

        enddo

        if (timon) call timstop (ithandl)
        return
    end

end module m_proces
