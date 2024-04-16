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
module m_integration_scheme_21_22
    use m_waq_precision
    use m_zlayer
    use m_zercum
    use m_sgmres
    use m_setset
    use m_proint
    use m_proces
    use m_hsurf
    use m_dlwq_output_theta
    use m_dlwqtr
    use m_dlwqt0
    use m_dlwqo2

    implicit none

contains


    subroutine integration_scheme_21_22 (buffer, lun, lchar, &
            action, dlwqd, gridps)

        !       Deltares Software Centre

        !>\file
        !>                         Self adjusting theta method (21 & 22)
        !>
        !>                         Performs time dependent integration upwind in space,
        !>                         implicit in time using the GMRES method with Krilov
        !>                         orthonormal subspace vectors.
        !>                         Method adjust each time step for each cell the theta.
        !>                         Is implemented as:
        !>                            - method 21 with the flux limiter of Salezac
        !>                            - method 22 with the flux limiter of Boris and Book

        !     Created            :          2007 by Paulien van Slingerland

        !     Modified           : July      2009 Leo Postma Double precission version
        !                          August    2010 Leo Postma OMP parallel version
        !                          January   2011 Leo Postma Boris and Book limiter added

        !     logical units      : lun(19) , output, monitoring file
        !                          lun(20) , output, formatted dump file
        !                          lun(21) , output, unformatted hist. file
        !                          lun(22) , output, unformatted dump file
        !                          lun(23) , output, unformatted dump file
        !
        !     subroutines called : dlwqtr, user transport routine
        !                          proces, delwaq proces system
        !                          dlwqo2, delwaq output system
        !                          dlwq13, system postpro-dump routine
        !                          dlwq14, scales waterquality
        !                          dlwq15, wasteload routine
        !                          dlwq17, boundary routine
        !                          dlwq41, updates volumes
        !                          dlwqt0, updates other time dependent items
        !                          dlwq62, adds transport to matrix and rhs
        !                          dlwqb3, computes volumes
        !                          dlwqb4, computation of mass array
        !                          dlwqb5, performs mass balance computation
        !                          dlwqb6, updates right hand side
        !                          dlwqb7, adds open boundaries to deriv
        !                          dlwqb8, restores conc array
        !                          dlwqf1, initializes matrix pointer administration
        !                          dlwql1, computes variable theta coefficients
        !                          dlwql2, fills matrix
        !                          dlwql3, sets (scaled) rhs of system of equations
        !                          move,   copies one array to another
        !                          proint, integration of fluxes
        !                          open_waq_files, opens files
        !                          sgmres, solves (iteratively) system of equations
        !                          zercum, zero's the cummulative array's

        use m_dlwqm8
        use m_dlwqm7
        use m_dlwqm5
        use m_dlwqm4
        use m_dlwqm3
        use m_dlwqm2
        use m_dlwqm1
        use m_dlwqm0
        use m_dlwqf8
        use m_dlwqf5
        use m_dlwqf1
        use m_dlwqce
        use m_dlwqb8
        use m_dlwqb4
        use m_dlwqb3
        use m_dlwq41
        use m_dlwq17
        use m_dlwq15
        use m_dlwq14
        use m_dlwq13
        use m_delpar01
        use m_array_manipulation, only : copy_real_array_elements
        use data_processing, only : close_files
        use m_grid_utils_external
        use timers
        use variable_declaration                         ! Global memory with allocatable GMRES arrays
        use delwaq2_data
        use m_waq_openda_exchange_items, only : get_openda_buffer
        use m_actions
        use m_sysn          ! System characteristics
        use m_sysi          ! Timer characteristics
        use m_sysa          ! Pointers in real array workspace
        use m_sysj          ! Pointers in integer array workspace
        use m_sysc          ! Pointers in character array workspace
        use m_dlwqdata_save_restore
        use omp_lib

        implicit none


        !     Declaration of arguments

        type(waq_data_buffer), target :: buffer      !< System total array space
        integer(kind = int_wp) :: lun   (*)  !< file unit numbers
        character(*) :: lchar (*)  !< file names
        integer(kind = int_wp) :: action     !< handle to stepwise call
        type(delwaq_data), target :: dlwqd      !< data structure stepwize call
        type(GridPointerColl) :: GridPs     !< collection of all grid definitions

        ! local declarations

        real(kind = real_wp) :: rdummy(1)
        logical         imflag, idflag, ihflag
        logical         update, lrewin
        logical         timon_old
        integer(kind = int_wp) :: laatst

        logical         antidiffusion
        INTEGER(kind = int_wp) :: sindex

        integer(kind = int_wp), save :: ithand1 = 0 ! Leave local

        integer(kind = int_wp) :: isys
        integer(kind = int_wp) :: nstep

        integer(kind = int_wp) :: noth
        integer(kind = int_wp) :: ith

        integer(kind = int_wp) :: ibnd

        !       Variables specific to this method: leave them SAVEd

        integer(kind = int_wp), save :: ioptpc
        integer(kind = int_wp), save :: iter
        integer(kind = int_wp), save :: iscale
        
        integer(kind=int_wp), pointer :: p_iknmkv(:)
        p_iknmkv(1:size(iknmkv)) => iknmkv

        associate (a => buffer%rbuf, j => buffer%ibuf, c => buffer%chbuf)

            !     special remarks    : mass-array is used for rhs vector!!
            !
            !     this option is a mix of option 1 (discretization of transport
            !     in space) and option 6 (matrix inversion to perform implicit
            !     integration in time.
            !     the processes part is integrated explicitly, in order to allow
            !     for any complexity of the processes.
            !     strictly speaking, a loop over substances should be added
            !     to anticipate this, the method uses an
            !     extra volume-array (ivol2), and uses the amass-array (imass)
            !     for the rhs-matrix, instead of the deriv-array as in method 6.
            !     (jvg, may 8 1992)
            !
            !     this option implements:
            !     1) euler backward time integration
            !     2) upwind differences for advection
            !     3) central differences for diffusion
            !     the resulting systems of equations are solved by an iterative
            !     solution method (gmres).
            !     with such an iterative method, systems with multiple rhs cannot be solved
            !     (simultaneously). so we loop over the substances and solve each system
            !     individually. so rhs can be reduced to an real array of size noseg+nobnd.
            !
            !     possible improvements:
            !
            !     - use fgmres instead of gmres for solving system of equations.
            !       this makes it possible to keep search directions which have already
            !       been computed in previous fgmres calls and hence find the solution
            !       of new systems at lower costs!
            !
            !     - tune the preconditioner to speed up the iteration process.
            !       only gaus-seidel, and ssor preconditioning has been implemented yet.
            !
            !     - integrate processes in an implicit way as well. enables users to
            !       potentially take larger time steps (better stability properties)
            !       or even compute steady states in "one time step" (the latter subject
            !       to constraint that proces formulation is time independent).
            !       implicit time integration of processes requires the inexact newton
            !       solution method described in:
            !
            !       "delwaq fastsolver ii"
            !       newton-krylov methods for solving linear and non-linear equations
            !       report t1596, january 1996, Deltares
            !                                                              (kht, 13/11/96)

            if (action == ACTION_FINALISATION) then
                call dlwqdata_restore(dlwqd)
                if (timon) call timstrt ("integration_scheme_21_22", ithandl)
                goto 50
            endif

            if (action == ACTION_INITIALISATION  .or. &
                    action == ACTION_FULLCOMPUTATION) then

                !        some initialisation
                !        ioptpc = preconditioner switch [0 = none, 1 = gs (l), 2 = gs (u),
                !        3 = ssor], iter = maximum number of iterations [ > 0],
                !        tol = relative tolerance [10^-3, 10^-10], iscale = row scaling
                !        of system of equations [0 = no, 1 =yes], klat = number of
                !        layers in preconditioner [1,kmax]

                call dlwqf5 (lun(19), nocons, c(icnam:), a(icons:), ioptpc, &
                        iter, tol, iscale, litrep, noseg, &
                        noq3, noq, nobnd, novec, nomat, &
                        nolay, intsrt, intopt)

                ithandl = 0
                itime = itstrt
                nstep = (itstop - itstrt) / idt
                ifflag = 0
                iaflag = 0
                ibflag = 0
                if (mod(intopt, 16) >= 8) ibflag = 1
                if (ndspn == 0) then
                    nddim = nodisp
                else
                    nddim = ndspn
                endif
                if (nveln == 0) then
                    nvdim = novelo
                else
                    nvdim = nveln
                endif
                updatr = .true.
                lstrec = icflag == 1
                nosss = noseg + nseg2
                noqtt = noq + noq4
                inwtyp = intyp + nobnd
                noqt = noq1 + noq2


                ! initialize second volume array with the first one

                call copy_real_array_elements   (a(ivol:), a(ivol2:), nosss)
            endif

            !
            !     Save/restore the local persistent variables,
            !     if the computation is split up in steps
            !
            !     Note: the handle to the timer (ithandl) needs to be
            !     properly initialised and restored
            !
            if (action == ACTION_INITIALISATION) then
                if (timon) call timstrt ("integration_scheme_21_22", ithandl)
                call dlwqdata_save(dlwqd)
                if (timon) call timstop (ithandl)
                return
            endif

            if (action == ACTION_SINGLESTEP) then
                call dlwqdata_restore(dlwqd)
            endif

            antidiffusion = btest(intopt, 16)

            if (timon) call timstrt ("integration_scheme_21_22", ithandl)

            !======================= simulation loop ============================
            10 continue

            !     Determine the volumes and areas that ran dry at start of time step

            call hsurf  (noseg, nopa, c(ipnam:), a(iparm:), nosfun, &
                    c(isfna:), a(isfun:), surface, lun(19))
            call dryfld (noseg, nosss, nolay, a(ivol:), noq1 + noq2, &
                    a(iarea:), nocons, c(icnam:), a(icons:), surface, &
                    j(iknmr:), iknmkv)

            !          user transport processes

            update = updatr
            call dlwqtr (notot, nosys, nosss, noq, noq1, &
                    noq2, noq3, nopa, nosfun, nodisp, &
                    novelo, j(ixpnt:), a(ivol:), a(iarea:), a(iflow:), &
                    a(ileng:), a(iconc:), a(idisp:), a(icons:), a(iparm:), &
                    a(ifunc:), a(isfun:), a(idiff:), a(ivelo:), itime, &
                    idt, c(isnam:), nocons, nofun, c(icnam:), &
                    c(ipnam:), c(ifnam:), c(isfna:), update, ilflag)
            if (update) updatr = .true.

            !jvb  Temporary ? set the variables grid-setting for the DELWAQ variables

            call setset (lun(19), nocons, nopa, nofun, nosfun, &
                    nosys, notot, nodisp, novelo, nodef, &
                    noloc, ndspx, nvelx, nlocx, nflux, &
                    nopred, novar, nogrid, j(ivset:))

            !        return conc and take-over from previous step or initial condition,
            !        and do particle tracking of this step (will be back-coupled next call)

            call delpar01(itime, noseg, nolay, noq, nosys, &
                    notot, a(ivol:), surface, a(iflow:), c(isnam:), &
                    nosfun, c(isfna:), a(isfun:), a(imass:), a(iconc:), &
                    iaflag, intopt, ndmps, j(isdmp:), a(idmps:), &
                    a(imas2:))

            !        call PROCES subsystem

            call proces (notot, noseg, a(iconc:), a(ivol:), itime, &
                    idt, a(iderv:), ndmpar, nproc, nflux, &
                    j(iipms:), j(insva:), j(iimod:), j(iiflu:), j(iipss:), &
                    a(iflux:), a(iflxd:), a(istoc:), ibflag, ipbloo, &
                    ioffbl, a(imass:), nosys, &
                    itfact, a(imas2:), iaflag, intopt, a(iflxi:), &
                    j(ixpnt:), p_iknmkv, noq1, noq2, noq3, &
                    noq4, ndspn, j(idpnw:), a(idnew:), nodisp, &
                    j(idpnt:), a(idiff:), ndspx, a(idspx:), a(idsto:), &
                    nveln, j(ivpnw:), a(ivnew:), novelo, j(ivpnt:), &
                    a(ivelo:), nvelx, a(ivelx:), a(ivsto:), a(idmps:), &
                    j(isdmp:), j(ipdmp:), ntdmpq, a(idefa:), j(ipndt:), &
                    j(ipgrd:), j(ipvar:), j(iptyp:), j(ivarr:), j(ividx:), &
                    j(ivtda:), j(ivdag:), j(ivtag:), j(ivagg:), j(iapoi:), &
                    j(iaknd:), j(iadm1:), j(iadm2:), j(ivset:), j(ignos:), &
                    j(igseg:), novar, a, nogrid, ndmps, &
                    c(iprna:), intsrt, &
                    j(iprvpt:), j(iprdon:), nrref, j(ipror:), nodef, &
                    surface, lun(19))


            !     set new boundaries
            if (itime >= 0) then
                ! first: adjust boundaries by OpenDA
                if (dlwqd%inopenda) then
                    do ibnd = 1, nobnd
                        do isys = 1, nosys
                            call get_openda_buffer(isys, ibnd, 1, 1, &
                                    A(ibset:+(ibnd - 1) * nosys + isys - 1))
                        enddo
                    enddo
                endif

                call dlwq17 (a(ibset:), a(ibsav:), j(ibpnt:), nobnd, nosys, &
                        notot, idt, a(iconc:), a(iflow:), a(iboun:))
            endif

            !     call output system
            call dlwqo2 (notot, nosss, nopa, nosfun, itime, &
                    c(imnam:), c(isnam:), c(idnam:), j(idump:), nodump, &
                    a(iconc:), a(icons:), a(iparm:), a(ifunc:), a(isfun:), &
                    a(ivol:), nocons, nofun, idt, noutp, &
                    lchar, lun, j(iiout:), j(iiopo:), a(iriob:), &
                    c(iosnm:), c(iouni:), c(iodsc:), c(issnm:), c(isuni:), c(isdsc:), &
                    c(ionam:), nx, ny, j(igrid:), c(iedit:), &
                    nosys, a(iboun:), j(ilp:), a(imass:), a(imas2:), &
                    a(ismas:), nflux, a(iflxi:), isflag, iaflag, &
                    ibflag, imstrt, imstop, imstep, idstrt, &
                    idstop, idstep, ihstrt, ihstop, ihstep, &
                    imflag, idflag, ihflag, noloc, a(iploc:), &
                    nodef, a(idefa:), itstrt, itstop, ndmpar, &
                    c(idana:), ndmpq, ndmps, j(iqdmp:), j(isdmp:), &
                    j(ipdmp:), a(idmpq:), a(idmps:), a(iflxd:), ntdmpq, &
                    c(icbuf:), noraai, ntraaq, j(ioraa:), j(nqraa:), &
                    j(iqraa:), a(itrra:), c(irnam:), a(istoc:), nogrid, &
                    novar, j(ivarr:), j(ividx:), j(ivtda:), j(ivdag:), &
                    j(iaknd:), j(iapoi:), j(iadm1:), j(iadm2:), j(ivset:), &
                    j(ignos:), j(igseg:), a, nobnd, nobtyp, &
                    c(ibtyp:), j(intyp:), c(icnam:), noq, j(ixpnt:), &
                    intopt, c(ipnam:), c(ifnam:), c(isfna:), j(idmpb:), &
                    nowst, nowtyp, c(iwtyp:), j(iwast:), j(inwtyp:), &
                    a(iwdmp:), iknmkv, isegcol)

            ! zero cumulative arrays
            if (imflag .or. (ihflag .and. noraai > 0)) then
                call zercum (notot, nosys, nflux, ndmpar, ndmpq, &
                        ndmps, a(ismas:), a(iflxi:), a(imas2:), &
                        a(idmpq:), a(idmps:), noraai, imflag, ihflag, &
                        a(itrra:), ibflag, nowst, a(iwdmp:))
            endif

            !     simulation done ?
            if (itime < 0) goto 9999
            if (itime >= itstop) goto 50

            !        restore conc-array from mass array

            call dlwqb8 (nosys, notot, nototp, noseg, a(ivol:), &
                    surface, a(imass:), a(iconc:))

            !     add processes
            call dlwq14 (a(iderv:), notot, noseg, itfact, a(imas2:), &
                    idt, iaflag, a(idmps:), intopt, j(isdmp:))

            !     get new volumes
            itimel = itime
            itime = itime + idt
            select case (ivflag)
            case (1)                 !     computation of volumes for computed volumes only
                call copy_real_array_elements   (a(ivol:), a(ivol2:), noseg)
                call dlwqb3 (a(iarea:), a(iflow:), a(ivnew:), j(ixpnt:), notot, &
                        noq, nvdim, j(ivpnw:), a(ivol2:), intopt, &
                        a(imas2:), idt, iaflag, nosys, a(idmpq:), &
                        ndmpq, j(iqdmp:))
                updatr = .true.
            case (2)                 !     the fraudulent computation option
                call dlwq41 (lun, itime, itimel, a(iharm:), a(ifarr:), &
                        j(inrha:), j(inrh2:), j(inrft:), noseg, a(ivoll:), &
                        j(ibulk:), lchar, ftype, isflag, ivflag, &
                        updatr, j(inisp:), a(inrsp:), j(intyp:), j(iwork:), &
                        lstrec, lrewin, a(ivol2:), dlwqd)
                call dlwqf8 (noseg, noq, j(ixpnt:), idt, iknmkv, &
                        a(ivol:), a(iflow:), a(ivoll:), a(ivol2:))
                updatr = .true.
                lrewin = .true.
                lstrec = .true.
            case default               !     read new volumes from files
                call dlwq41 (lun, itime, itimel, a(iharm:), a(ifarr:), &
                        j(inrha:), j(inrh2:), j(inrft:), noseg, a(ivol2:), &
                        j(ibulk:), lchar, ftype, isflag, ivflag, &
                        updatr, j(inisp:), a(inrsp:), j(intyp:), j(iwork:), &
                        lstrec, lrewin, a(ivoll:), dlwqd)
            end select

            !     Update the info on dry volumes with the new volumes        ( dryfle )
            !      Compute new from-topointer on the basis of non-zeroflows  ( zflows )
            !       Initialize pointer matices for fast solvers              ( dlwqf1 )

            call dryfle (noseg, nosss, a(ivol2:), nolay, nocons, &
                    c(icnam:), a(icons:), surface, j(iknmr:), iknmkv)
            call zflows (noq, noqt, nolay, nocons, c(icnam:), &
                    a(iflow:), j(ixpnt:))
            call dlwqf1 (noseg, nobnd, noq, noq1, noq2, &
                    nomat, j(ixpnt:), j(iwrk:), j(imat:), rowpnt, &
                    fmat, tmat)

            ! add the waste loads

            call dlwq15 (nosys, notot, noseg, noq, nowst, &
                    nowtyp, ndmps, intopt, idt, itime, &
                    iaflag, c(isnam:), a(iconc:), a(ivol:), a(ivol2:), &
                    a(iflow:), j(ixpnt:), c(iwsid:), c(iwnam:), c(iwtyp:), &
                    j(inwtyp:), j(iwast:), iwstkind, a(iwste:), a(iderv:), &
                    iknmkv, nopa, c(ipnam:), a(iparm:), nosfun, &
                    c(isfna:), a(isfun:), j(isdmp:), a(idmps:), a(imas2:), &
                    a(iwdmp:), 1, notot)

            !          Here we implement a loop that inverts the same matrix
            !          for series of subsequent substances having the same
            !          additional VELO and DISPER array. (JvG, April 24, 1993).

            !          In solving equations with multiple rhs, the *FULL* fast (or should
            !          I say slow?) solver algorithm needs to be applied to each rhs vector
            !          So DELMAT may outperform FS when we deal with a large number of
            !          substances with the same additional velocity and dispersion field
            !          in future we need a smart switch between DELMAT and FS at this place
            !          For now always do FS!
            !                                                               (KHT, 11/11/96)

            call dlwqm7 (noq, noq1, noq2, a(iarea:), a(iflow:), &
                    a(ileng:), ilflag, intopt, j(ixpnt:), mixlen, &
                    iknmkv)

            if (timon) call timstrt ("ADE solver", ithand1)
            timon_old = timon
            noth = OMP_GET_MAX_THREADS()
            if (noth > 1) timon = .false.
            !$OMP PARALLEL
            !$OMP DO PRIVATE(ith)
            ! start of loop over substances

            do isys = 1, nosys

                ith = OMP_GET_THREAD_NUM() + 1

                !     make flow and dispersion arrays
                call dlwqm0 (isys, nosys, noq, noq1, noq2, &
                        a(iarea:), a(iflow:), flowtot(1, ith), nvdim, j(ivpnw:), &
                        a(ivnew:), a(idisp:), disptot(1, ith), nddim, j(idpnw:), &
                        a(idnew:), mixlen)

                !     compute variable theta coefficients
                call dlwqm1 (idt, noseg, nobnd, a(ivol:), noq, &
                        noq1, noq2, j(ixpnt:), flowtot(1, ith), disptot(1, ith), &
                        theta(1:, ith), thetaseg(1, ith), antidiffusion, iexseg (:, ith))

                if (isys == 1) call dlwq_output_theta (nrvart, c(ionam:), j(iiopo:), nocons, nopa, &
                        nofun, nosfun, notot, noseg, noloc, &
                        a(iploc:), nodef, thetaseg(1, ith))

                !     construct matrix
                call dlwqm2 (idt, noseg, a(ivol2:), nobnd, noq, &
                        j(ixpnt:), flowtot(1, ith), disptot(1, ith), theta(1:, ith), gm_diag(1, ith), &
                        iscale, gm_diac(1:, ith), nomat, gm_amat(1, ith), rowpnt, &
                        fmat, tmat, iexseg (:, ith))

                !     construct rhs
                call dlwqm3 (idt, isys, nosys, notot, noseg, &
                        a(iconc:), a(iderv:), a(ivol:), nobnd, a(iboun:), &
                        noq, j(ixpnt:), flowtot(1, ith), disptot(1, ith), theta(1:, ith), &
                        gm_diac(1:, ith), iscale, gm_rhs (1, ith), gm_sol(1, ith))

                !     solve linear system of equations by means of gmres to obtain local theta solution estimation
                call sgmres (noseg + nobnd, gm_rhs (1, ith), gm_sol(1, ith), novec, gm_work(1, ith), &
                        noseg + nobnd, gm_hess(1, ith), novec + 1, iter, tol, &
                        nomat, gm_amat(1, ith), j(imat:), gm_diag(1, ith), rowpnt, &
                        nolay, ioptpc, nobnd, gm_trid(1, ith), iexseg (:, ith), &
                        lun(19), litrep)

                !     mass balance of transport
                call dlwqm4 (isys, nosys, notot, noseg, a(iconc:), &
                        gm_sol(1, ith), nobnd, a(iboun:), noq, j(ixpnt:), &
                        theta (1, ith), flowtot(1, ith), disptot(1, ith), a(imas2:), ndmpq, &
                        j(iqdmp:), a(idmpq:), idt)

                !     apply flux corrected transport to obtain the local theta fct solution estimation
                if (intsrt == 21)       & ! Flux correction according to Salezac  (Pauline)
                        call dlwqm5 (idt, isys, nosys, notot, noseg, &
                                a(iconc:), gm_sol(1, ith), a(ivol2:), nobnd, a(iboun:), &
                                noq, noq1, noq2, noq3, j(ixpnt:), &
                                iknmkv, a(iarea:), a(ileng:), theta(1:, ith), flowtot(1, ith), &
                                disptot(1, ith), intopt, a(imas2:), ndmpq, j(iqdmp:), &
                                a(idmpq:), flux(1, ith), lim(1, ith), maxi (1, ith), mini   (1, ith), &
                                l1(1, ith), l2  (1, ith), m1 (1, ith), m2   (1, ith), n1     (1, ith), &
                                n2(1, ith))
                if (intsrt == 22)       & ! Flux correction according to Boris and Book  (Leo)
                        call dlwqm8 (idt, isys, nosys, notot, noseg, &
                                a(iconc:), gm_sol (1, ith), a(ivol2:), nobnd, a(iboun:), &
                                noq, iknmkv, j(ixpnt:), a(iarea:), a(ileng:), &
                                theta(1:, ith), flowtot(1, ith), intopt, a(imas2:), ndmpq, &
                                j(iqdmp:), a(idmpq:))

            end do
            !$OMP ENDDO
            !$OMP ENDPARALLEL
            if (noth > 1) timon = timon_old

            if (timon) call timstop (ithand1)

            ! update mass array, explicit step for passive substances

            call dlwqb4 (nosys, notot, nototp, noseg, a(ivol2:), &
                    surface, a(imass:), a(iconc:), a(iderv:), idt)

            !     calculate closure error
            if (lrewin .and. lstrec) then
                call dlwqce (a(imass:), a(ivoll:), a(ivol2:), nosys, notot, &
                        noseg, lun(19))
                call copy_real_array_elements   (a(ivoll:), a(ivol:), noseg)
            else
                !     replace old by new volumes
                call copy_real_array_elements   (a(ivol2:), a(ivol:), noseg)
            endif

            !     integrate the fluxes at dump segments fill asmass with mass
            if (ibflag > 0) then
                call proint (nflux, ndmpar, idt, itfact, a(iflxd:), &
                        a(iflxi:), j(isdmp:), j(ipdmp:), ntdmpq)
            endif

            !     update all other time functions
            call dlwqt0 (lun, itime, itimel, a(iharm:), a(ifarr:), &
                    j(inrha:), j(inrh2:), j(inrft:), idt, a(ivol:), &
                    a(idiff:), a(iarea:), a(iflow:), a(ivelo:), a(ileng:), &
                    a(iwste:), a(ibset:), a(icons:), a(iparm:), a(ifunc:), &
                    a(isfun:), j(ibulk:), lchar, c(ilunt:), ftype, &
                    intsrt, isflag, ifflag, ivflag, ilflag, &
                    update, j(iktim:), j(iknmr:), j(inisp:), a(inrsp:), &
                    j(intyp:), j(iwork:), .false., ldummy, rdummy, &
                    .false., gridps, dlwqd)
            if (update) updatr = .true.

            !     end of time loop
            if (action == ACTION_FULLCOMPUTATION) then
                goto 10
            endif

            50 continue

            if (action == ACTION_FINALISATION    .or. &
                    action == ACTION_FULLCOMPUTATION) then

                ! close files, except monitor file

                call CloseHydroFiles(dlwqd%collcoll)
                call close_files(lun)

                ! write restart file
                call dlwq13 (lun, lchar, a(iconc:), itime, c(imnam:), &
                        c(isnam:), notot, noseg)
            endif

        end associate
        9999 if (timon) call timstop (ithandl)

        dlwqd%iaflag = iaflag
        dlwqd%itime = itime

        return

    end subroutine integration_scheme_21_22

end module m_integration_scheme_21_22
