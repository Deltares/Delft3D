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
module m_integration_scheme_18
    use m_waq_precision
    use m_sgmres
    use m_setset
    use m_proces
    use m_hsurf
    use m_dlwqtr
    use m_dlwqo2

    implicit none

contains


    subroutine integration_scheme_18 (buffer, file_unit_list, file_name_list, action, dlwqd, gridps)
        !> Iterative stationary vertical central method (18)
        !>
        !>                         Performs substance by substance a stationairy solution.\n
        !>                         Uses the GMRES method with Krilov sub-spaces.\n
        !>                         Horizontal fluxes are discretized upwind.\n
        !>                         Vertical fluxes are discretized central.\n

        !     CREATED            : june 1988 by L. Postma
        !
        !     MODIFIED           : feb 1997, by RJ Vos; like 6 but steady state solver
        !                          is GMRES and compact storage from option 16
        !
        !     LOGICAL UNITS      : file_unit_list(19) , output, monitoring file
        !                          file_unit_list(20) , output, formatted dump file
        !                          file_unit_list(21) , output, unformatted hist. file
        !                          file_unit_list(22) , output, unformatted dump file
        !                          file_unit_list(23) , output, unformatted restart file
        !
        !     SUBROUTINES CALLED : DLWQTR, user transport routine
        !                          DLWQO2, DELWAQ4 output routine
        !                          DLWQ13, system postpro-dump routine
        !                          DLWQ15, wasteload routine
        !                          DLWQ60, scales water quality
        !                          DLWQH1, set diagonal and deriv in deriv(1)
        !                          DLWQG3, fills the matrix (except diagonal)
        !                          DLWQ63, stores the results
        !                          DLWQ64, performs mass balance computation
        !                          DLWQ65, computes closure error
        !                          DLWQ66, makes masses
        !                          DLWQH3, check diagonal on zero's
        !                          SGMRES, fast solver
        !                          open_waq_files, opens files
        !                          MOVE  , copy an array
        !
        !      NOTE             :   " DELWAQ FASTSOLVERS 2 " (R.J.Vos, M.Borsboom and K.
        !       Newton-Krylov methods for solving linear and non-linear equations
        !       report T1596, January 1996, Deltares
        !                       :   NSYS = 1 always
        !
        !     PARAMETERS    :
        !
        !     NAME    KIND     LENGTH   FUNC.  DESCRIPTION
        !     ---------------------------------------------------------
        !     A       REAL       *      LOCAL  real      workspace array
        !     J       INTEGER    *      LOCAL  integer   workspace array
        !     C       CHARACTER  *      LOCAL  character workspace array
        !     file_unit_list     INTEGER    *      INPUT  array with unit numbers
        !     file_name_list   CHARACTER  *      INPUT  filenames
        !
        !     Declaration of arguments
        !
        use m_dlwqg3
        use m_dlwqh6
        use m_dlwqh3
        use m_dlwqh1
        use m_dlwqf5
        use m_dlwqf1
        use m_dlwq66
        use m_dlwq65
        use m_dlwq64
        use m_dlwq60
        use m_dlwq41
        use m_dlwq15
        use m_dlwq13
        use m_array_manipulation, only : initialize_real_array
        use data_processing, only : close_files
        use m_grid_utils_external
        use timers
        use variable_declaration
        use delwaq2_data
        use m_waq_openda_exchange_items, only : get_openda_buffer
        use m_actions
        use m_sysn          ! System characteristics
        use m_sysi          ! Timer characteristics
        use m_sysa          ! Pointers in real array workspace
        use m_sysj          ! Pointers in integer array workspace
        use m_sysc          ! Pointers in character array workspace


        type(waq_data_buffer), target :: buffer      !< System total array space
        INTEGER(kind = int_wp), DIMENSION(*) :: file_unit_list
        CHARACTER*(*), DIMENSION(*) :: file_name_list
        INTEGER(kind = int_wp) :: ACTION
        TYPE(DELWAQ_DATA) :: DLWQD
        type(GridPointerColl) :: GridPs               ! collection off all grid definitions

        LOGICAL          IMFLAG, IDFLAG, IHFLAG
        LOGICAL          LDUMMY, LSTREC, LREWIN
        logical, save :: litrep
        logical          update
        real(kind = dp) :: tol

        INTEGER(kind = int_wp) :: ITIME
        INTEGER(kind = int_wp) :: ITIMEL
        INTEGER(kind = int_wp) :: IFFLAG
        INTEGER(kind = int_wp) :: IAFLAG
        INTEGER(kind = int_wp) :: IBFLAG
        INTEGER(kind = int_wp) :: NDDIM
        INTEGER(kind = int_wp) :: NVDIM
        INTEGER(kind = int_wp) :: ISYS
        INTEGER(kind = int_wp) :: ICSYS
        INTEGER(kind = int_wp) :: NSYS
        INTEGER(kind = int_wp) :: INWTYP
        INTEGER(kind = int_wp) :: ISTEP
        INTEGER(kind = int_wp) :: ITH
        INTEGER(kind = int_wp) :: I
        INTEGER(kind = int_wp) :: ISCALE
        INTEGER(kind = int_wp) :: NOPRED
        INTEGER(kind = int_wp) :: ITER
        INTEGER(kind = int_wp) :: IOPTPC
        INTEGER(kind = int_wp) :: NOSSS
        INTEGER(kind = int_wp) :: NOQTT
        INTEGER(kind = int_wp) :: sindex

        integer(kind = int_wp) :: ithandl
        integer(kind = int_wp), save :: ithand1 = 0 ! Leave local
        
        integer(kind=int_wp), pointer :: p_iknmkv(:)
        p_iknmkv(1:size(iknmkv)) => iknmkv

        associate (a => buffer%rbuf, j => buffer%ibuf, c => buffer%chbuf)

            if (action == ACTION_INITIALISATION  .or. &
                    action == ACTION_FINALISATION) then
                return
            endif

            !
            !          some initialisation for fast solver
            !
            !       IOPTPC = preconditioner switch [0 = none, 1 = GS (L), 2 = GS (U),
            !       3 = SSOR], ITER = maximum number of iterations [ > 0],
            !       TOL = relative tolerance [10^-3, 10^-10], ISCALE = row scaling
            !       of system of equations [0 = no, 1 =yes], KLAT = number of
            !       layers in preconditioner [1,KMAX]
            !
            ithandl = 0
            if (timon) call timstrt ("integration_scheme_18", ithandl)

            call dlwqf5 (file_unit_list(19), nocons, c(icnam:), a(icons:), ioptpc, &
                    iter, tol, iscale, litrep, noseg, &
                    noq3, noq, nobnd, novec, nomat, &
                    nolay, intsrt, intopt)

            itime = itstrt + idt
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
            lstrec = icflag == 1
            nosss = noseg + nseg2
            noqtt = noq + noq4
            inwtyp = intyp + nobnd

            !      Initialize pointer matices for fast solvers

            call dlwqf1 (noseg, nobnd, noq, noq1, noq2, &
                    nomat, j(ixpnt:), j(iwrk:), j(imat:), rowpnt, &
                    fmat, tmat)

            iexseg = 1     !  There is nothing to mask. This array is meant for method 21

            !
            !======================= simulation loop ============================
            !
            !          make closure error correction
            !
            if (j(inrh2 + 1) >= 0 .and. ivflag == 0 .and. &
                    idt       > 0 .and. lstrec) then
                call dlwq41 (file_unit_list, itstrt + idt, itstrt, a(iharm:), a(ifarr:), &
                        j(inrha:), j(inrh2:), j(inrft:), noseg, a(ivol2:), &
                        j(ibulk:), file_name_list, ftype, isflag, ivflag, &
                        update, j(inisp:), a(inrsp:), j(intyp:), j(iwork:), &
                        lstrec, lrewin, a(ivoll:), dlwqd)
                call dlwq65 (a(ivol2:), a(ivol:), idt, noseg)
            else
                call initialize_real_array   (a(ivol2:), noseg)
            endif

            !        Determine the volumes and areas that ran dry,
            !        They cannot have explicit processes during this time step

            call hsurf  (noseg, nopa, c(ipnam:), a(iparm:), nosfun, &
                    c(isfna:), a(isfun:), surface, file_unit_list(19))
            call dryfld (noseg, nosss, nolay, a(ivol:), noq1 + noq2, &
                    a(iarea:), nocons, c(icnam:), a(icons:), surface, &
                    j(iknmr:), iknmkv)

            !          user transport processes

            call dlwqtr (notot, nosys, nosss, noq, noq1, &
                    noq2, noq3, nopa, nosfun, nodisp, &
                    novelo, j(ixpnt:), a(ivol:), a(iarea:), a(iflow:), &
                    a(ileng:), a(iconc:), a(idisp:), a(icons:), a(iparm:), &
                    a(ifunc:), a(isfun:), a(idiff:), a(ivelo:), itime, &
                    idt, c(isnam:), nocons, nofun, c(icnam:), &
                    c(ipnam:), c(ifnam:), c(isfna:), ldummy, ilflag)

            !jvb  Temporary ? set the variables grid-setting for the DELWAQ variables

            call setset (file_unit_list(19), nocons, nopa, nofun, nosfun, &
                    nosys, notot, nodisp, novelo, nodef, &
                    noloc, ndspx, nvelx, nlocx, nflux, &
                    nopred, novar, nogrid, j(ivset:))

            !          call PROCES subsystem

            call proces (notot, nosss, a(iconc:), a(ivol:), itime, &
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
                    surface, file_unit_list(19))

            !          loop over the systems

            ith = 1
            IAFLAG = 1
            DO ISYS = 1, NOSYS

                !          do the user water quality processes

                ICSYS = ISYS
                CALL DLWQ60 (A(IDERV:), A(ICONC:), NOTOT, NOSEG, ITFACT, &
                        A(IMAS2:), ISYS, 1, A(IDMPS:), INTOPT, &
                        J(ISDMP:))

                !          add the waste loads

                call dlwq15 (nosys, notot, noseg, noq, nowst, &
                        nowtyp, ndmps, intopt, 1, itime, &
                        iaflag, c(isnam:), a(iconc:), a(ivol:), a(ivol2:), &
                        a(iflow:), j(ixpnt:), c(iwsid:), c(iwnam:), c(iwtyp:), &
                        j(inwtyp:), j(iwast:), iwstkind, a(iwste:), a(iderv:), &
                        iknmkv, nopa, c(ipnam:), a(iparm:), nosfun, &
                        c(isfna:), a(isfun:), j(isdmp:), a(idmps:), a(imas2:), &
                        a(iwdmp:), isys, 1)

                !          fill the diagonal of the matrix, with conc-array and closure error

                call dlwqh1 (noseg, notot, nobnd, isys, gm_diag(1, ith), &
                        a(ivol2:), a(iconc:))

                !          build rest of matrix like in option 16

                call dlwqg3 (noseg, nobnd, noq1, noq2, noq, &
                        j(ixpnt:), nddim, nvdim, j(idpnw:), j(ivpnw:), &
                        a(iarea:), a(iflow:), a(ileng:), a(idisp:), a(idnew:), &
                        a(ivnew:), isys, intopt, ilflag, nomat, &
                        gm_amat(1, ith), j(imat:), rowpnt, gm_diag(1, ith), gm_diac(1:, ith), &
                        iscale, fmat, tmat, iknmkv)

                !          initial guess : take rhs / diagonal

                call dlwqh3 (noseg, nosys, notot, nobnd, isys, &
                        a(iderv:), a(iboun:), gm_rhs(1, ith), gm_diac(1:, ith), gm_sol (1, ith))

                !          solve linear system of equations
                !          note that RHS is in A(IDERV:) for steady state otpions

                call sgmres (noseg + nobnd, gm_rhs (1, ith), gm_sol (1, ith), novec, gm_work(1, ith), &
                        noseg + nobnd, gm_hess(1, ith), novec + 1, iter, tol, &
                        nomat, gm_amat(1, ith), j(imat:), gm_diag(1, ith), rowpnt, &
                        nolay, ioptpc, nobnd, gm_trid(1, ith), iexseg (:, ith), &
                        file_unit_list(19), litrep)

                !           copy solution for this substance into concentration array, note that the array for
                !                                                                      segment dumps is not filled yet

                call dlwqh6 (noseg, notot, isys, 1, a(iconc:), &
                        gm_sol(1, ith), a(imas2:), a(idmps:), intopt, j(isdmp:))

            end do
            !
            !          mass balance
            !
            IAFLAG = 1
            CALL DLWQ64 (A(IDISP:), A(IDNEW:), A(IAREA:), A(IFLOW:), A(ILENG:), &
                    A(IVNEW:), A(ICONC:), A(IBOUN:), J(IXPNT:), NOSYS, &
                    NOTOT, NOQ1, NOQ2, NOQ, NDDIM, &
                    NVDIM, J(IDPNW:), J(IVPNW:), INTOPT, A(IMAS2:), &
                    ILFLAG, A(IDMPQ:), NDMPQ, J(IQDMP:))
            CALL DLWQ66 (A(IDERV:), A(IVOL:), A(ICONC:), NOTOT, NOSEG)
            !
            !     Call OUTPUT system ( note that mass is in A(IDERV:) )
            !
            CALL DLWQO2 (NOTOT, NOSEG, NOPA, NOSFUN, ITSTRT, &
                    C(IMNAM:), C(ISNAM:), C(IDNAM:), J(IDUMP:), NODUMP, &
                    A(ICONC:), A(ICONS:), A(IPARM:), A(IFUNC:), A(ISFUN:), &
                    A(IVOL:), NOCONS, NOFUN, 1, NOUTP, &
                    file_name_list, file_unit_list, J(IIOUT:), J(IIOPO:), A(IRIOB:), &
                    C(IOSNM:), C(IOUNI:), C(IODSC:), C(ISSNM:), C(ISUNI:), C(ISDSC:), &
                    C(IONAM:), NX, NY, J(IGRID:), C(IEDIT:), &
                    NOSYS, A(IBOUN:), J(ILP:), A(IDERV:), A(IMAS2:), &
                    A(ISMAS:), NFLUX, A(IFLXI:), ISFLAG, IAFLAG, &
                    IBFLAG, IMSTRT, IMSTOP, IMSTEP, IDSTRT, &
                    IDSTOP, IDSTEP, IHSTRT, IHSTOP, IHSTEP, &
                    IMFLAG, IDFLAG, IHFLAG, NOLOC, A(IPLOC:), &
                    NODEF, A(IDEFA:), ITSTRT, ITSTOP, NDMPAR, &
                    C(IDANA:), NDMPQ, NDMPS, J(IQDMP:), J(ISDMP:), &
                    J(IPDMP:), A(IDMPQ:), A(IDMPS:), A(IFLXD:), NTDMPQ, &
                    C(ICBUF:), NORAAI, NTRAAQ, J(IORAA:), J(NQRAA:), &
                    J(IQRAA:), A(ITRRA:), C(IRNAM:), A(ISTOC:), NOGRID, &
                    NOVAR, J(IVARR:), J(IVIDX:), J(IVTDA:), J(IVDAG:), &
                    J(IAKND:), J(IAPOI:), J(IADM1:), J(IADM2:), J(IVSET:), &
                    J(IGNOS:), J(IGSEG:), A, NOBND, NOBTYP, &
                    C(IBTYP:), J(INTYP:), C(ICNAM:), NOQ, J(IXPNT:), &
                    INTOPT, C(IPNAM:), C(IFNAM:), C(ISFNA:), J(IDMPB:), &
                    NOWST, NOWTYP, C(IWTYP:), J(IWAST:), J(INWTYP:), &
                    A(IWDMP:), iknmkv, isegcol)
            !
            !          close files, except monitor file
            !

            call CloseHydroFiles(dlwqd%collcoll)
            call close_files(file_unit_list)
            !
            !          write restart file
            !
            CALL DLWQ13 (file_unit_list, file_name_list, A(ICONC:), ITSTRT, C(IMNAM:), &
                    C(ISNAM:), NOTOT, NOSEG)

            !
        end associate
        if (timon) call timstop (ithandl)
        RETURN
    END

end module m_integration_scheme_18
