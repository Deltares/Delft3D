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
module m_prepare_output_data
    use m_waq_precision
    use timers

    implicit none

    private
    public :: write_concentrations_in_grid_layout, fill_output_buffer_base_grid, fill_transport_terms_transects, &
            fill_output_buffer_sub_grid, fill_dump_areas_balances, fill_transect_output_buffer, &
            update_base_grid_local_array, calculate_balance_terms, write_balance_history_output

contains


    subroutine write_concentrations_in_grid_layout(iout, lchout, itime, mname, nx, &
            ny, lgrid, cgrid, notot, nosys, &
            sname, conc, bound, notot2, synam2, &
            conc2, ip, isflag, iniout)

        ! Writes concentrations in grid-layout and
        ! writes the result in IOUT .
        !
        !     LOGICAL UNITS      : IOUT = number of dump file


        !     IOUT    INTEGER  1           INPUT   unit number output file
        !     LCHOUT  CHAR*(*) 1           INPUT   name output file
        !     ITIME   INTEGER  1           INPUT   present time in clock units
        !     MNAME   CHAR*40  4           INPUT   model identhification
        !     NX      INTEGER  1           INPUT   number of columns in grid
        !     NY      INTEGER  1           INPUT   number of rows in grid
        !     LGRID   INTEGER  NX*NY       INPUT   grid layout
        !     CGRID   CHAR*6   20*NY       LOCAL   concentrations in grid layout
        !     NOTOT   INTEGER  1           INPUT   total number of systems
        !     NOSYS   INTEGER  1           INPUT   number of active systems
        !     SNAME   CHAR*20  NOTOT       INPUT   names of substances
        !     CONC    REAL     NOTOT*?     INPUT   concentration values
        !     BOUND   REAL     NOTOT*?     INPUT   boundary      values
        !     NOTOT2  INTEGER  1           INPUT   number of extra output vars
        !     SYNAM2  CHAR*20  NOTOT       INPUT   names of extra vars
        !     CONC2   REAL    NOTOT2,NX*NY INPUT   values for extra vars
        !     IP      INTEGER  6           IN/OUT  paging structure
        !     ISFLAG  INTEGER  1           INPUT   if 1 then dd-hh:mm'ss'
        !     INIOUT  INTEGER  1           IN/OUT  Initialize flag

        use date_time_utils, only: report_time

        integer(kind = int_wp) :: iout, itime, nx, ny, notot, &
                nosys, isflag, notot2, iniout
        integer(kind = int_wp) :: lgrid(*), ip(*)
        real(kind = real_wp) :: conc(notot, *), bound(nosys, *), &
                conc2 (*)
        character(len = 6)   cgrid (20, *)
        character(len = 20)  sname (*), synam2(*)
        character(len = 40)  mname (*)
        character(len = *) lchout

        ! local declaration
        integer(kind = int_wp) :: itot, i, i1, i2, i3, k, j, iscale, factor, nend
        real(kind = real_wp) :: cmax, c
        real(kind = real_wp), parameter :: rmiss = -999.
        character(len = 6)   point, padder
        data          point / '  .   ' /
        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("write_concentrations_in_grid_layout", ithandl)

        if (nx * ny == 0) goto 9999  !   return

        ! initialise the paging
        if (ip(3) == 0) then
            ip(3) = max(1, ip(1) / (7 + (ny + 5) * ((nx + ip(2) - 1) / ip(2))))
            ip(4) = 0
        endif

        ! repeat output for every substance
        do itot = 1, notot

            ! calculate maximum concentration of displayed segments
            cmax = 0.0
            do i1 = 1, ny
                do i2 = 1, nx
                    i3 = lgrid ((i1 - 1) * nx + i2)
                    if (i3 > 0) cmax = amax1 (cmax, conc (itot, i3))
                    if (i3 < 0 .and. itot <= nosys) &
                            cmax = amax1 (cmax, bound(itot, -i3))
                end do
            end do

            ! calculate scale factor
            if (cmax <= 0.0) then
                iscale = 0
            else if (cmax >= 0.5) then
                iscale = aint(alog10(cmax) + 2.2e-5)
            else
                iscale = -aint(-alog10(cmax) - 2.2e-5) - 1
            endif

            ! start printing
            if (mod(ip(4), ip(3)) == 0) then
                write (iout, '('' '')')
                write (iout, 2040) (mname(k), k = 1, 4)
            endif
            ip(4) = ip(4) + 1
            call report_time(iout, itime, isflag, -999.)
            write(iout, 2000) sname(itot), iscale

            ! put concentration values in grid layout
            factor = 10.0**iscale
            do i = 1, nx, ip(2)
                do j = 1, ny
                    nend = min (nx, i + ip(2) - 1)
                    do k = i, nend
                        cgrid (k - i + 1, j) = point
                        i3 = lgrid ((j - 1) * nx + k)

                        if (i3 > 0) then
                            write (padder, '(f6.3)')   conc (itot, i3) / factor
                            cgrid(k - i + 1, j) = padder
                        endif

                        if (i3 < 0 .and. itot <= nosys) then
                            write (padder, '(f6.3)')   bound(itot, -i3) / factor
                            cgrid(k - i + 1, j) = padder
                        endif
                    end do
                    write (iout, 2030) (cgrid (k - i + 1, j), k = i, nend)
                end do
                write (iout, '('' '')')
            end do
        end do

        ! repeat output for extra substance
        do itot = 1, notot2

            ! calculate maximum concentration of displayed segments
            cmax = 0.0
            do i1 = 1, ny
                do i2 = 1, nx
                    cmax = amax1 (cmax, conc2(itot + (i2 * i1 - 1) * notot2))
                end do
            end do

            ! calculate scale factor
            if (cmax <= 0.0) then
                iscale = 0
            else if (cmax >= 0.5) then
                iscale = aint(alog10(cmax) + 2.2e-5)
            else
                iscale = -aint(-alog10(cmax) - 2.2e-5) - 1
            endif

            ! start printing
            if (mod(ip(4), ip(3)) == 0) then
                write (iout, '('' '')')
                write (iout, 2040) (mname(k), k = 1, 4)
            endif
            ip(4) = ip(4) + 1
            call report_time (iout, itime, isflag, -999.)
            write(iout, 2000) synam2(itot), iscale

            ! Put concentration values in grid layout
            factor = 10.0**iscale
            do i = 1, nx, ip(2)
                do j = 1, ny
                    nend = min (nx, i + ip(2) - 1)
                    do k = i, nend
                        cgrid (k - i + 1, j) = point
                        c = conc2(itot + ((j - 1) * nx + k - 1) * notot2)
                        if (c /= rmiss) then
                            write (padder, '(f6.3)')   c / factor
                            cgrid(k - i + 1, j) = padder
                        endif
                    end do
                    write (iout, 2030) (cgrid (k - i + 1, j), k = i, nend)
                end do
                write (iout, '('' '')')
            end do
        end do

        9999 if (timon) call timstop (ithandl)
        RETURN

        2000 FORMAT (/' DUMP OF RESULTS OF ', A20, &
                ' SCALE FACTOR = 10.0 EXP (', I3, ' ).'//)
        2030 FORMAT (6X, 20A6)
        2040 FORMAT (45X, A40)

    end subroutine write_concentrations_in_grid_layout

    subroutine fill_output_buffer_base_grid(outval, iopoin, nrvar, nocons, nopa, &
            nofun, nosfun, notot, conc, segfun, &
            func, param, cons, idt, itime, &
            volume, noseg, nosys, nodump, idump, &
            nx, ny, lgrid, igrid, bound, &
            noloc, proloc, nodef, defaul)

        ! Fills output buffer OUTVAL.
        !
        !     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
        !     ----    -----    ------     ------- -----------
        !     OUTVAL  REAL    NRVAR,*     OUTPUT  Values for vars on output grid
        !     IOPOIN  INTEGER       *     INPUT   Pointers to arrays for vars
        !     NRVAR   INTEGER       1     INPUT   Number of output vars
        !     NOCONS  INTEGER       1     INPUT   Number of constants used
        !     NOPA    INTEGER       1     INPUT   Number of parameters
        !     NOFUN   INTEGER       1     INPUT   Number of functions ( user )
        !     NOSFUN  INTEGER       1     INPUT   Number of segment functions
        !     NOTOT   INTEGER       1     INPUT   Total number of substances
        !     CONC    REAL   NOTOT,NOSEG  INPUT   Model concentrations
        !     SEGFUN  REAL   NOSEG,NOSFUN IN/OUT  Segment functions at ITIME
        !     FUNC    REAL          *     IN/OUT  Model functions at ITIME
        !     PARAM   REAL    NOPA,NOSEG  IN/OUT  Model parameters
        !     CONS    REAL          *     IN/OUT  Model constants
        !     IDT     INTEGER       1     INPUT   Simulation timestep
        !     ITIME   INTEGER       1     INPUT   Time in system clock units
        !     VOLUME  REAL      NOSEG     INPUT   Segment volumes
        !     NOSEG   INTEGER       1     INPUT   Nr. of computational elements
        !     NOSYS   INTEGER       1     INPUT   Number of active substances
        !     NODUMP  INTEGER       1     INPUT   number of dump locations
        !     IDUMP   INTEGER    NODUMP   INPUT   dump segment numbers
        !     NX      INTEGER       1     INPUT   Width of output grid
        !     NY      INTEGER       1     INPUT   Depth of output grid
        !     LGRID   INTEGER     NX*NY   INPUT   grid-layout
        !     IGRID   INTEGER       1     INPUT   Output grid indication
        !     BOUND   REAL     NOTOT*?    INPUT   boundary      values
        !     NOLOC   INTEGER       1     INPUT   Number of variables in PROLOC
        !     PARAM   REAL   NOLOC,NOSEG  INPUT   Parameters local in PROCES system
        !     NODEF   INTEGER       1     INPUT   Number of used defaults
        !     DEFAUL  REAL          *     INPUT   Default proces parameters

        integer(kind = int_wp) :: nrvar, nocons, nopa, nofun, nosfun, &
                notot, idt, itime, noseg, nosys, &
                nodump, nx, ny, igrid, noloc, &
                nodef
        integer(kind = int_wp) :: iopoin(*), idump(*), &
                lgrid(*)
        real(kind = real_wp) :: outval(*), conc(notot, *), &
                segfun(noseg, *), func(*), &
                param(*), cons(*), &
                volume(*), bound(*), &
                proloc(*), defaul(*)
        !
        !     local
        !
        integer(kind = int_wp), parameter :: igseg = 1, igmon = 2, iggrd = 3, igsub = 4
        real(kind = real_wp), parameter :: rmiss = -999.
        integer(kind = int_wp), parameter :: nopred = 6
        integer(kind = int_wp) :: iopa, iofunc, iosfun, ioconc, ioloc, &
                iodef, ip, icel, iseg, iocons, nocel, i, iicel, iip
        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("fill_output_buffer_base_grid", ithandl)
        !
        !     pointer offsets
        !
        iocons = nopred + 1
        iopa = iocons + nocons
        iofunc = iopa + nopa
        iosfun = iofunc + nofun
        ioconc = iosfun + nosfun
        ioloc = ioconc + notot
        iodef = ioloc + noloc
        !
        !     grid
        !
        if (igrid == igseg) then
            nocel = noseg
        elseif (igrid == igmon) then
            nocel = nodump
        elseif (igrid == iggrd) then
            nocel = nx * ny
        endif
        !
        !     fill outval
        !
        do icel = 1, nocel
            !
            !        what segment ?
            !
            if (igrid == igseg) then
                iseg = icel
            elseif (igrid == igmon) then
                iseg = idump(icel)
            elseif (igrid == iggrd) then
                iseg = lgrid(icel)
            endif

            do i = 1, nrvar
                iicel = (icel - 1) * nrvar + i
                ip = iopoin(i)
                !
                !           what value
                !
                if (iseg < 0) then
                    if (ip >= ioconc .and. ip < ioconc + nosys) then
                        iip = (-iseg - 1) * nosys + ip - ioconc + 1
                        outval(iicel) = bound(iip)
                    else
                        outval(iicel) = rmiss
                    endif
                elseif (iseg == 0) then
                    outval(iicel) = rmiss
                else
                    if (ip >= iodef) then
                        outval(iicel) = defaul(ip - iodef + 1)
                    elseif (ip >= ioloc) then
                        iip = (iseg - 1) * noloc + ip - ioloc + 1
                        outval(iicel) = proloc(iip)
                    elseif (ip >= ioconc) then
                        outval(iicel) = conc(ip - ioconc + 1, iseg)
                    elseif (ip >= iosfun) then
                        outval(iicel) = segfun(iseg, ip - iosfun + 1)
                    elseif (ip >= iofunc) then
                        outval(iicel) = func(ip - iofunc + 1)
                    elseif (ip >= iopa) then
                        iip = (iseg - 1) * nopa + ip - iopa + 1
                        outval(iicel) = param(iip)
                    elseif (ip >= iocons) then
                        outval(iicel) = cons(ip - iocons + 1)
                    elseif (ip == 3) then
                        outval(iicel) = real(idt)
                    elseif (ip == 2) then
                        outval(iicel) = real(itime)
                    elseif (ip == 1) then
                        outval(iicel) = volume(iseg)
                    elseif (ip <= 0) then
                        outval(iicel) = rmiss
                    endif
                endif
            end do
        end do

        if (timon) call timstop (ithandl)

    end subroutine fill_output_buffer_base_grid

    subroutine fill_transport_terms_transects(nosys, ndmpq, noraai, ntraaq, ioraai, nqraai, iqraai, iqdmp, dmpq, trraai)
        !! Fills transport terms for raaien


        !     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
        !     ----    -----    ------     ------- -----------
        !     NOSYS   INTEGER       1     INPUT   Total number of active substances
        !     NDMPQ   INTEGER       1     INPUT   Number of dump exchanges
        !     NORAAI  INTEGER       1     INPUT   Number of raaien
        !     NTRAAQ  INTEGER       1     INPUT   Total number of exch. in raaien
        !     IORAAI  INTEGER       *     INPUT   Output option for raai
        !     NQRAAI  INTEGER       *     INPUT   Number of exchanges in raai
        !     IQRAAI  INTEGER       *     INPUT   Exchanges in raai
        !     IQDMP   INTEGER       *     INPUT   Exchange to dumped exchange pointer
        !     DMPQ    REAL  NOSYS*NDMPQ*? INPUT   mass balance dumped exchange
        !     TRRAAI  REAL NOTOT*NDMPAR*6 IN/OUT  Cummulative transport over raai

        integer(kind = int_wp) :: nosys, ndmpq, noraai, ntraaq
        integer(kind = int_wp) :: ioraai(*), nqraai(*), iqraai(*), iqdmp(*)
        real(kind = real_wp) :: dmpq(nosys, ndmpq, *), trraai(nosys, *)

        integer(kind = int_wp) :: itel1, isys, iraai, nqc, integration_id, iqc, iq, ipq

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("fill_transport_terms_transects", ithandl)

        ! loop over the raaien
        itel1 = 0
        do iraai = 1, noraai

            ! the exchange contributes
            nqc = nqraai(iraai)
            integration_id = ioraai(iraai)
            do iqc = 1, nqc
                itel1 = itel1 + 1
                iq = iqraai(itel1)
                if (iq > 0) then
                    ipq = iqdmp(iq)
                    do isys = 1, nosys
                        if (integration_id == 1) then
                            trraai(isys, iraai) = trraai(isys, iraai) + dmpq(isys, ipq, 1) - dmpq(isys, ipq, 2)
                        elseif (integration_id == 2) then
                            trraai(isys, iraai) = trraai(isys, iraai) + dmpq(isys, ipq, 1)
                        elseif (integration_id == 3) then
                            trraai(isys, iraai) = trraai(isys, iraai) - dmpq(isys, ipq, 2)
                        endif
                    end do
                else
                    ipq = iqdmp(-iq)
                    do isys = 1, nosys
                        if (integration_id == 1) then
                            trraai(isys, iraai) = trraai(isys, iraai) - dmpq(isys, ipq, 1) + dmpq(isys, ipq, 2)
                        elseif (integration_id == 2) then
                            trraai(isys, iraai) = trraai(isys, iraai) + dmpq(isys, ipq, 2)
                        elseif (integration_id == 3) then
                            trraai(isys, iraai) = trraai(isys, iraai) - dmpq(isys, ipq, 1)
                        endif
                    end do
                endif
            end do
        end do

        if (timon) call timstop (ithandl)
    end subroutine fill_transport_terms_transects


    subroutine fill_output_buffer_sub_grid(outval, iopoin, nrvar, nocons, nopa, &
            nofun, nosfun, notot, conc, segfun, &
            func, param, cons, idt, itime, &
            volume, noseg, nosys, ndmpar, ipdmp, &
            bound, noloc, proloc, nodef, defaul, &
            ncout, ntdmpq, paname, sfname, funame, &
            danam)
        use m_array_manipulation, only: initialize_real_array
        use m_string_utils, only: index_in_array



        ! Fills output buffer OUTVAL on sub grid.

        !     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
        !     ----    -----    ------     ------- -----------
        !     OUTVAL  REAL  NCOUT+NRVAR,* OUTPUT  Values for vars on output grid
        !     IOPOIN  INTEGER       *     INPUT   Pointers to arrays for vars
        !     NRVAR   INTEGER       1     INPUT   Number of extra output vars
        !     NOCONS  INTEGER       1     INPUT   Number of constants used
        !     NOPA    INTEGER       1     INPUT   Number of parameters
        !     NOFUN   INTEGER       1     INPUT   Number of functions ( user )
        !     NOSFUN  INTEGER       1     INPUT   Number of segment functions
        !     NOTOT   INTEGER       1     INPUT   Total number of substances
        !     CONC    REAL   NOTOT,NOSEG  INPUT   Model concentrations
        !     SEGFUN  REAL   NOSEG,NOSFUN INPUT   Segment functions at ITIME
        !     FUNC    REAL          *     INPUT   Model functions at ITIME
        !     PARAM   REAL    NOPA,NOSEG  INPUT   Model parameters
        !     CONS    REAL          *     INPUT   Model constants
        !     IDT     INTEGER       1     INPUT   Simulation timestep
        !     ITIME   INTEGER       1     INPUT   Time in system clock units
        !     VOLUME  REAL      NOSEG     INPUT   Segment volumes
        !     NOSEG   INTEGER       1     INPUT   Nr. of computational elements
        !     NOSYS   INTEGER       1     INPUT   Number of active substances
        !     NDMPAR  INTEGER       1     INPUT   number of dump locations
        !     IPDMP   INTEGER       *     INPUT   pointer structure dump area's
        !     BOUND   REAL     NOTOT*?    INPUT   boundary      values
        !     NOLOC   INTEGER       1     INPUT   Number of variables in PROLOC
        !     PARAM   REAL   NOLOC,NOSEG  INPUT   Parameters local in PROCES system
        !     NODEF   INTEGER       1     INPUT   Number of used defaults
        !     DEFAUL  REAL          *     INPUT   Default proces parameters
        !     NCOUT   INTEGER       1     INPUT   number of conc in output
        !     NTDMPQ  INTEGER       1     INPUT   total number exchanges in dump area
        integer(kind = int_wp) :: nrvar, nocons, nopa, nofun, nosfun, &
                notot, idt, itime, noseg, nosys, &
                ndmpar, noloc, nodef, ncout, ntdmpq
        integer(kind = int_wp) :: iopoin(*), ipdmp(*)
        real(kind = real_wp) :: outval(*), conc(notot, *), &
                segfun(noseg, nosfun), func(*), &
                param (nopa, noseg), cons(*), &
                volume(*), bound(*), &
                proloc(*), defaul(*)
        character(20) paname(*), sfname(*)
        character(len = 20), intent(in) :: funame(*) ! function names
        character(len = 20), intent(in) :: danam(*)  ! dump area names
        !
        !     Local
        !
        real(kind = real_wp), parameter :: rmiss = -999.
        integer(kind = int_wp), parameter :: nopred = 6
        integer(kind = int_wp) :: iopa, iofunc, iosfun, ioconc, ioloc, &
                iodef, ip, ip1, ip2, itel2, &
                isys, ivar, idump, isc, iseg, &
                nsc, iofdmp, iocons, iip, iidump, &
                indx
        integer(kind = int_wp) :: ifun   ! index in function arrays
        real(kind = real_wp) :: hlpvar, hlpcum, valcum, valvar, srf, cumsrf
        logical     parm
        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("fill_output_buffer_sub_grid", ithandl)
        ! Pointer offsets
        iocons = nopred + 1
        iopa = iocons + nocons
        iofunc = iopa + nopa
        iosfun = iofunc + nofun
        ioconc = iosfun + nosfun
        ioloc = ioconc + notot
        iodef = ioloc + noloc
        !
        !     Zero the output buffer for it is used as accumulation variable.
        !
        call initialize_real_array(outval, (ncout + nrvar) * ndmpar)
        !
        !     Fill the output buffer OUTVAL
        !
        ip1 = ndmpar + ntdmpq
        itel2 = ndmpar + ntdmpq + ndmpar
        !
        !     Loop over the dump area's
        !
        do idump = 1, ndmpar
            !
            !        the segment contributes
            !
            nsc = ipdmp(ip1 + idump)
            iofdmp = (idump - 1) * (ncout + nrvar)
            !
            !        If one segment don't bother to calculate mean value
            !
            if (nsc == 1) then
                itel2 = itel2 + 1
                iseg = ipdmp(itel2)
                if (danam(idump)(1:6) == 'MOVING') then
                    do ifun = 1, nofun
                        if (danam(idump) == funame(ifun)) then
                            iseg = nint(func(ifun))
                        endif
                    enddo
                endif
                !
                !           The substances
                !
                if (ncout > 0) then
                    if (iseg < 0) then
                        do isys = 1, nosys
                            iidump = iofdmp + isys
                            iip = (-iseg - 1) * nosys + isys
                            outval(iidump) = bound(iip)
                        enddo
                        do isys = nosys + 1, notot
                            iidump = iofdmp + isys
                            outval(iidump) = rmiss
                        enddo
                    elseif (iseg == 0) then
                        do isys = 1, notot
                            iidump = iofdmp + isys
                            outval(iidump) = rmiss
                        enddo
                    else
                        do isys = 1, notot
                            iidump = iofdmp + isys
                            outval(iidump) = conc(isys, iseg)
                        enddo
                    endif
                endif
                !
                !           The extra variables
                !
                do ivar = 1, nrvar
                    ip = iopoin(ivar)
                    iidump = iofdmp + ncout + ivar
                    if (iseg < 0) then
                        if (ip >= ioconc .and. ip < ioconc + nosys) then
                            iip = (-iseg - 1) * nosys + ip - ioconc + 1
                            outval(iidump) = bound(iip)
                        else
                            outval(iidump) = rmiss
                        endif
                    elseif (iseg == 0) then
                        outval(iidump) = rmiss
                    else
                        if (ip >= iodef) then
                            outval(iidump) = defaul(ip - iodef + 1)
                        elseif (ip >= ioloc) then
                            iip = (iseg - 1) * noloc + ip - ioloc + 1
                            outval(iidump) = proloc(iip)
                        elseif (ip >= ioconc) then
                            outval(iidump) = conc(ip - ioconc + 1, iseg)
                        elseif (ip >= iosfun) then
                            outval(iidump) = segfun(iseg, ip - iosfun + 1)
                        elseif (ip >= iofunc) then
                            outval(iidump) = func(ip - iofunc + 1)
                        elseif (ip >= iopa) then
                            outval(iidump) = param(ip - iopa + 1, iseg)
                        elseif (ip >= iocons) then
                            outval(iidump) = cons(ip - iocons + 1)
                        elseif (ip == 3) then
                            outval(iidump) = real(idt)
                        elseif (ip == 2) then
                            outval(iidump) = real(itime)
                        elseif (ip == 1) then
                            outval(iidump) = volume(iseg)
                        elseif (ip <= 0) then
                            outval(iidump) = rmiss
                        endif
                    endif
                    !
                enddo
                !
            else
                !
                !           The substances ( if asked ) in one loop
                !
                if (ncout > 0) then
                    !
                    !              Zero the accummulative variables, OUTVAL already done.
                    !
                    hlpcum = 0.0
                    cumsrf = 0.0
                    if (nosys /= notot) then
                        cumsrf = 1.0
                        indx = index_in_array('SURF      ', paname(:nopa))
                        if  (indx > 0) then
                            cumsrf = 0.0
                            parm = .true.
                        else
                            indx = index_in_array('SURF      ', sfname(:nosfun))
                            if  (indx > 0) then
                                cumsrf = 0.0
                                parm = .false.
                            endif
                        endif
                    endif
                    !
                    do isc = 1, nsc
                        iseg = ipdmp(itel2 + isc)
                        !
                        !                 Accumulate VOLUME, substances in OUTVAL
                        !
                        if (iseg > 0) then
                            hlpvar = volume(iseg)
                            hlpcum = hlpcum + hlpvar
                            !
                            !                    All active substances weighted by volume
                            !
                            do isys = 1, nosys
                                iidump = iofdmp + isys
                                outval(iidump) = outval(iidump) + &
                                        conc(isys, iseg) * hlpvar
                            enddo

                            !       take care of mass/m2 for inactive substances if possible

                            if (nosys /= notot) then
                                if  (indx > 0) then
                                    if (parm) then
                                        srf = param (indx, iseg)
                                    else
                                        srf = segfun(iseg, indx)
                                    endif
                                    cumsrf = cumsrf + srf
                                    do isys = nosys + 1, notot
                                        iidump = iofdmp + isys
                                        outval(iidump) = outval(iidump) + conc(isys, iseg) * srf
                                    enddo
                                else
                                    do isys = nosys + 1, notot
                                        iidump = iofdmp + isys
                                        outval(iidump) = outval(iidump) + conc(isys, iseg)
                                    enddo
                                endif
                            endif
                        endif
                        !
                    enddo
                    !
                    !              Calculate mean for then active and inactive substances
                    !
                    if (abs(hlpcum) > 1.0e-20) then
                        do isys = 1, nosys
                            iidump = iofdmp + isys
                            outval(iidump) = outval(iidump) / hlpcum
                        enddo
                    endif
                    if (abs(cumsrf) > 1.0e-20) then
                        do isys = nosys + 1, notot
                            iidump = iofdmp + isys
                            outval(iidump) = outval(iidump) / cumsrf
                        enddo
                    endif
                    !
                endif
                !
                !           The extra output variables each in a seperate loop
                !
                do ivar = 1, nrvar
                    !
                    !              Accumulate
                    !
                    ip = iopoin(ivar)
                    ip2 = iopoin(nrvar + ivar)
                    valcum = 0.0
                    hlpcum = 0.0
                    do isc = 1, nsc
                        iseg = ipdmp(itel2 + isc)

                        ! The output variable
                        if (iseg > 0) then
                            ip = iopoin(ivar)
                            if (iseg < 0) then
                                if (ip>=ioconc .and. ip<ioconc + nosys) then
                                    iip = (-iseg - 1) * nosys + ip - ioconc + 1
                                    valvar = bound(iip)
                                else
                                    valvar = 0.0
                                endif
                            elseif (iseg == 0) then
                                valvar = 0.0
                            else
                                if (ip >= iodef) then
                                    valvar = defaul(ip - iodef + 1)
                                elseif (ip >= ioloc) then
                                    iip = (iseg - 1) * noloc + ip - ioloc + 1
                                    valvar = proloc(iip)
                                elseif (ip >= ioconc) then
                                    valvar = conc(ip - ioconc + 1, iseg)
                                elseif (ip >= iosfun) then
                                    valvar = segfun(iseg, ip - iosfun + 1)
                                elseif (ip >= iofunc) then
                                    valvar = func(ip - iofunc + 1)
                                elseif (ip >= iopa) then
                                    valvar = param(ip - iopa + 1, iseg)
                                elseif (ip >= iocons) then
                                    valvar = cons(ip - iocons + 1)
                                elseif (ip == 3) then
                                    valvar = real(idt)
                                elseif (ip == 2) then
                                    valvar = real(itime)
                                elseif (ip == 1) then
                                    valvar = volume(iseg)
                                elseif (ip <= 0) then
                                    valvar = rmiss
                                endif
                            endif
                            ! The weigth variable
                            if (iseg < 0) then
                                hlpvar = 1.0
                            elseif (iseg == 0) then
                                hlpvar = 0.0
                            else
                                if (ip2 >= iodef) then
                                    hlpvar = defaul(ip2 - iodef + 1)
                                elseif (ip2 >= ioloc) then
                                    iip = (iseg - 1) * noloc + ip2 - ioloc + 1
                                    hlpvar = proloc(iip)
                                elseif (ip2 >= ioconc) then
                                    hlpvar = conc(ip2 - ioconc + 1, iseg)
                                elseif (ip2 >= iosfun) then
                                    hlpvar = segfun(iseg, ip2 - iosfun + 1)
                                elseif (ip2 >= iofunc) then
                                    hlpvar = func(ip2 - iofunc + 1)
                                elseif (ip2 >= iopa) then
                                    hlpvar = param(ip2 - iopa + 1, iseg)
                                elseif (ip2 >= iocons) then
                                    hlpvar = cons(ip2 - iocons + 1)
                                elseif (ip2 == 3) then
                                    hlpvar = real(idt)
                                elseif (ip2 == 2) then
                                    hlpvar = real(itime)
                                elseif (ip2 == 1) then
                                    hlpvar = volume(iseg)
                                elseif (ip2 == 0) then
                                    hlpvar = 0.0
                                elseif (ip2 < 0) then
                                    hlpvar = 1.
                                endif
                            endif

                            if (ip2 == 0) then
                                valcum = valcum + valvar
                            else
                                valcum = valcum + valvar * hlpvar
                                hlpcum = hlpcum + hlpvar
                            endif
                        endif

                    enddo

                    ! Calculate mean , HLPCUM = 0.0 has a double meaning
                    ! 1. only accumulation, 2. no divide by zero HLPCUM

                    iidump = iofdmp + ncout + ivar

                    if (abs(hlpcum) > 1.0e-20) then
                        outval(iidump) = valcum / hlpcum
                    else
                        outval(iidump) = valcum
                    endif

                enddo

                itel2 = itel2 + nsc
            endif

        enddo

        if (timon) call timstop (ithandl)

    end subroutine fill_output_buffer_sub_grid

    subroutine fill_dump_areas_balances(NOTOT, NOSYS, NOFLUX, NDMPAR, NDMPQ, &
            NDMPS, NTDMPQ, IQDMP, ISDMP, IPDMP, &
            DMPQ, MASS, DMPS, FLXDMP, ASMASS, &
            FLXINT)

        ! Fills balances for sub-area's


        !     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
        !     ----    -----    ------     ------- -----------
        !     NOTOT   INTEGER       1     INPUT   Total number of substances
        !     NOSYS   INTEGER       1     INPUT   Total number of active substances
        !     NOFLUX  INTEGER       1     INPUT   Nr. of fluxes
        !     NDMPAR  INTEGER       1     INPUT   Number of dump areas
        !     NDMPQ   INTEGER       1     INPUT   Number of dump exchanges
        !     NDMPS   INTEGER       1     INPUT   Number of dump segments
        !     NTDMPQ  INTEGER       1     INPUT   total number exchanges in dump area
        !     IQDMP   INTEGER       *     INPUT   Exchange to dumped exchange pointer
        !     ISDMP   INTEGER       *     INPUT   Segment to dumped segment pointer
        !     IPDMP   INTEGER       *     INPUT   pointer structure dump area's
        !     DMPQ    REAL  NOSYS*NDMPQ*? INPUT   mass balance dumped exchange
        !     DMPS    REAL  NOTOT*NDMPS*? INPUT   mass balance dumped segments
        !     FLXDMP  REAL  NOFLUX*NDMPS  INPUT   Integrated fluxes
        !     ASMASS  REAL NOTOT*NDMPAR*6 OUTPUT  Mass balance terms
        !     FLXINT  REAL  NOFLUX*NDMPAR OUTPUT  Integrated fluxes

        use timers

        INTEGER(kind = int_wp) :: NOTOT, NOSYS, NOFLUX, NDMPAR, NDMPQ, &
                NDMPS, NTDMPQ
        INTEGER(kind = int_wp) :: IQDMP(*), ISDMP(*), &
                IPDMP(*)
        REAL(kind = real_wp) :: DMPQ(NOSYS, NDMPQ, *), MASS(NOTOT, *), &
                DMPS(NOTOT, NDMPS, *), FLXDMP(NOFLUX, *), &
                ASMASS(NOTOT, NDMPAR, *), FLXINT(NOFLUX, *)

        INTEGER(kind = int_wp) :: ITEL1, ITEL2, IP1, IDUMP, NQC, &
                IQC, IQ, IPQ, ISYS, NSC, &
                ISC, ISEG, IPS
        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("fill_dump_areas_balances", ithandl)

        !
        !     Loop over the dump area's
        !
        ITEL1 = NDMPAR
        IP1 = NDMPAR + NTDMPQ
        ITEL2 = NDMPAR + NTDMPQ + NDMPAR
        DO IDUMP = 1, NDMPAR
            !
            !        the exchange contributes
            !
            NQC = IPDMP(IDUMP)
            DO IQC = 1, NQC
                ITEL1 = ITEL1 + 1
                IQ = IPDMP(ITEL1)
                IF (IQ > 0) THEN
                    IPQ = IQDMP(IQ)
                    DO ISYS = 1, NOSYS
                        ASMASS(ISYS, IDUMP, 5) = ASMASS(ISYS, IDUMP, 5) + &
                                DMPQ(ISYS, IPQ, 2)
                        ASMASS(ISYS, IDUMP, 6) = ASMASS(ISYS, IDUMP, 6) + &
                                DMPQ(ISYS, IPQ, 1)
                    end do
                ELSE
                    IPQ = IQDMP(-IQ)
                    DO ISYS = 1, NOSYS
                        ASMASS(ISYS, IDUMP, 5) = ASMASS(ISYS, IDUMP, 5) + &
                                DMPQ(ISYS, IPQ, 1)
                        ASMASS(ISYS, IDUMP, 6) = ASMASS(ISYS, IDUMP, 6) + &
                                DMPQ(ISYS, IPQ, 2)
                    end do
                ENDIF
            end do
            !
            !        the segment contributes
            !
            DO ISYS = 1, NOTOT
                ASMASS(ISYS, IDUMP, 1) = 0.0
            ENDDO
            NSC = IPDMP(IP1 + IDUMP)
            DO ISC = 1, NSC
                ITEL2 = ITEL2 + 1
                ISEG = IPDMP(ITEL2)
                IF (ISEG > 0) THEN
                    IPS = ISDMP(ISEG)
                    DO ISYS = 1, NOTOT
                        ASMASS(ISYS, IDUMP, 1) = ASMASS(ISYS, IDUMP, 1) + &
                                MASS(ISYS, ISEG)
                        ASMASS(ISYS, IDUMP, 2) = ASMASS(ISYS, IDUMP, 2) + &
                                DMPS(ISYS, IPS, 1)
                        ASMASS(ISYS, IDUMP, 3) = ASMASS(ISYS, IDUMP, 3) + &
                                DMPS(ISYS, IPS, 2)
                        ASMASS(ISYS, IDUMP, 4) = ASMASS(ISYS, IDUMP, 4) + &
                                DMPS(ISYS, IPS, 3)
                    end do
                ENDIF

            end do

        end do

        if (timon) call timstop (ithandl)

    end subroutine fill_dump_areas_balances

    subroutine fill_transect_output_buffer(OUTVAL, NRVAR, TRRAAI, NORAAI, NOSYS)

        !  Fills output buffer OUTVAL for raaien

        !
        !     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
        !     ----    -----    ------     ------- -----------
        !     OUTVAL  REAL    NRVAR,*     OUTPUT  Values for vars on output grid
        !     NRVAR   INTEGER       1     INPUT   Number of output vars
        !     TRRAAI  REAL    NOSYS,*     INPUT   Tranport over raai for active substanc
        !     NORAAI  INTEGER       1     INPUT   Number of raaien
        !     NOSYS   INTEGER       1     INPUT   Number of parameters in TRRAAI

        use timers

        INTEGER(kind = int_wp) :: NRVAR, NORAAI, NOSYS
        REAL(kind = real_wp) :: OUTVAL(NRVAR, *), TRRAAI(NOSYS, *)

        integer(kind = int_wp) :: iraai, isys
        real(kind = real_wp), PARAMETER :: RMISS = -999.
        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("fill_transect_output_buffer", ithandl)

        ! Copy values into output buffer
        DO IRAAI = 1, NORAAI
            DO ISYS = 1, NOSYS
                OUTVAL(ISYS, IRAAI) = TRRAAI(ISYS, IRAAI)
            end do
            DO ISYS = NOSYS + 1, NRVAR
                OUTVAL(ISYS, IRAAI) = RMISS
            end do
        end do

        if (timon) call timstop (ithandl)

    END SUBROUTINE fill_transect_output_buffer

    subroutine update_base_grid_local_array(IOPOIN, NRVAR, NOCONS, NOPA, NOFUN, &
            nosfun, notot, noseg, noloc, nogrid, &
            novar, vararr, varidx, vartda, vardag, &
            arrknd, arrpoi, arrdm1, arrdm2, vgrset, &
            grdnos, grdseg, a)
        ! Sets all variable from the LOCAL array used for output actual for the base grid.
        ! (ouput always uses the value from base grid)


        !
        !     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
        !     ----    -----    ------     ------- -----------
        !     IOPOIN  INTEGER       *     INPUT   Pointers to arrays for vars
        !     NRVAR   INTEGER       1     INPUT   Number of output vars
        !     NOCONS  INTEGER       1     INPUT   Number of constants used
        !     NOPA    INTEGER       1     INPUT   Number of parameters
        !     NOFUN   INTEGER       1     INPUT   Number of functions ( user )
        !     NOSFUN  INTEGER       1     INPUT   Number of segment functions
        !     NOTOT   INTEGER       1     INPUT   Total number of substances
        !     NOSEG   INTEGER       1     INPUT   Nr. of computational elements
        !     NOLOC   INTEGER       1     INPUT   Number of variables in PROLOC
        !     NOGRID  INTEGER       1     INPUT   Number of grids
        !     NOVAR   INTEGER       1     INPUT   Number of variables
        !     VARARR  INTEGER   NOVAR     INPUT   Variable array number
        !     VARIDX  INTEGER   NOVAR     INPUT   Variable index in array
        !     VARTDA  INTEGER   NOVAR     INPUT   Type of disaggregation
        !     VARDAG  INTEGER   NOVAR     INPUT   Variable disaggr. weight var.
        !     ARRKND  INTEGER   NOARR     INPUT   Kind of array
        !     ARRPOI  INTEGER   NOARR     INPUT   Array pointer in A
        !     ARRDM1  INTEGER   NOARR     INPUT   First dimension
        !     ARRDM2  INTEGER   NOARR     INPUT   Second dimension
        !     VGRSET  INTEGER   NOVAR,*   IN/OUT  Actual indication
        !     GRDNOS  INTEGER   NOGRID    INPUT   Number of segments in grid
        !     GRDSEG  INTEGER   NOGRID    INPUT   Segment pointering
        !     A       REAL      *         IN/OUT  Real array work space

        use m_dhgvar
        use m_array_manipulation, only: set_array_parameters
        use timers
        use aggregation, only: resample_v2

        integer(kind = int_wp) :: nrvar, nocons, nopa, nofun, nosfun, &
                notot, noseg, noloc, nogrid, novar, &
                nototo, nototi, noseg2, nopred, i, ix_hlp, &
                iv_idx, iv_hlp, iv_da, ivar, isyso, isysi, &
                nototw, ix_da, isysw, isysh, ip_hlp, ip_da, &
                iparw, ip_arr, ip_aro, ipari, iocons, iloc, &
                ik_hlp, nototh, iswcum, ip_arw, ip_ari, ip_arh, &
                ik_da, igrid, idim2, idim1, idatyp, id2_da, id2hlp, &
                id1_da, id1hlp, ia_loc, ia_hlp, ia_da, iarr, iarknd

        integer(kind = int_wp) :: iopoin(nrvar), vararr(novar), &
                varidx(novar), vartda(novar), &
                vardag(novar), arrknd(*), &
                arrpoi(*), arrdm1(*), &
                arrdm2(*), vgrset(novar, *), &
                grdnos(nogrid), grdseg(noseg, nogrid)
        real(kind = real_wp) :: a(*)
        !
        !     local
        !
        parameter (nopred = 6)
        integer(kind = int_wp) :: iopa, iofunc, iosfun, ioconc, ioloc, &
                iodef, ip
        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("update_base_grid_local_array", ithandl)
        !
        !     if no locals get out of here
        !
        if (noloc == 0) return
        !
        !     pointer offsets
        !
        iocons = nopred + 1
        iopa = iocons + nocons
        iofunc = iopa + nopa
        iosfun = iofunc + nofun
        ioconc = iosfun + nosfun
        ioloc = ioconc + notot
        iodef = ioloc + noloc
        !
        ia_loc = 33
        ix_hlp = 1
        ia_hlp = 33
        call dhgvar(ia_hlp, ix_hlp, iv_hlp)
        ik_hlp = arrknd(ia_hlp)
        ip_hlp = arrpoi(ia_hlp)
        id1hlp = arrdm1(ia_hlp)
        id2hlp = arrdm2(ia_hlp)
        !
        do i = 1, nrvar
            ip = iopoin(i)
            !
            !        is it a local value
            !
            if (ip < iodef .and. ip >= ioloc) then
                !
                !           get variable number
                !
                iloc = ip - ioloc + 1
                call dhgvar(ia_loc, iloc, ivar)
                !
                !           check is variable is active for base grid
                !
                if (vgrset(ivar, 1) == 0) then
                    !
                    iarr = ia_loc
                    iv_idx = varidx(ivar)
                    iarknd = arrknd(iarr)
                    ip_arr = arrpoi(iarr)
                    idim1 = arrdm1(iarr)
                    idim2 = arrdm2(iarr)
                    !
                    !              set variable
                    !
                    do igrid = 2, nogrid
                        if (vgrset(ivar, igrid) == 1) then
                            noseg2 = grdnos(igrid)
                            !
                            !                    determine characteristics of variable
                            !
                            call set_array_parameters(ivar, iarr, &
                                    iarknd, iv_idx, &
                                    idim1, idim2, &
                                    ip_arr, igrid, &
                                    isysi, nototi, &
                                    ip_ari)
                            call set_array_parameters(ivar, iarr, &
                                    iarknd, iv_idx, &
                                    idim1, idim2, &
                                    ip_arr, 1, &
                                    isyso, nototo, &
                                    ip_aro)
                            !
                            !                    determine characteristics of weight variable
                            !                    ( don't mind if this one is actuel ? )
                            !
                            idatyp = vartda(ivar)
                            if (idatyp == 2) then
                                iv_da = vardag(ivar)
                                ia_da = vararr(iv_da)
                                ik_da = arrknd(ia_da)
                                if (ik_da == 1) then
                                    !
                                    !                          not variable in space use help var
                                    !
                                    idatyp = 3
                                    iv_da = iv_hlp
                                    ia_da = vararr(iv_da)
                                    ik_da = arrknd(ia_da)
                                endif
                                ix_da = varidx(iv_da)
                                ip_da = arrpoi(ia_da)
                                id1_da = arrdm1(ia_da)
                                id2_da = arrdm2(ia_da)
                                call set_array_parameters(iv_da, ia_da, &
                                        ik_da, ix_da, &
                                        id1_da, id2_da, &
                                        ip_da, 1, &
                                        isysw, nototw, &
                                        ip_arw)
                                call set_array_parameters(iv_hlp, ia_hlp, &
                                        ik_hlp, ix_hlp, &
                                        id1hlp, id2hlp, &
                                        ip_hlp, igrid, &
                                        isysh, nototh, &
                                        ip_arh)
                            elseif (idatyp == 3) then
                                iv_da = iv_hlp
                                ia_da = vararr(iv_da)
                                ik_da = arrknd(ia_da)
                                ix_da = varidx(iv_da)
                                ip_da = arrpoi(ia_da)
                                id1_da = arrdm1(ia_da)
                                id2_da = arrdm2(ia_da)
                                call set_array_parameters(iv_da, ia_da, &
                                        ik_da, ix_da, &
                                        id1_da, id2_da, &
                                        ip_da, 1, &
                                        isysw, nototw, &
                                        ip_arw)
                                call set_array_parameters(iv_hlp, ia_hlp, &
                                        ik_hlp, ix_hlp, &
                                        id1hlp, id2hlp, &
                                        ip_hlp, igrid, &
                                        isysh, nototh, &
                                        ip_arh)
                            else
                                !
                                !                       weight and help array's dummy's
                                !                       so set to the variable itself
                                !
                                isysw = isyso
                                isysh = isysi
                                nototw = nototo
                                nototh = nototi
                                ip_arw = ip_aro
                                ip_arh = ip_ari
                                !
                            endif
                            !
                            iswcum = 0
                            call resample_v2(noseg, noseg2, &
                                    nototi, nototw, &
                                    nototh, nototo, &
                                    isysi, isysw, &
                                    isysh, isyso, &
                                    grdseg(1, igrid), idatyp, &
                                    a(ip_ari), a(ip_arw), &
                                    iswcum, a(ip_arh), &
                                    a(ip_aro))
                            vgrset(ivar, 1) = 1
                        endif
                    enddo

                endif

            endif

        enddo

        if (timon) call timstop (ithandl)

    end subroutine update_base_grid_local_array

    subroutine calculate_balance_terms(NOTOT, NOFLUX, NDMPAR, NOBALT, STOCHI, &
            FLXINT, ASMASS, BALINT)

        ! Makes BALINT from FLXINT and STOCHI

        !     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
        !     ----    -----    ------     ------- -----------
        !     NOTOT   INTEGER       1     INPUT   Total number of substances
        !     NOFLUX  INTEGER       1     INPUT   Nr. of fluxes
        !     NDMPAR  INTEGER       1     INPUT   Nr. of dump areas
        !     NOBALT  INTEGER       1     INPUT   Nr. of balance terms total
        !     STOCHI  REAL   NOTOT*NOFLUX INPUT   Proces stochiometry
        !     FLXINT  REAL  NOFLUX*NDMPAR INPUT   Accumulated fluxes
        !     ASMASS  REAL NOTOT*NDMPAR*6 INPUT   Mass balance terms
        !     BALINT  REAL  NOBALT*NDMPAR OUTPUT  Balance terms

        use m_logger_helper, only: stop_with_error, get_log_unit_number
        use timers

        INTEGER(kind = int_wp) :: NOTOT, NOFLUX, NDMPAR, NOBALT
        REAL(kind = real_wp) :: STOCHI(NOTOT, NOFLUX), FLXINT(NOFLUX, NDMPAR), &
                ASMASS(NOTOT, NDMPAR, 6), BALINT(NOBALT, NDMPAR)

        ! local
        integer(kind = int_wp) :: ibalt, isys, i, idmp, iflx, lurep
        real(kind = real_wp) :: st
        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("calculate_balance_terms", ithandl)
        !
        !     We construeren nu de BALINT's
        !
        IBALT = 0
        DO ISYS = 1, NOTOT
            DO I = 1, 4
                IBALT = IBALT + 1
                IF (I == 1 .OR. I == 3) THEN
                    DO IDMP = 1, NDMPAR
                        BALINT(IBALT, IDMP) = ASMASS(ISYS, IDMP, I + 2)
                    ENDDO
                ELSE
                    DO IDMP = 1, NDMPAR
                        BALINT(IBALT, IDMP) = -ASMASS(ISYS, IDMP, I + 2)
                    ENDDO
                ENDIF
            ENDDO
            DO IFLX = 1, NOFLUX
                ST = STOCHI(ISYS, IFLX)
                IF (ABS(ST) > 1.E-20) THEN
                    IBALT = IBALT + 1
                    IF (IBALT > NOBALT) THEN
                        CALL get_log_unit_number(LUREP)
                        WRITE(LUREP, *) 'ERROR, INTERNAL calculate_balance_terms'
                        WRITE(*, *)     'ERROR, INTERNAL calculate_balance_terms'
                        CALL stop_with_error()
                    ENDIF
                    DO IDMP = 1, NDMPAR
                        BALINT(IBALT, IDMP) = FLXINT(IFLX, IDMP) * ST
                    ENDDO
                ENDIF
            ENDDO
        ENDDO

        if (timon) call timstop (ithandl)

    end subroutine calculate_balance_terms

    subroutine write_balance_history_output(balance_file_unit, simulation_time, model_name, num_substances, &
            num_fluxes, substances_names, num_dump_segments, monitoring_station_names, mass_balance_terms, &
            integrated_fluxes, num_extra_variables, extra_variables, initialize_file)
        ! Writes balance output

        use timers

        integer(kind = int_wp), intent(in) :: balance_file_unit     !! balance_file_unit
        integer(kind = int_wp), intent(in) :: simulation_time
        integer(kind = int_wp), intent(in) :: num_fluxes
        integer(kind = int_wp) :: initialize_file
        integer(kind = int_wp), intent(in) :: num_substances        !! Total number of substances
        integer(kind = int_wp), intent(in) :: num_dump_segments
        integer(kind = int_wp), intent(in) :: num_extra_variables
        character(len = 40), intent(in) :: model_name(4)

        real(kind = real_wp), intent(in) :: mass_balance_terms(num_substances, num_dump_segments, 6)
        real(kind = real_wp), intent(in) :: integrated_fluxes(num_fluxes, num_dump_segments)
        real(kind = real_wp), intent(in) :: extra_variables(num_extra_variables, num_dump_segments)
        character(len = 20), intent(in) :: substances_names(*), monitoring_station_names(*)

        integer(kind = int_wp) :: j, i, k, isys, iflx, ihlp
        integer(kind = int_wp) :: ithandl = 0
        integer(kind = int_wp) :: nopout
        if (timon) call timstrt ("write_balance_history_output", ithandl)

        ! Initialize file
        if (initialize_file == 1) then
            initialize_file = 0

            ! write header
            write (balance_file_unit) (model_name(i), i = 1, 4)
            nopout = 6 * num_substances + num_fluxes + 2
            write (balance_file_unit) nopout, num_dump_segments, num_substances
            write (balance_file_unit) (substances_names(i), i = 1, num_substances)
            write (balance_file_unit) (monitoring_station_names(i), i = 1, num_dump_segments)
        endif

        ! Perform output
        write (balance_file_unit) simulation_time, (&
                ((mass_balance_terms(isys, j, k), k = 1, 6), isys = 1, num_substances), &
                (integrated_fluxes(iflx, j), iflx = 1, num_fluxes), &
                (extra_variables(ihlp, j), ihlp = 1, 2), &
                j = 1, num_dump_segments)

        if (timon) call timstop (ithandl)

    end subroutine write_balance_history_output

end module m_prepare_output_data
