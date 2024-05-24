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
module m_fill_output_arrays
    use m_waq_precision

    implicit none

    private
    public :: writes_concentrations_in_grid_layout, store_variables_in_output_grid

contains


    subroutine writes_concentrations_in_grid_layout(iout, lchout, itime, mname, nx, &
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
        use timers

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
        if (timon) call timstrt ("writes_concentrations_in_grid_layout", ithandl)

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
            call report_time (iout, itime, isflag, -999.)
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

    end subroutine writes_concentrations_in_grid_layout

    subroutine store_variables_in_output_grid(outval, iopoin, nrvar, nocons, nopa, &
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

        use timers

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
        if (timon) call timstrt ("store_variables_in_output_grid", ithandl)
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

    end subroutine store_variables_in_output_grid

end module m_fill_output_arrays
