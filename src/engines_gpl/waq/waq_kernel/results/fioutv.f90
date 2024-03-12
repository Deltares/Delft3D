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
module m_fioutv
    use m_waq_precision

    implicit none

contains


    SUBROUTINE FIOUTV (OUTVAL, IOPOIN, NRVAR, NOCONS, NOPA, &
            NOFUN, NOSFUN, NOTOT, CONC, SEGFUN, &
            FUNC, PARAM, CONS, IDT, ITIME, &
            VOLUME, NOSEG, NOSYS, NODUMP, IDUMP, &
            NX, NY, LGRID, IGRID, BOUND, &
            NOLOC, PROLOC, NODEF, DEFAUL)
        !
        !     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
        !
        !     CREATED:            : may 1993 by Jan van Beek
        !
        !     FUNCTION            : Fills output buffer OUTVAL.
        !
        !     SUBROUTINES CALLED  : -
        !
        !     FILES               : -
        !
        !     PARAMETERS          : 29
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
        !
        !     Declaration of arguments
        !
        use timers

        INTEGER(kind = int_wp) :: NRVAR, NOCONS, NOPA, NOFUN, NOSFUN, &
                NOTOT, IDT, ITIME, NOSEG, NOSYS, &
                NODUMP, NX, NY, IGRID, NOLOC, &
                NODEF
        INTEGER(kind = int_wp) :: IOPOIN(*), IDUMP(*), &
                LGRID(*)
        REAL(kind = real_wp) :: OUTVAL(*), CONC(NOTOT, *), &
                SEGFUN(NOSEG, *), FUNC(*), &
                PARAM(*), CONS(*), &
                VOLUME(*), BOUND(*), &
                PROLOC(*), DEFAUL(*)
        !
        !     Local
        !
        integer(kind = int_wp), PARAMETER :: IGSEG = 1, IGMON = 2, IGGRD = 3, IGSUB = 4
        real(kind = real_wp), PARAMETER :: RMISS = -999.
        integer(kind = int_wp), PARAMETER :: NOPRED = 6
        INTEGER(kind = int_wp) :: IOPA, IOFUNC, IOSFUN, IOCONC, IOLOC, &
                IODEF, IP, icel, iseg, iocons, nocel, i, iicel, iip
        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("fioutv", ithandl)
        !
        !     Pointer offsets
        !
        IOCONS = NOPRED + 1
        IOPA = IOCONS + NOCONS
        IOFUNC = IOPA + NOPA
        IOSFUN = IOFUNC + NOFUN
        IOCONC = IOSFUN + NOSFUN
        IOLOC = IOCONC + NOTOT
        IODEF = IOLOC + NOLOC
        !
        !     GRID
        !
        IF (IGRID == IGSEG) THEN
            NOCEL = NOSEG
        ELSEIF (IGRID == IGMON) THEN
            NOCEL = NODUMP
        ELSEIF (IGRID == IGGRD) THEN
            NOCEL = NX * NY
        ENDIF
        !
        !     FILL OUTVAL
        !
        DO ICEL = 1, NOCEL
            !
            !        What segment ?
            !
            IF (IGRID == IGSEG) THEN
                ISEG = ICEL
            ELSEIF (IGRID == IGMON) THEN
                ISEG = IDUMP(ICEL)
            ELSEIF (IGRID == IGGRD) THEN
                ISEG = LGRID(ICEL)
            ENDIF

            DO I = 1, NRVAR
                IICEL = (ICEL - 1) * NRVAR + I
                IP = IOPOIN(I)
                !
                !           What value
                !
                IF (ISEG < 0) THEN
                    IF (IP >= IOCONC .AND. IP < IOCONC + NOSYS) THEN
                        IIP = (-ISEG - 1) * NOSYS + IP - IOCONC + 1
                        OUTVAL(IICEL) = BOUND(IIP)
                    ELSE
                        OUTVAL(IICEL) = RMISS
                    ENDIF
                ELSEIF (ISEG == 0) THEN
                    OUTVAL(IICEL) = RMISS
                ELSE
                    IF (IP >= IODEF) THEN
                        OUTVAL(IICEL) = DEFAUL(IP - IODEF + 1)
                    ELSEIF (IP >= IOLOC) THEN
                        IIP = (ISEG - 1) * NOLOC + IP - IOLOC + 1
                        OUTVAL(IICEL) = PROLOC(IIP)
                    ELSEIF (IP >= IOCONC) THEN
                        OUTVAL(IICEL) = CONC(IP - IOCONC + 1, ISEG)
                    ELSEIF (IP >= IOSFUN) THEN
                        OUTVAL(IICEL) = SEGFUN(ISEG, IP - IOSFUN + 1)
                    ELSEIF (IP >= IOFUNC) THEN
                        OUTVAL(IICEL) = FUNC(IP - IOFUNC + 1)
                    ELSEIF (IP >= IOPA) THEN
                        IIP = (ISEG - 1) * NOPA + IP - IOPA + 1
                        OUTVAL(IICEL) = PARAM(IIP)
                    ELSEIF (IP >= IOCONS) THEN
                        OUTVAL(IICEL) = CONS(IP - IOCONS + 1)
                    ELSEIF (IP == 3) THEN
                        OUTVAL(IICEL) = REAL(IDT)
                    ELSEIF (IP == 2) THEN
                        OUTVAL(IICEL) = REAL(ITIME)
                    ELSEIF (IP == 1) THEN
                        OUTVAL(IICEL) = VOLUME(ISEG)
                    ELSEIF (IP <= 0) THEN
                        OUTVAL(IICEL) = RMISS
                    ENDIF
                ENDIF
            end do
        end do
        !
        if (timon) call timstop (ithandl)
        RETURN
    END

end module m_fioutv
