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
module m_outdmp
    use m_waq_precision

    implicit none

contains


    SUBROUTINE OUTDMP (IOUT, LCHOUT, ITIME, MNAME, NX, &
            NY, LGRID, CGRID, NOTOT, NOSYS, &
            SNAME, CONC, BOUND, NOTOT2, SYNAM2, &
            CONC2, IP, ISFLAG, INIOUT)
        !
        !
        !     Deltares        SECTOR WATERRESOURCES AND ENVIRONMENT
        !
        !     CREATED            : may  1993  BY Jan van Beek
        !                          from DLWQ11 : june 1988  BY M.E.Sileon
        !
        !     FUNCTION           : Writes concentrations in grid-layout and
        !                          writes the result in IOUT .
        !
        !     LOGICAL UNITS      : IOUT = number of dump file
        !
        !     SUBROUTINES CALLED : none
        !
        !     PARAMETERS         : 19
        !
        !     NAME    KIND     LENGTH      FUNCT.  DESCRIPTION
        !     ---------------------------------------------------------
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
        !
        !     Declaration of arguments
        !
        use date_time_utils, only : report_time
        use timers

        INTEGER(kind = int_wp) :: IOUT, ITIME, NX, NY, NOTOT, &
                NOSYS, ISFLAG, NOTOT2, INIOUT
        INTEGER(kind = int_wp) :: LGRID(*), IP(*)
        REAL(kind = real_wp) :: CONC(NOTOT, *), BOUND(NOSYS, *), &
                CONC2 (*)
        CHARACTER*6   CGRID (20, *)
        CHARACTER*20  SNAME (*), SYNAM2(*)
        CHARACTER*40  MNAME (*)
        CHARACTER*(*) LCHOUT
        !
        !     Local declaration
        !
        integer(kind = int_wp) :: itot, i, i1, i2, i3, k, j, iscale, factor, nend
        real(kind = real_wp) :: cmax, c
        real(kind = real_wp), PARAMETER :: RMISS = -999.
        CHARACTER*6   POINT, PADDER
        DATA          POINT / '  .   ' /
        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("outdmp", ithandl)
        !
        IF (NX * NY == 0) goto 9999  !   RETURN
        !
        !         initialise the paging
        !
        IF (IP(3) == 0) THEN
            IP(3) = MAX(1, IP(1) / (7 + (NY + 5) * ((NX + IP(2) - 1) / IP(2))))
            IP(4) = 0
        ENDIF
        !
        !      repeat output for every substance
        !
        DO ITOT = 1, NOTOT
            !
            !      Calculate maximum concentration of displayed segments
            !
            CMAX = 0.0
            DO I1 = 1, NY
                DO I2 = 1, NX
                    I3 = LGRID ((I1 - 1) * NX + I2)
                    IF (I3 > 0) CMAX = AMAX1 (CMAX, CONC (ITOT, I3))
                    IF (I3 < 0 .AND. ITOT <= NOSYS) &
                            CMAX = AMAX1 (CMAX, BOUND(ITOT, -I3))
                end do
            end do
            !
            !      Calculate scale factor
            !
            IF (CMAX <= 0.0) THEN
                ISCALE = 0
            ELSE IF (CMAX >= 0.5) THEN
                ISCALE = AINT(ALOG10(CMAX) + 2.2E-5)
            ELSE
                ISCALE = -AINT(-ALOG10(CMAX) - 2.2E-5) - 1
            ENDIF
            !
            !         start printing
            !
            IF (MOD(IP(4), IP(3)) == 0) THEN
                WRITE (IOUT, '('' '')')
                WRITE (IOUT, 2040) (MNAME(K), K = 1, 4)
            ENDIF
            IP(4) = IP(4) + 1
            CALL report_time (IOUT, ITIME, ISFLAG, -999.)
            WRITE(IOUT, 2000) SNAME(ITOT), ISCALE
            !
            !      Put concentration values in grid layout
            !
            FACTOR = 10.0**ISCALE
            DO I = 1, NX, IP(2)
                DO J = 1, NY
                    NEND = MIN (NX, I + IP(2) - 1)
                    DO K = I, NEND
                        CGRID (K - I + 1, J) = POINT
                        I3 = LGRID ((J - 1) * NX + K)
                        IF (I3 > 0) THEN
                            WRITE (PADDER, '(F6.3)')   CONC (ITOT, I3) / FACTOR
                            CGRID(K - I + 1, J) = PADDER
                        ENDIF
                        IF (I3 < 0 .AND. ITOT <= NOSYS) THEN

                            WRITE (PADDER, '(F6.3)')   BOUND(ITOT, -I3) / FACTOR
                            CGRID(K - I + 1, J) = PADDER
                        ENDIF
                    end do
                    WRITE (IOUT, 2030) (CGRID (K - I + 1, J), K = I, NEND)
                end do
                WRITE (IOUT, '('' '')')
            end do
            !
        end do
        !
        !      repeat output for extra substance
        !
        DO ITOT = 1, NOTOT2
            !
            !      Calculate maximum concentration of displayed segments
            !
            CMAX = 0.0
            DO I1 = 1, NY
                DO I2 = 1, NX
                    CMAX = AMAX1 (CMAX, CONC2(ITOT + (I2 * I1 - 1) * NOTOT2))
                end do
            end do
            !
            !      Calculate scale factor
            !
            IF (CMAX <= 0.0) THEN
                ISCALE = 0
            ELSE IF (CMAX >= 0.5) THEN
                ISCALE = AINT(ALOG10(CMAX) + 2.2E-5)
            ELSE
                ISCALE = -AINT(-ALOG10(CMAX) - 2.2E-5) - 1
            ENDIF
            !
            !         start printing
            !
            IF (MOD(IP(4), IP(3)) == 0) THEN
                WRITE (IOUT, '('' '')')
                WRITE (IOUT, 2040) (MNAME(K), K = 1, 4)
            ENDIF
            IP(4) = IP(4) + 1
            CALL report_time (IOUT, ITIME, ISFLAG, -999.)
            WRITE(IOUT, 2000) SYNAM2(ITOT), ISCALE
            !
            !      Put concentration values in grid layout
            !
            FACTOR = 10.0**ISCALE
            DO I = 1, NX, IP(2)
                DO J = 1, NY
                    NEND = MIN (NX, I + IP(2) - 1)
                    DO K = I, NEND
                        CGRID (K - I + 1, J) = POINT
                        C = CONC2(ITOT + ((J - 1) * NX + K - 1) * NOTOT2)
                        IF (C /= RMISS) THEN
                            WRITE (PADDER, '(F6.3)')   C / FACTOR
                            CGRID(K - I + 1, J) = PADDER
                        ENDIF
                    end do
                    WRITE (IOUT, 2030) (CGRID (K - I + 1, J), K = I, NEND)
                end do
                WRITE (IOUT, '('' '')')
            end do
            !
        end do
        !
        9999 if (timon) call timstop (ithandl)
        RETURN
        !
        2000 FORMAT (/' DUMP OF RESULTS OF ', A20, &
                ' SCALE FACTOR = 10.0 EXP (', I3, ' ).'//)
        2030 FORMAT (6X, 20A6)
        2040 FORMAT (45X, A40)
        !
    END

end module m_outdmp
