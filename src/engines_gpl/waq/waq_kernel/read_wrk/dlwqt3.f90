!!  Copyright (C)  Stichting Deltares, 2012-2025.
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
module m_dlwqt3
    use m_waq_precision

    implicit none

contains


    SUBROUTINE DLWQT3 (ITIME, IPERIO, APHASE, AVALUE, NRHARM, &
            NOSUB, NOSPAC, IPOINT, NPOINT, RESULT, &
            LUNTXT, input_file, LUNOUT, ISFLAG, IFFLAG, &
            UPDATE)
        !
        !     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
        !
        !     CREATED: april- 8-1988 by L.Postma
        !
        !     FUNCTION            : Makes harmonic function values.
        !
        !     LOGICAL UNITNUMBERS : input_file file for initialisation of harmonics
        !                           LUNOUT - monitor file
        !
        !     SUBROUTINES CALLED  : stop_with_error, stops execution
        !
        !     PARAMETERS          :
        !
        !     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
        !     ----    -----    ------     ------- -----------
        !     ITIME   INTEGER       1     INPUT   Model timer
        !     IPERIO  INTEGER   NRHARM    IN/OUT  harmonic periods
        !     APHASE  REAL      NRHARM    IN/OUT  harmonic phases
        !     AVALUE  REAL   NOSUB*NRHARM IN/OUT  amplitudes for NOSUB values
        !     NRHARM  INTEGER       1     INPUT   number of harmonic records
        !     NOSUB   INTEGER       1     INPUT   number of values in an item
        !     NOSPAC  INTEGER       1     OUTPUT  space occupied by harmonics
        !     IPOINT  INTEGER       ?     INPUT   pointer to output array
        !     NPOINT  INTEGER       1     OUTPUT  last pointer in IPOINT
        !     RESULT  REAL     NOSUB*?    OUTPUT  function values at ITIME
        !     LUNTXT  CHAR*(*)      1     INPUT   text with unitnumber
        !     input_file   INTEGER       1     INPUT   unit number intermediate file
        !     LUNOUT  INTEGER       1     INPUT   unit number monitor file
        !     ISFLAG  INTEGER       1     INPUT   = 1, 'DDHHMMSS' format
        !     IFFLAG  INTEGER       1     INPUT   = 1, first invocation
        !     UPDATE  LOGICAL       1     OUTPUT  set to T if function is updated
        !                                         else set to F
        !
        !     Internal file structure:
        !     IPERIO    APHASE     AVALUE --->
        !     npoints1  n1-harmos  nosub averages             .
        !     period1   phase1     nosub amplitudes           .
        !        .         .                                  .
        !     period-n1 phase-n1   nosub amplitudes           .
        !     npoints2  n2-harmos  nosub averages             .
        !     period1   phase1     nosub amplitudes           .
        !        .         .                                  .
        !     period-n2 phase-n2   nosub amplitudes   ---- NRHARMth line
        !     IPOINT:
        !     -npoints1--->---npoints2----->.....NPOINT     pointers
        !
        !     DECLARATIONS        :
        !
        use m_logger_helper, only : stop_with_error
        use timers

        real(kind = real_wp), PARAMETER :: TWOPI = 6.28319
        integer(kind = int_wp) :: IPERIO(*), IPOINT(*)
        real(kind = real_wp) :: APHASE(*), AVALUE(*), RESULT(NOSUB, *)
        integer(kind = int_wp) :: ITIME, NRHARM, NOSUB, NOSPAC, NPOINT, &
                input_file, LUNOUT, ISFLAG, IFFLAG

        character(len=*) LUNTXT
        LOGICAL       UPDATE

        integer(kind = int_wp) :: k, num_substances_total
        integer(kind = int_wp) :: irec, itel, ib, ih, ihstop, istart, i1, i2, iv
        real(kind = real_wp) :: func

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("dlwqt3", ithandl)
        !
        !         are there harmonics? is this the initialisation phase?
        !
        NOSPAC = 0
        NPOINT = 0
        UPDATE = .FALSE.
        IF (NRHARM == 0) goto 9999  !   RETURN
        UPDATE = .TRUE.
        IREC = 1
        ITEL = 1
        IF (IFFLAG == 0) GOTO 40
        !
        !         at first time, initialise arrays
        !         loop over the blocks of harmonics ( must be less than NRHARM )
        !
        DO IB = 1, NRHARM + 1
            IF (IREC > NRHARM) GOTO 30
            !
            !         loop over the number of harmonics
            !
            READ (input_file, END = 80, ERR = 80)   num_substances_total, APHASE(IREC), &
                    (AVALUE(K + NOSPAC), K = 1, num_substances_total)
            NOSPAC = NOSPAC + num_substances_total
            IPERIO(IREC) = num_substances_total
            IHSTOP = APHASE(IREC) + 0.5
            IREC = IREC + 1
            DO IH = 1, IHSTOP
                READ (input_file, END = 80, ERR = 80) IPERIO(IREC), APHASE(IREC), &
                        (AVALUE(K + NOSPAC), K = 1, num_substances_total)
                NOSPAC = NOSPAC + num_substances_total
                IREC = IREC + 1
            end do
            !
            !         return only by IREC > NRHARM
            !
        end do
        30 NOSPAC = 0
        NPOINT = 0
        IREC = 1
        ITEL = 1
        !
        !         loop over the blocks of harmonics ( must be less than NRHARM )
        !
        40 DO IB = 1, NRHARM + 1
            IF (IREC > NRHARM) goto 9999  !   RETURN
            !
            !         loop over the number of harmonics
            !
            num_substances_total = IPERIO(IREC)
            IHSTOP = APHASE(IREC) + 0.5
            ISTART = NPOINT + 1
            NPOINT = NPOINT + num_substances_total / NOSUB
            DO IH = 1, IHSTOP + 1
                !
                !         harmonic function
                !
                IF (IH == 1) THEN
                    FUNC = 1.0
                ELSE
                    FUNC = SIN((real(ITIME) / IPERIO(IREC) - APHASE(IREC)) * TWOPI)
                ENDIF
                !
                !         loop over the pointers and the values
                !
                DO I1 = ISTART, NPOINT
                    IV = IPOINT(I1)
                    DO I2 = 1, NOSUB
                        RESULT(I2, IV) = RESULT(I2, IV) + FUNC * AVALUE(ITEL)
                        ITEL = ITEL + 1
                    end do
                end do
                !
                !         increase the record counter
                !
                IREC = IREC + 1
                NOSPAC = NOSPAC + num_substances_total
            end do
            !
            !         return only by IREC > NRHARM
            !
        end do
        !
        !         errors during read
        !
        80 IF (ISFLAG == 1) THEN
            WRITE(LUNOUT, 2020) input_file, LUNTXT, &
                    ITIME / 86400, MOD(ITIME, 86400 / 3600), &
                    MOD(ITIME, 3600) / 60, MOD(ITIME, 60)
        ELSEIF (ISFLAG == 2) THEN
            WRITE(LUNOUT, 2030) input_file, LUNTXT, &
                    ITIME / 31536000, &
                    MOD(ITIME, 31536000) / 86400, &
                    MOD(ITIME, 86400) / 3600, &
                    MOD(ITIME, 3600) / 60, &
                    MOD(ITIME, 60)
        ELSE
            WRITE(LUNOUT, 2010) input_file, LUNTXT, ITIME
        ENDIF
        CALL stop_with_error()
        9999 if (timon) call timstop (ithandl)
        RETURN
        !
        2010 FORMAT (' ERROR   ON UNIT:', I10, ', READING: ', A, / &
                ' SIMULATION TIME:', I10, ' !')
        2020 FORMAT (' ERROR   ON UNIT:', I10, ', READING: ', A, / &
                ' SIMULATION TIME:', I5, 'D ', I2, 'H ', I2, 'M ', I2, 'S !')
        2030 FORMAT (' ERROR   ON UNIT:', I10, ', READING: ', A, / &
                ' SIMULATION TIME:', I2, 'Y ', I3, 'D ', I2, 'H ', I2, 'M ', I2, 'S .')
        !
    END

end module m_dlwqt3
