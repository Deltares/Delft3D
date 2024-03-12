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
module m_fffind
    use m_waq_precision
    use time_module

    implicit none

contains


    SUBROUTINE FFFIND (LUNUT, SGET, AFILE, BFILE, ISTEP, &
            IT2, IT3, IT4, NUMBR, IERR)
        !
        use m_get_filepath_and_pathlen
        use m_open_waq_files
        use m_sysi          ! Timer characteristics

        integer(kind = int_wp) :: ILUN, IERR, I, K, A, IT2, IT3, IT4
        integer(kind = int_wp) :: IYEAR1, IMONTH1, IDAY1, IHOUR1, IMIN1, ISEC1
        integer(kind = int_wp) :: IYEAR2, IMONTH2, IDAY2, IHOUR2, IMIN2, ISEC2
        integer(kind = int_wp) :: IYEAR3, IMONTH3, IDAY3, IHOUR3, IMIN3, ISEC3
        integer(kind = int_wp) :: IYEAR4, IMONTH4, IDAY4, IHOUR4, IMIN4, ISEC4
        integer(kind = int_wp) :: IDATE, ITIME, ITIM, ITIM2, ISTEP, IDTF, LUNUT
        INTEGER(kind = int_wp) :: NUMBR

        CHARACTER*25  SGET, S1
        CHARACTER*255 AFILE, BFILE

        !
        !     local declarations
        !
        REAL(kind = dp) :: REFTIM, STARTTIM, STOPTIM, AFACT
        CHARACTER*255 FILPATH
        INTEGER(kind = int_wp) :: PATHLEN
        !
        !          Open the ASCII .hyd file
        !
        ILUN = 148
        CALL open_waq_files  (ILUN, AFILE, 33, 1, IERR)
        IF (IERR > 0) THEN
            WRITE (LUNUT, 1000) AFILE
            RETURN
        ENDIF
        CALL get_filepath_and_pathlen(AFILE, FILPATH, PATHLEN)
        !
        !          Search for the file name of this item
        !
        DO I = 1, 10000
            READ (ILUN, *, END = 20) S1, BFILE
            IF (S1 == 'conversion-ref-time  ') &
                    READ (BFILE, '(I4,I2,I2,I2,I2,I2)') &
                            IYEAR1, IMONTH1, IDAY1, IHOUR1, IMIN1, ISEC1
            IF (S1 == 'conversion-start-time') &
                    READ (BFILE, '(I4,I2,I2,I2,I2,I2)') &
                            IYEAR2, IMONTH2, IDAY2, IHOUR2, IMIN2, ISEC2
            IF (S1 == 'conversion-stop-time ') &
                    READ (BFILE, '(I4,I2,I2,I2,I2,I2)') &
                            IYEAR3, IMONTH3, IDAY3, IHOUR3, IMIN3, ISEC3
            IF (S1 == 'conversion-timestep  ') &
                    READ (BFILE, '(I4,I2,I2,I2,I2,I2)') &
                            IYEAR4, IMONTH4, IDAY4, IHOUR4, IMIN4, ISEC4
            IF (S1 == SGET) GOTO 30
        end do
        20 WRITE (LUNUT, 1010) SGET
        IERR = 1
        RETURN
        30 CONTINUE
        IF (PATHLEN > 0) THEN
            BFILE = FILPATH(1:PATHLEN) // BFILE
        ENDIF
        IDATE = IYEAR1 * 10000 + IMONTH1 * 100 + IDAY1
        ITIME = IHOUR1 * 10000 + IMIN1 * 100 + ISEC1
        REFTIM = julian_with_leapyears (IDATE, ITIME)
        IDATE = IYEAR2 * 10000 + IMONTH2 * 100 + IDAY2
        ITIME = IHOUR2 * 10000 + IMIN2 * 100 + ISEC2
        STARTTIM = julian_with_leapyears (IDATE, ITIME)
        IDATE = IYEAR3 * 10000 + IMONTH3 * 100 + IDAY3
        ITIME = IHOUR3 * 10000 + IMIN3 * 100 + ISEC3
        STOPTIM = julian_with_leapyears (IDATE, ITIME)
        IT4 = IYEAR4 * 31536000 + IMONTH4 * 2592000 + IDAY4 * 86400 + &
                IHOUR4 * 3600 + IMIN4 * 60 + ISEC4
        AFACT = ISFACT / 864.0D+02
        IF (ISFACT < 0) AFACT = -1.0D+00 / ISFACT / 864.0D+02
        IT2 = (STARTTIM - REFTIM) / AFACT + 0.5
        IT3 = (STOPTIM - REFTIM) / AFACT + 0.5
        CLOSE (ILUN)
        !
        !          Open the binary file for this item
        !
        CALL open_waq_files  (ILUN, BFILE, 33, 2, IERR)
        IF (IERR > 0) THEN
            WRITE (LUNUT, 1020) BFILE
            RETURN
        ENDIF
        !
        !          Find the time step in the file where to start
        !
        READ (ILUN, END = 50) ITIM, (A, K = 1, ABS(NUMBR))
        IF (ITIM /= IT2) THEN
            WRITE (LUNUT, 1030) IT2, ITIM, BFILE
            IERR = 1
            RETURN
        ENDIF
        READ (ILUN, END = 50) ITIM2, (A, K = 1, ABS(NUMBR))
        IDTF = ITIM2 - ITIM
        IF (IDTF /= IT4) THEN
            WRITE (LUNUT, 1040) IT4, IDTF, BFILE
            IERR = 1
            RETURN
        ENDIF
        IF (((ISTEP - ITIM) / IDTF) * IDTF == ISTEP - ITIM) THEN
            CLOSE (ILUN)
            RETURN
        ENDIF
        !
        50 WRITE (LUNUT, 1050) ISTEP, BFILE
        IERR = 1
        RETURN
        !
        1000 FORMAT (/' ERROR: Opening ASCII coupling file: ', A)
        1010 FORMAT (/' ERROR: Search string not found: ', A)
        1020 FORMAT (/' ERROR: Opening binary file: ', A)
        1030 FORMAT (/' ERROR: Start time is not: ', I10, ' but: ', I10, &
                ' in file: ', A)
        1040 FORMAT (/' ERROR: Time step is not: ', I10, ' but: ', I10, &
                ' in file: ', A)
        1050 FORMAT (/' ERROR: Time: ', I10, ' not found in file: ', A)
        !
    END
    !                                      *** end of addition of July 2002
end module m_fffind
