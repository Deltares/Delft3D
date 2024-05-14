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
module m_dlwqt2
    use m_waq_precision

    implicit none

contains


    SUBROUTINE DLWQT2 (input_file, LUNOUT, ITIME, RESULT, NTOTAL, LUNTXT, ISFLAG, IFFLAG, ONLINE)
        ! Makes values at ITIME for user supplied binary intermediate files
        !
        !     LOGICAL UNITNUMBERS : input_file  - input unit intermediate file
        !                           LUNOUT - monitor file
        !
        !     SUBROUTINES CALLED  : terminate_execution, stops execution
        !
        !     PARAMETERS          :
        !
        !     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
        !     ----    -----    ------     ------- -----------
        !     input_file   INTEGER       1     INPUT   unit number intermediate file
        !     LUNOUT  INTEGER       1     INPUT   unit number monitor file
        !     ITIME   INTEGER       1     INPUT   Model timer
        !     RESULT  REAL     NTOTAL     OUTPUT  result array at time ITIME
        !     NTOTAL  INTEGER       1     INPUT   number of items to be filled
        !     LUNTXT  CHAR*(*)      1     INPUT   text concerning unit numbers
        !     ISFLAG  INTEGER       1     INPUT   = 1 then 'ddhhmmss' format
        !     IFFLAG  INTEGER       1     INPUT   = 1 then first invocation
        use m_logger, only : terminate_execution
        use timers

        real(kind = real_wp) :: RESULT(NTOTAL)
        integer(kind = int_wp) :: input_file, LUNOUT, ITIME, NTOTAL, ISFLAG, IFFLAG
        CHARACTER*10  MSGTXT(3)
        CHARACTER*(*) LUNTXT
        LOGICAL       ONLINE
        DATA          MSGTXT /' REWIND   ', ' CONSTANT ', ' ERROR    '/
        logical        stream_access                     ! help variable to detect the type of file access
        character(20)  access                            ! help variable to detect the type of file access

        integer(kind = int_wp) :: ierr, itime1, messge
        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("dlwqt2", ithandl)

        IF (ONLINE) THEN
            if (input_file == 20) write (*, *) ' Read VOLUME record'
            if (input_file == 24) write (*, *) ' Read FLOW   record'
        ENDIF
        !
        !         is this the first time?
        !         BYPASS FOR ONLINE MODE, TO AVOID APPARENT CONSTANT FUNCTION
        !
        MESSGE = 0
        IF (IFFLAG == 1 .AND. .NOT. ONLINE) GOTO 20
        !
        !         normal time varying read
        !
        READ  (input_file, END = 10, ERR = 40) ITIME1, RESULT
        goto 9999  !   RETURN
        !
        !         normal rewind.
        !
        10 MESSGE = 1
        IF (ONLINE) STOP 'REWIND NOT POSSIBLE IN ON-LINE MODE'
        inquire(input_file, access = access)
        stream_access = access == 'STREAM'
        if (stream_access) then
            read(input_file, iostat = ierr, pos = 1)
        else
            rewind input_file                            ! Start at the beginning again
        endif
        READ  (input_file, END = 40, ERR = 40) ITIME1, RESULT
        GOTO 50
        !
        !         This is the first time, check only for nr of records.
        !
        20 CONTINUE
        READ  (input_file, END = 40, ERR = 40) ITIME1, RESULT
        READ  (input_file, END = 30, ERR = 40) ITIME1, RESULT
        inquire(input_file, access = access)
        stream_access = access == 'STREAM'
        if (stream_access) then
            read(input_file, iostat = ierr, pos = 1)
        else
            rewind input_file                            ! Start at the beginning again
        endif
        READ  (input_file, END = 30, ERR = 40) ITIME1, RESULT
        goto 9999  !   RETURN
        !
        !         file has only one record, array is constant
        !
        30 MESSGE = 2
        inquire(input_file, access = access)
        stream_access = access == 'STREAM'
        if (stream_access) then
            read(input_file, iostat = ierr, pos = 1)
        else
            rewind input_file                            ! Start at the beginning again
        endif
        READ  (input_file, END = 40, ERR = 40) ITIME1, RESULT
        IFFLAG = -1
        GOTO 50
        !
        !         error, during read
        !
        40 MESSGE = 3
        50 IF (ISFLAG == 1) THEN
            WRITE(LUNOUT, 2010) MSGTXT(MESSGE), input_file, LUNTXT, &
                    ITIME / 86400, MOD(ITIME, 86400) / 3600, &
                    MOD(ITIME, 3600) / 60, MOD(ITIME, 60), &
                    ITIME1 / 86400, MOD(ITIME1, 86400) / 3600, &
                    MOD(ITIME1, 3600) / 60, MOD(ITIME1, 60)
        ELSEIF (ISFLAG == 2) THEN
            WRITE(LUNOUT, 2020) MSGTXT(MESSGE), input_file, LUNTXT, &
                    ITIME / 31536000, &
                    MOD(ITIME, 31536000) / 86400, &
                    MOD(ITIME, 86400) / 3600, &
                    MOD(ITIME, 3600) / 60, &
                    MOD(ITIME, 60), &
                    ITIME1 / 31536000, &
                    MOD(ITIME1, 31536000) / 86400, &
                    MOD(ITIME1, 86400) / 3600, &
                    MOD(ITIME1, 3600) / 60, &
                    MOD(ITIME1, 60)
        ELSE
            WRITE(LUNOUT, 2000) MSGTXT(MESSGE), input_file, LUNTXT, &
                    ITIME, ITIME1
        ENDIF
        IF (MESSGE < 3) goto 9999  !   RETURN
        CALL terminate_execution (1)
        9999 if (timon) call timstop (ithandl)
        RETURN
        !
        2000 FORMAT (A10, 'ON UNIT:', I10, ', READING: ', A, / &
                ' SIMULATION TIME :', I10, ' !  TIME IN FILE: ', I10, ' !')
        2010 FORMAT (A10, 'ON UNIT:', I10, ', READING: ', A, / &
                ' SIMULATION TIME :', I5, 'D ', I2, 'H ', I2, 'M ', I2, 'S ! ', / &
                ' TIME IN FILE    :', I5, 'D ', I2, 'H ', I2, 'M ', I2, 'S ! ')
        2020 FORMAT (A10, 'ON UNIT:', I10, ', READING: ', A, / &
                ' SIMULATION TIME :', I2, 'Y ', I3, 'D ', I2, 'H ', I2, 'M ', I2, 'S .', / &
                ' TIME IN FILE    :', I2, 'Y ', I3, 'D ', I2, 'H ', I2, 'M ', I2, 'S .')
        !
    END

end module m_dlwqt2
