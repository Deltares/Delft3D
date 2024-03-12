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
module m_dlwqkv
    use m_waq_precision

    implicit none

contains


    SUBROUTINE DLWQKV (LUNIN, LUNOUT, ITIME, IARRAY, NTOTAL, &
            LUNTXT, ISFLAG, IFFLAG)
        !
        !     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
        !
        !
        !     FUNCTION            : Makes values at ITIME for user supplied
        !                                         binary intermediate files
        !
        !     LOGICAL UNITNUMBERS : LUNIN  - input unit intermediate file
        !                           LUNOUT - monitor file
        !
        !     SUBROUTINES CALLED  : SRSTOP, stops execution
        !
        !     PARAMETERS          :
        !
        !     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
        !     ----    -----    ------     ------- -----------
        !     LUNIN   INTEGER       1     INPUT   unit number intermediate file
        !     LUNOUT  INTEGER       1     INPUT   unit number monitor file
        !     ITIME   INTEGER       1     INPUT   Model timer
        !     IARRAY  INTEGER  NTOTAL     OUTPUT  result array at time ITIME
        !     NTOTAL  INTEGER       1     INPUT   number of items to be filled
        !     LUNTXT  CHAR*(*)      1     INPUT   text concerning unit numbers
        !     ISFLAG  INTEGER       1     INPUT   = 1 then 'ddhhmmss' format
        !     IFFLAG  INTEGER       1     INPUT   = 1 then first invocation
        !
        !     DECLARATIONS        :
        !
        use m_srstop
        use timers
        INTEGER(kind = int_wp) :: LUNIN, LUNOUT, ITIME, NTOTAL, ISFLAG, &
                IFFLAG
        INTEGER(kind = int_wp) :: IARRAY(NTOTAL)
        CHARACTER*(*) LUNTXT
        !
        !     Local
        !
        logical        stream_access                     ! help variable to detect the type of file access
        character(20)  access                            ! help variable to detect the type of file access
        CHARACTER*10  MSGTXT(3)
        DATA          MSGTXT /' REWIND   ', ' CONSTANT ', ' ERROR    '/

        integer(kind = int_wp) :: messge, itime1, ierr

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("dlwqkv", ithandl)
        !
        !         is this the first time?
        !
        MESSGE = 0
        IF (IFFLAG == 1) GOTO 20
        !
        !         normal time varying read
        !
        READ  (LUNIN, END = 10, ERR = 40) ITIME1, IARRAY
        goto 9999
        !
        !         normal rewind.
        !
        10 MESSGE = 1
        inquire(lunin, access = access)
        stream_access = access == 'STREAM'
        if (stream_access) then
            read(lunin, iostat = ierr, pos = 1)
        else
            rewind lunin                            ! Start at the beginning again
        endif
        READ  (LUNIN, END = 40, ERR = 40) ITIME1, IARRAY
        GOTO 50
        !
        !         This is the first time, check only for nr of records.
        !
        20 CONTINUE
        READ  (LUNIN, END = 40, ERR = 40) ITIME1, IARRAY
        READ  (LUNIN, END = 30, ERR = 40) ITIME1, IARRAY
        inquire(lunin, access = access)
        stream_access = access == 'STREAM'
        if (stream_access) then
            read(lunin, iostat = ierr, pos = 1)
        else
            rewind lunin                            ! Start at the beginning again
        endif
        READ  (LUNIN, END = 30, ERR = 40) ITIME1, IARRAY
        goto 9999
        !
        !         file has only one record, array is constant
        !
        30 MESSGE = 2
        inquire(lunin, access = access)
        stream_access = access == 'STREAM'
        if (stream_access) then
            read(lunin, iostat = ierr, pos = 1)
        else
            rewind lunin                            ! Start at the beginning again
        endif
        READ  (LUNIN, END = 40, ERR = 40) ITIME1, IARRAY
        IFFLAG = -1
        GOTO 50
        !
        !         error, during read
        !
        40 MESSGE = 3
        50 IF (ISFLAG /= 1) THEN
            WRITE(LUNOUT, 2000) MSGTXT(MESSGE), LUNIN, LUNTXT, &
                    ITIME, ITIME1
        ELSE
            WRITE(LUNOUT, 2010) MSGTXT(MESSGE), LUNIN, LUNTXT, &
                    ITIME / 86400, MOD(ITIME, 86400) / 3600, &
                    MOD(ITIME, 3600) / 60, MOD(ITIME, 60), &
                    ITIME1 / 86400, MOD(ITIME1, 86400) / 3600, &
                    MOD(ITIME1, 3600) / 60, MOD(ITIME1, 60)
        ENDIF
        IF (MESSGE < 3) goto 9999
        CALL SRSTOP (1)
        9999 if (timon) call timstop (ithandl)
        !
        2000 FORMAT (A10, 'ON UNIT:', I10, ', READING: ', A20, / &
                ' SIMULATION TIME :', I10, ' !  TIME IN FILE: ', I10, ' !')
        2010 FORMAT (A10, 'ON UNIT:', I10, ', READING: ', A20, / &
                ' SIMULATION TIME :', I5, 'D ', I2, 'H ', I2, 'M ', I2, 'S ! ', &
                ' TIME IN FILE    :', I5, 'D ', I2, 'H ', I2, 'M ', I2, 'S ! ')
        !
    END

end module m_dlwqkv
