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
module m_dlwqio
    use m_waq_precision

    implicit none

contains


    SUBROUTINE DLWQIO (LUNWRO, LCH, LUREP, NOUTP, NRVART, &
            NBUFMX, IOUTPS, IOPOIN, OUNAM, OUSNM, &
            OUUNI, OUDSC, NOTOT, SYSNM, SYUNI, &
            SYDSC, LUN, LCHAR, IERR)
        !
        !     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
        !
        !     CREATED:            : may 1993 by Jan van Beek
        !
        !     FUNCTION            : Initialisation of OUTPUT system.
        !                           Reads output work file.
        !
        !     SUBROUTINES CALLED  : open_waq_files, Opens files
        !
        !     FILES               : LUNWRO, Proces work file
        !                           LUREP , Monitoring file
        !
        !     PARAMETERS          : 12
        !
        !     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
        !     ----    -----    ------     ------- -----------
        !     LUNWRO  INTEGER       1     INPUT   Output work file
        !     LCH     CHA*(*)       1     INPUT   Name output work file
        !     LUREP   INTEGER       1     INPUT   Monitoring file
        !     NOUTP   INTEGER       1     INPUT   Number of output files
        !     NRVART  INTEGER       1     INPUT   Number of extra output vars
        !     NBUFMX  INTEGER       1     INPUT   length of output buffer
        !     IOUTPS  INTEGER 7*NOUTP    OUTPUT   Output structure
        !                                            index 1 = start time
        !                                            index 2 = stop time
        !                                            index 3 = time step
        !                                            index 4 = number of vars
        !                                            index 5 = kind of output
        !                                            index 6 = format of output
        !                                            index 7 = initialize flag
        !     IOPOIN  INTEGER  NRVART    OUTPUT   Pointer to DELWAQ array's
        !     OUNAM   CHAR*(*) NRVART    OUTPUT   name of output variable
        !     OUSNM   CHAR*(*) NRVART    OUTPUT   standard name of output variable
        !     OUUNI   CHAR*(*) NRVART    OUTPUT   unit of output variable
        !     OUDSC   CHAR*(*) NRVART    OUTPUT   description of output variable
        !     OSSNM   CHAR*(*) NRVART    OUTPUT   standard name of substance
        !     OSUNI   CHAR*(*) NRVART    OUTPUT   unit of substance
        !     OSDSC   CHAR*(*) NRVART    OUTPUT   description of substance
        !     LUN     INTEGER    *        INPUT   array with unit numbers
        !     LCHAR   CHAR*(*)   *        INPUT   filenames
        !     IERR    INTEGER       1    IN/OUT   cummulative error count
        !
        !     Declaration of arguments
        !
        use m_srstop
        use m_open_waq_files
        use timers
        use results

        INTEGER(kind = int_wp) :: LUNWRO, LUREP, NOUTP, NRVART, NBUFMX, NOSYS, &
                IERR, NOTOT
        INTEGER(kind = int_wp) :: IOUTPS(7, *), IOPOIN(*), LUN(*)
        character(len=*) LCH, LCHAR(*)
        character(len=20)  OUNAM(*)
        character(len=100) OUSNM(*), SYSNM(*)
        character(len=40)  OUUNI(*), SYUNI(*)
        character(len=60)  OUDSC(*), SYDSC(*)
        !
        !     Local declarations
        !
        integer(kind = int_wp), PARAMETER :: LUOFF = 18
        integer(kind = int_wp), PARAMETER :: LUOFF2 = 36
        INTEGER(kind = int_wp) :: NOUTPD, NRVARD, NBUFMD
        REAL(kind = real_wp) :: VERSIO

        integer(kind = int_wp) :: k, isrtou, ifi, idum

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("dlwqio", ithandl)
        !
        !     read and check version number
        !
        READ (LUNWRO, ERR = 900, END = 900) VERSIO
        !
        !     read and check dimensions
        !
        READ (LUNWRO, ERR = 900, END = 900) NOUTPD, NRVARD, NBUFMD, NCOPT
        IF (NOUTPD /= NOUTP) THEN
            WRITE (LUREP, 2020) NOUTPD, NOUTP
            IERR = IERR + 1
        ENDIF
        IF (NRVARD /= NRVART) THEN
            WRITE (LUREP, 2030) NRVARD, NRVART
            IERR = IERR + 1
        ENDIF
        IF (NBUFMD /= NBUFMX) THEN
            WRITE (LUREP, 2040) NBUFMD, NBUFMX
            IERR = IERR + 1
        ENDIF
        IF (IERR > 0) GOTO 910
        !
        READ (LUNWRO, ERR = 900, END = 900) (IOUTPS(1, K), K = 1, NOUTP)
        READ (LUNWRO, ERR = 900, END = 900) (IOUTPS(2, K), K = 1, NOUTP)
        READ (LUNWRO, ERR = 900, END = 900) (IOUTPS(3, K), K = 1, NOUTP)
        READ (LUNWRO, ERR = 900, END = 900) (IOUTPS(4, K), K = 1, NOUTP)
        READ (LUNWRO, ERR = 900, END = 900) (IOUTPS(5, K), K = 1, NOUTP)
        READ (LUNWRO, ERR = 900, END = 900) (IOUTPS(6, K), K = 1, NOUTP)
        IF (NRVART>0) THEN
            READ (LUNWRO, ERR = 900, END = 900) (IOPOIN(K), K = 1, NRVART)
            READ (LUNWRO, ERR = 900, END = 900) (OUNAM (K), K = 1, NRVART)
            READ (LUNWRO, ERR = 900, END = 900) (OUSNM (K), K = 1, NRVART)
            READ (LUNWRO, ERR = 900, END = 900) (OUUNI (K), K = 1, NRVART)
            READ (LUNWRO, ERR = 900, END = 900) (OUDSC (K), K = 1, NRVART)
        ENDIF
        IF (NOTOT>0) THEN
            READ (LUNWRO, ERR = 900, END = 900) (SYSNM (K), K = 1, NOTOT)
            READ (LUNWRO, ERR = 900, END = 900) (SYUNI (K), K = 1, NOTOT)
            READ (LUNWRO, ERR = 900, END = 900) (SYDSC (K), K = 1, NOTOT)
        ENDIF
        !
        !     Set initialize flag, open files: only on first subdomain
        !
        DO K = 1, NOUTP
            ISRTOU = IOUTPS(5, K)
            IF (K <= 4) THEN
                IFI = K + LUOFF
            ELSEIF (K <= 7) THEN
                IFI = K + LUOFF2 - 4
            ELSE
                IFI = K + LUOFF2 - 2
            ENDIF
            !
            !           Open the output-file in the correct way, depending on type of output
            !
            IOUTPS(7, K) = 1
            IF (ISRTOU == IMON .OR. ISRTOU == IMO2 .OR. &
                    ISRTOU == IMO3 .OR. ISRTOU == IMO4) THEN
                !
                !              Do not open the normal monitor file
                !
                IF (K /= 1) THEN
                    CALL open_waq_files (LUN(IFI), LCHAR(IFI), 19, 1, IDUM)
                ENDIF
            ELSEIF (ISRTOU == IDMP .OR. ISRTOU == IDM2) THEN
                CALL open_waq_files (LUN(IFI), LCHAR(IFI), 20, 1, IDUM)
            ELSEIF (ISRTOU == IHIS .OR. ISRTOU == IHI2 .OR. &
                    ISRTOU == IHI3 .OR. ISRTOU == IHI4) THEN
                CALL open_waq_files (LUN(IFI), LCHAR(IFI), 21, 1, IDUM)
            ELSEIF (ISRTOU == IMAP .OR. ISRTOU == IMA2) THEN
                CALL open_waq_files (LUN(IFI), LCHAR(IFI), 22, 1, IDUM)
            ELSEIF (ISRTOU == IBAL .OR. ISRTOU == IBA2) THEN
                CALL open_waq_files (LUN(IFI), LCHAR(IFI), 37, 1, IDUM)
            ENDIF
        end do
        !
        if (timon) call timstop (ithandl)
        RETURN
        !
        !     unsuccessful read
        !
        900 CONTINUE
        WRITE (LUREP, 2050) LCH, LUNWRO
        IERR = IERR + 1
        !
        910 CONTINUE
        if (timon) call timstop (ithandl)
        RETURN
        !
        !     output formats
        !
        2020 FORMAT (' ERROR  : Output work file doesn''t match dimensions in' &
                /'          DELWAQ boot file for NOUTP', &
                /'          ', I6, ' in output,', I6, ' in boot file.')
        2030 FORMAT (' ERROR  : Output work file doesn''t match dimensions in' &
                /'          DELWAQ boot file for NRVART', &
                /'          ', I6, ' in output,', I6, ' in boot file.')
        2040 FORMAT (' ERROR  : Output work file doesn''t match dimensions in' &
                /'          DELWAQ boot file for NBUFMX', &
                /'          ', I6, ' in output,', I6, ' in boot file.')
        2050 FORMAT (' ERROR  : Reading output work file;', A, &
                /'          on unit number ', I3)
        !
    END

end module m_dlwqio
