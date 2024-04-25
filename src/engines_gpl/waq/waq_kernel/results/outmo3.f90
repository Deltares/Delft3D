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
module m_outmo3
    use m_waq_precision

    implicit none

contains


    SUBROUTINE OUTMO3 (IOUT, AMASS2, ITIME, SNAME, MNAME, &
            NOTOT, IP, ISFLAG, ASMASS, IBFLAG, &
            NOTOT2, SYNAM2, CONC2, ITSTRT, ITSTOP, &
            NDMPAR, DANAM)
        !
        !     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
        !
        !     CREATED: april 4, 1988 by L.Postma
        !
        !     FUNCTION            : Writes monitoring results to IOUT in
        !                                          blocks of 10 systems.
        !
        !     LOGICAL UNITNUMBERS : IOUT = number of monitoring output file
        !
        !     SUBROUTINES CALLED  : OUTMO1, print routine
        !                           OUTMO2, print routine
        !                           report_time, writes time in specific formats
        !
        !     PARAMETERS          :
        !
        !     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
        !     ----    -----    ------     ------- -----------
        !     IOUT    INTEGER     1       INPUT   unit number output file
        !     AMASS2  REAL     NOTOT*5    INPUT   mass balance whole system
        !     ITIME   INTEGER     1       INPUT   present time in clock units
        !     SNAME   CHAR*20   NOTOT     INPUT   names of substances
        !     MNAME   CHAR*40     4       INPUT   model identification
        !     NOTOT   INTEGER     1       INPUT   total number of systems
        !     IP      INTEGER     4       IN/OUT  paging structure
        !     ISFLAG  INTEGER     1       INPUT   if 1 then dd-hh:mm'ss"
        !     ASMASS  REAL NOTOT*NDMPAR*? INPUT   Mass balance per segment
        !     IBFLAG  INTEGER     1       INPUT   Flag = 1 then balances
        !     NOTOT2  INTEGER             INPUT   Number of extra output vars
        !     SYNAM2  CHAR*20             INPUT   Names of extra output vars
        !     CONC2   REAL    NOTOT1*?    INPUT   Value of all vars
        !     ITSTRT  INTEGER     1       INPUT   start time
        !     ITSTOP  INTEGER     1       INPUT   stop time
        !     NDMPAR  INTEGER     1       INPUT   number of dump area's
        !     DANAM   CHAR*20  NDMPAR     INPUT   names of dump area's
        !
        !
        use m_outmo2
        use date_time_utils, only : report_time
        use timers

        INTEGER(kind = int_wp) :: IOUT, ITIME, NOTOT, ISFLAG, IBFLAG, &
                NOTOT2, ITSTRT, ITSTOP, NDMPAR
        INTEGER(kind = int_wp) :: IP(4)
        REAL(kind = real_wp) :: AMASS2(NOTOT, 5), ASMASS(NOTOT, NDMPAR, *), &
                CONC2(*)
        character(len=20) SNAME(*), SYNAM2(*), DANAM(*)
        character(len=40) MNAME(*)
        !
        !     Local declaration
        !
        character(len=40) VNAME
        integer(kind = int_wp) :: idmp, k, id, id2, nend, nend2
        real(kind = real_wp) :: percit

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("outmo3", ithandl)
        !
        !         initialise the paging, accumulation arrays and acumul flag
        !
        IF (IP(3) == 0) THEN
            IP(3) = MAX(1, IP(1) / (7 + (NDMPAR + 7) * ((NOTOT + IP(2) - 1) / IP(2))))
            IP(4) = 0
        ENDIF
        !
        !         start printing
        !
        IF (MOD(IP(4), IP(3)) == 0) THEN
            WRITE (IOUT, '('' '')')
            WRITE (IOUT, 2100) (MNAME(K), K = 1, 4)
        ENDIF
        IP(4) = IP(4) + 1
        !
        IF (ITSTOP - ITSTRT > 0) THEN
            PERCIT = 100. * (ITIME - ITSTRT) / (ITSTOP - ITSTRT)
        ELSE
            PERCIT = 100.
        ENDIF
        WRITE (IOUT, 2080) PERCIT
        CALL report_time (6, ITIME, ISFLAG, PERCIT)
        WRITE (IOUT, 2000)
        CALL report_time (IOUT, ITIME, ISFLAG, -999.)
        WRITE (IOUT, *)
        !
        DO ID = 1, NOTOT, IP(2)
            NEND = MIN (NOTOT, ID + IP(2) - 1)
            WRITE (IOUT, 2030) (AMASS2(K, 1), K = ID, NEND)
            WRITE (IOUT, 2040) (AMASS2(K, 2), K = ID, NEND)
            WRITE (IOUT, 2050) (AMASS2(K, 3), K = ID, NEND)
            WRITE (IOUT, 2060) (AMASS2(K, 4), K = ID, NEND)
            WRITE (IOUT, 2070) (AMASS2(K, 5), K = ID, NEND)
            WRITE (IOUT, 2020) (SNAME(K)(1:10), K = ID, NEND)
            WRITE (IOUT, 2020) (SNAME(K)(11:20), K = ID, NEND)
            !
            VNAME = 'CONCENTRATION'
            CALL OUTMO2 (IOUT, CONC2, VNAME, DANAM, NDMPAR, &
                    ID, NEND, NOTOT + NOTOT2)
            IF (IBFLAG == 1) THEN
                VNAME = 'MASS'
                CALL OUTMO2 (IOUT, ASMASS(1, 1, 1), VNAME, DANAM, NDMPAR, &
                        ID, NEND, NOTOT)
                VNAME = 'PROCESSES'
                CALL OUTMO2 (IOUT, ASMASS(1, 1, 2), VNAME, DANAM, NDMPAR, &
                        ID, NEND, NOTOT)
                VNAME = 'LOADS ( IN )'
                CALL OUTMO2 (IOUT, ASMASS(1, 1, 3), VNAME, DANAM, NDMPAR, &
                        ID, NEND, NOTOT)
                VNAME = 'LOADS ( OUT )'
                CALL OUTMO2 (IOUT, ASMASS(1, 1, 4), VNAME, DANAM, NDMPAR, &
                        ID, NEND, NOTOT)
                VNAME = 'TRANSPORT ( IN )'
                CALL OUTMO2 (IOUT, ASMASS(1, 1, 5), VNAME, DANAM, NDMPAR, &
                        ID, NEND, NOTOT)
                VNAME = 'TRANSPORT ( OUT )'
                CALL OUTMO2 (IOUT, ASMASS(1, 1, 6), VNAME, DANAM, NDMPAR, &
                        ID, NEND, NOTOT)
            ENDIF
            !
            WRITE (IOUT, '('' '')')
        end do
        !
        !     extra vars
        !
        DO ID = 1, NOTOT2, IP(2)
            NEND = MIN (NOTOT2, ID + IP(2) - 1)
            ID2 = ID + NOTOT
            NEND2 = NEND + NOTOT
            WRITE (IOUT, 2020) (SYNAM2(K)(1:10), K = ID, NEND)
            WRITE (IOUT, 2020) (SYNAM2(K)(11:20), K = ID, NEND)
            !
            VNAME = 'VALUE'
            CALL OUTMO2 (IOUT, CONC2, VNAME, DANAM, NDMPAR, &
                    ID2, NEND2, NOTOT + NOTOT2)
            !
            WRITE (IOUT, '('' '')')
        end do
        !
        if (timon) call timstop (ithandl)
        RETURN
        !
        2000 FORMAT (//' DUMP OF INTERMEDIATE RESULTS IN SELECTED SEGMENTS')
        2020 FORMAT (22X, 10(A10, ' '))
        2030 FORMAT (' TOTAL MASS IN SYSTEM', 10(1P, E11.4))
        2040 FORMAT (' CHANGES BY PROCESSES', 10(1P, E11.4))
        2050 FORMAT (' CHANGES BY LOADS    ', 10(1P, E11.4))
        2060 FORMAT (' BOUNDARY INFLOWS    ', 10(1P, E11.4))
        2070 FORMAT (' BOUNDARY OUTFLOWS   ', 10(1P, E11.4))
        2080 FORMAT (' ', F6.2, '% Completed')
        2100 FORMAT (45X, A40)
        !
    END

end module m_outmo3
