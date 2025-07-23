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
module m_stadsc
    use m_waq_precision
    use m_logger_helper, only: get_log_unit_number

    implicit none

contains


    subroutine stadsc (process_space_real, fl, ipoint, increm, num_cells, &
            noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
            num_exchanges_z_dir, num_exchanges_bottom_dir)
        use m_extract_waq_attribute

        !>\file
        !>       Mean, min, max, stdev of a variable during a certain time

        !
        !     Description of the module :
        !
        ! Name    T   L I/O   Description                                  Units
        ! ----    --- -  -    -------------------                          -----
        !
        ! CONC           I    Concentration of the substance            1
        ! TSTART         I    Start of statistical period               2
        ! TSTOP          I    Stop of statistical period                3
        ! TIME           I    Time in calculation                       4
        ! DELT           I    Timestep                                  5
        !
        ! TCOUNT         O    Count of times (must be imported!)        6
        ! CMAX           O    Maximum value over the given period       7
        ! CMIN           O    Minimum value over the given period       8
        ! CMEAN          O    Mean value over the given period          9
        ! CSTDEV         O    Standard deviation over the given period 10
        !

        !     Logical Units : -

        !     Modules called : -

        !     Name     Type   Library
        !     ------   -----  ------------

        IMPLICIT NONE

        REAL(kind = real_wp) :: process_space_real  (*), FL    (*)
        INTEGER(kind = int_wp) :: IPOINT(*), INCREM(*), num_cells, NOFLUX, &
                IEXPNT(4, *), IKNMRK(*), num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir
        !
        INTEGER(kind = int_wp) :: IP1, IP2, IP3, IP4, IP5, &
                IP6, IP7, IP8, IP9, IP10, &
                IN1, IN2, IN3, IN4, IN5, &
                IN6, IN7, IN8, IN9, IN10
        INTEGER(kind = int_wp) :: ISEG
        INTEGER(kind = int_wp) :: IACTION, lunrep
        INTEGER(kind = int_wp) :: ATTRIB
        REAL(kind = real_wp) :: TSTART, TSTOP, TIME, DELT, TCOUNT
        REAL(kind = real_wp) :: CDIFF

        INTEGER(kind = int_wp), PARAMETER :: MAXWARN = 50
        INTEGER(kind = int_wp), SAVE :: NOWARN = 0

        call get_log_unit_number(lunrep)

        IP1 = IPOINT(1)
        IP2 = IPOINT(2)
        IP3 = IPOINT(3)
        IP4 = IPOINT(4)
        IP5 = IPOINT(5)
        IP6 = IPOINT(6)
        IP7 = IPOINT(7)
        IP8 = IPOINT(8)
        IP9 = IPOINT(9)
        IP10 = IPOINT(10)

        IN1 = INCREM(1)
        IN2 = INCREM(2)
        IN3 = INCREM(3)
        IN4 = INCREM(4)
        IN5 = INCREM(5)
        IN6 = INCREM(6)
        IN7 = INCREM(7)
        IN8 = INCREM(8)
        IN9 = INCREM(9)
        IN10 = INCREM(10)

        !
        !     There are five cases, defined by the time:
        !                        TIME <  TSTART-0.5*DELT : do nothing
        !     TSTART-0.5*DELT <= TIME <  TSTART+0.5*DELT : initialise
        !     TSTART          <  TIME <  TSTOP           : accumulate
        !     TSTOP           <= TIME <  TSTOP+0.5*DELT  : finalise
        !     TSTOP+0.5*DELT  <  TIME                    : do nothing
        !
        !     (Use a safe margin)
        !
        TSTART = process_space_real(IP2)
        TSTOP = process_space_real(IP3)
        TIME = process_space_real(IP4)
        DELT = process_space_real(IP5)

        !
        !      Start and stop criteria are somewhat involved. Be careful
        !      to avoid spurious calculations (initial and final) when
        !      none is expected.
        !      Notes:
        !      - The initial value for TCOUNT must be 0.0
        !      - Time is expected to be the model time (same time frame
        !        as the start and stop times of course)
        !      - Check that the NEXT timestep will not exceed the stop time,
        !        otherwise this is the last one
        !
        IACTION = 0
        IF (TIME >= TSTART - 0.5 * DELT .AND. TIME <= TSTOP + 0.5 * DELT) THEN
            IACTION = 2
            IF (TIME <= TSTART + 0.5 * DELT) THEN
                DO ISEG = 1, num_cells
                    IP6 = IPOINT(6) + (ISEG - 1) * INCREM(6)
                    IP7 = IPOINT(7) + (ISEG - 1) * INCREM(7)
                    IP8 = IPOINT(8) + (ISEG - 1) * INCREM(8)
                    IP9 = IPOINT(9) + (ISEG - 1) * INCREM(9)
                    IP10 = IPOINT(10) + (ISEG - 1) * INCREM(10)
                    process_space_real(IP6) = 0.0
                    process_space_real(IP7) = 0.0
                    process_space_real(IP8) = 0.0
                    process_space_real(IP9) = 0.0
                    process_space_real(IP10) = 0.0
                ENDDO
            ENDIF
        ENDIF

        IF (TIME >= TSTOP - 0.5 * DELT .AND. TIME <= TSTOP + 0.5 * DELT) THEN
            IACTION = 3
        ENDIF

        IF (IACTION == 0) RETURN

        IP6 = IPOINT(6)
        IP7 = IPOINT(7)
        IP8 = IPOINT(8)
        IP9 = IPOINT(9)
        IP10 = IPOINT(10)

        DO ISEG = 1, num_cells

            IF (BTEST(IKNMRK(ISEG), 0)) THEN
                !
                !           Keep track of the time within the current descriptive statistics specification
                !           that each segment is active
                !
                TCOUNT = process_space_real(IP6) + 1.0
                process_space_real(IP6) = TCOUNT
                !
                IF (TCOUNT == 1.0) THEN
                    process_space_real(IP7) = process_space_real(IP1)
                    process_space_real(IP8) = process_space_real(IP1)
                ELSE
                    process_space_real(IP7) = MAX(process_space_real(IP7), process_space_real(IP1))
                    process_space_real(IP8) = MIN(process_space_real(IP8), process_space_real(IP1))
                ENDIF
                process_space_real(IP9) = process_space_real(IP9) + process_space_real(IP1)
                process_space_real(IP10) = process_space_real(IP10) + process_space_real(IP1)**2
            ENDIF

            !
            !        Always do the final processing whether the segment is active at this moment or not
            !
            IF (IACTION == 3) THEN
                TCOUNT = process_space_real(IP6)
                IF (TCOUNT > 0.0) THEN
                    process_space_real(IP9) = process_space_real(IP9) / TCOUNT
                ELSE
                    process_space_real(IP9) = 0.0
                    process_space_real(IP10) = 0.0

                    IF (NOWARN < MAXWARN) THEN
                        CALL extract_waq_attribute(3, IKNMRK(ISEG), ATTRIB)
                        IF (ATTRIB /= 0) THEN
                            NOWARN = NOWARN + 1
                            WRITE(lunrep, '(a,i0)') 'Average could not be determined for segment ', ISEG
                            WRITE(lunrep, '(a)')    '    - segment not active in the given period. Average and standard deviation set to zero'

                            IF (NOWARN == MAXWARN) THEN
                                WRITE(lunrep, '(a)') '(Further messages suppressed)'
                            ENDIF
                        ENDIF
                    ENDIF
                ENDIF
                IF (TCOUNT > 1.0) THEN
                    !
                    !              If we do not have the standard deviation or the mean,
                    !              then the calculation may fail (horribly). We detect
                    !              whether the mean and the standard deviation are there
                    !              by examining the increments.
                    !              Be tolerant to possible negative values, there may be
                    !              some roundoff errors!
                    !
                    IF (IN9 /= 0 .AND. IN10 /= 0) THEN
                        CDIFF = (process_space_real(IP10) - TCOUNT * process_space_real(IP9)**2)
                        process_space_real(IP10) = SQRT(MAX(CDIFF, 0.0) / (TCOUNT - 1.0))
                    ENDIF
                ELSE
                    process_space_real(IP10) = 0.0

                    IF (NOWARN < MAXWARN) THEN
                        CALL extract_waq_attribute(3, IKNMRK(ISEG), ATTRIB)
                        IF (ATTRIB /= 0) THEN
                            NOWARN = NOWARN + 1
                            WRITE(lunrep, '(a,i0)') 'Standard deviation could not be determined for segment ', ISEG
                            WRITE(lunrep, '(a)')    '    - not enough values. Standard deviation set to zero'

                            IF (NOWARN == MAXWARN) THEN
                                WRITE(lunrep, '(a)') '(Further messages suppressed)'
                            ENDIF
                        ENDIF
                    ENDIF
                ENDIF
            ENDIF

            IP1 = IP1 + IN1
            IP6 = IP6 + IN6
            IP7 = IP7 + IN7
            IP8 = IP8 + IN8
            IP9 = IP9 + IN9
            IP10 = IP10 + IN10

        end do

        RETURN
    END

end module m_stadsc
