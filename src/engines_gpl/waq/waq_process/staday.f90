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
module m_staday
    use m_waq_precision
    use m_logger_helper, only: get_log_unit_number

    implicit none

contains


    subroutine staday (process_space_real, fl, ipoint, increm, num_cells, &
            noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
            num_exchanges_z_dir, num_exchanges_bottom_dir)
        use m_extract_waq_attribute

        !>\file
        !>       Periodic (day) average, minimum and maximum of a given substance

        !
        !     Description of the module :
        !
        ! Name    T   L I/O   Description                                  Units
        ! ----    --- -  -    -------------------                          -----
        !
        ! CONC           I    Concentration of the substance            1
        ! TINIT         I/O   Initial time (reset at end period)        2
        ! PERIOD         I    Period of the periodic average            3
        ! TIME           I    Time in calculation                       4
        ! DELT           I    Timestep                                  5
        !
        ! TCOUNT         O    Count of times (must be imported!)        6
        ! AVCUM          O    Work array for summing over time          7
        ! MINDYN         O    Dynamic minimum over period               8
        ! MAXDYN         O    Dynamic maximum over period               9
        ! AVPERD         O    Periodic average (calculated at the end)  10
        ! MINPERD        O    Periodic minimum (given at the end)       11
        ! MAXPERD        O    Periodic maximum (given at the end)       12
        !
        ! Note: to prevent strange results, the actual output parameter is
        !       AVPERD. This is updated once in a while!
        ! Note: The result (unit 10, 11 and 12) is of the previous day.
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
                IP11, IP12, &
                IN1, IN2, IN3, IN4, IN5, &
                IN6, IN7, IN8, IN9, IN10, &
                IN11, IN12
        INTEGER(kind = int_wp) :: IKMRK, ISEG
        INTEGER(kind = int_wp) :: IACTION, lunrep
        INTEGER(kind = int_wp) :: ATTRIB
        REAL(kind = real_wp) :: TINIT, PERIOD, TIME, DELT, TCOUNT

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
        IP11 = IPOINT(11)
        IP12 = IPOINT(12)

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
        IN11 = INCREM(11)
        IN12 = INCREM(12)

        TINIT = process_space_real(IP2)
        PERIOD = process_space_real(IP3)
        TIME = process_space_real(IP4)
        DELT = process_space_real(IP5)
        TCOUNT = process_space_real(IP6)

        !
        !      Start and stop criteria are somewhat involved:
        !      - The first time for the first period is special, as this
        !        is the only time there is no previous period.
        !      - If there is a previous period, update the averages
        !        for that period and reset the accumulative values
        !        for the next
        !
        !      To formulate the ideas more clearly:
        !      - The first period is a closed interval
        !      - All other periods are half-open intervals (the last time
        !        of the previous period should not be reused.)
        !
        IACTION = 0
        IF (TIME >= TINIT - 0.5 * DELT) THEN
            IACTION = 2
            IF (TIME <= TINIT + 0.5 * DELT) THEN
                DO ISEG = 1, num_cells
                    IP6 = IPOINT(6) + (ISEG - 1) * INCREM(6)
                    IP7 = IPOINT(7) + (ISEG - 1) * INCREM(7)
                    IP8 = IPOINT(8) + (ISEG - 1) * INCREM(8)
                    IP9 = IPOINT(9) + (ISEG - 1) * INCREM(9)
                    process_space_real(IP6) = 0.0
                    process_space_real(IP7) = 0.0
                    process_space_real(IP8) = HUGE(1.0)
                    process_space_real(IP9) = -HUGE(1.0)
                ENDDO
            ENDIF
        ENDIF

        IF (TIME >= TINIT + PERIOD - 0.5 * DELT .AND. TIME <= TINIT + PERIOD + 0.5 * DELT) THEN
            IACTION = 3
        ENDIF

        IF (IACTION == 0) RETURN

        IP6 = IPOINT(6)
        IP7 = IPOINT(7)
        IP8 = IPOINT(8)
        IP9 = IPOINT(9)

        DO ISEG = 1, num_cells
            IF (BTEST(IKNMRK(ISEG), 0)) THEN

                !
                !           Keep track of the time within the current quantile specification
                !           that each segment is active
                !
                TCOUNT = process_space_real(IP6) + DELT
                process_space_real(IP6) = TCOUNT

                process_space_real(IP7) = process_space_real(IP7) + process_space_real(IP1) * DELT
                process_space_real(IP8) = MIN(process_space_real(IP8), process_space_real(IP1))
                process_space_real(IP9) = MAX(process_space_real(IP9), process_space_real(IP1))

            ENDIF
            !
            !        Always do the final processing whether the segment is active at this moment or not
            !

            IF (IACTION == 3) THEN
                IF (TCOUNT > 0.0) THEN
                    process_space_real(IP10) = process_space_real(IP7) / TCOUNT
                    process_space_real(IP11) = process_space_real(IP8)
                    process_space_real(IP12) = process_space_real(IP9)
                ELSE
                    process_space_real(IP10) = 0.0
                    process_space_real(IP11) = 0.0
                    process_space_real(IP12) = 0.0

                    IF (NOWARN < MAXWARN) THEN
                        CALL extract_waq_attribute(3, IKNMRK(ISEG), ATTRIB)
                        IF (ATTRIB /= 0) THEN
                            NOWARN = NOWARN + 1
                            WRITE(lunrep, '(a,i0)') 'Periodic average, minimum and maximum could not be determined for segment ', ISEG
                            WRITE(lunrep, '(a)')    '    - segment was not active. Average set to zero'

                            IF (NOWARN == MAXWARN) THEN
                                WRITE(lunrep, '(a)') '(Further messages suppressed)'
                            ENDIF
                        ENDIF
                    ENDIF
                ENDIF

                !
                !           Reset for the next round
                !
                process_space_real(IP6) = 0.0
                process_space_real(IP7) = 0.0
                process_space_real(IP8) = HUGE(1.0)
                process_space_real(IP9) = -HUGE(1.0)

            ENDIF

            IP1 = IP1 + IN1
            IP6 = IP6 + IN6
            IP7 = IP7 + IN7
            IP8 = IP8 + IN8
            IP9 = IP9 + IN9
            IP10 = IP10 + IN10
            IP11 = IP11 + IN11
            IP12 = IP12 + IN12

        end do

        !
        !     Be sure to also reset the initial time, so that we can restart the
        !     averaging for the next period
        !
        IF (IACTION == 3) THEN
            process_space_real(IP2) = TINIT + PERIOD
        ENDIF

        RETURN
    END

end module m_staday
