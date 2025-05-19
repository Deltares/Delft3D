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
module m_averad
    use m_waq_precision

    implicit none

contains


    SUBROUTINE AVERAD (process_space_real, FL, IPOINT, INCREM, num_cells, &
            NOFLUX, IEXPNT, IKNMRK, num_exchanges_u_dir, num_exchanges_v_dir, &
            num_exchanges_z_dir, num_exchanges_bottom_dir)
        use m_extract_waq_attribute

        IMPLICIT NONE

        !     arguments

        REAL(kind = real_wp) :: process_space_real(*)            ! in/out input-output array space to be adressed with IPOINT/INCREM
        REAL(kind = real_wp) :: FL(*)              ! in/out flux array
        INTEGER(kind = int_wp) :: IPOINT(*)          ! in     start index input-output parameters in the process_space_real array (segment or exchange number 1)
        INTEGER(kind = int_wp) :: INCREM(*)          ! in     increment for each segment-exchange for the input-output parameters in the process_space_real array
        INTEGER(kind = int_wp) :: num_cells              ! in     number of segments
        INTEGER(kind = int_wp) :: NOFLUX             ! in     total number of fluxes (increment in FL array)
        INTEGER(kind = int_wp) :: IEXPNT(4, *)        ! in     exchange pointer table
        INTEGER(kind = int_wp) :: IKNMRK(*)          ! in     segment features array
        INTEGER(kind = int_wp) :: num_exchanges_u_dir               ! in     number of exchanges in first direction
        INTEGER(kind = int_wp) :: num_exchanges_v_dir               ! in     number of exchanges in second direction
        INTEGER(kind = int_wp) :: num_exchanges_z_dir               ! in     number of exchanges in third direction
        INTEGER(kind = int_wp) :: num_exchanges_bottom_dir               ! in     number of exchanges in fourth direction

        !     from process_space_real array

        !REAL(kind = real_wp) :: RADSURF            ! 1  in  actual irradiation at the water surface            (W/m2)
        !REAL(kind = real_wp) :: TINIT              ! 2  in  DELWAQ time                                  (scu)
        !REAL(kind = real_wp) :: PERIOD             ! 3  in  Period of the periodic average                ()
        !REAL(kind = real_wp) :: TIME               ! 4  in  Time in calculation          (d)
        !REAL(kind = real_wp) :: DELT               ! 5  in  Timestep          (scu/d)
        !REAL(kind = real_wp) :: Sum_AVERAD         ! 6  in/out Work array for summing over time
        !INTEGER(kind = int_wp) :: TCOUNT           ! 7  in/out
        !REAL(kind = real_wp) :: RadSurfAve         ! 7  out average irradiance over the day              (W/m2)
        !DOUBLE PRECISION ::                  ! 8  out average irradiance                           (W/m2)
        !DOUBLE PRECISION :: RADTIME                ! 9  out average irradiance                           (W/m2)

        INTEGER(kind = int_wp) :: IP1, IP2, IP3, IP4, IP5, &
                IP6, IP7, IP8, IP9, IP10, &
                IN1, IN2, IN3, IN4, IN5, &
                IN6, IN7, IN8, IN9, IN10 
        INTEGER(kind = int_wp) :: IKMRK, ISEG
        INTEGER(kind = int_wp) :: IACTION, lunrep
        INTEGER(kind = int_wp) :: ATTRIB
        REAL(kind = real_wp) :: TINIT, PERIOD, TIME, DELT, TCOUNT

        INTEGER(kind = int_wp), PARAMETER :: MAXWARN = 50
        INTEGER(kind = int_wp), SAVE :: NOWARN = 0

        call get_log_unit_number(lunrep)
        
        !     IACTION is in 3 parts. 0, 2, 3.


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

        TINIT = process_space_real(IP2)
        PERIOD = process_space_real(IP3)
        TIME = process_space_real(IP4)
        DELT = process_space_real(IP5)
        !Sum_AVERAD = process_space_real(IP6)        ! Work array for summing over time
        !TCOUNT_AVERAD = process_space_real(IP7)      ! time

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
                    IP8 = IPOINT(8) + (ISEG - 1) * INCREM(8)
                    IP9 = IPOINT(9) + (ISEG - 1) * INCREM(9)
                    process_space_real(IP8) = 0.0
                    process_space_real(IP9) = 0.0
                ENDDO
            ENDIF
        ENDIF

        IF (TIME >= TINIT + PERIOD - 0.5 * DELT .AND. TIME <= TINIT + PERIOD + 0.5 * DELT) THEN
            IACTION = 3
        ENDIF

        IF (IACTION == 0) RETURN

        IP8 = IPOINT(8)
        IP9 = IPOINT(9)
        DO ISEG = 1, num_cells
                !
                !           Keep track of the time within the current quantile specification
                !           that each segment is active
                !
                TCOUNT = process_space_real(IP7) + DELT
                process_space_real(IP9) = TCOUNT

                process_space_real(IP8) = process_space_real(IP6) + process_space_real(IP1) * DELT

            !
            !        Always do the final processing whether the segment is active at this moment or not
            !

            IF (IACTION == 3) THEN
                IF (TCOUNT > 0.0) THEN
                    process_space_real(IP10) = process_space_real(IP8) / TCOUNT
                ELSE
                    process_space_real(IP10) = 0.0

                    IF (NOWARN < MAXWARN) THEN
                        CALL extract_waq_attribute(3, IKNMRK(ISEG), ATTRIB)
                        IF (ATTRIB /= 0) THEN
                            NOWARN = NOWARN + 1
                            WRITE(lunrep, '(a,i0)') 'Periodic average of RadSurf could not be determined for segment ', ISEG
                            WRITE(lunrep, '(a)')    '    - division by zero. Average set to zero'

                            IF (NOWARN == MAXWARN) THEN
                                WRITE(lunrep, '(a)') '(Further messages suppressed)'
                            ENDIF
                        ENDIF
                    ENDIF
                ENDIF

                !
                !           Reset for the next round
                !
                process_space_real(IP8) = 0.0
                process_space_real(IP9) = 0.0

            ENDIF

            IP1 = IP1 + IN1
            IP6 = IP6 + IN6
            IP7 = IP7 + IN7
            IP8 = IP8 + IN8
            IP9 = IP9 + IN9
            IP10 = IP10 + IN10
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
end module m_averad
