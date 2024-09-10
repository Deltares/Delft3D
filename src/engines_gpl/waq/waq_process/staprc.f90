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
module m_staprc
    use m_waq_precision

    implicit none

contains


    subroutine staprc (process_space_real, fl, ipoint, increm, num_cells, &
            noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
            num_exchanges_z_dir, num_exchanges_bottom_dir)
        use m_extract_waq_attribute

        !>\file
        !>       Exceedence frequency, its complement and the mean

        !
        !     Description of the module :
        !
        !        General water quality module for DELWAQ:
        !
        ! Name    T   L I/O   Description                                  Units
        ! ----    --- -  -    -------------------                          -----
        !
        ! CONC           I    Concentration of the substance              1
        ! TSTART         I    Start of statistical period                 2
        ! TSTOP          I    Stop of statistical period                  3
        ! TIME           I    Time in calculation                         4
        ! DELT           I    Timestep                                    5
        ! CCRIT          I    Critical value                              6
        ! ABOVE          I    Whether to register values above or below   7
        !
        ! TCOUNT         O    Count of timesteps                          8
        ! CEXCD          O    Fraction of the time the value is exceeded  9
        ! CEXMN          O    Mean value over that period                10
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
        INTEGER(kind = int_wp) :: IACTION
        INTEGER(kind = int_wp) :: ATTRIB
        REAL(kind = real_wp) :: TSTART, TSTOP, TIME, DELT
        REAL(kind = real_wp) :: CCRIT, TCOUNT
        REAL(kind = real_wp) :: ABOVE, BELOW

        INTEGER(kind = int_wp), PARAMETER :: MAXWARN = 50
        INTEGER(kind = int_wp), SAVE :: NOWARN = 0

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
        CCRIT = process_space_real(IP6)
        IF (process_space_real(IP7) == 1.0) THEN
            ABOVE = DELT
            BELOW = 0.0
        ELSE
            ABOVE = 0.0
            BELOW = DELT
        ENDIF

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
                    IP8 = IPOINT(8) + (ISEG - 1) * INCREM(8)
                    IP9 = IPOINT(9) + (ISEG - 1) * INCREM(9)
                    IP10 = IPOINT(10) + (ISEG - 1) * INCREM(10)
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

        IP8 = IPOINT(8)
        IP9 = IPOINT(9)
        IP10 = IPOINT(10)

        DO ISEG = 1, num_cells
            IF (BTEST(IKNMRK(ISEG), 0)) THEN

                !
                !           Keep track of the time within the current exceedance specification
                !           that each segment is active
                !
                TCOUNT = process_space_real(IP8) + DELT
                process_space_real(IP8) = TCOUNT

                IF (process_space_real(IP1) >= CCRIT) THEN
                    process_space_real(IP9) = process_space_real(IP9) + ABOVE
                    process_space_real(IP10) = process_space_real(IP10) + ABOVE * process_space_real(IP1)
                ELSE
                    process_space_real(IP9) = process_space_real(IP9) + BELOW
                    process_space_real(IP10) = process_space_real(IP10) + BELOW * process_space_real(IP1)
                ENDIF
            ENDIF

            !
            !        Always do the final processing whether the segment is active at this moment or not
            !
            IF (IACTION == 3) THEN
                IF (TCOUNT > 0.0) THEN
                    IF (process_space_real(IP9) > 0) THEN
                        process_space_real(IP10) = process_space_real(IP10) / process_space_real(IP9)
                    ENDIF
                    process_space_real(IP9) = process_space_real(IP9) / TCOUNT
                ELSE
                    process_space_real(IP9) = 0.0

                    IF (NOWARN < MAXWARN) THEN
                        CALL extract_waq_attribute(3, IKNMRK(ISEG), ATTRIB)
                        IF (ATTRIB /= 0) THEN
                            NOWARN = NOWARN + 1
                            WRITE(*, '(a,i0)')      'Exceedance could not be determined for segment ', ISEG
                            WRITE(*, '(a,e12.4,a)') '    - segment not active in the given period. Exceedance set to zero'

                            IF (NOWARN == MAXWARN) THEN
                                WRITE(*, '(a)') '(Further messages suppressed)'
                            ENDIF
                        ENDIF
                    ENDIF

                ENDIF
            ENDIF

            IP1 = IP1 + IN1
            IP8 = IP8 + IN8
            IP9 = IP9 + IN9
            IP10 = IP10 + IN10

        end do

        RETURN
    END

end module m_staprc
