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
module m_staqtl
    use m_waq_precision

    implicit none

contains


    subroutine staqtl (pmsa, fl, ipoint, increm, noseg, &
            noflux, iexpnt, iknmrk, noq1, noq2, &
            noq3, noq4)
        use m_srstop
        use m_monsys
        use m_evaluate_waq_attribute

        !>\file
        !>       Quantiles for a given substance during a given period

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
        ! NOBUCK         I    Number of buckets to use                  6
        ! CLOBND         I    Expected lower bound                      7
        ! CUPBND         I    Expected upper bound                      8
        ! CQLEV          I    Quantile (in %) to be reported            9
        ! TCOUNT       I/O    Count of times (must be imported!)       10
        ! CWRK01       I/O    First work array                    10 +  1
        !    ...
        ! CWRKxx       I/O    Last work array (NOBUCK in total)   10 + NOBUCK
        ! CQUANT         O    Estimated quantile                  11 + NOBUCK
        !
        !     Logical Units : -

        !     Modules called : -

        !     Name     Type   Library
        !     ------   -----  ------------

        IMPLICIT NONE

        REAL(kind = real_wp) :: PMSA  (*), FL    (*)
        INTEGER(kind = int_wp) :: IPOINT(*), INCREM(*), NOSEG, NOFLUX, &
                IEXPNT(4, *), IKNMRK(*), NOQ1, NOQ2, NOQ3, NOQ4
        !
        INTEGER(kind = int_wp) :: IP1, IP2, IP3, IP4, IP5, &
                IP6, IP7, IP8, IP9, IP10, &
                IP11, IP, &
                IN1, IN2, IN3, IN4, IN5, &
                IN6, IN7, IN8, IN9, IN10, &
                IN11
        INTEGER(kind = int_wp) :: ISEG
        INTEGER(kind = int_wp) :: IB, IPBUCK, IPTCNT, LUNREP, IACTION, ATTRIB

        INTEGER(kind = int_wp) :: NOBUCK
        INTEGER(kind = int_wp) :: MAXBCK
        PARAMETER (MAXBCK = 101)
        INTEGER(kind = int_wp) :: IBUCK(MAXBCK), INCBCK(MAXBCK)
        REAL(kind = real_wp) :: BCKLIM(MAXBCK)
        REAL(kind = real_wp) :: BMIN, BMAX, BDIFF, BSUM
        REAL(kind = real_wp) :: PQUANT
        REAL(kind = real_wp) :: TSTART, TSTOP, TIME, DELT, TCOUNT

        INTEGER(kind = int_wp), PARAMETER :: MAXWARN = 50
        INTEGER(kind = int_wp), SAVE :: NOWARN = 0

        IP1 = IPOINT(1)
        IP2 = IPOINT(2)
        IP3 = IPOINT(3)
        IP4 = IPOINT(4)
        IP5 = IPOINT(5)
        IP6 = IPOINT(6)
        NOBUCK = NINT(PMSA(IP6)) + 1
        IP7 = IPOINT(7)
        IP8 = IPOINT(8)
        IP9 = IPOINT(9)
        IP10 = IPOINT(10)
        IP11 = IPOINT(11 + NOBUCK)
        !
        !     Names for indices that turn up in various places
        !
        IPTCNT = IP10
        IPBUCK = 11

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
        IN11 = INCREM(11 + NOBUCK)

        BMIN = PMSA(IP7)
        BMAX = PMSA(IP8)

        IF (NOBUCK > MAXBCK) THEN
            CALL GETMLU(LUNREP)
            WRITE(LUNREP, *) 'ERROR in STAQTL'
            WRITE(LUNREP, *) &
                    'Number of buckets too large'
            WRITE(LUNREP, *) &
                    'Number of buckets: ', NOBUCK - 1, ' - maximum: ', MAXBCK - 1
            CALL SRSTOP(1)
        ENDIF

        BDIFF = (BMAX - BMIN) / REAL(NOBUCK - 1)

        DO IB = 1, NOBUCK
            IBUCK(IB) = IPOINT(IPBUCK + IB)
            INCBCK(IB) = INCREM(IPBUCK + IB)
            BCKLIM(IB) = BMIN + REAL(IB - 1) * BDIFF
        ENDDO

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
        TSTART = PMSA(IP2)
        TSTOP = PMSA(IP3)
        TIME = PMSA(IP4)
        DELT = PMSA(IP5)

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
                DO ISEG = 1, NOSEG
                    IP = IPOINT(10) + (ISEG - 1) * INCREM(10)
                    PMSA(IP) = 0.0

                    DO IB = 1, NOBUCK
                        IP = IPOINT(IPBUCK + IB) + (ISEG - 1) * INCREM(IPBUCK + IB)
                        PMSA(IP) = 0.0
                    ENDDO

                ENDDO
            ENDIF
        ENDIF

        IF (TIME >= TSTOP - 0.5 * DELT .AND. TIME <= TSTOP + 0.5 * DELT) THEN
            IACTION = 3
        ENDIF

        IF (IACTION == 0) RETURN

        DO ISEG = 1, NOSEG
            IF (BTEST(IKNMRK(ISEG), 0)) THEN
                !
                !           Keep track of the time within the current quantile specification
                !           that each segment is active
                !
                TCOUNT = PMSA(IPTCNT) + DELT
                PMSA(IPTCNT) = TCOUNT

                DO IB = 1, NOBUCK
                    IF (PMSA(IP1) <= BCKLIM(IB)) THEN
                        PMSA(IBUCK(IB)) = PMSA(IBUCK(IB)) + DELT
                        EXIT
                    ENDIF
                ENDDO
            ENDIF

            !
            !        Always do the final processing whether the segment is active at this moment or not
            !

            IF (IACTION == 3) THEN
                !
                !           Determine the length of the period for the quantile
                !
                PQUANT = PMSA(IP9) * 0.01 * TCOUNT
                !
                !           Accumulate the values in the buckets until we add up
                !           to at least the requested percentage of total time.
                !           Then interpolate assuming a uniform distribution
                !           within each bucket.
                !           Special note:
                !           If the ranges have been set wrongly, then the
                !           outer buckets will contain the quantile. In that
                !           case: use the lower and upper bounds.
                !
                PMSA(IP11) = -999.0
                BSUM = PMSA(IBUCK(1))
                IF (BSUM < PQUANT) THEN
                    DO IB = 2, NOBUCK
                        BSUM = BSUM + PMSA(IBUCK(IB))
                        IF (BSUM >= PQUANT) THEN
                            PMSA(IP11) = BCKLIM(IB) - &
                                    (BSUM - PQUANT) * BDIFF / PMSA(IBUCK(IB))
                            EXIT
                        ENDIF
                    ENDDO

                    IF (BSUM < PQUANT) THEN
                        PMSA(IP11) = BMAX

                        IF (NOWARN < MAXWARN) THEN
                            NOWARN = NOWARN + 1
                            WRITE(*, '(a,i0)')      'Quantile could not be determined for segment ', ISEG
                            WRITE(*, '(a,e12.4,a)') '    - too many values above ', BMAX, ' (assuming this value)'

                            IF (NOWARN == MAXWARN) THEN
                                WRITE(*, '(a)') '(Further messages suppressed)'
                            ENDIF
                        ENDIF
                    ENDIF
                ELSE
                    PMSA(IP11) = BMIN

                    IF (NOWARN < MAXWARN) THEN
                        CALL evaluate_waq_attribute(IKNMRK(ISEG), 3, ATTRIB)
                        IF (ATTRIB /= 0) THEN
                            NOWARN = NOWARN + 1
                            WRITE(*, '(a,i0)')    'Quantile could not be determined for segment ', ISEG
                            WRITE(*, '(a,e12.4)') '    - too many values below ', BMIN, ' (assuming this value)'

                            IF (NOWARN == MAXWARN) THEN
                                WRITE(*, '(a)') '(Further messages suppressed)'
                            ENDIF
                        ENDIF
                    ENDIF
                ENDIF
            ENDIF

            IP1 = IP1 + IN1
            IPTCNT = IPTCNT + IN10
            IP11 = IP11 + IN11

            DO IB = 1, NOBUCK
                IBUCK(IB) = IBUCK(IB) + INCBCK(IB)
            ENDDO

        end do

        RETURN
    END

end module m_staqtl
