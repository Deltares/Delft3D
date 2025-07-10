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
module m_botmin
    use m_waq_precision

    implicit none

contains


    subroutine botmin (process_space_real, fl, ipoint, increm, num_cells, &
            noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
            num_exchanges_z_dir, num_exchanges_bottom_dir)
        use m_extract_waq_attribute

        !>\file
        !>       Mineralisation of organic substances and desorption of AAP in the bed (S1,S2) for C, N, P and Si.

        !
        !     Description of the module :
        !
        !        General water quality module for DELWAQ:
        !        GENERAL MINERALISATION OF ORGANIC SUBSTANCES IN THE BOTTOM LAYERS
        !        FOR CARBON, NITROGEN, PHOSPHORUS AND SILICATE). FORMULATION
        !        IS ZERO AND FIRST ORDER AND TEMPERATURE CORRECTED.
        !
        ! Name    T   L I/O   Description                                   Units
        ! ----    --- -  -    -------------------                            ----
        ! CRTEMP  R*4 1 I critical temperature for mineralisation             [xC]
        ! DEPTH   R*4 1 I actual depth of a segment                            [m]
        ! FL (1)  R*4 1 O mineralisation flux mixing layer (x=C,N,P,Si)  [gX/m3/d]
        ! MINRC   R*4 1 I first order mineralisation rate                    [1/d]
        ! MINTCR  R*4 1 I temperature coefficient two bottom layers          [1/d]
        ! ORG     R*4 1 I amount decaying organic material in mixing layer    [gX/m2]
        ! TEMP    R*4 1 I ambient temperature                                 [xC]
        ! TEMP20  R*4 1 L ambient temperature - stand. temp (20)              [xC]
        ! TEMPC   R*4 1 L temperature coefficient                              [-]
        ! VOLUME  R*4 1 L volume calculated by DELWAQ                         [m3]
        ! ZEMIN   R*4 1 I zeroth order mineralisation rate mixing layer  [gX/m2/d]

        !     Logical Units : -

        !     Modules called : -

        !     Name     Type   Library
        !     ------   -----  ------------

        IMPLICIT NONE

        REAL(kind = real_wp) :: process_space_real  (*), FL    (*)
        INTEGER(kind = int_wp) :: IPOINT(*), INCREM(*), num_cells, NOFLUX, &
                IEXPNT(4, *), IKNMRK(*), num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir

        INTEGER(kind = int_wp) :: IFLUX, ISEG, IKMRK2
        INTEGER(kind = int_wp) :: IN1, IN2, IN3, IN4, IN5, IN6, IN7, IN8, IN9, IN10
        INTEGER(kind = int_wp) :: IP1, IP2, IP3, IP4, IP5, IP6, IP7, IP8, IP9, IP10

        REAL(kind = real_wp) :: TEMP, CRTEMP, MINRC, MINTC, TEMP20, TEMFAK
        REAL(kind = real_wp) :: ZEMIN, ORG, VOLUME, DEPTH, SWITCH

        LOGICAL  TFACT

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
        !
        IF (IN3 == 0 .AND. IN4 == 0 .AND. &
                IN5 == 0 .AND. IN6 == 0) THEN
            TEMP = process_space_real(IP5)
            CRTEMP = process_space_real(IP6)
            IF (TEMP < CRTEMP) THEN
                !        Only the zeroth order term
                TEMFAK = 0.0
            ELSE
                MINRC = process_space_real(IP3)
                MINTC = process_space_real(IP4)
                TEMP20 = TEMP - 20.0
                TEMFAK = MINRC * MINTC ** TEMP20
            ENDIF
            TFACT = .FALSE.
        ELSE
            TFACT = .TRUE.
        ENDIF
        !
        IFLUX = 0
        DO ISEG = 1, num_cells

            IF (BTEST(IKNMRK(ISEG), 0)) THEN
                CALL extract_waq_attribute(2, IKNMRK(ISEG), IKMRK2)
                IF ((IKMRK2==0).OR.(IKMRK2==3)) THEN
                    !
                    IF (TFACT) THEN
                        TEMP = process_space_real(IP5)
                        CRTEMP = process_space_real(IP6)
                        IF (TEMP < CRTEMP) THEN
                            !        Only the zeroth order term
                            TEMFAK = 0.0
                        ELSE
                            MINRC = process_space_real(IP3)
                            MINTC = process_space_real(IP4)
                            TEMP20 = TEMP - 20.0
                            TEMFAK = MINRC * MINTC ** TEMP20
                        ENDIF
                    ENDIF
                    !

                    ZEMIN = process_space_real(IP1)
                    ORG = MAX (0.0, process_space_real(IP2))
                    VOLUME = process_space_real(IP7)
                    DEPTH = process_space_real(IP8)
                    SWITCH = process_space_real(IP9)

                    !***********************************************************************
                    !**** Processes connected to the MINERALISATION
                    !***********************************************************************
                    !
                    !
                    !        Calculation of mineralisation flux ( M.L-3.t-1)
                    !
                    process_space_real(IP10) = ZEMIN + TEMFAK * ORG
                    IF (ABS(SWITCH)<0.5) THEN
                        !       NO SWITCH
                        FL(1 + IFLUX) = ZEMIN / DEPTH + TEMFAK * ORG / DEPTH
                        FL(2 + IFLUX) = 0.0
                    ELSE
                        !       SWITCH
                        FL(1 + IFLUX) = 0.0
                        FL(2 + IFLUX) = ZEMIN / DEPTH + TEMFAK * ORG / DEPTH
                    ENDIF
                    !
                ENDIF
            ENDIF
            !
            IFLUX = IFLUX + NOFLUX
            IP1 = IP1 + IN1
            IP2 = IP2 + IN2
            IP3 = IP3 + IN3
            IP4 = IP4 + IN4
            IP5 = IP5 + IN5
            IP6 = IP6 + IN6
            IP7 = IP7 + IN7
            IP8 = IP8 + IN8
            IP9 = IP9 + IN9
            IP10 = IP10 + IN10
            !
        end do
        !
        RETURN
        !
    END

end module m_botmin
