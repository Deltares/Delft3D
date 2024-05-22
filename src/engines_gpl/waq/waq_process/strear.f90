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
module m_strear
    use m_waq_precision

    implicit none

contains


    subroutine strear (pmsa, fl, ipoint, increm, noseg, &
            noflux, iexpnt, iknmrk, noq1, noq2, &
            noq3, noq4)
        use m_logger_helper, only : stop_with_error, get_log_unit_number

        !>\file
        !>       Aeration at weirs (Gameson and Nakasone) (input is array of structures)

        !
        !     Description of the module :
        !
        ! Name    T   L I/O   Description                                   Units
        ! ----    --- -  -    -------------------                            ----
        ! NOSTR   R*4 1 I number of structures                                 [-]
        ! TEMP    R*4 1 I ambient temperature                                 [°C]
        ! OXYDN   R*4 1 I oxygen concentration downstream of structure     [gO/m3]
        ! OXYUP   R*4 1 I oxygen concentration upstream of structure       [gO/m3]
        ! BOD5    R*4 1 I calculated carbonaceous BOD at 5 days            [gO/m3]
        ! DELT    R*4 1 I DELWAQ timestep                                    [scu]
        ! CSAT    R*4 1 I saturation concentration of oxygen               [gO/m3]
        ! b       R*4 1 I dam reaeration coefficient                           [-]
        ! DISCH   R*4 1 I discharge over structure                          [m3/s]
        ! WIDTH   R*4 1 I width of structure                                   [m]
        ! DEPTH   R*4 1 I depth in delwaq segment                              [m]
        ! SWAER   R*4 1 I switch for gameson (0) or hybride (1)                [-]
        ! WLL     R*4 1 I water level in delwaq segment left of structure      [m]
        ! WLR     R*4 1 I water level in delwaq segment right of structure     [m]
        ! UPWL    R*4 1 L water level in delwaq segment upstream of structure  [m]
        ! DNWL    R*4 1 L water level in delwaq segment downstream of structure[m]
        ! SEGL    R*4 1 I segment number left of structure                     [-]
        ! SEGR    R*4 1 I segment number right of structure                    [-]
        ! UPSEG   R*4 1 L segment number upstream of structure                 [-]
        ! DNSEG   R*4 1 L segment number downstream of structure               [-]
        ! WLDIF   R*4 1 I difference in water level up- and downstream of struc[m]
        ! a       R*4 1 L water quality factor                                 [-]
        ! OXYDR   R*4 1 L oxygen deficit ratio                                 [-]
        ! LOGNAK  R*4 1 L logarithm of deficit ratio by Nakasone               [-]
        ! DRNAK   R*4 1 L oxygen deficit ratio by Nakasone                     [-]
        ! OXYCAL  R*4 1 L oxygen concentration after dam aeration            [g/d]
        ! OXYPL   R*4 1 O oxygen production flux at weirs               [gO2/m3/d]
        !
        !     Logical Units : -

        !     Modules called : -

        !     Name     Type   Library
        !     ------   -----  ------------
        !
        IMPLICIT NONE
        REAL(kind = real_wp) :: PMSA  (*), FL  (*)
        INTEGER(kind = int_wp) :: NOSEG, NOFLUX, NOQ1, NOQ2, NOQ3, NOQ4
        INTEGER(kind = int_wp) :: IPOINT(*), INCREM(*), &
                IEXPNT(4, *), IKNMRK(*)
        !
        INTEGER(kind = int_wp) :: IP1, IP2, IP3, IP4, IP5, IP6, IP7, IP8, &
                IN1, IN2, IN3, IN4, IN5, IN6, IN7, IN8
        INTEGER(kind = int_wp) :: ITEL, ISTRUC, NOSTR, SEGL, SEGR, UPSEG, DNSEG
        INTEGER(kind = int_wp) :: LUNREP
        REAL(kind = real_wp) :: WLL, WLR
        REAL(kind = real_wp) :: OXYDN, OXYUP, DELT, CSAT, BOD5, B, UPWL, DNWL, &
                TEMP, DISCH, WIDTH, DEPTH, WLDIF, A, SWAER, &
                OXYDR, LOGNAK, DRNAK, OXYCAL, OXYPL
        !
        IP1 = IPOINT(1)
        IP2 = IPOINT(2)
        IP3 = IPOINT(3)
        IP4 = IPOINT(4)
        IP5 = IPOINT(5)
        IP6 = IPOINT(6)
        IP7 = IPOINT(7)
        IP8 = IPOINT(8)
        !
        IN1 = INCREM(1)
        IN2 = INCREM(2)
        IN3 = INCREM(3)
        IN4 = INCREM(4)
        IN5 = INCREM(5)
        IN6 = INCREM(6)
        IN7 = INCREM(7)
        IN8 = INCREM(8)
        !
        NOSTR = NINT(PMSA(IP1))
        !
        IF (NOSTR > 100.0) THEN
            CALL get_log_unit_number(lunrep)
            write(lunrep, *) 'Error: Number of structures', &
                    ' greater than 100'
            CALL stop_with_error()
        ENDIF
        !
        !     segment loop over structures-------------------------------
        !
        DO ISTRUC = 1, NOSTR
            !
            !
            ITEL = 8 + (ISTRUC - 1) * 7
            !
            !        read input structure---------------------------------------
            !
            DISCH = PMSA(IPOINT(ITEL + 1))
            WLL = PMSA(IPOINT(ITEL + 2))
            WLR = PMSA(IPOINT(ITEL + 3))
            SEGL = NINT(PMSA(IPOINT(ITEL + 4)))
            SEGR = NINT(PMSA(IPOINT(ITEL + 5)))
            b = PMSA(IPOINT(ITEL + 6))
            WIDTH = PMSA(IPOINT(ITEL + 7))
            !
            !        Oxygen production if discharge over crest is larger than
            !        zero. Aeration takes place in downstream segment-----------
            !
            IF (DISCH > 0.0) THEN
                UPSEG = SEGL
                DNSEG = SEGR
                UPWL = WLL
                DNWL = WLR
            ELSEIF (DISCH < 0.0) THEN
                UPSEG = SEGR
                DNSEG = SEGL
                UPWL = WLR
                DNWL = WLL
            ELSE
                GOTO 9000
            ENDIF
            !
            WLDIF = UPWL - DNWL
            !
            !        reading input of segment downstream of structure-----------
            OXYDN = MAX(0.0, PMSA(IP2 + (DNSEG - 1) * IN2))
            DELT = PMSA(IP3 + (DNSEG - 1) * IN3)
            CSAT = PMSA(IP4 + (DNSEG - 1) * IN4)
            BOD5 = MAX(0.0, PMSA(IP5 + (DNSEG - 1) * IN5))
            TEMP = PMSA(IP6 + (DNSEG - 1) * IN6)
            DEPTH = PMSA(IP7 + (DNSEG - 1) * IN7)
            SWAER = PMSA(IP8 + (DNSEG - 1) * IN8)
            !

            !        reading input of segment upstream of structure-----------
            !
            OXYUP = MAX(0.0, PMSA(IP2 + (UPSEG - 1) * IN2))
            !
            !        calculation a, a factor for the contamination of the water-
            !
            a = MIN(1.90 / (BOD5 ** 0.44), 1.80)
            !
            !        Switch for gameson (SWAER = 0) or hybride of gameson and
            !        nakasone (SWAER = 1)-----------------------------------
            !
            IF (SWAER < 0.5) THEN
                !
                !            SWAER = 0, gameson---------------------------------
                !
                OXYDR = 1.0 + 0.38 * a * b * WLDIF * &
                        (1 - 0.11 * WLDIF) * (1 + 0.046 * TEMP)
                !
            ELSE
                !
                !            SWAER = 1, hybride of gameson and nakasone---------
                !
                LOGNAK = 0.0675 * (WLDIF ** 1.28) * &
                        ((DISCH / WIDTH * 3600) ** 0.62) * &
                        (DEPTH ** 0.439)

                !
                DRNAK = EXP(LOGNAK)

                !
                OXYDR = ((DRNAK - 1) * a * b * (1 + 0.02 * &
                        (TEMP - 20))) + 1
                !
            ENDIF

            !
            !        Oxygen concentration downstream of structure-----------
            !
            OXYCAL = (CSAT * (OXYDR - 1) + OXYUP) / OXYDR
            !
            !        Calculate flux-----------------------------------------
            !
            OXYPL = (OXYCAL - OXYDN) / DELT
            FL(1 + (DNSEG - 1) * NOFLUX) = OXYPL
            !
            9000 CONTINUE
        end do
        !
        RETURN
        !
    END

end module m_strear
