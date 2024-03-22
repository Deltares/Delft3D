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
module m_dsptra
    use m_waq_precision

    implicit none

contains


    subroutine dsptra (pmsa, fl, ipoint, increm, noseg, &
            noflux, iexpnt, iknmrk, noq1, noq2, &
            noq3, noq4)
        !>\file
        !>       Dispersion/diffusion in the sediment

        !
        !     Description of the module :
        !
        !        General water quality module for DELWAQ:
        !        BIOTURBATION/BIO-IRRIGATION BETWEEN SEDIMENT LAYERS
        !
        ! Name    T   L I/O   Description                                    Units
        ! ----    --- -  -    -------------------                            -----
        ! Coll Struct 1  O    Structure with collection of bottom collumn info
        !                  Contains:
        !    type(BotColmn), pointer :: set(:)  ! array with info for all bottom collumns
        !    integer                 :: maxsize ! maximum size of the current array
        !    integer                 :: cursize ! filled up to this size
        ! BotColm Struct 1   O  Structure with bottom collumn info
        !                  Contains:
        !    integer :: fstwatsed  ! first water sediment exchange number
        !    integer :: lstwatsed  ! last  water sediment exchange number
        !    integer :: topsedsed  ! first within collumn exchange number
        !    integer :: botsedsed  ! last exchange of collumn to deeper bnd

        !     Logical Units : -

        !     Modules called : -

        !     Name     Type   Library
        !     ------   -----  ------------
        use m_advtra
        USE BottomSet     !  Module with derived types and add function

        !     type ( BotColmnColl ) :: Coll  <= is defined in the module

        IMPLICIT REAL (A-H, J-Z)

        REAL(kind = real_wp) :: PMSA  (*), FL    (*)
        INTEGER(kind = int_wp) :: IPOINT(*), INCREM(*), NOSEG, NOFLUX, &
                IEXPNT(4, *), IKNMRK(*), NOQ1, NOQ2, NOQ3, NOQ4

        INTEGER(kind = int_wp) :: IP1, IP2, IP3, IP4, IP5, IP6, IP7
        INTEGER(kind = int_wp) :: IN1, IN2, IN3, IN4, IN5, IN6, IN7
        INTEGER(kind = int_wp) :: IVAN, INAAR, IK, IQ
        INTEGER(kind = int_wp) :: IWA1, IWA2, ITOP, IBOT, IOFFSE
        REAL(kind = real_wp) :: TURCOE, DIFCOE, VD_SOL, VU_SOL, &
                DIFLEN, ACTHS1, ACTHS2, POROS1, POROS2, &
                XFROM, XTO, VD_DIS, VU_DIS

        !     Include column structure
        !     we define a double column structure, one for downward,
        !     and one for upward transport

        CALL MAKKO2 (IEXPNT, IKNMRK, NOQ1, NOQ2, NOQ3, &
                NOQ4)

        IP1 = IPOINT(1)
        IP2 = IPOINT(2)
        IP3 = IPOINT(3)
        IP4 = IPOINT(4)
        IP5 = IPOINT(5)
        IP6 = IPOINT(6)
        IP7 = IPOINT(7)
        !
        IN1 = INCREM(1)
        IN2 = INCREM(2)
        IN3 = INCREM(3)
        IN4 = INCREM(4)
        IN5 = INCREM(5)
        IN6 = INCREM(6)
        IN7 = INCREM(7)

        !.....Segmentloop om uitvoergrootheden op segmentniveau op 0 te zetten
        !     DO 9000 ISEG=1,NOSEG
        !9000 CONTINUE
        !
        !.....Exchangeloop over de horizontale richtingen om op 0 te zetten
        !.....en om de pointers te zetten
        DO IQ = 1, NOQ1 + NOQ2
            !         Uitvoeritems op exchange level
            PMSA(IP6) = 0.0
            PMSA(IP7) = 0.0
            IP6 = IP6 + IN6
            IP7 = IP7 + IN7
        end do
        IP6 = IPOINT(6)
        IP7 = IPOINT(7)
        !
        !.....Loop over kolommen
        DO IK = 1, Coll%cursize

            !         Select first column of exchanges for DOWNWARD advection

            IWA1 = Coll%set(IK)%fstwatsed
            IWA2 = Coll%set(IK)%lstwatsed
            ITOP = Coll%set(IK)%topsedsed
            IBOT = Coll%set(IK)%botsedsed

            !        Offset to reach second colum for UPWARD advection

            IOFFSE = IBOT - (IWA1 - 1)

            !        Loop over exchanges

            DO IQ = IWA1, IBOT

                IVAN = IEXPNT(1, IQ)
                INAAR = IEXPNT(2, IQ)
                TURCOE = PMSA(IP4 + (Ivan - 1) * IN4)
                DIFCOE = PMSA(IP5 + (Inaar - 1) * IN5)

                IF (IQ <= IWA2) THEN

                    !.....WATER-SEDIMENT INTERFACE

                    DIFLEN = PMSA(IP2 + (IVAN - 1) * IN2)
                    ACTHS2 = PMSA(IP1 + (INAAR - 1) * IN1)
                    POROS1 = PMSA(IP3 + (IVAN - 1) * IN3)
                    POROS2 = PMSA(IP3 + (INAAR - 1) * IN3)
                    XFROM = DIFLEN
                    XTO = 0.5 * ACTHS2
                    VD_SOL = 0.0
                    VU_SOL = 0.0
                    VD_DIS = DIFCOE * MIN(POROS1, POROS2) / POROS1 / (XFROM + XTO)
                    VU_DIS = -DIFCOE * MIN(POROS1, POROS2) / POROS2 / (XFROM + XTO)

                ELSEIF (IQ == IBOT) THEN

                    !.....DEEP SEDIMENT BOUNDARY

                    ACTHS1 = PMSA(IP1 + (IVAN - 1) * IN1)
                    POROS1 = PMSA(IP3 + (IVAN - 1) * IN3)
                    POROS2 = POROS1
                    XFROM = 0.5 * ACTHS1
                    XTO = 0.5 * ACTHS1
                    VD_SOL = TURCOE * MIN((1. - POROS1), (1. - POROS2)) / (1. - POROS1) / &
                            (XFROM + XTO)
                    VU_SOL = -TURCOE * MIN((1. - POROS1), (1. - POROS2)) / (1. - POROS2) / &
                            (XFROM + XTO)
                    VD_DIS = 0.0
                    VU_DIS = 0.0

                ELSE

                    !.....SEDIMENT-SEDIMENT INTERFACE

                    ACTHS1 = PMSA(IP1 + (IVAN - 1) * IN1)
                    ACTHS2 = PMSA(IP1 + (INAAR - 1) * IN1)
                    POROS1 = PMSA(IP3 + (IVAN - 1) * IN3)
                    POROS2 = PMSA(IP3 + (INAAR - 1) * IN3)
                    XFROM = 0.5 * ACTHS1
                    XTO = 0.5 * ACTHS2
                    VD_SOL = TURCOE * MIN((1. - POROS1), (1. - POROS2)) / (1. - POROS1) / &
                            (XFROM + XTO)
                    VU_SOL = -TURCOE * MIN((1. - POROS1), (1. - POROS2)) / (1. - POROS2) / &
                            (XFROM + XTO)
                    VD_DIS = DIFCOE * MIN(POROS1, POROS2) / POROS1 / (XFROM + XTO)
                    VU_DIS = -DIFCOE * MIN(POROS1, POROS2) / POROS2 / (XFROM + XTO)

                ENDIF

                PMSA(IP6 + (IQ - 1) * IN6) = VD_SOL / 86400.
                PMSA(IP6 + (IQ - 1 + IOFFSE) * IN6) = VU_SOL / 86400.
                PMSA(IP7 + (IQ - 1) * IN7) = VD_DIS / 86400.
                PMSA(IP7 + (IQ - 1 + IOFFSE) * IN7) = VU_DIS / 86400.

            ENDDO

        end do

        RETURN
    END

end module m_dsptra
