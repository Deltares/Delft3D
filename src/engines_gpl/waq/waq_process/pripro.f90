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
module m_pripro
    use m_waq_precision

    implicit none

contains


    subroutine pripro (pmsa, fl, ipoint, increm, noseg, &
            noflux, iexpnt, iknmrk, noq1, noq2, &
            noq3, noq4)
        use m_logger

        !>\file
        !>       Nett primary production and mortality DYNAMO algae

        !
        !     Description of the module :
        !
        ! Name    T   L I/O   Description                                   Unit
        ! ----    --- -  -    -------------------                            ---
        ! DL      R*4 1 I daylength for growth saturation green-algae          [
        ! EFF     R*4 1 L average light efficiency green-algae                 [
        ! FNUT    R*4 1 L nutrient limitation function green-algae             [
        ! PPMAX1  R*4 1 I pot. max. pr. prod. rc. green-algae (st.temp)      [1/
        ! PMSA    R*4 1 L Gross act. pr. prod. rc. green-algae               [1/
        ! TFUNG1  R*4 1 L temp. function for growth processes green            [

        !     Logical Units : -

        !     Modules called : -

        !     Name     Type   Library
        !     ------   -----  ------------

        IMPLICIT REAL    (A-H, J-Z)
        IMPLICIT INTEGER (I)

        REAL(kind = real_wp) :: PMSA  (*), FL    (*)
        INTEGER(kind = int_wp) :: IPOINT(*), INCREM(*), NOSEG, NOFLUX, &
                IEXPNT(4, *), IKNMRK(*), NOQ1, NOQ2, NOQ3, NOQ4
        !
        !     Local declaration
        !
        integer(kind = int_wp) :: iseg
        REAL(kind = real_wp) :: ALGMIN
        INTEGER(kind = int_wp) :: NR_MES
        SAVE     NR_MES
        DATA     NR_MES / 0 /
        !
        CALL get_log_unit_number(ILUMON)
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
        IP11 = IPOINT(11)
        IP12 = IPOINT(12)
        IP13 = IPOINT(13)
        IP14 = IPOINT(14)
        IP15 = IPOINT(15)
        IP16 = IPOINT(16)
        IP17 = IPOINT(17)
        IP18 = IPOINT(18)
        IP19 = IPOINT(19)
        IP20 = IPOINT(20)
        !
        IFLUX = 0
        DO ISEG = 1, NOSEG

            IF (BTEST(IKNMRK(ISEG), 0)) THEN
                !
                ALG = PMSA(IP1)
                IF (ALG < 0.0) THEN
                    IF (NR_MES < 25) THEN
                        NR_MES = NR_MES + 1
                        WRITE (ILUMON, *) 'WARNING :negative algae correction', &
                                ' segment=', ISEG, ' conc=', ALG
                    ENDIF
                    IF (NR_MES == 25) THEN
                        NR_MES = NR_MES + 1
                        WRITE(ILUMON, *) ' 25 WARNINGS on negative algae'
                        WRITE(ILUMON, *) ' Further messages on algae surpressed'
                    ENDIF
                    ALG = 0.0
                ENDIF
                DL = PMSA(IP2)
                FNUT = PMSA(IP3)
                EFF = PMSA(IP4)
                TFUNG = PMSA(IP5)
                TFUNM = PMSA(IP6)
                PPMAX = PMSA(IP7)
                MRESP = PMSA(IP8)
                GRESP = PMSA(IP9)
                MORT0 = PMSA(IP10)
                MORTS = PMSA(IP11)
                SAL1 = PMSA(IP12)
                SAL2 = PMSA(IP13)
                SAL = PMSA(IP14)
                ALGMIN = PMSA(IP15)
                ACTMOR = MORT0

                !     Mortality coefficient depends on salinity
                !     Value for low salinity is MORT0
                !     Value for high salinity is MORTS
                !     Linear transition from MORT0 to MORTS
                !        between SAL1 and SAL2

                IF (SAL1 > 0.0 .AND. SAL2 > SAL1) THEN
                    IF (SAL <= SAL1) THEN
                        ACTMOR = MORT0
                    ELSEIF (SAL >= SAL2) THEN
                        ACTMOR = MORTS
                    ELSE
                        ACTMOR = MORT0 + (SAL - SAL1) / (SAL2 - SAL1) * (MORTS - MORT0)
                    ENDIF
                ENDIF

                !     Gross primary production
                PPROD = DL * EFF * FNUT * TFUNG * PPMAX

                !     The respiration does not include excretion!!
                !     The proces formulation used here does not release nutrients due
                !     to respiration, but reduces the uptake of nutrients.
                !     Respiration = maintainance part + growth part
                RESP = MRESP * TFUNM + GRESP * (PPROD - MRESP * TFUNM)

                !     Nett primary production
                FL (1 + IFLUX) = (PPROD - RESP) * ALG

                !     Mortality, including processes as autolysis and zooplankton 'graas
                FL (2 + IFLUX) = ACTMOR * TFUNM * MAX(ALG - ALGMIN, 0.0)

                PMSA (IP16) = PPROD - RESP
                PMSA (IP17) = ACTMOR * TFUNM
                PMSA (IP18) = RESP
                PMSA (IP19) = (PPROD - RESP) * ALG
                PMSA (IP20) = ACTMOR * TFUNM * MAX(ALG - ALGMIN, 0.0)

            ENDIF
            !
            IFLUX = IFLUX + NOFLUX
            IP1 = IP1 + INCREM (1)
            IP2 = IP2 + INCREM (2)
            IP3 = IP3 + INCREM (3)
            IP4 = IP4 + INCREM (4)
            IP5 = IP5 + INCREM (5)
            IP6 = IP6 + INCREM (6)
            IP7 = IP7 + INCREM (7)
            IP8 = IP8 + INCREM (8)
            IP9 = IP9 + INCREM (9)
            IP10 = IP10 + INCREM (10)
            IP11 = IP11 + INCREM (11)
            IP12 = IP12 + INCREM (12)
            IP13 = IP13 + INCREM (13)
            IP14 = IP14 + INCREM (14)
            IP15 = IP15 + INCREM (15)
            IP16 = IP16 + INCREM (16)
            IP17 = IP17 + INCREM (17)
            IP18 = IP18 + INCREM (18)
            IP19 = IP19 + INCREM (19)
            IP20 = IP20 + INCREM (20)
            !
        end do
        !
        RETURN
    END

end module m_pripro
