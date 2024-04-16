!!  Copyright(C) Stichting Deltares, 2012-2024.
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
module m_mpbnut
    use m_waq_precision

    implicit none

contains


    SUBROUTINE MPBNUT (PMSA, FL, IPOINT, INCREM, NOSEG, &
            NOFLUX, IEXPNT, IKNMRK, NOQ1, NOQ2, &
            NOQ3, NOQ4)
        use m_zerome
        use m_evaluate_waq_attribute

        !     **********************************************************************
        !          +----------------------------------------+
        !          |    D E L F T   H Y D R A U L I C S     |
        !          |    Water Resources and Environment     |
        !          +----------------------------------------+
        !
        !     ***********************************************************************
        !
        !          Project      : Implementatie GEM (Z2556)
        !          Formulations :
        !          Programmer   : A. Blauw
        !          Date         : 02-03-99           Version : 1.0
        !
        !          History :
        !
        !          Date    Programmer      Description
        !          ------  --------------  ------------------------------------------
        !          020399  A. Blauw        First version
        !
        !     ***********************************************************************
        !
        !          Description of the module :
        !
        !            Calculation of nutrient concentrations in the bottom layer,
        !            for the growth of microfytobenthos, when MFB is modelled as
        !            an inactive substance in a water segment
        !            (when they're modelled explicitly in bottom segments, nutrient
        !            concentrations are modelled themselves)
        !
        !
        !      Name    T   L I/O   Description                                  Units
        !      ----    --- -  -    -------------------                          -----
        !      xxxxx   xxx x  x    xxxxxxxxxxxxxx                               [xxxxx]
        !      etc.

        !          Logical Units : -

        !          Modules called : -

        !          Name     Type   Library
        !          ------   -----  ------------

        !          IMPLICIT REAL(kind=real_wp) ::(A-H,J-Z)

        REAL(kind = real_wp) :: PMSA  (*), FL    (*)
        INTEGER(kind = int_wp) :: IPOINT(*), INCREM(*), NOSEG, NOFLUX, &
                IEXPNT(4, *), IKNMRK(*), NOQ1, NOQ2, NOQ3, NOQ4

        INTEGER(kind = int_wp) :: IP1, IP2, IP3, IP4, IP5, IP6, IP7, IP8, IP9, IP10, &
                IP11, IP12, IP13, IP14, IP15, IP16, IP17, IP18, IP19, IP20, &
                IP21, IP22, IP23, IP24
        INTEGER(kind = int_wp) :: IN1, IN2, IN3, IN4, IN5, IN6, IN7, IN8, IN9, IN10, &
                IN11, IN12, IN13, IN14, IN15, IN16, IN17, IN18, IN19, IN20, &
                IN21, IN22, IN23, IN24
        INTEGER(kind = int_wp) :: IKMRK1, ISEG
        REAL(kind = real_wp) :: LEN, DIF, FNBM, FNSW, FNH4GS, FNO3GS, FPBM, &
                FPSW, FPGS, FSiBM, FSiSW, NH4, NO3, PO4, &
                Si, TCNIT, FRNO3, TEMP, DEPTH, SURF, &
                FNO3, FNH4, FN, FTMP, FPO4, FSi, &
                NH4S1, NO3S1, PO4S1, SiS1

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
        IP21 = IPOINT(21)
        IP22 = IPOINT(22)
        IP23 = IPOINT(23)
        IP24 = IPOINT(24)

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
        IN13 = INCREM(13)
        IN14 = INCREM(14)
        IN15 = INCREM(15)
        IN16 = INCREM(16)
        IN17 = INCREM(17)
        IN18 = INCREM(18)
        IN19 = INCREM(19)
        IN20 = INCREM(20)
        IN21 = INCREM(21)
        IN22 = INCREM(22)
        IN23 = INCREM(23)
        IN24 = INCREM(24)

        DO ISEG = 1, NOSEG

            CALL evaluate_waq_attribute(1, IKNMRK(ISEG), IKMRK1)

            IF (IKMRK1==1) THEN

                LEN = PMSA(IP1)
                DIF = PMSA(IP2)

                IF (DIF<1E-20) CALL ZEROME ('DIF in MFBNUT')

                FNBM = PMSA(IP3)
                FNSW = PMSA(IP4)
                FNH4GS = PMSA(IP5)
                FNO3GS = PMSA(IP6)
                FPBM = PMSA(IP7)
                FPSW = PMSA(IP8)
                FPGS = PMSA(IP9)
                FSiBM = PMSA(IP10)
                FSiSW = PMSA(IP11)
                NH4 = PMSA(IP12)
                NO3 = PMSA(IP13)
                PO4 = PMSA(IP14)
                Si = PMSA(IP15)
                TCNIT = PMSA(IP16)
                FRNO3 = PMSA(IP17)
                TEMP = PMSA(IP18)
                DEPTH = PMSA(IP19)
                SURF = PMSA(IP20)
                !                VOLWAT = DEPTH * SURF

                IF ((FNBM + FNSW) <= 0.0) THEN
                    FNO3 = FNO3GS
                    FNH4 = FNH4GS
                ELSE
                    FN = MAX(FNBM, FNSW)
                    FTMP = TCNIT ** (TEMP - 20.0)
                    FNO3 = FRNO3 * FTMP * FN
                    FNO3 = MIN (FN, FNO3)
                    FNH4 = FN - FNO3
                ENDIF
                !          conversie van mineralisatieflux van g/m3/dag naar g/m2/dag
                FNH4 = FNH4 * DEPTH
                FNO3 = FNO3 * DEPTH
                FPO4 = MAX(FPBM, FPSW, FPGS) * DEPTH
                FSi = MAX(FSiBM, FSiSW) * DEPTH
                !
                !          meenemen van nutrientenconc. in water leidt tot conflict met BLOOM
                !               NH4S1 = (LEN * FNH4/DIF) + NH4
                !               NO3S1 = (LEN * FNO3/DIF) + NO3
                !               PO4S1 = (LEN * FPO4/DIF) + PO4
                !               SiS1  = (LEN * FSi/DIF)  + Si
                NH4S1 = (LEN * FNH4 / DIF)
                NO3S1 = (LEN * FNO3 / DIF)
                PO4S1 = (LEN * FPO4 / DIF)
                SiS1 = 5.0
                !      -------------------------------------------------------
                !          tijdens debuggen voor GEM-Waddenzee studie poriewater verzadigd met Si verondersteld
                !          omdat dat in de praktijk zo is (pers. comm. Johannes Smits) en niet afhankelijk van DetSiS1
                !          aanpassing d.d. 15 aug 2001 door A. Blauw (oude formulering:
                !          SiS1  = (LEN * FSi/DIF)                                     )
                !          (op advies van Arno Nolte is als poriewaterconc. de verzadigingswaarde (= ca 50) gedeeld door
                !          10 gebruikt, bij wijze van correctie voor uitwisseling met de waterfase dd 16-08-01, A Blauw)
                !      --------------------------------------------------------

                PMSA(IP21) = NH4S1
                PMSA(IP22) = NO3S1
                PMSA(IP23) = PO4S1
                PMSA(IP24) = SiS1

            ENDIF

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
            IP11 = IP11 + IN11
            IP12 = IP12 + IN12
            IP13 = IP13 + IN13
            IP14 = IP14 + IN14
            IP15 = IP15 + IN15
            IP16 = IP16 + IN16
            IP17 = IP17 + IN17
            IP18 = IP18 + IN18
            IP19 = IP19 + IN19
            IP20 = IP20 + IN20
            IP21 = IP21 + IN21
            IP22 = IP22 + IN22
            IP23 = IP23 + IN23
            IP24 = IP24 + IN24

        end do

        RETURN
    END

end module m_mpbnut
