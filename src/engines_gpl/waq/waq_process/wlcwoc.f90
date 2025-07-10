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
module m_wlcwoc
    use m_waq_precision

    implicit none

contains


    subroutine wlcwoc (process_space_real, fl, ipoint, increm, num_cells, &
            noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
            num_exchanges_z_dir, num_exchanges_bottom_dir)

        !>\file
        !>       Heat Load Capacity (WLC) and Heat Extraction Capacity (WOC)

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

        REAL(kind = real_wp) :: Temp               !1   in ambient water temperature                      (oC)
        REAL(kind = real_wp) :: Surtemp            !2  Surplus temperature                                (oC)
        REAL(kind = real_wp) :: Surf               !3  horizontal surface area of a DELWAQ segment        (m2)
        REAL(kind = real_wp) :: Volume             !4  volume of computational cell                       (m3)
        REAL(kind = real_wp) :: RhoWater           !5  density of water                                   (kg/m3)
        REAL(kind = real_wp) :: CP                 !6  specific heat of water                             (J/kg/oC)
        REAL(kind = real_wp) :: Depth              !7  depth of segment                                   (m)
        REAL(kind = real_wp) :: Width              !8  total width                                        (m)
        REAL(kind = real_wp) :: Velocity           !9  horizontal flow velocity                           (m/s)
        REAL(kind = real_wp) :: SWCalcVelo         !10  switch (1=lin avg, 2=Flow avg, 3=Area avg)        (-)
        REAL(kind = real_wp) :: flowseg            !11  flow from routine velocity SW=3                   (m3/s)
        REAL(kind = real_wp) :: fSpeHeDis          !12  Zelfkoelgetal                                     (W/m2/C)

        REAL(kind = real_wp) :: WL_Tmax            !13  maximum wT after heat discharge               (oC)
        REAL(kind = real_wp) :: WL_dTmax           !14  maximum wT increase after heat discharge      (oC)
        REAL(kind = real_wp) :: WO_Tbgn            !15  wT at beginning cooling (WO)                  (oC)
        REAL(kind = real_wp) :: WO_Tend            !16  wT at end cooling (WO)                        (oC)
        REAL(kind = real_wp) :: WO_dTmax           !17  maximum wT decrease during cooling (WO)       (oC)
        REAL(kind = real_wp) :: KO_Tbgn            !18  wT at beginning heating (KO)                  (oC)
        REAL(kind = real_wp) :: KO_Tend            !19  wT at end of heating (KO)                     (oC)
        REAL(kind = real_wp) :: KO_Tmin            !20  minimum wT to prevent freezing intake (WO)    (oC)
        REAL(kind = real_wp) :: KO_dTmax           !21  maximum wT increase during heating (KO)       (oC)

        !     fluxes

        !     Dummy              :: dummy              ! 1      dummy                                         [oC/d]

        !     local declarations

        REAL(kind = real_wp) :: flowabs            ! flow absolute value used in wlc and woc              (m3/s)
        REAL(kind = real_wp) :: flowsegvel         ! flow calc from velocity and width and depth          (m3/s)

        real(kind = real_wp) :: dTWLC               ! dT for heat loading capacity
        real(kind = real_wp) :: dTWOC               ! dT for heat extraction capacity
        real(kind = real_wp) :: dTKOC               ! dT for cold extraction capacity
        !
        REAL(kind = real_wp) :: WLC_Q_s             ! O  Heat Loading Capacity Flow per segment             MW
        REAL(kind = real_wp) :: WLC_A_s             ! O  Heat Loading Capacity Atmosphere per segment       MW
        REAL(kind = real_wp) :: WLC_m2              ! O  Heat Loading Capacity total  per m2           W/m2
        !
        REAL(kind = real_wp) :: WOC_Q_s             ! O  Heat Extraction Capacity per segment               MW
        REAL(kind = real_wp) :: WOC_A_s             ! O  Heat Extraction Capacity Atmosphere per segment    MW
        REAL(kind = real_wp) :: WOC_m2              ! O  Heat Extraction Capacity Total per m2             W/m2

        REAL(kind = real_wp) :: KOC_Q_s             ! O  Cold Extraction Capacity per segment               MW
        REAL(kind = real_wp) :: KOC_A_s             ! O  Cold Extraction Capacity Atmosphere per segment    MW
        REAL(kind = real_wp) :: KOC_m2              ! O  Cold Extraction Capacity Total per m2             W/m2

        !          tbv alternatieve berekening met voorbelasting
        real(kind = real_wp) :: dTWLC2              ! dT for heat loading capacity second method
        real(kind = real_wp) :: dTWOC2              ! dT for heat extraction capacity second metod
        real(kind = real_wp) :: dTKOC2              ! dT for cold extraction capacity

        REAL(kind = real_wp) :: WLC2_Q_s     ! O  Heat Loading Capacity Flow per segment             MW
        REAL(kind = real_wp) :: WLC2_A_s     ! O  Heat Loading Capacity Atmosphere per segment       MW
        REAL(kind = real_wp) :: WLC2_m2      ! O  Heat Loading Capacity total  per m2           W/m2
        !
        REAL(kind = real_wp) :: WOC2_Q_s     ! O  Heat Extraction Capacity per segment               MW
        REAL(kind = real_wp) :: WOC2_A_s     ! O  Heat Extraction Capacity Atmosphere per segment    MW
        REAL(kind = real_wp) :: WOC2_m2      ! O  Heat Extraction Capacity total per m2              W/m2
        !
        REAL(kind = real_wp) :: KOC2_Q_s     ! O  Cold Extraction Capacity per segment               MW
        REAL(kind = real_wp) :: KOC2_A_s     ! O  Cold Extraction Capacity Atmosphere per segment    MW
        REAL(kind = real_wp) :: KOC2_m2      ! O  Cold Extraction Capacity total per m2              W/m2

        REAL(kind = real_wp), PARAMETER :: factorMW = 1.0e6 ! Conversion from MW to W and vice versa

        INTEGER(kind = int_wp) :: IP1, IP2, IP3, IP4, IP5, IP6, IP7, IP8, IP9, IP10, &
                IP11, IP12, IP13, IP14, IP15, IP16, IP17, IP18, IP19, IP20, &
                IP21, IP22, IP23, IP24, IP25, IP26, IP27, IP28, IP29, IP30, &
                IP31, IP32, IP33, IP34, IP35, IP36, IP37, IP38, IP39, IP40, &
                IP41, IP42, IP43, IP44, IP45, IP46, IP47
        INTEGER(kind = int_wp) :: IFLUX, ISEG, IKMRK2

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
        IP25 = IPOINT(25)
        IP26 = IPOINT(26)
        IP27 = IPOINT(27)
        IP28 = IPOINT(28)
        IP29 = IPOINT(29)
        IP30 = IPOINT(30)
        IP31 = IPOINT(31)
        IP32 = IPOINT(32)
        IP33 = IPOINT(33)
        IP34 = IPOINT(34)
        IP35 = IPOINT(35)
        IP36 = IPOINT(36)
        IP37 = IPOINT(37)
        IP38 = IPOINT(38)
        IP39 = IPOINT(39)
        IP40 = IPOINT(40)
        IP41 = IPOINT(41)
        IP42 = IPOINT(42)
        IP43 = IPOINT(43)
        IP44 = IPOINT(44)
        IP45 = IPOINT(45)
        IP46 = IPOINT(46)
        IP47 = IPOINT(47)

        !
        IFLUX = 0
        DO ISEG = 1, num_cells
            !
            Temp = process_space_real(IP1)
            Surtemp = process_space_real(IP2)
            Surf = process_space_real(IP3)
            Volume = process_space_real(IP4)
            RhoWater = process_space_real(IP5)
            CP = process_space_real(IP6)
            Depth = process_space_real(IP7)
            Width = process_space_real(IP8)
            Velocity = process_space_real(IP9)
            SWCalcVelo = process_space_real(IP10)
            flowseg = process_space_real(IP11)
            fSpeHeDis = process_space_real(IP12)
            WL_Tmax = process_space_real(IP13)
            WL_dTmax = process_space_real(IP14)
            WO_Tbgn = process_space_real(IP15)
            WO_Tend = process_space_real(IP16)
            WO_dTmax = process_space_real(IP17)
            KO_Tbgn = process_space_real(IP18)
            KO_Tend = process_space_real(IP19)
            KO_Tmin = process_space_real(IP20)
            KO_dTmax = process_space_real(IP21)


            !             Calculate FLOW from from Velocity, Width and Depth if possible (WIDTH required, not available in D3D)
            !             Velocity (module VELOC) can be calculated using various averaging options but Width is not available in D3D (it is in Sobek)
            !             Module VELOC calculates FLOW too (only for SWCalVelo=3). This is the preffered option which works for D3D and Sobek
            !             If other options for Velociy calculation are needed this works for Sobek, to make it work in D3D provide WIDTH as input.
            flowsegvel = depth * width * velocity
            if (int (SWCalcVelo + 0.5) == 3) then
                flowabs = abs(flowseg)
            else
                flowabs = abs(flowsegvel)
            endif

            !
            !            WarmteLozingsCapaciteit (WLC) = warmtelozing van bv energie centrales (intake-putfall) en industrie
            !
            dTWLC = 0
            dTWLC2 = 0
            !
            !             maximale opwarming - maximale temperatuur (in praktijk 28 graden voor zomer warmtelozingen en dT 3graden tov achtergrond
            !                                - dTmaxIncr corrigeren voor al geloosde warmte via SurTemp
            !
            dTWLC = max(min(WL_Tmax - Temp, WL_dTmax), 0.0)

            !             Extra beperking door gerealiseerde bijdrage lozingen
            !             voorbeeld: 25g waarvan 1 surplus, dan nog maar 2 opwarming
            !             warmtelozingen (Surtemp) dan nog maar 2 graden opwarming mogelijk
            !
            WL_dTmax = WL_dTmax - Surtemp
            dTWLC2 = max(min(WL_Tmax - Temp, WL_dTmax), 0.0)

            !             potentiele bijdrage lucht-water aan koelcapaciteit via zelfkoelgetal
            !             m2 . gC . W/m2/oC / 10^6 =  MW per segment
            WLC_A_s = Surf * dTWLC * fSpeHeDis / factorMW
            WLC2_A_s = Surf * dTWLC2 * fSpeHeDis / factorMW

            !             potentiele bijdrage debiet aan koelcapaciteit
            !             m3/s . gC . J/kg/gC . kg/m3 = J/s / 10^6 = MW per segment
            WLC_Q_s = (flowabs * dTWLC * CP * RhoWater) / factorMW
            WLC2_Q_s = (flowabs * dTWLC2 * CP * RhoWater) / factorMW

            if (surf > 1.0) then
                !                MW * 10^6 ->  W / m2
                WLC_m2 = (WLC_A_s + WLC_Q_s) / Surf * factorMW
                WLC2_m2 = (WLC2_A_s + WLC2_Q_s) / Surf * factorMW
            endif
            !
            !            WarmteOtrekkingscapaciteit (WOC) = koudelozing
            !
            dTWOC = 0
            dTWOC2 = 0

            if (Temp >= WO_Tbgn) then
                dTWOC = min(max((Temp - WO_Tbgn), WO_Tbgn - WO_Tend), &
                        WO_dTmax)
                !                 verruim WOC met surplustemperatuur
                !                 bv water is 18 met 1surplus (begin=17,end=14, dTmax=6)
                WO_dTmax = WO_dTmax + Surtemp
                dTWOC2 = min(max((Temp - WO_Tbgn), &
                        WO_Tbgn - WO_Tend + Surtemp), &
                        WO_dTmax)
            endif

            !             potentiele bijdrage lucht-water aan koelcapaciteit via zelfkoelgetal
            !             m2 . gC . J.s-1.m-2.gC-1 = J.s-1 = W * 10-6 = MW per segment
            !             per segment in MW
            WOC_A_s = Surf * dTWOC * fSpeHeDis / factorMW
            WOC2_A_s = Surf * dTWOC2 * fSpeHeDis / factorMW

            !             potentiele bijdrage debiet aan koelcapaciteit
            !             m3/s . gC . J/kg/gC . kg/m3 = J/s = W * 10-6 = MW per segment
            WOC_Q_s = (flowabs * dTWOC * CP * RhoWater) / factorMW
            WOC2_Q_s = (flowabs * dTWOC2 * CP * RhoWater) / factorMW

            !             bijdrage stroming en oppervlak per surface area in W/m2
            if (surf > 1.0) then
                WOC_m2 = (WOC_A_s + WOC_Q_s) / Surf * factorMW
                WOC2_m2 = (WOC2_A_s + WOC2_Q_s) / Surf * factorMW
            endif
            !
            !
            !            KoudeOnttrekkingscapaciteit (KOC) = warmtelozing
            !

            dTKOC = 0
            dTKOC2 = 0

            if ((Temp >= KO_Tmin) .and. (Temp <= KO_Tbgn)) then

                dTKOC = min(max(KO_Tbgn - Temp, KO_Tend - KO_Tbgn), &
                        KO_dTmax)
                !
                !                  Extra beperking door gerealiseerde bijdrage lozingen
                KO_dTmax = KO_dTmax - Surtemp
                dTKOC2 = min(max(KO_Tbgn - Temp, KO_Tend - KO_Tbgn), &
                        KO_dTmax)
            endif

            !             m2 . gC . W/m2/oC / 10^6 =  MW per segment
            KOC_A_s = Surf * dTKOC * fSpeHeDis / factorMW
            KOC2_A_s = Surf * dTKOC2 * fSpeHeDis / factorMW

            !             potentiele bijdrage debiet aan koelcapaciteit
            !             m3/s . gC . J/kg/gC . kg/m3 = J/s / 10^6 = MW per segment
            KOC_Q_s = (flowabs * dTKOC * CP * RhoWater) / factorMW
            KOC2_Q_s = (flowabs * dTKOC2 * CP * RhoWater) / factorMW

            if (surf > 1.0) then
                !                MW * 10^6 ->  W / m2
                KOC_m2 = (KOC_A_s + KOC_Q_s) / Surf * factorMW
                KOC2_m2 = (KOC2_A_s + KOC2_Q_s) / Surf * factorMW
            endif


            !
            !        *****     End of your code       *****
            !
            process_space_real(IP22) = WLC_Q_s
            process_space_real(IP23) = WLC_A_s
            process_space_real(IP24) = WLC2_Q_s
            process_space_real(IP25) = WLC2_A_s
            process_space_real(IP26) = dTWLC
            process_space_real(IP27) = dTWLC2
            process_space_real(IP28) = WLC_m2
            process_space_real(IP29) = WLC2_m2

            process_space_real(IP30) = WOC_Q_s
            process_space_real(IP31) = WOC_A_s
            process_space_real(IP32) = WOC2_Q_s
            process_space_real(IP33) = WOC2_A_s
            process_space_real(IP34) = dTWOC
            process_space_real(IP35) = dTWOC2
            process_space_real(IP36) = WOC_m2
            process_space_real(IP37) = WOC2_m2

            process_space_real(IP38) = KOC_Q_s
            process_space_real(IP39) = KOC_A_s
            process_space_real(IP40) = KOC2_Q_s
            process_space_real(IP41) = KOC2_A_s
            process_space_real(IP42) = dTKOC
            process_space_real(IP43) = dTKOC2
            process_space_real(IP44) = KOC_m2
            process_space_real(IP45) = KOC2_m2

            process_space_real(IP46) = flowsegvel
            process_space_real(IP47) = flowabs
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
            IP21 = IP21 + INCREM (21)
            IP22 = IP22 + INCREM (22)
            IP23 = IP23 + INCREM (23)
            IP24 = IP24 + INCREM (24)
            IP25 = IP25 + INCREM (25)
            IP26 = IP26 + INCREM (26)
            IP27 = IP27 + INCREM (27)
            IP28 = IP28 + INCREM (28)
            IP29 = IP29 + INCREM (29)
            IP30 = IP30 + INCREM (30)
            IP31 = IP31 + INCREM (31)
            IP32 = IP32 + INCREM (32)
            IP33 = IP33 + INCREM (33)
            IP34 = IP34 + INCREM (34)
            IP35 = IP35 + INCREM (35)
            IP36 = IP36 + INCREM (36)
            IP37 = IP37 + INCREM (37)
            IP38 = IP38 + INCREM (38)
            IP39 = IP39 + INCREM (39)
            IP40 = IP40 + INCREM (40)
            IP41 = IP41 + INCREM (41)
            IP42 = IP42 + INCREM (42)
            IP43 = IP43 + INCREM (43)
            IP44 = IP44 + INCREM (44)
            IP45 = IP45 + INCREM (45)
            IP46 = IP46 + INCREM (46)
            IP47 = IP47 + INCREM (47)

        end do
        !
        RETURN
        !
    END

end module m_wlcwoc
