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
module m_heatfl
    use m_waq_precision

    implicit none

contains


    subroutine heatfl (process_space_real, fl, ipoint, increm, num_cells, &
            noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
            num_exchanges_z_dir, num_exchanges_bottom_dir)
        !>\file
        !>       Total heat flux for surface water absolute temperature model

        !
        !     Description of the module :
        !
        ! Name    T   L I/O   Description                                    Units
        ! ----    --- -  -    -------------------                            -----
        ! beta    R*4 1 L Bowens constant                                [mbar/°C]
        ! cloud   R*4 1 L Cloud coverage (fraction)                            [-]
        ! cpa     R*4 1 I Specific heat capacity of air                   [J/kg,K]
        ! cp      R*4 1 I Specific heat capacity of water                [J/kg,°C]
        ! CwindA  R*4 1 I Coefficient in wind function a                       [-]
        ! CwindB  R*4 1 I Coefficient in wind function b                       [-]
        ! CwindC  R*4 1 I Coefficient in wind function c                       [-]
        ! DeltaT  R*4 1 I Correction factor for TempWa near discharge          [-]
        ! Depth   R*4 1 I Water depth of segment                               [m]
        ! dTemp   R*4 1 O Flux on water temperature                         [°C/d]
        ! Emiss   R*4 1 L Emissivity (colour factor) for the atmosphere        [-]
        ! EWater  R*4 1 I Emissivity (colour factor) of surface water          [-]
        ! Fa      R*4 1 I Reflection factor for Qa                             [-]
        ! Fsw     R*4 1 I Reflection factor for Qsw                            [-]
        ! Fwind   R*4 1 L Wind function for evaporation                 [m/s,mbar]
        ! Fwind2  R*4 1 L Wind function for evaporation, incl corr.    [W/m2,mbar]
        ! Ha      R*4 1 I Height of wind speed needed for wind function        [m]
        ! Hm      R*4 1 I Height at which wind speed is measured               [m]
        ! HTVap   R*4 1 L Heat of evaporation                               [J/kg]
        ! HTVap   R*4 1 L Heat of evaporation at reference temperature      [J/kg]
        ! K       R*4 1 I Roughness coef for calculation vwind                 [-]
        ! Patm    R*4 1 I Air pressure                                 [mb of HPa]
        ! Pvap    R*4 1 L Actual vapour pressure in air                     [mbar]
        ! Psvap   R*4 1 L Saturated vapour pressure in air                  [mbar]
        ! PvapWa  R*4 1 L Saturated vapour pressure at water-air interface  [mbar]
        ! Qa      R*4 1 L Long wave atmospheric radiation reaching water    [W/m2]
        ! Qan     R*4 1 L Non refl. (net) atm. long wave rad. reaching water[W/m2]
        ! Qbr     R*4 1 L Long wave back radiation from water               [W/m2]
        ! Ql      R*4 1 L Latent heat flux by evaporation or condensation   [W/m2]
        ! Qrb     R*4 1 I Heat exchange between water and riverbed          [W/m2]
        ! Qsg     R*4 1 L Sensible heat of conduction                       [W/m2]
        ! Qsn     R*4 1 L Non refl. (net) short wave rad. reaching water    [W/m2]
        ! Qsw     R*4 1 L Short wave radiation reaching water               [W/m2]
        ! Qt      R*4 1 L Total heat flux                                   [W/m2]
        ! RelHum  R*4 1 I Relative air humidity                                [%]
        ! Rho0    R*4 1 I Density of surface water at 4°C                  [kg/m3]
        ! RhoRef  R*4 1 L Density of surface water at reference temperature[kg/m3]
        ! SBC     R*4 1 I StefanBolzman Constant                        [W/m2/K-4]
        ! SWEmis  R*4 1 I Switch for calculation of emissivity                 [-]
        ! TempAt  R*4 1 I Air temperature                                     [°C]
        ! TempWa  R*4 1 I Surface water temperature                           [°C]
        ! Tref    R*4 1 I Reference water temperature for wind function       [°C]
        ! Vevap   R*4 1 L Evaporation rate                                   [m/s]
        ! VWinda  R*4 1 L Calculated wind speed                              [m/s]
        ! VWindm  R*4 1 I Measured wind speed                                [m/s]
        !
        !     Logical Units : -

        !     Modules called : -

        !     Name     Type   Library
        !     ------   -----  ------------

        use m_logger_helper, only : stop_with_error, get_log_unit_number
        use m_extract_waq_attribute
        USE PHYSICALCONSTS, ONLY : CtoKelvin
        IMPLICIT NONE
        REAL(kind = real_wp) :: process_space_real  (*), FL  (*)
        INTEGER(kind = int_wp) :: IPOINT(*), INCREM(*), num_cells, NOFLUX, &
                IEXPNT(4, *), IKNMRK(*), num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir, &
                ISEG, IFLUX, IKMRK2
        INTEGER(kind = int_wp) :: IP1, IP2, IP3, IP4, IP5, IP6, IP7, IP8, IP9, IP10, &
                IP11, IP12, IP13, IP14, IP15, IP16, IP17, IP18, IP19, &
                IP20, IP21, IP22, IP23, IP24, IP25, IP26, IP27, IP28, &
                IP29, IP30, IP31, IP32, IP33, IP34, IP35, IP36, IP37, &
                IP38, IP39, IP40
        INTEGER(kind = int_wp) :: ISWTEMP, ISWEMIS
        REAL(kind = real_wp) :: Qsw, Fsw, Pvap, TempAt, SBC, Fa, EWater, &
                TempWa, RhoWat, CWindA, CWindB, CWindC, VWindm, K, &
                PvapWa, Cpa, Patm, Qsn, Cloud, Emiss, Qa, Qan, Qbr, C, &
                HTVap, Fwind, Ql, Beta, Qsg, Qt, Qrb, Tref, &
                dTemp, cp, DEPTH, RelHum, Psvap, Hm, Ha, Vevap, Fwind2, &
                VWinda, DeltaT, HtVRef, RhoRef, Rho0, MODTEMP, mindeptht
        REAL(kind = real_wp), PARAMETER :: sCtoKelvin = real(CtoKelvin)
        INTEGER(kind = int_wp) :: LUNREP

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

        !
        IFLUX = 0
        DO ISEG = 1, num_cells

            !     Heat exchange only for active water segments
            !
            IF (BTEST(IKNMRK(ISEG), 0)) THEN
                !
                !     Heat exchange only for top layer segments
                !
                CALL extract_waq_attribute(2, IKNMRK(ISEG), IKMRK2)
                IF (IKMRK2==0 .OR. IKMRK2==1) THEN
                    !
                    Qsw = process_space_real(IP1)
                    Fsw = process_space_real(IP2)
                    Cloud = process_space_real(IP3) / 100.0
                    ISWEmis = NINT(process_space_real(IP4))
                    TempAt = process_space_real(IP5)
                    SBC = process_space_real(IP6)
                    Fa = process_space_real(IP7)
                    Ewater = process_space_real(IP8)
                    TempWa = process_space_real(IP9)
                    Rho0 = process_space_real(IP10)
                    CWinda = process_space_real(IP11)
                    CWindb = process_space_real(IP12)
                    CWindc = process_space_real(IP13)
                    VWindm = process_space_real(IP14)
                    hm = process_space_real(IP15)
                    ha = process_space_real(IP16)
                    k = process_space_real(IP17)
                    cpa = process_space_real(IP18)
                    Patm = process_space_real(IP19)
                    Qrb = process_space_real(IP20)
                    Cp = process_space_real(IP21)
                    DEPTH = process_space_real(IP22)
                    RelHum = process_space_real(IP23) / 100.0
                    DeltaT = process_space_real(IP24)
                    Tref = process_space_real(IP25)
                    ISWTEMP = NINT(process_space_real(IP26))
                    mindeptht = process_space_real(IP27)

                    !
                    !     ------Short wave solar radiation-----------
                    !
                    Qsn = Qsw * (1 - Fsw)
                    !
                    !     ------Long wave atmospheric radiation------
                    !
                    !           cloud = (100.0 - SunFac) / 100.0
                    !
                    Psvap = 6.131 + 0.467 * TempAt + 0.0089 * &
                            TempAt ** 2.0 + 0.000527 * TempAt ** 3.0
                    Psvap = max(Psvap, 0.001)  ! to avoid values < 0 at very low air temperature
                    !
                    Pvap = RelHum * Psvap
                    !
                    !           ----1.  Option 1 = Brunt, 1932
                    !
                    IF (ISWEmis==1) THEN
                        !
                        Emiss = (0.51 + 0.066 * SQRT(Pvap)) * &
                                (1.0 + 0.17 * cloud ** 2.0)
                        !
                        Qa = Emiss * SBC * (TempAt + sCtoKelvin) ** 4.0
                        !
                        !           ----2.  Option 2 = Edinger, 1965 and Koberg, 1962
                        !
                    ELSEIF (ISWEmis==2) THEN
                        !
                        !           Pascal heeft formulering voor C (Brunt coefficient)
                        !           aangevraagd, wordt later ingevuld...
                        !
                        C = 1
                        !
                        Emiss = 1.1 * C + 0.030 * SQRT(Pvap)
                        !
                        Qa = Emiss * SBC * (TempAt + sCtoKelvin) ** 4.0
                        !
                        !           ----3.  Option 3 = Edinger, 1965
                        !
                    ELSEIF (ISWEmis==3) THEN
                        !
                        Emiss = 0.74 * (1 + 0.17 * cloud) + &
                                0.0045 * (1 - 0.4 * cloud) * &
                                        Pvap
                        !
                        Qa = Emiss * SBC * (TempAt + sCtoKelvin) ** 4.0
                        !
                        !           ----4.  Option 4 = Ludikhuize, 1994 as in WAQUA
                        !
                    ELSEIF (ISWEmis==4) THEN
                        !
                        Qa = (218.0 + 6.3 * TempAt) * &
                                (1.0 + 0.17 * cloud ** 2.0)
                        !
                    ELSE
                        CALL get_log_unit_number(LUNREP)
                        WRITE (LUNREP, *) ' Illegal option for emissivity formula'
                        WRITE (*, *) ' Illegal option for emissivity formula'
                        CALL stop_with_error()
                    ENDIF
                    !
                    Qan = Qa * (1 - Fa)
                    !
                    !     ------Long wave back radiation from water------
                    !
                    Qbr = Ewater * SBC * ((TempWa + DeltaT + sCtoKelvin) ** 4.0)
                    !
                    !     ------Latent heat (evaporation or condensation)-----
                    !
                    RhoWat = Rho0 * (1.0 - 7.17e-6 * &
                            (TempWa + DeltaT - 4.0) ** 2.0)
                    !
                    RhoRef = Rho0 * (1.0 - 7.17e-6 * &
                            (Tref - 4.0) ** 2.0)
                    !
                    HtVap = 2.5e+06 - 2300.0 * (TempWa + DeltaT)
                    !
                    HtVRef = 2.5e+06 - 2300.0 * Tref
                    !
                    PvapWa = 6.131 + 0.467 * (TempWa + DeltaT) + 0.0089 * &
                            (TempWa + DeltaT) ** 2.0 + 0.000527 * &
                            (TempWa + DeltaT) ** 3.0
                    !
                    IF (ABS(Hm - Ha) < 0.0001) THEN
                        VWinda = VWindm
                    ELSE
                        IF  ((Hm > (10 * k)) .AND. (Ha > (10 * k))) THEN
                            VWinda = VWindm * LOG(Hm / k) / LOG(Ha / k)
                        ELSE
                            VWinda = VWindm
                        ENDIF
                    ENDIF
                    !
                    FWind = CwindA + CwindB * VWinda + CwindC * VWinda ** 2.0
                    !
                    FWind2 = Fwind / (RhoRef * HtVRef)
                    !
                    Vevap = FWind2 * (PvapWa - Pvap)
                    !
                    Ql = RhoWat * HtVap * Vevap
                    !
                    !     ------Convective heat-----------------------------
                    !
                    beta = (cpa * Patm) / (0.62 * HtVap)
                    !
                    Qsg = beta * (TempWa + DeltaT - TempAt) * RhoWat * HtVap * Fwind2
                    !
                    !     ------Convective heat to river bed----------------
                    !           formulations can be included later...
                    !
                    !     ------Total heat flux-----------------------------
                    !
                    Qt = Qsn + Qan - Qbr - Ql - Qsg + Qrb
                    !
                    !     ------Change of water temperature-----------------
                    !
                    dTemp = (Qt * 86400.0) / (RhoWat * Cp * Depth)
                    !
                    !     ------For output to exces temperature moddeling
                    !
                    IF (ISWTEMP == 0) THEN
                        MODTEMP = TEMPWA
                    ELSE
                        MODTEMP = 0.0
                    ENDIF

                    process_space_real(IP28) = Qsn
                    process_space_real(IP29) = Psvap
                    process_space_real(IP30) = Emiss
                    process_space_real(IP31) = Qan
                    process_space_real(IP32) = Qbr
                    process_space_real(IP33) = Ql
                    process_space_real(IP34) = Qsg
                    process_space_real(IP35) = Qt
                    process_space_real(IP36) = PvapWa
                    process_space_real(IP37) = VWinda
                    process_space_real(IP38) = Vevap
                    process_space_real(IP39) = RhoWat
                    process_space_real(IP40) = MODTEMP

                    IF (depth < mindeptht) THEN

                        ! depth less then mindeptht no fluxes

                        dTemp = 0.0

                    ENDIF
                    !
                ELSE
                    !
                    !           For non top layer segments no heat flux is calculated
                    !
                    dTemp = 0.0
                    !
                ENDIF
                !
            ENDIF
            !
            FL(1 + IFLUX) = dTemp

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
            !
        end do
        !

        RETURN
    END

end module m_heatfl
