!
!    Copyright 2013-2020.unhoven
!
!    This file is part of SolveSAPHE.

!    SolveSAPHE is free software: you can redistribute it and/or modify
!    it under the terms of the GNU Lesser General Public License as published by
!    the Free Software Foundation, either version 3 of the License, or
!    (at your option) any later version.
!
!    SolveSAPHE is distributed in the hope that it will be useful,
!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!    GNU Lesser General Public License for more details.
!
!    You should have received a copy of the GNU Lesser General Public License
!    along with SolveSAPHE.  If not, see <http://www.gnu.org/licenses/>.
!





MODULE MOD_CHEMCONST

USE MOD_PRECISION
USE PHYSICALCONSTS

IMPLICIT NONE

! --------------------------------------------------------
! List of subroutines for the chemical constants (PRIVATE)
! --------------------------------------------------------

PRIVATE AK_CARB_0_WEIS74
PRIVATE AK_CARB_1_MILL95, AK_CARB_2_MILL95
PRIVATE AK_CARB_1_LUEK00, AK_CARB_2_LUEK00
PRIVATE AK_CARB_1_ROYE93, AK_CARB_2_ROYE93
PRIVATE AK_BORA_DICK90
PRIVATE AK_PHOS_1_MILL95, AK_PHOS_2_MILL95, AK_PHOS_3_MILL95
PRIVATE AK_SILI_1_MILL95
PRIVATE AK_H2S_1_MILL95
PRIVATE AK_AMMO_1_YAMI95
PRIVATE AK_W_MILL95
PRIVATE AK_HSO4_DICK90
PRIVATE ABETA_HF_DIRI79
PRIVATE AK_HF_PEFR87


! --------------------------------------
! Parameters for usage within the module
! --------------------------------------

! Gas constant
! ------------

REAL(KIND=wp), PARAMETER, PRIVATE :: gasconst_bar_cm3_o_mol_k = 83.14510_wp ! libthdyct
!REAL(KIND=wp), PARAMETER, PRIVATE :: gasconst_bar_cm3_o_mol_k = 83.14472_wp ! Handbook (2007)

! 0 degrees centigrade in Kelvin
! ------------------------------

REAL(KIND=wp), PARAMETER, PRIVATE :: t_k_zerodegc = real(CtoKelvin, wp) ! Handbook (2007)


! --------------------------------------------------------------
! Chemical constants' products: for usage by users of the module
! --------------------------------------------------------------

! For each acid system A,
! - api1_aaa <-- K_A1
! - api2_aaa <-- K_A1*K_A2
! - api3_aaa <-- K_A1*K_A2*K_A3
! - ...

REAL(KIND=wp) :: api1_dic, api2_dic
REAL(KIND=wp) :: api1_bor
REAL(KIND=wp) :: api1_po4, api2_po4, api3_po4
REAL(KIND=wp) :: api1_sil
REAL(KIND=wp) :: api1_nh4
REAL(KIND=wp) :: api1_h2s
REAL(KIND=wp) :: api1_so4
REAL(KIND=wp) :: api1_flu
REAL(KIND=wp) :: api1_wat, aphscale


!*******************************************************************************
CONTAINS
!*******************************************************************************

!=======================================================================
 SUBROUTINE SETUP_API4PHTOT(t_k, s, p_bar)
!=======================================================================

IMPLICIT NONE

! ------------------
! Argument variables
! ------------------

!     t_k    : temperature in Kelvin
!     s      : salinity
!     p_bar  : applied pressure in bar

REAL(KIND=wp), INTENT(IN) :: t_k
REAL(KIND=wp), INTENT(IN) :: s
REAL(KIND=wp), INTENT(IN) :: p_bar

! ---------------
! Local variables
! ---------------

REAL(KIND=wp) :: zcvt_htot_o_hsws, zcvt_htot_o_hfree


zcvt_htot_o_hsws  = 1._wp/ACVT_HSWS_O_HTOT(t_k, s, p_bar)
zcvt_htot_o_hfree = ACVT_HTOT_O_HFREE(t_k, s, p_bar)


api1_dic =            AK_CARB_1_LUEK00(t_k, s, p_bar)
api2_dic = api1_dic * AK_CARB_2_LUEK00(t_k, s, p_bar)

api1_bor =            AK_BORA_DICK90(t_k, s, p_bar)

api1_po4 =            AK_PHOS_1_MILL95(t_k, s, p_bar) * zcvt_htot_o_hsws
api2_po4 = api1_po4 * AK_PHOS_2_MILL95(t_k, s, p_bar) * zcvt_htot_o_hsws
api3_po4 = api2_po4 * AK_PHOS_3_MILL95(t_k, s, p_bar) * zcvt_htot_o_hsws

api1_sil =            AK_SILI_1_MILL95(t_k, s       ) * zcvt_htot_o_hsws

api1_nh4 =            AK_AMMO_1_YAMI95(t_k, s, p_bar) * zcvt_htot_o_hsws

api1_h2s =            AK_H2S_1_MILL95 (t_k, s, p_bar) * zcvt_htot_o_hsws

api1_so4 =            AK_HSO4_DICK90(t_k, s, p_bar) * zcvt_htot_o_hfree

api1_flu =            AK_HF_PEFR87(t_k, s, p_bar)

api1_wat =            AK_W_MILL95(t_k, s, p_bar)      * zcvt_htot_o_hsws

aphscale =            zcvt_htot_o_hfree

!=======================================================================
 END SUBROUTINE SETUP_API4PHTOT
!=======================================================================




!=======================================================================
 SUBROUTINE SETUP_API4PHSWS(t_k, s, p_bar)
!=======================================================================

! ------------------
! Argument variables
! ------------------

!     t_k    : temperature in Kelvin
!     s      : salinity
!     p_bar  : applied pressure in bar

REAL(KIND=wp), INTENT(IN) :: t_k
REAL(KIND=wp), INTENT(IN) :: s
REAL(KIND=wp), INTENT(IN) :: p_bar

! ---------------
! Local variables
! ---------------

REAL(KIND=wp) :: zcvt_hsws_o_htot, zcvt_hsws_o_hfree


zcvt_hsws_o_htot  = ACVT_HSWS_O_HTOT(t_k, s, p_bar)
zcvt_hsws_o_hfree = ACVT_HSWS_O_HFREE(t_k, s, p_bar)


api1_dic =            AK_CARB_1_MILL95(t_k, s, p_bar)
api2_dic = api1_dic * AK_CARB_2_MILL95(t_k, s, p_bar)

api1_bor =            AK_BORA_DICK90(t_k, s, p_bar) * zcvt_hsws_o_htot

api1_po4 =            AK_PHOS_1_MILL95(t_k, s, p_bar)
api2_po4 = api1_po4 * AK_PHOS_2_MILL95(t_k, s, p_bar)
api3_po4 = api2_po4 * AK_PHOS_3_MILL95(t_k, s, p_bar)

api1_sil =            AK_SILI_1_MILL95(t_k, s       )

api1_nh4 =            AK_AMMO_1_YAMI95(t_k, s, p_bar)

api1_h2s =            AK_H2S_1_MILL95 (t_k, s, p_bar)

api1_so4 =            AK_HSO4_DICK90(t_k, s, p_bar) * zcvt_hsws_o_hfree

api1_flu =            zcvt_hsws_o_hfree/ABETA_HF_DIRI79(t_k, s, p_bar)

api1_wat =            AK_W_MILL95(t_k, s, p_bar)

aphscale =            zcvt_hsws_o_hfree

!=======================================================================
 END SUBROUTINE SETUP_API4PHSWS
!=======================================================================




!=======================================================================
 FUNCTION AK_CARB_0_WEIS74(t_k, s)
!=======================================================================

! Function calculates K0 in (mol/kg-SW)/atmosphere

! References: Weiss (1979) [(mol/kg-SW)/atm]
! pH scale  : N/A
! Note      : currently no pressure correction


IMPLICIT NONE

REAL(KIND=wp) :: AK_CARB_0_WEIS74


! ------------------
! Argument variables
! ------------------

!     s      : salinity
!     t_k    : temperature in K

REAL(KIND=wp), INTENT(IN) :: t_k
REAL(KIND=wp), INTENT(IN) :: s


! ---------------
! Local variables
! ---------------

!     zt_k_o_100   : zt_k/100

REAL(KIND=wp) :: zt_k_o_100



zt_k_o_100 = t_k/100._wp

AK_CARB_0_WEIS74                                                       &
          = EXP( -60.2409_wp + 93.4517_wp/zt_k_o_100                   &
                + 23.3585_wp*LOG(zt_k_o_100)                           &
                + (   0.023517_wp - 0.023656_wp*zt_k_o_100             &
                   + 0.0047036_wp*zt_k_o_100*zt_k_o_100)*s )


RETURN

!=======================================================================
 END FUNCTION AK_CARB_0_WEIS74
!=======================================================================




!=======================================================================
 FUNCTION AK_CARB_1_MILL95(t_k, s, p_bar)
!=======================================================================
! Function calculates first dissociation constant of carbonic acid
! in mol/kg-SW on the SWS pH-scale.

! References: Millero (1995, eq 50 -- ln K1(COM))
!             Millero (1982) pressure correction
! pH scale:   SWS


IMPLICIT NONE

REAL(KIND=wp) :: AK_CARB_1_MILL95


! ------------------
! Argument variables
! ------------------

!     t_k    : temperature in Kelvin
!     s      : salinity
!     p_bar  : applied pressure in bar

REAL(KIND=wp), INTENT(IN) :: t_k
REAL(KIND=wp), INTENT(IN) :: s
REAL(KIND=wp), INTENT(IN) :: p_bar


! ---------------
! Local variables
! ---------------

!     zrt            : R*t_k, R in bar*cm3/(mol*K)
!     zt_degc        : temperature in degrees Celsius
!     zdvi           : volume change for ionization
!     zdki           : compressibility change for ionization
!     zsqrts         : square root of salinity
!     zds            : salinity-34.8
!     zln_kc1_p0     : ln(K_C1) at p_bar = 0
!     zln_kc1_pp     : pressure correction for p_bar /= 0

REAL(KIND=wp) :: zrt, zt_degc, zdvi, zdki, zds, zsqrts
REAL(KIND=wp) :: zln_kc1_p0, zln_kc1_pp


! ln(K_C1) value at p_bar = 0

zsqrts     = SQRT(s)

zln_kc1_p0 =     2.18867_wp                                                    &
             - 2275.0360_wp/t_k                                                &
             - 1.468591_wp*LOG(t_k)                                            &
             + ( -0.138681_wp - 9.33291_wp/t_k)*zsqrts                         &
             + 0.0726483_wp*s - 0.00574938_wp*s*zsqrts


! Pressure correction

zt_degc    = t_k - t_k_zerodegc
zds        = s - 34.8_wp
zrt        = gasconst_bar_cm3_o_mol_k * t_k

zdvi       =  -25.50_wp - 0.151_wp*zds + 0.1271_wp*zt_degc
zdki       = ( -3.08_wp - 0.578_wp*zds + 0.0877_wp*zt_degc)*1.0E-03_wp

zln_kc1_pp = (-zdvi + zdki*p_bar/2._wp)*p_bar/zrt


! Final K_C1 value

AK_CARB_1_MILL95 = EXP( zln_kc1_p0 + zln_kc1_pp )

RETURN

!=======================================================================
 END FUNCTION AK_CARB_1_MILL95
!=======================================================================




!=======================================================================
 FUNCTION AK_CARB_2_MILL95(t_k, s, p_bar)
!=======================================================================
! Function calculates second dissociation constant K1
! in mol/kg-SW on the SWS pH-scale.

! References: Millero (1995, eq 51 -- ln K2(COM))
!             Millero (1979) pressure correction
! pH scale:   SWS


IMPLICIT NONE

REAL(KIND=wp) :: AK_CARB_2_MILL95


! Argument variables
! ------------------

!     t_k    : temperature in Kelvin
!     s      : salinity
!     p_bar  : applied pressure in bar

REAL(KIND=wp), INTENT(IN) :: t_k
REAL(KIND=wp), INTENT(IN) :: s
REAL(KIND=wp), INTENT(IN) :: p_bar


! Local variables
! ---------------

!     zrt            : R*t_k, R in bar*cm3/(mol*K)
!     zt_degc        : temperature in degrees Celsius
!     zdvi           : volume change for ionization
!     zdki           : compressibility change for ionization
!     zsqrts         : square root of salinity
!     zds            : salinity-34.8
!     zln_kc2_p0     : ln(K_C2) at p_bar = 0
!     zln_kc2_pp     : pressure correction for p_bar /= 0

REAL(KIND=wp) :: zrt, zt_degc, zdvi, zdki, zds, zsqrts
REAL(KIND=wp) :: zln_kc2_p0, zln_kc2_pp


! ln(K_C2) value at p_bar = 0

zsqrts     = SQRT(s)

zln_kc2_p0 =    -0.84226_wp                                                    &
             - 3741.1288_wp/t_k                                                &
             -  1.437139_wp*LOG(t_k)                                           &
             + (-0.128417_wp - 24.41239_wp/t_k)*zsqrts                         &
             +  0.1195308_wp*s                                                 &
             - 0.00912840_wp*s*zsqrts


! Pressure correction

zt_degc    = t_k - t_k_zerodegc
zds        = s - 34.8_wp
zrt        = gasconst_bar_cm3_o_mol_k * t_k

zdvi       =  -15.82_wp + 0.321_wp*zds - 0.0219_wp*zt_degc
zdki       =  ( 1.13_wp - 0.314_wp*zds - 0.1475_wp*zt_degc)*1.0E-03_wp

zln_kc2_pp =  (-zdvi + zdki*p_bar/2._wp)*p_bar/zrt


! Final K_C2 value

AK_CARB_2_MILL95  = EXP( zln_kc2_p0 + zln_kc2_pp )

RETURN

!=======================================================================
 END FUNCTION AK_CARB_2_MILL95
!=======================================================================




!=======================================================================
 FUNCTION AK_CARB_1_LUEK00(t_k, s, p_bar)
!=======================================================================
! Function calculates first dissociation constant of carbonic acid
! in mol/kg-SW on the Total pH-scale.

! References: Luecker et al. (2000) -- also Handbook (2007)
!             Millero (1979) pressure correction
! pH scale:   Total


IMPLICIT NONE

REAL(KIND=wp) :: AK_CARB_1_LUEK00


! Argument variables
! ------------------

!     t_k    : temperature in Kelvin
!     s      : salinity
!     p_bar  : applied pressure in bar

REAL(KIND=wp), INTENT(IN) :: t_k
REAL(KIND=wp), INTENT(IN) :: s
REAL(KIND=wp), INTENT(IN) :: p_bar


! Local variables
! ---------------

!     zrt            : R*t_k, R in bar*cm3/(mol*K)
!     zt_degc        : temperature in degrees Celsius
!     zdvi           : volume change for ionization
!     zdki           : compressibility change for ionization
!     zds            : salinity-34.8
!     zlog10_kc1_p0  : log_10(k_C1) at p_bar = 0
!     zln_kc1_pp     : pressure correction for p_bar /= 0

REAL(KIND=wp) :: zrt, zt_degc, zdvi, zdki, zds, zsqrts
REAL(KIND=wp) :: zlog10_kc1_p0, zln_kc1_pp


! log_10(K_C1) value at p_bar = 0

zlog10_kc1_p0 =   61.2172_wp                                                   &
                - 3633.86_wp/t_k                                               &
                - 9.67770_wp*LOG(t_k)                                          &
                + s*(0.011555 - s*0.0001152_wp)


! Pressure correction

zt_degc    = t_k - t_k_zerodegc
zds        = s - 34.8_wp
zrt        = gasconst_bar_cm3_o_mol_k * t_k

zdvi       =  -25.50_wp - 0.151_wp*zds + 0.1271_wp*zt_degc
zdki       = ( -3.08_wp - 0.578_wp*zds + 0.0877_wp*zt_degc)*1.0E-03_wp

zln_kc1_pp = (-zdvi + zdki*p_bar/2._wp)*p_bar/zrt


! Final K_C1 value

AK_CARB_1_LUEK00 = 10._wp**zlog10_kc1_p0 * EXP(zln_kc1_pp)

RETURN

!=======================================================================
 END FUNCTION AK_CARB_1_LUEK00
!=======================================================================




!=======================================================================
 FUNCTION AK_CARB_2_LUEK00(t_k, s, p_bar)
!=======================================================================
! Function calculates second dissociation constant K1
! in mol/kg-SW on the Total pH-scale.

! References: Luecker et al. (2000) -- also Handbook (2007)
!             Millero (1979) pressure correction
! pH scale:   Total


IMPLICIT NONE

REAL(KIND=wp) :: AK_CARB_2_LUEK00


! Argument variables
! ------------------

!     t_k    : temperature in Kelvin
!     s      : salinity
!     p_bar  : applied pressure in bar

REAL(KIND=wp), INTENT(IN) :: t_k
REAL(KIND=wp), INTENT(IN) :: s
REAL(KIND=wp), INTENT(IN) :: p_bar


! Local variables
! ---------------

!     zrt            : R*t_k, R in bar*cm3/(mol*K)
!     zt_degc        : temperature in degrees Celsius
!     zdvi           : volume change for ionization
!     zdki           : compressibility change for ionization
!     zsqrts         : square root of salinity
!     zds            : salinity-34.8
!     zlog10_kc2_p0  : log_10(K_C2) at p_bar = 0
!     zln_kc2_pp     : pressure correction for p_bar /= 0

REAL(KIND=wp) :: zrt, zt_degc, zdvi, zdki, zds, zsqrts
REAL(KIND=wp) :: zlog10_kc2_p0, zln_kc2_pp


! log_10(K_C2) value at p_bar = 0

zlog10_kc2_p0 =  -25.9290_wp                                                   &
                -  471.78_wp/t_k + 3.16967_wp*LOG(t_k)                         &
                + s*(0.01781_wp - s*0.0001122_wp)


! Pressure correction

zt_degc    = t_k - t_k_zerodegc
zds        = s - 34.8_wp
zrt        = gasconst_bar_cm3_o_mol_k * t_k

zdvi       =  -15.82_wp + 0.321_wp*zds - 0.0219_wp*zt_degc
zdki       =  ( 1.13_wp - 0.314_wp*zds - 0.1475_wp*zt_degc)*1.0E-03_wp

zln_kc2_pp =  (-zdvi + zdki*p_bar/2._wp)*p_bar/zrt


! Final K_C2 value

AK_CARB_2_LUEK00  = 10._wp**zlog10_kc2_p0 *EXP(zln_kc2_pp)

RETURN

!=======================================================================
 END FUNCTION AK_CARB_2_LUEK00
!=======================================================================








!=======================================================================
 FUNCTION AK_CARB_1_ROYE93(t_k, s, p_bar)
!=======================================================================
! Function calculates first dissociation constant of carbonic acid
! in mol/kg-SW on the Total pH-scale.

! References: Roy et al. (1993) -- also Handbook (1994)
!             Millero (1979) pressure correction
! pH scale  : Total
! Note      : converted here from mol/kg-H2O to mol/kg-SW


IMPLICIT NONE

REAL(KIND=wp) :: AK_CARB_1_ROYE93


! Argument variables
! ------------------

!     t_k    : temperature in Kelvin
!     s      : salinity
!     p_bar  : applied pressure in bar

REAL(KIND=wp), INTENT(IN) :: t_k
REAL(KIND=wp), INTENT(IN) :: s
REAL(KIND=wp), INTENT(IN) :: p_bar


! Local variables
! ---------------

!     zrt            : R*t_k, R in bar*cm3/(mol*K)
!     zt_degc        : temperature in degrees Celsius
!     zdvi           : volume change for ionization
!     zdki           : compressibility change for ionization
!     zds            : salinity-34.8
!     zln_kc1_p0     : ln(k_C1) at p_bar = 0
!     zln_kc1_pp     : pressure correction for p_bar /= 0

REAL(KIND=wp) :: zsqrts, zcvt_to_kgsw
REAL(KIND=wp) :: zrt, zt_degc, zdvi, zdki, zds
REAL(KIND=wp) :: zln_kc1_p0, zln_kc1_pp


! ln(K_C1) value at p_bar = 0

zsqrts     = SQRT(s)
zcvt_to_kgsw    = ACVT_KGH2O_O_KGSW(s)

zln_kc1_p0 =   -2307.1255_wp/t_k + 2.83655_wp - 1.5529413_wp*LOG(t_k)        &
             + (-4.0484_wp/t_k - 0.20760841)*zsqrts                          &
             + 0.08468345*s                                                  &
             - 0.00654208*zsqrts*s


! Pressure correction

zt_degc    = t_k - t_k_zerodegc
zds        = s - 34.8_wp
zrt        = gasconst_bar_cm3_o_mol_k * t_k

zdvi       =  -25.50_wp - 0.151_wp*zds + 0.1271_wp*zt_degc
zdki       = ( -3.08_wp - 0.578_wp*zds + 0.0877_wp*zt_degc)*1.0E-03_wp

zln_kc1_pp = (-zdvi + zdki*p_bar/2._wp)*p_bar/zrt


! Final K_C1 value

AK_CARB_1_ROYE93 = EXP(zln_kc1_p0 + zln_kc1_pp) * zcvt_to_kgsw

RETURN

!=======================================================================
 END FUNCTION AK_CARB_1_ROYE93
!=======================================================================




!=======================================================================
 FUNCTION AK_CARB_2_ROYE93(t_k, s, p_bar)
!=======================================================================
! Function calculates second dissociation constant K1
! in mol/kg-SW on the Total pH-scale.

! References: Roy et al. (1993) -- also Handbook (1994)
!             Millero (1979) pressure correction
! pH scale  : Total
! Note      : converted here from mol/kg-H2O to mol/kg-SW


IMPLICIT NONE

REAL(KIND=wp) :: AK_CARB_2_ROYE93


! Argument variables
! ------------------

!     t_k    : temperature in Kelvin
!     s      : salinity
!     p_bar  : applied pressure in bar

REAL(KIND=wp), INTENT(IN) :: t_k
REAL(KIND=wp), INTENT(IN) :: s
REAL(KIND=wp), INTENT(IN) :: p_bar


! Local variables
! ---------------

!     zrt            : R*t_k, R in bar*cm3/(mol*K)
!     zt_degc        : temperature in degrees Celsius
!     zdvi           : volume change for ionization
!     zdki           : compressibility change for ionization
!     zsqrts         : square root of salinity
!     zds            : salinity-34.8
!     zln_kc2_p0     : ln(K_C2) at p_bar = 0
!     zln_kc2_pp     : pressure correction for p_bar /= 0

REAL(KIND=wp) :: zsqrts, zcvt_to_kgsw
REAL(KIND=wp) :: zrt, zt_degc, zdvi, zdki, zds
REAL(KIND=wp) :: zln_kc2_p0, zln_kc2_pp


! ln(K_C2) value at p_bar = 0

zsqrts     = SQRT(s)
zcvt_to_kgsw    = ACVT_KGH2O_O_KGSW(s)

zln_kc2_p0 =   -3351.6106_wp/t_k - 9.226508_wp - 0.2005743_wp*LOG(t_k)         &
             + ( -23.9722_wp/t_k - 0.106901773_wp)*zsqrts                      &
             + 0.1130822*s - 0.00846934_wp*zsqrts*s


! Pressure correction

zt_degc    = t_k - t_k_zerodegc
zds        = s - 34.8_wp
zrt        = gasconst_bar_cm3_o_mol_k * t_k

zdvi       =  -15.82_wp + 0.321_wp*zds - 0.0219_wp*zt_degc
zdki       =  ( 1.13_wp - 0.314_wp*zds - 0.1475_wp*zt_degc)*1.0E-03_wp

zln_kc2_pp =  (-zdvi + zdki*p_bar/2._wp)*p_bar/zrt


! Final K_C2 value

AK_CARB_2_ROYE93  = EXP(zln_kc2_p0 + zln_kc2_pp) * zcvt_to_kgsw

RETURN

!=======================================================================
 END FUNCTION AK_CARB_2_ROYE93
!=======================================================================




!=======================================================================
 FUNCTION AK_BORA_DICK90(t_k, s, p_bar)
!=======================================================================
! Function calculates boric acid dissociation constant KB
! in mol/kg-SW on the total pH-scale.

! References: Dickson (1990, eq. 23) -- also Handbook (2007, eq. 37)
!             Millero (1979) pressure correction
! pH scale  : total


IMPLICIT NONE

REAL(KIND=wp) :: AK_BORA_DICK90


! ------------------
! Argument variables
! ------------------

!     t_k    : temperature in Kelvin
!     s      : salinity
!     p_bar  : applied pressure in bar

REAL(KIND=wp), INTENT(IN) :: t_k
REAL(KIND=wp), INTENT(IN) :: s
REAL(KIND=wp), INTENT(IN) :: p_bar


! ---------------
! Local variables
! ---------------

!     zrt          : R*t_k, R in bar*cm3/(mol*K)
!     zt_degc      : temperature in degrees Celsius
!     zdvi         : volume change for ionization
!     zdki         : compressibility change for ionization
!     zsqrts       : square root of salinity
!     zds          : salinity-34.8
!     zln_kb_p0    : K_b at p_bar = 0
!     zln_kb_pp    : pressure correction for p_bar /= 0

REAL(KIND=wp) :: zrt, zt_degc, zdvi, zdki, zds, zsqrts
REAL(KIND=wp) :: zln_kb_p0, zln_kb_pp


! ln(K_B) value at p_bar = 0

zsqrts     = SQRT(s)

zln_kb_p0  = ( -8966.90_wp                                                     &
                          + zsqrts*( -2890.53_wp                               &
                          + zsqrts*(  -77.942_wp                               &
                          + zsqrts*(    1.728_wp - 0.0996_wp*zsqrts)))) / t_k  &
               +  148.0248_wp + zsqrts*(137.1942_wp + zsqrts*1.62142_wp)       &
               + (-24.4344_wp + zsqrts*(-25.085_wp - zsqrts*0.2474_wp)) * LOG(t_k) &
               + 0.053105_wp*zsqrts*t_k


! Pressure correction

zt_degc   = t_k - t_k_zerodegc
zds       = s - 34.8_wp
zrt       = gasconst_bar_cm3_o_mol_k * t_k

zdvi      = -29.48_wp + 0.295_wp*zds + 0.1622_wp*zt_degc - 0.002608_wp*zt_degc*zt_degc
zdki      = (-2.84_wp + 0.354_wp*zds)*1.0E-03_wp

zln_kb_pp =  (-zdvi + zdki*p_bar/2._wp)*p_bar/zrt


! Final K_B value

AK_BORA_DICK90   = EXP( zln_kb_p0 + zln_kb_pp )

!=======================================================================
 END FUNCTION AK_BORA_DICK90
!=======================================================================




!=======================================================================
 FUNCTION AK_W_MILL95(t_k, s, p_bar)
!=======================================================================

! Function calculates water dissociation constant Kw in (mol/kg-SW)^2

! References: Millero (1995) for value at p_bar = 0
!             Millero (pers. comm. 1996) for pressure correction
! pH scale  : SWS


IMPLICIT NONE

REAL(KIND=wp) :: AK_W_MILL95


! ------------------
! Argument variables
! ------------------

!     t_k    : temperature in K
!     s      : salinity
!     p_bar  : applied pressure in bar

REAL(KIND=wp), INTENT(IN) ::  t_k, s, p_bar


! ---------------
! Local variables
! ---------------

!     zrt        : R*t_k
!     zt_degc    : temperature in degrees Celsius
!     zdvi       : volume change for ionization
!     zdki       : compressibility change for ionization
!     zln_kw_p0  : ln(K_w) at p_bar = 0
!     zln_kw_pp  : pressure correction for p_bar /= 0

REAL(KIND=wp) :: zrt, zt_degc, zdvi, zdki, zds, zsqrts
REAL(KIND=wp) :: zln_kw_p0, zln_kw_pp


! ln(K_w) value at p_bar = 0

zln_kw_p0 =    148.9802_wp                                                     &
             - 13847.26_wp/t_k                                                 &
             -  23.6521_wp*LOG(t_k)                                            &
             + ( -5.977_wp + 118.67_wp/t_k + 1.0495_wp*LOG(t_k))*SQRT(s)       &
             - 0.01615_wp*s


! Pressure correction

zt_degc = t_k - t_k_zerodegc
zrt     = gasconst_bar_cm3_o_mol_k * t_k

zdvi    =  -20.02_wp + 0.1119_wp*zt_degc - 0.1409E-02_wp*zt_degc*zt_degc
zdki    = ( -5.13_wp + 0.0794_wp*zt_degc)*1.0E-03_wp

zln_kw_pp =  (-zdvi + zdki*p_bar/2._wp)*p_bar/zrt


! Final K_w value

AK_W_MILL95 = EXP( zln_kw_p0 + zln_kw_pp )


RETURN

!=======================================================================
END FUNCTION AK_W_MILL95
!=======================================================================




!=======================================================================
FUNCTION AK_PHOS_1_MILL95(t_k, s, p_bar)
!=======================================================================

! Function returns the first dissociation constant
! of phosphoric acid (H3PO4) in seawater

! References: Yao and Millero (1995)
!             Millero (1995) for pressure correction
! pH scale  : SWS


IMPLICIT NONE

REAL(KIND=wp) :: AK_PHOS_1_MILL95

! ------------------
! Argument variables
! ------------------

!     t_k    : temperature in K
!     s      : salinity
!     p_bar  : applied pressure in bar

REAL(KIND=wp), INTENT(IN) ::  t_k, s, p_bar

! ---------------
! Local variables
! ---------------

!     zrt          : R*t_k, R in bar*cm3/(mol*K)
!     zt_degc      : temperature in degrees Celsius
!     zdvi         : volume change for ionization
!     zdki         : compressibility change for ionization
!     zln_kp1_p0   : ln(K_p1) at p_bar = 0
!     zln_kp1_pp   : pressure correction for p_bar /= 0

REAL(KIND=wp) :: zrt, zt_degc, zdvi, zdki
REAL(KIND=wp) :: zln_kp1_p0, zln_kp1_pp


! ln(K_P1) for p_bar = 0

zln_kp1_p0 =      115.54_wp - 4576.752_wp/t_k - 18.453_wp*LOG(t_k)     &
             + ( 0.69171_wp -  106.736_wp/t_k)* SQRT(s)               &
             + (-0.01844_wp -  0.65643_wp/t_k)*s


! Pressure correction

zt_degc   = t_k - t_k_zerodegc
zrt       = gasconst_bar_cm3_o_mol_k * t_k

zdvi      =  -14.51_wp + 0.1211_wp*zt_degc - 0.321E-03*zt_degc*zt_degc
zdki      = ( -2.67_wp + 0.0427_wp*zt_degc)*1.0E-03_wp

zln_kp1_pp = (-zdvi + zdki*p_bar/2._wp)*p_bar/zrt


! Final value of K_P1

AK_PHOS_1_MILL95 = EXP(zln_kp1_p0 + zln_kp1_pp)

RETURN

!=======================================================================
 END FUNCTION AK_PHOS_1_MILL95
!=======================================================================



!=======================================================================
 FUNCTION AK_PHOS_2_MILL95(t_k, s, p_bar)
!=======================================================================

! Function returns the second dissociation constant
! of phosphoric acid (H3PO4) in seawater

! References: Yao and Millero (1995)
!             Millero (1995) for pressure correction
! pH scale  : SWS


IMPLICIT NONE

REAL(KIND=wp) :: AK_PHOS_2_MILL95

! ------------------
! Argument variables
! ------------------

!     t_k    : temperature in K
!     s      : salinity
!     p_bar  : applied pressure in bar

REAL(KIND=wp), INTENT(IN) ::  t_k, s, p_bar


! ---------------
! Local variables
! ---------------

!     zrt          : R*t_k, R in bar*cm3/(mol*K)
!     zt_degc      : temperature in degrees Celsius
!     zdvi         : volume change for ionization
!     zdki         : compressibility change for ionization
!     zln_kp2_p0   : ln(K_P2) at p_bar = 0
!     zln_kp2_pp   : pressure correction for p_bar /= 0

REAL(KIND=wp) :: zrt, zt_degc, zdvi, zdki
REAL(KIND=wp) :: zln_kp2_p0, zln_kp2_pp


! ln(K_P2) for p_bar = 0

zln_kp2_p0 =   172.1033_wp                                                     &
             - 8814.715_wp/t_k                                                 &
             -   27.927_wp*LOG(t_k)                                            &
             + (  1.3566_wp -  160.340_wp/t_k)*SQRT(s)                         &
             + (-0.05778_wp +  0.37335_wp/t_k)*s


! Pressure correction

zt_degc    = t_k - t_k_zerodegc
zrt        = gasconst_bar_cm3_o_mol_k * t_k

zdvi       =  -23.12_wp + 0.1758_wp*zt_degc -2.647E-03_wp*zt_degc*zt_degc
zdki       = ( -5.15_wp +   0.09_wp*zt_degc)*1.0E-03_wp

zln_kp2_pp = (-zdvi + zdki*p_bar/2._wp)*p_bar/zrt


! Final K_P2 value

AK_PHOS_2_MILL95  = EXP( zln_kp2_p0 + zln_kp2_pp )

RETURN

!=======================================================================
 END FUNCTION AK_PHOS_2_MILL95
!=======================================================================



!=======================================================================
FUNCTION AK_PHOS_3_MILL95(t_k, s, p_bar)
!=======================================================================

! Function returns the third dissociation constant
! of phosphoric acid (H3PO4) in seawater

! References: Yao and Millero (1995)
!             Millero (1995) for pressure correction
! pH scale  : SWS


IMPLICIT NONE

REAL(KIND=wp) :: AK_PHOS_3_MILL95

! ------------------
! Argument variables
! ------------------

!     t_k    : temperature in K
!     s      : salinity
!     p_bar  : applied pressure in bar

REAL(KIND=wp), INTENT(IN) ::  t_k, s, p_bar


! ---------------
! Local variables
! ---------------

!     zrt          : R*t_k, R in bar*cm3/(mol*K)
!     zt_degc      : temperature in degrees Celsius
!     zdvi         : volume change for ionization
!     zdki         : compressibility change for ionization
!     zln_kp3_p0   : ln(K_P3) at p_bar = 0
!     zln_kp3_pp   : pressure correction for p_bar /= 0

REAL(KIND=wp) :: zrt, zt_degc, zdvi, zdki
REAL(KIND=wp) :: zln_kp3_p0, zln_kp3_pp


! ln(K_P3) for p_bar = 0

zln_kp3_p0 =     -18.126_wp  -  3070.75_wp/t_k                        &
               + ( 2.81197_wp + 17.27039_wp/t_k)*SQRT(s)               &
               + (-0.09984_wp - 44.99486_wp/t_k)*s


! Pressure correction

zt_degc   = t_k - t_k_zerodegc
zrt       = gasconst_bar_cm3_o_mol_k * t_k

zdvi      =  -26.57_wp + 0.2020_wp*zt_degc -3.042E-03*zt_degc*zt_degc
zdki      = ( -4.08_wp + 0.0714_wp*zt_degc)*1.0E-03_wp

zln_kp3_pp = (-zdvi + zdki*p_bar/2._wp)*p_bar/zrt


! Final K_P3 value

AK_PHOS_3_MILL95 = EXP( zln_kp3_p0 + zln_kp3_pp )

RETURN

!=======================================================================
END FUNCTION AK_PHOS_3_MILL95
!=======================================================================



!=======================================================================
FUNCTION AK_SILI_1_MILL95(t_k, s)
!=======================================================================

! Function returns the first dissociation constant
! of silicic acid (H4SiO4) in seawater

! References: Yao and Millero (1995) cited by Millero (1995)
! pH scale  : SWS (according to Dickson et al, 2007)
! Note      : No pressure correction available
! Note      : converted here from mol/kg-H2O to mol/kg-sw


IMPLICIT NONE

REAL(KIND=wp) :: AK_SILI_1_MILL95

! ------------------
! Argument variables
! ------------------

!     t_k    : temperature in K
!     s      : salinity

REAL(KIND=wp), INTENT(IN) :: t_k, s


! ---------------
! Local variables
! ---------------

!     zcvt_to_kgsw: fraction of pure water in 1 kg seawater at salinity s
!     zionst      : ionic strength [mol/kg-H2O]
!     zln_ksi1_p0 : ln(K_Si1) at p_bar = 0
!     zln_ksi1_pp : pressure correciotn for p_bar /= 0

REAL(KIND=wp) :: zionst, zcvt_to_kgsw
REAL(KIND=wp) :: zln_ksi1_p0, zln_ksi1_pp


! K_Si1 value at p_bar = 0

zcvt_to_kgsw = ACVT_KGH2O_O_KGSW(s)
zionst       = A_IONSTRENGTH_SALIN(s)/zcvt_to_kgsw  ! mol/kg-H2O !!

zln_ksi1_p0  =     117.40_wp -  8904.2_wp/t_k - 19.334_wp * LOG(t_k)    &
               + ( 3.5913_wp -  458.79_wp/t_k) * SQRT(zionst)           &
               + (-1.5998_wp +  188.74_wp/t_k) * zionst                 &
               + (0.07871_wp - 12.1652_wp/t_k) * zionst*zionst


! Pressure correction : currently none

zln_ksi1_pp = 0._wp


! Final value

AK_SILI_1_MILL95 = EXP( zln_ksi1_p0 + zln_ksi1_pp ) * zcvt_to_kgsw


RETURN

!=======================================================================
END FUNCTION AK_SILI_1_MILL95
!=======================================================================



!=======================================================================
FUNCTION AK_H2S_1_MILL95(t_k, s, p_bar)
!=======================================================================

! Function returns the dissociation constant of hydrogen sulfide in sea-water


! References: Millero et al. (1988) (cited by Millero (1995)
!             Millero (1995) for pressure correction
! pH scale  : - SWS (according to Yao and Millero, 1995, p. 82: "refitted if necessary")
!             - Total (according to Lewis and Wallace, 1998)
! Note      : we stick to SWS here for the time being
! Note      : the fits from Millero (1995) and Yao and Millero (1995)
!             derive from Millero et al. (1998), with all the coefficients
!             multiplied by -ln(10)

IMPLICIT NONE

REAL(KIND=wp) :: AK_H2S_1_MILL95

! ------------------
! Argument variables
! ------------------

!     t_k    : temperature in K
!     s      : salinity
!     p_bar  : applied pressure in bar

REAL(KIND=wp), INTENT(IN) ::  t_k, s, p_bar


! ---------------
! Local variables
! ---------------

!     zt_degc      : temperature in degrees Celsius
!     zrt          : R*t_k, R in bar*cm3/(mol*K)
!     zdvi         : volume change for ionization
!     zdki         : compressibility change for ionization
!     zln_kh2s_p0  : ln(K_H2S) at p_bar = 0
!     zln_kh2s_pp  : pressure correction for p_bar /= 0

REAL(KIND=wp) :: zrt, zt_degc, zdvi, zdki
REAL(KIND=wp) :: zln_kh2s_p0, zln_kh2s_pp


! K_H2S value at p_bar = 0
! ------------------------

zln_kh2s_p0  =    225.838_wp                                                     &
                - 13275.3_wp/t_k                                                 &
                - 34.6435_wp * LOG(t_k)                                          &
                +  0.3449_wp*SQRT(s)                                             &
                -  0.0274_wp*s


! Pressure correction
! -------------------

zt_degc      = t_k - t_k_zerodegc
zrt          = gasconst_bar_cm3_o_mol_k * t_k

zdvi         =  -14.80_wp + zt_degc*(0.0020_wp - zt_degc*0.400E-03_wp)
zdki         = (  2.89_wp + zt_degc*0.054_wp)*1.0E-03_wp

zln_kh2s_pp  = (-zdvi + zdki*p_bar/2._wp)*p_bar/zrt


! Final K_H2S value
! -----------------

AK_H2S_1_MILL95 = EXP( zln_kh2s_p0 + zln_kh2s_pp )


RETURN

!=======================================================================
END FUNCTION AK_H2S_1_MILL95
!=======================================================================



!=======================================================================
FUNCTION AK_AMMO_1_YAMI95(t_k, s, p_bar)
!=======================================================================

! Function returns the dissociation constant
! of ammonium in sea-water [mol/kg-SW]


! References: Yao and Millero (1995)
!             Millero (1995) for pressure correction
! pH scale  : SWS


IMPLICIT NONE

REAL(KIND=wp) :: AK_AMMO_1_YAMI95

! ------------------
! Argument variables
! ------------------

!     t_k    : temperature in K
!     s      : salinity
!     p_bar  : applied pressure in bar

REAL(KIND=wp), INTENT(IN) ::  t_k, s, p_bar


! ---------------
! Local variables
! ---------------

!     zt_degc      : temperature in degrees Celsius
!     zrt          : R*t_k, R in bar*cm3/(mol*K)
!     zdvi         : volume change for ionization
!     zdki         : compressibility change for ionization
!     zln_knh4_p0  : ln(K_NH4) at p_bar = 0
!     zln_knh4_pp  : pressure correction for p_bar /= 0

REAL(KIND=wp) :: zrt, zt_degc, zdvi, zdki
REAL(KIND=wp) :: zln_knh4_p0, zln_knh4_pp


! K_NH4 value at p_bar = 0
! ------------------------

zln_knh4_p0  =    -0.25444_wp -  6285.33_wp/t_k + 0.0001635_wp*t_k             &
               + ( 0.46532_wp - 123.7184_wp/t_k) * SQRT(s)                     &
               + (-0.01992_wp +  3.17556_wp/t_k) * s


! Pressure correction
! -------------------

zt_degc      = t_k - t_k_zerodegc
zrt          = gasconst_bar_cm3_o_mol_k * t_k

zdvi         =  -26.43_wp + zt_degc*(0.0889_wp - zt_degc*0.905E-03_wp)
zdki         = ( -5.03_wp + zt_degc*0.0814_wp)*1.0E-03_wp

zln_knh4_pp  = (-zdvi + zdki*p_bar/2._wp)*p_bar/zrt


! Final K_NH4 value
! -----------------

AK_AMMO_1_YAMI95  = EXP( zln_knh4_p0 + zln_knh4_pp )

RETURN

!=======================================================================
END FUNCTION AK_AMMO_1_YAMI95
!=======================================================================



!=======================================================================
FUNCTION ACVT_KGH2O_O_KGSW(s)
!=======================================================================

! Function returns the mass of pure water in one kg of seawater
! of salinity s

! References: "libthdyct" -- derived by Munhoven (1997) from data by Millero (1982)
!             "Handbook (2007)" -- Handbook (2007)
! pH scale:   N/A


IMPLICIT NONE

REAL(KIND=wp) :: ACVT_KGH2O_O_KGSW

REAL(KIND=wp), INTENT(IN) :: s

!ACVT_KGH2O_O_KGSW = 1._wp - 0.0010049_wp*s ! libthdyct
ACVT_KGH2O_O_KGSW = 1._wp - 0.001005_wp*s ! Handbook (2007)

RETURN


!=======================================================================
END FUNCTION ACVT_KGH2O_O_KGSW
!=======================================================================



!=======================================================================
FUNCTION A_IONSTRENGTH_SALIN(s)
!=======================================================================

! Function calculates ionic strength in mol/kg-SW, for given salinity.

! References: "libthdyct" -- derived by Munhoven (1997) from data by Millero (1982)
!             "Handbook (2007)" -- Handbook (2007)
! pH scale:   N/A


IMPLICIT NONE

REAL(KIND=wp) :: A_IONSTRENGTH_SALIN


! ------------------
! Argument variables
! ------------------

REAL(KIND=wp), INTENT(IN) :: s



!A_IONSTRENGTH_SALIN = (0.019920D+00*s) ! libthdyct
A_IONSTRENGTH_SALIN = (0.019924D+00*s) ! Handbook (2007)

RETURN

!=======================================================================
END FUNCTION A_IONSTRENGTH_SALIN
!=======================================================================




!=======================================================================
FUNCTION ABETA_HF_DIRI79(t_k, s, p_bar)
!=======================================================================

! Function calculates association constant \beta_{HF} [(mol/kg-SW)^{-1}]
! in (mol/kg-SW)^{-1}, where
!   \beta_{HF} = \frac{ [HF] }{ [H^{+}] [F^{-}] }

! References: Dickson and Riley (1979)
!             Millero (1995) for pressure correction
! pH scale  : free
! Note      : converted here from mol/kg-H2O to mol/kg-SW

IMPLICIT NONE

REAL(KIND=wp) :: ABETA_HF_DIRI79


! ------------------
! Argument variables
! ------------------

!     t_k    : temperature in K
!     s      : salinity
!     p_bar  : applied pressure in bar

REAL(KIND=wp), INTENT(IN) ::  t_k, s, p_bar


! ---------------
! Local variables
! ---------------

!     zrt            : R*t_k, R in bar*cm3/(mol*K)
!     zt_degc        : temperature in degrees Celsius
!     zdvi           : volume change for ionization
!     zdki           : compressibility change for ionization
!     zionst         : ionic strength [mol/kg-H2O]
!     zcvt_to_kgsw   : mass of pure water in 1kg of seawater as a fct. of salinity
!     zln_bhf_p0     : \beta_HF at p_bar = 0
!     zln_khf_pp     : pressure correction for k_HF = 1/\beta_HF at p_bar /= 0


REAL(KIND=wp) :: zrt, zt_degc, zdvi, zdki, zds, zsqrts
REAL(KIND=wp) :: zionst, zcvt_to_kgsw
REAL(KIND=wp) :: zln_bhf_p0, zln_khf_pp


! \beta_HF at p_bar = 0
! ---------------------

zcvt_to_kgsw    = ACVT_KGH2O_O_KGSW(s)
zionst          = A_IONSTRENGTH_SALIN(s)/zcvt_to_kgsw

zln_bhf_p0      = -1590.2_wp/t_k + 12.641_wp - 1.525_wp*SQRT(zionst)


! Pressure correction
! -------------------

zt_degc      = t_k - t_k_zerodegc
zrt          = gasconst_bar_cm3_o_mol_k * t_k

zdvi         =   -9.78_wp + zt_degc*(-0.0090_wp - zt_degc*0.942E-03_wp)
zdki         = ( -3.91_wp + zt_degc*0.054_wp)*1.0E-03_wp

zln_khf_pp   = (-zdvi + zdki*p_bar/2._wp)*p_bar/zrt


! Final \beta_HF value
! --------------------
!  notice that  ln(k_HF(P)) = ln(k_HF(0)) + zln_khf_pp
!         <=>  -ln(\beta_HF(P)) = -ln(\beta_HF(0)) + zln_khf_pp
!         <=>   ln(\beta_HF(P)) =  ln(\beta_HF(0)) - zln_khf_pp

ABETA_HF_DIRI79 = EXP(zln_bhf_p0 - zln_khf_pp ) / zcvt_to_kgsw

RETURN

!=======================================================================
END FUNCTION ABETA_HF_DIRI79
!=======================================================================




!=======================================================================
FUNCTION AK_HF_PEFR87(t_k, s, p_bar)
!=======================================================================

! Function calculates dissociation constant for hydrogen fluoride
! in mol/kg-SW

! References: Perez and Fraga (1987)
!             Millero (1995) for pressure correction
! pH scale  : Total (according to Handbook, 2007)

IMPLICIT NONE

REAL(KIND=wp) :: AK_HF_PEFR87


! ------------------
! Argument variables
! ------------------

!     t_k    : temperature in K
!     s      : salinity
!     p_bar  : applied pressure in bar

REAL(KIND=wp), INTENT(IN) ::  t_k, s, p_bar


! ---------------
! Local variables
! ---------------

!     zrt            : R*t_k, R in bar*cm3/(mol*K)
!     zt_degc        : temperature in degrees Celsius
!     zdvi           : volume change for ionization
!     zdki           : compressibility change for ionization
!     zln_khf_p0     : ln(K_HF) at p_bar = 0
!     zln_khf_pp     : pressure correction for p_bar /= 0

REAL(KIND=wp) :: zrt, zt_degc, zdvi, zdki, zds, zsqrts
REAL(KIND=wp) :: zln_khf_p0, zln_khf_pp


! ln(K_HF) at p_bar = 0

zln_khf_p0   = 874._wp/t_k - 9.68_wp + 0.111_wp*SQRT(s)


! Pressure correction

zt_degc      = t_k - t_k_zerodegc
zrt          = gasconst_bar_cm3_o_mol_k * t_k

zdvi         =   -9.78_wp + zt_degc*(-0.0090_wp - zt_degc*0.942E-03_wp)
zdki         = ( -3.91_wp + zt_degc*0.054_wp)*1.0E-03_wp

zln_khf_pp   = (-zdvi + zdki*p_bar/2._wp)*p_bar/zrt


! Final value of K_HF

AK_HF_PEFR87 = EXP( zln_khf_p0 + zln_khf_pp )

RETURN

!=======================================================================
END FUNCTION AK_HF_PEFR87
!=======================================================================




!=======================================================================
FUNCTION AK_HSO4_DICK90(t_k, s, p_bar)
!=======================================================================

! Function returns the dissociation constant of hydrogen sulfate (bisulfate)

! References: Dickson (1990) -- also Handbook (2007)
!             Millero (1995) for pressure correction
! pH scale  : free
! Note      : converted here from mol/kg-H2O to mol/kg-SW

IMPLICIT NONE

REAL(KIND=wp) :: AK_HSO4_DICK90


! ------------------
! Argument variables
! ------------------

!     t_k    : temperature in K
!     s      : salinity
!     p_bar  : applied pressure in bar

REAL(KIND=wp), INTENT(IN) ::  t_k, s, p_bar


! ---------------
! Local variables
! ---------------

!     zrt            : R*t_k, R in bar*cm3/(mol*K)
!     zt_degc        : temperature in degrees Celsius
!     zdvi           : volume change for ionization
!     zdki           : compressibility change for ionization
!     zionst         : ionic strength in mol/-kg-H2O
!     zsqrti         : square root og ion strength
!     zcvt_to_kgsw   : mass of pure water in 1kg of seawater as a fct. of salinity
!     zln_khso4_p0   : K_HSO4 at p_bar = 0
!     zln_khso4_pp   : pressure correction for p_bar /= 0

REAL(KIND=wp) :: zrt, zt_degc, zdvi, zdki
REAL(KIND=wp) :: zcvt_to_kgsw, zionst, zsqrti
REAL(KIND=wp) :: zln_khso4_p0, zln_khso4_pp


! ln(K_HSO4) at p_bar = 0

zcvt_to_kgsw = ACVT_KGH2O_O_KGSW(s)
zionst       = A_IONSTRENGTH_SALIN(s)/zcvt_to_kgsw
zsqrti       = SQRT(zionst)

zln_khso4_p0 =     -4276.1_wp/t_k + 141.328_wp -  23.093_wp*LOG(t_k)           &
               + (-13856._wp/t_k  +  324.57_wp -  47.986_wp*LOG(t_k)) * zsqrti &
               + ( 35474._wp/t_k  -  771.54_wp + 114.723_wp*LOG(t_k)) * zionst &
               - (  2698._wp/t_k)*zsqrti * zionst                              &
               + (  1776._wp/t_k)*zionst*zionst


! Pressure correction

zt_degc      = t_k - t_k_zerodegc
zrt          = gasconst_bar_cm3_o_mol_k * t_k

zdvi         =  -18.03_wp + zt_degc*(0.0466_wp + zt_degc*0.316E-03_wp)
zdki         = ( -4.53_wp + zt_degc*0.0900_wp)*1.0E-03_wp

zln_khso4_pp = (-zdvi + zdki*p_bar/2._wp)*p_bar/zrt


! ln(K_HSO4) at p_bar = 0

AK_HSO4_DICK90 = zcvt_to_kgsw * EXP( zln_khso4_p0 + zln_khso4_pp )

RETURN

!=======================================================================
END FUNCTION AK_HSO4_DICK90
!=======================================================================




!=======================================================================
FUNCTION ASP_CALC_MUCC83(t_k, s, p_bar)
!=======================================================================

! Function returns stoechiometric solubility product
! of calcite in seawater

! References: Mucci (1983)
!             Millero (1995) for pressure correction
! pH scale  : N/A
! Units     : (mol/kg-SW)^2


IMPLICIT NONE

REAL(KIND=wp) :: ASP_CALC_MUCC83

! ------------------
! Argument variables
! ------------------

!     t_k    : temperature in K
!     s      : salinity
!     p_bar  : applied pressure in bar

REAL(KIND=wp), INTENT(IN) ::  t_k, s, p_bar

! ---------------
! Local variables
! ---------------

!     zrt          : R*t_k, R in bar*cm3/(mol*K)
!     zsqrts       : square root of salinity
!     zt_degc      : temperature in degrees Celsius
!     zdvi         : volume change for ionization
!     zdki         : compressibility change for ionization
!     zln_kp1_p0   : ln(K_p1) at p_bar = 0
!     zln_kp1_pp   : pressure correction for p_bar /= 0

REAL(KIND=wp) :: zrt, zsqrts, zt_degc, zdvi, zdki
REAL(KIND=wp) :: zlog10_kspcalc_p0, zln_kspcalc_pp


zsqrts    = SQRT(s)

! log10(Ksp_Calc) for p_bar = 0
zlog10_kspcalc_p0 = &
             -171.9065_wp - 0.077993_wp*t_k                           &
            + 2839.319_wp/t_k + 71.595_wp*LOG10(t_k)                  &
            + ( -0.77712_wp + 0.0028426*t_k + 178.34_wp/t_k)*zsqrts   &
            - 0.07711_wp*s + 0.0041249_wp*s*zsqrts


! Pressure correction
zt_degc   = t_k - t_k_zerodegc
zrt       = gasconst_bar_cm3_o_mol_k * t_k

zdvi      =  -48.76_wp + 0.5304_wp*zt_degc
zdki      = (-11.76_wp + 0.3692_wp*zt_degc)*1.0E-03_wp

zln_kspcalc_pp = (-zdvi + zdki*p_bar/2._wp)*p_bar/zrt


! Final value of Ksp_Calc

ASP_CALC_MUCC83 = 10._wp**(zlog10_kspcalc_p0) * EXP(zln_kspcalc_pp)

RETURN

!=======================================================================
 END FUNCTION ASP_CALC_MUCC83
!=======================================================================




!=======================================================================
FUNCTION ASP_ARAG_MUCC83(t_k, s, p_bar)
!=======================================================================

! Function returns stoechiometric solubility product
! of aragonite in seawater

! References: Mucci (1983)
!             Millero (1979) for pressure correction
! pH scale  : N/A
! Units     : (mol/kg-SW)^2


IMPLICIT NONE

REAL(KIND=wp) :: ASP_ARAG_MUCC83

! ------------------
! Argument variables
! ------------------

!     t_k    : temperature in K
!     s      : salinity
!     p_bar  : applied pressure in bar

REAL(KIND=wp), INTENT(IN) ::  t_k, s, p_bar

! ---------------
! Local variables
! ---------------

!     zrt          : R*t_k, R in bar*cm3/(mol*K)
!     zsqrts       : square root of salinity
!     zt_degc      : temperature in degrees Celsius
!     zdvi         : volume change for ionization
!     zdki         : compressibility change for ionization
!     zln_kp1_p0   : ln(K_p1) at p_bar = 0
!     zln_kp1_pp   : pressure correction for p_bar /= 0

REAL(KIND=wp) :: zrt, zsqrts, zt_degc, zdvi, zdki
REAL(KIND=wp) :: zlog10_ksparag_p0, zln_ksparag_pp


zsqrts    = SQRT(s)

! log10(Ksp_Arag) for p_bar = 0
zlog10_ksparag_p0 = &
             -171.945_wp - 0.077993_wp*t_k                                     &
            + 2903.293_wp/t_k + 71.595_wp*LOG10(t_k)                            &
            + ( -0.068393_wp + 0.0017276_wp*t_k + 88.135_wp/t_k)*zsqrts        &
            - 0.10018_wp*s + 0.0059415_wp*s*zsqrts


! Pressure correction
zt_degc   = t_k - t_k_zerodegc
zrt       = gasconst_bar_cm3_o_mol_k * t_k

zdvi      =  -48.76_wp + 0.5304_wp*zt_degc  + 2.8_wp
zdki      = (-11.76_wp + 0.3692_wp*zt_degc)*1.0E-03_wp

zln_ksparag_pp = (-zdvi + zdki*p_bar/2._wp)*p_bar/zrt


! Final value of Ksp_Arag

ASP_ARAG_MUCC83 = 10._wp**(zlog10_ksparag_p0) * EXP(zln_ksparag_pp)

RETURN

!=======================================================================
 END FUNCTION ASP_ARAG_MUCC83
!=======================================================================







!=======================================================================
FUNCTION A_BTOT_SALIN(s)
!=======================================================================

! Function returns total borate concentration in mol/kg-SW
! given the salinity of a sample

! References: Uppström (1974), cited by  Dickson et al. (2007, chapter 5, p 10)
!             Millero (1982) cited in Millero (1995)
! pH scale  : N/A


IMPLICIT NONE

REAL(KIND=wp) :: A_BTOT_SALIN


! ------------------
! Argument variables
! ------------------

REAL(KIND=wp), INTENT(IN) :: s


A_BTOT_SALIN = 0.000416_wp*(s/35._wp)

RETURN

!=======================================================================
END FUNCTION A_BTOT_SALIN
!=======================================================================



!=======================================================================
FUNCTION A_CATOT_SALIN(s)
!=======================================================================

! Function returns total calcium concentration in mol/kg-SW
! given the salinity of a sample

! References: Culkin (1965)
! pH scale  : N/A


IMPLICIT NONE

REAL(KIND=wp) :: A_CATOT_SALIN


! ------------------
! Argument variables
! ------------------

REAL(KIND=wp), INTENT(IN) :: s


A_CATOT_SALIN = 0.010282_wp*(s/35._wp)


RETURN

!=======================================================================
END FUNCTION A_CATOT_SALIN
!=======================================================================



!=======================================================================
FUNCTION A_FTOT_SALIN(s)
!=======================================================================

! Function returns total calcium concentration in mol/kg-SW
! given the salinity of a sample

! References: Culkin (1965) (???)
! pH scale  : N/A


IMPLICIT NONE

REAL(KIND=wp) :: A_FTOT_SALIN


! ------------------
! Argument variables
! ------------------

REAL(KIND=wp), INTENT(IN) :: s


A_FTOT_SALIN = 0.000068_wp*(s/35._wp)


RETURN

!=======================================================================
END FUNCTION A_FTOT_SALIN
!=======================================================================



!=======================================================================
FUNCTION A_SO4TOT_SALIN(s)
!=======================================================================

! Function returns total sulfate concentration in mol/kg-SW
! given the salinity of a sample

! References: Morris, A.W. and Riley, J.P. (1966) quoted in Handbook (2007)
! pH scale  : N/A


IMPLICIT NONE

REAL(KIND=wp) :: A_SO4TOT_SALIN


! ------------------
! Argument variables
! ------------------

REAL(KIND=wp), INTENT(IN) :: s


!A_SO4TOT_SALIN = 0.028234_wp*(s/35._wp) ! in libthdyct and Thesis
!A_SO4TOT_SALIN = 0.02824_wp*(s/35._wp)                ! Handbook (2007, chap 6, p 10, tab 2, col 3)
A_SO4TOT_SALIN = (0.1400_wp/96.062_wp)*(s/1.80655_wp)  ! Handbook (2007, chap 6, p 10)

RETURN

!=======================================================================
END FUNCTION A_SO4TOT_SALIN
!=======================================================================




!=======================================================================
 FUNCTION ACVT_HSWS_O_HTOT(t_k, s, p_bar)
!=======================================================================

! Function returns the ratio H_SWS/H_Tot as a function of salinity s

! Reference:  Munhoven
! pH scale:   all


IMPLICIT NONE

REAL(KIND=wp) :: ACVT_HSWS_O_HTOT


! ------------------
! Argument variables
! ------------------

!     t_k    : temperature in K
!     s      : salinity
!     p_bar  : applied pressure in bar

REAL(KIND=wp), INTENT(IN) ::  t_k
REAL(KIND=wp), INTENT(IN) ::  s
REAL(KIND=wp), INTENT(IN) ::  p_bar


! ---------------
! Local variables
! ---------------

!     zso4_tot: total sulfate concentration in mol/kg-SW
!     zf_tot  : total fluoride concentration in mol/kg-SW

REAL(KIND=wp) :: zso4_tot, zf_tot

!-----------------------------------------------------------------------


zso4_tot = A_SO4TOT_SALIN(s)
zf_tot   = A_FTOT_SALIN(s)


ACVT_HSWS_O_HTOT = 1._wp +  (zf_tot*ABETA_HF_DIRI79(t_k, s, p_bar)) &
                           /(1._wp + zso4_tot/AK_HSO4_DICK90(t_k,s, p_bar))

RETURN


!=======================================================================
 END FUNCTION ACVT_HSWS_O_HTOT
!=======================================================================




!=======================================================================
FUNCTION ACVT_HTOT_O_HFREE(t_k, s, p_bar)
!=======================================================================

! Function returns the ratio H_Tot/H_free as a function of salinity s

! Reference:  Munhoven
! pH scale:   N/A


IMPLICIT NONE

REAL(KIND=wp) :: ACVT_HTOT_O_HFREE


! ------------------
! Argument variables
! ------------------

!     t_k    : temperature in K
!     s      : salinity
!     p_bar  : applied pressure in bar

REAL(KIND=wp), INTENT(IN) ::  t_k
REAL(KIND=wp), INTENT(IN) ::  s
REAL(KIND=wp), INTENT(IN) ::  p_bar


! ---------------
! Local variables
! ---------------

!     zso4_tot: total sulfate concentration in mol/kg-SW

REAL(KIND=wp) :: zso4_tot

!-----------------------------------------------------------------------


zso4_tot = A_SO4TOT_SALIN(s)


ACVT_HTOT_O_HFREE = 1._wp + zso4_tot/AK_HSO4_DICK90(t_k,s, p_bar)

RETURN


!=======================================================================
 END FUNCTION ACVT_HTOT_O_HFREE
!=======================================================================




!=======================================================================
FUNCTION ACVT_HSWS_O_HFREE(t_k, s, p_bar)
!=======================================================================

! Function returns the ratio H_SWS/H_free as a function
! of salinity s

! Reference:  Munhoven
! pH scale:   N/A


IMPLICIT NONE

REAL(KIND=wp) :: ACVT_HSWS_O_HFREE


! ------------------
! Argument variables
! ------------------

!     t_k    : temperature in K
!     s      : salinity
!     p_bar  : applied pressure in bar

REAL(KIND=wp), INTENT(IN) ::  t_k
REAL(KIND=wp), INTENT(IN) ::  s
REAL(KIND=wp), INTENT(IN) ::  p_bar


! ---------------
! Local variables
! ---------------

!     zso4_tot: total sulfate concentration in mol/kg-SW
!     zf_tot  : total fluoride concentration in mol/kg-SW

REAL(KIND=wp) :: zso4_tot, zf_tot

!-----------------------------------------------------------------------


zso4_tot = A_SO4TOT_SALIN(s)
zf_tot   = A_FTOT_SALIN(s)


ACVT_HSWS_O_HFREE = 1._wp + zf_tot*ABETA_HF_DIRI79(t_k, s, p_bar)              &
                          + zso4_tot/AK_HSO4_DICK90(t_k,s, p_bar)

RETURN


!=======================================================================
 END FUNCTION ACVT_HSWS_O_HFREE
!=======================================================================



!=======================================================================
 FUNCTION A_RHOSW1_MUNH97(t_k, s, p_bar)
!=======================================================================

! Function returns first order approximation of \rho in (kg-SW)/(m^3-SW)

! References: Munhoven (1997)
!             after EOS80 (UNESCO, 1981, 1983)

IMPLICIT NONE

REAL(KIND=wp) :: A_RHOSW1_MUNH97

! ------------------
! Argument variables
! ------------------

!     s      : salinity
!     tk     : temperature in K
!     p_bar  : depth in m

REAL(KIND=wp), INTENT(IN) ::  t_k, s, p_bar


! ---------------
! Local variables
! ---------------

!     s0     : 35.5
!     t_k0   : 285.16 K
!     p_bar0 : 300 bar

REAL(KIND=wp), PARAMETER ::     s0 =  35.5_wp
REAL(KIND=wp), PARAMETER ::   t_k0 = 285.16_wp
REAL(KIND=wp), PARAMETER :: p_bar0 = 300.0_wp


A_RHOSW1_MUNH97 = 1039.9044_wp + 0.77629393_wp*(s-s0)                  &
                               - 0.19692738_wp*(t_k-t_k0)                &
                               + 0.044038615_wp*(p_bar-p_bar0)

RETURN

!=======================================================================
 END FUNCTION A_RHOSW1_MUNH97
!=======================================================================



!=======================================================================
 FUNCTION A_RHOSW2_MUNH97(t_k, s, p_bar)
!=======================================================================

! Function returns first order approximation of \rho in (kg-SW)/(m^3-SW)

! References: Munhoven (1997)
!             after EOS80 (UNESCO, 1981, 1983)

IMPLICIT NONE

REAL(KIND=wp) :: A_RHOSW2_MUNH97

! ------------------
! Argument variables
! ------------------

!     s      : salinity
!     tk     : temperature in K
!     p_bar  : depth in m

REAL(KIND=wp), INTENT(IN) ::  t_k, s, p_bar


! ---------------
! Local variables
! ---------------

!     s0     : 35.5
!     t_k0   : 285.16 K
!     p_bar0 : 300 bar

REAL(KIND=wp), PARAMETER ::     s0 =  35.5_wp
REAL(KIND=wp), PARAMETER ::   t_k0 = 285.16_wp
REAL(KIND=wp), PARAMETER :: p_bar0 = 300.0_wp


A_RHOSW2_MUNH97 = 1040.0145_wp                                         &
                   + 0.77629393_wp*(s-s0)                              &
                   - 0.25013591_wp*(t_k-t_k0)                          &
                   + 4.2026266E-02_wp*(p_bar-p_bar0)                   &
                   - 4.7473116E-03_wp*(t_k-t_k0)*(t_k-t_k0)            &
                   - 4.7974224E-06_wp*(p_bar-p_bar0)*(p_bar-p_bar0)    &
                   - 2.1404592E-04_wp*(t_k-t_k0)*(p_bar-p_bar0)

RETURN

!=======================================================================
 END FUNCTION A_RHOSW2_MUNH97
!=======================================================================




!=======================================================================
 SUBROUTINE CHECKCONSTANTS
!=======================================================================

IMPLICIT NONE

! ------------------
! Argument variables
! ------------------

! N/A


! ---------------
! Local variables
! ---------------

!     s      : salinity
!     tk     : temperature in K
!     p_bar  : applied pressure in bar

REAL(KIND=wp) :: t_k, s, p_bar

REAL(KIND=wp) :: zkc0, zkc1, zkc2
REAL(KIND=wp) :: zkb
REAL(KIND=wp) :: zkhf
REAL(KIND=wp) :: zkhso4
REAL(KIND=wp) :: zkp1, zkp2, zkp3
REAL(KIND=wp) :: zksi1
REAL(KIND=wp) :: zkw
REAL(KIND=wp) :: zknh4
REAL(KIND=wp) :: zkh2s

INTEGER :: logunit


OPEN(newunit=logunit,FILE='checkconst.log')

WRITE(logunit,*) 'Checking constant values generated from MOD_CHEMCONST'
WRITE(logunit,*)
WRITE(logunit,*) ' % indicates checking against the Handbook (1994);'
WRITE(logunit,*) ' $ indicates checking against the Lewis and Wallace (1998);'
WRITE(logunit,*) ' * indicates checking against the Handbook (2007);'
WRITE(logunit,*) '   target values are quoted in brackets'
WRITE(logunit,*)
WRITE(logunit,*)
WRITE(logunit,*) ' For S = 35, P = 0 and T/K = 298.15:'
WRITE(logunit,*)


s     = 35._wp
p_bar = 0._wp
t_k   = 298.15_wp


zkc0 = AK_CARB_0_WEIS74(t_k, s)
WRITE(logunit,*)
WRITE(logunit,*) 'K_0 -- Weiss (1974)'
WRITE(logunit,*) '==================='
WRITE(logunit,*)
WRITE(logunit,*) '   K_0            :', zkc0
WRITE(logunit,*) '   ln(K_0)        :', LOG(zkc0)
WRITE(logunit,*) '   pK_0           :', -LOG10(zkc0)
WRITE(logunit,'("  * ln(K_0)        :", F8.4, " (-3.5617)")') LOG(zkc0)
WRITE(logunit,*)


zkhso4 = AK_HSO4_DICK90(t_k, s, p_bar)

WRITE(logunit,*)
WRITE(logunit,*) 'K_HSO4 -- Dickson (1990) -- pH_free'
WRITE(logunit,*) '==================================='
WRITE(logunit,*)
WRITE(logunit,*) '   K_HSO4         :', zkhso4
WRITE(logunit,*) '   ln(K_HSO4)     :', LOG(zkhso4)
WRITE(logunit,*) '   pK_HSO4        :', -LOG10(zkhso4)
WRITE(logunit,'("  * ln(K_HSO4)     :", F6.2, " (-2.30)")') LOG(zkhso4)
WRITE(logunit,*)


zkb = AK_BORA_DICK90(t_k, s, p_bar)

WRITE(logunit,*)
WRITE(logunit,*) 'K_b -- Dickson (1990) -- pH_tot'
WRITE(logunit,*) '==============================='
WRITE(logunit,*)
WRITE(logunit,*) '   K_b            :', zkb
WRITE(logunit,*) '   ln(K_b)        :', LOG(zkb)
WRITE(logunit,*) '   pK_b           :', -LOG10(zkb)
WRITE(logunit,'("  * ln(K_b)        :", F9.4, " (-19.7964)")') LOG(zkb)
WRITE(logunit,*)




zkc1 = AK_CARB_1_LUEK00(t_k, s, p_bar)

WRITE(logunit,*)
WRITE(logunit,*) 'K_1 -- Luecker et al (2000) -- pH_tot'
WRITE(logunit,*) '====================================='
WRITE(logunit,*)
WRITE(logunit,*) '   K_1            :', zkc1
WRITE(logunit,*) '   ln(K_1)        :', LOG(zkc1)
WRITE(logunit,*) '   pK_1           :', -LOG10(zkc1)
WRITE(logunit,'("  * log10(K_1)     :", F8.4, " (-5.8472)")') LOG10(zkc1)
WRITE(logunit,*)


zkc2 = AK_CARB_2_LUEK00(t_k, s, p_bar)

WRITE(logunit,*)
WRITE(logunit,*) 'K_2 -- Luecker et al (2000) -- pH_tot'
WRITE(logunit,*) '====================================='
WRITE(logunit,*)
WRITE(logunit,*) '   K_2            :', zkc2
WRITE(logunit,*) '   ln(K_2)        :', LOG(zkc2)
WRITE(logunit,*) '   pK_2           :', -LOG10(zkc2)
WRITE(logunit,'("  * log10(K_2)     :", F8.4, " (-8.9660)")') LOG10(zkc2)
WRITE(logunit,*)




zkc1 = AK_CARB_1_ROYE93(t_k, s, p_bar)

WRITE(logunit,*)
WRITE(logunit,*) 'K_1 -- Roy et al (1993) -- pH_tot'
WRITE(logunit,*) '================================='
WRITE(logunit,*)
WRITE(logunit,*) '   K_1            :', zkc1
WRITE(logunit,*) '   ln(K_1)        :', LOG(zkc1)
WRITE(logunit,*) '   pK_1           :', -LOG10(zkc1)
WRITE(logunit,'("  % ln(K_1)        :", F9.4, " (-13.4847)")') LOG(zkc1)
WRITE(logunit,*)


zkc2 = AK_CARB_2_ROYE93(t_k, s, p_bar)

WRITE(logunit,*)
WRITE(logunit,*) 'K_2 -- Roy et al (1993) -- pH_tot'
WRITE(logunit,*) '================================='
WRITE(logunit,*)
WRITE(logunit,*) '   K_2            :', zkc2
WRITE(logunit,*) '   ln(K_2)        :', LOG(zkc2)
WRITE(logunit,*) '   pK_2           :', -LOG10(zkc2)
WRITE(logunit,'("  % ln(K_2)        :", F9.4, " (-20.5504)")') LOG(zkc2)
WRITE(logunit,*)




zkhf = AK_HF_PEFR87(t_k, s, p_bar)

WRITE(logunit,*)
WRITE(logunit,*) 'K_HF -- Perez and Fraga (1987) -- pH_tot'
WRITE(logunit,*) '========================================'
WRITE(logunit,*)
WRITE(logunit,*) '   K_HF           :', zkhf
WRITE(logunit,*) '   ln(K_HF)       :', LOG(zkhf)
WRITE(logunit,*) '   pK_HF          :', -LOG10(zkhf)
WRITE(logunit,'("  * ln(K_HF)       :", F6.2, " (-6.09)")') LOG(zkhf)
WRITE(logunit,*)


zkp1 = AK_PHOS_1_MILL95(t_k, s, p_bar)

WRITE(logunit,*)
WRITE(logunit,*) 'K_P1 -- Millero (1995) -- pH_SWS'
WRITE(logunit,*) '================================'
WRITE(logunit,*)
WRITE(logunit,*) '   K_P1           :', zkp1
WRITE(logunit,*) '   ln(K_P1)       :', LOG(zkp1)
WRITE(logunit,*) '   pK_1           :', -LOG10(zkp1)
WRITE(logunit,'("  * ln(K_P1)-0.015 :", F6.2, " (-3.71)")') LOG(zkp1)-0.015_wp
WRITE(logunit,*)


zkp2 = AK_PHOS_2_MILL95(t_k, s, p_bar)

WRITE(logunit,*)
WRITE(logunit,*) 'K_P2 -- Millero (1995) -- pH_SWS'
WRITE(logunit,*) '================================'
WRITE(logunit,*)
WRITE(logunit,*) '   K_2            :', zkp2
WRITE(logunit,*) '   ln(K_P2)       :', LOG(zkp2)
WRITE(logunit,*) '   pK_2           :', -LOG10(zkp2)
WRITE(logunit,'("  * ln(K_P2)-0.015 :", F8.3, " (-13.727)")') LOG(zkp2)-0.015_wp
WRITE(logunit,*)


zkp3 = AK_PHOS_3_MILL95(t_k, s, p_bar)

WRITE(logunit,*)
WRITE(logunit,*) 'K_P3 -- Millero (1995) -- pH_SWS'
WRITE(logunit,*) '================================'
WRITE(logunit,*)
WRITE(logunit,*) '   K_P3           :', zkp3
WRITE(logunit,*) '   ln(K_P3)       :', LOG(zkp3)
WRITE(logunit,*) '   pK_P3          :', -LOG10(zkp3)
WRITE(logunit,'("  * ln(K_P3)-0.015 :", F7.2, " (-20.24)")') LOG(zkp3)-0.015_wp
WRITE(logunit,*)


zksi1 = AK_SILI_1_MILL95(t_k, s)

WRITE(logunit,*)
WRITE(logunit,*) 'K_Si1 -- Millero (1995) -- pH_SWS'
WRITE(logunit,*) '================================='
WRITE(logunit,*)
WRITE(logunit,*) '   K_Si1          :', zksi1
WRITE(logunit,*) '   ln(K_Si1)      :', LOG(zksi1)
WRITE(logunit,*) '   pK_Si1         :', -LOG10(zksi1)
WRITE(logunit,'("  * ln(K_Si1)-0.015:", F7.2, " (-21.61)")') LOG(zksi1)-0.015_wp
WRITE(logunit,*)


zkw = AK_W_MILL95(t_k, s, p_bar)

WRITE(logunit,*)
WRITE(logunit,*) 'K_w -- Millero (1995) -- pH_SWS'
WRITE(logunit,*) '==============================='
WRITE(logunit,*)
WRITE(logunit,*) '   K_w            :', zkw
WRITE(logunit,*) '   ln(K_w)        :', LOG(zkw)
WRITE(logunit,*) '   pK_w           :', -LOG10(zkw)
WRITE(logunit,'("  * ln(K_w)-0.015  :", F8.3, " (-30.434)")') LOG(zkw)-0.015_wp
WRITE(logunit,*)


zkh2s = AK_H2S_1_MILL95(t_k, s, p_bar)

WRITE(logunit,*)
WRITE(logunit,*) 'K_H2S -- Millero (1995) -- pH_SWS'
WRITE(logunit,*) '================================='
WRITE(logunit,*)
WRITE(logunit,*) '   K_H2S          :', zkh2s
WRITE(logunit,*) '   ln(K_H2S)      :', LOG(zkh2s)
WRITE(logunit,*) '   pK_H2S         :', -LOG10(zkh2s)
WRITE(logunit,'("  $ pK_H2S         :", F5.2, " (6.51)")') -LOG10(zkh2s)
WRITE(logunit,*)


zknh4 = AK_AMMO_1_YAMI95(t_k, s, p_bar)

WRITE(logunit,*)
WRITE(logunit,*) 'K_NH4 -- Yao and Millero (1995) -- pH_SWS'
WRITE(logunit,*) '========================================='
WRITE(logunit,*)
WRITE(logunit,*) '   K_NH4          :', zknh4
WRITE(logunit,*) '   ln(K_NH4)      :', LOG(zknh4)
WRITE(logunit,*) '   pK_NH4         :', -LOG10(zknh4)
WRITE(logunit,'("  $ pK_NH4         :", F5.2, " (9.26)")') -LOG10(zknh4)
WRITE(logunit,*)



CLOSE(logunit)

RETURN

!=======================================================================
 END SUBROUTINE CHECKCONSTANTS
!=======================================================================

END MODULE MOD_CHEMCONST
