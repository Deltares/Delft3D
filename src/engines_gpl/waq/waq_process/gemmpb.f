!!  Copyright(C) Stichting Deltares, 2012-2023.
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
      module m_gemmpb

      implicit none

      contains


      SUBROUTINE GEMMPB ( PMSA   , FL     , IPOINT , INCREM , NOSEG  ,
     +                    NOFLUX , IEXPNT , IKNMRK , NOQ1   , NOQ2   ,
     +                    NOQ3   , NOQ4   )
C**********************************************************************
C     +----------------------------------------+
C     |    D E L F T   H Y D R A U L I C S     |
C     +----------------------------------------+
C***********************************************************************
C
C     Description of the module :
C
C       GEM microphytobenthos
C
C     Project      : Implementatie pilot GEM (T2087)
C     Formulations : NIOO-CEMO Yerseke
C     Programmer   : M. Bokhorst
C     Date         : 10-04-97           Version : 1.0
C
C     History :
C
C     Date    Programmer      Description
C     ------  --------------  ------------------------------------------
C     100497  M. Bokhorst     First version
C     050399  J.vGils         Update for optional S1 mode
C                             Add treshold value
C                             Explicit declarations
C     150399  A. Blauw        MPB can't consume more than the min.flux
C     311003  Jan van Beek    process two types at once and a lot more
C***********************************************************************

      use m_evaluate_waq_attribute
      use m_write_error_message

      IMPLICIT NONE

C     arguments

      REAL     PMSA  ( * ) , FL    (*)
      INTEGER  IPOINT( * ) , INCREM(*) , NOSEG , NOFLUX,
     +         IEXPNT(4,*) , IKNMRK(*) , NOQ1, NOQ2, NOQ3, NOQ4

C     from PMSA array

      REAL               :: TEMP               ! 1  in
      REAL               :: BIOMAS_MPB1        ! 2  in
      REAL               :: BIOMAS_MPB2        ! 3  in
      REAL               :: BIOMAS_S1_MPB1     ! 4  in
      REAL               :: BIOMAS_S1_MPB2     ! 5  in
      REAL               :: PMCH20_MPB1        ! 6  in
      REAL               :: PMCH20_MPB2        ! 7  in
      REAL               :: FLT_MPB1           ! 8  in
      REAL               :: FLT_MPB2           ! 9  in
      REAL               :: FTMP_MPB1          ! 10 in
      REAL               :: FTMP_MPB2          ! 11 in
      REAL               :: FNUT_MPB1          ! 12 in
      REAL               :: FNUT_MPB2          ! 13 in
      REAL               :: R_PR_MPB1          ! 14 in
      REAL               :: R_PR_MPB2          ! 15 in
      REAL               :: R_MT20_MPB1        ! 16 in
      REAL               :: R_MT20_MPB2        ! 17 in
      REAL               :: RT_MPB1            ! 18 in
      REAL               :: RT_MPB2            ! 19 in
      REAL               :: B_EX_MPB1          ! 20 in
      REAL               :: B_EX_MPB2          ! 21 in
      REAL               :: M1_20_MPB1         ! 22 in
      REAL               :: M1_20_MPB2         ! 23 in
      REAL               :: M2_20_MPB1         ! 24 in
      REAL               :: M2_20_MPB2         ! 25 in
      REAL               :: MT_MPB1            ! 26 in
      REAL               :: MT_MPB2            ! 27 in
      REAL               :: NCRAT_MPB1         ! 28 in
      REAL               :: NCRAT_MPB2         ! 29 in
      REAL               :: PCRAT_MPB1         ! 30 in
      REAL               :: PCRAT_MPB2         ! 31 in
      REAL               :: SCRAT_MPB1         ! 32 in
      REAL               :: SCRAT_MPB2         ! 33 in
      REAL               :: FAM_MPB1           ! 34 in
      REAL               :: FAM_MPB2           ! 35 in
      REAL               :: FNI_MPB1           ! 36 in
      REAL               :: FNI_MPB2           ! 37 in
      REAL               :: TRESH_MPB1         ! 38 in
      REAL               :: TRESH_MPB2         ! 39 in
      LOGICAL            :: S1_BOTTOM          ! 40 in  , switch for S1 bottom approach (.true.) or DELWAQ-G approach (.false.)
      REAL               :: FLT_S1_MPB1        ! 41 in
      REAL               :: FLT_S1_MPB2        ! 42 in
      REAL               :: FTMP_S1_MPB1       ! 43 in
      REAL               :: FTMP_S1_MPB2       ! 44 in
      REAL               :: FNUT_S1_MPB1       ! 45 in
      REAL               :: FNUT_S1_MPB2       ! 46 in
      REAL               :: FAM_S1_MPB1        ! 47 in
      REAL               :: FAM_S1_MPB2        ! 48 in
      REAL               :: FNI_S1_MPB1        ! 49 in
      REAL               :: FNI_S1_MPB2        ! 50 in
      REAL               :: NH4                ! 51 in
      REAL               :: NO3                ! 52 in
      REAL               :: PO4                ! 53 in
      REAL               :: SI                 ! 54 in
      REAL               :: ZSED               ! 55 in
      REAL               :: SURF               ! 56 in
      REAL               :: DEPTH              ! 57 in
      REAL               :: DELT               ! 58 in
      REAL               :: dBotN              ! 59 in
      REAL               :: dSWN               ! 60 in
      REAL               :: dGSNH              ! 61 in
      REAL               :: dGSNO              ! 62 in
      REAL               :: dBotP              ! 63 in
      REAL               :: dSWP               ! 64 in
      REAL               :: dGSP               ! 65 in
      REAL               :: dBotSi             ! 66 in
      REAL               :: dSWSi              ! 67 in
      REAL               :: CCAP_MPB1          ! 68 in   carrying capacity MPB1                     (gC/m2)
      REAL               :: CCAP_MPB2          ! 69 in   carrying capacity MPB2                     (gC/m2)
      REAL               :: LOCSEDDEPT         ! 70 in   Sediment layer depth to bottom of segment      (m)
      REAL               :: OXY                ! 71 in   Dissolved Oxygen                            (g/m3)
      REAL               :: MPBOXYCRIT         ! 72 in   Crit. oxygen conc. for growth and resp. MPB (g/m3)
      REAL               :: MPB1MO_20          ! 73 in   MPB1peli mortality at 20°C under Oxygen depl.(1/d)
      REAL               :: MPB2MO_20          ! 74 in   MPB2psam mortality at 20°C under Oxygen depl.(1/d)
      REAL               :: BIOMAS_MPB1_M2     ! 75 out, MPB1peli biomass per m3 in layer S1        (gC/m3)
      REAL               :: BIOMAS_MPB2_M2     ! 76 out, MPB2psam biomass per m3 in layer S1        (gC/m3)
      REAL               :: BIOMAS_S1_MPB1_M3  ! 77 out, MPB1peli biomass per m3 in layer S1        (gC/m3)
      REAL               :: BIOMAS_S1_MPB2_M3  ! 78 out, MPB2psam biomass per m3 in layer S1        (gC/m3)
      REAL               :: MPB1FMC            ! 79 out, logistic growth restaint factor MPB1           (-)
      REAL               :: MPB2FMC            ! 80 out, logistic growth restaint factor MPB2           (-)
      REAL               :: MPB1FMN            ! 81 out, correction factor insufficient nitrogen MPB1   (-)
      REAL               :: MPB2FMN            ! 82 out, correction factor insufficient nitrogen MPB2   (-)
      REAL               :: MPB1FMP            ! 83 out, correction factor insufficient phosphorus MPB1 (-)
      REAL               :: MPB2FMP            ! 84 out, correction factor insufficient phosphorus MPB2 (-)
      REAL               :: MPB1FMS            ! 85 out, correction factor insufficient silicate MPB1   (-)
      REAL               :: MPB2FMS            ! 86 out, correction factor insufficient silicate MPB2   (-)
      REAL               :: MPB1FMCS1          ! 87 out, logistic growth restaint factor MPB1 in S1     (-)
      REAL               :: MPB2FMCS1          ! 88 out, logistic growth restaint factor MPB2 in S1     (-)
      REAL               :: MPB1FMNS1          ! 89 out, corr. factor insufficient nitrogen MPB1 in S1  (-)
      REAL               :: MPB2FMNS1          ! 90 out, corr. factor insufficient nitrogen MPB2 in S1  (-)
      REAL               :: MPB1FMPS1          ! 91 out, corr. factor insufficient phosphorus MPB1 in S1(-)
      REAL               :: MPB2FMPS1          ! 92 out, corr. factor insufficient phosphorus MPB2 in S1(-)
      REAL               :: MPB1FMSS1          ! 93 out, corr. factor insufficient silicate MPB1 in S1  (-)
      REAL               :: MPB2FMSS1          ! 94 out, corr. factor insufficient silicate MPB2 in S1  (-)
      REAL               :: FMPB1NH4UP         ! 95 out, MPB1 net consumption rate for ammonium   (gN/m3/d)
      REAL               :: FMPB2NH4UP         ! 96 out, MPB2 net consumption rate for ammonium   (gN/m3/d)
      REAL               :: FMPB1NO3UP         ! 97 out, MPB1 net consumption rate for nitrate    (gN/m3/d)
      REAL               :: FMPB2NO3UP         ! 98 out, MPB2 net consumption rate for nitrate    (gN/m3/d)
      REAL               :: FMPB1PO4UP         ! 99 out, MPB1 net consumption rate for phosphate  (gP/m3/d)
      REAL               :: FMPB2PO4UP         !100 out, MPB2 net consumption rate for phosphate  (gP/m3/d)
      REAL               :: FMPB1SIUP          !101 out, MPB1 net consumption rate for silicate  (gSi/m3/d)
      REAL               :: FMPB2SIUP          !102 out, MPB2 net consumption rate for silicate  (gSi/m3/d)
      REAL               :: FMPB1EXC           !103 out, MPB1 excretion rate                      (gC/m3/d)
      REAL               :: FMPB2EXC           !104 out, MPB2 excretion rate                      (gC/m3/d)
      REAL               :: FMPB1FGP           !105 out, MPB1 gross primary production rate       (gC/m3/d)
      REAL               :: FMPB2FGP           !106 out, MPB2 gross primary production rate       (gC/m3/d)
      REAL               :: FMPB1MOR           !107 out, MPB1 total mortality rate                (gC/m3/d)
      REAL               :: FMPB2MOR           !108 out, MPB2 total mortality rate                (gC/m3/d)
      REAL               :: FMPB1POC1          !109 out, MPB1 net production rate for POC1        (gC/m3/d)
      REAL               :: FMPB2POC1          !110 out, MPB2 net production rate for POC1        (gC/m3/d)
      REAL               :: FMPB1PON1          !111 out, MPB1 net production rate for PON1        (gN/m3/d)
      REAL               :: FMPB2PON1          !112 out, MPB2 net production rate for PON1        (gN/m3/d)
      REAL               :: FMPB1POP1          !113 out, MPB1 net production rate for POP1        (gP/m3/d)
      REAL               :: FMPB2POP1          !114 out, MPB2 net production rate for POP1        (gP/m3/d)
      REAL               :: FMPB1OPAL          !115 out, MPB1 net production rate for OPAL Si    (gSi/m3/d)
      REAL               :: FMPB2OPAL          !116 out, MPB2 net production rate for OPAL Si    (gSi/m3/d)
      REAL               :: FMPB1OXY           !117 out, MPB1 net production rate for DO          (gO/m3/d)
      REAL               :: FMPB2OXY           !118 out, MPB2 net production rate for DO          (gO/m3/d)
      REAL               :: FMPB1RES           !119 out, MPB1 total respiration rate              (gC/m3/d)
      REAL               :: FMPB2RES           !120 out, MPB2 total respiration rate              (gC/m3/d)
      REAL               :: FMPB1FGPM2         !121 out, MPB1 gross primary production rate per m2(gC/m2/d)
      REAL               :: FMPB2FGPM2         !122 out, MPB2 gross primary production rate per m2(gC/m2/d)
      REAL               :: FMPB1FGPD          !123 out, MPB1 gross primary production rate per day   (1/d)
      REAL               :: FMPB2FGPD          !124 out, MPB2 gross primary production rate per day   (1/d)
      REAL               :: FMPB1NH4S1         !125 out, MPB1 net consumption rate for ammonium S1(gN/m3/d)
      REAL               :: FMPB2NH4S1         !126 out, MPB2 net consumption rate for ammonium S1(gN/m3/d)
      REAL               :: FMPB1NO3S1         !127 out, MPB1 net consumption rate for nitrate S1 (gN/m3/d)
      REAL               :: FMPB2NO3S1         !128 out, MPB2 net consumption rate for nitrate S1 (gN/m3/d)
      REAL               :: FMPB1PO4S1         !129 out, MPB1 net consumption rate for phosphateS1(gP/m3/d)
      REAL               :: FMPB2PO4S1         !130 out, MPB2 net consumption rate for phosphateS1(gP/m3/d)
      REAL               :: FMPB1SIS1          !131 out, MPB1 net consumption rate for silicateS1(gSi/m3/d)
      REAL               :: FMPB2SIS1          !132 out, MPB2 net consumption rate for silicateS1(gSi/m3/d)
      REAL               :: FMPB1EXCS1         !133 out, MPB1 excretion rate S1                   (gC/m3/d)
      REAL               :: FMPB2EXCS1         !134 out, MPB2 excretion rate S1                   (gC/m3/d)
      REAL               :: FMPB1FGPS1         !135 out, MPB1 gross primary production rate S1    (gC/m3/d)
      REAL               :: FMPB2FGPS1         !136 out, MPB2 gross primary production rate S1    (gC/m3/d)
      REAL               :: FMPB1MORS1         !137 out, MPB1 total mortality rate S1             (gC/m3/d)
      REAL               :: FMPB2MORS1         !138 out, MPB2 total mortality rate S1             (gC/m3/d)
      REAL               :: FMPB1POC1S         !139 out, MPB1 net production rate for POC1 S1     (gC/m3/d)
      REAL               :: FMPB2POC1S         !140 out, MPB2 net production rate for POC1 S1     (gC/m3/d)
      REAL               :: FMPB1PON1S         !141 out, MPB1 net production rate for PON1 S1     (gN/m3/d)
      REAL               :: FMPB2PON1S         !142 out, MPB2 net production rate for PON1 S1     (gN/m3/d)
      REAL               :: FMPB1POP1S         !143 out, MPB1 net production rate for POP1 S1     (gP/m3/d)
      REAL               :: FMPB2POP1S         !144 out, MPB2 net production rate for POP1 S1     (gP/m3/d)
      REAL               :: FMPB1OPALS         !145 out, MPB1 net production rate for OPAL Si S1 (gSi/m3/d)
      REAL               :: FMPB2OPALS         !146 out, MPB2 net production rate for OPAL Si S1 (gSi/m3/d)
      REAL               :: FMPB1OXYS1         !147 out, MPB1 net production rate for DO S1       (gO/m3/d)
      REAL               :: FMPB2OXYS1         !148 out, MPB2 net production rate for DO S1       (gO/m3/d)
      REAL               :: FMPB1RESS1         !149 out, MPB1 total respiration rate S1           (gC/m3/d)
      REAL               :: FMPB2RESS1         !150 out, MPB2 total respiration rate S1           (gC/m3/d)
      REAL               :: FMPB1GPS1M         !151 out, MPB1 gross primary production rate S1 m2 (gC/m2/d)
      REAL               :: FMPB2GPS1M         !152 out, MPB2 gross primary production rate S1 m2 (gC/m2/d)
      REAL               :: FMPB1GPS1D         !153 out, MPB1 gross primary production rate S1 per day(1/d)
      REAL               :: FMPB2GPS1D         !154 out, MPB2 gross primary production rate S1 per day(1/d)

C     local

      INTEGER            :: ISEG               ! loop counter segment loop
      INTEGER            :: IFLUX              ! index pointer in FL (flux) array
      INTEGER            :: IKMRK1             ! first feature inactive(0)-active(1)-bottom(2) segment
      INTEGER            :: IKMRK2             ! second feature 2D(0)-surface(1)-middle(2)-bottom(3) segment
      INTEGER, parameter :: NO_POINTER = 154   ! number of input output variables in PMSA array
      INTEGER            :: IP(NO_POINTER)     ! index pointer in PMSA array updated for each segment
      REAL               :: C_UPTAKE
      REAL               :: DMINN
      REAL               :: DMINP
      REAL               :: DMINS
      REAL               :: DN
      REAL               :: DP
      REAL               :: DSI
      REAL               :: FACTOR_MPB1
      REAL               :: FACTOR_MPB2
      REAL               :: FEXC_MPB1
      REAL               :: FEXC_MPB2
      REAL               :: FGP_MPB1
      REAL               :: FGP_MPB2
      REAL               :: FMOR_MPB1
      REAL               :: FMOR_MPB2
      REAL               :: FNH4_MPB1
      REAL               :: FNH4_MPB2
      REAL               :: FNO3_MPB1
      REAL               :: FNO3_MPB2
      REAL               :: FRES_MPB1
      REAL               :: FRES_MPB2
      REAL               :: FN_MPB1
      REAL               :: FN_MPB2
      REAL               :: NH4_UPTAKE
      REAL               :: NO3_UPTAKE
      REAL               :: N_UPTAKE
      REAL               :: OPAL_PROD
      REAL               :: PO4_UPTAKE
      REAL               :: POC_PROC
      REAL               :: PON_PROD
      REAL               :: POP_PROD
      REAL               :: P_UPTAKE
      REAL               :: SED2WAT
      REAL               :: SI_UPTAKE
      REAL               :: VOLSED             ! bulk volume of the sediment layer
      REAL               :: VOLWAT             ! volume
      REAL               :: MRES_MPB1          ! maintenance respiration MPB1
      REAL               :: MRES_MPB2          ! maintenance respiration MPB2
      REAL               :: FGP_MPB1_ORG       ! uncorrected FGP MPB1
      REAL               :: FGP_MPB2_ORG       ! uncorrected FGP MPB2
      REAL               :: EUF_FACT           ! correction factor for euphotic depth
      REAL               :: BIOMAS_MPB1_EUF    ! biomass corrected for euphotic depth
      REAL               :: BIOMAS_MPB2_EUF    ! biomass corrected for euphotic depth

      LOGICAL            :: WATER_OVERHEAD     ! guard against "dry" segments - no exchange with the water

C     initialise pointers for PMSA and FL array

      IP = IPOINT(1:NO_POINTER)
      IFLUX = 0

C     loop over the segments

      DO 1000 ISEG = 1 , NOSEG

         CALL evaluate_waq_attribute(1,IKNMRK(ISEG),IKMRK1)
         CALL evaluate_waq_attribute(2,IKNMRK(ISEG),IKMRK2)

         TEMP           = PMSA(IP(1))
         BIOMAS_MPB1    = MAX(0.0,PMSA(IP(2)))
         BIOMAS_MPB2    = MAX(0.0,PMSA(IP(3)))
         BIOMAS_S1_MPB1 = MAX(0.0,PMSA(IP(4)))
         BIOMAS_S1_MPB2 = MAX(0.0,PMSA(IP(5)))
         PMCH20_MPB1    = PMSA(IP(6))
         PMCH20_MPB2    = PMSA(IP(7))
         FLT_MPB1       = PMSA(IP(8))
         FLT_MPB2       = PMSA(IP(9))
         FTMP_MPB1      = PMSA(IP(10))
         FTMP_MPB2      = PMSA(IP(11))
         FNUT_MPB1      = PMSA(IP(12))
         FNUT_MPB2      = PMSA(IP(13))
         R_PR_MPB1      = PMSA(IP(14))
         R_PR_MPB2      = PMSA(IP(15))
         R_MT20_MPB1    = PMSA(IP(16))
         R_MT20_MPB2    = PMSA(IP(17))
         RT_MPB1        = PMSA(IP(18))
         RT_MPB2        = PMSA(IP(19))
         B_EX_MPB1      = PMSA(IP(20))
         B_EX_MPB2      = PMSA(IP(21))
         M1_20_MPB1     = PMSA(IP(22))
         M1_20_MPB2     = PMSA(IP(23))
         M2_20_MPB1     = PMSA(IP(24))
         M2_20_MPB2     = PMSA(IP(25))
         MT_MPB1        = PMSA(IP(26))
         MT_MPB2        = PMSA(IP(27))
         NCRAT_MPB1     = PMSA(IP(28))
         NCRAT_MPB2     = PMSA(IP(29))
         PCRAT_MPB1     = PMSA(IP(30))
         PCRAT_MPB2     = PMSA(IP(31))
         SCRAT_MPB1     = PMSA(IP(32))
         SCRAT_MPB2     = PMSA(IP(33))
         !FAM_MPB1       = PMSA(IP(34))  -- no longer used: change in nutrient preference
         !FAM_MPB2       = PMSA(IP(35))
         !FNI_MPB1       = PMSA(IP(36))
         !FNI_MPB2       = PMSA(IP(37))
         TRESH_MPB1     = PMSA(IP(38))
         TRESH_MPB2     = PMSA(IP(39))
         S1_BOTTOM      = NINT(PMSA(IP(40))) .EQ. 1
         FLT_S1_MPB1    = PMSA(IP(41))
         FLT_S1_MPB2    = PMSA(IP(42))
         FTMP_S1_MPB1   = PMSA(IP(43))
         FTMP_S1_MPB2   = PMSA(IP(44))
         FNUT_S1_MPB1   = PMSA(IP(45))
         FNUT_S1_MPB2   = PMSA(IP(46))
         !FAM_S1_MPB1    = PMSA(IP(47))  -- no longer used
         !FAM_S1_MPB2    = PMSA(IP(48))
         !FNI_S1_MPB1    = PMSA(IP(49))
         !FNI_S1_MPB2    = PMSA(IP(50))
         NH4            = MAX(0.0,PMSA(IP(51)))
         NO3            = MAX(0.0,PMSA(IP(52)))
         PO4            = MAX(0.0,PMSA(IP(53)))
         SI             = MAX(0.0,PMSA(IP(54)))
         ZSED           = PMSA(IP(55))
         SURF           = PMSA(IP(56))
         DEPTH          = PMSA(IP(57))
         DELT           = PMSA(IP(58))
         dBotN          = PMSA(IP(59))
         dSWN           = PMSA(IP(60))
         dGSNH          = PMSA(IP(61))
         dGSNO          = PMSA(IP(62))
         dBotP          = PMSA(IP(63))
         dSWP           = PMSA(IP(64))
         dGSP           = PMSA(IP(65))
         dBotSi         = PMSA(IP(66))
         dSWSi          = PMSA(IP(67))
         CCAP_MPB1      = PMSA(IP(68)) / ZSED
         CCAP_MPB2      = PMSA(IP(69)) / ZSED
         LOCSEDDEPT     = PMSA(IP(70))
         OXY            = PMSA(IP(71))
         MPBOXYCRIT     = PMSA(IP(72))
         MPB1MO_20      = PMSA(IP(73))
         MPB2MO_20      = PMSA(IP(74))

         FAM_MPB1       = NH4
         FAM_MPB2       = NO3
         FNI_MPB1       = NH4
         FNI_MPB2       = NO3

C           check proces parameters

         IF (ZSED.LT.1E-20)  CALL write_error_message_with_values('ZSed'   ,ZSED   ,ISEG,'GEMMPB')
         IF (SURF.LT.1E-20)  CALL write_error_message_with_values('Surf'   ,SURF   ,ISEG,'GEMMPB')
         IF (RT_MPB1.LE.0.0) CALL write_error_message_with_values('RT_MPB1',RT_MPB1,ISEG,'GEMMPB')
         IF (MT_MPB1.LE.0.0) CALL write_error_message_with_values('MT_MPB1',MT_MPB1,ISEG,'GEMMPB')
         IF (RT_MPB2.LE.0.0) CALL write_error_message_with_values('RT_MPB2',RT_MPB2,ISEG,'GEMMPB')
         IF (MT_MPB2.LE.0.0) CALL write_error_message_with_values('MT_MPB2',MT_MPB2,ISEG,'GEMMPB')

C        Active water segments and bottom segments

         IF ( IKMRK1.EQ.1 .OR. IKMRK1.EQ.2 ) THEN

C           for top layer thicker then euphotic depth all production in euphotic zone, this increases the biomass concentration
C           to avoid a whole lot of scaling of the fluxes etc we do this by enhancing the PPMAX ? this gives problems for logistic restraint which is on biomass

            IF ( IKMRK1 .EQ. 2 .AND. ABS(DEPTH - LOCSEDDEPT) .LT. 1.E-20 .AND. DEPTH .GT. ZSED ) THEN
               EUF_FACT   = DEPTH/ZSED
               TRESH_MPB1 = TRESH_MPB1 / EUF_FACT
               TRESH_MPB2 = TRESH_MPB2 / EUF_FACT
            ELSE
               EUF_FACT   = 1.0
            ENDIF
            BIOMAS_MPB1_EUF = BIOMAS_MPB1*EUF_FACT
            BIOMAS_MPB2_EUF = BIOMAS_MPB2*EUF_FACT

C           logistic growth restraint (o.a. CO2 limitation)
C           (only for the bottom segments)

            IF ( IKMRK1 .EQ. 2 ) THEN
               MPB1FMC = MAX((CCAP_MPB1-BIOMAS_MPB1_EUF)/CCAP_MPB1,0.0)
               MPB2FMC = MAX((CCAP_MPB2-BIOMAS_MPB2_EUF)/CCAP_MPB2,0.0)
            ELSE
               MPB1FMC = 1.0
               MPB2FMC = 1.0
            ENDIF

C           Gross primary production

            FGP_MPB1 = MIN(FNUT_MPB1,FLT_MPB1)*FTMP_MPB1*PMCH20_MPB1*MAX(BIOMAS_MPB1,TRESH_MPB1)*MPB1FMC
            FGP_MPB2 = MIN(FNUT_MPB2,FLT_MPB2)*FTMP_MPB2*PMCH20_MPB2*MAX(BIOMAS_MPB2,TRESH_MPB2)*MPB2FMC

C           Respiration

            MRES_MPB1 = R_MT20_MPB1 * RT_MPB1**(TEMP-20.) * BIOMAS_MPB1
            MRES_MPB2 = R_MT20_MPB2 * RT_MPB2**(TEMP-20.) * BIOMAS_MPB2
            FRES_MPB1 = MRES_MPB1 + R_PR_MPB1 * FGP_MPB1
            FRES_MPB2 = MRES_MPB2 + R_PR_MPB2 * FGP_MPB2

C           when oxygen depletion and when respiration exceeds growth then no growth and respiration (do not take into account excretion), extra mortality

            IF ( OXY .LT. MPBOXYCRIT ) THEN

               IF ( FRES_MPB1 .GT. FGP_MPB1 ) THEN
                  PMCH20_MPB1 = 0.0
                  R_MT20_MPB1 = 0.0
                  M1_20_MPB1  = MPB1MO_20
                  FGP_MPB1    = 0.0
                  MRES_MPB1   = 0.0
                  FRES_MPB1   = 0.0
               ENDIF

               IF ( FRES_MPB2 .GT. FGP_MPB2 ) THEN
                  PMCH20_MPB2 = 0.0
                  R_MT20_MPB2 = 0.0
                  M1_20_MPB2  = MPB2MO_20
                  FGP_MPB2    = 0.0
                  MRES_MPB2   = 0.0
                  FRES_MPB2   = 0.0
               ENDIF

            ENDIF

C           beschikbaarheid nutrienten (g/d)

            DN    = (NH4+NO3)/DELT
            DP    = PO4/DELT
            DSI   = SI/DELT

C           uptake mag niet groter zijn dan beschikbaarheid

            FGP_MPB1_ORG = FGP_MPB1
            FGP_MPB2_ORG = FGP_MPB2
            N_UPTAKE  = (FGP_MPB1-FRES_MPB1)*NCRAT_MPB1 + (FGP_MPB2-FRES_MPB2)*NCRAT_MPB2
            IF ( N_UPTAKE .GT. DN   ) THEN

               FACTOR_MPB1 = FGP_MPB1*NCRAT_MPB1/(FGP_MPB1*NCRAT_MPB1+FGP_MPB2*NCRAT_MPB2)
               FACTOR_MPB2 = 1.-FACTOR_MPB1

               FGP_MPB1 = ((FACTOR_MPB1*DN/NCRAT_MPB1)+ MRES_MPB1)/(1.-R_PR_MPB1)
               FGP_MPB2 = ((FACTOR_MPB2*DN/NCRAT_MPB2)+ MRES_MPB2)/(1.-R_PR_MPB2)

               FRES_MPB1 = MRES_MPB1 + R_PR_MPB1 * FGP_MPB1
               FRES_MPB2 = MRES_MPB2 + R_PR_MPB2 * FGP_MPB2

               IF ( FGP_MPB1_ORG .GT. 1.E-20) THEN
                  MPB1FMN = FGP_MPB1 / FGP_MPB1_ORG
               ELSE
                  MPB1FMN = 0.0
               ENDIF
               IF ( FGP_MPB2_ORG .GT. 1.E-20) THEN
                  MPB2FMN = FGP_MPB2 / FGP_MPB2_ORG
               ELSE
                  MPB2FMN = 0.0
               ENDIF

            ELSE
               MPB1FMN = 1.0
               MPB2FMN = 1.0
            ENDIF
            P_UPTAKE  = (FGP_MPB1-FRES_MPB1)*PCRAT_MPB1 + (FGP_MPB2-FRES_MPB2)*PCRAT_MPB2
            IF ( P_UPTAKE .GT. DP   ) THEN

               FACTOR_MPB1 = FGP_MPB1*PCRAT_MPB1/(FGP_MPB1*PCRAT_MPB1+FGP_MPB2*PCRAT_MPB2)
               FACTOR_MPB2 = 1.-FACTOR_MPB1

               FGP_MPB1 = ((FACTOR_MPB1*DP/PCRAT_MPB1)+ MRES_MPB1)/(1.-R_PR_MPB1)
               FGP_MPB2 = ((FACTOR_MPB2*DP/PCRAT_MPB2)+ MRES_MPB2)/(1.-R_PR_MPB2)

               FRES_MPB1 = MRES_MPB1 + R_PR_MPB1 * FGP_MPB1
               FRES_MPB2 = MRES_MPB2 + R_PR_MPB2 * FGP_MPB2

               IF ( FGP_MPB1_ORG .GT. 1.E-20) THEN
                  MPB1FMP = FGP_MPB1 / FGP_MPB1_ORG
               ELSE
                  MPB1FMP = 0.0
               ENDIF
               IF ( FGP_MPB2_ORG .GT. 1.E-20) THEN
                  MPB2FMP = FGP_MPB2 / FGP_MPB2_ORG
               ELSE
                  MPB2FMP = 0.0
               ENDIF

            ELSE
               MPB1FMP = 1.0
               MPB2FMP = 1.0
            ENDIF
            SI_UPTAKE  = (FGP_MPB1-FRES_MPB1)*SCRAT_MPB1 + (FGP_MPB2-FRES_MPB2)*SCRAT_MPB2
            IF ( SI_UPTAKE .GT. DSI  ) THEN

               FACTOR_MPB1 = FGP_MPB1*SCRAT_MPB1/(FGP_MPB1*SCRAT_MPB1+FGP_MPB2*SCRAT_MPB2)
               FACTOR_MPB2 = 1.-FACTOR_MPB1

               FGP_MPB1 = ((FACTOR_MPB1*DSI/SCRAT_MPB1)+ MRES_MPB1)/(1.-R_PR_MPB1)
               FGP_MPB2 = ((FACTOR_MPB2*DSI/SCRAT_MPB2)+ MRES_MPB2)/(1.-R_PR_MPB2)

               FRES_MPB1 = MRES_MPB1 + R_PR_MPB1 * FGP_MPB1
               FRES_MPB2 = MRES_MPB2 + R_PR_MPB2 * FGP_MPB2

               IF ( FGP_MPB1_ORG .GT. 1.E-20) THEN
                  MPB1FMS = FGP_MPB1 / FGP_MPB1_ORG
               ELSE
                  MPB1FMS = 0.0
               ENDIF
               IF ( FGP_MPB2_ORG .GT. 1.E-20) THEN
                  MPB2FMS = FGP_MPB2 / FGP_MPB2_ORG
               ELSE
                  MPB2FMS = 0.0
               ENDIF

            ELSE
               MPB1FMS = 1.0
               MPB2FMS = 1.0
            ENDIF

C           Excretion

            FEXC_MPB1 = B_EX_MPB1 * ( 1. - FNUT_MPB1 ) * FGP_MPB1
            FEXC_MPB2 = B_EX_MPB2 * ( 1. - FNUT_MPB2 ) * FGP_MPB2

C           Mortality

            FMOR_MPB1 = MT_MPB1**(TEMP-20.) * BIOMAS_MPB1 * (M1_20_MPB1 + M2_20_MPB1 * BIOMAS_MPB1)
            FMOR_MPB2 = MT_MPB2**(TEMP-20.) * BIOMAS_MPB2 * (M1_20_MPB2 + M2_20_MPB2 * BIOMAS_MPB2)

C           NH4 over NO3 preferency

            !FN_MPB1 = FAM_MPB1 + (1.-FAM_MPB1)*FNI_MPB1
            FN_MPB1 = FNI_MPB1 + FAM_MPB1
            IF ( FN_MPB1 .GT. 1.E-20 ) THEN
               FNO3_MPB1 = FNI_MPB1 / FN_MPB1
               FNH4_MPB1 = FAM_MPB1 / FN_MPB1
            ELSE
               FNO3_MPB1 = 0.0
               FNH4_MPB1 = 1.0
            ENDIF
            !FN_MPB2 = FAM_MPB2 + (1.-FAM_MPB2)*FNI_MPB2
            FN_MPB2 = FNI_MPB2 + FAM_MPB2
            IF ( FN_MPB2 .GT. 1.E-20 ) THEN
               FNO3_MPB2 = FNI_MPB2 / FN_MPB2
               FNH4_MPB2 = FAM_MPB2 / FN_MPB2
            ELSE
               FNO3_MPB2 = 0.0
               FNH4_MPB2 = 1.0
            ENDIF

C           additional output

            BIOMAS_MPB1_M2 = BIOMAS_MPB1 * DEPTH
            BIOMAS_MPB2_M2 = BIOMAS_MPB2 * DEPTH
            FMPB1NH4UP = (FGP_MPB1-FRES_MPB1)*NCRAT_MPB1*FNH4_MPB1
            FMPB2NH4UP = (FGP_MPB2-FRES_MPB2)*NCRAT_MPB2*FNH4_MPB2
            FMPB1NO3UP = (FGP_MPB1-FRES_MPB1)*NCRAT_MPB1*FNO3_MPB1
            FMPB2NO3UP = (FGP_MPB2-FRES_MPB2)*NCRAT_MPB2*FNO3_MPB2
            FMPB1PO4UP = (FGP_MPB1-FRES_MPB1)*PCRAT_MPB1
            FMPB2PO4UP = (FGP_MPB2-FRES_MPB2)*PCRAT_MPB2
            FMPB1SIUP  = (FGP_MPB1-FRES_MPB1)*SCRAT_MPB1
            FMPB2SIUP  = (FGP_MPB2-FRES_MPB2)*SCRAT_MPB2
            FMPB1EXC   = FEXC_MPB1
            FMPB2EXC   = FEXC_MPB2
            FMPB1FGP   = FGP_MPB1
            FMPB2FGP   = FGP_MPB2
            FMPB1MOR   = FMOR_MPB1
            FMPB2MOR   = FMOR_MPB2
            FMPB1POC1  = FMOR_MPB1+FEXC_MPB1
            FMPB2POC1  = FMOR_MPB2+FEXC_MPB2
            FMPB1PON1  = FMOR_MPB1 * NCRAT_MPB1
            FMPB2PON1  = FMOR_MPB2 * NCRAT_MPB2
            FMPB1POP1  = FMOR_MPB1 * PCRAT_MPB1
            FMPB2POP1  = FMOR_MPB2 * PCRAT_MPB2
            FMPB1OPAL  = FMOR_MPB1 * SCRAT_MPB1
            FMPB2OPAL  = FMOR_MPB2 * SCRAT_MPB2
            FMPB1OXY   = (FGP_MPB1-FRES_MPB1+FEXC_MPB1)*2.67
            FMPB2OXY   = (FGP_MPB1-FRES_MPB1+FEXC_MPB1)*2.67
            FMPB1RES   = FRES_MPB1
            FMPB2RES   = FRES_MPB2
            FMPB1FGPM2 = FMPB1FGP*DEPTH
            FMPB2FGPM2 = FMPB2FGP*DEPTH
            IF ( BIOMAS_MPB1 .GT. 1.E-20 ) THEN
               FMPB1FGPD  = FMPB1FGP/BIOMAS_MPB1
            ELSE
               FMPB1FGPD  = 0.0
            ENDIF
            IF ( BIOMAS_MPB2 .GT. 1.E-20 ) THEN
               FMPB2FGPD  = FMPB2FGP/BIOMAS_MPB2
            ELSE
               FMPB2FGPD  = 0.0
            ENDIF

C           uptake and production fluxes

            C_UPTAKE    = FGP_MPB1-FRES_MPB1+FEXC_MPB1 + FGP_MPB2-FRES_MPB2+FEXC_MPB2
            NO3_UPTAKE  = (FGP_MPB1-FRES_MPB1)*NCRAT_MPB1*FNO3_MPB1 + (FGP_MPB2-FRES_MPB2)*NCRAT_MPB2*FNO3_MPB2
            NH4_UPTAKE  = (FGP_MPB1-FRES_MPB1)*NCRAT_MPB1*FNH4_MPB1 + (FGP_MPB2-FRES_MPB2)*NCRAT_MPB2*FNH4_MPB2
            PO4_UPTAKE  = (FGP_MPB1-FRES_MPB1)*PCRAT_MPB1 + (FGP_MPB2-FRES_MPB2)*PCRAT_MPB2
            SI_UPTAKE   = (FGP_MPB1-FRES_MPB1)*SCRAT_MPB1 + (FGP_MPB2-FRES_MPB2)*SCRAT_MPB2
            POC_PROC    = FMOR_MPB1+FEXC_MPB1 + FMOR_MPB2+FEXC_MPB2
            PON_PROD    = FMOR_MPB1 * NCRAT_MPB1 + FMOR_MPB2 * NCRAT_MPB2
            POP_PROD    = FMOR_MPB1 * PCRAT_MPB1 + FMOR_MPB2 * PCRAT_MPB2
            OPAL_PROD   = FMOR_MPB1 * SCRAT_MPB1 + FMOR_MPB2 * SCRAT_MPB2

C           update the output flux array FL

            FL( 1+IFLUX) = FGP_MPB1
            FL( 2+IFLUX) = FGP_MPB2
            FL( 3+IFLUX) = FRES_MPB1
            FL( 4+IFLUX) = FRES_MPB2
            FL( 5+IFLUX) = FMOR_MPB1
            FL( 6+IFLUX) = FMOR_MPB2
            FL( 7+IFLUX) = C_UPTAKE
            FL( 8+IFLUX) = C_UPTAKE
            FL( 9+IFLUX) = NO3_UPTAKE
            FL(10+IFLUX) = NH4_UPTAKE
            FL(11+IFLUX) = PO4_UPTAKE
            FL(12+IFLUX) = SI_UPTAKE
            FL(13+IFLUX) = POC_PROC
            FL(14+IFLUX) = PON_PROD
            FL(15+IFLUX) = POP_PROD
            FL(16+IFLUX) = OPAL_PROD

C           output parameters in PMSA

            PMSA(IP(75)) = BIOMAS_MPB1_M2
            PMSA(IP(76)) = BIOMAS_MPB2_M2
            PMSA(IP(79)) = MPB1FMC
            PMSA(IP(80)) = MPB2FMC
            PMSA(IP(81)) = MPB1FMN
            PMSA(IP(82)) = MPB2FMN
            PMSA(IP(83)) = MPB1FMP
            PMSA(IP(84)) = MPB2FMP
            PMSA(IP(85)) = MPB1FMS
            PMSA(IP(86)) = MPB2FMS

            PMSA(IP(95 )) = FMPB1NH4UP
            PMSA(IP(96 )) = FMPB2NH4UP
            PMSA(IP(97 )) = FMPB1NO3UP
            PMSA(IP(98 )) = FMPB2NO3UP
            PMSA(IP(99 )) = FMPB1PO4UP
            PMSA(IP(100)) = FMPB2PO4UP
            PMSA(IP(101)) = FMPB1SIUP
            PMSA(IP(102)) = FMPB2SIUP
            PMSA(IP(103)) = FMPB1EXC
            PMSA(IP(104)) = FMPB2EXC
            PMSA(IP(105)) = FMPB1FGP
            PMSA(IP(106)) = FMPB2FGP
            PMSA(IP(107)) = FMPB1MOR
            PMSA(IP(108)) = FMPB2MOR
            PMSA(IP(109)) = FMPB1POC1
            PMSA(IP(110)) = FMPB2POC1
            PMSA(IP(111)) = FMPB1PON1
            PMSA(IP(112)) = FMPB2PON1
            PMSA(IP(113)) = FMPB1POP1
            PMSA(IP(114)) = FMPB2POP1
            PMSA(IP(115)) = FMPB1OPAL
            PMSA(IP(116)) = FMPB2OPAL
            PMSA(IP(117)) = FMPB1OXY
            PMSA(IP(118)) = FMPB2OXY
            PMSA(IP(119)) = FMPB1RES
            PMSA(IP(120)) = FMPB2RES
            PMSA(IP(121)) = FMPB1FGPM2
            PMSA(IP(122)) = FMPB2FGPM2
            PMSA(IP(123)) = FMPB1FGPD
            PMSA(IP(124)) = FMPB2FGPD

         ENDIF

C        S1_BOTTOM fluxes

         IF ( S1_BOTTOM .AND. ( IKMRK2 .EQ. 0 .OR. IKMRK2 .EQ. 3 ) ) THEN

C           Converting biomass from gC/m2 to gC/m3

            IF ( DEPTH > 0.001*ZSED ) THEN
               WATER_OVERHEAD = .TRUE.
               VOLWAT  = SURF * DEPTH
               VOLSED  = SURF * ZSED
               SED2WAT = VOLSED/VOLWAT
            ELSE
               WATER_OVERHEAD = .FALSE.
               SED2WAT        = 0.0     ! This is a bit of a puzzle ...
            ENDIF
            BIOMAS_S1_MPB1 = BIOMAS_S1_MPB1/ZSED
            BIOMAS_S1_MPB2 = BIOMAS_S1_MPB2/ZSED

C           logistic growth restraint (o.a. CO2 limitation)

            MPB1FMCS1 = MAX((CCAP_MPB1-BIOMAS_S1_MPB1)/CCAP_MPB1,0.0)
            MPB2FMCS1 = MAX((CCAP_MPB2-BIOMAS_S1_MPB2)/CCAP_MPB2,0.0)

C           Gross primary production

            FGP_MPB1 = MIN(FNUT_S1_MPB1,FLT_S1_MPB1)*FTMP_S1_MPB1*PMCH20_MPB1*MAX(BIOMAS_S1_MPB1,TRESH_MPB1)*MPB1FMCS1
            FGP_MPB2 = MIN(FNUT_S1_MPB2,FLT_S1_MPB2)*FTMP_S1_MPB2*PMCH20_MPB2*MAX(BIOMAS_S1_MPB2,TRESH_MPB2)*MPB2FMCS1

C           Respiration

            MRES_MPB1 = R_MT20_MPB1 * RT_MPB1**(TEMP-20.) * BIOMAS_S1_MPB1
            MRES_MPB2 = R_MT20_MPB2 * RT_MPB2**(TEMP-20.) * BIOMAS_S1_MPB2
            FRES_MPB1 = MRES_MPB1 + R_PR_MPB1 * FGP_MPB1
            FRES_MPB2 = MRES_MPB2 + R_PR_MPB2 * FGP_MPB2

C           mineralisatieflux vanuit 3 mogelijke bodemmodulen, schalen naar sediment volume

            IF ( WATER_OVERHEAD ) THEN
               dMinN = MAX(dBotN, dSWN, (dGSNH + dGSNO))/SED2WAT
               dMinP = MAX(dBotP, dSWP, dGSP)/SED2WAT
               dMinS = MAX(dBotSi, dSWSi)/SED2WAT
            ELSE
               dMinN = 0.0
               dMinP = 0.0
               dMinS = 0.0
            ENDIF

C           uptake mag niet groter zijn dan beschikbaarheid (mineralisatieflux)

            FGP_MPB1_ORG = FGP_MPB1
            FGP_MPB2_ORG = FGP_MPB2
            N_UPTAKE  = (FGP_MPB1-FRES_MPB1)*NCRAT_MPB1 + (FGP_MPB2-FRES_MPB2)*NCRAT_MPB2
            IF ( N_UPTAKE .GT. dMinN) THEN

               FACTOR_MPB1 = FGP_MPB1*NCRAT_MPB1/(FGP_MPB1*NCRAT_MPB1+FGP_MPB2*NCRAT_MPB2)
               FACTOR_MPB2 = 1.-FACTOR_MPB1

               FGP_MPB1 = ((FACTOR_MPB1*dMinN/NCRAT_MPB1)+ MRES_MPB1)/(1.-R_PR_MPB1)
               FGP_MPB2 = ((FACTOR_MPB2*dMinN/NCRAT_MPB2)+ MRES_MPB2)/(1.-R_PR_MPB2)

               FRES_MPB1 = MRES_MPB1 + R_PR_MPB1 * FGP_MPB1
               FRES_MPB2 = MRES_MPB2 + R_PR_MPB2 * FGP_MPB2

               IF ( FGP_MPB1_ORG .GT. 1.E-20) THEN
                  MPB1FMNS1 = FGP_MPB1 / FGP_MPB1_ORG
               ELSE
                  MPB1FMNS1 = 0.0
               ENDIF
               IF ( FGP_MPB2_ORG .GT. 1.E-20) THEN
                  MPB2FMNS1 = FGP_MPB2 / FGP_MPB2_ORG
               ELSE
                  MPB2FMNS1 = 0.0
               ENDIF

            ELSE
               MPB1FMNS1 = 1.0
               MPB2FMNS1 = 1.0
            ENDIF
            P_UPTAKE  = (FGP_MPB1-FRES_MPB1)*PCRAT_MPB1 + (FGP_MPB2-FRES_MPB2)*PCRAT_MPB2
            IF ( P_UPTAKE .GT. dMinP) THEN

               FACTOR_MPB1 = FGP_MPB1*PCRAT_MPB1/(FGP_MPB1*PCRAT_MPB1+FGP_MPB2*PCRAT_MPB2)
               FACTOR_MPB2 = 1.-FACTOR_MPB1

               FGP_MPB1 = ((FACTOR_MPB1*dMinP/PCRAT_MPB1)+ MRES_MPB1)/(1.-R_PR_MPB1)
               FGP_MPB2 = ((FACTOR_MPB2*dMinP/PCRAT_MPB2)+ MRES_MPB2)/(1.-R_PR_MPB2)

               FRES_MPB1 = MRES_MPB1 + R_PR_MPB1 * FGP_MPB1
               FRES_MPB2 = MRES_MPB2 + R_PR_MPB2 * FGP_MPB2

               IF ( FGP_MPB1_ORG .GT. 1.E-20) THEN
                  MPB1FMPS1 = FGP_MPB1 / FGP_MPB1_ORG
               ELSE
                  MPB1FMPS1 = 0.0
               ENDIF
               IF ( FGP_MPB2_ORG .GT. 1.E-20) THEN
                  MPB2FMPS1 = FGP_MPB2 / FGP_MPB2_ORG
               ELSE
                  MPB2FMPS1 = 0.0
               ENDIF

            ELSE
               MPB1FMPS1 = 1.0
               MPB2FMPS1 = 1.0
            ENDIF
            SI_UPTAKE  = (FGP_MPB1-FRES_MPB1)*SCRAT_MPB1 + (FGP_MPB2-FRES_MPB2)*SCRAT_MPB2
            IF ( SI_UPTAKE .GT. dMinS) THEN

               FACTOR_MPB1 = FGP_MPB1*SCRAT_MPB1/(FGP_MPB1*SCRAT_MPB1+FGP_MPB2*SCRAT_MPB2)
               FACTOR_MPB2 = 1.-FACTOR_MPB1

               FGP_MPB1 = ((FACTOR_MPB1*dMinS/SCRAT_MPB1)+ MRES_MPB1)/(1.-R_PR_MPB1)
               FGP_MPB2 = ((FACTOR_MPB2*dMinS/SCRAT_MPB2)+ MRES_MPB2)/(1.-R_PR_MPB2)

               FRES_MPB1 = MRES_MPB1 + R_PR_MPB1 * FGP_MPB1
               FRES_MPB2 = MRES_MPB2 + R_PR_MPB2 * FGP_MPB2

               IF ( FGP_MPB1_ORG .GT. 1.E-20) THEN
                  MPB1FMSS1 = FGP_MPB1 / FGP_MPB1_ORG
               ELSE
                  MPB1FMSS1 = 0.0
               ENDIF
               IF ( FGP_MPB2_ORG .GT. 1.E-20) THEN
                  MPB2FMSS1 = FGP_MPB2 / FGP_MPB2_ORG
               ELSE
                  MPB2FMSS1 = 0.0
               ENDIF

            ELSE
               MPB1FMSS1 = 1.0
               MPB2FMSS1 = 1.0
            ENDIF

C           Excretion

            FEXC_MPB1 = B_EX_MPB1 * ( 1. - FNUT_S1_MPB1 ) * FGP_MPB1
            FEXC_MPB2 = B_EX_MPB2 * ( 1. - FNUT_S1_MPB2 ) * FGP_MPB2

C           Mortality

            FMOR_MPB1 = MT_MPB1**(TEMP-20.) * BIOMAS_S1_MPB1 * (M1_20_MPB1 + M2_20_MPB1 * BIOMAS_S1_MPB1)
            FMOR_MPB2 = MT_MPB2**(TEMP-20.) * BIOMAS_S1_MPB2 * (M1_20_MPB2 + M2_20_MPB2 * BIOMAS_S1_MPB2)

C           NH4 over NO3 preferency
!
!           Note: this does not work properly, as the mineralisation flux is assigned exclusively to NH4.
!           The original code assumes however that part can be consumed as NH4 and part as NO3, so that
!           you get a correction on both fluxes. The fact that there is no flux to NO3 in the water means
!           you have an exclusively negative contribution to NO3 in the water.

            FNO3_MPB1 = 0.0
            FNH4_MPB1 = 1.0
            FNO3_MPB2 = 0.0
            FNH4_MPB2 = 1.0

C           uptake and production fluxes

            C_UPTAKE    = FGP_MPB1-FRES_MPB1+FEXC_MPB1 + FGP_MPB2-FRES_MPB2+FEXC_MPB2
            NO3_UPTAKE  = (FGP_MPB1-FRES_MPB1)*NCRAT_MPB1*FNO3_MPB1 + (FGP_MPB2-FRES_MPB2)*NCRAT_MPB2*FNO3_MPB2
            NH4_UPTAKE  = (FGP_MPB1-FRES_MPB1)*NCRAT_MPB1*FNH4_MPB1 + (FGP_MPB2-FRES_MPB2)*NCRAT_MPB2*FNH4_MPB2
            PO4_UPTAKE  = (FGP_MPB1-FRES_MPB1)*PCRAT_MPB1 + (FGP_MPB2-FRES_MPB2)*PCRAT_MPB2
            SI_UPTAKE   = (FGP_MPB1-FRES_MPB1)*SCRAT_MPB1 + (FGP_MPB2-FRES_MPB2)*SCRAT_MPB2
            POC_PROC    = FMOR_MPB1+FEXC_MPB1 + FMOR_MPB2+FEXC_MPB2
            PON_PROD    = FMOR_MPB1 * NCRAT_MPB1 + FMOR_MPB2 * NCRAT_MPB2
            POP_PROD    = FMOR_MPB1 * PCRAT_MPB1 + FMOR_MPB2 * PCRAT_MPB2
            OPAL_PROD   = FMOR_MPB1 * SCRAT_MPB1 + FMOR_MPB2 * SCRAT_MPB2

C           update the output flux array FL
C           fluxes scaled from the sediment volume to the water volume (SED2WAT)

            FL(17+IFLUX) = FGP_MPB1*SED2WAT
            FL(18+IFLUX) = FGP_MPB2*SED2WAT
            FL(19+IFLUX) = FRES_MPB1*SED2WAT
            FL(20+IFLUX) = FRES_MPB2*SED2WAT
            FL(21+IFLUX) = FMOR_MPB1*SED2WAT
            FL(22+IFLUX) = FMOR_MPB2*SED2WAT
            FL( 7+IFLUX) = FL( 7+IFLUX) + C_UPTAKE*SED2WAT
            FL( 8+IFLUX) = FL( 8+IFLUX) + C_UPTAKE*SED2WAT
            FL( 9+IFLUX) = FL( 9+IFLUX) + NO3_UPTAKE*SED2WAT
            FL(10+IFLUX) = FL(10+IFLUX) + NH4_UPTAKE*SED2WAT
            FL(11+IFLUX) = FL(11+IFLUX) + PO4_UPTAKE*SED2WAT
            FL(12+IFLUX) = FL(12+IFLUX) + SI_UPTAKE*SED2WAT
            FL(23+IFLUX) = POC_PROC*SED2WAT
            FL(24+IFLUX) = PON_PROD*SED2WAT
            FL(25+IFLUX) = POP_PROD*SED2WAT
            FL(26+IFLUX) = OPAL_PROD*SED2WAT

C           output parameters

            FMPB1NH4S1 = (FGP_MPB1-FRES_MPB1)*NCRAT_MPB1*FNH4_MPB1
            FMPB2NH4S1 = (FGP_MPB2-FRES_MPB2)*NCRAT_MPB2*FNH4_MPB2
            FMPB1NO3S1 = (FGP_MPB1-FRES_MPB1)*NCRAT_MPB1*FNO3_MPB1
            FMPB2NO3S1 = (FGP_MPB2-FRES_MPB2)*NCRAT_MPB2*FNO3_MPB2
            FMPB1PO4S1 = (FGP_MPB1-FRES_MPB1)*PCRAT_MPB1
            FMPB2PO4S1 = (FGP_MPB2-FRES_MPB2)*PCRAT_MPB2
            FMPB1SIS1  = (FGP_MPB1-FRES_MPB1)*SCRAT_MPB1
            FMPB2SIS1  = (FGP_MPB2-FRES_MPB2)*SCRAT_MPB2
            FMPB1EXCS1 = FEXC_MPB1
            FMPB2EXCS1 = FEXC_MPB2
            FMPB1FGPS1 = FGP_MPB1
            FMPB2FGPS1 = FGP_MPB2
            FMPB1MORS1 = FMOR_MPB1
            FMPB2MORS1 = FMOR_MPB2
            FMPB1POC1S = FMOR_MPB1+FEXC_MPB1
            FMPB2POC1S = FMOR_MPB2+FEXC_MPB2
            FMPB1PON1S = FMOR_MPB1 * NCRAT_MPB1
            FMPB2PON1S = FMOR_MPB2 * NCRAT_MPB2
            FMPB1POP1S = FMOR_MPB1 * PCRAT_MPB1
            FMPB2POP1S = FMOR_MPB2 * PCRAT_MPB2
            FMPB1OPALS = FMOR_MPB1 * SCRAT_MPB1
            FMPB2OPALS = FMOR_MPB2 * SCRAT_MPB2
            FMPB1OXYS1 = (FGP_MPB1-FRES_MPB1+FEXC_MPB1)*2.67
            FMPB2OXYS1 = (FGP_MPB1-FRES_MPB1+FEXC_MPB1)*2.67
            FMPB1RESS1 = FRES_MPB1
            FMPB2RESS1 = FRES_MPB2
            FMPB1GPS1M = FGP_MPB1/SURF
            FMPB2GPS1M = FGP_MPB1/SURF
            IF ( BIOMAS_S1_MPB1 .GT. 1.E-20 ) THEN
               FMPB1GPS1D = FGP_MPB1/BIOMAS_S1_MPB1
            ELSE
               FMPB1GPS1D = 0.0
            ENDIF
            IF ( BIOMAS_S1_MPB2 .GT. 1.E-20 ) THEN
               FMPB2GPS1D = FGP_MPB1/BIOMAS_S1_MPB2
            ELSE
               FMPB2GPS1D = 0.0
            ENDIF

            PMSA(IP(75)) = BIOMAS_S1_MPB1
            PMSA(IP(76)) = BIOMAS_S1_MPB2
            PMSA(IP(87)) = MPB1FMCS1
            PMSA(IP(88)) = MPB2FMCS1
            PMSA(IP(89)) = MPB1FMNS1
            PMSA(IP(90)) = MPB2FMNS1
            PMSA(IP(91)) = MPB1FMPS1
            PMSA(IP(92)) = MPB2FMPS1
            PMSA(IP(93)) = MPB1FMSS1
            PMSA(IP(94)) = MPB2FMSS1
            PMSA(IP(125)) = FMPB1NH4S1
            PMSA(IP(126)) = FMPB2NH4S1
            PMSA(IP(127)) = FMPB1NO3S1
            PMSA(IP(128)) = FMPB2NO3S1
            PMSA(IP(129)) = FMPB1PO4S1
            PMSA(IP(130)) = FMPB2PO4S1
            PMSA(IP(131)) = FMPB1SIS1
            PMSA(IP(132)) = FMPB2SIS1
            PMSA(IP(133)) = FMPB1EXCS1
            PMSA(IP(134)) = FMPB2EXCS1
            PMSA(IP(135)) = FMPB1FGPS1
            PMSA(IP(136)) = FMPB2FGPS1
            PMSA(IP(137)) = FMPB1MORS1
            PMSA(IP(138)) = FMPB2MORS1
            PMSA(IP(139)) = FMPB1POC1S
            PMSA(IP(140)) = FMPB2POC1S
            PMSA(IP(141)) = FMPB1PON1S
            PMSA(IP(142)) = FMPB2PON1S
            PMSA(IP(143)) = FMPB1POP1S
            PMSA(IP(144)) = FMPB2POP1S
            PMSA(IP(145)) = FMPB1OPALS
            PMSA(IP(146)) = FMPB2OPALS
            PMSA(IP(147)) = FMPB1OXYS1
            PMSA(IP(148)) = FMPB2OXYS1
            PMSA(IP(149)) = FMPB1RESS1
            PMSA(IP(150)) = FMPB2RESS1
            PMSA(IP(151)) = FMPB1GPS1M
            PMSA(IP(152)) = FMPB2GPS1M
            PMSA(IP(153)) = FMPB1GPS1D
            PMSA(IP(154)) = FMPB2GPS1D

         ENDIF

C        update pointering in PMSA and FL array

         IFLUX = IFLUX + NOFLUX
         IP    = IP    + INCREM(1:NO_POINTER)

 1000 CONTINUE

      RETURN
      END

      end module m_gemmpb
