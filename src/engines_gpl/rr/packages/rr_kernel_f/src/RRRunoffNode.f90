!----- AGPL ---------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2024.
!
!  This program is free software: you can redistribute it and/or modify
!  it under the terms of the GNU Affero General Public License as
!  published by the Free Software Foundation version 3.
!
!  This program is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU Affero General Public License for more details.
!
!  You should have received a copy of the GNU Affero General Public License
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D" and "Deltares"
!  are registered trademarks of Stichting Deltares, and remain the property of
!  Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------

 ! Last changed
! by:               $Author:: Prinsen           $
! at:               $Modtime:: April 2001       $
!
! current revision: $Revision:: 1               $

module WalrusData
    integer, parameter :: wc_par_cW = 1
    integer, parameter :: wc_par_cV = 2
    integer, parameter :: wc_par_cG = 3
    integer, parameter :: wc_par_cQ = 4
    integer, parameter :: wc_par_cS = 5
    integer, parameter :: wc_par_cD = 6
    integer, parameter :: wc_par_psi_ae = 7
    integer, parameter :: wc_par_b = 8
    integer, parameter :: wc_par_theta_s = 9
    integer, parameter :: wc_par_aS = 10
    integer, parameter :: wc_par_area = 11
    integer, parameter :: wc_par_cexpS = 12
    integer, parameter :: wc_sand = 21
    integer, parameter :: wc_loamy_sand = 22
    integer, parameter :: wc_sandy_loam = 23
    integer, parameter :: wc_silt_loam = 24
    integer, parameter :: wc_loam = 25
    integer, parameter :: wc_sandy_clay_loam = 26
    integer, parameter :: wc_silt_clay_loam = 27
    integer, parameter :: wc_clay_loam = 28
    integer, parameter :: wc_sandy_clay = 29
    integer, parameter :: wc_silty_clay = 30
    integer, parameter :: wc_clay = 31
    integer, parameter :: wc_cal_H = 32
    integer, parameter :: wc_cal_C = 33
    integer, parameter :: wc_custom = 34
    integer, parameter :: wc_par_min_deltime = 41
    integer, parameter :: wc_par_max_h_change = 42
    integer, parameter :: wc_par_min_h = 43
    integer, parameter :: wc_par_max_Pstep = 44
    integer, parameter :: wc_par_max_substeps = 45
    integer, parameter :: wc_fc_P = 50
    integer, parameter :: wc_fc_ETpot = 51
    integer, parameter :: wc_fc_XS = 52
    integer, parameter :: wc_fc_XG = 53
    integer, parameter :: wc_cur_time = 60
    integer, parameter :: wc_cur_dV = 61
    integer, parameter :: wc_cur_dG = 62
    integer, parameter :: wc_cur_hQ = 63
    integer, parameter :: wc_cur_hS = 64
    integer, parameter :: wc_cur_W = 71
    integer, parameter :: wc_cur_beta = 72
    integer, parameter :: wc_cur_dVeq = 73
    integer, parameter :: wc_last_deltime = 80
    integer, parameter :: wc_last_fXG = 81
    integer, parameter :: wc_last_fXS = 82
    integer, parameter :: wc_last_PQ = 83
    integer, parameter :: wc_last_PV = 84
    integer, parameter :: wc_last_PS = 85
    integer, parameter :: wc_last_ETV = 86
    integer, parameter :: wc_last_ETS = 87
    integer, parameter :: wc_last_ETact = 88
    integer, parameter :: wc_last_fQS = 89
    integer, parameter :: wc_last_fGS = 90
    integer, parameter :: wc_last_Q = 91
    integer, parameter :: wc_last_Qdischarge = 92
    integer, parameter :: wc_last_fXSact = 93
    integer, parameter :: wc_last_P = 94
    integer, parameter :: wc_last_ETPot = 95
    integer, parameter :: wc_hSmin = 96
end module WalrusData

module RRRunoff

  use Conf_fil
  use Conf_arr
  use Network
  use Link
  use NWRW
  use RR_Meteo
  use Boundary
  use NewTables
  use Openwater
  use RRConnectionBifurcationNodes
  use DH_Alloc
  use ReadLib
  use NewTables
  use WalrusData
  use ParallelData, only: NrEvapStations
  use netcdfdata, only : MeteoNetCdfInput
  use Snyder_hydrograph
  use GreenAmptInfiltration
  use Dio_plt_rw, prop_file_unused => prop_file



  ! variables
  implicit none

  ! *** Data RRRunoffNode knopen

  Integer                        LGSI_MaxNrSubAreas
  Integer                        LGSI_MaxInterpLength, LGSI_MaxInterpLengthPlus1
  Integer                        LGSI_MaxDelayLength, LGSI_MaxDelayLengthPlus1
  Logical                        UseWalrus
  Logical                        WalrusUserDefinedWetnessFunctionExists
  Logical                        WalrusUserDefinedBETAFunctionExists
  Logical                        WalrusUserDefinedVEQFunctionExists
  Logical                        WalrusUserDefinedQHFunctionExists
  Logical                        WalrusHSMinTimeTableExists
  Real                           WalrusZeta1, WalrusZeta2
  Double precision               Walrus_min_deltime, Walrus_maxhchange, Walrus_minh, Walrus_max_Pstep, Walrus_Max_Substeps



  DOUBLE PRECISION, PARAMETER :: RTOL = 1.0D-11
  Integer, Parameter  :: WagMod_NUJMX = 50000
  Integer, Parameter  :: WagMod_NUCDMX = 2500  ! 5000
  Integer, Parameter  :: WagMod_MaxNrTimesteps = 50000
  Integer, Parameter  :: WagMod_MaxNrTimestepsSimulation = WagMod_MaxNrTimesteps+1

  Integer, Parameter  :: Walrus_WIMaxLength = 50
  Integer, Parameter  :: Walrus_ERMaxLength = 50
  Integer, Parameter  :: Walrus_VEQMaxLength = 200
  Integer, Parameter  :: Walrus_QHMaxLength = 1000
! todo: sept 2016 analyse Wagmod
!  Integer, Parameter  :: WagMod_MaxNrTimestepsSimulation = 500000 moet so-wie-so om simulaties van 50 jaar aan te kunnen, maar
!  restart info en Wagmod_QG etc moet nog netter gemaakt worden (max. wel op WagMod_MaxNrTimesteps houden)

  Integer                WagMod_ActNrTimestepsSimulation
  Logical                GenerateOldWagmodOutputFiles

  REAL, Pointer, SAVE ::         AREA_RRRunoffNode(:)
  INTEGER, Pointer, SAVE ::      RRRunoff_BND(:), RRRunoff_Nam(:), RRRunoff_CompOption(:)
  INTEGER, Pointer, SAVE ::      RRRunoff_SubIndex(:)

! HBV input parameters
  REAL, Pointer, SAVE ::         HBV_MeltConst(:), HBV_SnowfallTemp(:), HBV_SnowMeltTemp(:), &
                                 HBV_TempAltitudeConstant(:), HBV_FreezEff(:), HBV_FreeWaterFraction(:), &
                                 HBV_Beta(:), HBV_FieldCapacity(:), HBV_EvapFraction(:), &
                                 HBV_KInterflow(:), HBV_KQuickFlow(:), HBV_QuickThreshold(:), &
                                 HBV_MaxPercolation(:), HBV_KBaseFlow(:), HBV_Altitude(:)
  REAL, Pointer, SAVE ::         HBV_InitialDrySnowContent(:), HBV_InitialFreeWaterContent(:), &
                                 HBV_InitialMoisture(:), HBV_InitialQRunoffInmm(:), &
                                 HBV_InitialUpperZoneContent(:), HBV_InitialLowerZoneContent(:)

  REAL, Pointer, SAVE ::         HBV_Rainfall(:),   HBV_SnowFall(:), HBV_Temperature(:), &
                                 HBV_PotEvap(:),    HBV_ActEvap(:), &
                                 HBV_BaseFlow(:),   HBV_InterFlow(:), HBV_QuickFlow(:), &
                                 HBV_Snowmelt(:),   HBV_Refreezing(:), HBV_Infiltration(:), &
                                 HBV_DirectRunoff(:), HBV_Seepage(:), HBV_Percolation(:), HBV_InUpperZone(:)

  REAL, Pointer, SAVE ::         HBV_DrySnowContent(:),   HBV_FreeWaterContent(:), &
                                 HBV_SoilMoisture(:),    HBV_QRunoffInmm(:), &
                                 HBV_UpperZoneContent(:), HBV_LowerZoneContent(:)

  REAL, Pointer, SAVE ::         HBV_DrySnowContent0(:),   HBV_FreeWaterContent0(:), &
                                 HBV_SoilMoisture0(:),   HBV_QRunoffInmm0(:), &
                                 HBV_UpperZoneContent0(:), HBV_LowerZoneContent0(:)

! SCS input parameters
  REAL, Pointer, SAVE ::         SCS_Slope(:), SCS_Length(:)
  Integer, Pointer, SAVE ::      SCS_UHChosen(:), SCS_AMC(:)
  Real   , Pointer, SAVE ::      SCS_CurveNumber(:), SCS_HMSLinResR(:), SCS_HMSC1(:), SCS_HMSC2(:)
  Real, Pointer, SAVE ::         SCS_CN1(:), SCS_CN2(:), SCS_CN3(:)

  REAL, Pointer, SAVE ::         SCS_MaxRetention(:), SCS_Tlag(:), SCS_Tc(:),  &
                                 SCS_Paccum(:), SCS_PExcess(:), &
                                 SCS_Paccum0(:), SCS_PExcess0(:), &
                                 SCS_Storage(:), SCS_Storage0(:), SCS_Rainfall(:), &
                                 SCS_UnitHydComp(:,:), SCS_AvailableRunoff(:,:)

Real   , Pointer, SAVE ::        SCS_HMSLinResInflow(:), SCS_HMSLinResInflowTot(:), SCS_HMSLinResOutflow0(:), SCS_HMSLinResOutflow(:), SCS_HMSLinResContent(:), SCS_HMSLinResContent0(:)

REAL   , Pointer, SAVE :: SCS_Snyder_Cp(:)                      ! Snyder Peaking Factor
REAL   , Pointer, SAVE :: SCS_Snyder_UH_decay_rate(:)           ! Decay rate (1/hour) of exponential part of Snyder UH
REAL   , Pointer, SAVE :: SCS_Snyder_UH_decay_frac(:)           ! fraction of peak flow at which exponential decay starts (0-0.5)
REAL   , Pointer, SAVE :: SCS_Snyder_BF_decay_rate(:)           ! Decay rate (linear or exponential) of Snyder base flow
REAL   , Pointer, SAVE :: SCS_Snyder_BF_STRTQ(:)                ! Base Flow Start Q, Flow rate the base flow starts at (mm/hour)
Integer, Pointer, SAVE :: SCS_Snyder_BF_interpolation_method(:) ! Base Flow interpolation method/decay type. 1 = constant 2 = exponential 3 = linear
  Integer                        MaxTc
! baseflow SCS
  Logical, Pointer, Save ::      SCS_UseBaseFlow(:)
  REAL, Pointer, SAVE ::         SCS_MaxGWCap(:), SCS_InitGWCap(:), SCS_GWRecessionConst(:)
  REAL, Pointer, SAVE ::         SCS_SurfMax(:), SCS_SurfInit(:), SCS_SurfAct(:), SCS_SurfAct0(:)
  REAL, Pointer, SAVE ::         SCS_EvapRD(:)
  REAL, Pointer, SAVE ::         SCS_SubSurfMax (:), SCS_SubSurfInit(:), SCS_SubSurfAct(:) , SCS_SubSurfAct0(:)
  REAL, Pointer, SAVE ::         SCS_PercSS(:)
  REAL, Pointer, SAVE ::         SCS_GWAct(:), SCS_GWAct0(:), SCS_GWOutflow(:)
! GreenAmpt infiltration
  Logical, Pointer, Save ::      SCS_UseGreenAmpt_Infiltration(:)
! input
  double precision, Pointer, SAVE ::         SCS_GreenAmpt_Ksat(:), SCS_GreenAmpt_Psi(:), SCS_GreenAmpt_theta_dmax(:)      ! Ksat in mm/hour, psi in mm, theta_dmax is dimensionless (between 0 and 1)
! computation/output                                                                   ``
  double precision, Pointer, SAVE ::         SCS_GreenAmpt_Lu(:), SCS_GreenAmpt_Kr(:), SCS_GreenAmpt_Tr(:)                 ! Lu in mm, Kr in 1/hour, Tr in hour
  double precision, Pointer, SAVE ::         SCS_GreenAmpt_theta_d(:), SCS_GreenAmpt_theta_du(:)  ! theta_d and theta_du dimensionless, PondingDepth in mm
  double precision, Pointer, SAVE ::         SCS_GreenAmpt_CumRain(:), SCS_GreenAmpt_CumInfiltration(:), SCS_GreenAmpt_InfRate(:), SCS_GreenAmpt_T(:) ! CumRain, CumInfiltration in mm, InfRate in mm/hour, T = recovery time remaining before next event (hour)
  double precision, Pointer, SAVE ::         SCS_GreenAmpt_InfCurrentStep(:)  ! infiltration current timestep in mm


! NAM input parameters
! Parameters
!
!  Integer, Parameter  :: NAM_UnsaSimMaxRecords = 6500
!  Integer, Parameter  :: NAM_UnsaSimMaxRecordsPerSet = 40
!  Integer, Parameter  :: NAM_UnsaSimMaxRecordsPerSoilType = 350
!  Integer                NAMMaxInterpLength
!
! Variables
!
!  Integer             NAM_UNSA_SIM_SoilType (NAM_UnsaSimMaxRecords)
!  Double precision    NAM_UNSA_SIM     (NAM_UnsaSimMaxRecords, 11)
            !  note 1 index different since first column is integer and has to be in separate array
            !  input:  ! 1=root zone depth, 2 = gwl, 3 = RZContent, 4 = MaxCapRis 5= storageCoeff
            !  added:  ! 6= WD_unsafree     7 = WD_unsa_wet 8 = WD_saturated 9 = TSMCeq 10 = Leq 11= GWSDeq

!  Double precision    X_Array(NAM_UnsaSimMaxRecordsPerSet), Y_Array(NAM_UnsaSimMaxRecordsPerSet)

  Double precision, Pointer, SAVE ::         NAM_CatchmentArea(:)
!  Integer, Pointer, SAVE ::                  NAM_SoilType(:)
  Double precision, Pointer, SAVE ::         NAM_InfCap(:), NAM_GWTDfc(:)
  Double precision, Pointer, SAVE ::         NAM_SurfaceLevel(:), NAM_RZBL(:), NAM_GWSBL(:), NAM_U0(:), NAM_L0(:), NAM_GWD0(:)
  Double precision, Pointer, SAVE ::         NAM_CatchmentLength(:), NAM_SurfaceSlope(:), NAM_Manning_n(:), &
                                             NAM_UTOF(:), NAM_CKIF(:), NAM_UTIF(:), NAM_LTIF(:)
  Double precision, Pointer, SAVE ::         NAM_CKFastBF(:), NAM_CKSlowBF(:), NAM_CKGWInflow(:), &
                                             NAM_LTG(:), NAM_TFastBF(:), NAM_TSlowBF(:)

!  Integer, Pointer, SAVE ::                  NAM_StartSoilType(:), NAM_EndSoilType(:)
!  Integer, Pointer, SAVE ::                  NAM_StartRecord(:), NAM_EndRecord(:)
  Double precision, Pointer, SAVE ::         NAM_RD(:), NAM_SubSoilThickness(:)
  Double precision, Pointer, SAVE ::         NAM_SY(:), NAM_SFC(:), NAM_LMax(:), NAM_GWSDrzmax(:), NAM_GWSDssmax(:), NAM_GWSDmax(:)
  Double precision, Pointer, SAVE ::         NAM_SYRZ(:), NAM_SYSS(:)
  Integer, Pointer, SAVE ::                  NAM_CapRisOpt(:), NAM_PercOpt(:)
  Integer, Pointer, SAVE ::                  NAM_CapRisTableStart(:), NAM_CapRisTableEnd(:)
  Integer, Pointer, SAVE ::                  NAM_PercTableStart(:), NAM_PercTableEnd(:)
  Double precision, Pointer, SAVE ::         NAM_CapRisPercTableGWTD(:), NAM_CapRisPercTableCapPerc(:)
  Double precision, Pointer, SAVE ::         NAM_CapRisConst(:), NAM_PercConst(:)
!  Double precision, Pointer, SAVE ::        NAM_UNSASIM_RD(:,:),NAM_UNSASIM_GWTD(:,:),NAM_UNSASIM_RZeqmc(:,:),NAM_UNSASIM_CRpotmax(:,:),NAM_UNSASIM_mu(:,:)
!  Double precision, Pointer, SAVE ::         NAM_WDunsafree(:,:), NAM_WDunsawet(:,:), NAM_WDsat(:,:)
!  Double precision, Pointer, SAVE ::         NAM_TSMCEq(:,:), NAM_Leq(:,:), NAM_GWSDeq(:,:)
  Double precision, Pointer, SAVE ::         NAM_InitialGWDepth(:), NAM_UInitial(:)
  Double precision, Pointer, SAVE ::         NAM_LInitial(:), NAM_GWSDInitial(:)
  Double precision, Pointer, SAVE ::         NAM_GWDepth(:), NAM_U(:)
  Double precision, Pointer, SAVE ::         NAM_L(:), NAM_GWSD(:)
  Double precision, Pointer, SAVE ::         NAM_GWSDrz(:), NAM_GWSDss(:)
  Double precision, Pointer, SAVE ::         NAM_E1(:), NAM_E2(:)
  Double precision, Pointer, SAVE ::         NAM_OF(:), NAM_INF(:), NAM_IF(:)
  Double precision, Pointer, SAVE ::         NAM_DL(:), NAM_G(:), NAM_E2LZS(:), NAM_E2GWSrz(:)
  Double precision, Pointer, SAVE ::         NAM_GWTD(:)
  Double precision, Pointer, SAVE ::         NAM_CR(:), NAM_GWL(:)
  Double precision, Pointer, SAVE ::         NAM_BF(:), NAM_VU(:), NAM_VL(:), NAM_VGWS(:), NAM_AVSoil(:), NAM_GWGWPump(:), NAM_GWSupAct(:), NAM_GWAbsAct(:)
  Double precision, Pointer, SAVE ::         NAM_FastBF(:), NAM_SlowBF(:)
  Double precision, Pointer, SAVE ::         NAM_DLExt(:), NAM_GWExt(:)
  Double precision, Pointer, SAVE ::         NAM_HOutside(:), NAM_GWInflow(:)
  Double precision, Pointer, SAVE ::         NAM_DLGWPump(:), NAM_GWPump(:)
  Double precision, Pointer, SAVE ::         NAM_GWPumpAct(:), NAM_GWPumpShortage(:)
! 26 Oct 2016
  Double precision, Pointer, SAVE ::         NAM_GWTDmax(:), NAM_TSMCEqMin(:), NAM_RZeqmcMin(:), NAM_GWSDEqMin(:)

! old
  REAL, Pointer, SAVE ::         NamSurfaceLevel(:), NamBaseStorageMax(:), &
                                 NAMTof(:), NAMTif(:), NAMTg(:), &
                                 NAMcqof(:), NAMckif(:), NAMck12(:), &
                                 NAMOFSmin(:), NAMBeta(:), Namckbf(:), NAMcklow(:), NAMckinf(:), &
                                 NAMSpecificYield(:), NamGwlBf0(:), NamGwlBf1(:), NamGwlFl1(:), NamGwlThickness(:), &
                                 NamPumpflow(:), NAMPumpFlowAlloc(:), &
                                 NAMU0(:), NAML0(:), NAMIF1(:), NAMIF2(:), NAMOF(:), NAMBF(:)
  Character(Len=CharIdLength), Pointer, SAVE ::   NAMPumpTable(:)
  Integer, Pointer, SAVE ::      NAMRefToGWPump_TTable(:), NAMRefToCapRisTable(:), NAmRefToPercTable(:)
  REAL, Pointer, SAVE ::         NAMSurfStorageFinal(:), NAMRootStorageFinal(:), &
                                 NAMInterflowStorage1Final(:), NAMInterflowStorage2Final(:), NAMOverlandStorageFinal(:), NAMBaseStorageFinal(:)
  Real, Pointer, SAVE ::         NAMGWLInitial(:), NAMGWLFinal(:)
  REAL, Pointer, SAVE ::         NAMSurfStorageInitial(:), NAMRootStorageInitial(:), &
                                 NAMInterflowStorage1Initial(:), NAMInterflowStorage2Initial(:), NAMOverlandStorageInitial(:), NAMBaseStorageInitial(:)
  REAL, Pointer, SAVE ::         NAMOverlandFlow(:), NAMInterFlow(:), NAMBaseFlow(:)
  REAL, Pointer, SAVE ::         NAMFastBaseFlow(:), NAMSlowBaseFlow(:), NAMInfiltrationBaseFlow(:)
  REAL, Pointer, SAVE ::         NAMRainfall(:), NAMPotEvapTot(:), NAMActEvapTot(:), NAMSurfEvap(:), NAMRootEvap(:)
  REAL, Pointer, SAVE ::         NAMInfiltration(:), NAMExcessWater(:), NAMGWRecharge(:), NamCapRis(:), NamMaxCapRis(:)
  REAL, Pointer, SAVE ::         NAMInflowInterflowRsv(:), NAMInflowOverlandflowRsv(:)
  REAL, Pointer, SAVE ::         NAMInflowInterflowRsv2(:)
  Integer, Pointer, SAVE ::      NAMPumpOption(:)

! LGSI input parameters
  double precision, Pointer, SAVE ::  LGSI_Area (:,:), LGSI_As(:,:), LGSI_AreaTot(:), LGSI_Ar(:,:), LGSI_SurfaceLevel(:,:)
  character(len=CharIdLength), Pointer, SAVE ::  LGSI_NameSubArea (:,:)
  double precision, Pointer, SAVE ::  LGSI_Erd (:,:), LGSI_Ersd(:,:)
  double precision, Pointer, SAVE ::  LGSI_Rdr (:,:), LGSI_Ddr (:,:)
  double precision, Pointer, SAVE ::  LGSI_Fas (:,:), LGSI_Asd (:,:), LGSI_Asdr(:,:)
  double precision, Pointer, SAVE ::  LGSI_Tets(:,:), LGSI_Alp (:,:), LGSI_n(:,:), LGSI_Qout(:,:)
  double precision, Pointer, SAVE ::  LGSI_HDif(:), LGSI_C (:)
  double precision, Pointer, SAVE ::  LGSI_InitGwl(:,:), LGSI_SpecifiedInitGwl(:,:)
  Integer, Pointer, SAVE ::           LGSI_NrSubAreas(:)

  Integer, Pointer, SAVE ::           LGSI_Type(:,:)    ! 1 = normal (low), 2= gamma (high)
  double precision, Pointer, SAVE ::  LGSI_NormalMaxSd(:,:)
  double precision, Pointer, SAVE ::  LGSI_NormalMinSd(:,:)
  double precision, Pointer, SAVE ::  LGSI_NormalNb(:,:)
  double precision, Pointer, SAVE ::  LGSI_NormalNusDmax(:,:)
  double precision, Pointer, SAVE ::  LGSI_NormalRex(:,:), LGSI_NormalRov(:,:)
  double precision, Pointer, SAVE ::  LGSI_NormalFp(:,:), LGSI_NormalFow(:,:)
  double precision, Pointer, SAVE ::  LGSI_GammaAg(:,:)
  double precision, Pointer, SAVE ::  LGSI_GammaBg(:,:)
  character(len=CharIdLength), Pointer, SAVE ::  LGSI_MeteoStation(:,:)
  character(len=CharIdLength), Pointer, SAVE ::  LGSI_InitialCondition(:,:)
  character(len=CharIdLength), Pointer, SAVE ::  LGSI_DelayDefinition(:)
  Integer, Pointer, SAVE ::  LGSI_DelayTimestepSize(:)
  Integer, Pointer, SAVE ::  LGSI_DelayNrTimesteps(:)

  Double precision, Pointer, SAVE ::  LGSI_NewGwl(:,:), LGSI_NewVolume(:,:), LGSI_Runoff(:), LGSI_InitVolume(:,:)
  Double precision, Pointer, SAVE ::  LGSI_Drainage(:,:), LGSI_GWFlow(:,:)
  Double precision, Pointer, SAVE ::  LGSI_Precipitation(:,:), LGSI_Evaporation(:,:)
  Double precision, Pointer, SAVE ::  LGSI_PrecipitationReduction(:,:), LGSI_EvaporationReduction(:,:), LGSI_Recharge(:,:)
  Double precision, Pointer, SAVE ::  LGSI_OverlandFlow(:,:), LGSI_QuickFlow(:,:), LGSI_QpDirect(:,:)
  Double precision, Pointer, SAVE ::  LGSI_Interflow(:)
  Double precision, Pointer, SAVE ::  LGSI_Rainfall(:,:), LGSI_PotEvap(:,:), LGSI_ActEvap(:,:)
  Double precision, Pointer, SAVE ::  LGSI_OverLandStorage(:,:), LGSI_GWStorage(:,:)
  Double precision, Pointer, SAVE ::  LGSI_DelayCoefficients(:,:), LGSI_HistoryQtot(:,:), LGSI_HistoryQDelayed(:,:)
  Double precision, Pointer, SAVE ::  LGSI_DefinedDelayCoefficients(:,:)
!
! alles van -10 tot +30 met stappen van 0.1 = 401 waarden
! seepage flux van -30 tot +10, ook 401)
  Double precision, Pointer, SAVE ::  LGSI_InterpGWLevelGWV(:,:,:)       ! -1, 10, 0.05     =11/0.05=220+1=221, daarna nog 20 voor diepere gw standen (+11, +12, .. +30)
  Double precision, Pointer, SAVE ::  LGSI_InterpGWVolume(:,:,:)
  Double precision, Pointer, SAVE ::  LGSI_InterpGWLevelUnsatV(:,:,:)    ! -1, 10, 0.15     =11/0.15 = orde 80, daarna nog 20 voor diepere gw standen (+11, +12, .. +30)
  Double precision, Pointer, SAVE ::  LGSI_InterpUnsatVolume(:,:,:)
  Double precision, Pointer, SAVE ::  LGSI_InterpGWLevelSurfV(:,:,:)     ! -2.01, 4, 0.3    = 6/0.3 = 20        daarna nog 26 voor diepere gw standen (+5 , +6 , .. +30)
  Double precision, Pointer, SAVE ::  LGSI_InterpSurfVolume(:,:,:)
  Double precision, Pointer, SAVE ::  LGSI_InterpGWLevelTotalV(:,:,:)    ! -10, 10, 0.1     = 20/0.1 = 200 +1   daarna nog 20 voor diepere gw standen (+11, +12, .. +30)
  Double precision, Pointer, SAVE ::  LGSI_InterpTotalVolume(:,:,:)
  Double precision, Pointer, SAVE ::  LGSI_InterpGWLevelDrain(:,:,:)     ! -10, 10., 0.05  voor alle flows = 20/0.05 = 400 +1  daarna nog 20 voor diepere gw standen (+11, +12, .. +30)
  Double precision, Pointer, SAVE ::  LGSI_InterpDrainageFlow(:,:,:)
  Double precision, Pointer, SAVE ::  LGSI_InterpOverlandFlow(:,:,:)
  Double precision, Pointer, SAVE ::  LGSI_InterpQuickFlow(:,:,:)
  Double precision, Pointer, SAVE ::  LGSI_InterpGWLevelSeepage(:,:)  ! -10, 10, 0.1     = 20 / 0.1 = 200 +1    en 20 voor diepere gw standen (-11, -12, .. -30)
  Double precision, Pointer, SAVE ::  LGSI_InterpSeepageFlow(:,:)

  Double precision  f05mean, f05sd
  Double precision  f06mean, f06sd
  Double precision  f07alp, f07n
  Double precision  f08mean, f08sd
  Double precision  f09mean, f09sd
  Double precision  f10mean, f10sd, f10Ddr

! Wageningen model input data
  double precision, Pointer, SAVE ::  WagMod_J(:), WagMod_E(:), WagMod_F(:)
  double precision, Pointer, SAVE ::  WagMod_CR(:), WagMod_REPA(:), WagMod_FOS(:)
  double precision, Pointer, SAVE ::  WagMod_SM0(:), WagMod_QG0(:), WagMod_FC(:), Wagmod_SAT(:), WagMod_SEEP(:)
  Integer, Pointer, SAVE ::           WagMod_ActEvapCompOption(:), WagMod_IUHSLOW(:), WagMod_IUHQUICK(:),WagMod_ISUBQUICK(:), WagMod_NTERMUJ(:)
! computed from input
  double precision, Pointer, SAVE ::  WagMod_UJ(:,:)                                           ! UJ for nr nodes * NuJMx (50000)
  double precision, Pointer, SAVE ::  WagMod_RUJREST(:)                                        ! RUJRest for nr nodes
  double precision, Pointer, SAVE ::  WagMod_UCD(:,:)                                          ! UJ for nr nodes * NuCDMx (2011)  !increased naar 5000 Juni2014
!
  character(len=39), Pointer, SAVE ::  WagMod_GROUPNAME(:)                                      ! group name (eg grift, oefraam, dommel0051)
  character(len=39), Pointer, SAVE ::  WagMod_UNITHY(:), WagMod_PLOTFILE(:), WagMod_OPTIMAAL(:) ! file names to get output per node in original format files
  Integer, Pointer, SAVE ::            WagMod_PlotFileUnit(:)                                   ! iounit for plotfile

! Wageningen model, output data (if not specified; at timestep t, for all Wagmod locations)
  double precision, Pointer, SAVE ::  WagMod_PEFCD(:), WagMod_PEFJ(:), WagMod_PEF(:)          ! effective rain for CD model, for J model, total
  double precision, Pointer, SAVE ::  WagMod_P(:), WagMod_ET(:), WagMod_ETG(:), WagMod_ETA(:) ! precipitation, evapotranspiration used, evap input, evap actual
  double precision, Pointer, SAVE ::  WagMod_SM (:), WagMod_SMT1(:)                           ! Soil moisture time t, time t-1
  double precision, Pointer, SAVE ::  WagMod_RoutVol(:), WagMod_RoutVol1(:)                   ! Routing volume time t, time t-1  (fluxes Wagmod_QD and QG for t=T+1,....
  double precision, Pointer, SAVE ::  WagMod_CAP (:)                                          ! cap. rise
  double precision, Pointer, SAVE ::  WagMod_QC (:), WagMod_QD(:,:), WagMod_QG(:,:)           ! total outflow, direct runoff, groundwater flow  in mm
  double precision, Pointer, SAVE ::  WagMod_Runoff (:)                                       ! total outflow (runoff) in m3/s
                                                                                              ! Qg en Qd voor all tijdstappen ivm convolutie J en CD model
  double precision, Pointer, SAVE ::  WagMod_QSNEW(:)                                         ! QS new
  double precision, Pointer, SAVE ::  WagMod_QGT1(:)                                          ! groundwater flow previous timestep
  double precision, Pointer, SAVE ::  WagMod_GSTORE(:), WagMod_GSTORET1(:)                    ! groundwater storage, groundwater storage previous timestep
  double precision, Pointer, SAVE ::  WagMod_DIV(:)                                           ! ratio CD and J model
  double precision, Pointer, SAVE ::  WagMod_FAFX(:)                                          ! FAFX


 !Walrus input parameters
  double precision                    WalrusStartTime
  logical, Pointer, SAVE          ::  Walrus_WA(:), Walrus_VA(:), Walrus_BA(:), Walrus_QA(:), WALRUS_HST(:)
  double precision, Pointer, SAVE ::  Walrus_CW(:), Walrus_CV(:), Walrus_CG(:), WALRUS_CQ(:)
  double precision, Pointer, SAVE ::  Walrus_CS(:), Walrus_CD(:), Walrus_XS(:), Walrus_HSMIN(:)
  double precision, Pointer, SAVE ::  Walrus_AS(:), Walrus_AG(:)
  character(len=20), Pointer, SAVE ::  Walrus_SoilType(:), WALRUS_HSminTable(:)
  integer, pointer, save           ::  Walrus_HSminRefTable(:)

  integer, Pointer, SAVE          ::  Walrus_IntSoilType(:)
  double precision, Pointer, SAVE ::  Walrus_B(:), Walrus_Psi_AE(:), Walrus_THETA_S(:)
  double precision, Pointer, SAVE ::  Walrus_HS0(:), Walrus_HQ0(:), Walrus_DG0(:), Walrus_DV0(:), Walrus_Q0(:)   ! initial conditions
  character(len=20), Pointer, SAVE ::  Walrus_MS(:), Walrus_EvapMS(:), Walrus_FXS(:), Walrus_FXG(:)
  character(len=20), Pointer, SAVE ::  Walrus_WIT(:), Walrus_VIT(:), Walrus_BIT(:), Walrus_QIT(:)
  double precision, Pointer, SAVE ::  Walrus_WIDV_WI(:,:), Walrus_ERDV_ER(:,:), Walrus_VEQDG_VEQ(:,:), Walrus_QH_Q(:,:)
  double precision, Pointer, SAVE ::  Walrus_WIDV_DV(:,:), Walrus_ERDV_DV(:,:), Walrus_VEQDG_DG (:,:), Walrus_QH_H(:,:)
  Integer,          Pointer, SAVE ::  Walrus_WILength(:), Walrus_ERLength(:), Walrus_VEQLength(:), Walrus_QHLength(:)
  double precision, Pointer, SAVE ::  Walrus_Temp1(:), Walrus_Temp2(:)
 !Walrus time parameters
  double precision, Pointer, SAVE ::  Walrus_forcingtime(:,:), Walrus_Precipitation(:,:), Walrus_Epot (:,:)
  double precision, Pointer, SAVE ::  Walrus_FXGValues(:,:), Walrus_FXSValues(:,:)
 !Walrus output
  ! states
  double precision, Pointer, SAVE ::  Walrus_DVCurrent(:), Walrus_DGCurrent(:), &
                                      Walrus_HQCurrent(:), Walrus_HSCurrent(:)
  double precision, Pointer, SAVE ::  Walrus_DVPrevious(:), Walrus_DGPrevious(:), &
                                      Walrus_HQPrevious(:), Walrus_HSPrevious(:)
  ! functions
  double precision, Pointer, SAVE ::  Walrus_WICurrent(:), Walrus_BETACurrent(:), Walrus_DVEQCurrent(:)
  ! results
  double precision, Pointer, SAVE ::  Walrus_P(:), Walrus_ETPot(:), Walrus_ETAct(:)
  double precision, Pointer, SAVE ::  Walrus_FXSAct(:), Walrus_FXSDef(:), Walrus_FXGAct(:)
  double precision, Pointer, SAVE ::  Walrus_Lastdt(:), Walrus_lastFXG(:), Walrus_lastFXS(:)
  double precision, Pointer, SAVE ::  Walrus_LastPQ(:), Walrus_lastPV(:), Walrus_lastPS(:)
  double precision, Pointer, SAVE ::  Walrus_LastETV(:), Walrus_lastETS(:), Walrus_lastETAct(:)
  double precision, Pointer, SAVE ::  Walrus_LastFQS(:), Walrus_lastFGS(:), Walrus_lastQ(:), Walrus_lastQdis(:)


! *** results RRRunoffNode knopen
  ! ***

  REAL, Pointer, SAVE :: RRRunoffNode_Outflow(:)
contains



  Subroutine RRRunoffNode_confAr0

  implicit none

    WalrusUserDefinedWetnessFunctionExists = .false.
    WalrusUserDefinedVEQFunctionExists = .false.
    WalrusUserDefinedBETAFunctionExists = .false.
    WalrusUserDefinedQHFunctionExists = .false.
    WalrusHSMinTimeTableExists = .false.

  Return
  End subroutine RRRunoffNode_confAr0



  Subroutine RRRunoffNode_confAr1

    implicit none
    Integer iOut1
    Logical Success

    iOut1 = ConfFil_get_iOut1()

    LGSI_MaxNrSubAreas   = 2
    LGSI_MaxInterpLength = 401
    LGSI_MaxInterpLengthPlus1 = LGSI_MaxInterpLength + 1
    LGSI_MaxDelayLength  = 120
    LGSI_MaxDelayLengthPlus1 = LGSI_MaxDelayLength + 1
    NRRRunoff = MAX (1, NCRRRunoff) !RRRunoffNode

    If ((NCRRRunoff .GT. 0) .and. (ConfFil_get_iOut1() .gt. 0)) then
      WRITE(IOUT1,*) ' RRRunoffNode Nodes    =',NRRRunoff
      if (NcRRRunoffExternal .gt. 0) WRITE(IOUT1,*) ' RRRunoffNode Nodes Ext   =',NcRRRunoffExternal
      if (NcRRRunoffHBV      .gt. 0) WRITE(IOUT1,*) ' RRRunoffNode Nodes HBV   =',NcRRRunoffHBV
      if (NcRRRunoffSCS      .gt. 0) WRITE(IOUT1,*) ' RRRunoffNode Nodes SCS   =',NcRRRunoffSCS
      if (NcRRRunoffNAM      .gt. 0) WRITE(IOUT1,*) ' RRRunoffNode Nodes D-NAM =',NcRRRunoffNAM
      if (NcRRRunoffLGSI     .gt. 0) WRITE(IOUT1,*) ' RRRunoffNode Nodes LGSI  =',NcRRRunoffLGSI
      if (NcRRRunoffWagmod   .gt. 0) WRITE(IOUT1,*) ' RRRunoffNode Nodes Wagmod=',NcRRRunoffWagmod
      if (NcRRRunoffWalrus   .gt. 0) WRITE(IOUT1,*) ' RRRunoffNode Nodes Walrus=',NcRRRunoffWalrus
    endif

   !*** Input Data RRRunoffNode knopen

    Success = Dh_AllocInit (NRRRunoff, Area_RRRunoffNode, 0E0)
    If (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', ' RRRunoffNode_ConfAr1' )

    Success = Success .and. Dh_AllocInit (NRRRunoff, RRRunoff_BND, 0)
    Success = Success .and. Dh_AllocInit (NRRRunoff, RRRunoff_Nam, 0)
    Success = Success .and. Dh_AllocInit (NRRRunoff, RRRunoff_CompOption, 0)
    Success = Success .and. Dh_AllocInit (NRRRunoff, RRRunoff_SubIndex, 0)
    If (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', ' RRRunoffNode_ConfAr1' )

    if (NcRRRunoffHBV .gt. 0) then
        Success = Success .and. Dh_AllocInit (NcRRRunoffHBV, HBV_MeltConst, 4E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffHBV, HBV_SnowfallTemp, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffHBV, HBV_SnowMeltTemp, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffHBV, HBV_TempAltitudeConstant, 0.6E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffHBV, HBV_FreezEff, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffHBV, HBV_FreeWaterFraction, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffHBV, HBV_Beta, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffHBV, HBV_FieldCapacity, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffHBV, HBV_EvapFraction, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffHBV, HBV_KBaseFlow, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffHBV, HBV_KInterFlow, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffHBV, HBV_KQuickFlow, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffHBV, HBV_QuickThreshold, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffHBV, HBV_MaxPercolation, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffHBV, HBV_Altitude, 0E0)

        Success = Success .and. Dh_AllocInit (NcRRRunoffHBV, HBV_InitialDrySnowContent, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffHBV, HBV_InitialFreeWaterContent, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffHBV, HBV_InitialMoisture, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffHBV, HBV_InitialQRunoffInmm, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffHBV, HBV_InitialUpperZoneContent,0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffHBV, HBV_InitialLowerZoneContent,0E0)

        Success = Success .and. Dh_AllocInit (NcRRRunoffHBV, HBV_DrySnowContent, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffHBV, HBV_FreeWaterContent, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffHBV, HBV_SoilMoisture, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffHBV, HBV_QRunoffInmm, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffHBV, HBV_UpperZoneContent, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffHBV, HBV_LowerZoneContent, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffHBV, HBV_DrySnowContent0, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffHBV, HBV_FreeWaterContent0, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffHBV, HBV_SoilMoisture0, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffHBV, HBV_QRunoffInmm0, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffHBV, HBV_UpperZoneContent0, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffHBV, HBV_LowerZoneContent0, 0E0)

        Success = Success .and. Dh_AllocInit (NcRRRunoffHBV, HBV_Rainfall, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffHBV, HBV_SnowFall, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffHBV, HBV_Temperature, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffHBV, HBV_PotEvap, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffHBV, HBV_ActEvap, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffHBV, HBV_BaseFlow, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffHBV, HBV_InterFlow, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffHBV, HBV_QuickFlow, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffHBV, HBV_Snowmelt, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffHBV, HBV_Refreezing, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffHBV, HBV_Infiltration, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffHBV, HBV_DirectRunoff, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffHBV, HBV_Seepage, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffHBV, HBV_Percolation, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffHBV, HBV_InUpperZone, 0E0)
        if (.not. Success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', ' RRrunoffnode_Confar1' )
    endif

    if (NcRRRunoffSCS .gt. 0) then
        Success = Success .and. Dh_AllocInit (NcRRRunoffSCS, SCS_Slope, 1E-3)
        Success = Success .and. Dh_AllocInit (NcRRRunoffSCS, SCS_Length, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffSCS, SCS_CurveNumber, 40E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffSCS, SCS_CN1, 40E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffSCS, SCS_CN2, 40E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffSCS, SCS_CN3, 40E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffSCS, SCS_UHChosen, 0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffSCS, SCS_AMC, 2)    ! default AMC 2 = average
        Success = Success .and. Dh_AllocInit (NcRRRunoffSCS, SCS_MaxRetention, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffSCS, SCS_HMSLinResR, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffSCS, SCS_HMSC1, 1E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffSCS, SCS_HMSC2, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffSCS, SCS_HMSLinResContent, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffSCS, SCS_HMSLinResContent0, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffSCS, SCS_HMSLinResInflowTot, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffSCS, SCS_HMSLinResOutflow0, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffSCS, SCS_HMSLinResOutflow, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffSCS, SCS_TLag, -999.9E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffSCS, SCS_Tc, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffSCS, SCS_PAccum, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffSCS, SCS_PAccum0, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffSCS, SCS_PExcess, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffSCS, SCS_PExcess0, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffSCS, SCS_Storage, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffSCS, SCS_Storage0, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffSCS, SCS_Rainfall, 0E0)

        Success = Success .and. Dh_AllocInit (NcRRRunoffSCS, SCS_Snyder_Cp, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffSCS, SCS_Snyder_UH_decay_rate, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffSCS, SCS_Snyder_UH_decay_frac, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffSCS, SCS_Snyder_BF_decay_rate, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffSCS, SCS_Snyder_BF_interpolation_method, 0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffSCS, SCS_Snyder_BF_STRTQ, 0E0)
! GreenAmpt
        Success = Success .and. Dh_AllocInit (NcRRRunoffSCS, SCS_UseGreenAmpt_Infiltration, .false.)
        Success = Success .and. Dh_AllocInit (NcRRRunoffSCS, SCS_GreenAmpt_Ksat, 0.D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffSCS, SCS_GreenAmpt_Psi,  0.D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffSCS, SCS_GreenAmpt_Theta_Dmax, 0.D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffSCS, SCS_GreenAmpt_Lu, 0.D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffSCS, SCS_GreenAmpt_Kr, 0.D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffSCS, SCS_GreenAmpt_Tr, 0.D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffSCS, SCS_GreenAmpt_Theta_D, 0.D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffSCS, SCS_GreenAmpt_Theta_Du, 0.D0)
!       Success = Success .and. Dh_AllocInit (NcRRRunoffSCS, SCS_UseGreenAmpt_PondingDepth, 0.D0)    use SCS_Storage instead
        Success = Success .and. Dh_AllocInit (NcRRRunoffSCS, SCS_GreenAmpt_CumRain, 0.D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffSCS, SCS_GreenAmpt_CumInfiltration, 0.00001D0)   ! not at zero, ivm divide by zero
        Success = Success .and. Dh_AllocInit (NcRRRunoffSCS, SCS_GreenAmpt_InfRate, 0.D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffSCS, SCS_GreenAmpt_InfCurrentStep, 0.D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffSCS, SCS_GreenAmpt_T, 0.D0)
! BaseFlow
        Success = Success .and. Dh_AllocInit (NcRRRunoffSCS, SCS_UseBaseFlow, .false.)
        Success = Success .and. Dh_AllocInit (NcRRRunoffSCS, SCS_MaxGWCap,   0E0 )
        Success = Success .and. Dh_AllocInit (NcRRRunoffSCS, SCS_InitGwCap,   0E0 )
        Success = Success .and. Dh_AllocInit (NcRRRunoffSCS, SCS_GwAct,   0E0 )
        Success = Success .and. Dh_AllocInit (NcRRRunoffSCS, SCS_GwAct0,   0E0 )
        Success = Success .and. Dh_AllocInit (NcRRRunoffSCS, SCS_GwOutflow,   0E0 )
        Success = Success .and. Dh_AllocInit (NcRRRunoffSCS, SCS_GwRecessionConst,  0E0  )
        Success = Success .and. Dh_AllocInit (NcRRRunoffSCS, SCS_SubSurfMax,  0E0  )
        Success = Success .and. Dh_AllocInit (NcRRRunoffSCS, SCS_SubSurfInit,  0E0  )
        Success = Success .and. Dh_AllocInit (NcRRRunoffSCS, SCS_SubSurfAct,  0E0  )
        Success = Success .and. Dh_AllocInit (NcRRRunoffSCS, SCS_SubSurfAct0,  0E0  )
        Success = Success .and. Dh_AllocInit (NcRRRunoffSCS, SCS_PercSS,  0E0  )
        Success = Success .and. Dh_AllocInit (NcRRRunoffSCS, SCS_EvapRD,  0E0  )
        Success = Success .and. Dh_AllocInit (NcRRRunoffSCS, SCS_SurfMax,  0E0  )
        Success = Success .and. Dh_AllocInit (NcRRRunoffSCS, SCS_SurfInit,  0E0  )
        Success = Success .and. Dh_AllocInit (NcRRRunoffSCS, SCS_SurfAct,  0E0  )
        Success = Success .and. Dh_AllocInit (NcRRRunoffSCS, SCS_SurfAct0,  0E0  )
        If (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', ' RRRunoffNode_ConfAr1' )
    endif

    if (NcRRRunoffNAM .gt. 0) then
! new
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_CatchmentArea, 0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_InfCap, 0.1D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_GWTDfc, 1.0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_SurfaceLevel, 0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_RZBL, -0.3D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_GWSBL, -10.D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_U0, 0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_L0, 0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_InitialGWDepth, 1D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_LInitial, 1D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_L, 1D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_GWSDInitial, 1D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_GWSD, 1D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_UInitial, 1D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_U, 1D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_GWD0, 1D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_CatchmentLength, 1D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_SurfaceSlope, 0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_Manning_n, 0.02D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_UTof, 5.D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_CKif, 2.D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_UTif, 1.D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_LTif, 5.D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_CKFastBF, 100.D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_CKSlowBF, 200.D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_CKGwInflow, 400.D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_LTG, 5D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_TFastBF, -5.D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_TSlowBF, -10.D0)

!        Success = Success .and. Dh_AllocInit (NAM_UnsaSimMaxRecordsPerSoilType, NAM_StartSoilType, 0)
!        Success = Success .and. Dh_AllocInit (NAM_UnsaSimMaxRecordsPerSoilType, NAM_EndSoilType, 0)
!        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_StartRecord, 0)
!        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_EndRecord, 0)

        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_RD, 0.3D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_SubSoilThickness, -9.7D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_SY, 0.5D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_SYRZ, 0.5D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_SYSS, 0.5D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_Sfc, 0.1D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_CapRisConst, 0.1D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_PercConst, 0.1D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM*30, NAM_CapRisPercTableGWTD, 0.0D0)         ! assume max. 15 rows per table, 2 per node, NcRRRunoffNAM nodes
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM*30, NAM_CapRisPercTableCapPerc, 0.0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_CapRisOpt, 0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_CapRisTableStart, 0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_CapRisTableEnd, 0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_PercOpt, 0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_PercTableStart, 0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_PercTableEnd, 0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_LMAX, 0.5D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_GWSDrzmax, 0.5D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_GWSDssmax, 0.5D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_GWSDmax, 0.5D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_E1, 0.D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_E2, 0.D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_OF, 0.D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_INF, 0.D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_IF, 0.D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_DL, 0.D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_G, 0.D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_GWSD, 0.D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_GWSDrz, 0.D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_GWSDss, 0.D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_E2LZS, 0.D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_E2GWSrz, 0.D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_GWTD, 0.D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_CR, 0.D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_GWL, 0.D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_BF, 0.D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_VU, 0.D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_VL, 0.D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_VGWS, 0.D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_AVSoil, 0.D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_GWGWPump, 0.D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_GWAbsAct, 0.D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_GWSupAct, 0.D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_FastBF, 0.D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_SlowBF, 0.D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_DLExt, 0.D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_GWExt, 0.D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_HOutside, 0.D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_GWInflow, 0.D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_DLGWPump, 0.D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_GWPump, 0.D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_GWPumpAct, 0.D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_GWPumpShortage, 0.D0)
! 26 Oct
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_GWTDMax, 0.D0)
!       Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_TSMCEqMin, 0.D0)
!       Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_RZEqMcMin, 0.D0)
!       Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_GWSDEqMin, 0.D0)
! old
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAMSurfaceLevel, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAMBaseStorageMax, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAMTof, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAMTif, 0.E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAMTg , 0.E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAMcqof, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAMckif, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAMck12, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAMOFSMin, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAMBeta, 0.1E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAMckbf, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAMcklow, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAMckinf, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAMSpecificYield, 0.1E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAMGwlBf0, 800.E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAMGwlBf1, 900.E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAMGwlFl1, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAMGwlThickness, 1000.E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAMPumpOption, 0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAMPumpFlow, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAMPumpFlowAlloc, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAMPumpTable, '')
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAMRefToGWPump_TTable, 0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAMU0, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAML0, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAMIF1, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAMIF2, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAMOF, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAMBF, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAMSurfSTorageFinal, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAMRootSTorageFinal, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAMInterflowSTorage1Final, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAMInterflowSTorage2Final, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAMOverlandSTorageFinal, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAMGwlInitial, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAMGwlFinal, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAMBaseStorageFinal, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAMSurfStorageInitial, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAMRootStorageInitial, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAMInterflowSTorage1Initial, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAMInterflowSTorage2Initial, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAMOverlandSTorageInitial, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAMBaseStorageInitial, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAMOverlandFlow, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAMInterFlow, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAMBaseFlow, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAMFastBaseFlow, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAMSlowBaseFlow, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAMInfiltrationBaseFlow, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAMRainfall, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAMPotEvapTot, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAMActEvapTot, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAMSurfEvap, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAMRootEvap, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAMInfiltration, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAMExcessWater, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAMGWRecharge, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAMCapRis, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAMMaxCapRis, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAMInflowInterflowRsv, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAMInflowInterflowRsv2, 0E0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAMInflowOverlandflowRsv, 0E0)
        If (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', ' RRRunoffNode_ConfAr1' )
    endif

    if (NcRRRunoffLGSI .gt. 0) then
        Success = Success .and. Dh_AllocInit (NcRRRunoffLGSI,LGSI_AREATot, 0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffLGSI,LGSI_MaxNrSubAreas,LGSI_AREA, 0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffLGSI,LGSI_MaxNrSubAreas,LGSI_As, 0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffLGSI,LGSI_MaxNrSubAreas,LGSI_Ar, 0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffLGSI,LGSI_MaxNrSubAreas,LGSI_SurfaceLevel, 0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffLGSI,LGSI_MaxNrSubAreas,LGSI_Erd, 0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffLGSI,LGSI_MaxNrSubAreas,LGSI_Ersd, 0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffLGSI,LGSI_MaxNrSubAreas,LGSI_Rdr, 0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffLGSI,LGSI_MaxNrSubAreas,LGSI_Ddr, 0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffLGSI,LGSI_MaxNrSubAreas,LGSI_Fas, 0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffLGSI,LGSI_MaxNrSubAreas,LGSI_Asd, 0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffLGSI,LGSI_MaxNrSubAreas,LGSI_Asdr, 0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffLGSI,LGSI_MaxNrSubAreas,LGSI_Tets, 0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffLGSI,LGSI_MaxNrSubAreas,LGSI_Alp, 0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffLGSI,LGSI_MaxNrSubAreas,LGSI_n, 0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffLGSI,LGSI_MaxNrSubAreas,LGSI_Qout, 0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffLGSI,LGSI_Hdif, 1D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffLGSI,LGSI_C, 100D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffLGSI,LGSI_MaxNrSubAreas,LGSI_InitGwl, 1D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffLGSI,LGSI_MaxNrSubAreas,LGSI_SpecifiedInitGwl, 1D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffLGSI,LGSI_NrSubAreas, 1)
        Success = Success .and. Dh_AllocInit (NcRRRunoffLGSI,LGSI_MaxNrSubAreas,LGSI_Type, 1)
        Success = Success .and. Dh_AllocInit (NcRRRunoffLGSI,LGSI_MaxNrSubAreas,LGSI_NormalMaxSd, 1D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffLGSI,LGSI_MaxNrSubAreas,LGSI_NormalMinSd, 0.5D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffLGSI,LGSI_MaxNrSubAreas,LGSI_NormalNb, 0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffLGSI,LGSI_MaxNrSubAreas,LGSI_NormalNusDMax, 0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffLGSI,LGSI_MaxNrSubAreas,LGSI_NormalRex, 0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffLGSI,LGSI_MaxNrSubAreas,LGSI_NormalRov, 0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffLGSI,LGSI_MaxNrSubAreas,LGSI_NormalFp, 0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffLGSI,LGSI_MaxNrSubAreas,LGSI_NormalFow, 0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffLGSI,LGSI_MaxNrSubAreas,LGSI_GammaAg, 0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffLGSI,LGSI_MaxNrSubAreas,LGSI_GammaBg, 0D0)

        Success = Success .and. Dh_AllocInit (NcRRRunoffLGSI,LGSI_MaxNrSubAreas,LGSI_NEWGwl, 1D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffLGSI,LGSI_MaxNrSubAreas,LGSI_NEWVolume, 1D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffLGSI,LGSI_MaxNrSubAreas,LGSI_InitVolume, 1D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffLGSI,LGSI_MaxNrSubAreas,LGSI_Drainage, 0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffLGSI,LGSI_MaxNrSubAreas,LGSI_GWFlow, 0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffLGSI,LGSI_MaxNrSubAreas,LGSI_OverlandFlow, 0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffLGSI,LGSI_MaxNrSubAreas,LGSI_QuickFlow, 0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffLGSI,LGSI_MaxNrSubAreas,LGSI_QpDirect, 0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffLGSI,LGSI_MaxNrSubAreas,LGSI_OverlandStorage, 0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffLGSI,LGSI_MaxNrSubAreas,LGSI_GWStorage, 0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffLGSI,LGSI_MaxNrSubAreas,LGSI_Rainfall, 0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffLGSI,LGSI_MaxNrSubAreas,LGSI_PotEvap, 0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffLGSI,LGSI_MaxNrSubAreas,LGSI_ActEvap, 0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffLGSI,LGSI_MaxNrSubAreas,LGSI_Precipitation, 0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffLGSI,LGSI_MaxNrSubAreas,LGSI_Evaporation, 0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffLGSI,LGSI_MaxNrSubAreas,LGSI_PrecipitationReduction, 0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffLGSI,LGSI_MaxNrSubAreas,LGSI_EvaporationReduction, 0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffLGSI,LGSI_MaxNrSubAreas,LGSI_Recharge, 0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffLGSI,LGSI_Interflow, 0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffLGSI,LGSI_Runoff, 0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffLGSI,LGSI_MaxNrSubAreas,LGSI_MeteoStation, '' )
        Success = Success .and. Dh_AllocInit (NcRRRunoffLGSI,LGSI_MaxNrSubAreas,LGSI_InitialCondition, '' )
        Success = Success .and. Dh_AllocInit (NcRRRunoffLGSI,LGSI_DelayDefinition, '' )
        Success = Success .and. Dh_AllocInit (NcRRRunoffLGSI,LGSI_MaxNrSubAreas,LGSI_NameSubArea, '' )
        Success = Success .and. Dh_AllocInit (NcRRRunoffLGSI,LGSI_DelayTimestepSize, 0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffLGSI,LGSI_DelayNrTimesteps, 0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffLGSI,LGSI_MaxDelayLengthPlus1,LGSI_DefinedDelayCoefficients, 0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffLGSI,LGSI_MaxDelayLengthPlus1,LGSI_DelayCoefficients, 0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffLGSI,LGSI_MaxDelayLengthPlus1,LGSI_HistoryQtot, 0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffLGSI,LGSI_MaxDelayLengthPlus1,LGSI_HistoryQDelayed, 0D0)

        Success = Success .and. Dh_AllocInit (LGSI_MaxInterpLengthPlus1,NcRRRunoffLGSI,LGSI_MaxNrSubAreas,LGSI_InterpGWLevelGWV, 0D0)
        Success = Success .and. Dh_AllocInit (LGSI_MaxInterpLengthPlus1,NcRRRunoffLGSI,LGSI_MaxNrSubAreas,LGSI_InterpGWVolume, 0D0)
        Success = Success .and. Dh_AllocInit (LGSI_MaxInterpLengthPlus1,NcRRRunoffLGSI,LGSI_MaxNrSubAreas,LGSI_InterpGWLevelUnsatV, 0D0)
        Success = Success .and. Dh_AllocInit (LGSI_MaxInterpLengthPlus1,NcRRRunoffLGSI,LGSI_MaxNrSubAreas,LGSI_InterpUnsatVolume, 0D0)
        Success = Success .and. Dh_AllocInit (LGSI_MaxInterpLengthPlus1,NcRRRunoffLGSI,LGSI_MaxNrSubAreas,LGSI_InterpGWLevelSurfV, 0D0)
        Success = Success .and. Dh_AllocInit (LGSI_MaxInterpLengthPlus1,NcRRRunoffLGSI,LGSI_MaxNrSubAreas,LGSI_InterpSurfVolume, 0D0)
        Success = Success .and. Dh_AllocInit (LGSI_MaxInterpLengthPlus1,NcRRRunoffLGSI,LGSI_MaxNrSubAreas,LGSI_InterpGWLevelTotalV, 0D0)
        Success = Success .and. Dh_AllocInit (LGSI_MaxInterpLengthPlus1,NcRRRunoffLGSI,LGSI_MaxNrSubAreas,LGSI_InterpTotalVolume, 0D0)
        Success = Success .and. Dh_AllocInit (LGSI_MaxInterpLengthPlus1,NcRRRunoffLGSI,LGSI_MaxNrSubAreas,LGSI_InterpGWLevelDrain, 0D0)
        Success = Success .and. Dh_AllocInit (LGSI_MaxInterpLengthPlus1,NcRRRunoffLGSI,LGSI_MaxNrSubAreas,LGSI_InterpDrainageFlow, 0D0)
        Success = Success .and. Dh_AllocInit (LGSI_MaxInterpLengthPlus1,NcRRRunoffLGSI,LGSI_MaxNrSubAreas,LGSI_InterpOverlandFlow, 0D0)
        Success = Success .and. Dh_AllocInit (LGSI_MaxInterpLengthPlus1,NcRRRunoffLGSI,LGSI_MaxNrSubAreas,LGSI_InterpQuickFlow, 0D0)
        Success = Success .and. Dh_AllocInit (LGSI_MaxInterpLengthPlus1,NcRRRunoffLGSI,LGSI_InterpGWLevelSeepage, 0D0)
        Success = Success .and. Dh_AllocInit (LGSI_MaxInterpLengthPlus1,NcRRRunoffLGSI,LGSI_InterpSeepageFlow, 0D0)
        If (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', ' RRRunoffNode_ConfAr1' )
    endif

    if (NcRRRunoffWagMod .gt. 0) then
        Success = Success .and. Dh_AllocInit (NcRRRunoffWagMod,WagMod_J, 20000.0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWagMod,WagMod_E, 350.0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWagMod,WagMod_F, 40.0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWagMod,WagMod_CR, 2.0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWagMod,WagMod_REPA, 0.05D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWagMod,WagMod_FOS, 0.15D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWagMod,WagMod_SM0, 40.0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWagMod,WagMod_QG0, 0.0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWagMod,WagMod_FC,  75.0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWagMod,WagMod_SAT, 150.0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWagMod,WagMod_SEEP, 0.0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWagMod,WagMod_ActEvapCompOption,0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWagMod,WagMod_IUHSLOW, 20000)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWagMod,WagMod_IUHQuick, 48)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWagMod,WagMod_ISubQuick, 20)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWagMod,WagMod_NTermUJ, 30)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWagMod,WagMod_Groupname, '')   ! group name = id or node name
        Success = Success .and. Dh_AllocInit (NcRRRunoffWagMod,WagMod_UNITHY, '')      ! .00O file
        Success = Success .and. Dh_AllocInit (NcRRRunoffWagMod,WagMod_PLOTFILE, '')    ! .00P file
        Success = Success .and. Dh_AllocInit (NcRRRunoffWagMod,WagMod_OPTIMAAL, '')    ! .00U file
        Success = Success .and. Dh_AllocInit (NcRRRunoffWagMod,WagMod_PlotFileUnit, 0)    ! unit nr .00P file

        Success = Success .and. Dh_AllocInit (NcRRRunoffWagmod,Wagmod_NUJMX , WagMod_UJ , 0.0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWagmod,Wagmod_NUCDMX, WagMod_UCD, 0.0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWagMod,WagMod_RUJREST, 0.0D0)

        Success = Success .and. Dh_AllocInit (NcRRRunoffWagMod, WagMod_PEFCD, 0.0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWagMod, WagMod_PEFJ , 0.0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWagMod, WagMod_PEF  , 0.0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWagMod, WagMod_P    , 0.0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWagMod, WagMod_ET   , 0.0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWagMod, WagMod_ETG  , 0.0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWagMod, WagMod_ETA  , 0.0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWagMod, WagMod_SM   , 0.0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWagMod, WagMod_SMT1 , 0.0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWagMod, WagMod_RoutVol , 0.0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWagMod, WagMod_RoutVol1, 0.0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWagMod, WagMod_CAP  , 0.0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWagMod, WagMod_QC   , 0.0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWagMod, WagMod_Runoff, 0.0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWagmod, Wagmod_MaxNrTimesteps+1, WagMod_QD   , 0.0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWagmod, Wagmod_MaxNrTimesteps+1, WagMod_QG   , 0.0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWagMod, WagMod_QSNEW , 0.0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWagMod, WagMod_QGT1 , 0.0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWagMod, WagMod_GSTORE, 0.0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWagMod, WagMod_GSTORET1, 0.0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWagMod, WagMod_DIV  , 0.0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWagMod, WagMod_FAFX, 0.0D0)
        If (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', ' RRRunoffNode_ConfAr1' )
    endif

    if (NcRRRunoffWalrus .gt. 0) then
        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,Walrus_WA, .true.)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,Walrus_CW, 200.0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,Walrus_CV, 4.0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,Walrus_CG, 5000000.D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,Walrus_CQ, 10.D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,Walrus_VA, .true.)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,Walrus_BA, .true.)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,Walrus_QA, .true.)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,Walrus_HST, .false.)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,Walrus_CS, 4.0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,Walrus_CD, 1500.0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,Walrus_XS, 1.5D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,Walrus_HSMIN, 0.0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,Walrus_AS, 0.1D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,Walrus_AG, 0.9D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,Walrus_HSminTable, '')
        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,Walrus_HSminRefTable, 0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,Walrus_SoilType, 'loamy_sand')
        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,Walrus_IntSoilType, 2)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,Walrus_B, 4.38D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,Walrus_PSI_AE,90.D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,Walrus_Theta_S, 0.41D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,Walrus_HS0, 500.D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,Walrus_HQ0,  1.0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,Walrus_DG0, 5000.0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,Walrus_DV0,   0.0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,Walrus_Q0,    4.0D0)

        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,Walrus_MS ,   '')
        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,Walrus_EVAPMS,'')
        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,Walrus_FXS,   '')
        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,Walrus_FXG,   '')

        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,Walrus_WIT, '')
        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,Walrus_VIT, '')
        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,Walrus_BIT, '')
        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,Walrus_QIT, '')

        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,Walrus_WIMaxLength,Walrus_WIDV_WI,  0.D00)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,Walrus_WIMaxLength,Walrus_WIDV_DV,  0.D00)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,Walrus_ERmaxLength,Walrus_ERDV_ER,  0.D00)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,Walrus_ERmaxLength,Walrus_ERDV_DV,  0.D00)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,Walrus_VEQMaxLength,Walrus_VEQDG_VEQ, 0.D00)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,Walrus_VEQMaxLength,Walrus_VEQDG_DG, 0.D00)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,Walrus_QHMaxLength,Walrus_QH_Q,  0.D00)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,Walrus_QHMaxLength,Walrus_QH_H,  0.D00)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,Walrus_WILength,  0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,Walrus_ERLength,  0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,Walrus_VEQLength,  0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,Walrus_QHLength,  0)

        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,Walrus_DVCurrent, 0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,Walrus_DGCurrent, 0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,Walrus_HQCurrent, 0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,Walrus_HSCurrent, 0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,Walrus_DVPrevious, 0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,Walrus_DGPrevious, 0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,Walrus_HQPrevious, 0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,Walrus_HSPrevious, 0D0)

        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,Walrus_WICurrent, 0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,Walrus_BETACurrent, 0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,Walrus_DVEQCurrent, 0D0)

        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,Walrus_P,  0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,Walrus_ETPot, 0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,Walrus_ETAct, 0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,Walrus_FXSAct, 0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,Walrus_FXSDef, 0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,Walrus_FXGAct, 0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,Walrus_P,  0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,Walrus_LastDt,  0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,Walrus_lastFXG,  0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,Walrus_lastFXS,  0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,Walrus_lastPQ,  0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,Walrus_lastPV,  0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,Walrus_lastPS,  0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,Walrus_lastETV,  0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,Walrus_lastETS,  0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,Walrus_lastETAct,  0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,Walrus_lastFQS,  0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,Walrus_lastFGS,  0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,Walrus_lastQ,  0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,Walrus_lastQdis,  0D0)

        If (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', ' RRRunoffNode_ConfAr1' )
    endif

  Return
  End subroutine RRRunoffNode_confAr1


  Subroutine RRRunoffNodeOutput_Confar (Nevnt)

    implicit none
    Integer Nevnt

!RRRunoffNode
  Return
  End subroutine RRRunoffNodeOutput_Confar


  SUBROUTINE RRRunoffNode_CONFAR3
    ! *** Output Data RRRunoffNode knopen
    use Snyder_hydrograph
    implicit none

    Logical Success

    Success = Dh_AllocInit (NRRRunoff, RRRunoffNode_Outflow, 0E0)
    if (SHG_set%size > 0) allocate(SHG_set%SHG(SHG_SET%size))
    If (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', ' RRRunoffNode_ConfAr3' )
    Return
  End subroutine RRRunoffNode_confar3




  Subroutine RRRunoffNode_readAscii(infile1)

    implicit none

    Integer :: RetVal
    Integer(4)      infile1
    Integer         teller, i, inode, j, IRRRunoff, IRRRunoff2, IRRRunoffSub, IRRRunoffRef, index, inod, nhlp, iout1, iecode, idebug, len_groupname, NrSeconds, NrValues
    Character(CharIdLength)   name, id, NodeId
    Character(len=9999) string, TempString, string1, String0
    Integer         lenString, lenString1
    Character(len=1)    KChar
    Logical         allow, found, endfil, occurs, occurs2, Err969, Err969all, TabYesNo
    Integer         TableNr, NrColumns
    Integer         LengthTable, ipos, ipos1, ipos2

!    Parameter     (NHLP=36)
!   Parameter     (NHLP=121)   ! must be LGSI_MaxDelayLengthPlus1
!   Parameter     (NHLP = max (LGSI_MaxDelayLengthPlus1, Walrus_WIMaxLength, Walrus_ERMaxLength, Walrus_VEQMaxLength, Walrus_QHMaxLength) )
!   Parameter     (NHLP = max (LGSI_MaxDelayLengthPlus1, Walrus_WIMaxLength, Walrus_ERMaxLength, Walrus_VEQMaxLength, Walrus_QHMaxLength) )
    Parameter     (NHLP = 1000)
    Integer       IDUM(NHLP)
    Real          RDUM(NHLP), Checksum
    Character(CharIdLength) CDUM(NHLP), Tablename
    Logical  LevelError
    Logical, Pointer :: AlreadyRead(:)
    Integer, Pointer :: ReferenceToDefinition(:), ReferenceToDefinition2(:)

    Character(Len=CharIdLength), pointer, save ::  SnowDef(:), SoilDef(:), FlowDef(:), HIniDef(:), &
                                                   NAM_SoilParameterDefinition(:), NAM_LevelAndDepthDefinition(:), &
                                                   NAM_SurfaceRunoffDefinition(:), NAM_BaseFlowDefinition(:), &
                                                   NAM_GroundwaterForcingDefinition(:)
    Character(Len=CharIdLength), pointer, save ::  LGSTDef(:), LGSTDef2(:)
    Logical  Success

    Character(Len=CharIdLength)  FileName
    Character(Len=1000000)       KeepBufString
    Integer                      IoUnit, iCount

    Integer  NrHbvNodes, NrExtNodes, NrScsNodes, NrNAMNodes, NrLGSINodes, NrWageningenNodes, NrWalrusNodes
    ! HBV
    Real     MeltConstDum, SnowFallTempDum, SnowMeltTempDum, TempAltConstDum, &
             FreezEffDum, FreeWaterFractionDum, &
             BetaDum, FieldCapacityDum, EvapFractionDum, &
             KBaseFlowDum, KInterFlowDum, KQuickFlowDum, MaxPercolationDum, QuickFlowThresholdDum, &
             InitialDrySnowDum, InitialFreeWaterDum, InitialSoilMoistureDum, &
             InitialUpperZoneDum, InitialLowerZoneDum
    !NAM
    Real     infcapdum, sldum, rzbldum, gwsbldum, u0dum, l0dum, gwd0dum, &
             cldum, ssdum, mandum, utofdum, ckifdum, utifdum, ltifdum, &
             ckfastbfdum, ckslowbfdum, ckgwinfdum, ltpdum, tfastbfdum, tslowbfdum
    Real     FieldCapDum, SpecYieldRZDum, SpecYieldSSDum
    Integer  podum, CapOptDum, PercOptDum
    Real     pudum, CapCDum, PercCDum
    Integer  NrPumpTimeTablesNeeded, INAM, istart, iend, istart2, iend2, IndexNextTableStart
    Character(Len=CharIdLength) ptDum, captDum, perctDum
             ! already defined:   BetaDum

    Integer  LGSITypeDum
    Real     LGSIMaxSDdum, LGSIMinSDdum, LGSINbDum, LGSINusdMaxDum, LGSIRexDum, LGSIFpDum, LGSIFowDum, LGSIAgDum, LGSIBgDum
    Real     LGSIRovDum

    Double precision ddum, eps
    Integer          D_ifreal


    Success = Dh_AllocInit (NcRRRUnoffHBV, SnowDef, SoilDef, FlowDef, ' ')
    Success = Dh_AllocInit (NcRRRunoffLGSI, LGSTDef, LGSTDef2, ' ')
    Success = Success .and. Dh_AllocInit (NcRRRUnoffHBV, HiniDef, ' ')
    Success = Success .and. Dh_AllocInit (NRRRunoff, AlreadyRead, .false.)
    Success = Success .and. Dh_AllocInit (NRRRunoff, ReferenceToDefinition,ReferenceToDefinition2, 0)
    Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_SoilParameterDefinition, ' ')
    Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_LevelAndDepthDefinition, ' ')
    Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_SurfaceRunoffDefinition, ' ')
    Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_BaseFlowDefinition, ' ')
    Success = Success .and. Dh_AllocInit (NcRRRunoffNAM, NAM_GroundwaterForcingDefinition, ' ')
    If (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', ' RRRunoffNode_ReadAscii' )

    iDebug = ConfFil_get_iDebug()

!   AlreadyRead = .false.

    allow = .false.
    found = .false.
    iOut1 = ConfFil_get_iOut1()
    Err969 = .false.
    LevelError = .false.

! *********************************************************************
! ***  If CleanRRFiles, also write cleaned input
! *********************************************************************
   if (CleanRRFiles) then
        FileName = ConfFil_get_namFil(44)
        FileName(1:) = Filename(1:Len_trim(FileName)) // '_cleaned'
        Call Openfl (iounit, FileName,1,3)  ! Sacrmnto.3b   ! maybe already existing (updating Sacr. input, append mode)
        Call ErrMsgStandard (999, 1, ' Cleaning Sacrmnto.3b for RRrunoff input to file', FileName)
   endif
! *********************************************************************
! Read RRRunoffNode.3B file   (=Sacrmnto.3b file!)
! *********************************************************************
    call SetMessage(LEVEL_INFO, 'Read RRRunoffNode.3b file')
    endfil = .false.
    teller = 0
    RetVal = 0
    NrExtNodes = 0
    NrHBVNodes = 0
    NrSCSNodes = 0
    NrNAMNodes = 0
    NrLGSINodes = 0
    NrWageningenNodes = 0
    NrWalrusNodes = 0
    Call SKPCOM (INfile1, ENDFIL,'ODS')
    Do while (.not. endfil)
       READ(Infile1,'(A1000)',END=21,ERR=150,IOSTAT=IECODE)  STRING
!      Write(*,*) 'Read from Sacrmnto.3b file ', String(1:999)
! skip regel als hij niet begint met juist keyword (RRRunoffNode): EXTR, HBV or SCS, LGSI, WAGM
       IF (STRING(1:4) .EQ. 'EXTR' .or. STRING(1:4) .eq. 'HBV ' .or. String(1:4) .eq. 'SCS ' .or. String(1:4) .eq. 'NAM '  &
               .or. STRING(1:4) .eq. 'LGSI' .or. STRING(1:4) .eq. 'WAGM' .or. STRING(1:4) .eq. 'WALR') Then
! RRRunoffNode node id
        Retval = RetVal + GetVAR2 (STRING,' id ',1,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                      ID, RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
        index = 0
        call fndNd2(index, id)
        if (index .gt. 0) then
         Inod = index
         IRRRunoff = EiNode(inod,2)
         if ( (String(1:4) .eq. 'EXTR' .and. EiNode(inod,3) .eq. 18)  .or. &
              (String(1:4) .eq. 'HBV ' .and. EiNode(inod,3) .eq. 19)  .or. &
              (String(1:4) .eq. 'SCS ' .and. EiNode(inod,3) .eq. 20)  .or. &
              (String(1:4) .eq. 'LGSI' .and. EiNode(inod,3) .eq. 22)  .or. &
              (String(1:4) .eq. 'WAGM' .and. EiNode(inod,3) .eq. 23)  .or. &
              (String(1:4) .eq. 'WALR' .and. EiNode(inod,3) .eq. 23)  .or. &
              (String(1:4) .eq. 'NAM ' .and. EiNode(inod,3) .eq. 31) ) then  ! record matches with node type
          if (AlreadyRead(IRRRunoff)) then
            call SetMessage(LEVEL_ERROR, 'Data for RRRunoffNode node '//id(1:Len_Trim(id))//' double in datafile RRRunoffNode.3B')
            LevelError = .true.
          else
           ! cleaning RR files
           If (CleanRRFiles) write(Iounit,'(A)') String (1:len_trim(String))

           teller = teller + 1
           AlreadyRead(IRRRunoff) = .true.
           if (String(1:4) .eq. 'EXTR') then
              NrExtNodes = NrExtNodes + 1
              RRRunoff_CompOption(IRRRunoff) = 0
              RRRunoff_SubIndex(IRRRunoff) = NrExtNodes
           elseif (String(1:4) .eq. 'HBV ') then
              NrHbvNodes = NrHbvNodes + 1
              RRRunoff_CompOption(IRRRunoff) = 1
              RRRunoff_SubIndex(IRRRunoff) = NrHbvNodes
           elseif (String(1:4) .eq. 'SCS ') then
              NrScsNodes = NrScsNodes + 1
              RRRunoff_CompOption(IRRRunoff) = 2
              RRRunoff_SubIndex(IRRRunoff) = NrSCSNodes
           elseif (String(1:4) .eq. 'NAM ') then
              NrNAMNodes = NrNAMNodes + 1
              RRRunoff_CompOption(IRRRunoff) = 3
              RRRunoff_SubIndex(IRRRunoff) = NrNAMNodes
           elseif (String(1:4) .eq. 'LGSI') then
              NrLGSINodes = NrLGSINodes + 1
              RRRunoff_CompOption(IRRRunoff) = 4
              RRRunoff_SubIndex(IRRRunoff) = NrLGSINodes
           elseif (String(1:4) .eq. 'WAGM') then
              NrWageningenNodes = NrWageningenNodes + 1
              RRRunoff_CompOption(IRRRunoff) = 5
              RRRunoff_SubIndex(IRRRunoff) = NrWageningenNodes
           elseif (String(1:4) .eq. 'WALR') then
              NrWalrusNodes = NrWalrusNodes + 1
              RRRunoff_CompOption(IRRRunoff) = 6
              RRRunoff_SubIndex(IRRRunoff) = NrWalrusNodes
            ! read tot string 'walr' is gevonden
 101          continue
              lenString = len_trim (String)
              if (String(lenString-3:lenstring) .ne. 'walr') then
                  READ(Infile1,'(A1000)',END=21,ERR=150,IOSTAT=IECODE)  String1
                  ! cleaning RR files
                  If (CleanRRFiles) write(Iounit,'(A)') String1 (1:len_trim(String1))
                  lenString1 = len_trim (String1)
                  String = String(1:lenString) // " " // String1 (1:lenString1)
                  goto 101
              endif
           endif
! subindex for all subtypes EXTR, HBV, SCS, NAM, LGSI, WagMod/Walrus, to be used for subtype specific arrays
           IRRRunoffSub = RRRunoff_SubIndex(IRRRunoff)

! for LGSI, 1 or 2 areas may be defined using keyword na
           allow = .false.
           if (RRRunoff_CompOption(IRRRunoff) .ne. 4) allow = .true.
           found = .false.
           Retval = RetVal + GetVAR2(STRING,' na ',3,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                        ID, RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
           if (found) LGSI_NrSubAreas(IRRRunoffSub) = IDUM(1)
! RRRunoffNode area, keyword ar
           allow = .false.
           found = .false.
           if (RRRunoff_CompOption(IRRRunoff) .ne. 4) then   ! not LGSI
              Retval = RetVal + GetVAR2(STRING,' ar ',2,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                          ID, RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
! nav issue 24338: oppervlak > 0
              AREA_RRRunoffNode(IRRRunoff) = Max(0.001, RDUM(1))
           elseif (RRRunoff_CompOption(IRRRunoff) .eq. 4) then  ! LGSI
              RetVal = RetVal + GetVRS2(STRING,' ar ',2,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file', &
                                        IOUT1, CDUM(1), RDUM(1), IDUM(1), LGSI_NrSubAreas(IRRRunoffSub), IflRtn)
              if (LGSI_NrSubAreas(IRRRUnoffSub) .eq. 1) then  ! LGSI with 1 subarea only
                 LGSI_area(IrrRunoffSub,1) = RDUM(1)
                 LGSI_areaTot(IrrRunoffSub) = RDUM(1)
                 AREA_RRRunoffNode(IRRRunoff) = Min(0.001, RDUM(1))
              elseif (LGSI_NrSubAreas(IRRRUnoffSub) .eq. 2) then  ! LGSI with 2 subareas
                 LGSI_area(IrrRunoffSub,1) = RDUM(1)
                 LGSI_area(IrrRunoffSub,2) = RDUM(2)
                 LGSI_areaTot(IrrRunoffSub) = RDUM(1) + RDUM(2)
                 AREA_RRRunoffNode(IRRRunoff) = Max(0.001, RDUM(1) + RDum(2) )
              endif
              allow = .true.
              Retval = RetVal + GetVAR2(STRING,' sl ',2,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                                        CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
              if (found) then
                 RetVal = RetVal + GetVRS2(STRING,' sl ',2,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file', &
                                           IOUT1, CDUM(1), RDUM(1), IDUM(1), LGSI_NrSubAreas(IRRRunoffSub), IflRtn)
                 if (LGSI_NrSubAreas(IRRRUnoffSub) .eq. 1) then  ! LGSI with 1 subarea only
                    LGSI_SurfaceLevel(IrrRunoffSub,1) = RDUM(1)
                 elseif (LGSI_NrSubAreas(IRRRUnoffSub) .eq. 2) then  ! LGSI with 2 subareas
                    LGSI_SurfaceLevel(IrrRunoffSub,1) = RDUM(1)
                    LGSI_SurfaceLevel(IrrRunoffSub,2) = RDUM(2)
                 endif
              else
                 LGSI_SurfaceLevel(IrrRunoffSub,1) = 0.0
                 LGSI_SurfaceLevel(IrrRunoffSub,2) = 0.0
              endif
           endif
! RRRunoffNode Runoff station definition, may not be missing if runoff option=0
           allow = .false.
           found = .false.
           if (RRRunoff_CompOption(IRRRunoff) .ne. 0) then
              allow = .true.
           endif
           Retval = RetVal + GetVAR2(STRING,' rs ',1,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                        CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
           NAMRunoff(index) = CDUM(1)
! RRRunoffNode Meteo station definition, may be missing if runoff option =0, may be 2 stations if LGSI
           allow = .false.
           found = .false.
           if (RRRunoff_CompOption(IRRRunoff) .eq. 0) then
               allow = .true.
           endif
           Retval = RetVal + GetVAR2(STRING,' ms ',1,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                        CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
           if (found) NAMMet(index) = CDUM(1)

! evaporation time series in rainfall file
           allow = .true.
           found = .false.
           Retval = RetVal + GetVAR2(STRING,' evapms ',1,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                        CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
           if (found) then
               NAMEVAP(index) = CDUM(1)
                if (NAMEVAP(index) .eq. '') then
                  call ErrMsgStandard (977, 0, ' For node ', ID_Nod(inod))
                  call ErrMsgStandard (977, 0, ' Warning reading Walrus data ', ' No Evaporation station defined from BUI file. Will be taken from EVAP file')
               endif
           else
              if ((String(1:4) .eq. 'WAGM' .and. EiNode(inod,3) .eq. 23) .or. &
                  (String(1:4) .eq. 'WALR' .and. EiNode(inod,3) .eq. 23)) then
              ! Wagmod/Walrus evaporation station not defined
                 found = .false.
                 Retval = RetVal + GetVAR2(STRING,' msevap ',1,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                                           CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
                 if (found) then
                    NAMEVAP(index) = CDUM(1)
                    if (NAMEVAP(index) .eq. '') then
                       call ErrMsgStandard (977, 0, ' For node ', ID_Nod(inod))
                       call ErrMsgStandard (977, 0, ' Warning reading Walrus data ', ' No Evaporation station defined from BUI file. Will be taken from EVAP file')
                    endif
                 else
!                    if (String(1:4) .eq. 'WAGM' .and. EiNode(inod,3) .eq. 23) RetVal = 0
                    call ErrMsgStandard (977, 0, ' For node ', ID_Nod(inod))
                    call ErrMsgStandard (977, 0, ' Warning reading Walrus/Wagmod data ', ' No evaporation station defined')
                 endif
              endif
           endif

! groundwater and surface water abstractions in rainfall file
           found = .false.
           Retval = RetVal + GetVAR2(STRING,' fxg ',1,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                        CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
           if (found) then
              NAMFXG (index) = CDUM(1)
              if (NAMFXG(index) .eq. '') then
                 call ErrMsgStandard (977, 0, ' For node ', ID_Nod(inod))
                 call ErrMsgStandard (977, 0, ' Warning reading Walrus data ', ' No FXG station defined, FXG=0 will be assumed')
              endif
           elseif (String(1:4) .eq. 'WALR' .and. EiNode(inod,3) .eq. 23) then
              ! Walrus FXG not defined
              call ErrMsgStandard (977, 0, ' For node ', ID_Nod(inod))
              call ErrMsgStandard (977, 0, ' Warning reading Walrus data ', ' No FXG station defined, FXG=0 will be assumed')
           endif
           found = .false.
           Retval = RetVal + GetVAR2(STRING,' fxs ',1,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                        CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
           if (found) then
              NAMFXS (index) = CDUM(1)
              if (NAMFXS(index) .eq. '') then
                 call ErrMsgStandard (977, 0, ' For node ', ID_Nod(inod))
                 call ErrMsgStandard (977, 0, ' Warning reading Walrus data ', ' No FXS station defined, FXS=0 will be assumed')
              endif
           elseif (String(1:4) .eq. 'WALR' .and. EiNode(inod,3) .eq. 23) then
              ! Walrus FXS not defined
              call ErrMsgStandard (977, 0, ' For node ', ID_Nod(inod))
              call ErrMsgStandard (977, 0, ' Warning reading Walrus data ', ' No FXS station defined, FXS=0 will be assumed')
           endif

           allow = .false.
           found = .false.
           if (RRRunoff_CompOption(IRRRunoff) .eq. 4) then  ! LGSI
              RetVal = RetVal + GetVRS2(STRING,' ms ',1,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file', &
                                        IOUT1, CDUM(1), RDUM(1), IDUM(1), LGSI_NrSubAreas(IRRRunoffSub), IflRtn)
              Do i=1,LGSI_NrSubAreas(IRRRUnoffSub)
                 LGSI_MeteoStation(IrrRunoffSub,i) = CDUM(i)
              enddo
           endif

! areal adjustment factor rainfall on node, maybe missing,
           allow = .true.
           Retval = RetVal + GetVAR2(STRING,' aaf ',2,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                        CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
           if (found) AAFNodeRainfall(index) = max(0.0, RDUM(1))    ! AAF >= 0
! RRRunoffNode HBV parameters, if HBV selected they should be present
           if (String(1:4) .eq. 'HBV ') then
            allow = .true.
            found = .false.
            if (RRRunoff_CompOption(IRRRunoff) .eq. 1) then
                allow = .false.
            endif
            Retval = RetVal + GetVAR2(STRING,' sl ',2,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                         CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
            HBV_Altitude(IRRRunoffSub) = RDUM(1)
            Retval = RetVal + GetVAR2(STRING,' snow ',1,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                         CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
            SnowDef(IRRRunoffSub) = CDUM(1)
            Retval = RetVal + GetVAR2(STRING,' soil ',1,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                         CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
            SoilDef(IRRRunoffSub) = CDUM(1)
            Retval = RetVal + GetVAR2(STRING,' flow ',1,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                         CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
            FlowDef(IRRRunoffSub) = CDUM(1)
            Retval = RetVal + GetVAR2(STRING,' hini ',1,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                         CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
            HIniDef(IRRRunoffSub) = CDUM(1)
            Retval = RetVal + GetVAR2(STRING,' ts ',1,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                         CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
            NamTemperature(index) = CDUM(1)
           elseif (String(1:4) .eq. 'SCS ') then
            ! RRRunoffNode SCS parameters, if SCS selected they should be present
            allow = .true.
            found = .false.
            if (RRRunoff_CompOption(IRRRunoff) .eq. 2) then
               allow = .false.
            endif

            Retval = RetVal + GetVAR2(STRING,' cn ',2,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
               CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
            SCS_CurveNumber(IRRRunoffSub) = RDUM(1)
            SCS_CN2(IRRRunoffSub) = IDUM(1)
            if (SCS_CurveNumber(IRRRunoffSub) .le. 0 .or. SCS_CurveNumber(IRRRunoffSub) .gt. 100) then
               NodeId = Id_Nod(inod)
               String = ' '
               String = NodeId(1:Len_Trim(NodeId))
               call ErrMsgStandard (977, 0, ' Curve Number should be > 0 and <=100 for External Runoff node ',String(1:Len_Trim(String)) )
            endif

            ! optional unit hydrograph selection  uh 0 or uh 1 or uh 2)
            allow = .true.
            found = .false.
            Retval = RetVal + GetVAR2(STRING,' uh ',3,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                         CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
            if (found) SCS_UHChosen(IRRRunoffSub) = IDUM(1)
            if ((found .and. SCS_UHChosen(IRRRunoffSub) < 2) .or. .not. found) then !
               ! RRRunoffNode SCS parameters, if SCS selected they should be present
               allow = .true.
               found = .false.
               Retval = RetVal + GetVAR2(STRING,' slp ',2,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                            CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
               SCS_Slope(IRRRunoffSub) = RDUM(1)
               Retval = RetVal + GetVAR2(STRING,' le ',2,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                            CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
               SCS_Length(IRRRunoffSub) = RDUM(1)
            else ! Snyder unit hydrograph
               allow = .true.
               found = .false.
               Retval = RetVal + GetVAR2(STRING,' cp ',2,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                         CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
               SCS_Snyder_Cp(IRRRunoffSub) = RDUM(1)

               allow = .true.
               found = .false.
               Retval = RetVal + GetVAR2(STRING,' uhdr ',2,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                         CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
               if (found) SCS_Snyder_UH_decay_rate(IRRRunoffSub) = RDUM(1)

               allow = .true.
               found = .false.
               Retval = RetVal + GetVAR2(STRING,' uhdf ',2,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                         CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
               if (found) SCS_Snyder_UH_decay_frac(IRRRunoffSub) = RDUM(1)

               allow = .true.
               found = .false.
               Retval = RetVal + GetVAR2(STRING,' bfdr ',2,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                         CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
               if (found) SCS_Snyder_BF_decay_rate(IRRRunoffSub) = RDUM(1)

               allow = .true.
               found = .false.
               Retval = RetVal + GetVAR2(STRING,' bfsq ',2,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                         CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
               if (found) SCS_Snyder_BF_STRTQ(IRRRunoffSub) = RDUM(1)

               allow = .true.
               found = .false.
               Retval = RetVal + GetVAR2(STRING,' bfim ',3,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                         CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
               if (found) SCS_Snyder_BF_interpolation_method(IRRRunoffSub) = IDUM(1)

               SHG_set%size = SHG_set%size + 1
            endif
            ! optional TimeLag specification (in hours)
            allow = .true.
            found = .false.
            Retval = RetVal + GetVAR2(STRING,' tl ',2,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                         CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
            if (found) SCS_TLag(IRRRunoffSub) = RDUM(1)
            if (.not. found .and. SCS_UHChosen(IRRRunoffSub) == 2) then ! Time lag is mandatory for Snyder
               NodeId = Id_Nod(inod)
               call ErrMsgStandard (977, 0, ' Basin Time Lag is required for Snyder UH SCS Runoff node ',trim(NodeId))
            endif
            ! optional reservoir coefficient for HEC-HMS formulation
            allow = .true.
            found = .false.
            Retval = RetVal + GetVAR2(STRING,' r ',2,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                         CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
            if (found) SCS_HMSLinResR(IRRRunoffSub) = RDUM(1)
            ! optional antecedent moisture conditions: amc 1,2,3
            allow = .true.
            found = .false.
            Retval = RetVal + GetVAR2(STRING,' amc ',3,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                         CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
            if (found) SCS_AMC(IRRRunoffSub) = IDUM(1)
            ! Optional Green-Ampt infiltration Sept2024
            allow = .true.
            found = .false.
            Retval = RetVal + GetVAR2(STRING,' ga ',3,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                         CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
            if (found)  SCS_UseGreenAmpt_Infiltration(IRRRunoffSub) = ( IDUM(1) .eq. 1 )
            if (SCS_UseGreenAmpt_Infiltration(IRRRunoffSub)) then
               allow = .false.
               Retval = RetVal + GetVAR2(STRING,' ksat ',2,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                                CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
               SCS_GreenAmpt_Ksat(IRRRunoffSub) = max(0.0, RDum(1))
               Retval = RetVal + GetVAR2(STRING,' psi ',2,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                                CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
               SCS_GreenAmpt_Psi(IRRRunoffSub) = max(0.0, RDum(1))
               Retval = RetVal + GetVAR2(STRING,' theta_dmax ',2,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                                CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
               SCS_GreenAmpt_theta_dmax(IRRRunoffSub) = max (0.0, RDum(1))
               SCS_GreenAmpt_theta_dmax(IRRRunoffSub) = min (1.0, SCS_GreenAmpt_theta_dmax(IRRRunoffSub))
               SCS_GreenAmpt_theta_d   (IRRRunoffSub) = SCS_GreenAmpt_theta_dmax(IRRRunoffSub)
               SCS_GreenAmpt_theta_du  (IRRRunoffSub) = SCS_GreenAmpt_theta_dmax(IRRRunoffSub)
            endif

            ! optional base flow parameters
            allow = .true.
            found = .false.
            Retval = RetVal + GetVAR2(STRING,' bu ',3,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                         CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
            if (found)  SCS_UseBaseFlow(IRRRunoffSub) = ( IDUM(1) .eq. 1 )
            if (SCS_UseBaseFlow(IRRRunoffSub)) then
               allow = .false.
               Retval = RetVal + GetVAR2(STRING,' gsmax ',3,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                                CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
               SCS_MaxGWCap(IRRRunoffSub) = max(0.0, RDum(1))
               Retval = RetVal + GetVAR2(STRING,' gsinit ',3,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                                CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
               SCS_InitGWCap(IRRRunoffSub) = max(0.0, RDum(1))
               SCS_InitGWCap(IRRRunoffSub) = min(SCS_InitGwCap(IRRRunoffSub), SCS_MaxGWCap(IRRRUnoffSub))
               Retval = RetVal + GetVAR2(STRING,' kr ',3,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                                CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
               SCS_GwRecessionConst(IRRRunoffSub) = max (0.0, RDum(1))
            endif
           elseif (String(1:4) .eq. 'NAM ') then
! RRRunoffNode NAM parameters, if NAM selected they should be present
            allow = .true.
            found = .false.
            if (RRRunoff_CompOption(IRRRunoff) .eq. 3) then
                allow = .false.
            endif
            Retval = RetVal + GetVAR2(STRING,' sp ',1,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                         CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
            NAM_SoilParameterDefinition(IRRRunoffSub) = CDUM(1)
            Retval = RetVal + GetVAR2(STRING,' lwd ',1,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                         CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
            NAM_LevelAndDepthDefinition(IRRRunoffSub) = CDUM(1)
            Retval = RetVal + GetVAR2(STRING,' sr ',1,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                         CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
            NAM_SurfaceRunoffDefinition(IRRRunoffSub) = CDUM(1)
            Retval = RetVal + GetVAR2(STRING,' bf ',1,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                         CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
            NAM_BaseFlowDefinition(IRRRunoffSub) = CDUM(1)
            Retval = RetVal + GetVAR2(STRING,' gw ',1,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                         CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
            NAM_GroundwaterForcingDefinition(IRRRunoffSub) = CDUM(1)
            NAM_CatchmentArea(IRRRUnoffSub) = AREA_RRRunoffNode(IRRRunoff)

          elseif (String(1:4) .eq. 'LGSI') then
! RRRunoffNode LGSI parameters, if LGSI selected they should be present
            allow = .true.
            found = .false.
            if (RRRunoff_CompOption(IRRRunoff) .eq. 4) then
                allow = .false.
            endif
            RetVal = RetVal + GetVRS2(STRING,' st ',1,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file', &
                                      IOUT1, CDUM(1), RDUM(1), IDUM(1), LGSI_NrSubAreas(IRRRunoffSub), IflRtn)
            Do i=1,LGSI_NrSubAreas(IRRRunoffSub)
               LGSI_NameSubArea(IRRRunoffSub,i) = CDUM(i)
            Enddo
            RetVal = RetVal + GetVRS2(STRING,' init ',1,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file', &
                                      IOUT1, CDUM(1), RDUM(1), IDUM(1), LGSI_NrSubAreas(IRRRunoffSub), IflRtn)
            Do i=1,LGSI_NrSubAreas(IRRRunoffSub)
               LGSI_InitialCondition(IRRRunoffSub,i) = CDUM(i)
            Enddo
            RetVal = RetVal + GetVRS2(STRING,' dd ',1,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file', &
                                      IOUT1, CDUM(1), RDUM(1), IDUM(1), LGSI_NrSubAreas(IRRRunoffSub), IflRtn)
            LGSI_DelayDefinition(IRRRunoffSub) = CDUM(1)
            RetVal = RetVal + GetVRS2(STRING,' erd ',2,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file', &
                                      IOUT1, CDUM(1), RDUM(1), IDUM(1), LGSI_NrSubAreas(IRRRunoffSub), IflRtn)
            Do i=1,LGSI_NrSubAreas(IRRRunoffSub)
               If (Rdum(i) .lt. 0.001D0) then
                   Err969 = .true.
                   call ErrMsgStandard (969, 0, ' Evap. reduction depth erd should be >0; typical value between 0.4 and 10',String(1:Len_Trim(String)) )
               endif
               LGSI_ERD(IRRRunoffSub,i) = RDUM(i)
            Enddo
            if (LGSI_NrSubAreas(IRRRunoffSub) .ge. 2) then
              Do i=2,LGSI_NrSubAreas(IRRRunoffSub)
                 if (LGSI_MeteoStation(IRRRunoffSub,i) .ne. LGSI_Meteostation(IRRRunoffSub,1)) then
                   call ErrMsgStandard (974, 0, ' Different meteo stations specified at LGSI subareas. However, this is not yet supported. The first station is used for all subareas. ',String(1:Len_Trim(String)) )
                 endif
              enddo
            endif
            RetVal = RetVal + GetVRS2(STRING,' ersd ',2,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file', &
                                      IOUT1, CDUM(1), RDUM(1), IDUM(1), LGSI_NrSubAreas(IRRRunoffSub), IflRtn)
            Do i=1,LGSI_NrSubAreas(IRRRunoffSub)
               If (Rdum(i) .lt. 0.001D0) then
                   Err969 = .true.
                   call ErrMsgStandard (969, 0, ' Evap. reduction depth st.dev. ersd should be >0; typical value between 0.4 and 10',String(1:Len_Trim(String)) )
               endif
               LGSI_ERSD(IRRRunoffSub,i) = RDUM(i)
            Enddo
            RetVal = RetVal + GetVRS2(STRING,' rdr ',2,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file', &
                                      IOUT1, CDUM(1), RDUM(1), IDUM(1), LGSI_NrSubAreas(IRRRunoffSub), IflRtn)
            Do i=1,LGSI_NrSubAreas(IRRRunoffSub)
               If (Rdum(i) .lt. 0.001D0) then
                   Err969 = .true.
                   call ErrMsgStandard (969, 0, ' Drainage resistance (tube drainage) should be >0; typical value between 5 and 25',String(1:Len_Trim(String)) )
               endif
               LGSI_RDR(IRRRunoffSub,i) = RDUM(i) * 24.   ! convert from days to hours ; hard coded
            Enddo
            RetVal = RetVal + GetVRS2(STRING,' ddr ',2,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file', &
                                      IOUT1, CDUM(1), RDUM(1), IDUM(1), LGSI_NrSubAreas(IRRRunoffSub), IflRtn)
            Do i=1,LGSI_NrSubAreas(IRRRunoffSub)
               If (Rdum(i) .lt. 0.001D0) then
                   Err969 = .true.
                   call ErrMsgStandard (969, 0, ' Drainage depth (tube drainage) should be >0; typical value between .3 and 2',String(1:Len_Trim(String)) )
               endif
               LGSI_DDR(IRRRunoffSub,i) = RDUM(i)
            Enddo
            RetVal = RetVal + GetVRS2(STRING,' fas ',2,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file', &
                                      IOUT1, CDUM(1), RDUM(1), IDUM(1), LGSI_NrSubAreas(IRRRunoffSub), IflRtn)
            Do i=1,LGSI_NrSubAreas(IRRRunoffSub)
               LGSI_FAS(IRRRunoffSub,i) = RDUM(i)
               If (Rdum(i) .lt. 0.D0 .or. Rdum(i) .gt. 1.D0) then
                   Err969 = .true.
                   call ErrMsgStandard (969, 0, ' Fraction LGSI-fas should be between 0 and 1; typical value between 0.3 and 0.9',String(1:Len_Trim(String)) )
               endif
               LGSI_ASD (IRRRunoffSub,i) = LGSI_Area(IRRRunoffSub,i)*LGSI_FAS(IRRRunoffSub,i)
               LGSI_ASDR(IRRRunoffSub,i) = LGSI_Area(IRRRunoffSub,i)-LGSI_ASD(IRRRunoffSub,i)
            Enddo
            RetVal = RetVal + GetVRS2(STRING,' tets ',2,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file', &
                                      IOUT1, CDUM(1), RDUM(1), IDUM(1), LGSI_NrSubAreas(IRRRunoffSub), IflRtn)
            Do i=1,LGSI_NrSubAreas(IRRRunoffSub)
               If (Rdum(i) .lt. 0.D0 .or. Rdum(i) .gt. 1.D0) then
                   Err969 = .true.
                   call ErrMsgStandard (969, 0, ' Porosity LGSI-Tets should be between 0 and 1; typical values 0.25<tets<0.8',String(1:Len_Trim(String)) )
               endif
               LGSI_TETS(IRRRunoffSub,i) = RDUM(i)
            Enddo
            RetVal = RetVal + GetVRS2(STRING,' alp ',2,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file', &
                                      IOUT1, CDUM(1), RDUM(1), IDUM(1), LGSI_NrSubAreas(IRRRunoffSub), IflRtn)
            Do i=1,LGSI_NrSubAreas(IRRRunoffSub)
               If (Rdum(i) .lt. 0.D0 .or. Rdum(i) .gt. 20.D0) then
                   Err969 = .true.
                   call ErrMsgStandard (969, 0, ' alp (van Genuchten parameter 1) should be between 0 and 20; ',String(1:Len_Trim(String)) )
               endif
               LGSI_ALP(IRRRunoffSub,i) = RDUM(i)
            Enddo
            RetVal = RetVal + GetVRS2(STRING,' n ',2,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file', &
                                      IOUT1, CDUM(1), RDUM(1), IDUM(1), LGSI_NrSubAreas(IRRRunoffSub), IflRtn)
            Do i=1,LGSI_NrSubAreas(IRRRunoffSub)
               LGSI_N(IRRRunoffSub,i) = RDUM(i)
               if (LGSI_N(IRRRunoffSub,i) .le. 1.00) call SetMessage(LEVEL_WARN, 'LGSI_n (van Genuchten parameter 2) should be > 1')
               LGSI_N(IRRRunoffSub,i) = max(1.01d0, LGSI_N(IRRRunoffSub,i))  ! ivm functie 1/(n-1) moet n>1 zijn
            Enddo
            RetVal = RetVal + GetVRS2(STRING,' qout ',2,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file', &
                                      IOUT1, CDUM(1), RDUM(1), IDUM(1), LGSI_NrSubAreas(IRRRunoffSub), IflRtn)
            Do i=1,LGSI_NrSubAreas(IRRRunoffSub)
!               LGSI_QOUT(IRRRunoffSub,i) = RDUM(i) / 24.   ! convert from mm/day to mm/hour ; hard coded
               LGSI_QOUT(IRRRunoffSub,i) = RDUM(i) / 24. * (TimeSettings%Timestepsize / 3600.)   ! converted to mm per computation timestep
            Enddo
            allow = .true.
            Retval = RetVal + GetVAR2(STRING,' hdif ',2,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                         CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
            LGSI_HDIF(IRRRunoffSub) = RDUM(1)
            Retval = RetVal + GetVAR2(STRING,' c ',2,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                         CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
            LGSI_C(IRRRunoffSub) = RDUM(1) * 24.   ! convert from days to hours ; hard coded
          elseif (String(1:4) .eq. 'WAGM') then
! RRRunoffNode Wageningen model parameters, if WAGM selected they should be present
            allow = .true.
            found = .false.
            if (RRRunoff_CompOption(IRRRunoff) .eq. 5) then
                allow = .false.
            endif
!           Retval = RetVal + GetVAR2 (STRING,' id ',1,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
!                      ID, RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
            WagMod_GROUPNAME(IRRRunoffSub) = Id_Nod(inod)     ! of toch:  id
            WagMod_UNITHY  (IRRRunoffSub)  = WagMod_GROUPNAME(IRRRunoffSub)
            WagMod_PLOTFILE(IRRRunoffSub)  = WagMod_GROUPNAME(IRRRunoffSub)
            WagMod_OPTIMAAL(IRRRunoffSub)  = WagMod_GROUPNAME(IRRRunoffSub)
            Len_Groupname = 1
            KChar = WagMod_GroupName(IRRRunoffSub)(Len_groupname:Len_Groupname)
            DO WHILE (KChar .NE. ' ' .AND. KChar .NE. '.' .AND. Len_groupname .LE. 33)
              LEN_GroupName = LEN_Groupname + 1
              IF (LEN_Groupname .LE. 33) THEN
                KChar = WagMod_GroupName(IRRRunoffSub)(Len_groupname:len_groupname)
              ENDIF
            ENDDO
            WagMod_UNITHY  (IRRRunoffSub)(Len_Groupname:Len_groupname+3) = '.00U'
            WagMod_PLOTFILE(IRRRunoffSub)(Len_Groupname:Len_groupname+3) = '.00P'
            WagMod_OPTIMAAL(IRRRunoffSub)(Len_Groupname:Len_Groupname+3) = '.00O'
            Retval = RetVal + GetVAR2(STRING,' j ',2,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                                       CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
            ! todo: reservoir coeff. input in day
            If (Rdum(1) .lt. 0.D0) then
                Err969 = .true.
                call ErrMsgStandard (969, 0, ' Wageningen model reservoir coefficient j should be > 0; ',String(1:Len_Trim(String)) )
            endif
            WagMod_J(IRRRunoffSub) = RDUM(1)
            Retval = RetVal + GetVAR2(STRING,' e ',2,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                                       CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
            If (Rdum(1) .lt. 0.D0) then
                Err969 = .true.
                call ErrMsgStandard (969, 0, ' Wageningen model coefficient E should be > 0; ',String(1:Len_Trim(String)) )
            endif
            WagMod_E(IRRRunoffSub) = RDUM(1)
            Retval = RetVal + GetVAR2(STRING,' f ',2,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                                       CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
            If (Rdum(1) .lt. 0.D0) then
                Err969 = .true.
                call ErrMsgStandard (969, 0, ' Wageningen model coefficient E should be > 0; ',String(1:Len_Trim(String)) )
            endif
            WagMod_F(IRRRunoffSub) = RDUM(1)
            Retval = RetVal + GetVAR2(STRING,' cr ',2,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                                       CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
            If (Rdum(1) .lt. 0.D0) then
                Err969 = .true.
                call ErrMsgStandard (969, 0, ' Wageningen model coefficient CR should be > 0; ',String(1:Len_Trim(String)) )
            endif
            WagMod_CR(IRRRunoffSub) = RDUM(1)
            Retval = RetVal + GetVAR2(STRING,' repa ',2,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                                       CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
            If (Rdum(1) .lt. 0.D0) then
                Err969 = .true.
                call ErrMsgStandard (969, 0, ' Wageningen model coefficient Repa should be > 0; ',String(1:Len_Trim(String)) )
            endif
            WagMod_REPA(IRRRunoffSub) = RDUM(1)
            Retval = RetVal + GetVAR2(STRING,' fos ',2,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                                       CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
            If (Rdum(1) .lt. 0.D0) then
                Err969 = .true.
                call ErrMsgStandard (969, 0, ' Wageningen model coefficient FOS should be > 0; ',String(1:Len_Trim(String)) )
            endif
            WagMod_FOS(IRRRunoffSub) = RDUM(1)
            Retval = RetVal + GetVAR2(STRING,' sm0 ',2,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                                       CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
            If (Rdum(1) .lt. 0.D0) then
                Err969 = .true.
                call ErrMsgStandard (969, 0, ' Wageningen model initial soil moisture SM0 should be > 0; ',String(1:Len_Trim(String)) )
            endif
            WagMod_SM0(IRRRunoffSub) = RDUM(1)
            Retval = RetVal + GetVAR2(STRING,' qg0 ',2,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                                       CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
            If (Rdum(1) .lt. 0.D0) then
                Err969 = .true.
                call ErrMsgStandard (969, 0, ' Wageningen model initial groundwater outflow QG0 should be > 0; ',String(1:Len_Trim(String)) )
            endif
            WagMod_QG0(IRRRunoffSub) = RDUM(1)
            Retval = RetVal + GetVAR2(STRING,' fc ',2,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                                       CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
            If (Rdum(1) .lt. 0.D0) then
                Err969 = .true.
                call ErrMsgStandard (969, 0, ' Wageningen model coefficient SM0 should be > 0; ',String(1:Len_Trim(String)) )
            endif
            If (Rdum(1) .lt. 0.D0) then
                Err969 = .true.
                call ErrMsgStandard (969, 0, ' Wageningen model field capacity FC should be > 0; ',String(1:Len_Trim(String)) )
            endif
            WagMod_FC(IRRRunoffSub) = RDUM(1)
            Retval = RetVal + GetVAR2(STRING,' sat ',2,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                                       CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
            If (Rdum(1) .lt. 0.D0) then
                Err969 = .true.
                call ErrMsgStandard (969, 0, ' Wageningen model saturation capacity SAT should be > 0; ',String(1:Len_Trim(String)) )
            endif
            WagMod_SAT(IRRRunoffSub) = RDUM(1)
            If (WagMod_SAT(IRRRunoffSub) .lt. WagMod_FC(IRRRunoffSub)) then
                Err969 = .true.
                call ErrMsgStandard (969, 0, ' Wageningen model Field capacity FC should be <= Saturation capacity SAT ; ',String(1:Len_Trim(String)) )
            endif
            Retval = RetVal + GetVAR2(STRING,' seep ',2,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                                       CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
            WagMod_SEEP(IRRRunoffSub) = RDUM(1)
            Retval = RetVal + GetVAR2(STRING,' co ',3,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                                       CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
            WagMod_ActEvapCompOption(IRRRunoffSub) = IDUM(1)
! Iuhslow, iuhquick, isubquick in days, days, seconds per substep
! Ntermuj
            Retval = RetVal + GetVAR2(STRING,' iuhslow ',2,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                                       CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
! convert from days to timesteps
            WagMod_IUHSLOW(IRRRunoffSub) = Int ( RDUM(1) * 86400. / TimeSettings%TimestepSize)
! GP 18 dec 2013: check max. value IUHSlow
            WagMod_IUHSLOW(IRRRunoffSub) = Min (WagMod_IUHSlow(IRRRunoffSub),Wagmod_MaxNrTimesteps)
            Retval = RetVal + GetVAR2(STRING,' iuhquick ',2,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                                       CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
            WagMod_IUHQuick(IRRRunoffSub) = Int ( RDUM(1) * 86400. / TimeSettings%TimestepSize)
! GP 18 dec 2013: check max. value IUHQuick
            WagMod_IUHQuick(IRRRunoffSub) = Min (WagMod_IUHQuick(IRRRunoffSub),Wagmod_MaxNrTimesteps)
            Retval = RetVal + GetVAR2(STRING,' isubquick ',3,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                                       CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
            WagMod_ISubQuick(IRRRunoffSub) = Int ( TimeSettings%TimestepSize / Idum(1) )
! GP 18 dec 2013: check min. value IsubQuick
            WagMod_ISubQuick(IRRRunoffSub) = Max (WagMod_ISubQuick(IRRRunoffSub),1)
            Retval = RetVal + GetVAR2(STRING,' ntermuj ',3,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                                       CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
            WagMod_NTERMUJ(IRRRunoffSub) = IDUM(1)

          elseif (String(1:4) .eq. 'WALR') then
! WALR record kan evt over meerdere regels verspreid
! RRRunoffNode Walrus model parameters, if WALR selected they should be present
            allow = .true.
            found = .false.
            if (RRRunoff_CompOption(IRRRunoff) .eq. 6) then
                allow = .false.
            endif
!           write(*,*) ' Read from String ', String(1:279)
            Retval = RetVal + GetVAR2(STRING,' wa ',3,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                                       CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
            Walrus_WA(IRRRunoffSub) = .not. (Idum(1) .eq. 0)
            Retval = RetVal + GetVAR2(STRING,' va ',3,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                                       CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
            Walrus_VA(IRRRunoffSub) = .not. (Idum(1) .eq. 0)
            Retval = RetVal + GetVAR2(STRING,' ba ',3,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                                       CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
            Walrus_BA(IRRRunoffSub) = .not. (Idum(1) .eq. 0)
            Retval = RetVal + GetVAR2(STRING,' qa ',3,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                                       CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
            Walrus_QA(IRRRunoffSub) = .not. (Idum(1) .eq. 0)
            Retval = RetVal + GetVAR2(STRING,' hst ',3,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                                       CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
            Walrus_HST(IRRRunoffSub) = .not. (Idum(1) .eq. 0)
! WA: CW or WIT
            if (Walrus_WA(IRRRunoffSub)) then
               Retval = RetVal + GetVAR2(STRING,' cw ',2,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                                          CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
               If (Rdum(1) .lt. 0.D0) then
                   Err969 = .true.
                   call ErrMsgStandard (969, 0, ' Walrus wetness index parameter should be > 0; ',String(1:Len_Trim(String)) )
               endif
               Walrus_CW(IRRRunoffSub) = RDUM(1)
            else
               Retval = RetVal + GetVAR2(STRING,' wit ',1,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                                          CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
               Walrus_WIT(IRRRunoffSub) = CDum(1)
               WalrusUserDefinedWetnessFunctionExists = .true.
            endif
! VA: CV always!, VIT optional
            if (Walrus_VA(IRRRunoffSub)) then
                ! CV read below, always
            else
               Retval = RetVal + GetVAR2(STRING,' vit ',1,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                                          CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
               Walrus_VIT(IRRRunoffSub) = CDum(1)
               WalrusUserDefinedVEQFunctionExists = .true.
            endif
! BA: nothing or BIT
            if (Walrus_BA(IRRRunoffSub)) then
               ! no other parameters for evaporation reduction factor beta
            else
               Retval = RetVal + GetVAR2(STRING,' bit ',1,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                                          CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
               Walrus_BIT(IRRRunoffSub) = CDum(1)
               WalrusUserDefinedBETAFunctionExists = .true.
            endif
! QA: CS, CD, XS, HST, HSMIN/HSMinTable or QIT
            if (Walrus_QA(IRRRunoffSub)) then
               Retval = RetVal + GetVAR2(STRING,' cs ',2,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                                          CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
               If (Rdum(1) .lt. 0.D0) then
                   Err969 = .true.
                   call ErrMsgStandard (969, 0, ' Walrus bankfull discharge cs [mm/hour] should be >=0; ',String(1:Len_Trim(String)) )
               endif
               Walrus_CS(IRRRunoffSub) = RDUM(1)
               Retval = RetVal + GetVAR2(STRING,' cd ',2,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                                          CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
               If (Rdum(1) .lt. 0.D0) then
                   Err969 = .true.
                   call ErrMsgStandard (969, 0, ' Walrus channel depth [mm] should be >=0; ',String(1:Len_Trim(String)) )
               endif
               Walrus_CD(IRRRunoffSub) = RDUM(1)
               Retval = RetVal + GetVAR2(STRING,' xs ',2,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                                          CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
               If (Rdum(1) .lt. 0.D0) then
                   Err969 = .true.
                   call ErrMsgStandard (969, 0, ' Walrus Qh relation exponent should be >=0; ',String(1:Len_Trim(String)) )
               endif
               Walrus_XS(IRRRunoffSub) = RDUM(1)
            else
               Retval = RetVal + GetVAR2(STRING,' qit ',1,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                                          CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
               Walrus_QIT(IRRRunoffSub) = CDum(1)
               WalrusUserDefinedQHFunctionExists = .true.
            endif
! Moved outside If QA test, GP 14 JULY 2020
            if (Walrus_HST(IRRRunoffSub)) then
                ! HSMin as time table
                Retval = RetVal + GetVAR2(STRING,' hstable', 1,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                                           CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
                If (Cdum(1) .eq. '') then
                    Err969 = .true.
                    call ErrMsgStandard (969, 0, ' Walrus hsmin table id should be non-empty; ',String(1:Len_Trim(String)) )
                endif
                WalrusHSMinTimeTableExists = .true.
                Walrus_HSMINTable(IRRRunoffSub) = CDUM(1)
            else
                Retval = RetVal + GetVAR2(STRING,' hsmin ', 2,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                                           CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
                If (Rdum(1) .lt. 0.D0) then
                    Err969 = .true.
                    call ErrMsgStandard (969, 0, ' Walrus channel depth hsmin when Q=0 should be >=0; ',String(1:Len_Trim(String)) )
                endif
                Walrus_HSMIN(IRRRunoffSub) = RDUM(1)
            endif
! other parameters always
! cV
            Retval = RetVal + GetVAR2(STRING,' cv ',2,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                                       CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
            If (Rdum(1) .lt. 0.D0) then
                Err969 = .true.
                call ErrMsgStandard (969, 0, ' Walrus vadoze zone relaxation time cv should be >=0; ',String(1:Len_Trim(String)) )
            endif
            Walrus_CV(IRRRunoffSub) = RDUM(1)
! CG
            Retval = RetVal + GetVAR2(STRING,' cg ', 2,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                                       CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
            If (Rdum(1) .le. 0.D0) then
                Err969 = .true.
                call ErrMsgStandard (969, 0, ' Walrus groundwater reservoir constant (mm.hour) should be >0; ',String(1:Len_Trim(String)) )
            endif
            Walrus_CG(IRRRunoffSub) = RDUM(1)
! CQ
            Retval = RetVal + GetVAR2(STRING,' cq ', 2,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                                       CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
            If (Rdum(1) .le. 0.D0) then
                Err969 = .true.
                call ErrMsgStandard (969, 0, ' Walrus quickflow reservoir constant (hour) should be >0; ',String(1:Len_Trim(String)) )
            endif
            Walrus_CQ(IRRRunoffSub) = RDUM(1)
! AS
            Retval = RetVal + GetVAR2(STRING,' as ', 2,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                                       CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
            If (Rdum(1) .lt. 0.001D0) then
                Err969 = .true.
                call ErrMsgStandard (969, 0, ' Walrus surface water area fraction should be >=0.001; ',String(1:Len_Trim(String)) )
            endif
            If (Rdum(1) .gt. 0.999D0) then
                Err969 = .true.
                call ErrMsgStandard (969, 0, ' Walrus surface water area fraction should be <=0.999; ',String(1:Len_Trim(String)) )
            endif
            Walrus_AS(IRRRunoffSub) = RDUM(1)
            Walrus_AG(IRRRunoffSub) = 1.0D0 - Walrus_AS(IRRRunoffSub)
! ST soil type
            Retval = RetVal + GetVAR2(STRING,' st ', 3,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                                       CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
            If (Idum(1) .le. 20 .or. Idum(1) .gt. 34) then
                Err969 = .true.
                call ErrMsgStandard (969, 0, ' Unknown Walrus soil type specified ',String(1:Len_Trim(String))   )
            endif
!            Walrus_SoilType(IRRRunoffSub) = CDUM(1)
            Walrus_IntSoilType(IRRRunoffSub) = IDUM(1)
! initial conditions
!           If (CDum (1) .eq. 'sand')            Walrus_IntSoilType(IRRRunoffSub) = 21
!           If (CDum (1) .eq. 'loamy_sand')      Walrus_IntSoilType(IRRRunoffSub) = 22
!           If (CDum (1) .eq. 'sandy_loam')      Walrus_IntSoilType(IRRRunoffSub) = 23
!           If (CDum (1) .eq. 'silt_loam')       Walrus_IntSoilType(IRRRunoffSub) = 24
!           If (CDum (1) .eq. 'loam')            Walrus_IntSoilType(IRRRunoffSub) = 25
!           If (CDum (1) .eq. 'sandy_clay_loam') Walrus_IntSoilType(IRRRunoffSub) = 26
!           If (CDum (1) .eq. 'silt_clay_loam')  Walrus_IntSoilType(IRRRunoffSub) = 27
!           If (CDum (1) .eq. 'clay_loam')       Walrus_IntSoilType(IRRRunoffSub) = 28
!           If (CDum (1) .eq. 'sandy_clay')      Walrus_IntSoilType(IRRRunoffSub) = 29
!           If (CDum (1) .eq. 'silty_clay')      Walrus_IntSoilType(IRRRunoffSub) = 30
!           If (CDum (1) .eq. 'clay')            Walrus_IntSoilType(IRRRunoffSub) = 31
!           If (CDum (1) .eq. 'cal_H')           Walrus_IntSoilType(IRRRunoffSub) = 32
!           If (CDum (1) .eq. 'cal_C')           Walrus_IntSoilType(IRRRunoffSub) = 33
!           If (CDum (1) .eq. 'custom')          Walrus_IntSoilType(IRRRunoffSub) = 34
            If (iDum (1) .eq. wc_sand )           Walrus_SoilType(IRRRunoffSub) = 'sand'
            If (iDum (1) .eq. wc_loamy_sand )     Walrus_SoilType(IRRRunoffSub) = 'loamy_sand'
            If (iDum (1) .eq. wc_sandy_loam )     Walrus_SoilType(IRRRunoffSub) = 'sandy_loam'
            If (iDum (1) .eq. wc_silt_loam )      Walrus_SoilType(IRRRunoffSub) = 'silt_loam'
            If (iDum (1) .eq. wc_loam )           Walrus_SoilType(IRRRunoffSub) = 'loam'
            If (iDum (1) .eq. wc_sandy_clay_loam) Walrus_SoilType(IRRRunoffSub) = 'sandy_clay_loam'
            If (iDum (1) .eq. wc_silt_clay_loam)  Walrus_SoilType(IRRRunoffSub) = 'silt_clay_loam'
            If (iDum (1) .eq. wc_clay_loam )      Walrus_SoilType(IRRRunoffSub) = 'clay_loam'
            If (iDum (1) .eq. wc_sandy_clay)      Walrus_SoilType(IRRRunoffSub) = 'sandy_clay'
            If (iDum (1) .eq. wc_silty_clay)      Walrus_SoilType(IRRRunoffSub) = 'silty_clay'
            If (iDum (1) .eq. wc_clay )           Walrus_SoilType(IRRRunoffSub) = 'clay'
            If (iDum (1) .eq. wc_cal_H )          Walrus_SoilType(IRRRunoffSub) = 'cal_H'
            If (iDum (1) .eq. wc_cal_C )          Walrus_SoilType(IRRRunoffSub) = 'cal_C'
            If (iDum (1) .eq. wc_custom)          Walrus_SoilType(IRRRunoffSub) = 'Custom'

! for Walrus custom type, get b, psi_ae, theta_s
            if (Walrus_IntSoilType(IRRRunoffSub) .eq. wc_custom) then
                Retval = RetVal + GetVAR2(STRING,' b ', 2,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                                          CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
                If (Rdum(1) .le. 0.D0) then
                    Err969 = .true.
                    call ErrMsgStandard (969, 0, ' Walrus b for custom soil type should be >0; ',String(1:Len_Trim(String))   )
                endif
                Walrus_B(IRRRunoffSub) = Dble (RDUM(1))
                Retval = RetVal + GetVAR2(STRING,' psi_ae ', 2,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                                          CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
                If (Rdum(1) .le. 0.D0) then
                    Err969 = .true.
                    call ErrMsgStandard (969, 0, ' Walrus psi_ae for custom soil type should be >0; ',String(1:Len_Trim(String))   )
                endif
                Walrus_PSI_AE(IRRRunoffSub) = Dble (RDUM(1))
                Retval = RetVal + GetVAR2(STRING,' theta_s ', 2,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                                          CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
                If (Rdum(1) .le. 0.D0) then
                    Err969 = .true.
                    call ErrMsgStandard (969, 0, ' Walrus theta-saturation for custom soil type should be >0; ',String(1:Len_Trim(String)) )
                endif
                Walrus_THETA_S(IRRRunoffSub) = Dble (RDUM(1))
            endif
! HS0 HQ0 DG0 DV0 q0
            Retval = RetVal + GetVAR2(STRING,' hs0 ', 2,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                                       CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
            If (Rdum(1) .lt. 0.D0) then
                Err969 = .true.
                call ErrMsgStandard (969, 0, ' Walrus initial surface water content (mm) should be >=0; ',String(1:Len_Trim(String))   )
            endif
            Walrus_HS0(IRRRunoffSub) = RDUM(1)
            Retval = RetVal + GetVAR2(STRING,' hq0 ', 2,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                                       CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
            If (Rdum(1) .lt. 0.D0) then
                Err969 = .true.
                call ErrMsgStandard (969, 0, ' Walrus initial quickflow content (mm) should be >=0; ',String(1:Len_Trim(String)) )
            endif
            Walrus_HQ0(IRRRunoffSub) = RDUM(1)
            Retval = RetVal + GetVAR2(STRING,' dg0 ', 2,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                                       CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
            If (Rdum(1) .lt. 0.D0) then
                Err969 = .true.
                call ErrMsgStandard (969, 0, ' Walrus initial gw content (mm) should be >=0; ',String(1:Len_Trim(String)) )
            endif
            Walrus_DG0(IRRRunoffSub) = RDUM(1)
            Retval = RetVal + GetVAR2(STRING,' dv0 ', 2,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                                       CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
            If (Rdum(1) .lt. 0.D0) then
                Err969 = .true.
                call ErrMsgStandard (969, 0, ' Walrus initial storage deficit (mm) should be >=0; ',String(1:Len_Trim(String)) )
            endif
            Walrus_DV0(IRRRunoffSub) = RDUM(1)
            Retval = RetVal + GetVAR2(STRING,' q0 ', 2,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file',IOUT1, &
                                       CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, Iflrtn)
            If (Rdum(1) .lt. 0.D0) then
                Err969 = .true.
                call ErrMsgStandard (969, 0, ' Walrus initial discharge (mm/hour) should be >=0; ',String(1:Len_Trim(String)) )
            endif
            Walrus_Q0(IRRRunoffSub) = RDUM(1)

!           write(*,*) ' Read ', IRRRunoffSub
!           write(*,*) ' WALRUS_WA(IRRRunoffSub)', WALRUS_WA(IRRRunoffSub)
!           write(*,*) ' WALRUS_CW(IRRRunoffSub)', WALRUS_CW(IRRRunoffSub)
!           write(*,*) ' WALRUS_WIT(IRRRunoffSub)', WALRUS_WIT(IRRRunoffSub)
!           write(*,*) ' WALRUS_VA(IRRRunoffSub)', WALRUS_VA(IRRRunoffSub)
!           write(*,*) ' WALRUS_CV(IRRRunoffSub)', WALRUS_CV(IRRRunoffSub)
!           write(*,*) ' WALRUS_VIT(IRRRunoffSub)', WALRUS_VIT(IRRRunoffSub)
!           write(*,*) ' WALRUS_BA(IRRRunoffSub)', WALRUS_BA(IRRRunoffSub)
!           write(*,*) ' WALRUS_BIT(IRRRunoffSub)', WALRUS_BIT(IRRRunoffSub)
!           write(*,*) ' WALRUS_QA(IRRRunoffSub)', WALRUS_QA(IRRRunoffSub)
!           write(*,*) ' WALRUS_XS(IRRRunoffSub)', WALRUS_XS(IRRRunoffSub)
!           write(*,*) ' WALRUS_HSMIN(IRRRunoffSub)', WALRUS_HSMIN(IRRRunoffSub)
!           write(*,*) ' WALRUS_CS(IRRRunoffSub)', WALRUS_CS(IRRRunoffSub)
!           write(*,*) ' WALRUS_CD(IRRRunoffSub)', WALRUS_CD(IRRRunoffSub)
!           write(*,*) ' WALRUS_QIT(IRRRunoffSub)', WALRUS_QIT(IRRRunoffSub)
!           write(*,*) ' WALRUS_CG(IRRRunoffSub)', WALRUS_CG(IRRRunoffSub)
!           write(*,*) ' WALRUS_CQ(IRRRunoffSub)', WALRUS_CQ(IRRRunoffSub)
!           write(*,*) ' WALRUS_AS(IRRRunoffSub)', WALRUS_AS(IRRRunoffSub)
!           write(*,*) ' WALRUS_AG(IRRRunoffSub)', WALRUS_AG(IRRRunoffSub)
!           write(*,*) ' WALRUS_HS0(IRRRunoffSub)', WALRUS_HS0(IRRRunoffSub)
!           write(*,*) ' WALRUS_HQ0(IRRRunoffSub)', WALRUS_HQ0(IRRRunoffSub)
!           write(*,*) ' WALRUS_DG0(IRRRunoffSub)', WALRUS_DG0(IRRRunoffSub)
!           write(*,*) ' WALRUS_DV0(IRRRunoffSub)', WALRUS_DV0(IRRRunoffSub)
!           write(*,*) ' WALRUS_Q0(IRRRunoffSub)', WALRUS_Q0(IRRRunoffSub)
!           write(*,*) ' WALRUS_SoilType(IRRRunoffSub)', WALRUS_SoilType(IRRRunoffSub)
!           write(*,*) ' WALRUS_IntSoilType(IRRRunoffSub)', WALRUS_IntSoilType(IRRRunoffSub)

           Endif
          Endif
         Endif
        Endif
      Endif
      Call SKPCOM (INfile1, ENDFIL,'ODS')
    enddo
 21 Continue
    If (RetVal .gt. 0)  call ErrMsgStandard (972, 0, ' Error reading data from RR-runoff file ', ' Error getting EXTR/SCS/HBV/NAM records')
    If (teller .lt. NRRRunoff)  Then
        Do inod=1,NcNode
          IRRRunoff = EiNode(inod,2)
          if ((EiNode(inod,3) .ge. 18) .and. (EiNode(inod,3) .le. 20) .or. &
               (EiNode(inod,3) .eq. 22) .or. (EiNode(inod,3) .eq. 23) .or. (EiNode(inod,3) .eq. 31))  then   ! en is RRRunoffNode knoop
            if (.not. AlReadyRead(IRRRunoff)) then
              NodeId = ' '
              NodeId = Id_Nod(inod)
              String = ' '
              String = ' ' // NodeId(1:Len_Trim(NodeId)) // ' is missing'
              if (Einode(inod,3) .eq. 18) then
                 call ErrMsgStandard (977, 0, ' Data for External Runoff node ',String(1:Len_Trim(String)) )
              elseif (Einode(inod,3) .eq. 19) then
                 call ErrMsgStandard (977, 0, ' Data for HBV Runoff node ',String(1:Len_Trim(String)) )
              elseif (Einode(inod,3) .eq. 20) then
                 call ErrMsgStandard (977, 0, ' Data for SCS Runoff node ',String(1:Len_Trim(String)) )
              elseif (Einode(inod,3) .eq. 22) then
                 call ErrMsgStandard (977, 0, ' Data for LGSI Runoff node ',String(1:Len_Trim(String)) )
              elseif (Einode(inod,3) .eq. 23) then
                 call ErrMsgStandard (977, 0, ' Data for Wageningen/Walrus runoff node ',String(1:Len_Trim(String)) )
              elseif (Einode(inod,3) .eq. 31) then
                 call ErrMsgStandard (977, 0, ' Data for D-NAM Runoff node ',String(1:Len_Trim(String)) )
              endif
            endif
          endif
        Enddo
       call ErrMsgStandard (972, 0, ' Not enough data for all RRRunoffNode-nodes in schematisation found', &
                            ' Some RRRunoffNode-nodes from schematisation not present in RRRunoffNode.3B file')
    Endif
    if (Err969) call ErrMsgStandard (972, 0, ' Some invalid data specified for RR-Runoff nodes', ' See log file and check input data')
    Err969 = .false.

! *******************************************************
! hierna andere record types voor HBV, LGSI, WALRUS, NAM,
! *******************************************************

! HBV Nodes, additional record types
    if (NrHbvNodes .gt. 0) then
         Rewind(infile1)
         call SetMessage(LEVEL_INFO, 'Read HBV data - Snow')

         RetVal = 0
         ReferenceToDefinition = 0
         endfil = .false.
         teller = 0
         CALL SKPCOM (Infile1, ENDFIL, 'ODS')
         Do while (.not. endfil)
           READ (Infile1,'(A1000)',END=2111,ERR=150,IOSTAT=IECODE) STRING
           IF (STRING(1:4) .EQ. 'SNOW') Then
           ! Read Snow definition  id
           Retval = RetVal + GetVAR2 (STRING,' id ',1,' RRRunoff_readAscii',' HBV Snow definition data',IOUT1, &
                         CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
           name = CDUM(1)
           teller = teller + 1
           ! Eerst testen of snow definition wel gebruikt wordt, dan pas verwerken
           IRRRunoff = FindString (NcRRRunoffHBV, SnowDef, Name, NcRRRunoffHBV, CaseSensitive)
           Occurs = (IRRRunoff .gt. 0)
           if (IRRRunoff .gt. 0) then
!             IRRRunoffSub = RRRunoff_SubIndex(IRRRunoff)
              IRRRunoffSub = IRRRunoff
              if (ReferenceToDefinition(iRRRunoffSub) .gt. 0) then
                 call SetMessage(LEVEL_ERROR, 'Snow Definition '//name(1:Len_Trim(Name))//' double in datafile Runoff nodes')
                 LevelError = .true.
                 Occurs = .false.    ! voorkomt verdere verwerking
              else
                 ! cleaning RR files
                 If (CleanRRFiles) write(Iounit,'(A)') String (1:len_trim(String))
              endif
           endif
       ! Verwerk Snow definition
           if (occurs) then
       ! Read snow melt parameters
             Retval = RetVal + GetVAR2 (STRING,' mc ',2,' RRRunoff_readAscii',' HBV Snow data',IOUT1, &
                         CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
             MeltConstDum = RDUM(1)
             Retval = RetVal + GetVAR2 (STRING,' sft ',2,' RRRunoff_readAscii',' HBV Snow data',IOUT1, &
                         CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
             SnowFallTempDum = RDUM(1)
             Retval = RetVal + GetVAR2 (STRING,' smt ',2,' RRRunoff_readAscii',' HBV Snow data',IOUT1, &
                         CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
             SnowMeltTempDum = RDUM(1)
             Retval = RetVal + GetVAR2 (STRING,' tac ',2,' RRRunoff_readAscii',' HBV Snow data',IOUT1, &
                         CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
             TempAltConstDum = RDUM(1)
             Retval = RetVal + GetVAR2 (STRING,' fe ',2,' RRRunoff_readAscii',' HBV Snow data',IOUT1, &
                         CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
             FreezEffDum = RDUM(1)
             Retval = RetVal + GetVAR2 (STRING,' fwf ',2,' RRRunoff_readAscii',' HBV Snow data',IOUT1, &
                         CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
             FreeWaterFractionDum = RDUM(1)
       ! Assign definition to individual nodes
             Do iRRRunoff = 1, ncRRRunoff
               if (RRRunoff_CompOption(iRRRunoff) .eq. 1) then
                 IRRRunoffSub = RRRUnoff_SubIndex(iRRRunoff)
                 if (StringComp(SnowDef(IRRRunoffSub), Name, CaseSensitive) )  then
                   ReferenceToDefinition(iRRRunoffSub) = teller
                   HBV_MeltConst (iRRRunoffSub) = MeltConstDum
                   HBV_SnowFallTemp (iRRRunoffSub) = SnowFallTempDum
                   HBV_SnowMeltTemp (iRRRunoffSub) = SnowMeltTempDum
                   HBV_TempAltitudeConstant (iRRRunoffSub) = TempAltConstDum
                   HBV_FreezEff (iRRRunoffSub) = FreezEffDum
                   HBV_FreeWaterFraction (iRRRunoffSub) = FreeWaterFractionDum
                 endif
               endif
             Enddo
           Endif
          Endif
          CALL SKPCOM (Infile1, ENDFIL, 'ODS')
         Enddo
    2111 Continue

          If (RetVal .gt. 0) call ErrMsgStandard (972, 0, ' Error reading data from RR-runoff file ', ' Error getting SNOW records')
          Err969 = .false.
          Do iRRRunoff = 1, ncRRRunoff
            IRRRunoffSub = RRRunoff_SubIndex(IRRRunoff)
            if (RRRunoff_CompOption(IRRRunoff) .eq. 1) then
                if (ReferenceToDefinition(iRRRunoffSub) .eq. 0 .and. SnowDEF (iRRRunoffSub) .ne. '')  then
                   Err969 = .true.
                   call ErrMsgStandard (969, 0, ' Snow definition not found in RRRunoff file.', SnowDef(iRRRunoffSub))
                endif
            endif
          Enddo
          If (Err969) call ErrMsgStandard (972, 0, ' Not enough HBV-Snow data found', &
                              ' Some SNOW Definitions not present in RRRunoff file')


        Rewind(infile1)
        call SetMessage(LEVEL_INFO, 'Read HBV data - Soil')

         ReferenceToDefinition = 0
         endfil = .false.
         teller = 0
         RetVal = 0
         CALL SKPCOM (Infile1, ENDFIL, 'ODS')
         Do while (.not. endfil)
           READ (Infile1,'(A1000)',END=3111,ERR=150,IOSTAT=IECODE) STRING
           IF (STRING(1:4) .EQ. 'SOIL') Then
           ! Read Soil definition  id
           Retval = RetVal + GetVAR2 (STRING,' id ',1,' RRRunoff_readAscii',' HBV Soil definition data',IOUT1, &
                         CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
           name = CDUM(1)
           teller = teller + 1
           ! Eerst testen of soil definition wel gebruikt wordt, dan pas verwerken
           IRRRunoff = FindString (NcRRRunoffHBV, SoilDef, Name, NcRRRunoffHBV, CaseSensitive)
           Occurs = (IRRRunoff .gt. 0)
           if (IRRRunoff .gt. 0) then
!             IRRRunoffSub = RRRunoff_SubIndex(IRRRunoff)
              IRRRunoffSub = IRRRunoff
              if (ReferenceToDefinition(iRRRunoffSub) .gt. 0) then
                 call SetMessage(LEVEL_ERROR, 'Soil Definition '//name(1:Len_Trim(Name))//' double in datafile Runoff nodes')
                 LevelError = .true.
                 Occurs = .false.    ! voorkomt verdere verwerking
              else
                 ! cleaning RR files
                 If (CleanRRFiles) write(Iounit,'(A)') String (1:len_trim(String))
              endif
           endif
       ! Verwerk Soil definition
           if (occurs) then
       ! Read data
             Retval = RetVal + GetVAR2 (STRING,' be ',2,' RRRunoff_readAscii',' HBV Soil data',IOUT1, &
                         CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
             BetaDum = RDUM(1)
             Retval = RetVal + GetVAR2 (STRING,' fc ',2,' RRRunoff_readAscii',' HBV Soil data',IOUT1, &
                         CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
             FieldCapacityDum = RDUM(1)
             Retval = RetVal + GetVAR2 (STRING,' ef ',2,' RRRunoff_readAscii',' HBV Soil data',IOUT1, &
                         CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
             EvapFractionDum = RDUM(1)
       ! Assign definition to individual nodes
             Do iRRRunoff = 1, ncRRRunoff
                if (RRRunoff_CompOption(iRRRunoff) .eq. 1) then
                  IRRRunoffSub = RRRUnoff_SubIndex(iRRRunoff)
                  if (StringComp(SoilDef(IRRRunoffSub), Name, CaseSensitive) )  then
                     if (RRRunoff_CompOption(IRRRunoff) .eq. 1) then
                       ReferenceToDefinition(iRRRunoffSub) = teller
                       HBV_Beta (iRRRunoffSub) = BetaDum
                       HBV_FieldCapacity(iRRRunoffSub) = FieldCapacityDum
                       HBV_EvapFraction (iRRRunoffSub) = EvapFractionDum
                     else
                       ReferenceToDefinition(iRRRunoffSub) = -999  ! not HBV option, soil def not relevant
                     endif
                  endif
                endif
             Enddo
           Endif
          Endif
          CALL SKPCOM (Infile1, ENDFIL, 'ODS')
         Enddo
    3111 Continue

          If (RetVal .gt. 0) call ErrMsgStandard (972, 0, ' Error reading data from RR-runoff file ', ' Error getting SOIL records')
          Err969 = .false.
          Do iRRRunoff = 1, ncRRRunoff
             IRRRunoffSub = RRRunoff_SubIndex(IRRRunoff)
             if (RRRunoff_CompOption(IRRRUnoff) .eq. 1) then
                if (ReferenceToDefinition(iRRRunoffSub) .eq. 0 .and. SoilDEF (iRRRunoffSub) .ne. '')  then
                   Err969 = .true.
                   call ErrMsgStandard (969, 0, ' Soil definition not found in RRRunoff file.', SoilDef(iRRRunoffSub))
                endif
             endif
          Enddo
          If (Err969) call ErrMsgStandard (972, 0, ' Not enough HBV-Soil data found', &
                              ' Some SOIL Definitions not present in RRRunoff file')


        Err969 = .false.
        Rewind(infile1)
        call SetMessage(LEVEL_INFO, 'Read HBV data - Flow')
         ReferenceToDefinition = 0
         endfil = .false.
         teller = 0
         RetVal = 0
         CALL SKPCOM (Infile1, ENDFIL, 'ODS')
         Do while (.not. endfil)
           READ (Infile1,'(A1000)',END=4111,ERR=150,IOSTAT=IECODE) STRING
           IF (STRING(1:4) .EQ. 'FLOW') Then
           ! Read Flow definition  id
           Retval = RetVal + GetVAR2 (STRING,' id ',1,' RRRunoff_readAscii',' HBV Flow definition data',IOUT1, &
                         CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
           name = CDUM(1)
           teller = teller + 1
           ! Eerst testen of flow definition wel gebruikt wordt, dan pas verwerken
           IRRRunoff = FindString (NcRRRunoffHBV, FlowDef, Name, NcRRRunoffHBV, CaseSensitive)
           Occurs = (IRRRunoff .gt. 0)
           if (IRRRunoff .gt. 0) then
!              IRRRunoffSub = RRRunoff_SubIndex(IRRRunoff)
              IRRRunoffSub = IRRRunoff
              if (ReferenceToDefinition(iRRRunoffSub) .gt. 0) then
                 call SetMessage(LEVEL_ERROR, 'Flow Definition '//name(1:Len_Trim(Name))//' double in datafile Runoff nodes')
                 LevelError = .true.
                 Occurs = .false.    ! voorkomt verdere verwerking
              else
                 ! cleaning RR files
                 If (CleanRRFiles) write(Iounit,'(A)') String (1:len_trim(String))
              endif
           endif
           ! Verwerk Flow definition
           if (occurs) then
             ! Read data
             Retval = RetVal + GetVAR2 (STRING,' kb ',2,' RRRunoff_readAscii',' HBV Soil data',IOUT1, &
                         CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
             KBaseFlowDum = RDUM(1)
             !check 0 < KBaseFlow < 1
             if (kBaseFlowDum .le. 0 .or. KBaseFlowDum .gt. 1) then
                Err969 = .true.
                call ErrMsgStandard (969, 0, ' BaseFlow recession constant HBV should be between 0 and 1 in HBV flow definition ', FlowDef(iRRRunoffSub))
             endif
             Retval = RetVal + GetVAR2 (STRING,' ki ',2,' RRRunoff_readAscii',' HBV Soil data',IOUT1, &
                         CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
             KInterflowDum = RDUM(1)
             ! check 0 < KInterFlow < 1
             if (kInterFlowDum .le. 0 .or. KInterFlowDum .gt. 1) then
                Err969 = .true.
                call ErrMsgStandard (969, 0, ' InterFlow recession constant HBV should be between 0 and 1 in HBV flow definition ', FlowDef(iRRRunoffSub))
             endif
             Retval = RetVal + GetVAR2 (STRING,' kq ',2,' RRRunoff_readAscii',' HBV Soil data',IOUT1, &
                         CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
             KQuickFlowDum = RDUM(1)
             !check 0 < KQuickFlow < 1
             if (kQuickFlowDum .le. 0 .or. KQuickFlowDum .gt. 1) then
                call ErrMsgStandard (969, 0, ' QuickFlow recession constant HBV should be between 0 and 1 in HBV flow definition ', FlowDef(iRRRunoffSub))
             endif
             !check KBaseblow < KInterFlow < KQuickFlow
             if (kBaseFlowDum .gt. KInterFlowDum .or. KInterFlowDum .gt. KQuickFlowDum) then
                call ErrMsgStandard (969, 0, ' BaseFlow should be <= InterFlow <= QuickFlow recession constants HBV in flow definition ', FlowDef(iRRRunoffSub))
             endif

             Retval = RetVal + GetVAR2 (STRING,' mp ',2,' RRRunoff_readAscii',' HBV Soil data',IOUT1, &
                         CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
             MaxPercolationDum = RDUM(1)
             Retval = RetVal + GetVAR2 (STRING,' qt ',2,' RRRunoff_readAscii',' HBV Soil data',IOUT1, &
                         CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
             QuickFlowThresholdDum = RDUM(1)
             ! Adjustment of max. percolation rates from 1/day to 1/timestep
             MaxPercolationDum  = MaxPercolationDum * (float(TimeSettings%TimestepSize) / float(NrsDay))
             ! Adjustment of depletion rates from 1/day to 1/timestep
             KBaseFlowDum  = 1 - (1 - KBaseFlowDum)  ** (float(TimeSettings%TimestepSize) / float(NrsDay))
             KInterFlowDum = 1 - (1 - KInterFlowDum) ** (float(TimeSettings%TimestepSize) / float(NrsDay))
             KQuickFlowDum = 1 - (1 - KQuickFlowDum) ** (float(TimeSettings%TimestepSize) / float(NrsDay))
             ! Assign definition to individual nodes
             Do iRRRunoff = 1, ncRRRunoff
               if (RRRunoff_CompOption(iRRRunoff) .eq. 1) then
                  IRRRunoffSub = RRRUnoff_SubIndex(iRRRunoff)
                  if (StringComp(FlowDef(IRRRunoffSub), Name, CaseSensitive) )  then
                    if (RRRunoff_CompOption(IRRRunoff) .eq. 1) then
                      ReferenceToDefinition(iRRRunoffSub) = teller
                      HBV_KBaseFlow (iRRRunoffSub) = KBaseFlowDum
                      HBV_KInterFlow(iRRRunoffSub) = KInterflowDum
                      HBV_KQuickFlow(iRRRunoffSub) = KQuickFlowDum
                      HBV_QuickThreshold(iRRRunoffSub) = QuickFlowThresholdDum
                      HBV_MaxPercolation(iRRRunoffSub) = MaxPercolationDum
                    else
                      ReferenceToDefinition(iRRRunoffSub) = -999  ! not HBV option, flow def not relevant
                    endif
                  endif
               endif
             Enddo
           Endif
          Endif
          CALL SKPCOM (Infile1, ENDFIL, 'ODS')
         Enddo
    4111 Continue

          If (RetVal .gt. 0)  call ErrMsgStandard (972, 0, ' Error reading data from RR-runoff file ', ' Error getting FLOW records')
          If (Err969) call ErrMsgStandard (972, 0, ' Inconsistent input data in HBV Flow definitions', &
                              ' ')
          Err969 = .false.
          Do iRRRunoff = 1, ncRRRunoff
             IRRRunoffSub = RRRunoff_SubIndex(IRRRunoff)
             if (RRRunoff_CompOption(IRRRUnoff) .eq. 1) then
                if (ReferenceToDefinition(iRRRunoffSub) .eq. 0 .and. FlowDEF (iRRRunoffSub) .ne. '')  then
                   Err969 = .true.
                   call ErrMsgStandard (969, 0, ' Flow definition not found in RRRunoff file.', FlowDef(iRRRunoffSub))
                endif
            endif
          Enddo
          If (Err969) call ErrMsgStandard (972, 0, ' Not enough HBV-Flow data found', &
                              ' Some FLOW Definitions not present in RRRunoff file')


        Err969 = .false.
        Rewind(infile1)
        call SetMessage(LEVEL_INFO, 'Read HBV data - Hini')
         ReferenceToDefinition = 0
         endfil = .false.
         teller = 0
         RetVal = 0
         CALL SKPCOM (Infile1, ENDFIL, 'ODS')
         Do while (.not. endfil)
           READ (Infile1,'(A1000)',END=5111,ERR=150,IOSTAT=IECODE) STRING
           IF (STRING(1:4) .EQ. 'HINI') Then
           ! Read HINI definition  id
           Retval = RetVal + GetVAR2 (STRING,' id ',1,' RRRunoff_readAscii',' HBV Flow definition data',IOUT1, &
                         CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
           name = CDUM(1)
           teller = teller + 1
           ! Eerst testen of Hini definition wel gebruikt wordt, dan pas verwerken
           IRRRunoff = FindString (NcRRRunoffHBV, HIniDef, Name, NcRRRunoffHBV, CaseSensitive)
           Occurs = (IRRRunoff .gt. 0)
           if (IRRRunoff .gt. 0) then
!              IRRRunoffSub = RRRunoff_SubIndex(IRRRunoff)
              IRRRunoffSub = IRRRunoff
              if (ReferenceToDefinition(iRRRunoffSub) .gt. 0) then
                 call SetMessage(LEVEL_ERROR, 'HINI Definition '//name(1:Len_Trim(Name))//' double in datafile Runoff nodes')
                 LevelError = .true.
                 Occurs = .false.    ! voorkomt verdere verwerking
              else
                 ! cleaning RR files
                 If (CleanRRFiles) write(Iounit,'(A)') String (1:len_trim(String))
              endif
           endif
           ! Verwerk HIni definition
           if (occurs) then
             ! Read data
             Retval = RetVal + GetVAR2 (STRING,' ds ',2,' RRRunoff_readAscii',' HBV Initialisation data',IOUT1, &
                         CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
             InitialDrySnowDum = RDUM(1)
             Retval = RetVal + GetVAR2 (STRING,' fw ',2,' RRRunoff_readAscii',' HBV Initialisation data',IOUT1, &
                         CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
             InitialFreeWaterDum = RDUM(1)
             Retval = RetVal + GetVAR2 (STRING,' sm ',2,' RRRunoff_readAscii',' HBV Initialisation data',IOUT1, &
                         CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
             InitialSoilMoistureDum = RDUM(1)
             Retval = RetVal + GetVAR2 (STRING,' uz ',2,' RRRunoff_readAscii',' HBV Initialisation data',IOUT1, &
                         CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
             InitialUpperZoneDum = RDUM(1)
             Retval = RetVal + GetVAR2 (STRING,' lz ',2,' RRRunoff_readAscii',' HBV Initialisation data',IOUT1, &
                         CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
             InitialLowerZoneDum = RDUM(1)
!            Retval = RetVal + GetVAR2 (STRING,' qi ',2,' RRRunoff_readAscii',' HBV Initialisation data',IOUT1, &
!                        CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
!            InitialQRunoffInMMDum = RDUM(1)
             ! Assign definition to individual nodes
             Do iRRRunoff = 1, ncRRRunoff
               if (RRRunoff_CompOption(iRRRunoff) .eq. 1) then
                  IRRRunoffSub = RRRUnoff_SubIndex(iRRRunoff)
                  if (StringComp(HiniDef(IRRRunoffSub), Name, CaseSensitive) )  then
                     if (RRRunoff_CompOption(IRRRunoff) .eq. 1) then
                       ReferenceToDefinition(iRRRunoffSub) = teller
                       HBV_InitialDrySnowContent (iRRRunoffSub) = InitialDrySnowDum
                       HBV_FreeWaterContent(iRRRunoffSub) = InitialFreeWaterDum
!                      HBV_InitialQRunoffInMM(iRRRunoffSub)  = InitialQRunoffInMMDum
                       HBV_InitialMoisture(iRRRunoffSub) = InitialSoilMoistureDum * HBV_FieldCapacity(IRRRunoffSub)
                       HBV_InitialUpperZoneContent(iRRRunoffSub) = InitialUpperZoneDum
                       HBV_InitialLowerZoneContent(iRRRunoffSub) = InitialLowerZoneDum
                                                            ! HBV_InitialQRunoffInMM(IRRRunoffSub) / HBV_KBaseFlow(IRRRunoffSub)
                     else
                       ReferenceToDefinition(iRRRunoffSub) = -999  ! not HBV option, hini def not relevant
                     endif
                  endif
               endif
             Enddo
           Endif
          Endif
          CALL SKPCOM (Infile1, ENDFIL, 'ODS')
         Enddo
    5111 Continue

          If (RetVal .gt. 0) call ErrMsgStandard (972, 0, ' Error reading data from RR-runoff file ', ' Error getting HINI records')
          If (Err969) call ErrMsgStandard (972, 0, ' Inconsistent input data in HBV initial definitions', &
                              ' ')
          Err969 = .false.
          Do iRRRunoff = 1, ncRRRunoff
            IRRRunoffSub = RRRunoff_SubIndex(IRRRunoff)
            if (RRRunoff_CompOption(IRRRunoff) .eq.1) then
               if (ReferenceToDefinition(iRRRunoffSub) .eq. 0 .and. HIniDEF (iRRRunoffSub) .ne. '')  then
                   Err969 = .true.
                   call ErrMsgStandard (969, 0, ' Flow definition not found in RRRunoff file.', FlowDef(iRRRunoffSub))
                endif
             endif
          Enddo
          If (Err969) call ErrMsgStandard (972, 0, ' Not enough HBV-Initialisation data found', &
                              ' Some HINI Definitions not present in RRRunoff file')

    endif


! NAM Nodes, additional record types

    if (NrNAMNodes .gt. 0) then
! NAM Soil parameters
         Rewind(infile1)
         call SetMessage(LEVEL_INFO, 'Read D-NAM data - Soil Parameters')
         Err969 = .false.
         IndexNextTablestart = 1

         RetVal = 0
         ReferenceToDefinition = 0
         endfil = .false.
         teller = 0
         CALL SKPCOM (Infile1, ENDFIL, 'ODS')
         Do while (.not. endfil)
           Success = GetRecord(Infile1, 'NAMS', Endfil, idebug, Iout1)     ! get record van keyword NAMS tot nams, zet in buffer
           IF (.not. success) GOTO 2112
           IF (ENDFIL) GOTO 2112
           Success = GetStringFromBuffer (KeepBufString)
           IF (.not. Success .and. CleanRRFiles)   then
               Call ErrMsgStandard (999, 3, ' Local buffer RRRunoffmodule NAMS record D-NAM too small', ' Input skipped')
               GOTO 2112
           Endif
           Success = Success .and. GetStringFromBuffer (String)
           IF (.not. success) then
              call SetMessage(LEVEL_ERROR, 'Read NAMS record result: '//String(1:len_trim(String)))
              LevelError = .true.
              call ErrMsgStandard (972, 0, ' Error reading NAMS record from inputfile Sacrmnto.3B', ' Reading D-NAM input')
           Endif
!          READ (Infile1,'(A1000)',END=2112,ERR=150,IOSTAT=IECODE) STRING
           IF (STRING(1:4) .EQ. 'NAMS') Then
           ! Read D-NAM soil parameter definition  id
           Retval = RetVal + GetVAR2 (STRING,' id ',1,' RRRunoff_readAscii',' NAM Parameter definition data',IOUT1, &
                                      CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
           name = CDUM(1)
           teller = teller + 1
           ! Eerst testen of NAM soil parameter definition wel gebruikt wordt, dan pas verwerken
           IRRRunoff = FindString (NcRRRunoffNAM, NAM_SoilParameterDefinition, Name, NcRRRunoffNAM, CaseSensitive)
           Occurs = (IRRRunoff .gt. 0)
           if (IRRRunoff .gt. 0) then
              IRRRunoffSub = IRRRunoff
              if (ReferenceToDefinition(iRRRunoffSub) .gt. 0) then
                call SetMessage(LEVEL_ERROR, 'D-NAM Soil Parameter Definition '//name(1:Len_Trim(Name))//' double in datafile Runoff nodes')
                LevelError = .true.
                Occurs = .false.    ! voorkomt verdere verwerking
              else
! clean RR files
                If (CleanRRFiles) then
                  iCount = 0
                  ! use KeepBufString to write to file
                  ! first till TBLE
                  ! then till < characters
                  ! then till second table
                  ! then till < characters
                  ! then till the end of the buffer string
 1030             continue
                  lenstring = len_trim(KeepBufString)
                  ipos  = FndFrst ('TBLE ',KeepBufString(1:lenstring),.false.)
                  if (ipos .gt. 0) then
                     iCount = iCount +1
                     write(Iounit,'(A)') KeepBufString (1:ipos+4)
                     KeepBufString(1:) = KeepBufString(ipos+5:)
                  else
                     ! warning/error: no TBLE found
                       call SetMessage(LEVEL_ERROR, 'NAMS optional table definitions capt TBLE and/or pert TBLE not found')
                       goto 1032
                  endif
 1031             continue
                  lenstring = len_trim(KeepBufString)
                  ipos  = FndFrst (' < ',KeepBufString(1:lenstring),.false.)
                  ipos1 = FndFrst ('tble ',KeepBufString(1:lenstring),.false.)
                  if (ipos .gt. 0 .and. ipos1 .gt. ipos) then
                     write(Iounit,'(A)') KeepBufString (1:ipos+2)
                     KeepBufString(1:) = KeepBufString(ipos+3:)
                     goto 1031
                  elseif (icount .eq. 1) then
                     write(Iounit,'(A)') KeepBufString (1:ipos1+4)
                     KeepBufString(1:) = KeepBufString(ipos1+5:)
                     goto 1030
                  else
                     ! write remaining part
 1032                continue
                     write(Iounit,'(A)') KeepBufString (1:lenstring)
                  endif
                Endif
              endif
           endif
       ! Verwerk NAM Soil Parameter definition
           if (occurs) then
       ! Read parameters
             Retval = RetVal + GetVAR2 (STRING,' sfc ',2,' RRRunoff_readAscii',' D-NAM Soil Parameter data - field capacity root zone',IOUT1,  &
                                        CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
             FieldCapDum = Max (0.0, RDUM(1))
             Retval = RetVal + GetVAR2 (STRING,' infcap ',2,' RRRunoff_readAscii',' D-NAM Soil Parameter data - InfCap',IOUT1, &
                                        CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
!             InfCapDum = Max (0.0, RDUM(1))
             InfCapDum = RDUM(1)
             Retval = RetVal + GetVAR2 (STRING,' syrz ',2,' RRRunoff_readAscii',' D-NAM Soil Parameter data - specific yield rootzone',IOUT1,  &
                                        CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
!             SpecYieldRZDum = Max (0, RDUM(1))
             SpecYieldRZDum = RDUM(1)
             Retval = RetVal + GetVAR2 (STRING,' syss ',2,' RRRunoff_readAscii',' D-NAM Soil Parameter data - specific yield subsoil',IOUT1,  &
                                        CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
!             SpecYieldSSDum = Max (0, RDUM(1))
             SpecYieldSSDum = RDUM(1)
             Retval = RetVal + GetVAR2 (STRING,' capo ',3,' RRRunoff_readAscii',' D-NAM Soil Parameter data - capillary rise option',IOUT1, &
                                        CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
             CapOptDum = Max (0, IDUM(1))
             if (CapOptDum .eq. 0) then
                Retval = RetVal + GetVAR2 (STRING,' capc ',2,' RRRunoff_readAscii',' D-NAM Soil Parameter data - Capillary rise ',IOUT1, &
                                           CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
                CapCDum = RDUM(1)
                istart = 0
                iend   = 0
             else
                Retval = RetVal + GetVAR2 (STRING,' id ',1,' RRRunoff_readAscii',' D-NAM Soil Parameter data ',IOUT1, &
                                          CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
                CaptDum = CDUM(1)
                CapCDum = 0.D0
                ipos  = FndFrst (' capt ',String(1:),.false.)
                if (ipos .le. 0) then
                    call ErrMsgStandard (972, 0, ' D-NAM Capillary rise table option selected but no table specified (capt keyword missing) in Soil definition',NAM_SoilParameterDefinition(iRRRunoffSub))
                endif
                TempString = ''
                ipos1 = FndFrst ('TBLE',String(1:),.false.)
                ipos2 = FndFrst ('tble',String(1:),.false.) + 4
                TempString = String (ipos1:ipos2)
                LengthTable = CntStr ('<',TempString(1:))
                Call RplStr (TempString, '<', ' ')
                if (IndexNextTableStart +LengthTable-1 .gt. NcRRRunoffNAM*30) then
                    call ErrMsgStandard (972, 0, ' Dimension of D-NAM tables Capillary Rise and Percolation insufficient;',' try shorter lengths of tables')
                endif
                Read (TempString(5:),*) (NAM_CapRisPercTableGWTD(i+IndexNextTableStart-1),NAM_CapRisPercTableCapPerc(i+IndexNextTableStart-1), i=1,LengthTable)
                istart = IndexNextTableStart
                iend   = IndexNextTableStart + LengthTable -1
                indexNextTableStart = IndexNextTableStart + LengthTable
                ! check CapRis Table for positive values
                ! input check 4
                do i=istart,iend
                   if (NAM_CapRisPercTableCapPerc(i) .lt. 0 .or. NAM_CapRisPercTableGWTD(i) .lt. 0) then
                       call ErrMsgStandard (969, 0, ' D-NAM capillary rise table values should be >=0  in Soil parameter definition',NAM_SoilParameterDefinition(iRRRunoffSub))
                       Err969 = .true.
                   endif
                enddo
                ! check CapRis Table for increasing GWTD and non-increasing CapRis values
                ! input check 4
                do i=istart,iend-1
                   if (NAM_CapRisPercTableGWTD(i) .gt. NAM_CapRisPercTableGWTD(i+1) ) then
                       call ErrMsgStandard (969, 0, ' D-NAM capillary rise table values for groundwater table depth should be increasing in Soil parameter definition',NAM_SoilParameterDefinition(iRRRunoffSub))
                       Err969 = .true.
                   endif
                   if (NAM_CapRisPercTableCapPerc(i) .lt. NAM_CapRisPercTableCapPerc(i+1)) then
                       call ErrMsgStandard (969, 0, ' D-NAM capillary rise table values for capillary rise should be non-increasing in Soil parameter definition',NAM_SoilParameterDefinition(iRRRunoffSub))
                       Err969 = .true.
                   endif
                enddo
                call SetMessage(LEVEL_DEBUG, 'D-NAM CapRis table read from NAMS record')
                do i=istart,iend
                    write(iout1,*) NAM_CapRisPercTableGWTD(i), NAM_CapRisPercTableCapPerc(i)
                enddo
             endif
             Retval = RetVal + GetVAR2 (STRING,' pero ',3,' RRRunoff_readAscii',' D-NAM Soil Parameter data - Percolation option',IOUT1, &
                                        CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
             PercOptDum = Max (0, IDUM(1))
             if (PercOptDum .eq. 0) then
                Retval = RetVal + GetVAR2 (STRING,' perc ',2,' RRRunoff_readAscii',' D-NAM Soil Parameter data - percolation ',IOUT1, &
                                           CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
                PercCDum = RDUM(1)
                istart2 = 0
                iend2   = 0
             else
                Retval = RetVal + GetVAR2 (STRING,' id ',1,' RRRunoff_readAscii',' D-NAM Soil Parameter data ',IOUT1, &
                                          CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
                perctDum = CDUM(1)
                PercCDum = 0.D0
                ipos  = FndFrst (' pert ',String(1:),.false.)
                if (ipos .le. 0) then
                    call ErrMsgStandard (972, 0, ' D-NAM percolation table option selected but no table specified (pert keyword missing) in Soil definition',NAM_SoilParameterDefinition(iRRRunoffSub))
                endif
                TempString = ''
                TempString = String(ipos+1:)
                ipos1 = FndFrst ('TBLE',TempString(1:),.false.)
                ipos2 = FndFrst ('tble',TempString(1:),.false.) + 4
                TempString(1:) = TempString (ipos1:ipos2)
                LengthTable = CntStr ('<',TempString(1:))
                Call RplStr (TempString, '<', ' ')
                if (IndexNextTableStart +LengthTable-1 .gt. NcRRRunoffNAM*30) then
                    call ErrMsgStandard (972, 0, ' Dimension of D-NAM tables Capillary Rise and Percolation insufficient;',' try shorter lengths of tables')
                endif
                Read (TempString(5:),*) (NAM_CapRisPercTableGWTD(i+IndexNextTableStart-1),NAM_CapRisPercTableCapPerc(i+IndexNextTableStart-1), i=1,LengthTable)
                istart2 = IndexNextTableStart
                iend2   = IndexNextTableStart + LengthTable -1
                IndexNextTableStart = IndexNextTableStart + LengthTable
                ! check Percolation Table for increasing GWTD and non-decreasing CapRis values
                ! input check 5
                do i=istart2,iend2-1
                   if (NAM_CapRisPercTableGWTD(i) .gt. NAM_CapRisPercTableGWTD(i+1) ) then
                       call ErrMsgStandard (969, 0, ' D-NAM percolation table values for groundwater table depth should be increasing in D-NAM Soil parameter definition',NAM_SoilParameterDefinition(iRRRunoffSub))
                       Err969 = .true.
                   endif
                   if (NAM_CapRisPercTableCapPerc(i) .gt. NAM_CapRisPercTableCapPerc(i+1)) then
                       call ErrMsgStandard (969, 0, ' D-NAM percolation table values for capillary rise should be non-decreasing in D-NAM Soil parameter definition',NAM_SoilParameterDefinition(iRRRunoffSub))
                       Err969 = .true.
                   endif
                enddo
                call SetMessage(LEVEL_DEBUG, 'D-NAM Percolation table read from NAMS record')
                do i=istart2,iend2
                    write(iout1,*) NAM_CapRisPercTableGWTD(i), NAM_CapRisPercTableCapPerc(i)
                enddo
             endif
             !check all data  >= 0
             ! input check 1
             if (InfCapDum .lt. 0)  then
                call ErrMsgStandard (969, 0, ' D-NAM infiltration capacity should be >=0 in D-NAM Soil parameter definition', NAM_SoilParameterDefinition(iRRRunoffSub))
                Err969 = .true.
             endif
             ! input check 2
             if (FieldCapDum .le. 0 .or. FieldCapDum .gt. 1)  then
                call ErrMsgStandard (969, 0, ' D-NAM field capacity sfc should be >0 and <=1 in D-NAM Soil parameter definition',NAM_SoilParameterDefinition(iRRRunoffSub))
                Err969 = .true.
             endif
             ! input check 2
             if (FieldCapDum .gt. SpecYieldRzDum)  then
                call ErrMsgStandard (969, 0, ' D-NAM specific yield rootzone syrz should be >= D-NAM Field capacity sfc in D-NAM Soil definition',NAM_SoilParameterDefinition(iRRRunoffSub))
                Err969 = .true.
             endif
             ! input check 2
             if (SpecYieldRzDum .le. 0 .or. SpecYieldRzDum .gt. 1)  then
                call ErrMsgStandard (969, 0, ' D-NAM specific yield rootzone syrz should be >0 and <=1 in D-NAM Soil parameter definition',NAM_SoilParameterDefinition(iRRRunoffSub))
                Err969 = .true.
             endif
             ! input check 3
             if (SpecYieldSsDum .le. 0 .or. SpecYieldSsDum .gt. 1)  then
                call ErrMsgStandard (969, 0, ' D-NAM specific yield subsoil syss should be >0 and <=1 in D-NAM Soil parameter definition',NAM_SoilParameterDefinition(iRRRunoffSub))
                Err969 = .true.
             endif
             ! input check 4
             if (CapOptDum .eq. 0 .and.  CapCDum .lt. 0)  then
                call ErrMsgStandard (969, 0, ' D-NAM capillary rise should be >=0  in D-NAM Soil parameter definition',NAM_SoilParameterDefinition(iRRRunoffSub))
                Err969 = .true.
             endif
             ! input check 5
             if (PercOptDum .eq. 0 .and.  PercCDum .lt. 0)  then
                call ErrMsgStandard (969, 0, ' D-NAM percolation should be >=0  in D-NAM Soil parameter definition',NAM_SoilParameterDefinition(iRRRunoffSub))
                Err969 = .true.
             endif

       ! Assign definition to individual nodes
             Do iRRRunoff = 1, ncRRRunoff
               if (RRRunoff_CompOption(iRRRunoff) .eq. 3) then
                  IRRRunoffSub = RRRunoff_SubIndex(iRRRunoff)
                  Do j=1,ncnode
                     if (Einode(j,2) .eq. IRRRunoff .and. Einode(j,3) .eq. 31) Inode = j        ! kind 31= NAM
                  Enddo
                  if (StringComp(NAM_SoilParameterDefinition(IRRRunoffSub), Name, CaseSensitive) )  then
                    ReferenceToDefinition(iRRRunoffSub) = teller
                    NAM_SFC  (iRRRunoffSub) = FieldCapDum
                    NAM_SYRZ (iRRRunoffSub) = SpecYieldRZDum
                    NAM_SYSS (iRRRunoffSub) = SpecYieldSSDum
                    NAM_InfCap  (iRRRunoffSub) = InfCapDum
                    NAM_CapRisOpt   (iRRRunoffSub) = CapOptDum
                    NAM_CapRisConst (iRRRunoffSub) = CapCDum
                    NAM_CapRisTableStart (iRRRunoffSub) = istart
                    NAM_CapRisTableEnd   (iRRRunoffSub) = iend
                    NAM_PercOpt  (iRRRunoffSub) = PercOptDum
                    NAM_PercConst(iRRRunoffSub) = PercCDum
                    NAM_PercTableStart (iRRRunoffSub) = istart2
                    NAM_PercTableEnd   (iRRRunoffSub) = iend2
                    If (Err969) Write(iout1,*) ' This input is used for node ', ID_Nod(Inode)(1:len_trim(id_nod(inode)))
                  endif
               endif
             Enddo
           Endif
          Endif
          CALL SKPCOM (Infile1, ENDFIL, 'ODS')
         Enddo
    2112 Continue

         If (RetVal .gt. 0 .or. Err969) call ErrMsgStandard (972, 0, ' Error reading data from RR-runoff file ', ' Error getting NAMS records')
         Err969 = .false.
         Do iRRRunoff = 1, ncRRRunoff
           IRRRunoffSub = RRRunoff_SubIndex(IRRRunoff)
           if (RRRunoff_CompOption(IRRRunoff) .eq.3) then
               if (ReferenceToDefinition(iRRRunoffSub) .eq. 0 .and. NAM_SoilParameterDefinition (iRRRunoffSub) .ne. '') then
                  Err969 = .true.
                  call ErrMsgStandard (969, 0, ' D-NAM Soil Parameter definition not found in RRRunoff file.', NAM_SoilParameterDefinition(iRRRunoffSub))
               endif
           endif
         Enddo
         If (Err969) call ErrMsgStandard (972, 0, ' Not enough D-NAM Soil parameter data found', &
                                  ' Some NAMS Definitions not present in RRRunoff file')


         Rewind(infile1)
         call SetMessage(LEVEL_INFO, 'Read D-NAM data - Level Depth Definition')
         Err969 = .false.

         RetVal = 0
         ReferenceToDefinition = 0
         endfil = .false.
         teller = 0
         CALL SKPCOM (Infile1, ENDFIL, 'ODS')
         Do while (.not. endfil)
           READ (Infile1,'(A1000)',END=2212,ERR=150,IOSTAT=IECODE) STRING
           IF (STRING(1:4) .EQ. 'NAML') Then
           ! Read NAM level and depth definition  id
           Retval = RetVal + GetVAR2 (STRING,' id ',1,' RRRunoff_readAscii',' D-NAM Parameter definition data',IOUT1, &
                                      CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
           name = CDUM(1)
           teller = teller + 1
           ! Eerst testen of NAM level depth  definition wel gebruikt wordt, dan pas verwerken
           IRRRunoff = FindString (NcRRRunoffNAM, NAM_LevelAndDepthDefinition, Name, NcRRRunoffNAM, CaseSensitive)
           Occurs = (IRRRunoff .gt. 0)
           if (IRRRunoff .gt. 0) then
              IRRRunoffSub = IRRRunoff
              if (ReferenceToDefinition(iRRRunoffSub) .gt. 0) then
                 call SetMessage(LEVEL_ERROR, 'D-NAM Level and Depth Definition '//name(1:Len_Trim(Name))//' double in datafile Runoff nodes')
                 LevelError = .true.
                 Occurs = .false.    ! voorkomt verdere verwerking
              else
                 ! cleaning RR files
                 If (CleanRRFiles) write(Iounit,'(A)') String (1:len_trim(String))
              endif
           endif
       ! Verwerk NAM Level and Depth definition
           if (occurs) then
       ! Read parameters
             Retval = RetVal + GetVAR2 (STRING,' sl ',2,' RRRunoff_readAscii',' D-NAM Level Depth data',IOUT1, &
                                        CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
             Sldum = RDUM(1)
             Retval = RetVal + GetVAR2 (STRING,' rzbl ',2,' RRRunoff_readAscii',' D-NAM Level Depth data',IOUT1, &
                                        CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
             rzblDum = RDUM(1)
             Retval = RetVal + GetVAR2 (STRING,' gwsbl ',2,' RRRunoff_readAscii',' D-NAM Level Depth data',IOUT1, &
                                        CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
             gwsbldum = RDUM(1)
             Retval = RetVal + GetVAR2 (STRING,' ui ',2,' RRRunoff_readAscii',' D-NAM Level Depth data',IOUT1, &
                                        CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
             u0dum = RDUM(1)
             Retval = RetVal + GetVAR2 (STRING,' li ',2,' RRRunoff_readAscii',' D-NAM Level Depth data',IOUT1, &
                                        CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
             l0dum = RDUM(1)
             Retval = RetVal + GetVAR2 (STRING,' gwsdi ',2,' RRRunoff_readAscii',' D-NAM Level Depth data',IOUT1, &
                                        CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
             Gwd0dum = RDUM(1)
             ! input check 8, 9, 10 (part)
             if (u0dum .lt. 0 .or. l0dum .lt. 0 .or. gwd0dum .lt. 0) then
                call ErrMsgStandard (969, 0, ' Initial depths Ui, Li and GWSDi should be >=0 in D-NAM Level Depth definition', NAM_LevelAndDepthDefinition(iRRRunoffSub))
                Err969 = .true.
             endif
             !U0 >= 0    already taken care of
             !lo >= 0    already taken care of
             !gwd0>= 0   already taken care of
             !check Surface Level >= Bed level Root zone >= Bed level Subsoil
             ! input check 6
             if (sldum .lt. rzbldum .or. rzbldum .lt. gwsbldum)  then
                call ErrMsgStandard (969, 0, ' D-NAM surface level should be >= bed level root zone >= bed level subsoil in D-NAM Level Depth definition', NAM_LevelAndDepthDefinition(iRRRunoffSub))
                Err969 = .true.
             endif
             ! input check 7
             if (sldum - rzbldum .lt. 0.10)  then
                call ErrMsgStandard (969, 0, ' (non-fatal error): Root zone thickness less than 0.10 m in level-depth definition, which is unlikely; definition= ', NAM_LevelAndDepthDefinition(iRRRunoffSub))
             endif

       ! Assign definition to individual nodes
             Do iRRRunoff = 1, ncRRRunoff
               if (RRRunoff_CompOption(iRRRunoff) .eq. 3) then
                  IRRRunoffSub = RRRunoff_SubIndex(iRRRunoff)
                  Do j=1,ncnode
                     if (Einode(j,2) .eq. IRRRunoff .and. Einode(j,3) .eq. 31) Inode = j        ! kind 31= NAM
                  Enddo
                  if (StringComp(NAM_LevelandDepthDefinition(IRRRunoffSub), Name, CaseSensitive) )  then
                    If (Err969) Write(iout1,*) ' This input is used for node ', ID_Nod(Inode)(1:len_trim(id_nod(inode)))
                    ReferenceToDefinition(iRRRunoffSub) = teller
                    NAM_SurfaceLevel(iRRRunoffSub) = sldum
                    NAM_RZBL    (iRRRunoffSub) = rzblDum
                    NAM_GWSBL   (iRRRunoffSub) = Gwsbldum
                    NAM_U0      (iRRRunoffSub) = U0dum
                    NAM_L0      (iRRRunoffSub) = L0dum
                    NAM_GWD0    (iRRRunoffSub) = Gwd0dum
!                   Write(*,*) ' L0dum ', L0dum
!                   Write(*,*) ' GWD0Dum ', GWD0dum
!                   Write(*,*) ' U0Dum ', U0dum
                    ! input check 9
                    ddum = 1000.D0 * NAM_SFC(iRRRunoffSub) * (SlDum - RzblDum)
                    eps  = 1.D-7
                    idum(1) = D_IfReal (Dble(L0dum), Ddum, eps)
                    if (idum(1) .gt. 0) then
                       Write(Iout1,*) ' NAM_LInitial ', L0dum
                       Write(Iout1,*) ' Sfc          ', NAM_SFC(iRRRunoffSub)
                       Write(Iout1,*) ' SL - RZBL    ', Sldum - RzblDum
                       Write(Iout1,*) ' Lmax value   ', Ddum
                       call ErrMsgStandard (969, 0, ' NAM L_initial should be <= 1000.* sfc (SL - RZBL) for D-NAM Node with id: ',ID_Nod(inode)(1:len_trim(Id_Nod(inode))) )
                       Err969 = .true.
                    else
                       ! to prevent rounding off errors
                       L0Dum = min (L0dum, real(ddum))
                    endif
                    ! input check 10
                    ddum = 1000.D0 * NAM_SYSS(iRRRunoffSub) * (RzblDum-GwsblDum) + &
                             1000.D0* (NAM_SYRZ(iRRRunoffSub) - NAM_SFC(IRRRunoffSub)) * (SlDum-RzblDum)
                    idum(1) = D_IfReal (Dble(Gwd0dum), Ddum, eps)
                    if (idum(1) .gt. 0) then
                       Write(Iout1,*) ' GwsdInitial ', Gwd0dum
                       Write(Iout1,*) ' SySS        ', NAM_SYSS(iRRRunoffSub)
                       Write(Iout1,*) ' RZBL - GWSBL', Rzbldum - GwsblDum
                       Write(Iout1,*) ' SyRZ - Sfc  ', NAM_SYRZ(iRRRunoffSub) - NAM_SFC(iRRRunoffSub)
                       Write(Iout1,*) ' SL - RZBL   ', SLdum - RZBLDum
                       Write(Iout1,*) ' max allowed ', 1000.D0 * NAM_SYSS(iRRRunoffSub) * (RzblDum-GwsblDum) + 1000.D0* (NAM_SYRZ(iRRRunoffSub) - NAM_SFC(IRRRunoffSub)) * (SlDum-RzblDum)
                       call ErrMsgStandard (969, 0, ' NAM GWSD_initial should be <= 1000.* syss (RZBL-GWSBL) + 1000.*(syrz-sfc) * (SL-RZBL) for D-NAM Node with id: ',&
                                      ID_Nod(inode)(1:len_trim(Id_Nod(inode))) )
                       Err969 = .true.
                    else
                       ! to prevent rounding off errors
                       Gwd0Dum = min (Gwd0dum, real(ddum))
                    endif
                  endif
               endif
             Enddo
           Endif
          Endif
          CALL SKPCOM (Infile1, ENDFIL, 'ODS')
         Enddo
    2212 Continue

          If (RetVal .gt. 0 .or. Err969) call ErrMsgStandard (972, 0, ' Error reading data from RR-runoff file ', ' Error getting NAML records')
          Err969 = .false.
          Do iRRRunoff = 1, ncRRRunoff
            IRRRunoffSub = RRRunoff_SubIndex(IRRRunoff)
            if (RRRunoff_CompOption(IRRRunoff) .eq.3) then
                if (ReferenceToDefinition(iRRRunoffSub) .eq. 0 .and. NAM_LevelAndDepthDefinition (iRRRunoffSub) .ne. '')  then
                   Err969 = .true.
                   call ErrMsgStandard (969, 0, ' D-NAM Level Depth definition not found in RRRunoff file.',NAM_LevelandDepthDefinition(iRRRunoffSub))
                endif
            endif
          Enddo
          If (Err969) call ErrMsgStandard (972, 0, ' Not enough D-NAM Level-Depth data found', ' Some NAML Definitions not present in RRRunoff file')


! NAM parameters surface runoff
         Rewind(infile1)
         call SetMessage(LEVEL_INFO, 'Read D-NAM data - Parameters  - Surface Runoff')
         Err969 = .false.

         RetVal = 0
         ReferenceToDefinition = 0
         endfil = .false.
         teller = 0
         CALL SKPCOM (Infile1, ENDFIL, 'ODS')
         Do while (.not. endfil)
           READ (Infile1,'(A1000)',END=2512,ERR=150,IOSTAT=IECODE) STRING
           IF (STRING(1:4) .EQ. 'NAMR') Then
           ! Read NAM parameter surface runoff definition  id
           Retval = RetVal + GetVAR2 (STRING,' id ',1,' RRRunoff_readAscii',' D-NAM Parameter definition data',IOUT1, &
                                      CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
           name = CDUM(1)
           teller = teller + 1
           ! Eerst testen of NAM parameter runoff definition wel gebruikt wordt, dan pas verwerken
           IRRRunoff = FindString (NcRRRunoffNAM, NAM_SurfaceRunoffDefinition, Name, NcRRRunoffNAM, CaseSensitive)
           Occurs = (IRRRunoff .gt. 0)
           if (IRRRunoff .gt. 0) then
              IRRRunoffSub = IRRRunoff
              if (ReferenceToDefinition(iRRRunoffSub) .gt. 0) then
                 call SetMessage(LEVEL_ERROR, 'D-NAM Surface Runoff Definition '//name(1:Len_Trim(Name))//' double in datafile Runoff nodes')
                 LevelError = .true.
                 Occurs = .false.    ! voorkomt verdere verwerking
              else
                 ! cleaning RR files
                 If (CleanRRFiles) write(Iounit,'(A)') String (1:len_trim(String))
              endif
           endif
       ! Verwerk NAM Parameter definition
           if (occurs) then
       ! Read parameters
             Retval = RetVal + GetVAR2 (STRING,' cl ',2,' RRRunoff_readAscii',' D-NAM Parameter data',IOUT1, &
                                        CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
             CLdum = RDUM(1)
             Retval = RetVal + GetVAR2 (STRING,' ss ',2,' RRRunoff_readAscii',' D-NAM Parameter data',IOUT1, &
                                        CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
             SSDum = RDUM(1)
             Retval = RetVal + GetVAR2 (STRING,' man ',2,' RRRunoff_readAscii',' D-NAM Parameter data',IOUT1, &
                                        CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
             ManDum = RDUM(1)
             Retval = RetVal + GetVAR2 (STRING,' utof ',2,' RRRunoff_readAscii',' D-NAM Parameter data',IOUT1, &
                                        CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
             UtofDum = RDUM(1)
             Retval = RetVal + GetVAR2 (STRING,' ckif ',2,' RRRunoff_readAscii',' D-NAM Parameter data',IOUT1, &
                                        CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
             CkifDum = RDUM(1)
             Retval = RetVal + GetVAR2 (STRING,' utif ',2,' RRRunoff_readAscii',' D-NAM Parameter data',IOUT1, &
                                        CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
             UtifDum = RDUM(1)
             Retval = RetVal + GetVAR2 (STRING,' ltif ',2,' RRRunoff_readAscii',' D-NAM Parameter data',IOUT1, &
                                        CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
             LtifDum = RDUM(1)
             !check all input data for validity: data  >= 0 and other checks
             ! input check 12
             if (cldum .le. 0) then
                call ErrMsgStandard (969, 0, ' Catchment length NAM [m] should be > 0 in D-NAM surface runoff definition',NAM_SurfaceRunoffDefinition(iRRRunoffSub))
                Err969 = .true.
             endif
             ! input check 13
             if (SSDum .lt. 0) then
                call ErrMsgStandard (969, 0, ' Surface slope [-] NAM should be >= 0 in D-NAM surface runoff definition ',NAM_SurfaceRunoffDefinition(iRRRunoffSub))
                Err969 = .true.
             endif
             ! input check 14
             if (ManDum .le. 0 .or. ManDum .gt. 1) then
                call ErrMsgStandard (969, 0, ' Manning coefficient NAM should be >0 and have realistic value in D-NAM surface runoff definition', NAM_SurfaceRunoffDefinition(iRRRunoffSub))
                Err969 = .true.
             endif
             ! input check 15
             if (UtofDum .lt. 0) then
                call ErrMsgStandard (969, 0, ' Surface Storage Threshold Utof NAM should be >= 0 in D-NAM surface runoff definition ',NAM_SurfaceRunoffDefinition(iRRRunoffSub))
                Err969 = .true.
             endif
             ! input check 16
             if (CkifDum .lt. 1) then
                call ErrMsgStandard (969, 0, ' Infiltration reservoir coefficient CKIF NAM should be > 1 in D-NAM surface runoff definition', NAM_SurfaceRunoffDefinition(iRRRunoffSub))
                Err969 = .true.
             endif
             ! input check 15 / 17
             if (utifdum .lt. 0 .or. ltifdum .lt. 0)  then
                call ErrMsgStandard (969, 0, ' Surface and Lower zone thresholds for Interflow Utif and Ltif D-NAM should be > 0 in D-NAM  surface runoff definition', NAM_SurfaceRunoffDefinition(iRRRunoffSub))
                Err969 = .true.
             endif
             !check UTOF>=UTIF
             ! input check 15
             if (UtofDum .lt. UTifdum) then
                call ErrMsgStandard (969, 0, ' Surface storage threshold overland flow UTOF should be >= threshold for interflow UTIF in D-NAM surface runoff definition ', NAM_SurfaceRunoffDefinition(iRRRunoffSub))
                Err969 = .true.
             endif

       ! Assign definition to individual nodes
             Do iRRRunoff = 1, ncRRRunoff
                if (RRRunoff_CompOption(iRRRunoff) .eq. 3) then
                   IRRRunoffSub = RRRUnoff_SubIndex(iRRRunoff)
                   Do j=1,ncnode
                      if (Einode(j,2) .eq. IRRRunoff .and. Einode(j,3) .eq. 31) Inode = j        ! kind 31= NAM
                   Enddo
                   if (StringComp(NAM_SurfaceRUnoffDefinition(IRRRunoffSub), Name, CaseSensitive) )  then
                      ReferenceToDefinition(iRRRunoffSub) = teller
                      NAM_CatchmentLength(iRRRunoffSub) = cldum
                      NAM_SurfaceSlope   (iRRRunoffSub) = ssdum
                      NAM_Manning_n      (iRRRunoffSub) = Mandum
                      NAM_ckif (iRRRunoffSub) = ckifdum
                      NAM_UTOF (iRRRunoffSub) = utofdum
                      NAM_UTIF (iRRRunoffSub) = utifdum
                      NAM_LTIF (iRRRunoffSub) = ltifdum
                      If (Err969) Write(Iout1,*) ' This input is used for node ', ID_Nod(Inode)(1:len_trim(id_nod(inode)))
                   endif
                endif
             Enddo
           Endif
          Endif
          CALL SKPCOM (Infile1, ENDFIL, 'ODS')
         Enddo
    2512 Continue

          If (RetVal .gt. 0 .or. Err969) call ErrMsgStandard (972, 0, ' Error reading data from RR-runoff file ', ' Error getting NAMR records')
          Err969 = .false.
          Do iRRRunoff = 1, ncRRRunoff
             IRRRunoffSub = RRRunoff_SubIndex(IRRRunoff)
             if (RRRunoff_CompOption(IRRRunoff) .eq. 3) then
                if (ReferenceToDefinition(iRRRunoffSub) .eq. 0 .and. NAM_SurfaceRunoffDefinition (iRRRunoffSub) .ne. '')  then
                    Err969 = .true.
                    call ErrMsgStandard (969, 0, ' D-NAM Surface Runoff definition not found in RRRunoff file.',NAM_SurfaceRunoffDefinition(iRRRunoffSub))
                endif
             endif
          Enddo
          If (Err969) call ErrMsgStandard (972, 0, ' Not enough D-NAM Surface runoff data found', ' Some NAMR Definitions not present in RRRunoff file')


! NAM parameters base flow
         Rewind(infile1)
         call SetMessage(LEVEL_INFO, 'Read D-NAM data - Parameters  - Base Flow')
         Err969 = .false.

         RetVal = 0
         ReferenceToDefinition = 0
         endfil = .false.
         teller = 0
         CALL SKPCOM (Infile1, ENDFIL, 'ODS')
         Do while (.not. endfil)
           READ (Infile1,'(A1000)',END=2612,ERR=150,IOSTAT=IECODE) STRING
           IF (STRING(1:4) .EQ. 'NAMB') Then
           ! Read NAM parameter base flow definition
           Retval = RetVal + GetVAR2 (STRING,' id ',1,' RRRunoff_readAscii',' D-NAM Parameter definition data',IOUT1, &
                                      CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
           name = CDUM(1)
           teller = teller + 1
           ! Eerst testen of NAM base flow definition wel gebruikt wordt, dan pas verwerken
           IRRRunoff = FindString (NcRRRunoffNAM, NAM_BaseFlowDefinition, Name, NcRRRunoffNAM, CaseSensitive)
           Occurs = (IRRRunoff .gt. 0)
           if (IRRRunoff .gt. 0) then
              IRRRunoffSub = IRRRunoff
              if (ReferenceToDefinition(iRRRunoffSub) .gt. 0) then
                 call SetMessage(LEVEL_ERROR, 'D-NAM BaseFlow Definition '//name(1:Len_Trim(Name))//' double in datafile Runoff nodes')
                 LevelError = .true.
                 Occurs = .false.    ! voorkomt verdere verwerking
              else
                 ! cleaning RR files
                 If (CleanRRFiles) write(Iounit,'(A)') String (1:len_trim(String))
              endif
           endif
       ! Verwerk NAM Base Flow definition
           if (occurs) then
       ! Read parameters
             Retval = RetVal + GetVAR2 (STRING,' ckfastbf ',2,' RRRunoff_readAscii',' D-NAM Parameter data',IOUT1, &
                                        CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
             Ckfastbfdum = RDUM(1)
             Retval = RetVal + GetVAR2 (STRING,' ckslowbf ',2,' RRRunoff_readAscii',' D-NAM Parameter data',IOUT1, &
                                        CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
             ckslowbfDum = RDUM(1)
             Retval = RetVal + GetVAR2 (STRING,' ckgwinf ',2,' RRRunoff_readAscii',' D-NAM Parameter data',IOUT1, &
                                        CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
             ckgwinfdum = RDUM(1)
             Retval = RetVal + GetVAR2 (STRING,' ltp ',2,' RRRunoff_readAscii',' D-NAM Parameter data',IOUT1, &
                                        CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
             ltpdum = RDUM(1)
             Retval = RetVal + GetVAR2 (STRING,' tfastbf ',2,' RRRunoff_readAscii',' D-NAM Parameter data',IOUT1, &
                                        CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
             tfastbfdum = RDUM(1)
             Retval = RetVal + GetVAR2 (STRING,' tslowbf ',2,' RRRunoff_readAscii',' D-NAM Parameter data',IOUT1, &
                                        CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
             tslowbfDum = RDUM(1)
             !check data
             if (ltpdum .lt. 0) then
                call ErrMsgStandard (969, 0, ' Lower Zone Threshold Percolation NAM should be >= 0 in D-NAM baseflow definition',NAM_BaseFlowDefinition(iRRRunoffSub))
                Err969 = .true.
             endif
             !check CKGWInf >= CKSLOWBF >= CKFASTBF >=1
             if (CKSlowBFDum .gt. CKGWInfDum .or. CKFastBFDum .gt. CKSlowBFDUm .or. CKFastBFDum .lt. 1D0) then
                call ErrMsgStandard (969, 0, ' Base Flow parameters should be CKGwInf >= CkSlowBF >= CKFastBF >=1 in D-NAM baseflow definition ', NAM_BaseFlowDefinition(iRRRunoffSub))
                Err969 = .true.
             endif
             !check TFastBF>=TSlowBf
             if (TFastBFDum .lt. TSlowBFDUm) then
                call ErrMsgStandard (969, 0, ' Base Flow thresholds should be TFastBF>=TSlowBF in D-NAM baseflow definition ', NAM_BaseFlowDefinition(iRRRunoffSub))
                Err969 = .true.
             endif

       ! Assign definition to individual nodes
             Do iRRRunoff = 1, ncRRRunoff
                if (RRRunoff_CompOption(iRRRunoff) .eq. 3) then
                   IRRRunoffSub = RRRUnoff_SubIndex(iRRRunoff)
                   Do j=1,ncnode
                      if (Einode(j,2) .eq. IRRRunoff .and. Einode(j,3) .eq. 31) Inode = j        ! kind 31= NAM
                   Enddo
                   if (StringComp(NAM_BaseFlowDefinition(IRRRunoffSub), Name, CaseSensitive) )  then
                      ReferenceToDefinition(iRRRunoffSub) = teller
                      NAM_CKFastBF(iRRRunoffSub) = ckfastbfdum
                      NAM_CKSlowBF(iRRRunoffSub) = ckslowbfdum
                      NAM_CKGwInflow (iRRRunoffSub) = ckgwinfdum
                      NAM_Ltg (iRRRunoffSub) = ltpdum
                      NAM_TFastBf (iRRRunoffSub) = tfastbfdum
                      NAM_TSlowBf (iRRRunoffSub) = tslowbfdum
                      If (Err969) Write(Iout1,*) ' This input is used for node ', ID_Nod(Inode)(1:len_trim(id_nod(inode)))
                   endif
                endif
             Enddo
           Endif
          Endif
          CALL SKPCOM (Infile1, ENDFIL, 'ODS')
         Enddo
    2612 Continue

          If (RetVal .gt. 0 .or. Err969) call ErrMsgStandard (972, 0, ' Error reading data from RR-runoff file ', ' Error getting NAMB records')
          Err969 = .false.
          Do iRRRunoff = 1, ncRRRunoff
             IRRRunoffSub = RRRunoff_SubIndex(IRRRunoff)
             if (RRRunoff_CompOption(IRRRunoff) .eq. 3) then
                if (ReferenceToDefinition(iRRRunoffSub) .eq. 0 .and. NAM_SurfaceRunoffDefinition (iRRRunoffSub) .ne. '')  then
                    Err969 = .true.
                    call ErrMsgStandard (969, 0, ' D-NAM Surface Runoff definition not found in RRRunoff file.', NAM_SurfaceRunoffDefinition(iRRRunoffSub))
                endif
             endif
          Enddo
          If (Err969) call ErrMsgStandard (972, 0, ' Not enough D-NAM Surface runoff data found', ' Some NAMR Definitions not present in RRRunoff file')


! NAM parameters groundwater forcing
         Rewind(infile1)
         call SetMessage(LEVEL_INFO, 'Read D-NAM data - Groundwater')
         Err969 = .false.

         NrPumpTimeTablesNeeded = 0
         RetVal = 0
         ReferenceToDefinition = 0
         endfil = .false.
         teller = 0
         CALL SKPCOM (Infile1, ENDFIL, 'ODS')
         Do while (.not. endfil)
           READ (Infile1,'(A1000)',END=2712,ERR=150,IOSTAT=IECODE) STRING
           IF (STRING(1:4) .EQ. 'NAMG') Then
           ! Read NAM parameter gw definition  id
           Retval = RetVal + GetVAR2 (STRING,' id ',1,' RRRunoff_readAscii',' D-NAM Parameter gw definition data',IOUT1, &
                                      CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
           name = CDUM(1)
           teller = teller + 1
           ! Eerst testen of NAM parameter groundwater definition wel gebruikt wordt, dan pas verwerken
           IRRRunoff = FindString (NcRRRunoffNAM, NAM_GroundwaterForcingDefinition, Name, NcRRRunoffNAM, CaseSensitive)
           Occurs = (IRRRunoff .gt. 0)
           if (IRRRunoff .gt. 0) then
!              IRRRunoffSub = RRRunoff_SubIndex(IRRRunoff)
              IRRRunoffSub = IRRRunoff
              if (ReferenceToDefinition(iRRRunoffSub) .gt. 0) then
                 call SetMessage(LEVEL_ERROR, 'D-NAM Parameter gw Definition '//name(1:Len_Trim(Name))//' double in datafile Runoff nodes')
                 LevelError = .true.
                 Occurs = .false.    ! voorkomt verdere verwerking
              else
              endif
           endif
       ! Verwerk NAM Parameter definition
           ipos = 0
           if (occurs) then
       ! Read parameters
             Retval = RetVal + GetVAR2 (STRING,' po ',3,' RRRunoff_readAscii',' D-NAM Parameter data',IOUT1, &
                                        CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
             poDum = IDUM(1)
             if (Podum .eq. 0) then
                Retval = RetVal + GetVAR2 (STRING,' pu ',2,' RRRunoff_readAscii',' D-NAM Parameter data',IOUT1, &
                                          CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
                puDum = RDUM(1)
                ptDum = ''
                if (CleanRRFiles) then
                  lenstring = len_trim(String)
                  ipos  = FndFrst (' pt ',String(1:lenstring),.false.)
                  Retval = RetVal + GetVAR2 (STRING,' id ',1,' RRRunoff_readAscii',' D-NAM Parameter data',IOUT1, &
                                          CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
                  if (ipos .gt. 0) ptDum = CDUM(1)
                endif
             else
               ! zet time table name gelijk aan het id
               puDum = 0.
               Retval = RetVal + GetVAR2 (STRING,' id ',1,' RRRunoff_readAscii',' D-NAM Parameter data',IOUT1, &
                                          CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
               ptDum = CDUM(1)
             endif

             !check all input data for validity: data  >= 0 and other checks
             if (poDum .ne. 0 .and. poDum .ne. 1) then
                call ErrMsgStandard (969, 0, ' Pumping option should be 0 or 1 in D-NAM parameter definition ',NAM_GroundwaterForcingDefinition(iRRRunoffSub))
                Err969 = .true.
             endif

             if (CleanRRFiles) then
                lenstring = len_trim(String)
                ipos  = FndFrst (' pt ',String(1:lenstring),.false.)
             endif

             if (poDum .ge. 1 .or. ipos .gt. 0) then    ! when cleaning RR files, also handle case that podum=0 but table is defined in input
                 NrPumpTimeTablesNeeded = NrPumpTimeTablesNeeded+1
             elseif (CleanRRFiles) then
                 ! cleaning RR files for NAMG record without pump with closing string namg on same line (1 line only)
                 ipos  = FndFrst (' namg',String(5:lenstring),.false.)
                 if (ipos .gt. 0) write(Iounit,'(A)') String (1:len_trim(String))
!                NAMG records with pump require special treat below
             endif

       ! Assign definition to individual nodes
             Do iRRRunoff = 1, ncRRRunoff
                if (RRRunoff_CompOption(iRRRunoff) .eq. 3) then
                   IRRRunoffSub = RRRUnoff_SubIndex(iRRRunoff)
                   Do j=1,ncnode
                      if (Einode(j,2) .eq. IRRRunoff .and. Einode(j,3) .eq. 31) Inode = j        ! kind 31= NAM
                   Enddo
                   if (StringComp(NAM_GroundwaterForcingDefinition(IRRRunoffSub), Name, CaseSensitive) )  then
                      ReferenceToDefinition(iRRRunoffSub) = teller
                      NAMPumpOption (iRRRunoffSub)    = podum
                      NAMPumpTable  (iRRRunoffSub)    = ptdum
                      NAMPumpFlow   (iRRRunoffSub)    = pudum
                      If (Err969) Write(Iout1,*) ' This input is used for node ', ID_Nod(Inode)(1:len_trim(id_nod(inode)))
                   endif
                endif
             Enddo
           Endif
          Endif
          CALL SKPCOM (Infile1, ENDFIL, 'ODS')
         Enddo
    2712 Continue

          If (RetVal .gt. 0 .or. Err969) call ErrMsgStandard (972, 0, ' Error reading data from RR-runoff file ', ' Error getting NAMG records')
          Err969 = .false.
          Do iRRRunoff = 1, ncRRRunoff
             IRRRunoffSub = RRRunoff_SubIndex(IRRRunoff)
             if (RRRunoff_CompOption(IRRRunoff) .eq. 3) then
                if (ReferenceToDefinition(iRRRunoffSub) .eq. 0 .and. NAM_GroundwaterForcingDefinition (iRRRunoffSub) .ne. '')  then
                    Err969 = .true.
                    call ErrMsgStandard (969, 0, ' D-NAM Parameters gw definition not found in RRRunoff file.', NAM_GroundwaterForcingDefinition(iRRRunoffSub))
                endif
             endif
          Enddo
          If (Err969) call ErrMsgStandard (972, 0, ' Not enough D-NAM Parameters data found', ' Some NAMG Definitions not present in RRRunoff file')

 ! Reading of pumping time table, if somewhere it is defined
         If (NrPumpTimeTablesNeeded .gt. 0) then
!           Read NAMG records again for pump time tables
            rewind (infile1)
            endfil = (NrPumpTimeTablesNeeded .eq. 0)
            if (.not. endfil) call SetMessage(LEVEL_INFO, 'Read NAMG records time tables')
            if (idebug .ne. 0) write(idebug,*) ' Read NAMG records'
            if (.not. endfil) Call SKPCOM (Infile1, ENDFIL,'ODS')
            Do while (.not. endfil)
               Success = GetRecord(Infile1, 'NAMG', Endfil, idebug, Iout1)     ! get record van keyword NAMG tot namg, zet in buffer
               IF (.not. success) GOTO 3113
               IF (ENDFIL) GOTO 3113
               Success = GetStringFromBuffer (KeepBufString)
               IF (.not. Success .and. CleanRRFiles)   then
                   Call ErrMsgStandard (999, 3, ' Local buffer RRRunoffmodule NAMG record D-NAM too small', ' Input skipped')
                   GOTO 3113
               Endif
               Success = GetTableName (TabYesNo, TableName, ' id ', Iout1)     ! get table name via keyword ' id ', TabYesNo=TBLE found
               IF (.not. success) GOTO 3113
               If (TabYesNo .and. TableName .ne. '') Then
!                 Er is een tabel gedefinieerd, met een niet-lege naam
!                 Eerst testen of tabel definition wel gebruikt wordt, dan pas verwerken
                  NrColumns = 1
                  INAM = FindString (NcRRRunoffNAM, NAMPumpTable, TableName, NcRRRunoffNAM, CaseSensitive)
                  Occurs = (INAM .gt. 0)
                  if (INAM .gt. 0) then
                     if (NAMRefToGWPump_TTable(iNAM) .gt. 0) then
                       call SetMessage(LEVEL_ERROR, 'D-NAM Pump Table Definition '//Tablename(1:Len_trim(TableName))//' double in datafile Sacrmnto.3b')
                       LevelError = .true.
                       NrColumns = 0  ! voorkomt verdere verwerking
                     endif
                  endif
!                 Verwerken NAM pump table definition
                  if (occurs .and. NrColumns .gt. 0) then
!         Get table with name TableName, Nrcolumns data fields, result in global arrays; tabel nummer is TableNr
                    Success = GetTable (TableHandle, TableName, NrColumns, TableNr, idebug, Iout1)
                    IF (.not. success) GOTO 3113
                    ! clean RR files
                    If (CleanRRFiles) then
                      ! use KeepBufString to write to file
                      ! first till TBLE
                      ! then till < characters
                      ! then till the end of the buffer string
                      lenstring = len_trim(KeepBufString)
                      ipos  = FndFrst ('TBLE ',KeepBufString(1:lenstring),.false.)
                      if (ipos .gt. 0) then
                         write(Iounit,'(A)') KeepBufString (1:ipos+4)
                         KeepBufString(1:) = KeepBufString(ipos+5:)
                      else
                         ! error: no TBLE found
                           call SetMessage(LEVEL_ERROR, 'NAMG Table Definition '//Tablename(1:Len_trim(TableName))//' TBLE not found')
                      endif
 1041                 continue
                      lenstring = len_trim(KeepBufString)
                      ipos  = FndFrst (' < ',KeepBufString(1:lenstring),.false.)
                      if (ipos .gt. 0) then
                         write(Iounit,'(A)') KeepBufString (1:ipos+2)
                         KeepBufString(1:) = KeepBufString(ipos+3:)
                         goto 1041
                      else
                         ! write remaining part
                         write(Iounit,'(A)') KeepBufString (1:lenstring)
                      endif
                    Endif
!         Set references
                    Do iNAM = 1, ncRRRunoffNAM
                      if (StringComp (NAMPumpTable(INAM), TableName, CaseSensitive) ) NAMRefToGWPump_TTable(iNam) = TableNr
                    Enddo
                  Endif
               Endif
               Call SKPCOM (Infile1, ENDFIL,'ODS')
             Enddo
3113       Continue
         Endif
 !
 !check consistency different records SL and TFastBF
       Err969All = .false.
       Do iRRRunoff=1,NcRRRunoff
          IRRRunoffSub = RRRunoff_SubIndex(IRRRunoff)
          Do j=1,ncnode
             if (Einode(j,2) .eq. IRRRunoff .and. Einode(j,3) .eq. 31) Inode = j        ! kind 31= NAM
          Enddo
          Err969 = .false.
          If (RRRunoff_CompOption(IRRRunoff) .eq. 3) then  !NAM
             NAM_LMax(iRRRunoffSub) = 1000.D0 * NAM_SFC(iRRRunoffSub) * (NAM_SurfaceLevel(IRRRunoffSub) - NAM_RZBL(IRRRunoffSub))
             NAM_GWSDRzMax(iRRRunoffSub) = 1000.D0 * (NAM_SYRZ(iRRRunoffSub) - NAM_SFC(IRRRunoffSub)) * (NAM_SurfaceLevel(IRRRunoffSub) - NAM_RZBL(IRRRunoffSub))
             NAM_GWSDSSMax(iRRRunoffSub) = 1000.D0 * NAM_SYSS(iRRRunoffSub) * (NAM_RZBL(IRRRunoffSub) - NAM_GWSBL(IRRRunoffSub))
             NAM_GWSDMax(iRRRunoffSub) = NAM_GWSDRZMax(iRRRunoffSub) +  NAM_GWSDSSMax(iRRRunoffSub)
             if (NAM_SurfaceLevel(iRRRunoffSub) .lt. NAM_TFastBF(iRRRunoffSub)) then
                call ErrMsgStandard (969, 0, ' Threshold Fast BaseFlow should be below Surface level for node with id: ',ID_Nod(inode)(1:len_trim(Id_Nod(inode))) )
                Err969 = .true.
             endif
             if (NAM_GWSBL(iRRRunoffSub) .ge. NAM_TSlowBF(iRRRunoffSub)) then
                call ErrMsgStandard (969, 0, ' Threshold Slow BaseFlow should be above Groundwater storage bed level for node with id: ',ID_Nod(inode)(1:len_trim(Id_Nod(inode))) )
                Err969 = .true.
             endif
             if (NAM_LTIF(iRRRunoffSub) .ge. NAM_LMAX(iRRRunoffSub)) then
                call ErrMsgStandard (969, 0, ' Threshold Lower Zone Interflow LTif should be <= Maximum lower zone content Lmax for node with id: ', ID_Nod(inode)(1:len_trim(Id_Nod(inode))) )
                Write(Iout1,*) ' NAM_LMAX () = ', NAM_LMAX(IRRRunoffSub)
                Write(Iout1,*) ' NAM_LTIF () = ', NAM_LTIF(IRRRunoffSub)
                Err969 = .true.
             endif
             if (NAM_LTG(iRRRunoffSub) .ge. NAM_LMAX(iRRRunoffSub)) then
                call ErrMsgStandard (969, 0, ' Threshold Lower Zone percolation LTP should be <= Maximum lower zone content Lmax for node with id: ', ID_Nod(inode)(1:len_trim(Id_Nod(inode))) )
                Write(Iout1,*) ' NAM_LMAX () = ', NAM_LMAX(IRRRunoffSub)
                Write(Iout1,*) ' NAM_LTP  () = ', NAM_LTG (IRRRunoffSub)
                Err969 = .true.
             endif
             If (Err969) Write(Iout1,*) ' This input is used for node ', ID_Nod(Inode)(1:len_trim(id_nod(inode)))
          endif
          If (Err969) Err969All = .true.
       enddo

       If (Err969All) call ErrMsgStandard (972, 0, ' Error in D-NAM input', ' See above messages and make sure consistent input is specified in NAMB/NAMS/NAML/NAMR records')

    endif

! LGSI Nodes
    Err969 = .false.
    if (NrLGSINodes .gt. 0) then
         Rewind(infile1)
         call SetMessage(LEVEL_INFO, 'Read LGSI data - type distribution info')

         RetVal = 0
         ReferenceToDefinition = 0
         ReferenceToDefinition2 = 0
         endfil = .false.
         teller = 0
         LGSTDef(:) = LGSI_NameSubArea(:,1)
         LGSTDef2(:) = LGSI_NameSubArea(:,2)
         CALL SKPCOM (Infile1, ENDFIL, 'ODS')
         Do while (.not. endfil)
           READ (Infile1,'(A1000)',END=5113,ERR=150,IOSTAT=IECODE) STRING
           IF (STRING(1:4) .EQ. 'LGST') Then
           ! Read LGSI type definition  id
           Retval = RetVal + GetVAR2 (STRING,' id ',1,' RRRunoff_readAscii',' LGSI Type definition data',IOUT1, &
                                      CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
           name = CDUM(1)
           teller = teller + 1
           ! Eerst testen of LGST definition wel gebruikt wordt, dan pas verwerken
           IRRRunoff = FindString (NcRRRunoffLGSI, LGSTDef, Name, NcRRRunoffLGSI, CaseSensitive)
           Occurs = (IRRRunoff .gt. 0)
           IRRRunoff2 = FindString (NcRRRunoffLGSI, LGSTDef2, Name, NcRRRunoffLGSI, CaseSensitive)
           Occurs2 = (IRRRunoff2 .gt. 0)
           if (IRRRunoff .gt. 0) then
              IRRRunoffSub = IRRRunoff
!              IRRRunoffSub = RRRunoff_SubIndex(IRRRunoff)
              if (ReferenceToDefinition(iRRRunoffSub) .gt. 0) then
                 call SetMessage(LEVEL_ERROR, 'LGST Definition '//name(1:Len_Trim(Name))//' double in datafile Runoff nodes')
                 LevelError = .true.
                 Occurs = .false.    ! voorkomt verdere verwerking
              else
                 ! cleaning RR files
                 If (CleanRRFiles) write(Iounit,'(A)') String (1:len_trim(String))
              endif
           endif
           if (IRRRunoff2 .gt. 0) then
              IRRRunoffSub = IRRRunoff2
!              IRRRunoffSub = RRRunoff_SubIndex(IRRRunoff2)
              if (ReferenceToDefinition2(iRRRunoffSub) .gt. 0) then
                 call SetMessage(LEVEL_ERROR, 'LGST Definition '//name(1:Len_Trim(Name))//' double in datafile Runoff nodes')
                 LevelError = .true.
                 Occurs = .false.    ! voorkomt verdere verwerking
              else
                 ! cleaning RR files
                 If (CleanRRFiles) write(Iounit,'(A)') String (1:len_trim(String))
              endif
           endif
       ! Verwerk LGSI Type definition
           if (occurs .or. occurs2) then
             ! Read type definition parameters
             Retval = RetVal + GetVAR2 (STRING,' ty ',3,' RRRunoff_readAscii',' LGSI Type data',IOUT1, &
                         CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
             LGSITypeDum = IDUM(1)
             if (LGSITypeDum .eq. 1) then
                Retval = RetVal + GetVAR2 (STRING,' maxsd ',2,' RRRunoff_readAscii','  LGSI Type data',IOUT1, &
                                           CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
                LGSIMaxSDDum = RDUM(1)
                Retval = RetVal + GetVAR2 (STRING,' minsd ',2,' RRRunoff_readAscii','  LGSI Type data',IOUT1, &
                                           CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
                LGSIMinSDDum = RDUM(1)
                If (LGSIMinSDDum .lt. 0.001D0 .or. LGSIMinSDDUm .gt. LGSIMaxSDDum) then
                   Err969 = .true.
                   call ErrMsgStandard (969, 0, ' LGSI Maxsd and Minsd not correctly specified; 0.001<MinSd<MaxSd is required; typical values <2',String(1:Len_Trim(String)) )
                endif
                Retval = RetVal + GetVAR2 (STRING,' nb ',2,' RRRunoff_readAscii','  LGSI Type data',IOUT1, &
                                           CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
                LGSINbDum = RDUM(1)
                If (LGSINbDum .lt. 0.001D0 .or. LGSINbDum .gt. 10.0) then
                   Err969 = .true.
                   call ErrMsgStandard (969, 0, ' LGSI Nb not correctly specified; 0.001<Nb<10 is required; typical value <3',String(1:Len_Trim(String)) )
                endif
                Retval = RetVal + GetVAR2 (STRING,' nusdmax ',2,' RRRunoff_readAscii','  LGSI Type data',IOUT1, &
                                           CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
                LGSINusdMaxDum = RDUM(1)
                If (LGSINusdMaxDum .lt. 0.001D0 .or. LGSINusDmaxDum .gt. 30.0) then
                   Err969 = .true.
                   call ErrMsgStandard (969, 0, ' LGSI Nusdmax not correctly specified; 0.001<Nusdmax<30 is required; typical value 0.1<Nusdmax<1.5',String(1:Len_Trim(String)) )
                endif
                Retval = RetVal + GetVAR2 (STRING,' rex ',2,' RRRunoff_readAscii','  LGSI Type data',IOUT1, &
                                           CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
                LGSIRexDum = RDUM(1) * 24.   ! convert from days to hours ; hard coded
                If (LGSIRexDum .lt. 0.001D0 .or. LGSIRexDum .gt. 100.0*24.) then
                   Err969 = .true.
                   call ErrMsgStandard (969, 0, ' LGSI Rex not correctly specified; 0.001<Rex<100 is required; typical value 0.01<Rex<100',String(1:Len_Trim(String)) )
                endif
                Retval = RetVal + GetVAR2 (STRING,' rov ',2,' RRRunoff_readAscii','  LGSI Type data',IOUT1, &
                                           CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
                LGSIRovDum = RDUM(1) * 24.   ! convert from days to hours ; hard coded
                If (LGSIRovDum .lt. 0.001D0 .or. LGSIRovDum .gt. 1000.0*24.) then
                   Err969 = .true.
                   call ErrMsgStandard (969, 0, ' LGSI Rov not correctly specified; 0.001<Rov<1000 is required; typical value 0.01<Rov<100',String(1:Len_Trim(String)) )
                endif
                Retval = RetVal + GetVAR2 (STRING,' fp ',2,' RRRunoff_readAscii','  LGSI Type data',IOUT1, &
                                           CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
                LGSIFpDum = RDUM(1)
                If (LGSIFpDum .lt. 0.0D0 .or. LGSIFpDum .gt. 1.0) then
                   Err969 = .true.
                   call ErrMsgStandard (969, 0, ' LGSI Fp not correctly specified; 0.0<Fp<1.0 is required; typical value 0.001<Fp<1',String(1:Len_Trim(String)) )
                endif
                Retval = RetVal + GetVAR2 (STRING,' fow ',2,' RRRunoff_readAscii','  LGSI Type data',IOUT1, &
                                           CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
                LGSIFowDum = RDUM(1)
                If (LGSIFowDum .lt. 0.0D0 .or. LGSIFowDum .gt. 1.0) then
                   Err969 = .true.
                   call ErrMsgStandard (969, 0, ' LGSI Fow not correctly specified; 0.0<Fow<1.0 is required; typical value 0.001<Fow<0.5',String(1:Len_Trim(String)) )
                endif
                If (LGSIFowDum+LGSIFpDum .gt. 1.0D0) then
                   Err969 = .true.
                   call ErrMsgStandard (969, 0, ' LGSI fow+fp should be <= 1',String(1:Len_Trim(String)) )
                endif
             elseif (LGSITypeDum .eq. 2) then
                 Retval = RetVal + GetVAR2 (STRING,' ag ',2,' RRRunoff_readAscii','  LGSI Type data',IOUT1, &
                                            CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
                 LGSIAgDum = RDUM(1)
                 Retval = RetVal + GetVAR2 (STRING,' bg ',2,' RRRunoff_readAscii','  LGSI Type data',IOUT1, &
                                            CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
                 LGSIBgDum = RDUM(1)
             endif
       ! Assign definition to individual nodes
             Do iRRRunoff = 1, ncRRRunoff
               if (RRRunoff_CompOption(iRRRunoff) .eq. 4) then
                   IRRRunoffSub = RRRUnoff_SubIndex(iRRRunoff)
                   if (StringComp(LGSTDef(IRRRunoffSub), Name, CaseSensitive) )  then
                     ReferenceToDefinition(iRRRunoffSub) = teller
                     LGSI_Type (IRRRunoffSub,1)   = LGSITypeDum
                     if (LGSITypeDUM .eq. 1) then
                       LGSI_NormalMaxSD(IRRRunoffSub,1)   = LGSIMaxSDDum
                       LGSI_NormalMinSD(IRRRunoffSub,1)   = LGSIMinSDDum
                       LGSI_NormalNb   (IRRRunoffSub,1)   = LGSINbDum
                       LGSI_NormalNusDmax(IRRRunoffSub,1) = LGSINusDMaxDum
                       LGSI_NormalRex(IRRRunoffSub,1) = LGSIRexDum
                       LGSI_NormalRov(IRRRunoffSub,1) = LGSIRovDum
                       LGSI_NormalFp(IRRRunoffSub,1)  = LGSIFpDum
                       LGSI_As     (IRRRunoffSub,1)  = LGSIFpDum * LGSI_Area(IRRRunoffSub,1)
                       LGSI_NormalFow(IRRRunoffSub,1) = LGSIFowDum
!                      LGSI_Ar     (IRRRunoffSub,1)  = LGSIFowDum * LGSI_AreaTot(IRRRunoffSub)
                       LGSI_Ar     (IRRRunoffSub,1)  = LGSIFowDum * LGSI_Area(IRRRunoffSub,1)
                     elseif (LGSITypeDum .eq. 2) then
                       LGSI_GammaAg(IRRRunoffSub,1)  = LGSIAgDum
                       LGSI_GammaBg(IRRRunoffSub,1)  = LGSIBgDum
                     endif
                   endif
                   if (StringComp(LGSTDef2(IRRRunoffSub), Name, CaseSensitive) )  then
                     ReferenceToDefinition2(iRRRunoffSub) = teller
                     LGSI_Type (IRRRunoffSub,2)   = LGSITypeDum
                     if (LGSITypeDum .eq. 1) then
                       LGSI_NormalMaxSD(IRRRunoffSub,2)   = LGSIMaxSDDum
                       LGSI_NormalMinSD(IRRRunoffSub,2)   = LGSIMinSDDum
                       LGSI_NormalNb   (IRRRunoffSub,2)   = LGSINbDum
                       LGSI_NormalNusDmax(IRRRunoffSub,2) = LGSINusDMaxDum
                       LGSI_NormalRex(IRRRunoffSub,2) = LGSIRexDum
                       LGSI_NormalRov(IRRRunoffSub,2) = LGSIRovDum
                       LGSI_NormalFp(IRRRunoffSub,2)  = LGSIFpDum
                       LGSI_As     (IRRRunoffSub,2)  = LGSIFpDum * LGSI_Area(IRRRunoffSub,2)
                       LGSI_NormalFow(IRRRunoffSub,2) = LGSIFowDum
!                      LGSI_Ar     (IRRRunoffSub,2)  = LGSIFowDum * LGSI_Area(IRRRunoffSub,2)
                       LGSI_Ar     (IRRRunoffSub,2)  = LGSIFowDum * LGSI_AreaTot(IRRRunoffSub)
                     elseif (LGSITypeDum .eq. 2) then
                       LGSI_GammaAg(IRRRunoffSub,2)  = LGSIAgDum
                       LGSI_GammaBg(IRRRunoffSub,2)  = LGSIBgDum
                     endif
                   endif
               endif
             Enddo
           Endif
          Endif
          CALL SKPCOM (Infile1, ENDFIL, 'ODS')
         Enddo
    5113 Continue

          If (Err969) call ErrMsgStandard (972, 0, ' Some LGSI invalid input data values', ' Check log file and adjust input')
          If (RetVal .gt. 0) call ErrMsgStandard (972, 0, ' Error reading data from RR-runoff file ', ' Error getting LGST records')
          Do iRRRunoff = 1, ncRRRunoff
             IRRRunoffSub = RRRunoff_SubIndex(IRRRunoff)
             if (RRRunoff_CompOption(IRRRunoff) .eq. 4) then
                if (ReferenceToDefinition(iRRRunoffSub) .eq. 0 .and. LGSTDEF (iRRRunoffSub) .ne. '')  then
                   Err969 = .true.
                   call ErrMsgStandard (969, 0, ' LGST definition not found in RRRunoff file.', LGSTDef(iRRRunoffSub))
                endif
                if (ReferenceToDefinition2(iRRRunoffSub) .eq. 0 .and. LGSTDEF2 (iRRRunoffSub) .ne. '')  then
                   Err969 = .true.
                   call ErrMsgStandard (969, 0, ' LGST definition not found in RRRunoff file.', LGSTDef2(iRRRunoffSub))
                endif
             endif
          Enddo
          If (Err969) call ErrMsgStandard (972, 0, ' Not enough LGSI-Type data found', ' Some LGST Definitions not present in RRRunoff file')


        Rewind(infile1)
        call SetMessage(LEVEL_INFO, 'Read LGSI data - Initial Condition')

         ReferenceToDefinition = 0
         ReferenceToDefinition2 = 0
         LGSTDef(:) = LGSI_InitialCondition(:,1)
         LGSTDef2(:) = LGSI_InitialCondition(:,2)
         endfil = .false.
         teller = 0
         RetVal = 0
         CALL SKPCOM (Infile1, ENDFIL, 'ODS')
         Do while (.not. endfil)
           READ (Infile1,'(A1000)',END=5114,ERR=150,IOSTAT=IECODE) STRING
           IF (STRING(1:4) .EQ. 'LGSC') Then
           ! Read LGSI Initial condition definition id
           Retval = RetVal + GetVAR2 (STRING,' id ',1,' RRRunoff_readAscii',' LGSI Initial condition definition data',IOUT1, &
                                      CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
           name = CDUM(1)
           teller = teller + 1
           ! Eerst testen of soil definition wel gebruikt wordt, dan pas verwerken
           IRRRunoff = FindString (NcRRRunoffLGSI, LGSTDef, Name, NcRRRunoffLGSI, CaseSensitive)
           Occurs = (IRRRunoff .gt. 0)
           IRRRunoff2 = FindString (NcRRRunoffLGSI, LGSTDef2, Name, NcRRRunoffLGSI, CaseSensitive)
           Occurs2 = (IRRRunoff2 .gt. 0)
           if (IRRRunoff .gt. 0) then
              IRRRunoffSub = IRRRunoff
!              IRRRunoffSub = RRRunoff_SubIndex(IRRRunoff)
              if (ReferenceToDefinition(iRRRunoffSub) .gt. 0) then
                 call SetMessage(LEVEL_ERROR, 'LGSI Initial Condition Definition '//name(1:Len_Trim(Name))//' double in datafile Runoff nodes')
                 LevelError = .true.
                 Occurs = .false.    ! voorkomt verdere verwerking
              else
                 ! cleaning RR files
                 If (CleanRRFiles) write(Iounit,'(A)') String (1:len_trim(String))
              endif
           endif
           if (IRRRunoff2 .gt. 0) then
              IRRRunoffSub = IRRRunoff2
!             IRRRunoffSub = RRRunoff_SubIndex(IRRRunoff2)
              if (ReferenceToDefinition2(iRRRunoff2) .gt. 0) then
                 call SetMessage(LEVEL_ERROR, 'LGSI Initial Condition Definition '//name(1:Len_Trim(Name))//' double in datafile Runoff nodes')
                 LevelError = .true.
                 Occurs = .false.    ! voorkomt verdere verwerking
              else
                 ! cleaning RR files
                 If (CleanRRFiles) write(Iounit,'(A)') String (1:len_trim(String))
              endif
           endif
       ! Verwerk LGSC definition
           if (occurs .or. occurs2) then
       ! Read data
             Retval = RetVal + GetVAR2 (STRING,' ig ',2,' RRRunoff_readAscii',' LGSI Initial condition data',IOUT1, &
                         CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
             BetaDum = RDUM(1)
       ! Assign definition to individual nodes
             Do iRRRunoff = 1, ncRRRunoff
                if (RRRunoff_CompOption(iRRRunoff) .eq. 4) then
                   IRRRunoffSub = RRRUnoff_SubIndex(iRRRunoff)
                   if (StringComp(LGSTDef(IRRRunoffSub), Name, CaseSensitive) )  then
                      ReferenceToDefinition(iRRRunoffSub) = teller
                      LGSI_SpecifiedInitGwl(IRRRunoffSub,1) = BetaDum
                      LGSI_InitGwl(IRRRunoffSub,1) = BetaDum
                   endif
                   if (StringComp(LGSTDef2(IRRRunoffSub), Name, CaseSensitive) )  then
                      ReferenceToDefinition2(iRRRunoffSub) = teller
                      LGSI_SpecifiedInitGwl(IRRRunoffSub,2) = BetaDum
                      LGSI_InitGwl(IRRRunoffSub,2) = BetaDum
                   endif
                endif
             Enddo
           Endif
          Endif
          CALL SKPCOM (Infile1, ENDFIL, 'ODS')
         Enddo
5114 Continue

          If (RetVal .gt. 0) call ErrMsgStandard (972, 0, ' Error reading data from RR-runoff file ', ' Error getting LGSC records')
          Err969 = .false.
          Do iRRRunoff = 1, ncRRRunoff
             IRRRunoffSub = RRRunoff_SubIndex(IRRRunoff)
             if (RRRunoff_CompOption(IRRRunoff) .eq. 4) then
                if (ReferenceToDefinition(iRRRunoffSub) .eq. 0 .and. LGSTDEF (iRRRunoffSub) .ne. '')  then
                   Err969 = .true.
                   call ErrMsgStandard (969, 0, ' LGSC definition not found in RRRunoff file.', LGSTDef(iRRRunoffSub))
                endif
                if (ReferenceToDefinition2(iRRRunoffSub) .eq. 0 .and. LGSTDEF2 (iRRRunoffSub) .ne. '')  then
                   Err969 = .true.
                   call ErrMsgStandard (969, 0, ' LGSC definition not found in RRRunoff file.', LGSTDef2(iRRRunoffSub))
                endif
             endif
          Enddo
          If (Err969) call ErrMsgStandard (972, 0, ' Not enough LGSI-Condition data found', ' Some LGSC Definitions not present in RRRunoff file')


        Err969 = .false.

        Rewind(infile1)
        call SetMessage(LEVEL_INFO, 'Read LGSI data - Delay Definition')

         ReferenceToDefinition = 0
         ReferenceToDefinition2 = 0
         LGSTDef(:) = LGSI_DelayDefinition(:)
         endfil = .false.
         teller = 0
         RetVal = 0
         CALL SKPCOM (Infile1, ENDFIL, 'ODS')
         Do while (.not. endfil)
           READ (Infile1,'(A1000)',END=5115,ERR=150,IOSTAT=IECODE) STRING
           IF (STRING(1:4) .EQ. 'LGSD') Then
           ! Read LGSI Delay definition id
           Backspace(Infile1)
           Success = GetRecord(Infile1, 'LGSD', Endfil, idebug, Iout1)  ! get record van keyword LGSD tot lgsd, zet in buffer
           if (len_trim(Buffer) .gt. 9999) then
                Err969 = .true.
                call ErrMsgStandard (969, 0, ' LGSD delay definition - too long for local String array. Use larger input timestepsize.', Buffer(1:999))
           endif
           String(1:) = Buffer(1:)
           Retval = RetVal + GetVAR2 (STRING,' id ',1,' RRRunoff_readAscii',' LGSI Delay definition data',IOUT1, &
                                      CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
           name = CDUM(1)
           teller = teller + 1
           ! Eerst testen of delay definition wel gebruikt wordt, dan pas verwerken
           IRRRunoff = FindString (NcRRRunoffLGSI, LGSTDef, Name, NcRRRunoffLGSI, CaseSensitive)
           Occurs = (IRRRunoff .gt. 0)
           if (IRRRunoff .gt. 0) then
!              IRRRunoffSub = RRRunoff_SubIndex(IRRRunoff)
              IRRRunoffSub = IRRRunoff
              if (ReferenceToDefinition(iRRRunoffSub) .gt. 0) then
                 call SetMessage(LEVEL_ERROR, 'LGSI Delay Definition '//name(1:Len_Trim(Name))//' double in datafile Runoff nodes')
                 LevelError = .true.
                 Occurs = .false.    ! voorkomt verdere verwerking
              elseIf (CleanRRFiles) then
                 ! cleaning RR files
                 ! bit more intelligent for long VEQD records
                 String0 = String
 1051            continue
                 lenstring = len_trim(String)
                 if (LenString .le. 999) then
                     write(Iounit,'(A)') String (1:len_trim(String))
                 else
                     ipos  = FndFrst (' ',String(979:999),.false.)
                     if (ipos .gt. 0) then
                        ipos = 979+ipos-1
                        write(Iounit,'(A)') String (1:ipos)
                        String = String(ipos+1:)
                        goto 1051
                     endif
                 endif
                 String = String0
              endif
           endif
       ! Verwerk LGSD definition
           if (occurs) then
       ! Read data
             Retval = RetVal + GetVAR2 (STRING,' dt ',3,' RRRunoff_readAscii',' LGSI Delay definition data',IOUT1, &
                         CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
             NrSeconds = IDUM(1)
             Retval = RetVal + GetVAR2 (STRING,' nt ',3,' RRRunoff_readAscii',' LGSI Delay definition data',IOUT1, &
                         CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
             NrValues=Idum(1)
             If (NrValues .gt. LGSI_MaxDelayLength .or. NHLP .lt. LGSI_MaxDelayLengthPlus1) then
                Err969 = .true.
                call ErrMsgStandard (969, 0, ' LGSD delay definition - too many delay coefficients for max. array size. Use larger input timestepsize.', LGSTDef(iRRRunoffSub))
             Endif
             RetVal = RetVal + GetVRS2(STRING,' dc ',2,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file', &
                                       IOUT1, CDUM(1), RDUM(1), IDUM(1), NrValues, IflRtn)
       ! Assign definition to individual nodes
             Do iRRRunoff = 1, ncRRRunoff
               if (RRRunoff_CompOption(IRRRUnoff) .eq. 4) then
                 iRRRunoffSub = RRRunoff_SubIndex(iRRRunoff)
                 if (StringComp(LGSTDef(IRRRunoffSub), Name, CaseSensitive) )  then
                   ReferenceToDefinition(iRRRunoffSub) = teller
                   LGSI_DelayTimestepSize(IRRRunoffSub) = NrSeconds
                   LGSI_DelayNrTimesteps(IRRRunoffSub) = NrValues
                   Checksum = 0.0
                   Do i=1,NrValues
                      LGSI_DefinedDelayCoefficients(IRRRunoffSub,i) = Dble ( RDum(i) )
                      Checksum = Checksum + Rdum(i)
                   Enddo
                   If (Abs (Checksum -1.0D0) .ge. .01) then
                      Err969 = .true.
                      Write(Iout1,*) ' Checksum value = ', Checksum, ' for definition ', LGSTDef(iRRRunoffSub)
                      call ErrMsgStandard (969, 0, ' LGSD definition: sum of values (dc) for specified number of values (nt) does not add up to 1 for definition ', LGSTDef(iRRRunoffSub))
                   Endif
                 endif
               endif
             Enddo
           Endif
          Endif
          CALL SKPCOM (Infile1, ENDFIL, 'ODS')
         Enddo
5115 Continue

          If (RetVal .gt. 0) call ErrMsgStandard (972, 0, ' Error reading data from RR-runoff file ', ' Error getting LGSD records')
          Do iRRRunoff = 1, ncRRRunoff
             IRRRunoffSub = RRRunoff_SubIndex(IRRRunoff)
             if (RRRunoff_CompOption(IRRRunoff) .eq. 4) then
                 if (ReferenceToDefinition(iRRRunoffSub) .eq. 0 .and. LGSTDEF (iRRRunoffSub) .ne. '')  then
                     Err969 = .true.
                     call ErrMsgStandard (969, 0, ' LGSD definition not found in RRRunoff file.', LGSTDef(iRRRunoffSub))
                 endif
             endif
          Enddo
          If (Err969) call ErrMsgStandard (972, 0, ' Not enough LGSI data found', ' Some LGSD Definitions not present or not correctly specified in RRRunoff file')
    endif

! Wageningen Nodes: no additional type of records
! Walrus nodes: additional records for user defined tables WETN, BETA, VEQD, WQH, HSTT  (HSmin Time Table)
! WETN records
    Err969 = .false.
    if (NrWalrusNodes .gt. 0 .and. WalrusUserDefinedWetnessFunctionExists) then
       Rewind(infile1)
       call SetMessage(LEVEL_INFO, 'Read Walrus data - user defined Wetness Index function')

       RetVal = 0
       ReferenceToDefinition = 0
       endfil = .false.
       teller = 0
       CALL SKPCOM (Infile1, ENDFIL, 'ODS')
       Do while (.not. endfil)
        READ (Infile1,'(A1000)',END=6111,ERR=150,IOSTAT=IECODE) STRING
!       Write(*,*) 'String read', String(1:100)
        IF (STRING(1:4) .EQ. 'WETN') Then
         ! check for WETN record on one line
 102     continue
         lenString = len_trim (String)
         if (String(lenString-3:lenstring) .ne. 'wetn') then
             READ(Infile1,'(A1000)',END=21,ERR=150,IOSTAT=IECODE)  String1
             lenString1 = len_trim (String1)
             String = String(1:lenString) // " " // String1 (1:lenString1)
             goto 102
         endif
         ! Read Wetness definition  id
         Retval = RetVal + GetVAR2 (STRING,' id ',1,' RRRunoff_readAscii',' Walrus Wetness definition data',IOUT1, &
                                    CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
         name = CDUM(1)
         teller = teller + 1
         ! Eerst testen of WETN definition wel gebruikt wordt, dan pas verwerken
!        write(*,*) ' used Walrus Wetness functions'
!        Do i=1,NCRRRunoffWalrus
!           write(*,*) WALRUS_WIT(i)
!           write(*,*) 'Length', Len_trim (WALRUS_WIT(i))
!        Enddo
         IRRRunoff = FindString (NcRRRunoffWalrus, WALRUS_WIT, Name, NcRRRunoffWalrus, CaseSensitive)
         Occurs = (IRRRunoff .gt. 0)
         if (IRRRunoff .gt. 0) then
            IRRRunoffSub = IRRRunoff
            if (ReferenceToDefinition(iRRRunoffSub) .gt. 0) then
               call SetMessage(LEVEL_ERROR, 'WETN Definition '//name(1:Len_Trim(Name))//' double in datafile Runoff nodes')
               LevelError = .true.
               Occurs = .false.    ! voorkomt verdere verwerking
            else
               ! cleaning RR files
               If (CleanRRFiles) write(Iounit,'(A)') String (1:len_trim(String))
            endif
         endif
        ! Verwerk WETN Type definition
         if (occurs) then
           ! Read type definition parameters
           Retval = RetVal + GetVAR2 (STRING,' nv ',3,' RRRunoff_readAscii',' WETN data',IOUT1, &
                       CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
           nrValues = IDUM(1)
           if (NrValues .gt. NHLP) then
              write(iout1,*) ' User defined WETNess interpolation table too long; please limit to at most ', NHLP,' values'
              write(iout1,*) ' only first ', NHLP,' values are read and used'
           endif
           RetVal = RetVal + GetVRS2(STRING,' dv ',2,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file', &
                                     IOUT1, CDUM(1), RDUM(1), IDUM(1), NrValues, IflRtn)
           Do i=1, nrValues
               Walrus_WIDV_DV(IrrRunoffSub,i) = RDUM(i)
           enddo
           RetVal = RetVal + GetVRS2(STRING,' wi ',2,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file', &
                                     IOUT1, CDUM(1), RDUM(1), IDUM(1), NrValues, IflRtn)
           Do i=1, nrValues
               Walrus_WIDV_WI(IrrRunoffSub,i) = RDUM(i)
           enddo
!         check WI values
           IRRRunoffRef = IRRRunoffSub
           Do i=1, nrValues
              If (WALRUS_WIDV_DV(IRRRunoffRef,i) .lt. 0 ) then
                 Err969 = .true.
                 call ErrMsgStandard (969, 0, ' WETN definition: dv values should be non-negative for definition ', WALRUS_WIT(iRRRunoffSub))
              Endif
              If (WALRUS_WIDV_WI(IRRRunoffRef,i) .gt. 1 .or. WALRUS_WIDV_WI(IRRRunoffRef,i) .lt. 0) then
                 Err969 = .true.
                 call ErrMsgStandard (969, 0, ' WETN definition: wi values should be betweeen 0 and 1 for definition ', WALRUS_WIT(iRRRunoffSub))
              Endif
              If (i .gt. 1) then
                if (WALRUS_WIDV_DV(IRRRunoffRef,i) .le. WALRUS_WIDV_DV(IRRRunoffRef,i-1) ) then
                  Err969 = .true.
                  write(iout1,*) ' index i and i-1', i, i-1
                  write(iout1,*) ' Wetness dv values i and i-1', Walrus_WIDV_DV(IRRRunoffRef,i), Walrus_WIDV_DV(IRRRunoffRef,i-1)
                  call ErrMsgStandard (969, 0, ' WETN definition: dv values should be specified in increasing order for definition', WALRUS_WIT(iRRRunoffSub))
                Endif
              Endif
              If (i .gt. 1) then
                If(WALRUS_WIDV_WI(IRRRunoffRef,i) .gt. WALRUS_WIDV_WI(IRRRunoffRef,i-1) ) then
                  Err969 = .true.
                  write(iout1,*) ' index i and i-1', i, i-1
                  write(iout1,*) ' Wetness values i and i-1', Walrus_WIDV_WI(IRRRunoffRef,i), Walrus_WIDV_WI(IRRRunoffRef,i-1)
                  call ErrMsgStandard (969, 0, ' WETN definition: wi values should be specified in non-increasing order for definition', WALRUS_WIT(iRRRunoffSub))
                endif
              Endif
           Enddo
       ! Assign definition to individual nodes
           Do iRRRunoff = 1, ncRRRunoff
              if (RRRunoff_CompOption(IRRRUnoff) .eq. 6) then      ! WALRUS
                iRRRunoffSub = RRRunoff_SubIndex(iRRRunoff)
                if (StringComp(WALRUS_WIT(IRRRunoffSub), Name, CaseSensitive) )  then
                    ReferenceToDefinition(iRRRunoffSub) = teller
                    WALRUS_WILength (IRRRunoffSub) = NrValues
                    Do i=1,NrValues
                       WALRUS_WIDV_DV(IRRRunoffSub,i) = WALRUS_WIDV_DV(IRRRunoffRef,i)
                       WALRUS_WIDV_WI(IRRRunoffSub,i) = WALRUS_WIDV_WI(IRRRunoffRef,i)
                    Enddo
!                   write(*,*) ' Walrus index ', IRRRunoffSub
!                   write(*,*) ' Walrus WETN user defined table '
!                   write(*,*) ' length =', WALRUS_WILength(IRRRunoffSub)
!                   write(*,*) ' DV values =', (WALRUS_WIDV_DV(IRRRunoffSub,i), i=1,NrValues)
!                   write(*,*) ' WI values =', (WALRUS_WIDV_WI(IRRRunoffSub,i), i=1,NrValues)
                endif
              endif
           Enddo
         endif  ! end of Wetness Index function occurs
        endif  ! end of WETN
       Enddo
6111   Continue

       If (RetVal .gt. 0) call ErrMsgStandard (972, 0, ' Error reading data from RR-runoff file ', ' Error getting WETN records')
       Do iRRRunoff = 1, ncRRRunoff
          IRRRunoffSub = RRRunoff_SubIndex(IRRRunoff)
          if (RRRunoff_CompOption(IRRRunoff) .eq. 6) then
              if (ReferenceToDefinition(iRRRunoffSub) .eq. 0 .and. WALRUS_WIT(iRRRunoffSub) .ne. '')  then
                  Err969 = .true.
                  call ErrMsgStandard (969, 0, ' WETN definition not found in RRRunoff file.', WALRUS_WIT(iRRRunoffSub))
             endif
          endif
       Enddo
       If (Err969) call ErrMsgStandard (972, 0, ' Not enough WALRUS data found', ' Some WETNess user defined functions not present or not correctly specified in input file')
    endif

! BETA records
    Err969 = .false.
    if (NrWalrusNodes .gt. 0 .and. WalrusUserDefinedBETAFunctionExists) then
       Rewind(infile1)
       call SetMessage(LEVEL_INFO, 'Read Walrus data - user defined BETA evaporation reduction function')

       RetVal = 0
       ReferenceToDefinition = 0
       endfil = .false.
       teller = 0
       CALL SKPCOM (Infile1, ENDFIL, 'ODS')
       Do while (.not. endfil)
        READ (Infile1,'(A1000)',END=6112,ERR=150,IOSTAT=IECODE) STRING
!       Write(*,*) 'String read', String(1:100)
        IF (STRING(1:4) .EQ. 'BETA') Then
         ! check for BETA record on one line
 103     continue
         lenString = len_trim (String)
         if (String(lenString-3:lenstring) .ne. 'beta') then
             READ(Infile1,'(A1000)',END=21,ERR=150,IOSTAT=IECODE)  String1
             lenString1 = len_trim (String1)
             String = String(1:lenString) // " " // String1 (1:lenString1)
             goto 103
         endif
         ! Read BETA definition  id
         Retval = RetVal + GetVAR2 (STRING,' id ',1,' RRRunoff_readAscii',' BETA evaporation reduction data',IOUT1, &
                                    CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
         name = CDUM(1)
         teller = teller + 1
         ! Eerst testen of BETA definition wel gebruikt wordt, dan pas verwerken
!         write(*,*) ' used Walrus BETA functions'
!         Do i=1,NCRRRunoffWalrus
!            write(*,*) WALRUS_BIT(i)
!            write(*,*) 'Length', Len_trim (WALRUS_BIT(i))
!         Enddo
         IRRRunoff = FindString (NcRRRunoffWalrus, WALRUS_BIT, Name, NcRRRunoffWalrus, CaseSensitive)
         Occurs = (IRRRunoff .gt. 0)
         if (IRRRunoff .gt. 0) then
            IRRRunoffSub = IRRRunoff
            if (ReferenceToDefinition(iRRRunoffSub) .gt. 0) then
               call SetMessage(LEVEL_ERROR, 'BETA Definition '//name(1:Len_Trim(Name))//' double in datafile Runoff nodes')
               LevelError = .true.
               Occurs = .false.    ! voorkomt verdere verwerking
            else
               ! cleaning RR files
               If (CleanRRFiles) write(Iounit,'(A)') String (1:len_trim(String))
            endif
         endif
        ! Verwerk BETA Type definition
         if (occurs) then
           ! Read type definition parameters
           Retval = RetVal + GetVAR2 (STRING,' nv ',3,' RRRunoff_readAscii',' BETA evaporation reduction data',IOUT1, &
                       CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
           nrValues = IDUM(1)
           if (NrValues .gt. NHLP) then
              write(iout1,*) ' User defined BETA interpolation table too long; please limit to at most ', NHLP,' values'
              write(iout1,*) ' only first ', NHLP,' values are read and used'
           endif
           RetVal = RetVal + GetVRS2(STRING,' dv ',2,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file', &
                                     IOUT1, CDUM(1), RDUM(1), IDUM(1), NrValues, IflRtn)
           Do i=1, nrValues
               Walrus_ERDV_DV(IrrRunoffSub,i) = RDUM(i)
           enddo
           RetVal = RetVal + GetVRS2(STRING,' er ',2,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file', &
                                     IOUT1, CDUM(1), RDUM(1), IDUM(1), NrValues, IflRtn)
           Do i=1, nrValues
               Walrus_ERDV_ER(IrrRunoffSub,i) = RDUM(i)
           enddo
!         check ER values
           IRRRunoffRef = IRRRunoffSub
           Do i=1, nrValues
              If (WALRUS_ERDV_DV(IRRRunoffRef,i) .lt. 0 ) then
                 Err969 = .true.
                 call ErrMsgStandard (969, 0, ' BETA definition: dv values should be non-negative for definition ', WALRUS_BIT(iRRRunoffSub))
              Endif
              If (WALRUS_ERDV_ER(IRRRunoffRef,i) .gt. 1 .or. WALRUS_ERDV_ER(IRRRunoffRef,i) .lt. 0) then
                 Err969 = .true.
                 call ErrMsgStandard (969, 0, ' BETA definition: er values should be betweeen 0 and 1 for definition ', WALRUS_BIT(iRRRunoffSub))
              Endif
              If (i .gt. 1) then
                if (WALRUS_ERDV_DV(IRRRunoffRef,i) .le. WALRUS_ERDV_DV(IRRRunoffRef,i-1) ) then
                  Err969 = .true.
                  write(iout1,*) ' index i and i-1', i, i-1
                  write(iout1,*) ' beta dv values i and i-1', Walrus_ERDV_DV(IRRRunoffRef,i), Walrus_ERDV_DV(IRRRunoffRef,i-1)
                  call ErrMsgStandard (969, 0, ' BETA definition: dv values should be specified in increasing order for definition', WALRUS_BIT(iRRRunoffSub))
                Endif
              Endif
              If (i .gt. 1) then
                If(WALRUS_ERDV_ER(IRRRunoffRef,i) .gt. WALRUS_ERDV_ER(IRRRunoffRef,i-1) ) then
                  Err969 = .true.
                  write(iout1,*) ' index i and i-1', i, i-1
                  write(iout1,*) ' beta values i and i-1', Walrus_ERDV_ER(IRRRunoffRef,i), Walrus_ERDV_ER(IRRRunoffRef,i-1)
                  call ErrMsgStandard (969, 0, ' BETA definition: er values should be specified in non-increasing order for definition', WALRUS_BIT(iRRRunoffSub))
                endif
              Endif
           Enddo
       ! Assign definition to individual nodes
           Do iRRRunoff = 1, ncRRRunoff
              if (RRRunoff_CompOption(IRRRUnoff) .eq. 6) then      ! WALRUS
                iRRRunoffSub = RRRunoff_SubIndex(iRRRunoff)
                if (StringComp(WALRUS_BIT(IRRRunoffSub), Name, CaseSensitive) )  then
                    ReferenceToDefinition(iRRRunoffSub) = teller
                    WALRUS_ERLength (IRRRunoffSub) = NrValues
                    Do i=1,NrValues
                       WALRUS_ERDV_DV(IRRRunoffSub,i) = WALRUS_ERDV_DV(IRRRunoffRef,i)
                       WALRUS_ERDV_ER(IRRRunoffSub,i) = WALRUS_ERDV_ER(IRRRunoffRef,i)
                    Enddo
!                   write(*,*) ' Walrus index ', IRRRunoffSub
!                   write(*,*) ' Walrus BETA user defined table '
!                   write(*,*) ' length =', WALRUS_ERLength(IRRRunoffSub)
!                   write(*,*) ' DV values =', (WALRUS_ERDV_DV(IRRRunoffSub,i), i=1,NrValues)
!                   write(*,*) ' ER values =', (WALRUS_ERDV_ER(IRRRunoffSub,i), i=1,NrValues)
                endif
              endif
           Enddo
         endif  ! end of BETA evap reduction function occurs
        endif  ! end of BETA
       Enddo
6112   Continue

       If (RetVal .gt. 0) call ErrMsgStandard (972, 0, ' Error reading data from RR-runoff file ', ' Error getting BETA records')
       Do iRRRunoff = 1, ncRRRunoff
          IRRRunoffSub = RRRunoff_SubIndex(IRRRunoff)
          if (RRRunoff_CompOption(IRRRunoff) .eq. 6) then
              if (ReferenceToDefinition(iRRRunoffSub) .eq. 0 .and. WALRUS_BIT(iRRRunoffSub) .ne. '')  then
                  Err969 = .true.
                  call ErrMsgStandard (969, 0, ' BETA definition not found in RRRunoff file.', WALRUS_BIT(iRRRunoffSub))
             endif
          endif
       Enddo
       If (Err969) call ErrMsgStandard (972, 0, ' Not enough WALRUS data found', ' Some BETA user defined functions not present or not correctly specified in input file')
    endif

! VEQD records
    Err969 = .false.
    if (NrWalrusNodes .gt. 0 .and. WalrusUserDefinedVEQFunctionExists) then
       Rewind(infile1)
       call SetMessage(LEVEL_INFO, 'Read Walrus data - user defined VEQD groundwater - storage deficit function')

       RetVal = 0
       ReferenceToDefinition = 0
       endfil = .false.
       teller = 0
       CALL SKPCOM (Infile1, ENDFIL, 'ODS')
       Do while (.not. endfil)
        READ (Infile1,'(A1000)',END=6113,ERR=150,IOSTAT=IECODE) STRING
!        Write(*,*) 'String read', String(1:100)
        IF (STRING(1:4) .EQ. 'VEQD') Then
         ! check for VEQD record on one line
 104     continue
         lenString = len_trim (String)
         String0 = String
         if (String(lenString-3:lenstring) .ne. 'veqd') then
             READ(Infile1,'(A1000)',END=21,ERR=150,IOSTAT=IECODE)  String1
             lenString1 = len_trim (String1)
             String = String(1:lenString) // " " // String1 (1:lenString1)
             goto 104
         endif
         ! Read VEQD definition  id
         Retval = RetVal + GetVAR2 (STRING,' id ',1,' RRRunoff_readAscii',' VEQD interpolation table data',IOUT1, &
                                    CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
         name = CDUM(1)
         teller = teller + 1
         ! Eerst testen of VEQD definition wel gebruikt wordt, dan pas verwerken
!         write(*,*) ' used Walrus VEQD functions'
!         Do i=1,NCRRRunoffWalrus
!            write(*,*) WALRUS_VIT(i)
!            write(*,*) 'Length', Len_trim (WALRUS_VIT(i))
!         Enddo
         IRRRunoff = FindString (NcRRRunoffWalrus, WALRUS_VIT, Name, NcRRRunoffWalrus, CaseSensitive)
         Occurs = (IRRRunoff .gt. 0)
         if (IRRRunoff .gt. 0) then
            IRRRunoffSub = IRRRunoff
            if (ReferenceToDefinition(iRRRunoffSub) .gt. 0) then
               call SetMessage(LEVEL_ERROR, 'VEQD Definition '//name(1:Len_Trim(Name))//' double in datafile Runoff nodes')
               LevelError = .true.
               Occurs = .false.    ! voorkomt verdere verwerking
            elseIf (CleanRRFiles) then
               ! cleaning RR files
               ! bit more intelligent for long VEQD records
               String0 = String
 1061          continue
               lenstring = len_trim(String)
               if (LenString .le. 999) then
                   write(Iounit,'(A)') String (1:len_trim(String))
               else
                   ipos  = FndFrst (' ',String(979:999),.false.)
                   if (ipos .gt. 0) then
                      ipos = 979+ipos-1
                      write(Iounit,'(A)') String (1:ipos)
                      String = String(ipos+1:)
                      goto 1061
                   endif
               endif
               String = String0
            endif
         endif
        ! Verwerk VEQD Type definition
         if (occurs) then
           ! Read type definition parameters
           Retval = RetVal + GetVAR2 (STRING,' nv ',3,' RRRunoff_readAscii',' VEQD interpolation table data',IOUT1, &
                       CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
           nrValues = IDUM(1)
           if (NrValues .gt. NHLP) then
              write(iout1,*) ' User defined VEQD interpolation table too long; please limit to at most ', NHLP,' values'
              write(iout1,*) ' only first ', NHLP,' values are read and used'
           endif
           RetVal = RetVal + GetVRS2(STRING,' dg ',2,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file', &
                                     IOUT1, CDUM(1), RDUM(1), IDUM(1), NrValues, IflRtn)
           Do i=1, nrValues
               Walrus_VEQDG_DG(IrrRunoffSub,i) = RDUM(i)
           enddo
           RetVal = RetVal + GetVRS2(STRING,' veq ',2,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file', &
                                     IOUT1, CDUM(1), RDUM(1), IDUM(1), NrValues, IflRtn)
           Do i=1, nrValues
               Walrus_VEQDG_VEQ(IrrRunoffSub,i) = RDUM(i)
           enddo
!         check VEQ values
           IRRRunoffRef = IRRRunoffSub
           Do i=1, nrValues
              If (WALRUS_VEQDG_DG(IRRRunoffRef,i) .lt. 0 ) then
                 Err969 = .true.
                 call ErrMsgStandard (969, 0, ' VEQ definition: dg values should be non-negative for definition ', WALRUS_VIT(iRRRunoffSub))
              Endif
              If (WALRUS_VEQDG_VEQ(IRRRunoffRef,i) .lt. 0) then
                 Err969 = .true.
                 call ErrMsgStandard (969, 0, ' VEQ definition: er values should be >=0 for definition ',WALRUS_VIT(iRRRunoffSub))
              Endif
              If (i .gt. 1) then
                if (WALRUS_VEQDG_DG(IRRRunoffRef,i) .le. WALRUS_VEQDG_DG(IRRRunoffRef,i-1) ) then
                  Err969 = .true.
                  write(iout1,*) ' index i and i-1', i, i-1
                  write(iout1,*) ' VEQDG_DG values i and i-1', Walrus_VEQDG_DG(IRRRunoffRef,i), Walrus_VEQDG_DG(IRRRunoffRef,i-1)
                  call ErrMsgStandard (969, 0, ' VEQD definition: dg values should be specified in increasing order for definition', WALRUS_VIT(iRRRunoffSub))
                Endif
              Endif
              If (i .gt. 1) then
                If(WALRUS_VEQDG_VEQ(IRRRunoffRef,i) .lt. WALRUS_VEQDG_VEQ(IRRRunoffRef,i-1) ) then
                  Err969 = .true.
                  write(iout1,*) ' index i and i-1', i, i-1
                  write(iout1,*) ' VEQDG_VEQ values i and i-1', Walrus_VEQDG_VEQ(IRRRunoffRef,i), Walrus_VEQDG_VEQ(IRRRunoffRef,i-1)
                  call ErrMsgStandard (969, 0, ' VEQD definition: veq values should be specified in non-decreasing order for definition', WALRUS_VIT(iRRRunoffSub))
                endif
              Endif
           Enddo
       ! Assign definition to individual nodes
           Do iRRRunoff = 1, ncRRRunoff
              if (RRRunoff_CompOption(IRRRUnoff) .eq. 6) then      ! WALRUS
                iRRRunoffSub = RRRunoff_SubIndex(iRRRunoff)
                if (StringComp(WALRUS_VIT(IRRRunoffSub), Name, CaseSensitive) )  then
                    ReferenceToDefinition(iRRRunoffSub) = teller
                    WALRUS_VEQLength (IRRRunoffSub) = NrValues
                    Do i=1,NrValues
                       WALRUS_VEQDG_DG(IRRRunoffSub,i) = WALRUS_VEQDG_DG(IRRRunoffRef,i)
                       WALRUS_VEQDG_VEQ(IRRRunoffSub,i) = WALRUS_VEQDG_VEQ(IRRRunoffRef,i)
                    Enddo
!                   write(*,*) ' Walrus index ', IRRRunoffSub
!                   write(*,*) ' Walrus VEQD user defined table '
!                   write(*,*) ' length =', WALRUS_VEQLength(IRRRunoffSub)
!                   write(*,*) ' DG values =', (WALRUS_VEQDG_DG(IRRRunoffSub,i), i=1,NrValues)
!                   write(*,*) ' VEQvalues =', (WALRUS_VEQDG_VEQ(IRRRunoffSub,i), i=1,NrValues)
                endif
              endif
           Enddo
         endif  ! end of VEQD evap reduction function occurs
        endif  ! end of VEQD
       Enddo
6113   Continue

       If (RetVal .gt. 0) call ErrMsgStandard (972, 0, ' Error reading data from RR-runoff file ', ' Error getting VEQD records')
       Do iRRRunoff = 1, ncRRRunoff
          IRRRunoffSub = RRRunoff_SubIndex(IRRRunoff)
          if (RRRunoff_CompOption(IRRRunoff) .eq. 6) then
              if (ReferenceToDefinition(iRRRunoffSub) .eq. 0 .and. WALRUS_VIT(iRRRunoffSub) .ne. '')  then
                  Err969 = .true.
                  call ErrMsgStandard (969, 0, ' VEQD definition not found in RRRunoff file.', WALRUS_VIT(iRRRunoffSub))
             endif
          endif
       Enddo
       If (Err969) call ErrMsgStandard (972, 0, ' Not enough WALRUS data found', ' Some VEQD user defined functions not present or not correctly specified in input file')
    endif

! WQH records
    Err969 = .false.
    if (NrWalrusNodes .gt. 0 .and. WalrusUserDefinedQHFunctionExists) then
       Rewind(infile1)
       call SetMessage(LEVEL_INFO, 'Read Walrus data - user defined QH relation')

       RetVal = 0
       ReferenceToDefinition = 0
       endfil = .false.
       teller = 0
       CALL SKPCOM (Infile1, ENDFIL, 'ODS')
       Do while (.not. endfil)
        READ (Infile1,'(A1000)',END=6114,ERR=150,IOSTAT=IECODE) STRING
!        Write(*,*) 'String read', String(1:100)
        IF (STRING(1:4) .EQ. 'WQH ') Then
 105     continue
         lenString = len_trim (String)
         if (String(lenString-3:lenstring) .ne. ' wqh') then
             READ(Infile1,'(A1000)',END=21,ERR=150,IOSTAT=IECODE)  String1
             lenString1 = len_trim (String1)
             String = String(1:lenString) // " " // String1 (1:lenString1)
             goto 105
         endif
         ! Read WQH definition  id
         Retval = RetVal + GetVAR2 (STRING,' id ',1,' RRRunoff_readAscii',' WQH qh relation data',IOUT1, &
                                    CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
         name = CDUM(1)
         teller = teller + 1
         ! Eerst testen of WQH definition wel gebruikt wordt, dan pas verwerken
!         write(*,*) ' used Walrus WQH functions'
!         Do i=1,NCRRRunoffWalrus
!            write(*,*) WALRUS_QIT(i)
!            write(*,*) 'Length', Len_trim (WALRUS_QIT(i))
!         Enddo
         IRRRunoff = FindString (NcRRRunoffWalrus, WALRUS_QIT, Name, NcRRRunoffWalrus, CaseSensitive)
         Occurs = (IRRRunoff .gt. 0)
         if (IRRRunoff .gt. 0) then
            IRRRunoffSub = IRRRunoff
            if (ReferenceToDefinition(iRRRunoffSub) .gt. 0) then
               call SetMessage(LEVEL_ERROR, 'WQH Definition '//name(1:Len_Trim(Name))//' double in datafile Runoff nodes')
               LevelError = .true.
               Occurs = .false.    ! voorkomt verdere verwerking
            elseIf (CleanRRFiles) then
               ! cleaning RR files
               ! bit more intelligent for long WQH records
               String0 = String
 1071          continue
               lenstring = len_trim(String)
               if (LenString .le. 999) then
                   write(Iounit,'(A)') String (1:len_trim(String))
               else
                   ipos  = FndFrst (' ',String(979:999),.false.)
                   if (ipos .gt. 0) then
                      ipos = 979+ipos-1
                      write(Iounit,'(A)') String (1:ipos)
                      String = String(ipos+1:)
                      goto 1071
                   endif
               endif
               String = String0
            endif
         endif
        ! Verwerk WQH Type definition
         if (occurs) then
           ! Read type definition parameters
           Retval = RetVal + GetVAR2 (STRING,' nv ',3,' RRRunoff_readAscii',' WQH qh relation data',IOUT1, &
                       CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
           nrValues = IDUM(1)
           if (NrValues .gt. NHLP) then
              call SetMessage(LEVEL_FATAL, 'PARAMETER NHLP TOO SMALL')
           endif
           RetVal = RetVal + GetVRS2(STRING,' hs ',2,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file', &
                                     IOUT1, CDUM(1), RDUM(1), IDUM(1), NrValues, IflRtn)
           Do i=1, nrValues
               Walrus_QH_H(IrrRunoffSub,i) = RDUM(i)
           enddo
           RetVal = RetVal + GetVRS2(STRING,' q ',2,' RRRunoffNode-ReadAscii',' RRRunoffNode.3B file', &
                                     IOUT1, CDUM(1), RDUM(1), IDUM(1), NrValues, IflRtn)
           Do i=1, nrValues
               Walrus_QH_Q(IrrRunoffSub,i) = RDUM(i)
           enddo
!         check QH values
           IRRRunoffRef = IRRRunoffSub
           Do i=1, nrValues
              If (WALRUS_QH_H(IRRRunoffRef,i) .lt. 0 ) then
                 Err969 = .true.
                 call ErrMsgStandard (969, 0, ' QH definition: hs values should be non-negative for definition ', WALRUS_QIT(iRRRunoffSub))
              Endif
              If (WALRUS_QH_Q(IRRRunoffRef,i) .lt. 0) then
                 Err969 = .true.
                 call ErrMsgStandard (969, 0, ' QH definition: q values should be >=0 for definition ',WALRUS_QIT(iRRRunoffSub))
              Endif
              If (i .gt. 1) then
                if (WALRUS_QH_H(IRRRunoffRef,i) .le. WALRUS_QH_H(IRRRunoffRef,i-1) ) then
                  Err969 = .true.
                  write(iout1,*) ' index i and i-1', i, i-1
                  write(iout1,*) ' QH_H values i and i-1', Walrus_QH_H(IRRRunoffRef,i), Walrus_QH_H(IRRRunoffRef,i-1)
                  call ErrMsgStandard (969, 0, ' WQH definition: hs values should be specified in increasing order for definition', WALRUS_QIT(iRRRunoffSub))
                Endif
              Endif
              If (i .gt. 1) then
                If(WALRUS_QH_Q(IRRRunoffRef,i) .lt. WALRUS_QH_Q(IRRRunoffRef,i-1) ) then
                  Err969 = .true.
                  write(iout1,*) ' index i and i-1', i, i-1
                  write(iout1,*) ' QH_Q values i and i-1', Walrus_QH_Q(IRRRunoffRef,i), Walrus_QH_Q(IRRRunoffRef,i-1)
                  call ErrMsgStandard (969, 0, ' WQH definition: q values should be specified in non-decreasing order for definition', WALRUS_QIT(iRRRunoffSub))
                endif
              Endif
           Enddo
       ! Assign definition to individual nodes
           Do iRRRunoff = 1, ncRRRunoff
              if (RRRunoff_CompOption(IRRRUnoff) .eq. 6) then      ! WALRUS
                iRRRunoffSub = RRRunoff_SubIndex(iRRRunoff)
                if (StringComp(WALRUS_QIT(IRRRunoffSub), Name, CaseSensitive) )  then
                    ReferenceToDefinition(iRRRunoffSub) = teller
                    WALRUS_QHLength (IRRRunoffSub) = NrValues
                    Do i=1,NrValues
                       WALRUS_QH_H(IRRRunoffSub,i) = WALRUS_QH_H(IRRRunoffRef,i)
                       WALRUS_QH_Q(IRRRunoffSub,i) = WALRUS_QH_Q(IRRRunoffRef,i)
                    Enddo
!                   write(*,*) ' Walrus index ', IRRRunoffSub
!                   write(*,*) ' Walrus WQH user defined table '
!                   write(*,*) ' length =', WALRUS_VEQLength(IRRRunoffSub)
!                   write(*,*) ' H  values =', (WALRUS_QH_H(IRRRunoffSub,i), i=1,NrValues)
!                   write(*,*) ' Q  values =', (WALRUS_QH_Q(IRRRunoffSub,i), i=1,NrValues)
                endif
              endif
           Enddo
         endif  ! end of WQH occurs
        endif  ! end of WQH
       Enddo
6114   Continue

       If (RetVal .gt. 0) call ErrMsgStandard (972, 0, ' Error reading data from RR-runoff file ', ' Error getting WQH records')
       Do iRRRunoff = 1, ncRRRunoff
          IRRRunoffSub = RRRunoff_SubIndex(IRRRunoff)
          if (RRRunoff_CompOption(IRRRunoff) .eq. 6) then
              if (ReferenceToDefinition(iRRRunoffSub) .eq. 0 .and. WALRUS_QIT(iRRRunoffSub) .ne. '')  then
                  Err969 = .true.
                  call ErrMsgStandard (969, 0, ' WQH definition not found in RRRunoff file.', WALRUS_QIT(iRRRunoffSub))
             endif
          endif
       Enddo
       If (Err969) call ErrMsgStandard (972, 0, ' Not enough WALRUS data found', ' Some VEQD user defined functions not present or not correctly specified in input file')
    endif

! HSTT records
    Err969 = .false.
    if (NrWalrusNodes .gt. 0 .and. WalrusHSMinTimeTableExists) then
       Rewind(infile1)
       call SetMessage(LEVEL_INFO, 'Read Walrus data - HSmin time tables')

       RetVal = 0
       ReferenceToDefinition = 0
       endfil = .false.
       teller = 0
       CALL SKPCOM (Infile1, ENDFIL, 'ODS')
       Do while (.not. endfil)
          Success = GetRecord(Infile1, 'HSTT', Endfil, idebug, Iout1)     ! get record van keyword HSTT tot hstt, zet in buffer
          IF (.not. success) GOTO 6115
          IF (ENDFIL) GOTO 6115
          Success = GetTableName (TabYesNo, TableName, ' id ', Iout1)     ! get table name via keyword ' id ', TabYesNo=TBLE found
          IF (.not. success) GOTO 6115
           Success = GetStringFromBuffer (KeepBufString)
           IF (.not. Success .and. CleanRRFiles)   then
               Call ErrMsgStandard (999, 3, ' Local buffer RRRunoffmodule HSTT record WALRUS too small', ' Input skipped')
               GOTO 6115
           Endif
          If (TabYesNo .and. TableName .ne. '') Then
!            Er is een tabel gedefinieerd, met een niet-lege naam
!            Eerst testen of tabel definition wel gebruikt wordt, dan pas verwerken
             NrColumns = 1
             IRRRunoffSub = FindString (NcRRRunoff, Walrus_HSMINTable, TableName, NcRRRunoff, CaseSensitive)
             Occurs = (IRRRunoffSub .gt. 0)
             if (IRRRunoffSub .gt. 0) then
                if (Walrus_HSMINRefTable(iRRRunoffSub) .gt. 0) then
                  call SetMessage(LEVEL_ERROR, 'Walrus HsMin table Definition '//Tablename(1:Len_trim(TableName))// ' double in datafile Sacrmnto.3b')
                  NrColumns = 0  ! voorkomt verdere verwerking
                endif
             endif
!            Verwerken definitie
             if (occurs .and. NrColumns .gt. 0) then
! Get table with name TableName, NrColumns data fields, result in global arrays; tabel nummer is TableNr
                Success = GetTable (TableHandle, TableName, NrColumns, TableNr, idebug, Iout1)
                IF (.not. success) GOTO 6115
! clean RR files
                If (CleanRRFiles) then
                  ! use KeepBufString to write to file
                  ! first till TBLE
                  ! then till < characters
                  ! then till the end of the buffer string
                  lenstring = len_trim(KeepBufString)
                  ipos  = FndFrst ('TBLE ',KeepBufString(1:lenstring),.false.)
                  if (ipos .gt. 0) then
                     write(Iounit,'(A)') KeepBufString (1:ipos+4)
                     KeepBufString(1:) = KeepBufString(ipos+5:)
                  else
                     ! error: no TBLE found
                       call SetMessage(LEVEL_ERROR, 'Structure Table Definition '//Tablename(1:Len_trim(TableName))//' TBLE not found')
                  endif
 1081             continue
                  lenstring = len_trim(KeepBufString)
                  ipos  = FndFrst (' < ',KeepBufString(1:lenstring),.false.)
                  if (ipos .gt. 0) then
                     write(Iounit,'(A)') KeepBufString (1:ipos+2)
                     KeepBufString(1:) = KeepBufString(ipos+3:)
                     goto 1081
                  else
                     ! write remaining part
                     write(Iounit,'(A)') KeepBufString (1:lenstring)
                  endif
                Endif
! Set references
                Do iRRRunoffSub = 1, ncRRRunoff
                   if (StringComp (Walrus_HSMINTable(IRRRunoffSub), TableName, CaseSensitive) ) Walrus_HSMinRefTable(iRRRunoffSub) = TableNr
                Enddo
              Endif
          Endif
          Call SKPCOM (Infile1, ENDFIL,'ODS')
       Enddo
6115   Continue


       If (RetVal .gt. 0) call ErrMsgStandard (972, 0, ' Error reading data from RR-runoff file ', ' Error getting HSTT records')
       Do iRRRunoff = 1, ncRRRunoff
          IRRRunoffSub = RRRunoff_SubIndex(IRRRunoff)
          if (RRRunoff_CompOption(IRRRunoff) .eq. 6) then
             if (Walrus_HSMinRefTable(iRRRunoffSub) .eq. 0 .and. WALRUS_HSMinTable(iRRRunoffSub) .ne. '')  then
                  Err969 = .true.
                  call ErrMsgStandard (969, 0, ' HSTT definition not found in RRRunoff file.', WALRUS_HSMinTable(iRRRunoffSub))
             endif
          endif
       Enddo
       If (Err969) call ErrMsgStandard (972, 0, ' Not enough WALRUS data found', ' Some HSmin Time Tables functions not present or not correctly specified in input file')
    endif


     iDebug = 0

     NcRRRunoffNAM      = NrNAMNodes
     NcRRRunoffSCS      = NrScsNodes
     NcRRRunoffHBV      = NrHbvNodes
     NcRRRunoffExternal = NrExtNodes
     NcRRRunoffLGSI     = NrLGSINodes
     NcRRRunoffWagmod   = NrWageningenNodes
     NcRRRunoffWalrus   = NrWalrusNodes

     if (LevelError)  Call ErrMsgStandard (981, 0, ' Error: See earlier error messages from subroutine ', ' RRRunoffNode_ReadAscii' )

! cleaning RR files
     If (CleanRRFiles) Call closeGP (Iounit)

     return

 150 CONTINUE
     DeAllocate ( AlreadyRead)
     DeAllocate ( ReferenceToDefinition)
     call SetMessage(LEVEL_FATAL, 'Read error in RRRunoffNode ReadASCII')
! cleaning RR files
     If (CleanRRFiles) Call closeGP (Iounit)

     Return
  End subroutine RRRunoffNode_readASCII



  Subroutine ReadCacheFile (ICache, Iout1)

  implicit none

  Integer  ICache, Iout1
  Integer  i, j, k, j1, k1, IRRRunoff, Inode, IRRRunoffSub
  Character(Len=999) String

  Do i=1,NcRRRunoff
    IRRRunoffSub = RRRunoff_SubIndex(i)
    if (RRRunoff_CompOption(i) .eq. 4) then
       if (LGSI_DelayNrTimesteps(iRRRunoffSub) * LGSI_DelayTimestepSize(iRRRUnoffSub) .gt. LGSI_MaxDelayLength * TimeSettings%TimestepSize + 0.1) then
         write(Iout1,*) ' Delay coefficients for LGSI node ', i, ' too long'
         write(Iout1,*) ' Delay coefficients are ',  (LGSI_DefinedDelayCoefficients(iRRRunoffSub,k),k=1,LGSI_DelayNrTimesteps(iRRRunoffSub))
         call ErrMsgStandard (981, 0, ' Error LGSI_DelayDefinitions: too large for arraylength and timestep size', ' Increase simulation timestep or reduce nr. of delay coefficients')
       endif
       Read(Icache, '(A999)') String
       Do j=1,LGSI_MaxDelayLengthPlus1
          Read(icache,*) iRRRunoffSub, j1, LGSI_DelayCoefficients(iRRRunoffSub,j1)
          If (j1 .ne. j) write(Iout1,*) ' Error1  j1 <>j'
       Enddo
    endif
  Enddo

  Do iRRRunoff=1,NcRRRunoff
      IRRRunoffSub = RRRunoff_SubIndex(IRRRunoff)
      Do j=1,ncnode
         if (Einode(j,2) .eq. IRRRunoff .and. Einode(j,3) .eq. 22) Inode = j        ! kind 22= LGSI
      Enddo
      If (RRRunoff_CompOption(IRRRunoff) .eq. 4) then  !LGSI
         Do j=1,LGSI_NrSubAreas(IRRRunoffSub)
!           Read(Icache, '(A999)') String
!           Read(Icache, '(A999)') String
!           Do k=1,LGSI_MaxInterpLength
!              Read (Icache,'(I4,1X,4E15.5)') k, x, sD, PrecipReduction, EvapReduction
!           enddo
!           Write(Icache, *) 'GW Level        GW Volume '
           Read(Icache, '(A999)') String
           Read(Icache, '(A999)') String
           Do k=1,LGSI_MaxInterpLength
              read(Icache,*) k1, LGSI_InterpGWLevelGWV(k,IRRRunoffSub,j),LGSI_InterpGWVolume(k,IRRRunoffSub,j)
              If (k1 .ne. k) write(Iout1,*) ' Error1  k1 <>k'
              if (k .gt. 1) then
                if (LGSI_InterpGWVolume(k,IRRRunoffSub,j) .gt. LGSI_InterpGWVolume(k-1,IRRRunoffSub,j)+0.0001) then
                    call SetMessage(LEVEL_ERROR, 'Error in GwDepth-GWVolume relation for node with id: '//trim(Id_Nod(Inode)))
                    write(Iout1,'(A110,2I5,F6.2)') ' Error: GwDepth-GWVolume relation - volumes are increasing for lower depths for node/subarea/gwdepth', IRRRunoffSub, j, LGSI_InterpGWLevelGWV(k,IRRRunoffSub,j)
                endif
              endif
           enddo
!           Write(Icache, *) 'GW Level        Unsat Volume '
           Read(Icache, '(A999)') String
           Read(Icache, '(A999)') String
           Do k=1,LGSI_MaxInterpLength
              Read(Icache,*) k1, LGSI_InterpGWLevelUnsatV(k,IRRRunoffSub,j),LGSI_InterpUnsatVolume(k,IRRRunoffSub,j)
              If (k1 .ne. k) write(Iout1,*) ' Error2  k1 <>k'
              if (k .gt. 1) then
                if (LGSI_InterpUnsatVolume(k,IRRRunoffSub,j) .lt. LGSI_InterpUnsatVolume(k-1,IRRRunoffSub,j)-0.0001) then
                    call SetMessage(LEVEL_ERROR, 'Error in GwDepth-UnsatVolume relation for node with id: '//trim(Id_Nod(Inode)))
                    write(Iout1,'(A110,2I5,F6.2)') ' Error: GwDepth-UnsatVolume relation - volumes are decreasing for lower depths for node/subarea/gwdepth', IRRRunoffSub, j, LGSI_InterpGWLevelUnsatV(k,IRRRunoffSub,j)
                endif
              endif
           enddo
!           Write(Icache, *) 'GW Level        Surf Volume '
           Read(Icache, '(A999)') String
           Read(Icache, '(A999)') String
           Do k=1,LGSI_MaxInterpLength
              Read(Icache,*) k1, LGSI_InterpGWLevelSurfV(k,IRRRunoffSub,j),LGSI_InterpSurfVolume(k,IRRRunoffSub,j)
              If (k1 .ne. k) write(Iout1,*) ' Error3  k1 <>k'
              if (k .gt. 1) then
                if (LGSI_InterpSurfVolume(k,IRRRunoffSub,j) .gt. LGSI_InterpSurfVolume(k-1,IRRRunoffSub,j)+0.0001) then
                    call SetMessage(LEVEL_ERROR, 'Error in GwDepth-SurfVolume relation for node with id: '//trim(Id_Nod(Inode)))
                    write(Iout1,'(A110,2I5,F6.2)') ' Error: GwDepth-SurfVolume relation - volumes are increasing for lower depths for node/subarea/gwdepth', IRRRunoffSub, j, LGSI_InterpGWLevelSurfV(k,IRRRunoffSub,j)
                endif
              endif
           enddo
!           Write(Icache, *) 'GW Level        Total Volume '
           Read(Icache, '(A999)') String
           Read(Icache, '(A999)') String
           Do k=1,LGSI_MaxInterpLength
              Read(Icache,*) k1, LGSI_InterpGWLevelTotalV(k,IRRRunoffSub,j),LGSI_InterpTotalVolume(k,IRRRunoffSub,j)
              If (k1 .ne. k) write(Iout1,*) ' Error4  k1 <>k'
           enddo
!           Write(Icache, *) 'GW Level        DrainageFlow Overland Flow Quickflow '
           Read(Icache, '(A999)') String
           Read(Icache, '(A999)') String
           Do k=1,LGSI_MaxInterpLength
              Read(Icache,'(I5,4F12.6)') k1, LGSI_InterpGWLevelDrain(k,IRRRunoffSub,j),LGSI_InterpDrainageFlow(k,IRRRunoffSub,j), &
                                 LGSI_InterpOverlandFlow(k,IRRRunoffSub,j),LGSI_InterpQuickFlow(k,IRRRunoffSub,j)
              If (k1 .ne. k) write(Iout1,*) ' Error5  k1 <>k'
           enddo
!           Write(Icache, *) 'GW Level        Seepageflow '
           Read(Icache, '(A999)') String
           Do k=1,LGSI_MaxInterpLength
              Read(ICache, *) k1, LGSI_InterpGWLevelSeepage(k,IRRRunoffSub),LGSI_InterpSeepageFlow(k,IRRRunoffSub)
              If (k1 .ne. k) write(Iout1,*) ' Error6  k1 <>k'
           enddo
         enddo
      Endif
  Enddo

  end subroutine ReadCacheFile


  Subroutine WriteCacheFile(ICache)

  implicit none

  Integer  ICache
  Integer  i, j, k, IRRRunoff, IRRRunoffSub, Inode

  Do i=1,NcRRRunoff
    IRRRunoffSub = RRRunoff_SubIndex(i)
    if (RRRunoff_CompOption(i) .eq. 4) then
       write(Icache,*) ' Delay coefficients ', iRRRunoffSub
       Do j=1,LGSI_MaxDelayLengthPlus1
          write(icache,*) iRRRunoffSub, j, LGSI_DelayCoefficients(iRRRunoffSub,j)
       Enddo
    endif
  Enddo

  Do iRRRunoff=1,NcRRRunoff
      IRRRunoffSub = RRRunoff_SubIndex(IRRRunoff)
      Do j=1,ncnode
         if (Einode(j,2) .eq. IRRRunoff .and. Einode(j,3) .eq. 22) Inode = j        ! kind 22= LGSI
      Enddo
      If (RRRunoff_CompOption(IRRRunoff) .eq. 4) then  !LGSI
         Do j=1,LGSI_NrSubAreas(IRRRunoffSub)
           Write(Icache, *) 'IRRRunoffSub, subarea', IRRRunoffSub, j
           Write(Icache, *) 'GW Level        GW Volume '
           Do k=1,LGSI_MaxInterpLength
              write(Icache,*) k, LGSI_InterpGWLevelGWV(k,IRRRunoffSub,j),LGSI_InterpGWVolume(k,IRRRunoffSub,j)
           enddo
           Write(Icache, *) 'IRRRunoffSub, subarea', IRRRunoffSub, j
           Write(Icache, *) 'GW Level        Unsat Volume '
           Do k=1,LGSI_MaxInterpLength
              write(Icache,*) k, LGSI_InterpGWLevelUnsatV(k,IRRRunoffSub,j),LGSI_InterpUnsatVolume(k,IRRRunoffSub,j)
           enddo
           Write(Icache, *) 'IRRRunoffSub, subarea', IRRRunoffSub, j
           Write(Icache, *) 'GW Level        Surf Volume '
           Do k=1,LGSI_MaxInterpLength
              write(Icache,*) k, LGSI_InterpGWLevelSurfV(k,IRRRunoffSub,j),LGSI_InterpSurfVolume(k,IRRRunoffSub,j)
           enddo
           Write(Icache, *) 'IRRRunoffSub, subarea', IRRRunoffSub, j
           Write(Icache, *) 'GW Level        Total Volume '
           Do k=1,LGSI_MaxInterpLength
              write(Icache,*) k, LGSI_InterpGWLevelTotalV(k,IRRRunoffSub,j),LGSI_InterpTotalVolume(k,IRRRunoffSub,j)
           enddo
           Write(Icache, *) 'IRRRunoffSub, subarea', IRRRunoffSub, j
           Write(Icache, *) 'GW Level        DrainageFlow Overland Flow Quickflow '
           Do k=1,LGSI_MaxInterpLength
              write(Icache,'(I5,4F12.6)') k, LGSI_InterpGWLevelDrain(k,IRRRunoffSub,j),LGSI_InterpDrainageFlow(k,IRRRunoffSub,j), &
                                 LGSI_InterpOverlandFlow(k,IRRRunoffSub,j),LGSI_InterpQuickFlow(k,IRRRunoffSub,j)
           enddo
           Write(Icache, *) 'GW Level        Seepageflow '
           Do k=1,LGSI_MaxInterpLength
              write(Icache,*) k, LGSI_InterpGWLevelSeepage(k,IRRRunoffSub),LGSI_InterpSeepageFlow(k,IRRRunoffSub)
           enddo
         enddo
      endif
    enddo

  end subroutine WriteCacheFile





  Subroutine RRRunoffNode_Init1(IEvent, Iout1, ICache, NAMAlfa, FirstCall, ReadLGSICacheFile)

  implicit none

  Integer IEvent, IOut1, i, j, k, Idebug, ilast1, inode, iu, IOutWagMod, itmstp, IRRRunoff, IRRRunoffSub, isub, ICache
  Logical  LevelError, ReadLGSICacheFile

  Integer          Ibnd, iow
  real             NAMAlfa, fact

  integer          NrSteps, itmevap, NrsEvp


  Logical Success, FirstCall, err971
  Double precision  x, sD, PrecipReduction, Evapreduction, LGSIGwl, LGSIVolume
  Double precision  SumUCD, SumUJ

  Double precision  GWLevelArray(LGSI_MaxInterpLengthPlus1), GWVolumeArray (LGSI_MaxInterpLengthPlus1)

  integer           D_Ifreal, idum, LastTm
  double precision  eps, RLastTM, StrtTM

  Double Precision  Julian, Julian01011970
  Double Precision  JulianEventStart
  Integer           DateEventStart, TimeEventStart
  Integer, external :: addWalrusInstances, WalrusSet, WalrusSetST, WalrusSetSeqC, WalrusInit
  Integer, external :: WalrusSetWDVByTable, WalrusSetdVeqdGbyTable, WalrusSetBetadVByTable, WalrusSetQhSByTable
  Integer           :: retValWalrusCall, WalrusFirst
  double precision  :: dbleArea, GFrac
  double precision  :: timestepsize

   WalrusFirst = 1
   GFrac = 1.D0
   LevelError = .false.
   eps  = 1.D-7

   Idebug = Conffil_get_Idebug()

   if (NcRRRunoffHBV .gt. 0) then
     HBV_DrySnowContent     =  HBV_InitialDrySnowContent
     HBV_FreeWaterContent   =  HBV_InitialFreeWaterContent
     HBV_SoilMoisture       =  HBV_InitialMoisture
!    HBV_QRunoffInmm        =  HBV_InitialQRunoffInmm
     HBV_UpperZoneContent   =  HBV_InitialUpperZoneContent
     HBV_LowerZoneContent   =  HBV_InitialLowerZoneContent
   endif

   if (NcRRRunoffLGSI .gt. 0) then
     ! from input file:
     if (FirstCall) then
        LGSI_InitGwl = LGSI_SpecifiedInitGwl
     else
        LGSI_InitGwl = LGSI_NewGwl
     endif
     ! make initial and final equal
       LGSI_NewGwl  = LGSI_Initgwl
       If (Ievent .eq. 1 .and. FirstCall) then
        if (ReadLGSICacheFile) then
          Call ReadCacheFile(ICache, Iout1)
        else
          Call LGSI_ConstructDelayTable
          Call LGSI_ConstructInterpolationTables
          Idebug = Conffil_get_Idebug()
          Do iRRRunoff=1,NcRRRunoff
              IRRRunoffSub = RRRunoff_SubIndex(IRRRunoff)
              Do j=1,ncnode
                 if (Einode(j,2) .eq. IRRRunoff .and. Einode(j,3) .eq. 22) Inode = j        ! kind 22= LGSI
              Enddo
              If (RRRunoff_CompOption(IRRRunoff) .eq. 4) then  !LGSI
                 Do j=1,LGSI_NrSubAreas(IRRRunoffSub)
                   x = -10.1
                   if (idebug .ne. 0) Write(Idebug, *) 'IRRRunoffSub, subarea', IRRRunoffSub, j
                   if (idebug .ne. 0)Write(Idebug, *) 'GW Level  x  sD  PrecipReduction EvapReduction'
                   Do k=1,LGSI_MaxInterpLength
                      x = x + 0.1
                      If (LGSI_Type(IRRRunoffSub,j) .eq. 1) then  !normal distribution
                         Call ComputePsdNormal (LGSI_NormalMaxSD(IRRRunoffSub,j), LGSI_NormalMinSD(IRRRunoffSub,j), LGSI_NormalNusDMax(IRRRunoffSub,j), LGSI_NormalNb(IRRRunoffSub,j), x, Sd)
                      ElseIf (LGSI_Type(IRRRunoffSub,j) .eq. 2) then  !gamma distribution
                         Call ComputePsdGamma (LGSI_GammaAg(IRRRunoffSub,j), LGSI_GammaBg(IRRRunoffSub,j), x, sD)
                      endif
                      Call ComputePrecipitationReduction (x, sD, PrecipReduction)
                      Call ComputeEvaporationReduction   (x, LGSI_ERD(IRRRunoffSub,j), LGSI_ERSD(IRRRunoffSub,j), EvapReduction)
                      if (idebug .ne. 0)write(Idebug,'(I4,1X,4E15.5)') k, x, sD, PrecipReduction, EvapReduction
                   enddo
                   if (idebug .ne. 0) Write(Idebug, *) 'IRRRunoffSub, subarea', IRRRunoffSub, j
                   if (idebug .ne. 0) Write(Idebug, *) 'GW Level        GW Volume '
                   Do k=1,LGSI_MaxInterpLength
                      write(Idebug,*) k, LGSI_InterpGWLevelGWV(k,IRRRunoffSub,j),LGSI_InterpGWVolume(k,IRRRunoffSub,j)
                      if (k .gt. 1) then
                        if (LGSI_InterpGWVolume(k,IRRRunoffSub,j) .gt. LGSI_InterpGWVolume(k-1,IRRRunoffSub,j)+0.0001) then
                            call SetMessage(LEVEL_ERROR, 'Error in GwDepth-GWVolume relation for node with id: '//trim(Id_Nod(Inode)))
!                           LevelError = .true.
                            write(Iout1,'(A110,2I5,F6.2)') ' Error: GwDepth-GWVolume relation - volumes are increasing for lower depths for node/subarea/gwdepth', IRRRunoffSub, j, LGSI_InterpGWLevelGWV(k,IRRRunoffSub,j)
                        endif
                      endif
                   enddo
                   if (idebug .ne. 0) Write(Idebug, *) 'IRRRunoffSub, subarea', IRRRunoffSub, j
                   if (idebug .ne. 0) Write(Idebug, *) 'GW Level        Unsat Volume '
                   Do k=1,LGSI_MaxInterpLength
                      write(Idebug,*) k, LGSI_InterpGWLevelUnsatV(k,IRRRunoffSub,j),LGSI_InterpUnsatVolume(k,IRRRunoffSub,j)
                      if (k .gt. 1) then
                        if (LGSI_InterpUnsatVolume(k,IRRRunoffSub,j) .lt. LGSI_InterpUnsatVolume(k-1,IRRRunoffSub,j)-0.0001) then
                            call SetMessage(LEVEL_ERROR, 'Error in GwDepth-UnsatVolume relation for node with id: '//trim(Id_Nod(Inode)))
!                           LevelError = .true.
                            write(Iout1,'(A110,2I5,F6.2)') ' Error: GwDepth-UnsatVolume relation - volumes are decreasing for lower depths for node/subarea/gwdepth', IRRRunoffSub, j, LGSI_InterpGWLevelUnsatV(k,IRRRunoffSub,j)
                        endif
                      endif
                   enddo
                   if (idebug .ne. 0) Write(Idebug, *) 'IRRRunoffSub, subarea', IRRRunoffSub, j
                   if (idebug .ne. 0) Write(Idebug, *) 'GW Level        Surf Volume '
                   Do k=1,LGSI_MaxInterpLength
                      if (idebug .ne. 0) write(Idebug,*) k, LGSI_InterpGWLevelSurfV(k,IRRRunoffSub,j),LGSI_InterpSurfVolume(k,IRRRunoffSub,j)
                      if (k .gt. 1) then
                        if (LGSI_InterpSurfVolume(k,IRRRunoffSub,j) .gt. LGSI_InterpSurfVolume(k-1,IRRRunoffSub,j)+0.0001) then
                            call SetMessage(LEVEL_ERROR, 'Error in GwDepth-SurfVolume relation for node with id: '//trim(Id_Nod(Inode)))
!                           LevelError = .true.
                            write(Iout1,'(A110,2I5,F6.2)') ' Error: GwDepth-SurfVolume relation - volumes are increasing for lower depths for node/subarea/gwdepth', IRRRunoffSub, j, LGSI_InterpGWLevelSurfV(k,IRRRunoffSub,j)
                        endif
                      endif
                   enddo
                   if (idebug .ne. 0) Write(Idebug, *) 'IRRRunoffSub, subarea', IRRRunoffSub, j
                   if (idebug .ne. 0) Write(Idebug, *) 'GW Level        Total Volume '
                   Do k=1,LGSI_MaxInterpLength
                      if (idebug .ne. 0) write(Idebug,*) k, LGSI_InterpGWLevelTotalV(k,IRRRunoffSub,j),LGSI_InterpTotalVolume(k,IRRRunoffSub,j)
                   enddo
                   if (idebug .ne. 0) Write(Idebug, *) 'IRRRunoffSub, subarea', IRRRunoffSub, j
                   if (idebug .ne. 0) Write(Idebug, *) 'GW Level        DrainageFlow Overland Flow Quickflow '
                   Do k=1,LGSI_MaxInterpLength
                      if (idebug .ne. 0) write(Idebug,'(I5,4F12.6)') k, LGSI_InterpGWLevelDrain(k,IRRRunoffSub,j),LGSI_InterpDrainageFlow(k,IRRRunoffSub,j), &
                                         LGSI_InterpOverlandFlow(k,IRRRunoffSub,j),LGSI_InterpQuickFlow(k,IRRRunoffSub,j)
                   enddo
                   if (idebug .ne. 0) Write(Idebug, *) 'GW Level        Seepageflow '
                   Do k=1,LGSI_MaxInterpLength
                      if (idebug .ne. 0) write(Idebug,*) k, LGSI_InterpGWLevelSeepage(k,IRRRunoffSub),LGSI_InterpSeepageFlow(k,IRRRunoffSub)
                   enddo
                 enddo
              Endif
          Enddo
          Call WriteCacheFile(ICache)
        endif
       endif
   endif


   Idebug = Conffil_get_Idebug()
   if (NcRRRunoffNAM .gt. 0) then
       if (Ievent .eq. 1) then
         ! Initialisations
         Do iRRRunoff=1,NcRRRunoff
            IRRRunoffSub = RRRunoff_SubIndex(IRRRunoff)
            If (RRRunoff_CompOption(IRRRunoff) .eq. 3) then
               Do j=1,ncnode
                  if (Einode(j,2) .eq. IRRRunoff .and. Einode(j,3) .eq. 31) Inode = j        ! kind 31= NAM
               Enddo
               ! Note: references to equations in NAM design document of 30 December 2016
               ! eq. 5.1, root depth
               NAM_RD(IRRRunoffSub) = NAM_SurfaceLevel(IRRRunoffSub) - NAM_RZBL(IRRRunoffSub)
               ! eq. 5.2  LMax
               NAM_LMax(IRRRunoffSub) = 1000.D0* NAM_Sfc(IRRRunoffSub) * NAM_RD(IRRRUnoffSub)
               ! eq. 5.3  GWSDrzmax
               NAM_GWSDRZmax(IRRRunoffSub) = 1000.D0 * (NAM_SYRZ(IRRRunoffSub) - NAM_Sfc(IRRRUnoffSub)) * NAM_RD(IRRRUnoffSub)
               ! eq. 5.4  GWSDssmax
               NAM_SubSoilThickness(IRRRunoffSub) = NAM_RZBL(IRRRunoffSub) - NAM_GWSBL(IRRRunoffSub)
               NAM_GWSDssmax(IRRRunoffSub) = 1000.D0 * NAM_SYSS(IRRRunoffSub) * NAM_SubsoilThickness(IRRRUnoffSub)
               ! eq. 5.5  GWSDmax
               NAM_GWSDmax(IRRRunoffSub) = NAM_GWSDrzmax(IRRRUnoffSub) + NAM_GWSDssmax(IRRRUnoffSub)
            Endif
         Enddo
       Endif
   endif

   if (NcRRRunoffNAM .gt. 0) then
     ! new initialisations
       NAM_InitialGWDepth = NAM_GWD0
       NAM_LInitial = NAM_L0
       NAM_UInitial = NAM_U0

       NAM_L = NAM_LInitial
       NAM_U = NAM_UInitial
       NAM_GWSD = NAM_InitialGWDepth

       ! eq. 5.6 and 5.7 distribution of GWSD over subsoil and rootzone
       NAM_GWSDss = min (NAM_GWSDSSmax, NAM_GWSD)
       NAM_GWSDRZ = max (0.0d0, NAM_GWSD - NAM_GWSDSS)

     ! make initial and final equal
       NAM_LInitial = NAM_L
       NAM_UInitial = NAM_U
       NAM_GWSDInitial = NAM_GWSD

       if (NcRRRunoffNAM .gt. 0) then
          Do iRRRunoff=1,NcRRRunoff
             IRRRunoffSub = RRRunoff_SubIndex(IRRRunoff)
             If (RRRunoff_CompOption(IRRRunoff) .eq. 3) then  !NAM
                 if (idebug .ne. 0) write(Idebug,*) ' Init1'
                 if (idebug .ne. 0) write(Idebug,*) ' NAM_U', NAM_UInitial(IRRRunoffSub)
                 if (idebug .ne. 0) write(Idebug,*) ' NAM_L', NAM_LInitial(IRRRunoffSub)
                 if (idebug .ne. 0) write(Idebug,*) ' NAM_GWSD', NAM_GWSDInitial(IRRRunoffSub)
                 NAM_VU(IRRRunoffSub) = 1/1000.0D0 * NAM_U(IRRRunoffSub) * NAM_CatchmentArea(IRRRunoffSub)
                 NAM_VL(IRRRunoffSub) = 1/1000.0D0 * NAM_L(IRRRunoffSub) * NAM_CatchmentArea(IRRRunoffSub)
                 NAM_VGWS(IRRRunoffSub) = 1/1000.0D0 * NAM_GWSD(IRRRunoffSub) * NAM_CatchmentArea(IRRRunoffSub)
                 NAM_AVSoil(IRRRunoffSub) = 1/1000.0D0 * (NAM_Lmax(IRRRunoffSub) - NAM_L(IRRRunoffSub) + &
                                                         NAM_GWSDMax(IRRRunoffSub) - NAM_GWSD(IRRRunoffSub) ) * NAM_CatchmentArea(IRRRunoffSub)
                 ! set GWL and GWTD at t=0
                 ! eq. 5.8  GWL
                 if (NAM_GWSDRZ(IRRRunoffSub) .le. 0.D0) then
                    NAM_GWL(IRRRunoffSub) = NAM_GWSBL(IRRRunoffSub) + ( NAM_GWSDSS(IRRRunoffSub) / (1000.D0 * NAM_SYSS(IRRRunoffSub)) )
                 elseif (NAM_GWSDRZ(IRRRunoffSub) .gt. 0.D0 .and.  NAM_GWSDRZ(IRRRunoffSub) .lt. NAMAlfa * NAM_GWSDRZMax(IRRRunoff)) then
                    Fact =  NAM_GWSDRZ(IRRRunoffSub) / NAMalfa / NAM_GWSDRZMax(IRRRunoffSub)
                    NAM_GWL(IRRRunoffSub) = NAM_GWSBL(IRRRunoffSub) + ( NAM_GWSDSS(IRRRunoffSub) / (1000.D0 * NAM_SYSS(IRRRunoffSub)) ) + &
                                                               ( NAM_GWSDRZ(IRRRunoffSub) + NAM_LInitial(IRRRunoffSub) * Fact ) / (1000.D0 * NAM_SYRZ(IRRRunoffSub))
                 elseif (NAM_GWSDRZ(IRRRunoffSub) .ge. NAMAlfa * NAM_GWSDRZMax(IRRRunoff)) then
                    NAM_GWL(IRRRunoffSub) = NAM_GWSBL(IRRRunoffSub) + ( NAM_GWSDSS(IRRRunoffSub) / (1000.D0 * NAM_SYSS(IRRRunoffSub)) ) + &
                                                                      ( NAM_GWSDRZ(IRRRunoffSub) + NAM_LInitial(IRRRunoffSub) ) / (1000.D0 * NAM_SYRZ(IRRRunoffSub))
                 endif
                 ! eq. 5.9  GWTD
                 NAM_GWTD(IRRRunoffSub) = max (0.0d0, NAM_SurfaceLevel(IRRRunoffSub) - NAM_GWL(IRRRunoffSub))
                 if (idebug .ne. 0) then
                    write(Idebug,*) ' NAM_SFC ', NAM_SFC (IRRRunoffSub)
                    write(Idebug,*) ' NAM_SYRZ', NAM_SYRZ(IRRRunoffSub)
                    write(Idebug,*) ' NAM_SYSS', NAM_SYSS(IRRRunoffSub)
                    write(Idebug,*) ' NAM_LInitial', NAM_LInitial(IRRRunoffSub)
                    write(Idebug,*) ' NAM_GWSD', NAM_GWSD(IRRRunoffSub)
                    write(Idebug,*) ' NAM_GWSDSS', NAM_GWSDSS(IRRRunoffSub)
                    write(Idebug,*) ' NAM_GWSDRZ', NAM_GWSDRZ(IRRRunoffSub)
                    write(Idebug,*) ' NAM_GWSBL', NAM_GWSBL(IRRRunoffSub)
                    write(Idebug,*) ' NAM_GWL', NAM_GWL(IRRRunoffSub)
                    write(Idebug,*) ' NAM_GWTD', NAM_GWTD(IRRRunoffSub)
                 Endif
                 ! Tnul output
                 RSLMAP19_RRRunoff(NStartNAM+23,IrrRunoff,1) = NAM_VU(IRRRunoffSub)
                 RSLMAP19_RRRunoff(NStartNAM+24,IrrRunoff,1) = NAM_VL(IRRRunoffSub)
                 RSLMAP19_RRRunoff(NStartNAM+25,IrrRunoff,1) = NAM_VGWS(IRRRunoffSub)
                 RSLMAP19_RRRunoff(NStartNAM+26,IrrRunoff,1) = NAM_GWL(IRRRunoffSub)
                 RSLMAP19_RRRunoff(NStartNAM+27,IrrRunoff,1) = NAM_GWTD(IRRRunoffSub)
                 RSLMAP19_RRRunoff(NStartNAM+28,IrrRunoff,1) = NAM_AVSoil(IRRRunoffSub)
                 RRRunoff_Tnul(NStartNAM+23,irrRunoff) = RSLMAP19_RRRunoff(NStartNAM+23,IRRRunoff,1)
                 RRRunoff_Tnul(NStartNAM+24,irrRunoff) = RSLMAP19_RRRunoff(NStartNAM+24,IRRRunoff,1)
                 RRRunoff_Tnul(NStartNAM+25,irrRunoff) = RSLMAP19_RRRunoff(NStartNAM+25,IRRRunoff,1)
                 RRRunoff_Tnul(NStartNAM+26,irrRunoff) = RSLMAP19_RRRunoff(NStartNAM+26,IRRRunoff,1)
                 RRRunoff_Tnul(NStartNAM+27,irrRunoff) = RSLMAP19_RRRunoff(NStartNAM+27,IRRRunoff,1)
                 RRRunoff_Tnul(NStartNAM+28,irrRunoff) = RSLMAP19_RRRunoff(NStartNAM+28,IRRRunoff,1)
             Endif
          Enddo
       Endif

       err971 = .false.
       Do iRRRunoff=1,NcRRRunoff
          IRRRunoffSub = RRRunoff_SubIndex(IRRRunoff)
          Do j=1,ncnode
             if (Einode(j,2) .eq. IRRRunoff .and. Einode(j,3) .eq. 31) Inode = j        ! kind 31= NAM
          Enddo
          If (RRRunoff_CompOption(IRRRunoff) .eq. 3) then  !NAM
              NAM_HoutSide(IRRRunoffSub) = NAM_GWL(IRRRunoffSub)
              IBND = EIBND(INODE)  ! benedenstrooms een rand
              IOw  = EIOW (INODE)  ! benedenstrooms een open water
              if (IBND .gt. 0) then
                 NAM_HoutSide(IRRRunoffSub) = BndPar(IBND,1)
              elseif (Iow .gt. 0) then
                 NAM_HoutSide(IRRRunoffSub) = LVLOw0(iow)
              endif
              ! input check 17
              idum = D_IfReal (NAM_LMax(IRRRunoffSub), NAM_LTIF(IRRRunoffSub), eps)
              if (idum .lt. 0) then
!                NAM_LMax(IRRRunoffSub) < NAM_LTIF(IRRRunoffSub))
                 call SetMessage(LEVEL_ERROR, 'Error regarding the D-NAM model input for node with id: '//Id_Nod(inode)(1:len_trim(Id_Nod(inode))))
                 LevelError = .true.
                 write(Iout1,'(A)')   ' The specified Lower Zone Threshold for Interflow (LTIF) '
                 write(Iout1,'(A)')   ' exceeds the maximum water depth of the lower zone storage LMax '
                 write(Iout1,'(A,F9.1,A)')  '  Specified LTIF: ', NAM_LTIF(IRRRunoffSub), ' mm'
                 write(Iout1,'(A,F7.2,A)')  '  From input derived LMAX: ', NAM_LMAX(IRRRunoffSub), ' mm'
                 err971 = .true.
              else
                 ! to prevent rounding off errors
                 NAM_LTIF(IRRRunoffSub) = min (NAM_LMAX(IRRRUnoffSub), NAM_LTIF(IRRRunoffSub))
              endif
              ! input check 19
              idum = D_IfReal (NAM_LMax(IRRRunoffSub), NAM_LTG(IRRRunoffSub), eps)
              if (idum .lt. 0) then
!                NAM_LMax(IRRRunoffSub) < NAM_LTG(IRRRunoffSub)
                 call SetMessage(LEVEL_ERROR, 'Error regarding the D-NAM model input for node with id: '//Id_Nod(inode)(1:len_trim(Id_Nod(inode))))
                 LevelError = .true.
                 write(Iout1,'(A)')   ' The specified Lower Zone Threshold for Percolation (LTP) '
                 write(Iout1,'(A)')   ' exceeds the maximum water depth of the lower zone storage LMax '
                 write(Iout1,'(A,F9.1,A)')  ' Specified LTP: ', NAM_LTG(IRRRunoffSub), ' mm'
                 write(Iout1,'(A,F7.2,A)')  ' From input derived LMAX: ', NAM_LMAX(IRRRunoffSub), ' mm'
                 err971 = .true.
              else
                 ! to prevent rounding off errors
                 NAM_LTG(IRRRunoffSub) = min (NAM_LMAX(IRRRUnoffSub), NAM_LTG(IRRRunoffSub))
              endif
              ! input check 9
              idum = D_IfReal (NAM_LMax(IRRRunoffSub), NAM_L(IRRRunoffSub), eps)
              if (idum .lt. 0) then
!                NAM_LMax(IRRRunoffSub) < NAM_L(IRRRunoffSub)
                 call SetMessage(LEVEL_ERROR, 'Error regarding the D-NAM model input for node with id: '//Id_Nod(inode)(1:len_trim(Id_Nod(inode))))
                 LevelError = .true.
                 write(Iout1,'(A)')   ' The specified Initial Lower zone contanc (L) exceeds the maximum content of the lower zone storage LMax '
                 write(Iout1,'(A,F9.1,A)')  '  Specified L: ', NAM_L(IRRRunoffSub), ' mm'
                 write(Iout1,'(A,F7.2,A)')  '  From input derived LMAX: ', NAM_LMAX(IRRRunoffSub), ' mm'
                 err971 = .true.
              else
                 ! to prevent rounding off errors
                 NAM_L(IRRRunoffSub) = min (NAM_LMAX(IRRRUnoffSub), NAM_L(IRRRunoffSub))
              endif
              ! input check 11
              idum = D_IfReal (NAM_LMax(IRRRunoffSub), NAM_L(IRRRunoffSub), 10D0*eps)
              if (idum .gt. 0 .and. NAM_GWSDRZ(IRRRunoffSub) .gt. 0) then
                 !  (NAM_L(IRRRunoffSub) .lt. NAM_LMax(IRRRunoffSub) .and. NAM_GWSDRZ(IRRRunoffSub) .gt. 0)
                 call SetMessage(LEVEL_ERROR, 'Inconsistent input for Initial contents of lower zone and groundwater storage for D-NAM node with id: '//Id_Nod(inode)(1:len_trim(Id_Nod(inode))))
                 LevelError = .true.
                 write(Iout1,'(A)')   ' The specified Lower Zone Content (L) is less than the Lower Zone maximum content (LMax)'
                 write(Iout1,'(A)')   ' while the groundwaterstorage in the rootzone is larger than zero '
                 write(Iout1,'(A,F13.6,A)')  '  Specified Lower zone content  (L): ', NAM_L(IRRRunoffSub), ' mm'
                 write(Iout1,'(A,F13.6,A)')  '  Maximum Lower zone content (LMAX): ', NAM_LMAX(IRRRunoffSub), ' mm'
                 write(Iout1,'(A,F13.6,A)')  '  Groundwater storage in the rootzone (NAM_GWSDRZ): ', NAM_GWSDRZ(IRRRunoffSub), ' mm'
                 write(Iout1,'(A)')         '  Adjust model input and/or regenerate restart file'
                 write(Iout1,'(A,F13.6,A)')  '  In case initial contents are read from restart file, it means other parameters impacting LMAX have been adjusted in between, so the restart file needs to be regenerated again'
                 err971 = .true.
              else
                 ! no need for adjustment
              endif
          endif
       Enddo
       if (err971) call ErrMsgStandard (971, 0, ' Error in D-NAM input', ' RRRunoffNode_Init1' )
   endif

   if (NcRRRunoffWagmod .gt. 0) then
     Do iRRRunoff=1,NCRRRunoff
        IRRRunoffSub = RRRunoff_SubIndex(IRRRunoff)
        if (FirstCall .and. RRRunoff_CompOption(IRRRunoff) .eq. 5)  then
           ! Conversion of data to data per timestep, only once!!
           WagMod_J(IRRRunoffSub) = WagMod_J(IRRRunoffSub) * 86400. / TimeSettings%TimestepSize        ! from days to timesteps
           WagMod_Seep(IRRRunoffSub) = WagMod_SEEP(IRRRunoffSub) * timeSettings%TimestepSize / 86400.  ! from mm/day to mm/timestep
           WagMod_QG0(iRRRunoffSub)  = WagMod_QG0(iRRRunoffSub) * TimeSettings%TimestepSize / 86400.   ! from mm/day to mm/timestep
!          WagMod_FOS(iRRRunoffSub)  = WagMod_FOS(iRRRunoffSub) * TimeSettings%TimestepSize / 86400.   ! from 1/day to 1/timestep
!          WagMod_E(IRRRunoffSub) = WagMod_E(IRRRunoffSub) * TimeSettings%TimestepSize / 86400.        ! from days**0.5 to timesteps**0.5
!          WagMod_F(IRRRunoffSub) = WagMod_F(IRRRunoffSub) * TimeSettings%TimestepSize / 86400.        ! from days**0.5 to timesteps**0.5
          ! initial storage soil moisture and gw (GStore from QQ0), and initial groundwater flow
           WagMod_SM  (iRRRunoffSub)     = WagMod_SM0(iRRRunoffSub)
           WagMod_SMT1(iRRRunoffSub)     = WagMod_SM0(iRRRunoffSub)
           WagMod_GStoreT1(iRRRunoffSub) = WagMod_QG0(iRRRunoffSub) * WagMod_J(iRRRunoffSub)              ! QG0 in mm/timestep, J in timesteps, Gstore in mm)
           WagMod_GStore  (iRRRunoffSub) = WagMod_GStoreT1(iRRRunoffSub)
           WagMod_QGT1(iRRRunoffSub)     = WagMod_QG0(iRRRunoffSub)
           WagMod_RoutVol(iRRRunoffSub)  = 0.0
           WagMod_RoutVol1(iRRRunoffSub)  = 0.0
           Do itmstp = 1, Wagmod_MaxNrTimestepsSimulation
              WagMod_QG(IRRRunoffSub,Itmstp) = WagMod_QG0(IRRRunoffSub) * EXP( -Itmstp / Wagmod_J(IRRRunoffSub) )
              If (WagMod_QG(IRRRunoffSub,Itmstp) .lt. RTol*RTol) goto 199
           Enddo
199        Continue
        elseif (RRRunoff_CompOption(IRRRunoff) .eq. 5) then
           ! adjust if info from restart file available
           WagMod_SMT1(iRRRunoffSub)     = WagMod_SM0(iRRRunoffSub)
           WagMod_SM  (iRRRunoffSub)     = WagMod_SM0(iRRRunoffSub)
           ! WagMod_GStoreT1(iRRRunoffSub) was read from restart file, so no recomputation based on QG0
           WagMod_GStore  (iRRRunoffSub) = WagMod_GStoreT1(iRRRunoffSub)
           ! QGT1 from restart info
           ! QG from restart info, not based on QG0
           Do itmstp = 1, Wagmod_MaxNrTimestepsSimulation
              Wagmod_RoutVol(iRRRunoffSub) = Wagmod_RoutVol(iRRRunoffSub) + Wagmod_QD(IRRRunoffSub,Itmstp)
              Wagmod_RoutVol1(iRRRunoffSub) = Wagmod_RoutVol1(iRRRunoffSub) + Wagmod_QD(IRRRunoffSub,Itmstp)
           Enddo
        else
          ! do nothing
        endif

        if (Idebug .gt. 0 .and. RRRunoff_CompOption(IRRRunoff) .eq. 5) then
           Write(Idebug, *) 'IRRRunoffSub, Wagmod', IRRRunoff
           Write(Idebug, *) ' SM     SMT1   ', WagMod_SM(IRRRunoffSub), WagMod_SMT1(IRRRunoffSub)
           Write(Idebug, *) ' GStore Gstore1', WagMod_GStore(IRRRunoffSub), WagMod_GStoreT1(IRRRunoffSub)
           Write(Idebug, *) ' QGT1', WagMod_QGT1(IRRRunoffSub)
           Write(Idebug, *) ' QG', (WagMod_QG(IRRRunoffSub,j),j=1,11)
        endif

        ! remainder part only if FirstCall
        if (FirstCall .and. RRRunoff_CompOption(IRRRunoff) .eq. 5) then
            ! compute unit hydrograph components of J model and CD model
            Call WagMod_JMod (iRRRunoffSub)

            Call WagMod_CDMod(iRRRunoffSub)

            if (Wagmod_IUHQuick(iRRRunoffSub) .gt. WagMod_NUCDMX) then
               Write(iout1,*) ' Wagmod Iuhquick in timesteps:', Wagmod_IuhQuick(IRRRunoffSub)
               Write(iout1,*) ' Wagmod Isubquick in timesteps:', Wagmod_IsubQuick(IRRRunoffSub)
               call ErrMsgStandard (981, 0, ' IUHQuick in WagmodTimesteps is too large (>2500) ', 'RRRunoffNode_Wagmod_CDMod')
            endif

            ! Scale CD unit hydrograph by SumUCD
            SUMUCD = 0.0
            Do IU = 1,Wagmod_IUHQuick(iRRRunoffSub)
                SUMUCD = SUMUCD + Wagmod_UCD(IRRRunoffSub,IU)
            Enddo
            If (SUMUCD .NE. 0.0) then
              Do IU = 1,Wagmod_IUHQuick(iRRRunoffSub)
                 WagMod_UCD(IRRRunoffSub,IU) = WagMod_UCD(IRRRunoffSub,IU) / SUMUCD
              Enddo
            Endif

            ! Check sums
            SUMUCD = 0.0
            Do IU = 1,Wagmod_IUHQuick(iRRRunoffSub)
                SUMUCD = SUMUCD + Wagmod_UCD(IRRRunoffSub,IU)
            Enddo
            SUMUJ = 0.0
            Do IU = 1,Wagmod_IUHSlow(iRRRunoffSub)
                SUMUJ = SUMUJ + Wagmod_UJ(IRRRunoffSub,IU)
            Enddo

            ! 'UnitHY' output file .00U
            if (GenerateOldWagmodOutputFiles) then
               Call Openfl (IoutWagMod,WagMod_UnitHY(iRRRunoffSub),1,2)
               if (idebug .ne. 0) write(Idebug,*) 'IoutWagmod UnitHY file ', IOutWagmod
               Write(IoutWagMod,'(/5X,"DISCRETE VALUES OF UNIT HYDROGRAPHS:",/, &
                        5X,"(Na tabellen in wetenschappelijke notatie;",/,&
                        5X," zoek naar tekst ""E-formaat"")")')
               Write(IoutWagMod,200) WagMod_E(IRRRunoffSub),WagMod_F(IRRRunoffSub)
200            Format(/5X,'DIRECT RUNOFF',6X,'E=',F10.5,4X,'F=',F10.5,"(in values per timestep)"/)
               Write(IoutWagMod,'(5(F10.4,2X))') (WagMod_UCD(IRRRunoffSub,IU),IU=1,WagMod_IUHQuick(IRRRunoffSub))
               Write(IoutWagMod,'(//5X,"BASE FLOW",6X,"J=",F13.7,"(in timesteps)",/)') WagMod_J(IRRRunoffSub)
               Write(IoutWagMod,'(5(F10.4,2X))') (WagMod_UJ(IRRRunoffSub,IU),IU=1,WagMod_IUHSlow(IRRRunoffSub))
               ! schrijf nogmaals; nu in E-formaat
               Write(IoutWagMod,'(//"E-formaat DIRECT RUNOFF")')
               Write(IoutWagMod,'(1E20.10)') (WagMod_UCD(IRRRunoffSub,IU),IU=1,WagMod_IUHQuick(IRRRunoffSub))
               Write(IoutWagMod,'(//"E-formaat BASE FLOW")')
               Write(IoutWagMod,'(1E20.10)') (WagMod_UJ(IRRRunoffSub,IU),IU=1,WagMod_IUHSlow(IRRRunoffSub))
               Close(IoutWagMod)

            ! 'Optimaal' output file .00O
               Call Openfl (IoutWagMod,WagMod_Optimaal(iRRRunoffSub),1,2)
               if (idebug .ne. 0) write(Idebug,*) 'IoutWagmod Opt outputfile', IOutWagmod
               If (WagMod_ActEvapCompOption(iRRRunoffSub) .eq. 1) THEN
                  Write(IoutWagMod,'(A)') ' DE ACTUELE VERDAMPING WORDT BEREKEND'
               Else
                  Write(IoutWagMod,'(A)') ' DE ACTUELE VERDAMPING WORDT NIET BEREKEND'
               Endif
               Write(IoutWagMod,'("nieuw J-model algorithme",/," (zonder originele KIS van 1 of 3)")')      ! KISCODE 4
               Write(IoutWagMod,'("aantal termen bij UJ berekening",I10)') WagMod_NTERMUJ(IRRRunoffSub)        ! KIS 0
               Write(IoutWagMod,'(/,A)') ' RECONSTRUCTION'
               Write(IoutWagMod,70)
70             Format(/2X,'IINP',5X,'IENDU',1X,'IENDUC', 1X,'NSTEP')
               Write(IoutWagMod,'(4I6)') Wagmod_ActNrTimestepsSimulation, WagMod_IUHSlow(iRRRunoffSub),WagMod_IUHQuick(iRRRunoffSub), WagMod_ISubQuick(iRRRunoffSub)
               Write(IoutWagMod,*)
               Write(IoutWagMod,*)

            ! write output
               Write(IoutWagMod,320) SUMUCD, SumUJ
               Write(IoutWagMod,325) WagMod_J(iRRRunoffSub), WagMod_E(iRRRunoffSub), WagMod_F(iRRRunoffSub), &
                                     Wagmod_CR(iRRRunoffSub), Wagmod_REPA(iRRRunoffSub), WagMod_FOS(IRRRunoffSub)
320            Format(/,1X,'SUMUCD=',F14.4,3X,'SUMUJ=',F14.4)
325            Format(1X,'X(1-6) ',6(F11.6,1X))
               Close(IoutWagMod)

            ! 'Plotfile ' output file .00P
               Call Openfl (WagMod_PlotFileUnit(IRRRunoffSub), WagMod_Plotfile(iRRRunoffSub),1,2)
               if (idebug .ne. 0) write(Idebug,*) 'IoutWagmod Plotfile', IOutWagmod
               IoutWagMod = Wagmod_PlotfileUnit(IRRRunoffSub)
               if (idebug .ne. 0) write(Idebug,*) 'saved IoutWagmod Plotfile', WagMod_PlotfileUnit(IRRRunoffSub)
               If (Wagmod_ActEvapCompOption(IRRRunoffSub) .eq. 1) then
                  if (GenerateOldWagmodOutputFiles) write (IOutWagMod,227)
227               format ('    -P-   -ETG-','    -QM=0-      -I-       -QC-     ',&
                          '-ETA-      -QG-     -PEFJ-       -QD-    -PEFCD-',      &
                          '      -DIV-    -GSTORE-       -SM-      -CAP-')
               Else
                  if (GenerateOldWagmodOutputFiles) write(IoutWagMod,225)
225               format('    -P-   -ETG-    -QM=0-      -I-       -QC-      ', &
                         '-PEF-       -QG-     -PEFJ-       -QD-    -PEFCD-',   &
                         '      -DIV-    -GSTORE-       -SM-      -CAP-')
               endif
            endif
        endif
     Enddo
   endif

   if (NcRRRunoffWalrus .gt. 0) then
    TimestepSize = dble(TimeSettings%TimestepSize)
    if (NrSecsRai .lt. 3599) then
       call ErrMsgStandard (977, 0, ' Warning: WUR designed Walrus for hourly input data, and Walrus has not been validated for a smaller input&computation timestep', ' ')
    endif

    DateEventStart = EventStartDateTime(Ievent,1)*10000 + EventStartDateTime(Ievent,2)*100 + EventStartDateTime(Ievent,3)
    TimeEventStart = EventStartDateTime(Ievent,4)*10000 + EventStartDateTime(Ievent,5)*100 + EventStartDateTime(Ievent,6)
    JulianEventStart= Julian (DateEventStart, TimeEventStart )
    DateEventStart = 19700101.D0
    Julian01011970 = Julian (DateEventStart, 0)
    WalrusStartTime =  (JulianEventStart - Julian01011970 ) * 86400.D0   ! seconds since 01-01-1970
    if (FirstCall .and. iEvent .eq. 1) then
       if (idebug .ne. 0) write(Idebug, *) ' NcRRRunoffWalrus', NCRRRunoffWalrus
!        Write(*,*) ' Walrus MaxTimesteps', MaxTimesteps
        Success = Dh_AllocInit (NcRRRunoffWalrus,MaxTimesteps, Walrus_forcingtime,  0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,MaxTimesteps, Walrus_Precipitation,  0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,MaxTimesteps, Walrus_Epot,  0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,MaxTimesteps, Walrus_FXGValues ,  0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,MaxTimesteps, Walrus_FXSValues ,  0D0)
        If (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', ' RRRunoffNode_Init1' )
        ! create Walrus instances
        retValWalrusCall = addWalrusInstances (NcRRRunoffWalrus)
!        Write(*,*) ' AddWalrusInstances', NcRRRUnoffWalrus, 'retval', RetValWalrusCall
        if (retValWalrusCall .ne. 0) then
            call ErrMsgStandard (981, 0, ' Some error during WALRUS routine AddWalrusInstances ', ' RRRunoffNode_Init1' )
        endif
    elseif (FirstCall .and. iEvent .gt. 1) then
        DeAllocate(Walrus_forcingtime)
        DeAllocate(Walrus_Precipitation)
        DeAllocate(Walrus_Epot)
        DeAllocate(Walrus_FXGValues)
        DeAllocate(Walrus_FXSValues)
        Success = Dh_AllocInit (NcRRRunoffWalrus,MaxTimesteps, Walrus_forcingtime,  0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,MaxTimesteps, Walrus_Precipitation,  0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,MaxTimesteps, Walrus_Epot,  0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,MaxTimesteps, Walrus_FXGValues ,  0D0)
        Success = Success .and. Dh_AllocInit (NcRRRunoffWalrus,MaxTimesteps, Walrus_FXSValues ,  0D0)
        If (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', ' RRRunoffNode_Init1' )
    endif

     Do iRRRunoff=1,NCRRRunoff
        IRRRunoffSub = RRRunoff_SubIndex(IRRRunoff)
        Do j=1,ncnode
           if (Einode(j,2) .eq. IRRRunoff .and. Einode(j,3) .eq. 23) Inode = j        ! kind 23= Wagmod/Walrus
        Enddo
        if (RRRunoff_CompOption(IRRRunoff) .eq. 6) then
           ! Tnul output
           Walrus_HSCurrent(IrrRunoffSub) = Walrus_HS0(IRRRunoffSub)
           Walrus_HQCurrent(IrrRunoffSub) = Walrus_HQ0(IRRRunoffSub)
           Walrus_DGCurrent(IrrRunoffSub) = Walrus_DG0(IRRRunoffSub)
           Walrus_DVCurrent(IrrRunoffSub) = Walrus_DV0(IRRRunoffSub)
!           write(*,*) ' DV0 value to RRRUnoffTnul ', Walrus_DV0(IRRRunoffSub)
!           write(*,*) ' DG0 value to RRRUnoffTnul ', Walrus_DG0(IRRRunoffSub)
!           write(*,*) ' HQ0 value to RRRUnoffTnul ', Walrus_HQ0(IRRRunoffSub)
!           write(*,*) ' HS0 value to RRRUnoffTnul ', Walrus_HS0(IRRRunoffSub)
           RSLMAP19_RRRunoff(NStartWalrus+ 3,IrrRunoff,1) = Walrus_HS0(IRRRunoffSub)
           RSLMAP19_RRRunoff(NStartWalrus+ 2,IrrRunoff,1) = Walrus_HQ0(IRRRunoffSub)
           RSLMAP19_RRRunoff(NStartWalrus+ 1,IrrRunoff,1) = Walrus_DG0(IRRRunoffSub)
           RSLMAP19_RRRunoff(NStartWalrus   ,IrrRunoff,1) = Walrus_DV0(IRRRunoffSub)
           RSLMAP19_RRRunoff(4              ,IrrRunoff,1) = Walrus_Q0(IRRRunoffSub)
           RSLMAP19_RRRunoff(5              ,IrrRunoff,1) = Walrus_Q0(IRRRunoffSub) / timeSettings%TimestepSize * Area_RRRunoffNode(IRRRunoff) * mm2m
           RSLMAP19_RRRunoff(NStartWalrus+11,IrrRunoff,1) = Walrus_dVEQCurrent(IRRRunoffSub)
           RSLMAP19_RRRunoff(NStartWalrus+12,IrrRunoff,1) = Walrus_lastFQS(IRRRunoffSub)
           RSLMAP19_RRRunoff(NStartWalrus+13,IrrRunoff,1) = Walrus_lastFGS(IRRRunoffSub)
           RSLMAP19_RRRunoff(NStartWalrus+14,IrrRunoff,1) = Walrus_Q0     (IRRRunoffSub)
           RSLMAP19_RRRunoff(NStartWalrus+15,IrrRunoff,1) = Walrus_WICurrent(IRRRunoffSub)
           RSLMAP19_RRRunoff(NStartWalrus+16,IrrRunoff,1) = Walrus_BetaCurrent(IRRRunoffSub)
           RRRunoff_Tnul(NStartWalrus+ 3,irrRunoff) = RSLMAP19_RRRunoff(NStartWalrus+ 3,IRRRunoff,1)
           RRRunoff_Tnul(NStartWalrus+ 2,irrRunoff) = RSLMAP19_RRRunoff(NStartWalrus+ 2,IRRRunoff,1)
           RRRunoff_Tnul(NStartWalrus+ 1,irrRunoff) = RSLMAP19_RRRunoff(NStartWalrus+ 1,IRRRunoff,1)
           RRRunoff_Tnul(NStartWalrus   ,irrRunoff) = RSLMAP19_RRRunoff(NStartWalrus   ,IRRRunoff,1)
           RRRunoff_Tnul(4              ,irrRunoff) = RSLMAP19_RRRunoff(4              ,IRRRunoff,1)
           RRRunoff_Tnul(5              ,irrRunoff) = RSLMAP19_RRRunoff(5              ,IRRRunoff,1)
           RRRunoff_Tnul(NStartWalrus+11,irrRunoff) = RSLMAP19_RRRunoff(NStartWalrus+11,IRRRunoff,1)
           RRRunoff_Tnul(NStartWalrus+12,irrRunoff) = RSLMAP19_RRRunoff(NStartWalrus+12,IRRRunoff,1)
           RRRunoff_Tnul(NStartWalrus+13,irrRunoff) = RSLMAP19_RRRunoff(NStartWalrus+13,IRRRunoff,1)
           RRRunoff_Tnul(NStartWalrus+14,irrRunoff) = RSLMAP19_RRRunoff(NStartWalrus+14,IRRRunoff,1)
           RRRunoff_Tnul(NStartWalrus+15,irrRunoff) = RSLMAP19_RRRunoff(NStartWalrus+15,IRRRunoff,1)
           RRRunoff_Tnul(NStartWalrus+16,irrRunoff) = RSLMAP19_RRRunoff(NStartWalrus+16,IRRRunoff,1)
        endif
        if (idebug .ne. 0) write(Idebug, *) ' Initialize Walrus catchment', IRRRunoffSub
        if (FirstCall .and. RRRunoff_CompOption(IRRRunoff) .eq. 6)  then
           ! fill Walrus_forcingtime, Precipitation, Epot, FXS and FXG with data from bui file
           ! Walrus time in seconds since 01-01-1970, Precipitation, Epot, FXS and FXG in mm/hour (requires conversion!)
           RLASTTM = Float(EVDURA(1))*NRSDAY + Float(EVDURA(2))*NRSHR+Float(EVDURA(3))*NRSMIN + Float(EVDURA(4))
           LASTTM  = RLASTTM / NrSecsRai
           StrtTM  = 0
           NrsEvp  = Confarr_get_NrsEvap()
           nrsteps = NrsEvp / NrSecsRai
           StrtTM  = ( EvStrt(4)*NRSHR+EvStrt(5)*NrsMin+EvStrt(6) ) / NrSecsRai
           itmevap = 1
           if (abs(TimeSettings%TimestepSize - NrSecsRai) .le. 0.01) then
               ! timestep size computations and rainfall data same
               Do i=1,LastTM
                  ! Conversion of data from mm/rainfall timestep to mm/hour
                  ! Comput Walrus time in seconds since 01-01-1970
                  ! check al of niet 1 tijdstap schuiven!, i*(NrSecRai of (i-1)*NrSecsRai
                  Walrus_forcingtime  (IRRRunoffSub,i) = WalrusStartTime + dble ((i-1)* NrSecsRai)
                  Walrus_Precipitation(IRRRunoffSub,i) = Buidata (Ievent, NodMet (inode), i) * AAFNodeRainfall (inode) !* 3600.D0 / NrSecsRai
                  if (NodEvap(inode) .gt. 0) then
                      Walrus_Epot         (IRRRunoffSub,i) = Buidata (Ievent, NodEvap(inode), i) !* 3600.D0 / NrSecsRai  ! verdampingsstation uit bui file
                  else
                     ! from evap file, it station NodMet(inode) is existing in evapfile
                     if (NrEvapStations .ge. NodMet(inode)) then
                         if (i .eq. 1) call ErrMsgStandard (977, 0, ' Warning: Walrus Evaporation station taken from EVAP file using station: ', NamMet(inode))
                         if (  ((StrtTm + i*1.D0)/(nrsteps*1.D0)) .ge. 1.D0*itmevap) then
                             itmevap = itmevap + 1
                         endif
!                         if (abs(TimeSettings%TimestepSize - NrsEvp) .le. 0.01) then   ok, else niet ok
                         Walrus_Epot (IRRRunoffSub,i) = Evapdata (Ievent, NodMet(inode), itmevap) * NrSecsRai * 1.D0 / Float(NrsEvp)  ! * 3600.D0 / NrsEvp) ! verdampingsstation ook evap file (EVP file daily NrsEvp=86400, NetCdf maybe different)
                     else
                         if (i .eq. 1) then
                            call ErrMsgStandard (977, 0, ' For node ', ID_Nod(inode))
                            call ErrMsgStandard (977, 0, ' Walrus Evaporation station not defined in BUI file and series not present in EVAP file', ' Use the first station from the EVAP file')
                         endif
                         if (  ((StrtTm + i*1.D0)/(nrsteps*1.D0)) .ge. 1.D0*itmevap) then
                             itmevap = itmevap + 1
                         endif
!                         if (abs(TimeSettings%TimestepSize - NrsEvp) .le. 0.01) then   ok, else niet ok
                         Walrus_Epot (IRRRunoffSub,i) = Evapdata (Ievent, 1, itmevap) * NrSecsRai * 1.D0 / Float(NrsEvp)  ! * 3600.D0 / NrsEvp) ! verdampingsstation ook evap file (EVP file daily NrsEvp=86400, NetCdf maybe different)
                     endif
                  endif
                  Walrus_FXSValues    (IRRRunoffSub,i) = 0.0
                  Walrus_FXGValues    (IRRRunoffSub,i) = 0.0
                  if (NodFXS(inode) .gt. 0)  Walrus_FXSValues (IRRRunoffSub,i) = Buidata (Ievent, NodFXS (inode), i) !* 3600.D0 / NrSecsRai
                  if (NodFXG(inode) .gt. 0)  Walrus_FXGValues (IRRRunoffSub,i) = Buidata (Ievent, NodFXG (inode), i) !* 3600.D0 / NrSecsRai
               enddo
           else
               write(iout1,*) ' Remark: computation timestep and rainfall timestep WALRUS different !'
               Do i=1,LastTM
                   ! Conversion of data from mm/rainfall timestep to mm/hour
                   ! Comput Walrus time in seconds since 01-01-1970
                   ! check al of niet 1 tijdstap schuiven!, i*(NrSecRai of (i-1)*NrSecsRai
                   Walrus_forcingtime  (IRRRunoffSub,i) = WalrusStartTime + dble ((i-1)* NrSecsRai)
                   Walrus_Precipitation(IRRRunoffSub,i) = Buidata (Ievent, NodMet (inode), i) * AAFNodeRainfall (inode) !* 3600.D0 / NrSecsRai
                   if (NodEvap(inode) .gt. 0) then
                      Walrus_Epot         (IRRRunoffSub,i) = Buidata (Ievent, NodEvap(inode), i) !* 3600.D0 / NrSecsRai
                   else
                     ! from evap file, it station NodMet(inode) is existing in evapfile
                      if (NrEvapStations .ge. NodMet(inode)) then
                         if (i .eq. 1) call ErrMsgStandard (977, 0, ' Warning: Walrus Evaporation station taken from EVAP file using station', NamMet(inode))
                         if (  ((StrtTm + i*1.D0)/(nrsteps*1.D0)) .ge. 1.D0*itmevap) then
                            itmevap = itmevap + 1
                         endif
                         Walrus_Epot (IRRRunoffSub,i) = Evapdata (Ievent, NodMet(inode), itmevap) * NrSecsRai *1.D0 / Float(NrsEvp) !* 3600.D0 / NrsEvp ! verdampingsstation ook evap file
                      else
                         if (i .eq. 1) then
                            call ErrMsgStandard (977, 0, ' For node ', ID_Nod(inode))
                            call ErrMsgStandard (977, 0, ' Walrus Evaporation station not defined in BUI file and series not present in EVAP file', ' Use the first station from the EVAP file')
                         endif
                         if (  ((StrtTm + i*1.D0)/(nrsteps*1.D0)) .ge. 1.D0*itmevap) then
                             itmevap = itmevap + 1
                         endif
!                         if (abs(TimeSettings%TimestepSize - NrsEvp) .le. 0.01) then   ok, else niet ok
                         Walrus_Epot (IRRRunoffSub,i) = Evapdata (Ievent, 1, itmevap) * NrSecsRai * 1.D0 / Float(NrsEvp)  ! * 3600.D0 / NrsEvp) ! verdampingsstation ook evap file (EVP file daily NrsEvp=86400, NetCdf maybe different)
                      endif
                   endif
                   Walrus_FXSValues    (IRRRunoffSub,i) = 0.0
                   Walrus_FXGValues    (IRRRunoffSub,i) = 0.0
                   if (NodFXS(inode) .gt. 0) Walrus_FXSValues (IRRRunoffSub,i) = Buidata (Ievent, NodFXS (inode), i) !* 3600.D0 / NrSecsRai
                   if (NodFXG(inode) .gt. 0) Walrus_FXGValues (IRRRunoffSub,i) = Buidata (Ievent, NodFXG (inode), i) !* 3600.D0 / NrSecsRai
                 enddo
           endif
           ! pass rainfall, Epot, Fxs, Fxg
           ! Walrus.set_seq_C (50, Walrus_forcingtime, Walrus_Precipitation)
           Success = Success .and. Dh_AllocInit (LastTm, Walrus_Temp1, 0.D00)
           Success = Success .and. Dh_AllocInit (LastTm, Walrus_Temp2,  0.D00)
!          Write(*,*) ' WalrusSetSeqC Precipitation', IRRRunoffSub
           Do i=1,LastTM
              Walrus_Temp1(i) = Walrus_ForcingTime(IRRRunoffSub,i)
              Walrus_Temp2(i) = Walrus_Precipitation(IRRRunoffSub,i)
!             write(*,*) ' Forcing time and value ', Walrus_Temp1(i), Walrus_Temp2(i)
           Enddo
           retValWalrusCall = WalrusSetSeqC(IRRRunoffSub, wc_fc_P, Walrus_Temp1, LastTm, Walrus_Temp2, LastTm, Timestepsize, WalrusFirst)
!          Write(*,*) ' WalrusSetSeqC', IRRRUnoffSub, 'retval', RetValWalrusCall
           if (retValWalrusCall .ne. 0) then
               call ErrMsgStandard (981, 0, ' Some error during WALRUS routine SetSeqC Precipitation', ' RRRunoffNode_Init1')
           endif
           ! Walrus.set_seq_C (51, Walrus_forcingtime, Walrus_Epot)
!          Write(*,*) ' WalrusSetSeqC Epot'
           Do i=1,LastTM
              Walrus_Temp1(i) = Walrus_ForcingTime(IRRRunoffSub,i)
              Walrus_Temp2(i) = Walrus_Epot(IRRRunoffSub,i)
!             write(*,*) ' Forcing time and value ', Walrus_Temp1(i), Walrus_Temp2(i)
           Enddo
           retValWalrusCall = WalrusSetSeqC(IRRRunoffSub, wc_fc_ETpot, Walrus_Temp1, LastTm, Walrus_Temp2, LastTm, Timestepsize, WalrusFirst)
!          Write(*,*) ' WalrusSetSeqC', IRRRUnoffSub, 'retval', RetValWalrusCall
           if (retValWalrusCall .ne. 0) then
               call ErrMsgStandard (981, 0, ' Some error during WALRUS routine SetSeqC Epot ', ' RRRunoffNode_Init1' )
           endif
           ! Walrus.set_seq_C (52, Walrus_forcingtime, walrus_Fxs)
!          Write(*,*) ' WalrusSetSeqC Fxs'
           Do i=1,LastTM
              Walrus_Temp1(i) = Walrus_ForcingTime(IRRRunoffSub,i)
              Walrus_Temp2(i) = Walrus_FxsValues(IRRRunoffSub,i)
!             write(*,*) ' Forcing time and value ', Walrus_Temp1(i), Walrus_Temp2(i)
           Enddo
           retValWalrusCall = WalrusSetSeqC(IRRRunoffSub, wc_fc_XS, Walrus_Temp1, LastTm, Walrus_Temp2, LastTm, Timestepsize, WalrusFirst)
!          Write(*,*) ' WalrusSetSeqC', IRRRUnoffSub, 'retval', RetValWalrusCall
           if (retValWalrusCall .ne. 0) then
               call ErrMsgStandard (981, 0, ' Some error during WALRUS routine SetSeqC Fxs ', ' RRRunoffNode_Init1' )
           endif
           ! Walrus.set_seq_C (53, Walrus_forcingtime, Walrus_Fxg)
!          Write(*,*) ' WalrusSetSeqC Fxg'
           Do i=1,LastTM
              Walrus_Temp1(i) = Walrus_ForcingTime(IRRRunoffSub,i)
              Walrus_Temp2(i) = Walrus_FxgValues(IRRRunoffSub,i)
!             write(*,*) ' Forcing time and value ', Walrus_Temp1(i), Walrus_Temp2(i)
           Enddo
           retValWalrusCall = WalrusSetSeqC(IRRRunoffSub, wc_fc_XG, Walrus_Temp1, LastTm, Walrus_Temp2, LastTm, Timestepsize, WalrusFirst)
!          Write(*,*) ' WalrusSetSeqC', IRRRUnoffSub, 'retval', RetValWalrusCall
           if (retValWalrusCall .ne. 0) then
               call ErrMsgStandard (981, 0, ' Some error during WALRUS routine SetSeqC Fxg ', ' RRRunoffNode_Init1' )
           endif
           Deallocate(Walrus_Temp1)
           Deallocate(Walrus_Temp2)
           ! pass initial parameters WALRUS
           ! Walrus.set    (1  ,Walrus_CW(iRRRunoffSub)    )      ! par_cW
           ! Walrus.set    (2  ,Walrus_CV(iRRRunoffSub)    )      ! par_cV
           ! Walrus.set    (3  ,Walrus_CG(iRRRunoffSub)    )      ! par_cG
           ! Walrus.set    (4  ,Walrus_CQ(iRRRunoffSub)    )      ! par_cQ
           ! Walrus.set    (5  ,Walrus_CS(iRRRunoffSub)    )      ! par_cS
           ! Walrus.set    (6  ,Walrus_CD(iRRRunoffSub)    )      ! par_cD
           ! Walrus.set    (12 ,Walrus_XS(iRRRunoffSub)    )      ! par_cexpS
           ! Walrus.set    (?  ,Walrus_HSMIN(iRRRunoffSub) )      !
           ! Walrus.set    (10 ,Walrus_AS(iRRRunoffSub)    )      ! par_aS
           ! Walrus.set    (41-45, constant values    )    )      ! numerical parameters
           ! Walrus.set_st (Walrus_SoilType(iRRRunoffSub)  )      ! soil type
           ! Walrus parameters
!          Write(*,*) ' WalrusSet AREA'
           dbleArea = dble (Area_RRRunoffNode(IRRRunoff)) / 1000000.D0
           retValWalrusCall = WalrusSet(IRRRunoffSub, wc_par_area, dbleArea,WalrusFirst)
           if (retValWalrusCall .ne. 0) then
               call ErrMsgStandard (981, 0, ' Some error during WALRUS routine Set CW ', ' RRRunoffNode_Init1' )
           endif
! default numerical parameters
! check if consistent SOBEK3-1379
           if (Walrus_min_deltime * Walrus_max_substeps .lt.  TimeSettings%TimestepSize) then
               Write(Iout1,*) ' Computation time step in seconds: ', TimeSettings%TimestepSize
               Write(Iout1,*) ' Walrus_min_deltime (minimum Walrus internal timestep size) in seconds: ', Walrus_min_deltime
               Write(Iout1,*) ' Walrus_max_substeps (maximum Walrus internal nr. timesteps): ', Walrus_max_substeps
               call ErrMsgStandard (981, 0, ' Specified computation time step is larger than minimum Walrus time step times the max. number of Walrus substeps; Reduce the computation time step or increase the Walrus parameters in Delft_3b.Ini. ', &
                                            ' RRRunoffNode_Init1' )
           endif
!          Write(*,*) ' WalrusSet min_deltime'
           ! defalt 60.D0
           retValWalrusCall = WalrusSet(IRRRunoffSub, wc_par_min_deltime, Walrus_min_deltime,WalrusFirst)
           if (retValWalrusCall .ne. 0) then
               call ErrMsgStandard (981, 0, ' Some error during WALRUS routine Set min_deltime', ' RRRunoffNode_Init1' )
           endif
!          Write(*,*) ' WalrusSet max_hchange'
           ! default 10.D0
           retValWalrusCall = WalrusSet(IRRRunoffSub, wc_par_max_h_change, Walrus_maxhchange,WalrusFirst)
           if (retValWalrusCall .ne. 0) then
               call ErrMsgStandard (981, 0, ' Some error during WALRUS routine Set max_hchange ', ' RRRunoffNode_Init1' )
           endif
!          Write(*,*) ' WalrusSet minh'
           ! default 1.0D-3
           retValWalrusCall = WalrusSet(IRRRunoffSub, wc_par_min_h, Walrus_minh,WalrusFirst)
           if (retValWalrusCall .ne. 0) then
               call ErrMsgStandard (981, 0, ' Some error during WALRUS routine Set minh ', ' RRRunoffNode_Init1' )
           endif
!          Write(*,*) ' WalrusSet max_Pstep'
           ! default 10.D0
           retValWalrusCall = WalrusSet(IRRRunoffSub, wc_par_max_Pstep, Walrus_max_Pstep,WalrusFirst)
           if (retValWalrusCall .ne. 0) then
               call ErrMsgStandard (981, 0, ' Some error during WALRUS routine Set max_Pstep ', ' RRRunoffNode_Init1' )
           endif
!          Write(*,*) ' WalrusSet max_substeps'
           ! default 288.D0
           retValWalrusCall = WalrusSet(IRRRunoffSub, wc_par_max_substeps, Walrus_max_substeps,WalrusFirst)
           if (retValWalrusCall .ne. 0) then
               call ErrMsgStandard (981, 0, ' Some error during WALRUS routine Set max_substeps ', ' RRRunoffNode_Init1' )
           endif
! user defined parameters
!          Write(*,*) ' WalrusSet CW'
           retValWalrusCall = WalrusSet(IRRRunoffSub, wc_par_cW, Walrus_CW(IRRRunoffSub),WalrusFirst)
           if (retValWalrusCall .ne. 0) then
               call ErrMsgStandard (981, 0, ' Some error during WALRUS routine Set CW ', ' RRRunoffNode_Init1' )
           endif
!          Write(*,*) ' WalrusSet CV'
           retValWalrusCall = WalrusSet(IRRRunoffSub, wc_par_cV, Walrus_CV(IRRRunoffSub),WalrusFirst)
           if (retValWalrusCall .ne. 0) then
               call ErrMsgStandard (981, 0, ' Some error during WALRUS routine Set CV ', ' RRRunoffNode_Init1' )
           endif
!          Write(*,*) ' WalrusSet CG'
           retValWalrusCall = WalrusSet(IRRRunoffSub, wc_par_cG, Walrus_CG(IRRRunoffSub),WalrusFirst)
           if (retValWalrusCall .ne. 0) then
               call ErrMsgStandard (981, 0, ' Some error during WALRUS routine Set CG ', ' RRRunoffNode_Init1' )
           endif
!          Write(*,*) ' WalrusSet CQ'
           retValWalrusCall = WalrusSet(IRRRunoffSub, wc_par_cQ, Walrus_CQ(IRRRunoffSub),WalrusFirst)
           if (retValWalrusCall .ne. 0) then
               call ErrMsgStandard (981, 0, ' Some error during WALRUS routine Set CQ ', ' RRRunoffNode_Init1' )
           endif
!          Write(*,*) ' WalrusSet CS'
           retValWalrusCall = WalrusSet(IRRRunoffSub, wc_par_cS, Walrus_CS(IRRRunoffSub),WalrusFirst)
           if (retValWalrusCall .ne. 0) then
               call ErrMsgStandard (981, 0, ' Some error during WALRUS routine Set CS ', ' RRRunoffNode_Init1' )
           endif
!          Write(*,*) ' WalrusSet CD'
           retValWalrusCall = WalrusSet(IRRRunoffSub, wc_par_cD, Walrus_CD(IRRRunoffSub),WalrusFirst)
           if (retValWalrusCall .ne. 0) then
               call ErrMsgStandard (981, 0, ' Some error during WALRUS routine Set CD ', ' RRRunoffNode_Init1' )
           endif
!          Write(*,*) ' WalrusSet XS'
           retValWalrusCall = WalrusSet(IRRRunoffSub, wc_par_cexpS, Walrus_XS(IRRRunoffSub),WalrusFirst)
           if (retValWalrusCall .ne. 0) then
               call ErrMsgStandard (981, 0, ' Some error during WALRUS routine Set XS ', ' RRRunoffNode_Init1' )
           endif
! added hSmin
!          Write(*,*) ' WalrusSet HSMin'
           retValWalrusCall = WalrusSet(IRRRunoffSub, wc_hSmin, Walrus_HSMIN(IRRRunoffSub),WalrusFirst)
           if (retValWalrusCall .ne. 0) then
              call ErrMsgStandard (981, 0, ' Some error during WALRUS routine Set HSMin ', ' RRRunoffNode_Init1' )
           endif
!          Write(*,*) ' WalrusSet AS'
           retValWalrusCall = WalrusSet(IRRRunoffSub, wc_par_aS, Walrus_AS(IRRRunoffSub),WalrusFirst)
           if (retValWalrusCall .ne. 0) then
               call ErrMsgStandard (981, 0, ' Some error during WALRUS routine Set AS ', ' RRRunoffNode_Init1' )
           endif
!          Write(*,*) ' WalrusSetST'
           retValWalrusCall = WalrusSetST (IRRRunoffSub, Walrus_IntSoilType(IRRRunoffSub),WalrusFirst)
           if (retValWalrusCall .ne. 0) then
               call ErrMsgStandard (981, 0, ' Some error during WALRUS routine SetST ', ' RRRunoffNode_Init1' )
           endif
           if (Walrus_IntSoilType(IRRRUnoffSub) .eq. 34) then
              ! for custom soil type, set b, Psi_ae and theta_s
!             Write(*,*) ' WalrusSet psi_ae'
              retValWalrusCall = WalrusSet (IRRRunoffSub, wc_par_psi_ae, Walrus_Psi_ae(IRRRunoffSub),WalrusFirst)
              if (retValWalrusCall .ne. 0) then
                  call ErrMsgStandard (981, 0, ' Some error during WALRUS routine Set psi_ae ', ' RRRunoffNode_Init1' )
              endif
!             Write(*,*) ' WalrusSet b'
              retValWalrusCall = WalrusSet (IRRRunoffSub, wc_par_b, Walrus_B(IRRRunoffSub),WalrusFirst)
              if (retValWalrusCall .ne. 0) then
                  call ErrMsgStandard (981, 0, ' Some error during WALRUS routine Set B ', ' RRRunoffNode_Init1' )
              endif
!             Write(*,*) ' WalrusSet Theta_S'
              retValWalrusCall = WalrusSet (IRRRunoffSub, wc_par_theta_s, Walrus_theta_s(IRRRunoffSub),WalrusFirst)
              if (retValWalrusCall .ne. 0) then
                  call ErrMsgStandard (981, 0, ' Some error during WALRUS routine Set theta_s ', ' RRRunoffNode_Init1' )
              endif
           endif
           ! user defined tables
!            Write(*,*) ' Walrus_W_DV'
             if (.not. Walrus_WA(IRRRunoffSub)) then
                 Success = Success .and. Dh_AllocInit (Walrus_WILength(IRRRunoffSub),Walrus_Temp1,  0.D00)
                 Success = Success .and. Dh_AllocInit (Walrus_WILength(IRRRunoffSub),Walrus_Temp2,  0.D00)
                 Do i=1,Walrus_WILength(IRRRunoffSub)
                    Walrus_Temp1(i) = Walrus_WIDV_DV(IRRRunoffSub,i)
                    Walrus_Temp2(i) = Walrus_WIDV_WI(IRRRunoffSub,i)
                 Enddo
!                Write(*,*) ' Walrus_Set_W_DV'
           !     Walrus.set_W_dV_bytable (Walrus_Temp1, Walrus_Temp2)
                 retValWalrusCall = WalrusSetWdVByTable (IRRRunoffSub, Walrus_Temp1, Walrus_WILength(IRRRunoffSub), &
                                                                       Walrus_Temp2, Walrus_WILength(IRRRunoffSub),WalrusFirst)
                 if (retValWalrusCall .ne. 0) then
                     call ErrMsgStandard (981, 0, ' Some error during WALRUS routine SetWDVByTable ', ' RRRunoffNode_Init1' )
                 endif
                 Deallocate(Walrus_Temp1)
                 Deallocate(Walrus_Temp2)
             endif

             if (.not. Walrus_VA(IRRRunoffSub)) then
                 Success = Success .and. Dh_AllocInit (Walrus_VEQLength(IRRRunoffSub),Walrus_Temp1,  0.D00)
                 Success = Success .and. Dh_AllocInit (Walrus_VEQLength(IRRRunoffSub),Walrus_Temp2,  0.D00)
                 Do i=1,Walrus_VEQLength(IRRRunoffSub)
                    Walrus_Temp1(i) = Walrus_VEQDG_DG(IRRRunoffSub,i)
                    Walrus_Temp2(i) = Walrus_VEQDG_VEQ(IRRRunoffSub,i)
                 Enddo
!                Write(*,*) ' Walrus_Set_Veq_DG'
           !     Walrus.set_dVeq_dG_bytable (Walrus_Temp1, Walrus_Temp2)
                 retValWalrusCall = WalrusSetdVeqdGByTable (IRRRunoffSub, Walrus_Temp1, Walrus_VEQLength(IRRRunoffSub), &
                                                                       Walrus_Temp2, Walrus_VEQLength(IRRRunoffSub),WalrusFirst)
                 if (retValWalrusCall .ne. 0) then
                     call ErrMsgStandard (981, 0, ' Some error during WALRUS routine SetdVeqdGByTable ', ' RRRunoffNode_Init1' )
                 endif
                 Deallocate(Walrus_Temp1)
                 Deallocate(Walrus_Temp2)
             endif

             if (.not. Walrus_BA(IRRRunoffSub)) then
                 Success = Success .and. Dh_AllocInit (Walrus_ERLength(IRRRunoffSub),Walrus_Temp1,  0.D00)
                 Success = Success .and. Dh_AllocInit (Walrus_ERLength(IRRRunoffSub),Walrus_Temp2,  0.D00)
                 Do i=1,Walrus_ERLength(IRRRunoffSub)
                    Walrus_Temp1(i) = Walrus_ERDV_DV(IRRRunoffSub,i)
                    Walrus_Temp2(i) = Walrus_ERDV_ER(IRRRunoffSub,i)
                 Enddo
!                Write(*,*) ' Walrus_Set_Beta_dV'
           !     Walrus.set_beta_dV_bytable (Walrus_Temp1, Walrus_Temp2)
                 retValWalrusCall = WalrusSetBetadVByTable (IRRRunoffSub, Walrus_Temp1, Walrus_ERLength(IRRRunoffSub), &
                                                                       Walrus_Temp2, Walrus_ERLength(IRRRunoffSub),WalrusFirst)
                 if (retValWalrusCall .ne. 0) then
                     call ErrMsgStandard (981, 0, ' Some error during WALRUS routine SetBetadVByTable ', ' RRRunoffNode_Init1' )
                 endif
                 Deallocate(Walrus_Temp1)
                 Deallocate(Walrus_Temp2)
             endif

             if (.not. Walrus_QA(IRRRunoffSub)) then
                 Success = Success .and. Dh_AllocInit (Walrus_QHLength(IRRRunoffSub),Walrus_Temp1,  0.D00)
                 Success = Success .and. Dh_AllocInit (Walrus_QHLength(IRRRunoffSub),Walrus_Temp2,  0.D00)
                 Do i=1,Walrus_QHLength(IRRRunoffSub)
                    Walrus_Temp1(i) = Walrus_QH_H(IRRRunoffSub,i)
                    Walrus_Temp2(i) = Walrus_QH_Q(IRRRunoffSub,i)
                 Enddo
!                Write(*,*) ' Walrus_Set_Q_hS'
           !     Walrus.set_Q_hS_bytable (Walrus_Temp1, Walrus_Temp2)
                 retValWalrusCall = WalrusSetQhSByTable (IRRRunoffSub, Walrus_Temp1, Walrus_QHLength(IRRRunoffSub), &
                                                                       Walrus_Temp2, Walrus_QHLength(IRRRunoffSub),WalrusFirst)
                 if (retValWalrusCall .ne. 0) then
                     call ErrMsgStandard (981, 0, ' Some error during WALRUS routine SetQhSByTable ', ' RRRunoffNode_Init1' )
                 endif
                 Deallocate(Walrus_Temp1)
                 Deallocate(Walrus_Temp2)
             endif

           ! pass initial conditions WALRUS
           ! Walrus.init  (WalrusStartTime, Walrus_Q0(iRRRunoffSub), Walrus_HS0(iRRRunoffSub), Walrus_DG0(iRRRunoffSub), Walrus_DV0(iRRRunoffSub), Walrus_HQ0(iRRRunoffSub), 1D0)
           ! Walrus.set   (60, WalrusStartTime)
           ! Walrus.init  (WalrusStartTime, Walrus_Q0(iRRRunoffSub), Walrus_HS0(iRRRunoffSub), Walrus_DG0(iRRRunoffSub), Walrus_DV0(iRRRunoffSub), Walrus_HQ0(iRRRunoffSub), 1D0)
!          Write(*,*) ' Walrus_Set StartTime', WalrusStartTime
           retValWalrusCall = WalrusSet (IRRRunoffSub, wc_cur_time, WalrusStartTime, WalrusFirst)
           if (retValWalrusCall .ne. 0) then
               call ErrMsgStandard (981, 0, ' Some error during WALRUS routine Set StartTime ', ' RRRunoffNode_Init1' )
           endif
!          Write(*,*) ' Walrus_Init'
           retValWalrusCall = WalrusInit (IRRRunoffSub, WalrusStartTime, Walrus_Q0(iRRRunoffSub), Walrus_HS0(iRRRunoffSub), &
                                          Walrus_DG0(iRRRunoffSub), Walrus_DV0(iRRRunoffSub), Walrus_HQ0(iRRRunoffSub), GFrac, WalrusFirst)
           if (retValWalrusCall .ne. 0) then
               call ErrMsgStandard (981, 0, ' Some error during WALRUS routine Set Init ', ' RRRunoffNode_Init1' )
           endif

        elseif  (.not. FirstCall .and. RRRunoff_CompOption(IRRRunoff) .eq. 6)  then
           ! second call of INIT1, only update initial conditions WALRUS
           ! Walrus.init  (WalrusStartTime, Walrus_Q0(iRRRunoffSub), Walrus_HS0(iRRRunoffSub), Walrus_DG0(iRRRunoffSub), Walrus_DV0(iRRRunoffSub), Walrus_HQ0(iRRRunoffSub), 1D0)
           ! Walrus.set   (60, WalrusStartTime)
!          Write(*,*) ' Walrus_Set StartTime', WalrusStartTime
           retValWalrusCall = WalrusSet (IRRRunoffSub, wc_cur_time, WalrusStartTime, WalrusFirst)
           if (retValWalrusCall .ne. 0) then
               call ErrMsgStandard (981, 0, ' Some error during WALRUS routine Set StartTime ', ' RRRunoffNode_Init1' )
           endif
!          Write(*,*) ' Walrus_Init'
           retValWalrusCall = WalrusInit (IRRRunoffSub, WalrusStartTime, Walrus_Q0(iRRRunoffSub), Walrus_HS0(iRRRunoffSub), &
                                          Walrus_DG0(iRRRunoffSub), Walrus_DV0(iRRRunoffSub), Walrus_HQ0(iRRRunoffSub), GFrac, WalrusFirst)
           if (retValWalrusCall .ne. 0) then
               call ErrMsgStandard (981, 0, ' Some error during WALRUS routine Set Init ', ' RRRunoffNode_Init1' )
           endif

        endif
     Enddo
   endif

   do i=1,NCRRRunoff
      IRRRunoffSub = RRRunoff_SubIndex(i)
      if (RRRunoff_CompOption(i) .eq. 2) then ! SCS node
         if (SCS_UHChosen(iRRRunoffSub) .eq. 2) then ! Snyder
            call compute_snyder_hydrograph(SHG_set%SHG(iRRRunoffSub),  AREA_RRRunoffNode(i), SCS_Snyder_Cp(iRRRunoffSub), SCS_TLag(iRRRunoffSub),timeSettings%TimestepSize/3600.)
         endif
      endif
   enddo

   do i=1,NCRRRunoff
      IRRRunoffSub = RRRunoff_SubIndex(i)
      if (RRRunoff_CompOption(i) .eq. 2) then ! SCS node
         if (SCS_UseGreenAmpt_Infiltration(iRRRunoffSub)) then ! use GreenAmpt Infiltration
            call SetGreenAmptConstants (SCS_GreenAmpt_Ksat(iRRRunoffSub), SCS_GreenAmpt_Psi(iRRRunoffSub), SCS_GreenAmpt_Theta_Dmax(iRRRunoffSub), &
                 SCS_GreenAmpt_Lu(iRRRunoffSub), SCS_GreenAmpt_Kr(iRRRunoffSub), SCS_GreenAmpt_Tr(iRRRunoffSub), idebug)
         endif
      endif
   enddo

   if (NcRRRunoffSCS .gt. 0) then
     Do i=1,NCRRRunoff
        IRRRunoffSub = RRRunoff_SubIndex(i)
        If (RRRunoff_CompOption(i) .eq. 2) then
           ! set CN1, CN2, CN3 based on input
           SCS_CN1(iRRRunoffSub) = (4.2 * SCS_CurveNumber(iRRRunoffSub)) / (10 - 0.058 * SCS_CurveNumber(iRRRunoffSub))
           SCS_CN2(iRRRunoffSub) = SCS_CurveNumber(iRRRunoffSub)
           SCS_CN3(iRRRunoffSub) = (23. * SCS_CurveNumber(iRRRunoffSub)) / (10 + 0.13  * SCS_CurveNumber(iRRRunoffSub))
           ! set selected CN based on AMC
           if (SCS_AMC(iRRRunoffSub) .eq. 1) then
              SCS_CurveNumber(iRRRunoffSub) =  SCS_CN1(iRRRunoffSub)
           elseif (SCS_AMC(iRRRunoffSub) .eq. 2) then
              SCS_CurveNumber(iRRRunoffSub) =  SCS_CN2(iRRRunoffSub)
           elseif (SCS_AMC(iRRRunoffSub) .eq. 3) then
              SCS_CurveNumber(iRRRunoffSub) =  SCS_CN3(iRRRunoffSub)
           endif
        endif
     Enddo

     SCS_MaxRetention = (25400. - 254 * SCS_CurveNumber) / SCS_CurveNumber
     ! Tlag in hours, Tc in hours !!
     ! only compute Tlag if it is not yet specified by the user!
     Do i=1,NCRRRunoff
        IRRRunoffSub = RRRunoff_SubIndex(i)
        If (RRRunoff_CompOption(i) .eq. 2) then
           SCS_MaxRetention(iRRRunoffSub) = max (0.0, SCS_MaxRetention(iRRRunoffSub))   ! MaxRetention >= 0
           If (SCS_TLag(iRRRunoffSub) .lt. 0.0) then
              If (SCS_Slope(iRRRunoffSub) .gt. 0.0) then
                 SCS_TLag(iRRRunoffSub) = ((SCS_Length(iRRRunoffSub) ** 0.8) * ((2540.0 - 22.86*SCS_CurveNumber(iRRRunoffSub)) ** 0.7)) / &
                                         (14104*(SCS_CurveNumber(iRRRunoffSub) ** 0.7) * (SCS_Slope(iRRRunoffSub) ** 0.5 ))
              Else
                 call ErrMsgStandard (972, 0, ' SCS slope=0, but should be > 0 ', ' Check input data' )
              Endif
           Endif
        Endif
     Enddo

     SCS_Tc      = SCS_TLag / 0.6
     ! convert Tc to computation timesteps
     ! SCS_Tlag = ceiling ( SCS_Tlag * 3600. / timeSettings%TimestepSize)
     SCS_Tc = ( SCS_Tc * 3600. / timeSettings%TimestepSize)
     SCS_HMSLinResR = ( SCS_HMSLinResR * 3600. / timeSettings%TimestepSize)  ! R linear reservoir coefficient from hours to nr. timesteps
     Do IRRRunoffSub=1,NcRRRunoffSCS
        If (SCS_HMSLinResR(IRRRunoffSub) .gt. 0) SCS_HMSC1(IRRRunoffSub) = ( 1.0E0 / (SCS_HMSLinResR(IRRRunoffSub) + 0.5 ) )
     Enddo
     SCS_HMSC2   = 1.0E0 - SCS_HMSC1
     SCS_PAccum  = 0.0
     SCS_PExcess = 0.0
     SCS_Storage = 0.0
     SCS_Storage0= 0.0
     SCS_Rainfall= 0.0

     ! unit hydrograph max timesteps
     MaxTc = 0
     Do i=1,NCRRRunoff
        IRRRunoffSub = RRRunoff_SubIndex(i)
        If (RRRunoff_CompOption(i) .eq. 2) then
           If (SCS_UHChosen(iRRRunoffSub) .eq. 0) then
               ! HEC-HMS
               MaxTc = max (MaxTc, Ceiling(SCS_Tc(iRRRunoffSub)) + 1 )
                ! with linear reservoir: take into account further attenuation/extension of SCS_TC with factor 5
               if (SCS_HMSLinResR(iRRRunoffSub) .gt. 0) MaxTc = max (MaxTc, 5*Ceiling(SCS_Tc(iRRRunoffSub)) + 1 )
           else If (SCS_UHChosen(iRRRunoffSub) .eq. 1) then
               ! SCS dimensionless; first convert Timelag to nr of computation timesteps! The factor 5 is related to time-lage (< Tc) as specified in SCS method
               MaxTc = max (MaxTc, 1 + Ceiling (5* (SCS_Tlag(iRRRunoffSub) * 3600./timeSettings%TimestepSize + 0.5)))
           else if (SCS_UHChosen(iRRRunoffSub) .eq. 2) then ! Snyder
               SCS_Tc(iRRRunoffSub) = (SHG_set%SHG(iRRRunoffSub)%time_array(7) * 3600./timeSettings%TimestepSize)
               MaxTc = max (MaxTc,ceiling(SCS_Tc(iRRRunoffSub)) + 1)
           endif
        endif
     Enddo

     ! Baseflow extensions
     Do i=1,NCRRRunoff
        IRRRunoffSub = RRRunoff_SubIndex(i)
        If (RRRunoff_CompOption(i) .eq. 2) then
           SCS_SurfMax    (iRRRunoffSub) = max (0.0, 0.2 * (25400. - 254. * SCS_CN1        (iRRRunoffSub)) / SCS_CN1          (iRRRunoffSub) )
           SCS_SurfInit   (iRRRunoffSub) = max (0.0, 0.2 * (25400. - 254. * SCS_CurveNumber(iRRRunoffSub)) / SCS_CurveNumber  (iRRRunoffSub) )
           SCS_SubSurfMax (iRRRunoffSub) = max (0.0, 1.0 * (25400. - 254. * SCS_CN1        (iRRRunoffSub)) / SCS_CN1          (iRRRunoffSub) )
           SCS_SubSurfInit(iRRRunoffSub) = max (0.0, 1.0 * (25400. - 254. * SCS_CurveNumber(iRRRunoffSub)) / SCS_CurveNumber  (iRRRunoffSub) )
        Endif
     Enddo
     SCS_SurfAct     = SCS_SurfInit
     SCS_SubSurfAct  = SCS_SubSurfInit
     SCS_GWAct       = SCS_InitGWCap
   endif

   if (Ievent .eq. 1 .and. FirstCall .and. NcRRRunoffSCS .gt. 0) then
       Success = Dh_AllocInit (MaxTc, NcRRRunoff, SCS_UnitHydComp, 0E0)
       Success = Dh_AllocInit (MaxTc, SCS_HMSLinResInflow, 0E0)
       Success = Success .and. Dh_AllocInit (NcRRRunoff, MaxTc, SCS_AvailableRunoff, 0E0)
       If (.not. success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', ' RRRunoffNode_Init1' )
       Call ComputeSCSUnitHydrographComponents
       Do i=1,NCRRRunoff
          IRRRunoffSub = RRRunoff_SubIndex(i)
          If (RRRunoff_CompOption(i) .eq. 2) then
             if (SCS_UseBaseFlow(iRRRunoffSub)) then
                SCS_GWRecessionConst(iRRRunoffSub) = SCS_GWRecessionConst(iRRRunoffSub) * TimeSettings%TimestepSize / 86400.   ! convert 1/day to 1/timestep
             endif
          endif
       Enddo
   endif

   if (NcRRRunoffLGSI .gt. 0) then
     ! from input file:
       if (FirstCall) then
           LGSI_InitGwl = LGSI_SpecifiedInitGwl
       else
           LGSI_InitGwl = LGSI_NewGwl
       endif
       Do i=1,NcRRRunoff
          if (RRRunoff_CompOption(i) .eq. 4) then
             isub = RRRunoff_SubIndex(i)
             Do j=1,LGSI_NrSubAreas(isub)
                LGSIGwl = LGSI_InitGwl(isub,j)
                Do k=1,LGSI_MaxInterpLengthPlus1
                    GWLevelArray(k)  = LGSI_InterpGwLevelTotalV(k,isub,j)
                    GWVolumeArray(k) = LGSI_InterpTotalVolume(k,isub,j)
                Enddo
                Call RR_D_INTERP  (LGSI_MaxInterpLength, GWLevelArray, GWVolumeArray, LGSIGWL, LGSIVolume, Ilast1)
                LGSI_InitVolume(isub,j) = LGSIVolume
             Enddo
          Endif
       Enddo
       LGSI_NewGwl = LGSI_InitGwl
       LGSI_NewVolume = LGSI_InitVolume
   endif

   Call RRRunoffNode_Init2(1, 1, 1)
   if (LevelError)  Call ErrMsgStandard (981, 0, ' Error: See earlier error messages from subroutine ', ' RRRunoffNode_Init1' )

  Return
  End subroutine RRRunoffNode_Init1


  Subroutine GetHSminFromTable (HSmin, IRRRunoffSub)
    ! *********************************************************************
    ! *** Last update: 21 March 2000                      by: Geert Prinsen
    ! *********************************************************************
    ! *** Brief description:
    ! *** ------------------
    ! ***    Bepaal HSMin uit tijdtabel
    ! *********************************************************************
    ! *** Input/output parameters:
    ! *** ------------------------
    ! ***  Hsmin  = HSMin Walrus
    ! ***  IRRRunoffSub = index Walrus
    ! *********************************************************************

    INTEGER IRRRunoffSub, rowNr, TabelNr, Idebug, Iout1
    Double precision HSMin
    type (Date) currentDate
    type (Time) currentTime
    Logical DateTimeOutsideTable

    IOut1  = ConfFil_get_IOut1()
    Idebug = ConfFil_get_IDEBUG()

    currentDate%year = ConfArr_get_IYear()
    currentDate%month = ConfArr_get_iMonth()
    currentDate%day = ConfArr_get_iDay()
    currentTime%hour = ConfArr_get_iHour()
    currentTime%minute = ConfArr_get_iMinute()
    currentTime%second = 0
    RowNr = -1
    TabelNr = Walrus_HSminRefTable(iRRRunoffSub)
    HSmin   = Dble ( GetNewValue(TableHandle, TabelNr, 1, RowNr, CurrentDate, CurrentTime, Idebug, iout1, DateTimeOutsideTable, .true.) )
!   no further conversion, input is in mm
    if (idebug .ne. 0) WRITE (IDEBUG,*) ' From table GetNewValue HsMin', HSmin
    if (idebug .ne. 0) WRITE (IDEBUG,*) ' IRRRunoffSub rowNr HSmin', iRRRunoffSub, RowNr, HSmin

    RETURN
  END subroutine GetHSMinFromTable



  Subroutine ComputeSCSUnitHydrographComponents
  ! Compute Unit Hydrograph components of SCS - Clarke Unit Hydrograph method, sum for each location should be 1.

  implicit none

  Integer    IRRRunoff, j, NrUHComponents, NTpQp, i, idebug, idum, iRRRunoffsub
  Real       SumTp, SumQp, Tp, TpArray(34), QpArray(34), InputTpValue, OutputQpValue, ScaledSumQp

  TpArray = 0
  QpArray = 0
  nTpQp   = 34  ! 30

!  TpArray (1) = 0.0
!  TpArray (2) = 0.1
!  TpArray (3) = 0.2
!  TpArray (4) = 0.3
!  TpArray (5) = 0.4
!  TpArray (6) = 0.5
!  TpArray (7) = 0.6
!  TpArray (8) = 0.7
!  TpArray (9) = 0.8
!  TpArray (10) = 0.9
!  TpArray (11) = 1.0
!  TpArray (12) = 1.1
!  TpArray (13) = 1.2
!  TpArray (14) = 1.3
!  TpArray (15) = 1.4
!  TpArray (16) = 1.5
!  TpArray (17) = 1.6
!  TpArray (18) = 1.8
!  TpArray (19) = 2.0
!  TpArray (20) = 2.2
!  TpArray (21) = 2.4
!  TpArray (22) = 2.6
!  TpArray (23) = 2.8
!  TpArray (24) = 3.0
!  TpArray (25) = 3.5
!  TpArray (26) = 4.0
!  TpArray (27) = 4.5
!  TpArray (28) = 5.0
!  TpArray (29) = 5.5
!  TpArray (30) = 6.0

!  QpArray (1) = 0.0
!  QpArray (2) = 0.015
!  QpArray (3) = 0.075
!  QpArray (4) = 0.16
!  QpArray (5) = 0.28
!  QpArray (6) = 0.43
!  QpArray (7) = 0.6
!  QpArray (8) = 0.77
!  QpArray (9) = 0.89
!  QpArray (10) = 0.97
!  QpArray (11) = 1.00
!  QpArray (12) = 0.98
!  QpArray (13) = 0.92
!  QpArray (14) = 0.84
!  QpArray (15) = 0.75
!  QpArray (16) = 0.66
!  QpArray (17) = 0.56
!  QpArray (18) = 0.42
!  QpArray (19) = 0.32
!  QpArray (20) = 0.24
!  QpArray (21) = 0.18
!  QpArray (22) = 0.13
!  QpArray (23) = 0.098
!  QpArray (24) = 0.075
!  QpArray (25) = 0.036
!  QpArray (26) = 0.018
!  QpArray (27) = 0.009
!  QpArray (28) = 0.004
!  QpArray (29) = 0.0
!  QpArray (30) = 0.0

  TpArray (1) = 0.0
  TpArray (2) = 0.1
  TpArray (3) = 0.2
  TpArray (4) = 0.3
  TpArray (5) = 0.4
  TpArray (6) = 0.5
  TpArray (7) = 0.6
  TpArray (8) = 0.7
  TpArray (9) = 0.8
  TpArray (10) = 0.9
  TpArray (11) = 1.0
  TpArray (12) = 1.1
  TpArray (13) = 1.2
  TpArray (14) = 1.3
  TpArray (15) = 1.4
  TpArray (16) = 1.5
  TpArray (17) = 1.6
  TpArray (18) = 1.7
  TpArray (19) = 1.8
  TpArray (20) = 1.9
  TpArray (21) = 2.0
  TpArray (22) = 2.2
  TpArray (23) = 2.4
  TpArray (24) = 2.6
  TpArray (25) = 2.8
  TpArray (26) = 3.0
  TpArray (27) = 3.2
  TpArray (28) = 3.4
  TpArray (29) = 3.6
  TpArray (30) = 3.8
  TpArray (31) = 4.0
  TpArray (32) = 4.5
  TpArray (33) = 5.0
  TpArray (34) = 5.5

  QpArray (1) = 0.0
  QpArray (2) = 0.03
  QpArray (3) = 0.10
  QpArray (4) = 0.19
  QpArray (5) = 0.31
  QpArray (6) = 0.47
  QpArray (7) = 0.66
  QpArray (8) = 0.82
  QpArray (9) = 0.93
  QpArray (10) = 0.99
  QpArray (11) = 1.00
  QpArray (12) = 0.99
  QpArray (13) = 0.93
  QpArray (14) = 0.86
  QpArray (15) = 0.78
  QpArray (16) = 0.68
  QpArray (17) = 0.56
  QpArray (18) = 0.46
  QpArray (19) = 0.39
  QpArray (20) = 0.33
  QpArray (21) = 0.280
  QpArray (22) = 0.207
  QpArray (23) = 0.147
  QpArray (24) = 0.107
  QpArray (25) = 0.077
  QpArray (26) = 0.055
  QpArray (27) = 0.040
  QpArray (28) = 0.029
  QpArray (29) = 0.021
  QpArray (30) = 0.015
  QpArray (31) = 0.011
  QpArray (32) = 0.005
  QpArray (33) = 0.0
  QpArray (34) = 0.0

  SumTp = 0.
  SumQp = 0.
  Do i=1,nTpQp
     SumTp = SumTp + TpArray(i)
     SumQp = SumQp + QpArray(i)
  Enddo
  idum = 1

  Do IRRRunoff=1,NCRRRunoff
     iRRRunoffsub = RRRunoff_SubIndex(iRRRunoff)
     if (RRRunoff_CompOption(IRRRunoff) .eq. 2) then
        if (SCS_UHChosen(IRRRunoffSub) .eq. 0) then
           ! HEC-HMS unit hydrograph, as used in Jakarta Floods 2007
           ! time-area diagram g
           Do j=1,ceiling(SCS_Tc(IRRRunoffSub))
              if (j .le. 0.5*SCS_TC(IRRRunoffSub))  then
                 SCS_UnitHydComp(j,IRRRunoffSub) = 1.414 * ( (j / SCS_Tc(IRRRunoffSub)) ** 1.5)
              elseif (j .le. SCS_TC(IRRRunoffSub) ) then
                 SCS_UnitHydComp(j,IRRRunoffSub) = 1 - 1.414 * ( (1 - (j / SCS_Tc(IRRRunoffSub)) ) ** 1.5)
              endif
           Enddo
           SCS_UnitHydComp(Ceiling(SCS_Tc(IRRRunoffSub)),IRRRunoffSub) = 1.0
           ! Convert cumulative values to increments
           Do j=ceiling(SCS_Tc(IRRRunoffSub)), 2, -1
              SCS_UnitHydComp(j,IRRRunoffSub) = SCS_UnitHydComp(j,IRRRunoffSub) - SCS_UnitHydComp(j-1,IRRRunoffSub)
           Enddo
        else if (SCS_UHChosen(IRRRunoffSub) .eq. 1) then
           iDebug = ConfFil_get_iDebug()
           ! SCS dimensionless unit hydrograph
           Tp = (SCS_Tlag(iRRRunoffSub) * 3600./timeSettings%TimestepSize) + 0.5
           NrUHcomponents = Ceiling (5 * Tp)
           ScaledSumQp = 0.0
           if (idebug .ne. 0) write(idebug,*) ' Tlag, Tp, NrUHComponents ',SCS_Tlag(IRRRunoffSub), Tp, NrUHComponents
           Do j=1,NrUHComponents
              InputTpValue = float (j ) / Tp
              CALL RR_INTERP (NTpQp, TpArray, QpArray, InputTpValue, OutputQpValue, idum)
              ScaledSumQp = ScaledSumQp + OutputQpValue
              SCS_UnitHydComp(j,IRRRunoffSub) = OutputQpValue
              if (idebug .ne. 0) &
                 write(idebug,*) ' j, InputTp, OutputQp, SCS_UH,', InputTpValue, OutputQpValue, SCS_UnitHydComp(j,IRRRunoffSub)
           Enddo
        else if (SCS_UHChosen(IRRRunoffSub) .eq. 2) then! Snyder hydrograph
           call interpolate_snyder_hydrograph(SHG_set%SHG(iRRRunoffsub), SCS_UnitHydComp(:,IRRRunoffSub),Ceiling (SCS_Tc(iRRRunoffSub)),SCS_Snyder_UH_decay_frac(iRRRunoffsub),SCS_Snyder_UH_decay_rate(iRRRunoffSub))
        endif
     Endif
  Enddo

  Return
  End subroutine ComputeSCSUnitHydrographComponents



  Subroutine RRRunoffNode_Init2 (IFlagRRRunoff, CallFromInit1, Itmstp)

   use Snyder_hydrograph
   implicit none

   Integer i, j, IRRRunoff, NrUHComponents, IFlagRRRunoff, CallFromInit1, Itmstp, IRRRunoffSub
   Real    Tp
   double precision GwPumpFlow

   if (NcRRRunoffHBV .gt. 0) then
      HBV_DrySnowContent0    =  HBV_DrySnowContent
      HBV_FreeWaterContent0  =  HBV_FreeWaterContent
      HBV_SoilMoisture0      =  HBV_SoilMoisture
      HBV_QRunoffInmm0       =  HBV_QRunoffInmm
      HBV_UpperZoneContent0  =  HBV_UpperZoneContent
      HBV_LowerZoneContent0  =  HBV_LowerZoneContent
   endif

   if (NcRRRunoffSCS .gt. 0) then
      SCS_Storage0           =  SCS_Storage
      SCS_HMSLinResContent0  =  SCS_HMSLinResContent
      SCS_PAccum0            =  SCS_PAccum
      SCS_PExcess0           =  SCS_PExcess
      ! SCSbaseflow extensions
      SCS_GwAct0            =  SCS_GwAct
      SCS_SubSurfAct0       =  SCS_SubSurfAct
      SCS_SurfAct0          =  SCS_SurfAct
      ! HMS lin.res
      SCS_HMSLinResOutflow0 =  SCS_HMSLinResOutflow
      SCS_HMSLinResOutflow  =  0.0
      SCS_HMSLinResInflow   =  0.0
      SCS_HMSLinResInflowTot=  0.0
   endif

   if (NcRRRunoffLGSI .gt. 0) then
       LGSI_InitGwl  = LGSI_NewGwl
       LGSI_InitVolume = LGSI_NewVolume
       If (IFlagRRRunoff .eq. 1 .and. CallFromInit1 .eq. 0) then
          do i=1,NcRRRunoff
            iRRRunoffSub = RRRunoff_SubIndex(i)
            if (RRRunoff_CompOption(i) .eq. 4) then
                do j=LGSI_MaxDelayLength,1,-1
                   LGSI_HistoryQtot(iRRRunoffSub,j+1) = LGSI_HistoryQtot(iRRRunoffSub,j)
                   LGSI_HistoryQdelayed(iRRRunoffSub,j+1) = LGSI_HistoryQdelayed(iRRRunoffSub,j)
                enddo
            endif
          enddo
       endif
   endif

   if (NcRRRunoffWagmod .gt. 0) then
      WagMod_RoutVol1 = WagMod_RoutVol
      WagMod_SMT1     = WagMod_SM
      WagMod_GStoreT1 = WagMod_GStore
      if (itmstp .ge. 2) then
         Do iRRRunoff=1,NcRRRunoff
            If (RRRunoff_CompOption(IRRRunoff) .eq. 5) then  !Wagmod
               IRRRunoffSub = RRRunoff_SubIndex(IRRRunoff)
! 50469 shift WAGMOD_QG and _QD
               WagMod_QGT1(IRRRunoffSub) = WagMod_QG(IRRRunoffSub, 1)
               Do i = 1, Wagmod_MaxNrTimestepsSimulation-1
                 WagMod_QG(IRRRunoffSub,i) = WagMod_QG(IRRRunoffSub,i+1)
                 WagMod_QD(IRRRunoffSub,i) = WagMod_QD(IRRRunoffSub,i+1)
               Enddo
            endif
         Enddo
      endif
   endif

   if (NcRRRunoffWalrus .gt. 0) then
      Walrus_DVPrevious =  Walrus_DVCurrent
      Walrus_DGPrevious =  Walrus_DGCurrent
      Walrus_HQPrevious =  Walrus_HQCurrent
      Walrus_HSPrevious =  Walrus_HSCurrent
   endif

   if (NcRRRunoffNAM .gt. 0) then
 !    new
       NAM_U0                       = NAM_U
       NAM_UInitial                 = NAM_U0
       NAM_Linitial                 = NAM_L
       NAM_GWSDinitial              = NAM_GWSD
       NAM_GWD0                     = NAM_GWSD
       NAM_InitialGWDepth           = NAM_GWD0

       Do IRRRunoff=1,NCRRRunoff
          IRRRunoffSub = RRRunoff_SubIndex(IRRRunoff)
          if (RRRunoff_CompOption(IRRRunoff) .eq. 3) then
       !     Initialisation for variable GW pumping
             If (NAMPumpOption(iRRRunoffSub) .ge. 1) then
                 Call GetGWPumpFlowFromTable (GwPumpFlow, iRRRunoffSub)
                 NAMPumpFlow(iRRRUnoffSub) = GwPumpFlow
             endif
          endif
       Enddo
   endif

   If (IFlagRRRunoff .eq. 1 .and. CallFromInit1 .eq. 0 .and. NcRRRunoffSCS .gt. 0) then
      Do IRRRUnoff=1,NCRRRunoff
         IRRRunoffSub = RRRunoff_SubIndex(IRRRunoff)
         if (RRRunoff_CompOption(IRRRunoff) .eq. 2) then
            if (SCS_UHChosen(IRRRunoffSub) .eq. 0 .or. SCS_UHChosen(IRRRunoffSub) .eq. 2) then
               Do j=Ceiling(SCS_Tc(IRRRunoffSub))+1, 2, -1
                  SCS_AvailableRunoff(IRRRunoffSub,j) = SCS_AvailableRunoff(IRRRunoffSub,j-1)
               Enddo
            else if (SCS_UHChosen(IRRRunoffSub) .eq. 1) then
               Tp = (SCS_Tlag(iRRRunoffSub) * 3600./timeSettings%TimestepSize) + 0.5
               NrUHcomponents = Ceiling (5 * Tp) + 1
               Do j=NrUHComponents,2,-1
                  SCS_AvailableRunoff(IRRRunoffSub,j) = SCS_AvailableRunoff(IRRRUnoffSub,j-1)
               Enddo
            endif
         endif
      Enddo
   endif

  Return
  End subroutine RRRunoffNode_Init2



  Subroutine ReadOpenDAHBV (Infile1, iout1, update)

  ! read HBV restart info from ASCII OpenDA file
  integer      Infile1, iout1, idebug
  logical      update
  Integer      RetVal
  Integer      irrrunoff, IRRRunoffSub

  Integer       inod
  Integer       iecode
  Character(Len=1000) String
  Integer        Nhlp
  Parameter     (Nhlp = 32)
  Integer       IDUM(NHLP)
  Real          RDUM(NHLP)
  Character(Len=CharIdLength) CDUM(NHLP)
  Logical       allow, found, endfil
  Logical  LevelError

   LevelError = .false.
   Retval = 0

  ! file is already opened, rewind it
  Rewind(Infile1)
  update = .false.
  iDebug = ConfFil_get_iDebug()

  Call SKPCOM (Infile1, ENDFIL,'ODS')
  call SetMessage(LEVEL_INFO, 'Read OpenDA HBV data')
  do while (.not. endfil)
    ! read OpenDA record
     Read(Infile1,'(A1000)',END=21,ERR=150,IOSTAT=IECODE)  STRING
     ! skip regel als hij niet begint met juist keyword (HBV)
     If (STRING(1:4) .EQ. 'HBV') then
      ! HBV node id
        allow = .false.
        Retval = Retval + GetVAR2 (STRING,' id ',1,' HBV-ReadAscii',' OpenDA file',IOUT1, &
                                   CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
        call fndNd2(inod, CDUM(1))
        if (inod .gt. 0) then      ! knoop bestaat in schematisatie
            irrRunoff = EiNode(inod,2)
            if (EiNode(inod,3) .eq. 19) then  ! en is HBV
                ! get the data
                ! update the corresponding RR variables and related variables
                IRRRunoffSub = RRRunoff_SubIndex(IRRRunoff)
                allow = .true.
                Retval = Retval + GetVAR2 (STRING,' drysnowcontent ',2, ' HBV-ReadAscii',' OPENDA file',IOUT1, &
                                              CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
                if (found) then
!                  write(*,*) ' found hbv id and drysnowcontent ', Id_nod(inod)(1:len_trim(id_nod(inod))), rdum(1)
                   HBV_DrySnowContent(IRRRunoffSub) = Rdum(1)
                   HBV_DrySnowContent0(IRRRunoffSub) = Rdum(1)
                   HBV_InitialDrySnowContent(IRRRunoffSub) = Rdum(1)
                   RSLMAP19_RRRunoff(NStartHBV+4,IRRRunoff,1) = HBV_DrySnowContent(IRRRunoffSub)
                   RRRunoff_Tnul(NStartHBV+4,irrRunoff) = RSLMAP19_RRRunoff(NStartHBV+4,IRRRunoff,1)
                   update = .true.
                endif
                allow = .true.
                Retval = Retval + GetVAR2 (STRING,' freewatercontent ',2, ' HBV-ReadAscii',' OPENDA file',IOUT1, &
                                              CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
                if (found) then
!                  write(*,*) ' found hbv id and freewatercontent ', Id_nod(inod)(1:len_trim(id_nod(inod))), rdum(1)
                   HBV_FreeWaterContent(IRRRunoffSub) = Rdum(1)
                   HBV_FreeWaterContent0(IRRRunoffSub) = Rdum(1)
                   HBV_InitialFreeWaterContent(IRRRunoffSub) = Rdum(1)
                   RSLMAP19_RRRunoff(NStartHBV+5,IRRRunoff,1) = HBV_FreeWaterContent(IRRRunoffSub)
                   RRRunoff_Tnul(NStartHBV+5,irrRunoff) = RSLMAP19_RRRunoff(NStartHBV+5,IRRRunoff,1)
                   update = .true.
                endif
                allow = .true.
                Retval = Retval + GetVAR2 (STRING,' soilmoisture ',2, ' HBV-ReadAscii',' OPENDA file',IOUT1, &
                                              CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
                if (found) then
!                  write(*,*) ' found hbv id and soilmoisture ', Id_nod(inod)(1:len_trim(id_nod(inod))), rdum(1)
                   HBV_SoilMoisture(IRRRunoffSub) = Rdum(1)
                   HBV_SoilMoisture0(IRRRunoffSub) = Rdum(1)
                   HBV_InitialMoisture(IRRRunoffSub) = Rdum(1)
                   RSLMAP19_RRRunoff(NStartHBV+6,IRRRunoff,1) = HBV_SoilMoisture(IRRRunoffSub)
                   RRRunoff_Tnul(NStartHBV+6,irrRunoff) = RSLMAP19_RRRunoff(NStartHBV+6,IRRRunoff,1)
                   update = .true.
                endif
                allow = .true.
                Retval = Retval + GetVAR2 (STRING,' upperzonecontent ',2, ' HBV-ReadAscii',' OPENDA file',IOUT1, &
                                              CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
                if (found) then
!                  write(*,*) ' found hbv id and upperzonecontent ', Id_nod(inod)(1:len_trim(id_nod(inod))), rdum(1)
                   HBV_UpperZoneContent(IRRRunoffSub) = Rdum(1)
                   HBV_UpperZoneContent0(IRRRunoffSub) = Rdum(1)
                   HBV_InitialUpperZoneContent(IRRRunoffSub) = Rdum(1)
                   RSLMAP19_RRRunoff(NStartHBV+7,IRRRunoff,1) = HBV_UpperZoneContent(IRRRunoffSub)
                   RRRunoff_Tnul(NStartHBV+7,irrRunoff) = RSLMAP19_RRRunoff(NStartHBV+7,IRRRunoff,1)
                   update = .true.
                endif
                allow = .true.
                Retval = Retval + GetVAR2 (STRING,' lowerzonecontent ',2, ' HBV-ReadAscii',' OPENDA file',IOUT1, &
                                              CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
                if (found) then
!                  write(*,*) ' found hbv id and lowerzonecontent ', Id_nod(inod)(1:len_trim(id_nod(inod))), rdum(1)
                   HBV_LowerZoneContent(IRRRunoffSub) = Rdum(1)
                   HBV_LowerZoneContent0(IRRRunoffSub) = Rdum(1)
                   HBV_InitialLowerZoneContent(IRRRunoffSub) = Rdum(1)
                   RSLMAP19_RRRunoff(NStartHBV+8,IRRRunoff,1) = HBV_LowerZoneContent(IRRRunoffSub)
                   RRRunoff_Tnul(NStartHBV+8,irrRunoff) = RSLMAP19_RRRunoff(NStartHBV+8,IRRRunoff,1)
                   update = .true.
                endif
            endif
        endif
     Endif
  enddo
  goto 21

150 continue
    call SetMessage(LEVEL_ERROR, 'Error reading OpenDA restart record '//String(1:len_trim(string)))
    LevelError = .true.
21  continue

    if (LevelError)  Call ErrMsgStandard (981, 0, ' Error: see earlier error messages in subroutine ', ' RRRunoffNode_ReadAscii' )
  End subroutine ReadOpenDaHBV


  Subroutine WriteOpenDAHBV (Infile1)

  ! write HBV restart info to ASCII OpenDA file
  integer      Infile1, idebug

  Integer       inode, iRRRunoff, IRRRunoffSub
  Character(len=1) Quote

  ! file is already opened
  iDebug = ConfFil_get_iDebug()
  quote = ''''

  do inode=1,ncnode
     irrRunoff = EiNode(inode,2)
     if (EiNode(inode,3) .eq. 19) then  ! en is HBV
         IRRRunoffSub = RRRunoff_SubIndex(IRRRunoff)
         write(Infile1,'(A,A1,A,A1,5(1X,A,G15.8),A)') 'HBV id ',&
                                       quote,id_nod(inode)(1:len_trim(id_nod(inode))),quote,&
                                       ' drysnowcontent ', HBV_DrySnowContent(IRRRunoffSub), &
                                       ' freewatercontent ', HBV_FreeWaterContent(iRRRunoffSub), &
                                       ' soilmoisture ', HBV_SoilMoisture(iRRRunoffSub), &
                                       ' upperzonecontent ', HBV_UpperZoneContent(iRRRunoffSub), &
                                       ' lowerzonecontent ', HBV_LowerZoneContent(iRRRunoffSub), ' hbv'
     Endif
  enddo

  End subroutine WriteOpenDaHBV


  Subroutine ReadOpenDaDNAM (Infile1, iout1, update)

  ! read DNAM restart info from ASCII OpenDA file
  integer      Infile1, iout1, idebug
  logical      update
  Integer      RetVal
  Integer      iRRRunoff, IRRRunoffSub

  Integer       inod, inode
  Integer        iecode
  Character(Len=1000) String
  Integer        Nhlp
  Parameter     (Nhlp = 32)
  Integer       IDUM(NHLP), idum1
  Real          RDUM(NHLP)
  Character(Len=CharIdLength) CDUM(NHLP)
  Logical       allow, found, endfil
  Integer       D_ifreal
  Logical  LevelError

  LevelError = .false.
  Retval = 0

  ! file is already opened, rewind it
  Rewind(Infile1)
! update = .false.
  iDebug = ConfFil_get_iDebug()

  Call SKPCOM (Infile1, ENDFIL,'ODS')
  call SetMessage(LEVEL_INFO, 'Read OpenDA NAM data')
  do while (.not. endfil)
    ! read OpenDA record
     Read(Infile1,'(A1000)',END=21,ERR=150,IOSTAT=IECODE)  STRING
     ! skip regel als hij niet begint met juist keyword (DNAM)
     If (STRING(1:4) .EQ. 'DNAM') then
      ! DNAM node id
        allow = .false.
        Retval = Retval + GetVAR2 (STRING,' id ',1,' DNAM-ReadAscii',' OpenDA file',IOUT1, &
                                   CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
        call fndNd2(inod, CDUM(1))
        if (inod .gt. 0) then      ! knoop bestaat in schematisatie
            inode = inod
            irrRunoff = EiNode(inod,2)
            if (EiNode(inod,3) .eq. 31) then  ! en is DNAM
                ! get the data
                ! update the corresponding RR variables and related variables
                IRRRunoffSub = RRRunoff_SubIndex(IRRRunoff)
                allow = .true.
                Retval = Retval + GetVAR2 (STRING,' NAMupperzonecontent ',2, ' DNAM-ReadAscii',' OPENDA file',IOUT1, &
                                              CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
                if (found) then
!                  write(*,*) ' found nam id and NAMupperzonecontent ', Id_nod(inod)(1:len_trim(id_nod(inod))), rdum(1)
                   NAM_U(IRRRunoffSub) = Rdum(1)
                   NAM_U0(IRRRunoffSub) = Rdum(1)
                   RSLMAP19_RRRunoff(NStartNAM+20,IRRRunoff,1) = NAM_U(IRRRunoffSub)
                   RRRunoff_Tnul(NStartNAM+20,irrRunoff) = RSLMAP19_RRRunoff(NStartNAM+20,IRRRunoff,1)
                   update = .true.
                endif
                allow = .true.
                Retval = Retval + GetVAR2 (STRING,' NAMlowerzonecontent ',2, ' DNAM-ReadAscii',' OPENDA file',IOUT1, &
                                              CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
                if (found) then
!                  write(*,*) ' found nam id and NAMlowerzonecontent ', Id_nod(inod)(1:len_trim(id_nod(inod))), rdum(1)
                   NAM_L(IRRRunoffSub) = Rdum(1)
                   NAM_L0(IRRRunoffSub) = Rdum(1)
                   NAM_LInitial(IRRRunoffSub) = Rdum(1)
                   RSLMAP19_RRRunoff(NStartNAM+21,IRRRunoff,1) = NAM_L(IRRRunoffSub)
                   RRRunoff_Tnul(NStartNAM+21,irrRunoff) = RSLMAP19_RRRunoff(NStartNAM+21,IRRRunoff,1)
                   update = .true.
                endif
                allow = .true.
                Retval = Retval + GetVAR2 (STRING,' NAMgroundwaterdepth ',2, ' DNAM-ReadAscii',' OPENDA file',IOUT1, &
                                              CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
                if (found) then
!                  write(*,*) ' found nam id and NAMgroundwaterdepth ', Id_nod(inod)(1:len_trim(id_nod(inod))), rdum(1)
                   NAM_GWSD(IRRRunoffSub) = Rdum(1)
                   NAM_GWSDInitial(IRRRunoffSub) = Rdum(1)
                   NAM_InitialGWDepth(IRRRunoffSub) = Rdum(1)
                   NAM_GWD0(IRRRunoffSub) = Rdum(1)
                   RSLMAP19_RRRunoff(NStartNAM+22,IRRRunoff,1) = NAM_GWSD(IRRRunoffSub)
                   RRRunoff_Tnul(NStartNAM+22,irrRunoff) = RSLMAP19_RRRunoff(NStartNAM+22,IRRRunoff,1)
                   update = .true.
                endif
               ! checks on consistent input (cf. restart file!)
! NAM_LMAX SOBEK-50506
                idum1 = D_Ifreal (NAM_LInitial(IRRRunoffSub), NAM_LMAX(IRRRunoffSub),1D-6)
                if (idum1 .gt. 0) then
!                  NAM_LInitial(IRRRunoffSub) .gt. NAM_LMAX(IRRRunoffSub)
                   call SetMessage(LEVEL_ERROR, 'Error regarding the D-NAM model input for node:'//Id_Nod(inode)(1:len_trim(Id_Nod(inode))))
                   LevelError = .true.
                   write(Iout1,'(A)')   ' Specified lower zone storage depth in OPENDA file exceeds maximum lower zone storage depth'
                   write(Iout1,'(A,F6.1,A)')  ' Lower zone storage depth from OPENDA file ', NAM_LInitial(IRRRunoffSub), ' mm'
                   write(Iout1,'(A,F6.1,A)')  ' Lower zone storage maximum depth derived from input ', NAM_LMax(IRRRunoffSub), ' mm'
                   write(Iout1,'(A,F5.3,A)')  ' root depth (SL-RZBL) ', NAM_RD(IRRRunoffSub), ' m'
                   write(Iout1,'(A)')  ' OpenDA input is corrected by limiting new initial NAM_L to maximum value'
                endif
                NAM_LInitial(IRRRunoffSub) = min (NAM_LMAX(IRRRUnoffSub), NAM_LInitial(IRRRunoffSub))
! NAM_GWSDMAX SOBEK-50507
                idum1 = D_Ifreal (NAM_GWSDInitial(IRRRunoffSub), NAM_GWSDMAX(IRRRunoffSub),1D-6)
                if (idum1 .gt. 0) then
                   ! NAM_GWSDInitial(IRRRunoffSub) .gt. NAM_GWSDMax(IRRRunoffSub)
                   call SetMessage(LEVEL_ERROR, 'Error regarding the D-NAM model input for node:'//Id_Nod(inode)(1:len_trim(Id_Nod(inode))))
                   LevelError = .true.
                   write(Iout1,'(A)')   ' Specified groundwater storage depth in OPENDA file exceeds maximum groundwater storage depth'
                   write(Iout1,'(A,F6.1,A)')  ' Groundwater storage depth from OPENDA file ', NAM_GWSDInitial(IRRRunoffSub), ' mm'
                   write(Iout1,'(A,F6.1,A)')  ' Groundwater storage maximum depth from input ', NAM_GWSDMax(IRRRUnoffSub), ' mm; based on:'
                   write(Iout1,'(A,F6.3,A)')  ' surface level (SL)) ', NAM_SurfaceLevel(IRRRunoffSub), ' m AD'
                   write(Iout1,'(A,F7.2,A)')  ' root zone bed level (RZBL)', NAM_RZBL(IRRRunoffSub), ' m AD'
                   write(Iout1,'(A,F7.2,A)')  ' groundwater bed level (GWSBL) ',NAM_GWSBL(IRRRunoffSub), ' m AD'
                   write(Iout1,'(A)')  ' OpenDA input is corrected by limiting new initial GWSD to maximum value'
                endif
                NAM_GWSDInitial(IRRRunoffSub) = min (NAM_GWSDMAX(IRRRUnoffSub), NAM_GWSDInitial(IRRRunoffSub))
            endif
        endif
     Endif
  enddo
  goto 21

150 continue
    call SetMessage(LEVEL_ERROR, 'Error reading OpenDA restart record '//String(1:len_trim(string)))
    LevelError = .true.
21  continue

    if (LevelError)  Call ErrMsgStandard (981, 0, ' Error: see earlier error messages in subroutine ', ' RRRunoffNode_ReadAscii' )
  End subroutine ReadOpenDaDNAM


  Subroutine WriteOpenDaDNAM (Infile1)

  ! write DNAM restart info to ASCII OpenDA file
  integer      Infile1, idebug

  Integer       inode, irrRunoff, IRRRunoffSub
  Character(len=1) Quote

  ! file is already opened
  iDebug = ConfFil_get_iDebug()
  quote = ''''

  do inode=1,ncnode
     irrRunoff = EiNode(inode,2)
     if (EiNode(inode,3) .eq. 31) then  ! en is DNAM
         IRRRunoffSub = RRRunoff_SubIndex(IRRRunoff)
         write(Infile1,'(A,A1,A,A1,3(1X,A,G15.8),A)') 'DNAM id ',&
                                       quote,id_nod(inode)(1:len_trim(id_nod(inode))),quote,&
                                       ' NAMupperzonecontent ', NAM_U(IRRRunoffSub), &
                                       ' NAMlowerzonecontent ', NAM_L(IRRRunoffSub), &
                                       ' NAMGroundwaterdepth ', NAM_GWSD(IRRRunoffSub), ' dnam'
     Endif
  enddo

  End subroutine WriteOpenDaDNAM



  Subroutine ReadOpenDaWalrus (Infile1, iout1, update)

  ! read Walrus restart info from ASCII OpenDA file
  integer      Infile1, iout1, idebug
  logical      update
  Integer      RetVal
  Integer      iRRRunoff, IRRRunoffSub

  Integer       inod, inode
  Integer       iecode
  Character(Len=1000) String
  Integer        Nhlp
  Parameter     (Nhlp = 32)
  Integer       IDUM(NHLP)
  Real          RDUM(NHLP)
  Character(Len=CharIdLength) CDUM(NHLP)
  Logical       allow, found, endfil
  Logical  LevelError

  LevelError = .false.
  Retval = 0

  ! file is already opened, rewind it
  Rewind(Infile1)
! update = .false.
  iDebug = ConfFil_get_iDebug()

  Call SKPCOM (Infile1, ENDFIL,'ODS')
  call SetMessage(LEVEL_INFO, 'Read OpenDA Walrus data')
  do while (.not. endfil)
    ! read OpenDA record
     Read(Infile1,'(A1000)',END=21,ERR=150,IOSTAT=IECODE)  STRING
     ! skip regel als hij niet begint met juist keyword (WALR)
     If (STRING(1:4) .EQ. 'WALR') then
      ! WALR node id
        allow = .false.
        Retval = Retval + GetVAR2 (STRING,' id ',1,' WALR-ReadAscii',' OpenDA file',IOUT1, &
                                   CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
        call fndNd2(inod, CDUM(1))
        if (inod .gt. 0) then      ! knoop bestaat in schematisatie
            inode = inod
            irrRunoff = EiNode(inod,2)
            if (EiNode(inod,3) .eq. 23 .and. RRRunoff_CompOption(IrrRunoff) .eq. 6) then  ! en is Walrus
                ! get the data
                ! update the corresponding RR variables and related variables
                IRRRunoffSub = RRRunoff_SubIndex(IRRRunoff)
                allow = .true.
                Retval = Retval + GetVAR2 (STRING,' WalrusSurfacewaterreservoirLevel ',2, ' WALR-ReadAscii',' OPENDA file',IOUT1, &
                                              CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
                if (found) then
!                  write(*,*) ' found Walrus id surfacewaterlevel ', Id_nod(inod)(1:len_trim(id_nod(inod))), rdum(1)
                   Walrus_HS0(IRRRunoffSub) = Rdum(1)
                   Walrus_HSCurrent(IRRRunoffSub) = Rdum(1)
                   RSLMAP19_RRRunoff(NStartWalrus+3,IRRRunoff,1) = Walrus_HS0(IRRRunoffSub)
                   RRRunoff_Tnul(NStartWalrus+3,irrRunoff) = RSLMAP19_RRRunoff(NStartWalrus+3,IRRRunoff,1)
                   update = .true.
                endif
                allow = .true.
                Retval = Retval + GetVAR2 (STRING,' WalrusQuickflowreservoirLevel ',2, ' WALR-ReadAscii',' OPENDA file',IOUT1, &
                                              CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
                if (found) then
!                  write(*,*) ' found Walrus id and Quickflowreservoirlevel ', Id_nod(inod)(1:len_trim(id_nod(inod))), rdum(1)
                   Walrus_HQ0(IRRRunoffSub) = Rdum(1)
                   Walrus_HQCurrent(IRRRunoffSub) = Rdum(1)
                   RSLMAP19_RRRunoff(NStartWalrus+2,IRRRunoff,1) = Walrus_HQ0(IRRRunoffSub)
                   RRRunoff_Tnul(NStartWalrus+2,irrRunoff) = RSLMAP19_RRRunoff(NStartWalrus+2,IRRRunoff,1)
                   update = .true.
                endif
                allow = .true.
                Retval = Retval + GetVAR2 (STRING,' WalrusGroundwaterdepth ',2, ' DNAM-ReadAscii',' OPENDA file',IOUT1, &
                                              CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
                if (found) then
!                  write(*,*) ' found Walrus id and Walrusgroundwaterdepth ', Id_nod(inod)(1:len_trim(id_nod(inod))), rdum(1)
                   Walrus_DG0(IRRRunoffSub) = Rdum(1)
                   Walrus_DGCurrent(IRRRunoffSub) = Rdum(1)
                   RSLMAP19_RRRunoff(NStartWalrus+1,IRRRunoff,1) = Walrus_DG0(IRRRunoffSub)
                   RRRunoff_Tnul(NStartWalrus+1,irrRunoff) = RSLMAP19_RRRunoff(NStartWalrus+3,IRRRunoff,1)
                   update = .true.
                endif
                allow = .true.
                Retval = Retval + GetVAR2 (STRING,' WalrusStorageDeficit ',2, ' WALR-ReadAscii',' OPENDA file',IOUT1, &
                                              CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
                if (found) then
!                  write(*,*) ' found Walrus id and WalrusStorageDeficit ', Id_nod(inod)(1:len_trim(id_nod(inod))), rdum(1)
                   Walrus_DV0(IRRRunoffSub) = Rdum(1)
                   Walrus_DVCurrent(IRRRunoffSub) = Rdum(1)
                   RSLMAP19_RRRunoff(NStartWalrus  ,IRRRunoff,1) = Walrus_DV0(IRRRunoffSub)
                   RRRunoff_Tnul(NStartWalrus  ,irrRunoff) = RSLMAP19_RRRunoff(NStartWalrus+3,IRRRunoff,1)
                   update = .true.
                endif
                allow = .true.
                Retval = Retval + GetVAR2 (STRING,' WalrusLastDischarge ',2, ' WALR-ReadAscii',' OPENDA file',IOUT1, &
                                              CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
                if (found) then
!                  write(*,*) ' found Walrus id and WalrusLastDischarge ', Id_nod(inod)(1:len_trim(id_nod(inod))), rdum(1)
                   Walrus_Q0(IRRRunoffSub) = Rdum(1)
                   Walrus_lastQ(IRRRunoffSub) = Rdum(1)
!                   RSLMAP19_RRRunoff(NStartWalrus  ,IRRRunoff,1) = Walrus_DV0(IRRRunoffSub)
!                   RRRunoff_Tnul(NStartWalrus  ,irrRunoff) = RSLMAP19_RRRunoff(NStartWalrus+3,IRRRunoff,1)
                   update = .true.
                endif
               ! pm checks on consistent input (cf. restart file!)
            endif
        endif
     Endif
  enddo
  goto 21

150 continue
    call SetMessage(LEVEL_ERROR, 'Error reading OpenDA restart record '//String(1:len_trim(string)))
    LevelError = .true.
21  continue

    if (LevelError)  Call ErrMsgStandard (981, 0, ' Error: see earlier error messages in subroutine ', ' RRRunoffNode_ReadAscii' )
  End subroutine ReadOpenDaWalrus




  Subroutine WriteOpenDaWalrus (Infile1)

  ! write Walrus restart info to ASCII OpenDA file
  integer      Infile1, idebug

  Integer       inode, irrRunoff, IRRRunoffSub
  Character(len=1) Quote

  ! file is already opened
  iDebug = ConfFil_get_iDebug()
  quote = ''''

  do inode=1,ncnode
     irrRunoff = EiNode(inode,2)
     if (EiNode(inode,3) .eq. 23) then  ! en is Wagmod/Walrus
         IRRRunoffSub = RRRunoff_SubIndex(IRRRunoff)
         if (RRRunoff_CompOption(IrrRunoff) .eq. 6) then
            write(Infile1,'(A,A1,A,A1,5(1X,A,G15.8),A)') 'WALR id ',&
                                       quote,id_nod(inode)(1:len_trim(id_nod(inode))),quote,&
                                       ' WalrusSurfacewaterreservoirLevel ', Walrus_HSCurrent(IRRRunoffSub), &
                                       ' WalrusQuickflowreservoirLevel ', Walrus_HQCurrent(IRRRunoffSub), &
                                       ' WalrusGroundwaterdepth ', Walrus_DGCurrent(IRRRunoffSub), &
                                       ' WalrusStorageDeficit ', Walrus_DVCurrent(IRRRunoffSub), &
                                       ' WalrusLastDischarge ', Walrus_lastQ(IRRRunoffSub), ' walr'
         endif
     Endif
  enddo

  End subroutine WriteOpenDaWalrus



  SUBROUTINE CMPRRRunoffNode (ITMSTP, IRRRunoff, IRunoff, IMETEO, IEvap, ITemperature, INODE, ILink, GlobalNAMAlfa)
    ! *****************************************************************
    ! *** Brief description:
    ! *** ------------------
    ! ***    This subroutine performs computations for RRRunoffNode nodes
    ! *********************************************************************
    ! *** Input/output parameters:
    ! *** ------------------------
    ! ***  IEVENT = event number
    ! ***  ITMSTP = timestep number
    ! ***  IDEBUG = file unit number of debug file
    ! ***  IOUT1  = file unit number of output file with run messages
    ! ***  IRRRunoff  = intern nr. RRRunoffNode node
    ! ***  IMETEO = meteostation code
    ! ***  INODE  = knoopnr
    ! ***  ILink  = link nr
    ! *********************************************************************
    ! *** Berekeningen voor RRRunoffNode model
    ! *********************************************************************

    implicit none

    Integer iTmStp, IRRRunoff, iRunoff, iMeteo, IEvap, ITemperature, iNode, ILink
    Integer iDebug, Iout1
    Integer IRRRunoffSub

    Real    QF2, Qp, Tp, GlobalNAMAlfa
    Integer IOW, i,j, k
    Integer IBND, iPluv, iConn, IBifur

    Real Alt2Temp, Threshold, Precipitation, Temperature, PotEvap, ActEvap, &
         SnowFall, Rainfall, PotSnowMelt, PotRefreezing, Refreezing, Snowmelt, &
         MaxFreeWater, InSoil, DirectRunoff, NetInSoil, Seepage, InUpperZone, &
         Percolation, BaseFlow, InterFlow, QuickFlow, HBVRunoff
    Real SCS_Runoff, PAcc, SMax, SCS_RunoffHMS

! additional local variables for NAM
    double precision NamAlfa, Lalfa, Infcap, DLMax, GPotMax, GMax, AdInf, OFDt1, OFDt2
    double precision DLBeneathLTG, DLNHPotAboveLTG, DLNHAboveLTG, InfRest, ADaboveLTG, GNH, DLgravityCorr
    double precision DeltaLInfmaxcor, fact, E2pot, CRpotmax
    double precision ADLCRt2, ZFastBF, ZSlowBF, DHFastBF, DHSlowBF
    double precision GWSDZFastBF, GWSDZSlowBF, VPotFastBF, VPotSlowBF, CKFastBF, CKSlowBF, FastBFPot,SlowBFPot
    double precision GWOutflowPot
    double precision GWOutflow, DHGwInflow, VPotGwInflow, CkGwInflow
    double precision GwAbsMax, GwPump, GwAbsPot, GwAbsAct, GwSupMax, GwSupPot, GwSupAct, GwPumpAct, GwPumpShortage
    double precision ADgwsSSGwpump, ADLGwpump, IngwsSsGWpump  , GWRestSupAct, DLGwpump, INgwsrzGWpump
    integer          LengthTable, istart
    Double precision GWTDArray(99), CapPercArray(99), Rhelp

    double precision NAMPrecipitation, NAMPotEvap
    double precision ckif
    double precision NAMRunoff
    double precision HoutSide

! additional local variables for LGSI

    double precision LGSIPrecipitation, LGSIPotEvap, LGSITotalRunoff, LGSIPreviousTotalRunoff
    double precision LGSIGwl, LGSIRunoff, LGSIDelayedRunoff
    double precision LGSIInterflow, LGSIHeadDiff
    double precision LGSIPreviousGwl(2), LGSIInitVolume(2)
    double precision x, sD, PrecipReduction, EvapReduction, StorageChange, LGSIVolume, NewGWLevel
    double precision RiverOutflow, OverlandFlow, DrainageFlow
    double precision TotalInOutflow(LGSI_MaxNrSubAreas)
    double precision TempVolume (LGSI_MaxInterpLength)
    double precision RatioArea, RatioArea1, RatioArea2
    logical          LGSIConverged
    integer          iter, ilast1
!
    Double precision  GWLevelArray(LGSI_MaxInterpLengthPlus1), GWVolumeArray (LGSI_MaxInterpLengthPlus1)
    Double precision  SeepageArray(LGSI_MaxInterpLengthPlus1), SeepageFlowArray (LGSI_MaxInterpLengthPlus1)
    Double precision  LevelDrainArray(LGSI_MaxInterpLengthPlus1), QuickFLowArray (LGSI_MaxInterpLengthPlus1)
    Double precision  OverlandFLowArray (LGSI_MaxInterpLengthPlus1), DrainageFLowArray (LGSI_MaxInterpLengthPlus1)

    double precision Fract, FractHour
    double precision TempVar, Cur_Time
! Wagmod
    double precision TempVol
! Walrus
  Integer, external :: WalrusGet, WalrusDoStep, WalrusSet
  Integer           :: retValWalrusCall, WalrusFirst
! GreenAmpt infiltration
  Double precision     DepthPonding, Fprev

      WalrusFirst = 1
      Fract     = TimeSettings%Timestepsize / 86400.     ! fraction of day
      FractHour = TimeSettings%Timestepsize / 3600.  ! fraction of hour

      iDebug = ConfFil_get_iDebug()
      iOut1  = ConfFil_get_iOut1 ()
!     Write(*,*) ' Start CmpRRRunoff', IRRRunoffSub, Itmstp

! initial situation
      IF (iDebug .ne. 0) THEN
         WRITE(IDEBUG,*) 'CMPRRRunoffNode IRRRunoff=',IRRRunoff
         WRITE(IDEBUG,*) ' RRRunoffNode Node:', Id_Nod(INODE)
         WRITE(IDEBUG,*) ' Downstream link index', ilink
         WRITE(IDEBUG,*) ' Timestep nr                 :',ITMSTP
         WRITE(IDEBUG,*) ' Timestepsize in s           :',timeSettings%timestepSize
         WRITE(IDEBUG,*) ' Node Area (m2)              :',Area_RRRunoffNode(IRRRunoff)
      ENDIF

! simulate
! par. lijst = originele par. lijst (flosf) + common blocks - de niet gebruikte variabelen
! Initialise

      IPluv= EIPluv(INODE) ! benedenstrooms een NWRW node
      IBND = EIBND(INODE)  ! benedenstrooms een rand
      IOw  = EIOW (INODE)  ! benedenstrooms een open water
      IConn = EIConn(INODE)  ! benedenstrooms een RRConnection
      IBifur= EIBifur(INODE)  ! benedenstrooms een RRBifurcation

      IRRRunoffSub = RRRunoff_SubIndex(IRRRunoff)

! Runoff
      if (RRRunoff_CompOption(iRRRunoff) .eq. 0) then
!         simple runoff node option 0: use runoff timeseries (m3/s)
          QF2 = Runoff(irunoff)
          RRRunoffNode_Outflow(iRRRunoff) = Runoff(irunoff)
!         for balance: put rainfall equal to zero

      elseif (RRRunoff_CompOption(IRRRunoff) .eq. 1) then
!         Runoff node option 1: HBV model
!           Call CMPRRRunoffNode_HBV (ITMSTP, IRRRunoffSub, IMETEO, INODE, ILink)

            if (idebug .ne. 0) then
               Write(Idebug,*) ' AAF  ', AAFNodeRainfall(inode)
               Write(Idebug,*) ' Rain ', Rain(imeteo)
               Write(Idebug,*) ' Evap ', Evap(imeteo)
               Write(Idebug,*) ' Temp ', InputTemperature(itemperature)
            Endif
          ! Initialisations
            Alt2Temp  = HBV_TempAltitudeConstant(IRRRunoffSub) * HBV_Altitude(IRRRunoffSub) / 1000.D0
            Threshold = HBV_EvapFraction(IRRRunoffSub) * HBV_FieldCapacity(IRRRunoffSub)
            Precipitation = AAFNodeRainfall(inode) * Rain(imeteo) * TimeSettings%Timestepsize / mm2m   ! in mm
            Temperature = InputTemperature(itemperature) - Alt2Temp
            PotEvap     = Evap(imeteo) * TimeSettings%Timestepsize / mm2m   ! in mm
          ! Area_RRRunoffNode(IRRRunoff) *

          ! Snowfall/Snowmelt calculations
            SnowFall = 0.0
            RainFall = 0.0
            If (Temperature .le. HBV_SnowFallTemp(IRRRunoffSub)) SnowFall = Precipitation
            If (Temperature .gt. HBV_SnowFallTemp(IRRRunoffSub)) RainFall = Precipitation

            PotSnowMelt = 0.0
            If (Temperature .gt. HBV_SnowMeltTemp(IRRRunoffSub)) &
               PotSnowMelt = HBV_MeltConst(IRRRunoffSub) * (Temperature-HBV_SnowMeltTemp(IRRRunoffSub))
            PotRefreezing = 0.0
            If (Temperature .le. HBV_SnowMeltTemp(IRRRunoffSub)) &
               PotRefreezing = HBV_MeltConst(IRRRunoffSub) * HBV_FreezEff(IRRRunoffSub) * (HBV_SnowMeltTemp(IRRRunoffSub)-Temperature)
            Refreezing = 0.0
            If (Temperature .lt. HBV_SnowMeltTemp(IRRRunoffSub)) &
               Refreezing = min (PotRefreezing, HBV_FreeWaterContent0(IRRRunoffSub))

            SnowMelt = min (PotSnowMelt, HBV_DrySnowContent0(IRRRunoffSub))
            HBV_DrySnowContent(IRRRunoffSub) = HBV_DrySnowContent0(IRRRunoffSub) + SnowFall + Refreezing - SnowMelt
            MaxFreeWater = HBV_DrySnowContent(IRRRunoffSub)* HBV_FreeWaterFraction(IRRRunoffSub)
            HBV_FreeWaterContent(IRRRunoffSub) = HBV_FreeWaterContent0(IRRRunoffSub) - Refreezing + Snowmelt + Rainfall
            InSoil = max (HBV_FreeWaterContent(IRRRunoffSub)-MaxFreeWater, 0.0)
            HBV_FreeWaterContent(IRRRunoffSub) = HBV_FreeWaterContent(IRRRunoffSub) - InSoil

          ! Soilbucket calculations
            HBV_SoilMoisture(IRRRunoffSub) = HBV_SoilMoisture0(IRRRunoffSub) + InSoil
            DirectRunoff = max (HBV_SoilMoisture(IRRRunoffSub)-HBV_FieldCapacity(IRRRunoffSub),0.0)
            HBV_SoilMoisture(IRRRunoffSub) = HBV_SoilMoisture(IRRRunoffSub) - DirectRunoff
            NetInSoil = InSoil - DirectRunoff

            Seepage = ((HBV_SoilMoisture(IRRRunoffSub)/HBV_FieldCapacity(IRRRunoffSub))**HBV_Beta(IRRRunoffSub))*NetInSoil
            HBV_SoilMoisture(IRRRunoffSub) = HBV_SoilMoisture(IRRRunoffSub) - Seepage
            InUpperZone = DirectRunoff + Seepage

            ActEvap = PotEvap
            if (HBV_SoilMoisture(iRRRunoffSub) .le. Threshold) ActEvap = PotEvap*HBV_SoilMoisture(IRRRunoffSub)/Threshold;
            ActEvap = min (ActEvap, HBV_SoilMoisture(IRRRunoffSub))
            HBV_SoilMoisture(IRRRunoffSub) = HBV_SoilMoisture(IRRRunoffSub) - ActEvap

          ! Lower zone calculations
            Percolation = min (HBV_MaxPercolation(IRRRunoffSub),InUpperZone)
            InUpperZone = InUpperZone - Percolation

            HBV_LowerZoneContent(IRRRunoffSub) = HBV_LowerZoneContent0(IRRRunoffSub) + Percolation
            BaseFlow = HBV_KBaseFlow(iRRRunoffSub) * HBV_LowerZoneContent(IRRRunoffSub)
            HBV_LowerZoneContent(IRRRunoffSub) = HBV_LowerZoneContent (IRRRunoffSub) - BaseFlow

          ! Upper zone calculations
            HBV_UpperZoneContent(IRRRunoffSub) = HBV_UpperZoneContent0(IRRRunoffSub) + InUpperZone
            QuickFlow = 0.0
            if (HBV_UpperZoneContent(iRRRunoffSub) .gt. HBV_QuickThreshold(IRRRunoffSub)) &
               QuickFlow = HBV_KQuickFlow(iRRRunoffSub) * (HBV_UpperZoneContent(IRRRunoffSub) - HBV_QuickThreshold(IRRRunoffSub))
            HBV_UpperZoneContent(IRRRunoffSub) = HBV_UpperZoneContent(IRRRunoffSub) - QuickFlow

            InterFlow = HBV_KInterFlow(iRRRunoffSub) * min(HBV_UpperZoneContent(IRRRunoffSub), HBV_QuickThreshold(IRRRunoffSub))
            HBV_UpperZoneContent(IRRRunoffSub) = HBV_UpperZoneContent(IRRRunoffSub) - InterFlow

          ! Runoff calculations

            HBVRunoff = BaseFlow + InterFlow + QuickFlow
            HBV_QRunoffInmm(IRRRunoffSub) = HBVRunoff

            HBV_Temperature (IRRRunoffSub) = Temperature
            HBV_Rainfall    (IRRRunoffSub) = Rainfall
            HBV_Snowfall    (IRRRunoffSub) = Snowfall
            HBV_PotEvap     (IRRRunoffSub) = PotEvap
            HBV_ActEvap     (IRRRunoffSub) = ActEvap
            HBV_BaseFlow    (IRRRunoffSub) = BaseFlow
            HBV_InterFlow   (IRRRunoffSub) = InterFlow
            HBV_QuickFlow   (IRRRunoffSub) = QuickFlow
            HBV_Snowmelt    (IRRRunoffSub) = Snowmelt
            HBV_Refreezing  (IRRRunoffSub) = Refreezing
            HBV_Infiltration(IRRRunoffSub) = InSoil
            HBV_Seepage     (IRRRunoffSub) = Seepage
            HBV_DirectRunoff(IRRRunoffSub) = DirectRunoff
            HBV_Percolation (IRRRunoffSub) = Percolation
! not needed HBV_InUpperZone (IRRRunoffSub) = InUpperZone
            RRRunoffNode_Outflow(iRRRunoff) = HBVRunoff * Area_RRRunoffNode(IRRRunoff) * mm2m / timeSettings%TimestepSize

            IF (iDebug .ne. 0) THEN
               WRITE(IDEBUG,*) 'CMPRRRunoffNode HBV IRRRunoff=',IRRRunoff
               WRITE(IDEBUG,*) ' RRRunoffNode Node:', Id_Nod(INODE)
               WRITE(IDEBUG,*) ' Timestep nr                 :',ITMSTP
               WRITE(IDEBUG,*) ' Timestepsize in s           :',timeSettings%timestepSize
               WRITE(IDEBUG,*) ' Node Area (m2)              :',Area_RRRunoffNode(IRRRunoff)
               WRITE(IDEBUG,*) ' RRRunoff_CompOption (1=HBV) :',RRRunoff_CompOption(IRRRunoff)
               WRITE(IDEBUG,*) ' Temperature                 :',Temperature
               WRITE(IDEBUG,*) ' Precipitation (mm)          :',Precipitation
               WRITE(IDEBUG,*) ' Rainfall  (mm)              :',Rainfall
               WRITE(IDEBUG,*) ' Snowfall  (mm)              :',Snowfall
               WRITE(IDEBUG,*) ' DirectRunoff                :',DirectRunoff
               WRITE(IDEBUG,*) ' InSoil                      :',InSoil
               WRITE(IDEBUG,*) ' NetInSoil                   :',NetInSoil
               WRITE(IDEBUG,*) ' InUpperZone                 :',InUpperZone
               WRITE(IDEBUG,*) ' Seepage                     :',Seepage
               WRITE(IDEBUG,*) ' Percolation                 :',Percolation
               WRITE(IDEBUG,*) ' PotEvap   (mm)              :',PotEvap
               WRITE(IDEBUG,*) ' ActEvap   (mm)              :',ActEvap
               WRITE(IDEBUG,*) ' Base Flow (mm)              :',BaseFlow
               WRITE(IDEBUG,*) ' InterFlow (mm)              :',InterFlow
               WRITE(IDEBUG,*) ' QuickFlow (mm)              :',QuickFlow
               WRITE(IDEBUG,*) ' RunoffFlow (mm)             :',HBVRunoff
               WRITE(IDEBUG,*) ' Initial Contents'
               WRITE(IDEBUG,*) '   Snow        (mm)          :',HBV_DrySnowContent0(IRRRunoffSub)
               WRITE(IDEBUG,*) '   FreeWater   (mm)          :',HBV_FreeWaterContent0(IRRRunoffSub)
               WRITE(IDEBUG,*) '   SoilMoisture(mm)          :',HBV_SoilMoisture0(IRRRunoffSub)
               WRITE(IDEBUG,*) '   UpperZone   (mm)          :',HBV_UpperZoneContent0(IRRRunoffSub)
               WRITE(IDEBUG,*) '   LowerZone   (mm)          :',HBV_LowerZoneContent0(IRRRunoffSub)
               WRITE(IDEBUG,*) ' Final Contents  '
               WRITE(IDEBUG,*) '   Snow        (mm)          :',HBV_DrySnowContent(IRRRunoffSub)
               WRITE(IDEBUG,*) '   FreeWater   (mm)          :',HBV_FreeWaterContent(IRRRunoffSub)
               WRITE(IDEBUG,*) '   SoilMoisture(mm)          :',HBV_SoilMoisture(IRRRunoffSub)
               WRITE(IDEBUG,*) '   UpperZone   (mm)          :',HBV_UpperZoneContent(IRRRunoffSub)
               WRITE(IDEBUG,*) '   LowerZone   (mm)          :',HBV_LowerZoneContent(IRRRunoffSub)
            ENDIF

            QF2 = HBVRunoff * Area_RRRunoffNode(IRRRunoff) * mm2m / timeSettings%TimestepSize

      elseif (RRRunoff_CompOption(IRRRunoff) .eq. 2) then
!         Runoff node option 2: SCS method
            if (idebug .ne. 0) then
               Write(Idebug,*) ' Rain (m3/s) ', Rain(imeteo)
               Write(Idebug,*) ' Evap (m3/s) ', Evap(imeteo)
               Write(Idebug,*) ' Slope (m/m) ', SCS_Slope(iRRRunoffSub)
               Write(Idebug,*) ' Length (m)  ', SCS_Length(iRRRunoffSub)
               Write(Idebug,*) ' CurvNr', SCS_CurveNumber(iRRRunoffSub)
               Write(Idebug,*) ' Smax (mm) ', SCS_MaxRetention(iRRRunoffSub)
               Write(Idebug,*) ' TLag in hours ', SCS_TLag(iRRRunoffSub)
               Write(Idebug,*) ' Tc (nr. timesteps) ', SCS_Tc(iRRRunoffSub)
            Endif
          ! Initialisations

            If (SCS_UseGreenAmpt_Infiltration(IRRRunoffSub)) then
               Precipitation = AAFNodeRainfall(inode) * Rain(imeteo) * TimeSettings%Timestepsize / mm2m   ! in mm
               DepthPonding  = SCS_Storage0(IRRRunoffSub)
               FPrev = SCS_GreenAmpt_CumInfiltration(IRRRunoffSub)
               Call GreenAmpt (Precipitation/FractHour*1.D0, DepthPonding, SCS_GreenAmpt_Theta_D(IRRRunoffSub), SCS_GreenAmpt_Theta_DU(IRRRunoffSub), &
                               SCS_GreenAmpt_CumInfiltration(IRRRunoffSub), SCS_GreenAmpt_T(IRRRunoffSub), &
                               SCS_GreenAmpt_KSat(IRRRunoffSub), SCS_GreenAmpt_Psi(IRRRunoffSub), SCS_GreenAmpt_Theta_Dmax(IRRRunoffSub), &
                               SCS_GreenAmpt_LU(IRRRunoffSub), SCS_GreenAmpt_Kr(IRRRunoffSub), SCS_GreenAmpt_Tr(IRRRunoffSub), &
                               SCS_GreenAmpt_InfRate(IRRRunoffSub), Idebug, TimeSettings%Timestepsize )
               SCS_GreenAmpt_CumRain(IRRRunoffSub) = SCS_GreenAmpt_CumRain(IRRRUnoffSub) + Precipitation
               SCS_GreenAmpt_InfCurrentStep (IRRRunoffSub) = max (0.0D0, SCS_GreenAmpt_CumInfiltration(IRRRunoffSub)-FPrev)
               if (idebug .gt. 0) then
                  Write(Idebug,*) ' itmstp  RainRate RainStep CumRain InfRate Inf_currentstep CumInf Theta_DU  T'
                  Write(Idebug,'(I5,10F10.4)') itmstp,Precipitation/FractHour, SCS_GreenAmpt_CumRain(IRRRunoffSub), &
                                        SCS_GreenAmpt_InfRate(IRRRunoffSub), SCS_GreenAmpt_InfCurrentStep (IRRRunoffSub), &
                                        SCS_GreenAmpt_CumInfiltration(IRRRunoffSub), SCS_GreenAmpt_Theta_DU(IRRRunoffSub), SCS_GreenAmpt_T(IRRRunoffSub)
               endif
            Endif

            If (.not. SCS_UseBaseFlow(IRRRunoffSub)) then
               ! original SCS calculations, without baseflow extensions
               Precipitation = AAFNodeRainfall(inode) * Rain(imeteo) * TimeSettings%Timestepsize / mm2m   ! in mm
               SCS_Rainfall(IRRRunoffSub)= Precipitation
               ! subtract Infiltration already from PAccum (and PExcess)
               SCS_PAccum (IRRRunoffSub) = SCS_PAccum0(IRRRunoffSub) + Precipitation - SCS_GreenAmpt_InfCurrentStep(IRRRunoffSub)
               PAcc = SCS_PAccum(IRRRunoffSub)
               SMax = SCS_MaxRetention(IRRRunoffSub)
               if (PAcc + 0.8 * SMax .gt. 0) then
                  SCS_PExcess(IRRRunoffSub) = ( max (0.0, PAcc - 0.2*SMax) **2) / (PAcc + 0.8 * SMax)
               else
                  SCS_PExcess(IRRRunoffSub) = 0.
               endif

             ! available runoff current timestep
             ! also subtract infiltration current timestep   (discussion point is whether to subtract it from available runoff or already from PExcess .....
               SCS_AvailableRunoff(IRRRunoffSub,1) = SCS_PExcess(IRRRunoffSub) - SCS_PExcess0(IRRRunoffSub) ! - SCS_GreenAmpt_InfCurrentStep(IRRRunoffSub)  ! subtracted infiltration current timestep
               SCS_AvailableRunoff(IRRRunoffSub,1) = max (0.0, SCS_AvailableRunoff(IRRRunoffSub,1))

               If (SCS_UHChosen(IRRRunoffSub) .eq. 0 .or. SCS_UHChosen(IRRRunoffSub) .eq. 2) then
                 ! routing using Clarke unit hydrograph - HEC-HMS or Snyder
                  SCS_Runoff = calculate_base_flow(SCS_Snyder_BF_STRTQ(IRRRunoffSub),TimeSettings%TimestepSize*itmstp/3600.,SCS_Snyder_BF_decay_rate(IRRRunoffSub),SCS_Snyder_BF_interpolation_method(IRRRunoffSub))
                  Do j=1,Ceiling (SCS_Tc(IRRRunoffSub)) + 1
                     SCS_Runoff = SCS_Runoff + SCS_UnitHydComp(j,IRRRunoffSub) * SCS_AvailableRunoff(IRRRunoffSub,j)
                  Enddo
                  SCS_Storage(IRRRunoffSub) = SCS_Storage0(IRRRunoffSub) + Precipitation - SCS_Runoff
                  QF2 = SCS_Runoff * Area_RRRunoffNode(IRRRunoff) * mm2m / timeSettings%TimestepSize

                  ! adjustment in case of use HMS UH with linear reservoir (so far only the time-area diagram is taken into account)
                  if (SCS_UHChosen(IRRRunoffSub) .eq. 0  .and. SCS_HMSLinResR(iRRRunoffSub) .gt. 0) then
                      if (idebug .ne. 0) then
                         Write(Idebug,*) ' HMS LinResR', SCS_HMSLinResR(IRRRunoffSub)
                         Write(Idebug,*) ' SCS_AvailableRunoff', (SCS_AvailableRunoff(IRRRunoffSub,j),j=1,MaxTc)
                         Write(Idebug,*) ' SCS_UnitHydComp', (SCS_UnitHydComp(j,IRRRunoffSub),j=1,Ceiling (SCS_Tc(IRRRunoffSub)) + 1)
                         Write(Idebug,*) ' SCSRunoff     ', SCS_Runoff
                         Write(Idebug,*) ' SCS_HMSC1     ', SCS_HMSC1(IRRRunoffSub)
                         Write(Idebug,*) ' SCS_HMSC2     ', SCS_HMSC2(IRRRunoffSub)
                         Write(Idebug,*) ' SCS_HMSLinResContent0 ', SCS_HMSLinResContent0(IRRRunoffSub)
                      Endif
                      Do j=1,Ceiling (SCS_Tc(IRRRunoffSub)) + 1
                         SCS_HMSLinResInflow(j) = SCS_UnitHydComp(j,IRRRunoffSub) * SCS_AvailableRunoff(IRRRunoffSub,j)
                         SCS_HMSLinResInflowTot(IRRRunoffSub) = SCS_HMSLinResInflowTot(IRRRunoffSub) + SCS_HMSLinResInflow(j)
                         SCS_HMSLinResOutflow(IRRRunoffSub)= SCS_HMSLinResOutflow(IRRRunoffSub) + SCS_HMSC1(IRRRunoffSub) * SCS_HMSLinResInflow(j)
                      Enddo
                      SCS_HMSLinResOutflow(IRRRunoffSub)= SCS_HMSLinResOutflow(IRRRunoffSub) + SCS_HMSC2(IRRRunoffSub) * SCS_HMSLinResOutflow0(IRRRunoffSub)
                      SCS_HMSLinResContent(IRRRunoffSub)= SCS_HMSLinResContent(IRRRunoffSub) + SCS_HMSLinResInflowTot(IRRRunoffSub) - SCS_HMSLinResOutflow(IRRRunoffSub)
                      if (idebug .ne. 0) then
                         Write(Idebug,*) ' SCS_HMSLinResInflowTot    ', SCS_HMSLinResInflowTot(IRRRunoffSub)
                         Write(Idebug,*) ' SCS_HMSLinResOutflow      ', SCS_HMSLinResOutflow(IRRRunoffSub)
                         Write(Idebug,*) ' SCS_HMSLinResContentFinal ', SCS_HMSLinResContent(IRRRunoffSub)
                      Endif
                      SCS_RunoffHMS = SCS_HMSLinResOutflow(IRRRunoffSub)
                      QF2 = SCS_RunoffHMS * Area_RRRunoffNode(IRRRunoff) * mm2m / timeSettings%TimestepSize
                  endif
               else If (SCS_UHChosen(IRRRunoffSub) .eq. 1) then
                 ! routing using SCS dimensionless unit hydrograph
                  Tp = SCS_Tlag(iRRRunoffSub) + (TimeSettings%TimestepSize /2. / 3600.)
                  if (idebug .ne. 0) then
                     Write(Idebug,*) ' SCS dimensionless UH used'
                     Write(Idebug,*) ' Tp (hours) ', Tp
                  endif
                  QF2 = 0.0
                  Do j=1,Ceiling (5 * Tp * 3600. / TimeSettings%TimestepSize) + 1
                     Qp = 0.208 * Area_RRRunoffNode(IRRRunoff) / 1000000. / Tp * SCS_AvailableRunoff(IRRRunoffSub,j)
                     QF2= QF2 + SCS_UnitHydComp(j,IRRRunoffSub) * Qp
                     if (idebug .ne. 0) then
                        Write(Idebug,'(A,I4,1X,3(E12.5,1X))') ' component, UH, Qp(m3/s), cum. QF2(m3/s) ', j, SCS_UnitHydComp(j,IRRRunoffSub), Qp, QF2
                     endif
                  Enddo
                  SCS_Runoff = QF2 / Area_RRRunoffNode(IRRRunoff) / mm2m * TimeSettings%TimeStepSize
                  SCS_Storage(IRRRunoffSub) = SCS_Storage0(IRRRunoffSub) + Precipitation - SCS_Runoff
               endif

             ! storage balance and outflow
! already above
!              SCS_Storage(IRRRunoffSub) = SCS_Storage0(IRRRunoffSub) + Precipitation - SCS_Runoff
               if (SCS_UseGreenAmpt_Infiltration(IRRRunoffSub)) then
                  SCS_Storage(IRRRUnoffSub) = SCS_Storage(IRRRUnoffSub) - SCS_GreenAmpt_InfCurrentStep(IRRRunoffSub)
               endif
               RRRunoffNode_Outflow(iRRRunoff) = QF2

            else
             ! with baseflow extensions;  do not use since not complete / not tested
               Precipitation = AAFNodeRainfall(inode) * Rain(imeteo) * TimeSettings%Timestepsize / mm2m   ! in mm
               SCS_Rainfall(IRRRunoffSub)= Precipitation
               SCS_EvapRD(IRRRunoffSub)= Evap(imeteo) * TimeSettings%Timestepsize / mm2m   ! in mm
               SCS_PAccum (IRRRunoffSub) = SCS_PAccum0(IRRRunoffSub) + Precipitation
               PAcc = SCS_PAccum(IRRRunoffSub)
               SMax = SCS_MaxRetention(IRRRunoffSub)
               if (PAcc + 0.8 * SMax .gt. 0) then
                  SCS_PExcess(IRRRunoffSub) = ( max (0.0, PAcc - 0.2*SMax) **2) / (PAcc + 0.8 * SMax)
               else
                  SCS_PExcess(IRRRunoffSub) = 0.
               endif

             ! available runoff current timestep
!              SCS_AvailableRunoff(IRRRunoffSub,1) = SCS_PExcess(IRRRunoffSub) - SCS_PExcess0(IRRRunoffSub)
               SCS_AvailableRunoff(IRRRunoffSub,1) = SCS_PExcess(IRRRunoffSub) - SCS_PExcess0(IRRRunoffSub) !- SCS_GreenAmpt_InfCurrentStep(IRRRunoffSub)  ! subtracted infiltration current timestep
               SCS_AvailableRunoff(IRRRunoffSub,1) = max (0.0, SCS_AvailableRunoff(IRRRunoffSub,1))

            endif

            if (idebug .ne. 0) then
               Write(Idebug,*) ' Precipitation ', Precipitation
               Write(Idebug,*) ' SCS_AvailableRunoff', (SCS_AvailableRunoff(IRRRunoffSub,j),j=1,MaxTc)
               Write(Idebug,*) ' SCS_UnitHydComp', (SCS_UnitHydComp(j,IRRRunoffSub),j=1,maxTc)
               Write(Idebug,*) ' Paccum  init  ', SCS_PAccum0(IRRRunoffSub)
               Write(Idebug,*) ' Paccum        ', SCS_PAccum (IRRRunoffSub)
               Write(Idebug,*) ' Pexcess init  ', SCS_PExcess0(IRRRunoffSub)
               Write(Idebug,*) ' Pexcess       ', SCS_PExcess (IRRRunoffSub)
               Write(Idebug,*) ' SCSRunoff     ', SCS_Runoff
               Write(Idebug,*) ' Storage init  ', SCS_Storage0(IRRRunoffSub)
               Write(Idebug,*) ' Storage       ', SCS_Storage (IRRRunoffSub)
               Write(Idebug,*) ' QF2           ', QF2
               Write(Idebug,*) ' UseGreenAmpt  ', SCS_UseGreenAmpt_Infiltration(IRRRunoffSub)
               Write(Idebug,*) ' InfCurrentStep', SCS_GreenAmpt_InfCurrentStep(IRRRunoffSub)
            Endif

      elseif (RRRunoff_CompOption(IRRRunoff) .eq. 3) then
!         Runoff node option 3: NAM method
            if (idebug .ne. 0) then
               Write(Idebug,*) ' Rain (m3/s) ', Rain(imeteo)
               Write(Idebug,*) ' Evap (m3/s) ', Evap(imeteo)
            Endif
          ! Initialisations
          ! Note: depletion coefficients adjusted from 1/day to 1/deltat
          !      DUZ =1.-(1.-DCUZ )**FRACT    met dcuz in 1/day, duz het te gebruiken getal in 1/deltat
          !      voor dcuz en duz in dagen:
          !      DUZ =1./ (  1.-(1.-1./DCUZ )**FRACT )
            NAMPrecipitation = AAFNodeRainfall(inode) * Rain(imeteo) * TimeSettings%Timestepsize / mm2m   ! in mm
            NAMPotEvap     = Evap(imeteo) * TimeSettings%Timestepsize / mm2m   ! in mm
            if (idebug .ne. 0) write(Idebug,*) ' NAMPrecipitation', NAMPrecipitation
            if (idebug .ne. 0) write(Idebug,*) ' NAMPotEvap      ', NAMPotEvap
           ! new
            ! eq. 6.1
            NAM_U(IRRRunoffSub) = NAM_UInitial(IRRRunoffSub) + NamPrecipitation
            if (idebug .ne. 0) write(Idebug,*) ' 6.1  U', NAM_U(IRRRunoffSub)
            ! eq. 6.2
            NAM_E1 (IRRRUnoffSub) = min (NamPotEvap, NAM_U(IRRRunoffSub))
            if (idebug .ne. 0) write(Idebug,*) ' 6.2  E1', NAM_E1(IRRRunoffSub)
            ! eq. 6.3
            NAM_U(IRRRUnoffSub) = NAM_U(IRRRunoffSub) - NAM_E1(IRRRunoffSub)
            if (idebug .ne. 0) write(Idebug,*) ' 6.3  U', NAM_U(IRRRunoffSub)
            ! eq. 6.4
            ckif           = 1.D0 / (1.D0 -(1.D0-(1.D0/NAM_CKIF(iRRRunoffSub)))**Fract)
            ! eq. 6.5
            if (NAM_LInitial(IRRRunoffSub) .le. NAM_LTif(IRRRunoffSub)) then
               Lalfa = 0
            elseif (NAM_Lmax(IRRRunoffSub) .gt. NAM_LTif(IRRRunoffSub)) then
               Lalfa = (NAM_LInitial(IRRRunoffSub)- NAM_LTif(IRRRunoffSub)) / (NAM_LMax(IRRRunoffSub)- NAM_LTif(IRRRunoffSub))
            else  ! Lmax = Ltif = Linitial
               Lalfa = 1
            endif
            if (idebug .ne. 0) write(Idebug,*) ' 6.5 Lalfa', Lalfa
            ! eq. 6.6
            NAM_IF(IRRRunoffSub) = Lalfa * max (0.0d0, NAM_U(IRRRunoffSub) - NAM_UTif(IRRRUnoffSub)) / ckif
            if (idebug .ne. 0) write(Idebug,*) ' 6.6 IF', NAM_IF(IRRRunoffSub)
            ! eq. 6.7
            NAM_U(IRRRUnoffSub) = NAM_U(IRRRUnoffSub) - NAM_IF(IRRRunoffSub)
            if (idebug .ne. 0) write(Idebug,*) ' 6.7 U', NAM_U(IRRRunoffSub)
            ! eq. 6.8
            Infcap = NAM_InfCap(IRRRUnoffSub) * FractHour
            if (idebug .ne. 0) write(Idebug,*) ' 6.8 Infcap', InfCap
            ! eq. 6.9
            NAM_GWSDSS (iRRRunoffSub) = min (NAM_GWSD(IRRRunoffSub), NAM_GWSDSSMax(IRRRunoffSub))
            if (idebug .ne. 0) write(Idebug,*) ' 6.9  GWSDSS', NAM_GWSDSS(IRRRunoffSub)
            ! eq. 6.10
            NAM_GWSDRZ (iRRRunoffSub) = max (0.D0, NAM_GWSD(IRRRunoffSub) - NAM_GWSDSSMax(IRRRunoffSub))
            if (idebug .ne. 0) write(Idebug,*) ' 6.10 GWSDRZ', NAM_GWSDRZ(IRRRunoffSub)
            ! eq. 6.11
            NAMAlfa = GlobalNAMAlfa
            if (NAM_GWSDRZ(IRRRunoffSub) .le. 0.D0) then
               NAM_GWL(IRRRunoffSub) = NAM_GWSBL(IRRRunoffSub) + ( NAM_GWSDSS(IRRRunoffSub) / (1000.D0 * NAM_SYSS(IRRRunoffSub)) )
            elseif (NAM_GWSDRZ(IRRRunoffSub) .gt. 0.D0 .and.  NAM_GWSDRZ(IRRRunoffSub) .lt. NAMAlfa * NAM_GWSDRZMax(IRRRunoff)) then
               Fact =  NAM_GWSDRZ(IRRRunoffSub) / NAMalfa / NAM_GWSDRZMax(IRRRunoffSub)
               NAM_GWL(IRRRunoffSub) = NAM_GWSBL(IRRRunoffSub) + ( NAM_GWSDSS(IRRRunoffSub) / (1000.D0 * NAM_SYSS(IRRRunoffSub)) ) + &
                                                          ( NAM_GWSDRZ(IRRRunoffSub) + NAM_LInitial(IRRRunoffSub) * Fact ) / (1000.D0 * NAM_SYRZ(IRRRunoffSub))
            elseif (NAM_GWSDRZ(IRRRunoffSub) .ge. NAMAlfa * NAM_GWSDRZMax(IRRRunoff)) then
               NAM_GWL(IRRRunoffSub) = NAM_GWSBL(IRRRunoffSub) + ( NAM_GWSDSS(IRRRunoffSub) / (1000.D0 * NAM_SYSS(IRRRunoffSub)) ) + &
                                                                 ( NAM_GWSDRZ(IRRRunoffSub) + NAM_LInitial(IRRRunoffSub) ) / (1000.D0 * NAM_SYRZ(IRRRunoffSub))
            endif
            ! eq. 6.12
             NAM_GWTD(IRRRunoffSub) = max (0.0d0, NAM_SurfaceLevel(IRRRunoffSub) - NAM_GWL(IRRRunoffSub))
             if (idebug .ne. 0) write(Idebug,*) ' 6.12 GWTD', NAM_GWTD(IRRRunoffSub)
            ! eq. 6.13
            if (NAM_PercOpt(IRRRunoffSub) .eq. 1) then
               istart = NAM_PercTableStart (iRRRunoffSub)
               LengthTable = NAM_PercTableEnd(iRRRunoffSub) - NAM_PercTableStart(iRRRunoffSub) + 1
               Do k=1,LengthTable
                  GWTDArray(k)    = NAM_CapRisPercTableGWTD(k+istart-1)
                  CapPercArray(k) = NAM_CapRisPercTableCapPerc(k+istart-1)
               Enddo
               Call RR_D_INTERP  (LengthTable, GWTDArray, CapPercArray, NAM_GWTD(IRRRunoffSub), GPotMax, istart)
               Gpotmax = Fract * GPotMax
            else
               GPotMax = Fract * NAM_PercConst(IRRRunoffSub)
            endif
            if (idebug .ne. 0) write(Idebug,*) ' 6.13 GpotMax', GPotMax
            ! eq. 6.14
            DLMax = NAM_LMax(IRRRUnoffSub) - NAM_LInitial(IRRRunoffSub)
            if (idebug .ne. 0) write(Idebug,*) ' 6.14 DLMax', DLMax
            ! eq. 6.15
            GMax  = min (NAM_GWSDmax(IRRRUnoffSub) - NAM_GWSD(IRRRunoffSub), GPotMax)
            if (idebug .ne. 0) write(Idebug,*) ' 6.15 GMax', GMax
            ! eq. 6.16
            ADInf  = DLMax + GMax
            if (idebug .ne. 0) write(Idebug,*) ' 6.16 AdInf', AdInf
            ! eq. 6.17
            NAM_INF(IRRRunoffSub) = min (NAM_U(IRRRunoffSub), InfCap, AdInf)
            if (idebug .ne. 0) write(Idebug,*) ' 6.17 INF', NAM_INF(IRRRunoffSub)
            ! eq. 6.18
            NAM_U(IRRRUnoffSub) = NAM_U(IRRRunoffSub) - NAM_INF(IRRRunoffSub)
            if (idebug .ne. 0) write(Idebug,*) ' 6.18 U', NAM_U(IRRRunoffSub)
            ! eq. 6.19
            OFDt1 = max (0.0d0, NAM_U(IRRRunoffSub) - NAM_UTOF(IRRRunoffSub))
            if (idebug .ne. 0) write(Idebug,*) ' 6.19 OFDt1', OFDt1
            ! eq. 6.20
            if (OFDt1 .le. 0) then
                OFDt2 = 0.0
            else
                OFDt2 = 2. * Sqrt(NAM_SurfaceSlope(IRRRunoffSub)) * TimeSettings%TimestepSize / &
                         (3.*NAM_Manning_n(IRRRunoffSub) * NAM_CatchmentLength(IRRRUnoffSub) )
                OFDt2 = 1000.D0 * ( (OFDt1/1000.D0)**(-2.D0/3.) + OFDt2) ** (-3.D0/2.)
            endif
            if (idebug .ne. 0) write(Idebug,*) ' 6.20 OFDt2', OFDt2
            ! eq. 6.21
            NAM_OF(IRRRunoffSub) = OFDt1 - OFDt2
            if (idebug .ne. 0) write(Idebug,*) ' 6.21 OF', NAM_OF(IRRRunoffSub)
            ! eq. 6.22
            NAM_U(IRRRUnoffSub) = NAM_U(IRRRUnoffSub) - NAM_OF(IRRRunoffSub)
            if (idebug .ne. 0) write(Idebug,*) ' 6.22 U', NAM_U(IRRRunoffSub)
            ! eq. 6.23
            if (NAM_LInitial(IRRRUnoffSub) .ge. NAM_LTG(IRRRUnoffSub)) then
                DLbeneathLTG = 0.0
            else
                DLbeneathLTG = min (NAM_LTG(IRRRunoffSub) - NAM_LInitial(IRRRunoffSub), NAM_INF(IRRRunoffSub))
            endif
            if (idebug .ne. 0) write(Idebug,*) ' 6.23 DLBeneathLTG ', DLBeneathLTG
            ! eq. 6.24
            INFRest = NAM_INF(IRRRunoffSub) - DLBeneathLTG
            if (idebug .ne. 0) write(Idebug,*) ' 6.24 InfRest ', InfRest
            ! eq. 6.25
            If (NAM_LInitial(IRRRunoffSub) .le. NAM_LTG(IRRRunoffSub)) then
               ADAboveLTG = NAM_LMax(IRRRunoffSub) - NAM_LTG(IRRRunoffSub)
            elseif (NAM_LInitial(IRRRunoffSub) .lt. NAM_LMax(IRRRunoffSub)) then
               ADAboveLTG = NAM_LMax(IRRRunoffSub) - NAM_LInitial(IRRRunoffSub)
            else
               ADAboveLTG = 0.0
            endif
            if (idebug .ne. 0) write(Idebug,*) ' 6.25 AdAboveLTG ', AdAboveLTG
            ! eq. 6.26
            DLNHpotAboveLTG = (1 - (NAM_L(iRRRunoffSub) / NAM_LMax(IRRRUnoffSub)) ) * INFrest
            if (idebug .ne. 0) write(Idebug,*) ' 6.26 DLNHPotAboveLTG ', DLNHpotAboveLTG
            ! not needed: GNHpot = (NAM_L(iRRRunoffSub) / NAM_LMax(IRRRUnoffSub) ) * INFrest
            ! eq. 6.27
            DLNHAboveLTG = min (DLNHpotAboveLTG, ADAboveLTG)
            if (idebug .ne. 0) write(Idebug,*) ' 6.27 DLNHAboveLTG ', DLNHAboveLTG
            ! eq. 6.28
            GNH = NAM_INF(iRRRunoffSub) - DLBeneathLTG - DLNHAboveLTG
            if (idebug .ne. 0) write(Idebug,*) ' 6.28 GNH ', GNH
            ! eq. 6.29
            NAM_L (iRRRunoffSub) = NAM_LInitial(IRRRunoffSub) + DLBeneathLTG + DLNHAboveLTG
            if (idebug .ne. 0) write(Idebug,*) ' 6.29 L', NAM_L(IRRRunoffSub)
            ! eq. 6.30
            NAM_GWSD (iRRRunoffSub) = NAM_GWSDInitial(IRRRunoffSub) + GNH
            ! eq. 6.31
            DeltaLInfmaxcor = max (0.0D0, GNH - GPotmax)
            if (idebug .ne. 0) write(Idebug,*) ' 6.31 DeltaLInfmaxcor ', DeltaLInfmaxcor
            ! eq. 6.32
            NAM_DL(IRRRunoffSub) = DLBeneathLTG + DLNHAboveLTG + DeltaLInfmaxcor
            if (idebug .ne. 0) write(Idebug,*) ' 6.32 DL', NAM_DL(IRRRunoffSub)
            ! eq. 6.33
            NAM_L(IRRRunoffSub) = NAM_L(IRRRunoffSub) + DeltaLInfmaxcor
            if (idebug .ne. 0) write(Idebug,*) ' 6.33 L', NAM_L(IRRRunoffSub)
            ! eq. 6.34
            NAM_G(IRRRunoffSub) = GNH - DeltaLInfmaxcor
            if (idebug .ne. 0) write(Idebug,*) ' 6.34 G', NAM_G(IRRRunoffSub)
            ! eq. 6.35
            NAM_GWSD (iRRRunoffSub) = NAM_GWSD(IRRRunoffSub) - DeltaLInfmaxcor
            if (idebug .ne. 0) write(Idebug,*) ' 6.35 GWSD', NAM_GWSD(IRRRunoffSub)
            ! eq. 6.36
            NAM_GWSDrz (iRRRunoffSub) = max (0.0d0, NAM_GWSD(IRRRunoffSub) - NAM_GWSDssmax(IRRRunoffSub))
            if (idebug .ne. 0) write(Idebug,*) ' 6.36 GWSDrz', NAM_GWSDrz(IRRRunoffSub)
            ! eq. 6.37
            DLgravityCorr = min (NAM_LMAX(IRRRunoffSub) - NAM_L(IRRRunoffSub), NAM_GWSDRZ(IRRRunoffSub))
            if (idebug .ne. 0) write(Idebug,*) ' 6.37 DLgravityCorr',DLGravityCorr
            ! eq. 6.38
            NAM_DL(IRRRunoffSub) = NAM_DL(IRRRunoffSub) + DLGravityCorr
            if (idebug .ne. 0) write(Idebug,*) ' 6.38 DL    ', NAM_DL(IRRRunoffSub)
            ! eq. 6.39
            NAM_L(IRRRunoffSub) = NAM_L(IRRRunoffSub) + DLGravityCorr
            if (idebug .ne. 0) write(Idebug,*) ' 6.39 L     ', NAM_L(IRRRunoffSub)
            ! eq. 6.40
            NAM_G(iRRRunoffSub) = NAM_G(IRRRunoffSub) - DLGravityCorr
            if (idebug .ne. 0) write(Idebug,*) ' 6.40 G     ', NAM_G(IRRRunoffSub)
            ! eq. 6.41
            NAM_GWSD (iRRRunoffSub) = NAM_GWSD(IRRRunoffSub) - DLGravityCorr
            if (idebug .ne. 0) write(Idebug,*) ' 6.41 GWSD  ', NAM_GWSD(IRRRunoffSub)
            ! eq. 6.42
            NAM_GWSDrz (iRRRunoffSub) = max (0.0d0, NAM_GWSD(IRRRunoffSub) - NAM_GWSDssmax(IRRRunoffSub))
            if (idebug .ne. 0) write(Idebug,*) ' 6.42 GWSDrz', NAM_GWSDrz(IRRRunoffSub)
            ! eq. 6.43
            fact = (NAM_LInitial(IRRRunoffSub)+ NAM_GWSDrz(IRRRunoffSub)) / (NAM_LMax(IRRRunoffSub)+ NAM_GWSDrz(IRRRunoffSub))
            E2pot = (NamPotEvap - NAM_E1(IRRRunoffSub)) * fact
            if (idebug .ne. 0) write(Idebug,*) ' 6.43 E2pot', E2pot
            ! eq. 6.44
            NAM_E2(iRRRunoffSub) = min (NAM_LInitial(IRRRunoffSub) + NAM_GWSDrz(IRRRunoffSub), E2pot)
            if (idebug .ne. 0) write(Idebug,*) ' 6.44 E2', NAM_E2(IRRRunoffSub)
            ! eq. 6.45
            NAM_E2GWSrz(iRRRunoffSub) = min (NAM_E2(IRRRunoffSub), NAM_GWSDrz(IRRRunoffSub))
            if (idebug .ne. 0) write(Idebug,*) ' 6.45 E2GWSrz', NAM_E2GWSrz(IRRRunoffSub)
            ! eq. 6.46
            NAM_E2LZS(iRRRunoffSub) = NAM_E2(IRRRunoffSub) - NAM_E2GWSrz(IRRRunoffSub)
            if (idebug .ne. 0) write(Idebug,*) ' 6.46 E2LZS', NAM_E2LZS(IRRRunoffSub)
            ! eq. 6.47
            NAM_L(IRRRunoffSub) = NAM_L(IRRRunoffSub) - NAM_E2LZS(IRRRunoffSub)
            if (idebug .ne. 0) write(Idebug,*) ' 6.47 L', NAM_L(IRRRunoffSub)
            ! eq. 6.48
            NAM_GWSD (iRRRunoffSub) = NAM_GWSD(IRRRunoffSub) - NAM_E2GWSrz(IRRRunoffSub)
            if (idebug .ne. 0) write(Idebug,*) ' 6.48 GWSD', NAM_GWSD(IRRRunoffSub)
            ! eq. 6.49
            NAM_GWSDSS (iRRRunoffSub) = min (NAM_GWSD(IRRRunoffSub), NAM_GWSDSSMax(IRRRunoffSub))
            if (idebug .ne. 0) write(Idebug,*) ' 6.49 GWSDSS', NAM_GWSDSS(IRRRunoffSub)
            ! eq. 6.50
            NAM_GWSDRZ (iRRRunoffSub) = max (0.D0, NAM_GWSD(IRRRunoffSub) - NAM_GWSDSSMax(IRRRunoffSub))
            if (idebug .ne. 0) write(Idebug,*) ' 6.50 GWSDRZ', NAM_GWSDRZ(IRRRunoffSub)
            ! eq. 6.51
           if (NAM_GWSDRZ(IRRRunoffSub) .le. 0.D0) then
               NAM_GWL(IRRRunoffSub) = NAM_GWSBL(IRRRunoffSub) + ( NAM_GWSDSS(IRRRunoffSub) / (1000.D0 * NAM_SYSS(IRRRunoffSub)) )
            elseif (NAM_GWSDRZ(IRRRunoffSub) .gt. 0.D0 .and.  NAM_GWSDRZ(IRRRunoffSub) .lt. NAMAlfa * NAM_GWSDRZMax(IRRRunoff)) then
               Fact =  NAM_GWSDRZ(IRRRunoffSub) / NAMalfa / NAM_GWSDRZMax(IRRRunoffSub)
               NAM_GWL(IRRRunoffSub) = NAM_GWSBL(IRRRunoffSub) + ( NAM_GWSDSS(IRRRunoffSub) / (1000.D0 * NAM_SYSS(IRRRunoffSub)) ) + &
                                                          ( NAM_GWSDRZ(IRRRunoffSub) + NAM_L(IRRRunoffSub) * Fact ) / (1000.D0 * NAM_SYRZ(IRRRunoffSub))
            elseif (NAM_GWSDRZ(IRRRunoffSub) .ge. NAMAlfa * NAM_GWSDRZMax(IRRRunoff)) then
               NAM_GWL(IRRRunoffSub) = NAM_GWSBL(IRRRunoffSub) + ( NAM_GWSDSS(IRRRunoffSub) / (1000.D0 * NAM_SYSS(IRRRunoffSub)) ) + &
                                                                 ( NAM_GWSDRZ(IRRRunoffSub) + NAM_L(IRRRunoffSub) ) / (1000.D0 * NAM_SYRZ(IRRRunoffSub))
            endif
            if (idebug .ne. 0) write(Idebug,*) ' 6.51 GWL', NAM_GWL(IRRRunoffSub)
            ! eq. 6.52
             NAM_GWTD(IRRRunoffSub) = max (0.0d0, NAM_SurfaceLevel(IRRRunoffSub) - NAM_GWL(IRRRunoffSub))
             if (idebug .ne. 0) write(Idebug,*) ' 6.52 GWTD', NAM_GWTD(IRRRunoffSub)
            ! eq. 6.53
             if (NAM_GWL(IRRRunoffSub) .le. NAM_GWSBL(IRRRUnoffSub)) then
                CRPotMax = 0.0
             else
                if (NAM_CapRisOpt(IRRRunoffSub) .eq. 1) then
                    istart = NAM_CapRisTableStart (iRRRunoffSub)
                    LengthTable = NAM_CapRisTableEnd (iRRRunoffSub) - NAM_CapRisTableStart (iRRRunoffSub) + 1
                    Do k=1,LengthTable
                       GWTDArray(k)    = NAM_CapRisPercTableGWTD(k+istart-1)
                       CapPercArray(k) = NAM_CapRisPercTableCapPerc(k+istart-1)
                    Enddo
                    Call RR_D_INTERP  (LengthTable, GWTDArray, CapPercArray, NAM_GWTD(IRRRunoffSub), CRPotMax, istart)
                    CRpotmax = Fract * CRPotMax
                else
                    CRpotmax = Fract * NAM_CapRisConst(IRRRunoffSub)
                endif
             endif
             if (idebug .ne. 0) write(Idebug,*) ' 6.53 CRPotMax ', CRpotmax
            ! eq. 6.54
             ADLcrt2 = max (0D0, NAM_Lmax(IRRRunoffSub) - NAM_L(IRRRunoffSub))
             if (idebug .ne. 0) write(Idebug,*) ' 6.54 AdLcr2 ', ADLcrt2
            ! eq. 6.55
             NAM_CR(IRRRunoffSub) = min (ADLcrt2, NAM_GWSD(IRRRunoffSub), CRPotMax)
             if (idebug .ne. 0) write(Idebug,*) ' 6.55 CR', NAM_CR(IRRRunoffSub)
            ! eq. 6.56
             NAM_L(IRRRunoffSub) = NAM_L(IRRRunoffSub) + NAM_CR(IRRRunoffSub)
             if (idebug .ne. 0) write(Idebug,*) ' 6.56 L', NAM_L(IRRRunoffSub)
            ! eq. 6.57
             NAM_GWSD(IRRRunoffSub) = NAM_GWSD(IRRRunoffSub) - NAM_CR(IRRRunoffSub)
             if (idebug .ne. 0) write(Idebug,*) ' 6.57 GWSD', NAM_GWSD(IRRRunoffSub)
            ! eq. 6.58
             NAM_GWSDSS (iRRRunoffSub) = min (NAM_GWSD(IRRRunoffSub), NAM_GWSDSSMax(IRRRunoffSub))
             if (idebug .ne. 0) write(Idebug,*) ' 6.58 GWSDSS', NAM_GWSDSS(IRRRunoffSub)
            ! eq. 6.59
             NAM_GWSDRZ (iRRRunoffSub) = max (0.D0, NAM_GWSD(IRRRunoffSub) - NAM_GWSDSSMax(IRRRunoffSub))
             if (idebug .ne. 0) write(Idebug,*) ' 6.59 GWSDRZ', NAM_GWSDRZ(IRRRunoffSub)
            ! eq. 6.60
             if (NAM_GWSDRZ(IRRRunoffSub) .le. 0.D0) then
                NAM_GWL(IRRRunoffSub) = NAM_GWSBL(IRRRunoffSub) + ( NAM_GWSDSS(IRRRunoffSub) / (1000.D0 * NAM_SYSS(IRRRunoffSub)) )
             elseif (NAM_GWSDRZ(IRRRunoffSub) .gt. 0.D0 .and.  NAM_GWSDRZ(IRRRunoffSub) .lt. NAMAlfa * NAM_GWSDRZMax(IRRRunoff)) then
                Fact =  NAM_GWSDRZ(IRRRunoffSub) / NAMalfa / NAM_GWSDRZMax(IRRRunoffSub)
                NAM_GWL(IRRRunoffSub) = NAM_GWSBL(IRRRunoffSub) + ( NAM_GWSDSS(IRRRunoffSub) / (1000.D0 * NAM_SYSS(IRRRunoffSub)) ) + &
                                                           ( NAM_GWSDRZ(IRRRunoffSub) + NAM_L(IRRRunoffSub) * Fact ) / (1000.D0 * NAM_SYRZ(IRRRunoffSub))
             elseif (NAM_GWSDRZ(IRRRunoffSub) .ge. NAMAlfa * NAM_GWSDRZMax(IRRRunoff)) then
                NAM_GWL(IRRRunoffSub) = NAM_GWSBL(IRRRunoffSub) + ( NAM_GWSDSS(IRRRunoffSub) / (1000.D0 * NAM_SYSS(IRRRunoffSub)) ) + &
                                                                  ( NAM_GWSDRZ(IRRRunoffSub) + NAM_L(IRRRunoffSub) ) / (1000.D0 * NAM_SYRZ(IRRRunoffSub))
             endif
             if (idebug .ne. 0) write(Idebug,*) ' 6.60 GWL', NAM_GWL(IRRRunoffSub)
            ! eq. 6.61
             HoutSide      = NAM_GWL(IRRRunoffSub)
             if (IBND .gt. 0) then
                HoutSide   = BndPar(IBND,1)
             elseif (Iow .gt. 0) then
                HoutSide   = LVLOw0(iow)
             endif
             if (idebug .ne. 0) write(Idebug,*) '      Houtside', Houtside
             ZFastBF = max (HoutSide, NAM_TFastBF(IRRRunoffSub))
             if (idebug .ne. 0) write(Idebug,*) ' 6.61 ZFastBF ', ZFastBF
            ! eq. 6.62
             ZSlowBF = max (HoutSide, NAM_TSlowBF(IRRRunoffSub))
             if (idebug .ne. 0) write(Idebug,*) ' 6.62 ZSlowBF ', ZSlowBF
            ! eq. 6.63
             DHFastBF = max (0.0D0, NAM_GWL(IRRRunoffSub)-ZFastBF)
            ! eq. 6.64
             DHSlowBF = max (0.0D0, NAM_GWL(IRRRunoffSub)-ZSlowBF)
            ! eq. 6.65
             if (NAM_GWSBL(IRRRunoffSub) .lt. ZFastBF .and. ZFastBF .lt. NAM_RZBL(IRRRunoffSub) ) then
                GWSDZFastBF = 1000.D0 * NAM_SYSS(IRRRunoffSub) * (ZFastBF - NAM_GWSBL(IRRRunoffSub))
             elseif (ZFastBF .ge. NAM_RZBL(IRRRunoffSub) .and. ZFastBF .le. NAM_SurfaceLevel(IRRRunoffSub) ) then
                GWSDZFastBF = 1000.D0 * NAM_SYSS(IRRRunoffSub) * (NAM_RZBL(IRRRunoffSub) - NAM_GWSBL(IRRRunoffSub)) + &
                               1000.D0 * (NAM_SYRZ(IRRRunoffSub)-NAM_SFC(IRRRunoffSub)) * (ZFastBF - NAM_RZBL(IRRRunoffSub))
             elseif (ZFastBF .gt. NAM_SurfaceLevel(IRRRunoffSub)) then
                GWSDZFastBF = NAM_GWSDmax(IRRRunoffSub)
             endif
             if (idebug .ne. 0) write(Idebug,*) ' 6.65 GWSDZfastBF', GWSDZfastBF
            ! eq. 6.66
             if (NAM_GWSBL(IRRRunoffSub) .lt. ZSlowBF .and. ZSlowBF .lt. NAM_RZBL(IRRRunoffSub) ) then
                GWSDZSlowBF = 1000.D0 * NAM_SYSS(IRRRunoffSub) * (ZSlowBF - NAM_GWSBL(IRRRunoffSub))
             elseif (ZSlowBF .ge. NAM_RZBL(IRRRunoffSub) .and. ZSlowBF .le. NAM_SurfaceLevel(IRRRunoffSub) ) then
                GWSDZSlowBF = 1000.D0 * NAM_SYSS(IRRRunoffSub) * (NAM_RZBL(IRRRunoffSub) - NAM_GWSBL(IRRRunoffSub)) + &
                               1000.D0 * (NAM_SYRZ(IRRRunoffSub)-NAM_SFC(IRRRunoffSub)) * (ZSlowBF - NAM_RZBL(IRRRunoffSub))
             elseif (ZSlowBF .gt. NAM_SurfaceLevel(IRRRunoffSub)) then
                GWSDZSlowBF = NAM_GWSDmax(IRRRunoffSub)
             endif
             if (idebug .ne. 0) write(Idebug,*) ' 6.66 GWSDZslowBF', GWSDZslowBF
            ! eq. 6.67
             VpotFastBF = max (0D0, NAM_GWSD(IRRRunoffSub) - GWSDZfastBF)
            ! eq. 6.68
             VpotSlowBF = max (0D0, NAM_GWSD(IRRRunoffSub) - GWSDZslowBF)
            ! eq. 6.69
             ckfastbf       = 1.D0 / (1.D0 -(1.D0 -(1.D0/NAM_ckfastbf(iRRRunoffSub)))**Fract)
            ! eq. 6.70
             ckslowbf       = 1.D0 / (1.D0 -(1.D0 -(1.D0/NAM_ckslowbf(iRRRunoffSub)))**Fract)
            ! eq. 6.71
             FastBFpot = min (1000.D0 * DHFastBF / ckFastBF, VPotFastBF)
             if (idebug .ne. 0) write(Idebug,*) ' 6.71  FastBFPot', FastBFPot
            ! eq. 6.72
             SlowBFpot = min (1000.D0 * DHSlowBF / ckSlowBF, VPotSlowBF)
             if (idebug .ne. 0) write(Idebug,*) ' 6.72  SlowBFPot', SlowBFPot
            ! eq. 6.73
             GWOutflowPot = FastBFpot + SlowBFpot
            ! eq. 6.74
             If (GWOutflowPot .le. VpotFastBF) then
                NAM_FastBF(IRRRunoffSub) = FastBFpot
             else
                NAM_FastBF(IRRRunoffSub) = VpotFastBF * FastBFpot / GWOutflowPot
             endif
             if (idebug .ne. 0) write(Idebug,*) ' 6.74 FastBF', NAM_FastBF(IRRRUnoffSub)
            ! eq. 6.75
             If (NAM_FastBF(IRRRunoffSub) + SlowBFpot .le. VpotSlowBF) then
                NAM_SlowBF(IRRRunoffSub) = SlowBFpot
             else
                NAM_SlowBF(IRRRunoffSub) = VpotSlowBF - NAM_FastBF(IRRRunoffSub)
             endif
             if (idebug .ne. 0) write(Idebug,*) ' 6.75 SlowBF', NAM_SlowBF(IRRRUnoffSub)
            ! eq. 6.76
             GWOutflow = NAM_FastBF(IRRRunoffSub) + NAM_SlowBF(IRRRunoffSub)
            if (idebug .ne. 0) write(Idebug,*) ' 6.76 GwOutflow', GwOutflow
            ! eq. 6.77
             NAM_GWSD(IRRRunoffSub) = NAM_GWSD(IRRRunoffSub) - GWOutflow
             if (idebug .ne. 0) write(Idebug,*) ' 6.77 GWSD', NAM_GWSD(IRRRunoffSub)
            ! eq. 6.78
             DHGWInflow = max (0.0D0, HoutSide - NAM_GWL(IRRRunoffSub))
            ! eq. 6.79
             if (NAM_GWL(IRRRunoffSub) .ge. NAM_SurfaceLevel(IRRRunoffSub)) then
                VPotGwInflow = 0D0
             elseif (NAM_GWL(IRRRunoffSub) .ge. HOutSide) then
                VPotGwInflow = 0D0
             elseif (NAM_GWL(IRRRunoffSub) .lt. HoutSide .and. HOutSide .le. NAM_RZBL(IRRRunoffSub)) then
                VpotGWInflow =  1000.D0 * NAM_SYSS(IRRRunoffSub) * ( HOutside - NAM_GWL(IRRRunoffSub))
             elseif (NAM_GWL(IRRRunoffSub) .le. NAM_RZBL(IRRRunoffSub) .and. NAM_RZBL(IRRRunoffSub) .lt. Houtside .and. HOutside .le. NAM_SurfaceLevel(IRRRunoffSub) ) then
                VpotGWInflow =  1000.D0 * NAM_SYSS(IRRRunoffSub) * ( NAM_RZBL(IRRRunoffSub) - NAM_GWL(IRRRunoffSub) ) + &
                                 max (0D0, 1000.D0 * NAM_SYRZ(IRRRunoffSub) * ( HOutside - NAM_RZBL(IRRRunoffSub)) - NAM_L(IRRRunoffSub) )
             elseif (NAM_GWL(IRRRunoffSub) .le. NAM_RZBL(IRRRunoffSub) .and. NAM_RZBL(IRRRunoffSub) .le. NAM_SurfaceLevel(IRRRunoffSub) .and. HOutside .gt. NAM_SurfaceLevel(IRRRunoffSub) ) then
                VpotGWInflow =  1000.D0 * NAM_SYSS(IRRRunoffSub) * ( NAM_RZBL(IRRRunoffSub) - NAM_GWL(IRRRunoffSub) ) + &
                                 max (0D0, 1000.D0 * NAM_SYRZ(IRRRunoffSub) * ( NAM_SurfaceLevel(IRRRunoffSub) - NAM_RZBL(IRRRunoffSub)) - NAM_L(IRRRunoffSub))
             elseif (NAM_RZBL(IRRRunoffSub) .lt. NAM_GWL(IRRRunoffSub) .and. NAM_GWL(IRRRunoffSub) .lt. Houtside .and. HOutside  .le. NAM_SurfaceLevel(IRRRunoffSub) ) then
                VpotGWInflow =  max (0D0, 1000.D0 * NAM_SYRZ(IRRRunoffSub) * ( HOutSide - NAM_RZBL(IRRRunoffSub)) - NAM_L(IRRRunoffSub)) - &
                                  max (0D0, 1000.D0 * NAM_SYRZ(IRRRunoffSub) * ( NAM_GWL(IRRRunoffSub) - NAM_RZBL(IRRRunoffSub)) - NAM_L(IRRRunoffSub))
             elseif (NAM_RZBL(IRRRunoffSub) .lt. NAM_GWL(IRRRunoffSub) .and. NAM_GWL(IRRRunoffSub) .le.  NAM_SurfaceLevel(IRRRunoffSub) .and. HOutside .gt. NAM_SurfaceLevel(IRRRunoffSub) ) then
                VpotGWInflow =  max (0D0, 1000.D0 * NAM_SYRZ(IRRRunoffSub) * ( NAM_SurfaceLevel(IRRRunoffSub) - NAM_RZBL(IRRRunoffSub)) - NAM_L(IRRRunoffSub)) - &
                                  max (0D0, 1000.D0 * NAM_SYRZ(IRRRunoffSub) * ( NAM_GWL(IRRRunoffSub) - NAM_RZBL(IRRRunoffSub)) - NAM_L(IRRRunoffSub))
             endif
             if (idebug .ne. 0) write(Idebug,*) ' 6.78 VPotGwInflow', VPotGWInflow
            ! eq. 6.80
             ckgwinflow     = 1.D0 / (1.D0 -(1.D0 -(1.D0/NAM_ckgwinflow(iRRRunoffSub)))**Fract)
            ! eq. 6.81
             NAM_GWInflow(IRRRunoffSub) = min (VpotGWInflow, 1000.0D0 * DHGWInflow / ckGWInflow)
             if (idebug .ne. 0) write(Idebug,*) ' 6.81 GwInflow', NAM_GWInflow(IRRRunoffSub)
            ! eq. 6.82
             if (NAM_GWInflow(IRRRunoffSub) .le. 0.0) then
                NAM_DLExt(IRRRunoffSub) = 0.0
             elseif (NAM_GWSD(IRRRunoffSub) + NAM_GWInflow(IRRRunoffSub) .le. NAM_GWSDssmax(IRRRUnoff)) then
                NAM_DLExt(IRRRunoffSub) = 0.0
             elseif (NAM_GWSD(IRRRunoffSub) + NAM_GWInflow(IRRRunoffSub) .gt. NAM_GWSDssmax(IRRRUnoff)) then
                NAM_DLExt(IRRRunoffSub) = min (NAM_LMAX(IRRRunoffSub) - NAM_L(IRRRunoffSub), &
                               NAM_GWSD(IRRRunoffSub) + NAM_GWInflow(IRRRunoffSub) - NAM_GWSDssmax(IRRRunoffSub))
             endif
             if (idebug .ne. 0) write(Idebug,*) ' 6.82 DLExt', NAM_DLExt(IRRRunoffSub)
            ! eq. 6.83
             NAM_GWExt(IRRRunoffSub) = NAM_GWInflow(IRRRunoffSub) - NAM_DLExt(IRRRunoffSub)
             if (idebug .ne. 0) write(Idebug,*) ' 6.83 GWExt', NAM_GWExt(IRRRunoffSub)
            ! eq. 6.84
             NAM_L(IRRRunoffSub) = NAM_L(IRRRunoffSub) + NAM_DLExt(IRRRunoffSub)
             if (idebug .ne. 0) write(Idebug,*) ' 6.84 L', NAM_L(IRRRunoffSub)
            ! eq. 6.85
             NAM_GWSD(IRRRunoffSub) = NAM_GWSD(IRRRunoffSub) + NAM_GWExt(IRRRunoffSub)
             if (idebug .ne. 0) write(Idebug,*) ' 6.85 GWSD', NAM_GWSD(IRRRunoffSub)
            ! eq. 6.86
             GWAbsmax = NAM_GWSD(IRRRunoffSub)
             if (idebug .ne. 0) write(Idebug,*) ' 6.86 GwAbsMax', GWAbsMax
            ! eq. 6.87
             GWPump = NAMPumpflow(IRRRunoffSub)
             GWAbspot = max (0D0, 1000.D0*TimeSettings%TimestepSize / NAM_CatchmentArea(IRRRunoffSub) * GWPump)
             if (idebug .ne. 0) write(Idebug,*) ' 6.87 defined pumping GWPump', GWPump
             if (idebug .ne. 0) write(Idebug,*) ' 6.87 GWAbsPot ', GWAbsPot
            ! eq. 6.88
             GWAbsact = min (GWAbsmax, GWAbspot)
             NAM_GWAbsAct(IRRRunoffSub) = GWAbsAct
             if (idebug .ne. 0) write(Idebug,*) ' 6.88 GwAbsAct', GWAbsAct
            ! eq. 6.89
             GWSupmax = NAM_GWSDmax(IRRRunoffSub) - NAM_GWSD(IRRRunoffSub) + &
                             NAM_LMax(IRRRunoffSub) -  NAM_L(IRRRunoffSub)
             if (idebug .ne. 0) write(Idebug,*) ' 6.89 GwSupMax', GWSupMax
            ! eq. 6.90
             GWSuppot = max (0D0,-1000.D0*TimeSettings%TimestepSize / NAM_CatchmentArea(IRRRunoffSub) * GWPump)
             if (idebug .ne. 0) write(Idebug,*) ' 6.90 GwSupPot', GWSupPot
            ! eq. 6.91
             GWSupact = min (GWSupmax, GWSuppot)
             if (idebug .ne. 0) write(Idebug,*) ' 6.91 GwSupAct', GWSupAct
            ! eq. 6.92
             GWPumpAct = 0D0
             if (GWPump .gt. 0) then
                GWPumpAct = NAM_CatchmentArea(IRRRunoffSub) * GwAbsAct / (1000.D0 * TimeSettings%TimestepSize)
             else
                GWPumpAct = NAM_CatchmentArea(IRRRunoffSub) * GwSupAct / (-1000.D0 * TimeSettings%TimestepSize)
             endif
             if (idebug .ne. 0) write(Idebug,*) ' 6.92 GwPumpAct', GWPumpAct
            ! eq. 6.93
             GWPumpShortage = GWPumpAct - GWPump
            ! eq. 6.94
             NAM_GWSD(IRRRunoffSub) = NAM_GWSD(IRRRunoffSub) - GWAbsAct
             if (idebug .ne. 0) write(Idebug,*) ' 6.94 GWSD', NAM_GWSD(IRRRunoffSub)
            ! eq. 6.95
             ADgwsSsGwpump = max (0D0, NAM_GWSDssmax(IRRRunoffSub)-NAM_GWSD(IRRRunoffSub) )
            ! eq. 6.96
             ADLGwpump = NAM_LMAX(IRRRunoffSub)-NAM_L(IRRRunoffSub)
            ! eq. 6.97
             INgwsSsGWpump = min (ADgwsSsGwpump, GWSupAct)
             if (idebug .ne. 0) write(Idebug,*) ' 6.97 IngwsssgwPump', INgwsssGWPump
            ! eq. 6.98
             GWrestSupAct = GWSupAct - IngwsSsGwpump
            ! eq. 6.99
             DLGwpump = min (ADLGwpump, GWrestSupAct)
             if (idebug .ne. 0) write(Idebug,*) ' 6.99 DLGWPump', DLGWPump
            ! eq. 6.100
             INgwsrzGWpump = GWrestSupAct - DLgwpump
             if (idebug .ne. 0) write(Idebug,*) ' 6.100 IngwsrzgwPump', INgwsrzGWPump
            ! eq. 6.101
             NAM_L(IRRRunoffSub) = NAM_L(IRRRunoffSub) + DLgwpump
             if (idebug .ne. 0) write(Idebug,*) ' 6.101 L', NAM_L(IRRRunoffSub)
            ! eq. 6.102
             NAM_GWSD(IRRRunoffSub) = NAM_GWSD(IRRRunoffSub) + INgwsSsGWpump + INgwsRzGWpump
             if (idebug .ne. 0) write(Idebug,*) ' 6.102 GWSD', NAM_GWSD(IRRRunoffSub)
            ! eq. 6.103
             NAM_BF(IRRRunoffSub) = GWoutflow - NAM_GWInflow(IRRRunoffSub)
             if (idebug .ne. 0) write(Idebug,*) ' 6.103 BF', NAM_BF(IRRRunoffSub)
            ! eq. 6.104
            NAM_GWSDSS (iRRRunoffSub) = min (NAM_GWSD(IRRRunoffSub), NAM_GWSDSSMax(IRRRunoffSub))
            if (idebug .ne. 0) write(Idebug,*) ' 6.104 GWSDSS', NAM_GWSDSS(IRRRunoffSub)
            ! eq. 6.105
            NAM_GWSDRZ (iRRRunoffSub) = max (0.D0, NAM_GWSD(IRRRunoffSub) - NAM_GWSDSSMax(IRRRunoffSub))
            if (idebug .ne. 0) write(Idebug,*) ' 6.105 GWSDRZ', NAM_GWSDRZ(IRRRunoffSub)
            ! eq. 6.106
            if (NAM_GWSDRZ(IRRRunoffSub) .le. 0.D0) then
                NAM_GWL(IRRRunoffSub) = NAM_GWSBL(IRRRunoffSub) + ( NAM_GWSDSS(IRRRunoffSub) / (1000.D0 * NAM_SYSS(IRRRunoffSub)) )
             elseif (NAM_GWSDRZ(IRRRunoffSub) .gt. 0.D0 .and.  NAM_GWSDRZ(IRRRunoffSub) .lt. NAMAlfa * NAM_GWSDRZMax(IRRRunoff)) then
                Fact =  NAM_GWSDRZ(IRRRunoffSub) / NAMalfa / NAM_GWSDRZMax(IRRRunoffSub)
                NAM_GWL(IRRRunoffSub) = NAM_GWSBL(IRRRunoffSub) + ( NAM_GWSDSS(IRRRunoffSub) / (1000.D0 * NAM_SYSS(IRRRunoffSub)) ) + &
                                                           ( NAM_GWSDRZ(IRRRunoffSub) + NAM_L(IRRRunoffSub) * Fact ) / (1000.D0 * NAM_SYRZ(IRRRunoffSub))
             elseif (NAM_GWSDRZ(IRRRunoffSub) .ge. NAMAlfa * NAM_GWSDRZMax(IRRRunoff)) then
                NAM_GWL(IRRRunoffSub) = NAM_GWSBL(IRRRunoffSub) + ( NAM_GWSDSS(IRRRunoffSub) / (1000.D0 * NAM_SYSS(IRRRunoffSub)) ) + &
                                                                  ( NAM_GWSDRZ(IRRRunoffSub) + NAM_L(IRRRunoffSub) ) / (1000.D0 * NAM_SYRZ(IRRRunoffSub))
             endif
             if (idebug .ne. 0) write(Idebug,*) ' 6.106 GWL', NAM_GWL(IRRRunoffSub)
            ! eq. 6.107
             NAM_GWTD(iRRRunoffSub) = max (0.D0, NAM_SurfaceLevel(IRRRunoffSub) - NAM_GWL(IRRRunoffSub))
             if (idebug .ne. 0) write(Idebug,*) ' 6.107 GWTD', NAM_GWTD(IRRRunoffSub)
            ! eq. 6.108
             NAM_VU(IRRRunoffSub) = 1/1000.0D0 * NAM_U(IRRRunoffSub) * NAM_CatchmentArea(IRRRunoffSub)
             if (idebug .ne. 0) write(Idebug,*) ' 6.108 VU', NAM_VU(IRRRunoffSub)
            ! eq. 6.109
             NAM_VL(IRRRunoffSub) = 1/1000.0D0 * NAM_L(IRRRunoffSub) * NAM_CatchmentArea(IRRRunoffSub)
             if (idebug .ne. 0) write(Idebug,*) ' 6.109 VL', NAM_VL(IRRRunoffSub)
            ! eq. 6.110
             NAM_VGWS(IRRRunoffSub) = 1/1000.0D0 * NAM_GWSD(IRRRunoffSub) * NAM_CatchmentArea(IRRRunoffSub)
             if (idebug .ne. 0) write(Idebug,*) ' 6.110 VGWS', NAM_VGWS(IRRRunoffSub)
            ! eq. 6.111
             NAM_AVSoil(IRRRunoffSub) = 1/1000.0D0 * (NAM_Lmax(IRRRunoffSub) - NAM_L(IRRRunoffSub) + &
                                          NAM_GWSDMax(IRRRunoffSub) - NAM_GWSD(IRRRunoffSub) ) * NAM_CatchmentArea(IRRRunoffSub)
             if (idebug .ne. 0) write(Idebug,*) ' 6.111 AvSoil', NAM_AVSoil(IRRRunoffSub)
            ! eq. 6.112
             NAM_GWSupAct(IRRRunoffSub) = GWSupAct
             NAM_GWGWpump(IRRRunoffSub) = GWSupAct - DLGWPump

            ! eq. 6.113 balancing CapRis and Percolation
             if (NAM_CR(IRRRunoffSub) .gt. 0 .and. NAM_G(IRRRunoffSub) .gt. 0) then
                 Rhelp = min (NAM_CR(IRRRunoffSub), NAM_G(IRRRunoffSub))
                 NAM_CR(IRRRunoffSub) = NAM_CR(IRRRunoffSub) - Rhelp
                 NAM_G (IRRRunoffSub) = NAM_G(IRRRunoffSub) - Rhelp
             endif
            if (idebug .ne. 0) write(Idebug,*) ' 6.113 NAM_CR', NAM_CR(IRRRunoffSub)
            if (idebug .ne. 0) write(Idebug,*) ' 6.113 NAM_G ', NAM_G (IRRRunoffSub)

             NAMRunoff = NAM_BF(IRRRunoffSub) + NAM_IF(IRRRUnoffSub) + NAM_OF(IRRRunoffSub)
          ! store output in NAM arrays
            NAMRainfall(IRRRunoffSub)               = NamPrecipitation
            NAMPotEvapTot (IRRRunoffSub)            = NamPotEvap
            NAMActEvapTot (IRRRunoffSub)            = NAM_E1(IRRRunoffSub) + NAM_E2(IRRRUnoffSub)
            NAM_Houtside (IRRRunoffSub)             = HOutside
            NAM_DLGWPump (IRRRunoffSub)             = DLGWPump
            NAM_GWPumpAct(IRRRunoffSub)             = GWPumpact
            NAM_GWPump (IRRRunoffSub)               = GWPump
            NAM_GWPumpShortage(IRRRunoffSub)        = GWPumpShortage
! from mm/timestep to m3/s
            QF2 = NAMRunoff * Area_RRRunoffNode(IRRRunoff) * mm2m / timeSettings%TimestepSize
            RRRunoffNode_Outflow(iRRRunoff) = QF2

      elseif (RRRunoff_CompOption(IRRRunoff) .eq. 4) then
!         Runoff node option 4: LGSI method
!         All constructed flow interpolation tables are in mm/hour, so use ratio of FractHour (timestepsize/3600) to define total mm/timestep
            if (idebug .ne. 0) then
               Write(Idebug,*) ' Rain (m3/s) ', Rain(imeteo)
               Write(Idebug,*) ' Evap (m3/s) ', Evap(imeteo)
            Endif
!           fill local interpolation arrays for present LGSI node
            Do k=1,LGSI_MaxInterpLengthPlus1
                SeepageArray(k)     = LGSI_InterpGwLevelSeepage(k,iRRRunoffSub)
                SeepageFlowArray(k) = LGSI_InterpSeepageFlow(k,iRRRunoffSub)
            Enddo
! Initialisations
            LGSIPrecipitation = AAFNodeRainfall(inode) * Rain(imeteo) * TimeSettings%Timestepsize / mm2m   ! in mm
            LGSIPotEvap       = Evap(imeteo) * TimeSettings%Timestepsize / mm2m   ! in mm
            LGSITotalRunoff = 0.
            LGSIConverged = .false.
            iter = 0
            do while (.not. LGSIconverged)
              iter = iter + 1
              LGSIPreviousTotalRunoff = LGSITotalRunoff
              if (iter .eq. 1) then
                ! Initial calculation
                LGSIInterflow = 0.0
                RatioArea1 = LGSI_AREA(IRRRunoffSub,1) / LGSI_AreaTot(IRRRunoffSub)
                RatioArea2 = 0.0D0
                If (LGSI_NrSubAreas(IRRRunoffSub) .ge. 2) then
! 2 juli 2013:      LGSIHeadDiff = LGSI_InitGwl(IRRRUnoff,1) - LGSI_InitGwl(IRRRunoffSub,2)
                    LGSIHeadDiff = LGSI_InitGwl(IRRRunoffSub,2) - LGSI_InitGwl(IRRRunoffSub,1)
                    Call RR_D_INTERP  (LGSI_MaxInterpLength, SeepageArray, SeepageFlowArray, LGSIHeadDiff,LGSIInterflow,Ilast1)
                    LGSIInterflow = LGSIInterFlow * FractHour
                    RatioArea  = LGSI_AREA(IRRRUnoffSub,2) / LGSI_Area(IRRRunoffSub,1)
                    RatioArea2 = LGSI_AREA(IRRRUnoffSub,2) / LGSI_AreaTot(IRRRunoffSub)
                endif
                if (idebug .ne. 0) then
                   Write(Idebug,*) ' RatioArea, RatioArea2 ', RatioArea, RatioArea2
                   Write(Idebug,*) ' LGSIHeadDiff  ', LGSIHeadDiff
                   Write(Idebug,*) ' LGSIInterflow*FractHour ', LGSIInterflow
                   Write(Idebug,*) ' FractHour               ', FractHour
                   Write(Idebug,*) ' ilast1  ', ilast1
                Endif
                Do i=1,LGSI_NrSubAreas(IRRRunoffSub)
                  LGSIGWL  = LGSI_InitGwl(IRRRunoffSub,i)
                  LGSIPreviousGwl(i) = Lgsi_InitGwl(IRRRunoffSub,i)
                  ! compute precipitation reduction and evaporation reduction, depending on type of distribution
                  x = LGSIGWL
                  If (LGSI_Type(IRRRunoffSub,i) .eq. 1) then  !normal distribution
                     Call ComputePsdNormal (LGSI_NormalMaxSD(IRRRunoffSub,i), LGSI_NormalMinSD(IRRRunoffSub,i), LGSI_NormalNusDMax(IRRRunoffSub,i), LGSI_NormalNb(IRRRunoffSub,i), x, Sd)
                  ElseIf (LGSI_Type(IRRRunoffSub,i) .eq. 2) then  !gamma distribution
                     Call ComputePsdGamma (LGSI_GammaAg(IRRRunoffSub,i), LGSI_GammaBg(IRRRunoffSub,i), x, sD)
                  endif
                  if (idebug .ne. 0) then
                     Write(Idebug,*) ' subarea ', i
                     Write(Idebug,*) ' x   sD ',x, sD
                  Endif
                  Call ComputePrecipitationReduction (LGSIGWL, sD, PrecipReduction)
                  Call ComputeEvaporationReduction   (LGSIGWL, LGSI_ERD(IRRRunoffSub,i), LGSI_ERSD(IRRRunoffSub,i), EvapReduction)
                  if (idebug .ne. 0) then
                     Write(Idebug,*) ' PrecipReduction ', PrecipReduction
                     Write(Idebug,*) ' EvapReduction   ', EvapReduction
                  Endif
                  RiverOutFlow = 0.
                  OverlandFlow = 0.
                  DrainageFlow = 0.
                  If (LGSI_Type(IRRRunoffSub,i) .eq. 1) then  !river and overland flow: only for normal distribution
                     Do k=1,LGSI_MaxInterpLengthPlus1
                        LevelDrainArray(k) = LGSI_InterpGwLevelDrain(k,iRRRunoffsub,i)
                        QuickFlowArray(k)  = LGSI_InterpQuickFlow(k,iRRRunoffsub,i)
                        OverlandFlowArray(k) = LGSI_InterpOverlandFlow(k,iRRRunoffsub,i)
                        DrainageFlowArray(k) = LGSI_InterpDrainageFlow(k,iRRRunoffsub,i)
                     Enddo
                     Call RR_D_INTERP (LGSI_MaxInterpLength, LevelDrainArray, QuickFlowArray,LGSIGWL,RiverOutflow,Ilast1)
                     Call RR_D_INTERP (LGSI_MaxInterpLength, LevelDrainArray, OverlandFlowArray,LGSIGWL,OverlandFlow,Ilast1)
                  endif
                  ! drainage flow: both for normal and gamma type
                  Call RR_D_INTERP (LGSI_MaxInterpLength, LevelDrainArray, DrainageFlowArray,LGSIGWL,DrainageFlow,Ilast1)
                  if (idebug .ne. 0) then
                     Write(Idebug,*) ' LGSIGWL ', LGSIGwl
                     Write(Idebug,*) ' ilast1  ', ilast1
                     Write(Idebug,*) ' RiverOutflow ', RiverOutflow
                     Write(Idebug,*) ' Overlandflow ', Overlandflow
                     Write(Idebug,*) ' Drainageflow ', Drainageflow
                  Endif
! all to mm/timestep instead of mm/hour
!                 Precipitation, evaporation already ok
                  RiverOutFlow = RiverOutFlow * FractHour
                  OverlandFlow = OverlandFlow * FractHour
                  DrainageFlow = DrainageFlow * FractHour
!                 LGSI_Qout already done after reading input
                  if (i .eq. 1) then
                      if (itmstp .ge. 685) then
                          continue
                      endif
                      TotalInOutFlow(1) = ( (1.-PrecipReduction)*LGSIPrecipitation - EvapReduction*LGSIPotEvap) * mm2m - RiverOutflow - OverlandFlow - DrainageFlow + LGSIInterflow * mm2m * RatioArea - LGSI_Qout(IRRRunoffSub,i) * mm2m
                  elseif (i .eq. 2) then
                     TotalInOutFlow(2) = ( (1.-PrecipReduction)*LGSIPrecipitation - EvapReduction*LGSIPotEvap) * mm2m - RiverOutflow - OverlandFlow - DrainageFlow - LGSIInterflow * mm2m             - LGSI_Qout(IRRRunoffSub,i) * mm2m
                  endif
                  if (idebug .ne. 0) then
                     Write(Idebug,*) ' TotalInOutflow(i) ', TotalInOutflow(i)
                  Endif
                  StorageChange = TotalInOutflow(i)
                  Do k=1,LGSI_MaxInterpLengthPlus1
                      GWLevelArray(k)  = LGSI_InterpGwLevelTotalV(k,iRRRunoffsub,i)
                      GWVolumeArray(k) = LGSI_InterpTotalVolume(k,iRRRunoffsub,i)
                  Enddo
                  Call RR_D_INTERP (LGSI_MaxInterpLength, GWLevelArray, GWVolumeArray, LGSIGWL,LGSIInitVolume(i),Ilast1)
                  LGSIVolume = LGSIInitVolume(i) + StorageChange
                  if (idebug .ne. 0) then
                     Write(Idebug,*) ' LGSIVolume   ', LGSIVolume
                  Endif
                  Do k=1,LGSI_MaxInterpLength
                     TempVolume(k) = -1. * LGSI_InterpTotalVolume(k,IRRRunoffSub,i)
                  Enddo
                  Call RR_D_INTERP (LGSI_MaxInterpLength, TempVolume, GWLevelArray, -1.*LGSIVolume, NewGwLevel, Ilast1)
                  LGSI_NewGWL(IRRRunoffSub,i) = NewGwLevel
                  if (idebug .ne. 0) then
                     Write(Idebug,*) ' LGSI_NewGWL  ', NewGwLevel
                  Endif
                Enddo
              Endif    ! end of calculation for iter=1 with initial gwl

              If (LGSI_NrSubAreas(IRRRunoffSub) .ge. 2) then
! 2 juli 2013:    LGSIHeadDiff = LGSI_InitGwl(IRRRUnoff,1) - LGSI_InitGwl(IRRRunoffSub,2)
                  LGSIHeadDiff = LGSI_NewGwl(IRRRunoffSub,2) - LGSI_NewGwl(IRRRunoffSub,1)
                  Call RR_D_INTERP (LGSI_MaxInterpLength, SeepageArray, SeepageFlowArray, LGSIHeadDiff,LGSIInterflow,Ilast1)
                  LGSIInterflow = LGSIInterFlow * FractHour
              endif
              if (idebug .ne. 0) then
                 Write(Idebug,*) ' RatioArea, RatioArea2 ', RatioArea, RatioArea2
                 Write(Idebug,*) ' LGSIHeadDiff  ', LGSIHeadDiff
                 Write(Idebug,*) ' LGSIInterflow*FractHour ', LGSIInterflow
              Endif
              Do i=1,LGSI_NrSubAreas(IRRRunoffSub)
                LGSIGWL  = 0.5D0 * (LGSI_NewGwl(IRRRunoffSub,i) + LGSI_InitGwl(IRRRunoffSub,i))
                LGSIPreviousGwl(i) = Lgsi_NewGwl(IRRRunoffSub,i)
                ! compute precipitation reduction and evaporation reduction, depending on type of distribution
                x = LGSIGWL
                If (LGSI_Type(IRRRunoffSub,i) .eq. 1) then  !normal distribution
                   Call ComputePsdNormal (LGSI_NormalMaxSD(IRRRunoffSub,i), LGSI_NormalMinSD(IRRRunoffSub,i), LGSI_NormalNusDMax(IRRRunoffSub,i), LGSI_NormalNb(IRRRunoffSub,i), x, sD)
                ElseIf (LGSI_Type(IRRRunoffSub,i) .eq. 2) then  !gamma distribution
                   Call ComputePsdGamma (LGSI_GammaAg(IRRRunoffSub,i), LGSI_GammaBg(IRRRunoffSub,i), x, sD)
                endif
                Call ComputePrecipitationReduction (LGSIGWL, sD, PrecipReduction)
                Call ComputeEvaporationReduction   (LGSIGWL, LGSI_ERD(IRRRunoffSub,i), LGSI_ERSD(IRRRunoffSub,i), EvapReduction)
                if (idebug .ne. 0) then
                   Write(Idebug,*) ' subarea ', i
                   Write(Idebug,*) ' x   sD ',x, sD
                   Write(Idebug,*) ' PrecipReduction ', PrecipReduction
                   Write(Idebug,*) ' EvapReduction   ', EvapReduction
                Endif
                LGSI_PrecipitationReduction(IRRRunoffSub, i) = PrecipReduction
                LGSI_Precipitation (IRRRunoffSub, i) = LGSIPrecipitation
                LGSI_EvaporationReduction(IRRRunoffSub,i) = EvapReduction
                LGSI_Evaporation   (IRRRunoffSub,i)  = EvapReduction * LGSIPotEvap
                LGSI_PotEvap       (IRRRunoffSub,i)  = LGSIPotEvap
                RiverOutflow = 0.0D0
                Overlandflow = 0.0D0
                DrainageFlow = 0.0D0
                If (LGSI_Type(IRRRunoffSub,i) .eq. 1) then  !river and overland flow: only for normal distribution
                    Do k=1,LGSI_MaxInterpLengthPlus1
                       LevelDrainArray(k) = LGSI_InterpGwLevelDrain(k,iRRRunoffsub,i)
                       QuickFlowArray(k)  = LGSI_InterpQuickFlow(k,iRRRunoffsub,i)
                       OverlandFlowArray(k) = LGSI_InterpOverlandFlow(k,iRRRunoffsub,i)
                       DrainageFlowArray(k) = LGSI_InterpDrainageFlow(k,iRRRunoffsub,i)
                    Enddo
                    Call RR_D_INTERP (LGSI_MaxInterpLength, LevelDrainArray, QuickFlowArray, LGSIGWL,RiverOutflow,Ilast1)
                    Call RR_D_INTERP (LGSI_MaxInterpLength, LevelDrainArray, OverlandFlowArray,LGSIGWL,OverlandFlow,Ilast1)
                endif
                ! drainage flow: both for normal and gamma type
                Call RR_D_INTERP (LGSI_MaxInterpLength, LevelDrainArray, DrainageFlowArray, LGSIGWL,DrainageFlow,Ilast1)
! all to mm/timestep instead of mm/hour
!               Precipitation, evaporation already ok
                RiverOutFlow = RiverOutFlow * FractHour
                OverlandFlow = OverlandFlow * FractHour
                DrainageFlow = DrainageFlow * FractHour
!               LGSI_Qout done already after reading input
                if (i .eq. 1) then
                   TotalInOutFlow(1) = ( (1.0D0 -LGSI_PrecipitationReduction(IRRRunoffSub, i))*LGSIPrecipitation - LGSI_EvaporationReduction(IRRRunoffSub,i)*LGSIPotEvap) * mm2m - RiverOutflow - OverlandFlow - DrainageFlow + LGSIInterflow * mm2m * RatioArea - LGSI_Qout(IRRRunoffSub,i) * mm2m
                elseif (i .eq. 2) then
                   TotalInOutFlow(2) = ( (1.0D0 -LGSI_PrecipitationReduction(IRRRunoffSub, i))*LGSIPrecipitation - LGSI_EvaporationReduction(IRRRunoffSub,i)*LGSIPotEvap) * mm2m - RiverOutflow - OverlandFlow - DrainageFlow - LGSIInterflow * mm2m - LGSI_Qout(IRRRunoffSub,i) * mm2m
                endif
                StorageChange = TotalInOutflow(i)
                LGSIVolume = LGSIInitVolume(i) + StorageChange
                Do k=1,LGSI_MaxInterpLength
                   TempVolume(k) = -1.0D0 * LGSI_InterpTotalVolume(k,IRRRunoffSub,i)
                Enddo
                Do k=1,LGSI_MaxInterpLengthPlus1
                   GWLevelArray(k)  = LGSI_InterpGwLevelTotalV(k,iRRRunoffsub,i)
                   GWVolumeArray(k) = LGSI_InterpTotalVolume(k,iRRRunoffsub,i)
                Enddo
                Call RR_D_INTERP (LGSI_MaxInterpLength, TempVolume, GWLevelArray, -1.0D0*LGSIVolume,NewGwLevel,Ilast1)
                if (idebug .ne. 0) then
                   Write(Idebug,*) ' Input in interpolation: ', -1.0D0 * LGSIVolume
                   Write(Idebug,*) ' Array Vol-Lvl   k  volume  level '
                   Do k=1,LGSI_MaxInterpLength
                      Write(Idebug,*) k,TempVolume(k), LGSI_InterpGwLevelTotalV(k,IRRRunoffSub,i)
                   Enddo
                   Write(Idebug,*) ' Result ', NewGwLevel
                   Write(Idebug,*) ' average (init) LGSIGWL ', LGSIGwl
                   Write(Idebug,*) ' RiverOutflow ', RiverOutflow
                   Write(Idebug,*) ' Overlandflow ', Overlandflow
                   Write(Idebug,*) ' Drainageflow ', Drainageflow
                   Write(Idebug,*) ' TotalInOutflow ', TotalInOutflow(i)
                   Write(Idebug,*) ' LGSIVolume   ', LGSIVolume
                   Write(Idebug,*) ' LGSI_NewGWL  ', NewGwLevel
                Endif
                LGSI_NewVolume (IRRRunoffSub,i)   = LGSIVolume
                LGSI_NewGWL(IRRRunoffSub,i)       = NewGwLevel
                LGSI_Drainage(IRRRunoffSub,i)     = DrainageFlow * 1000.0D0
                LGSI_GWFlow      (IRRRunoffSub,i) = LGSIInterflow / 1000.0D0 * RatioArea
                LGSI_OverlandFlow(IRRRunoffSub,i) = OverlandFlow * 1000.0D0
                LGSI_QuickFlow(IRRRunoffSub,i)    = RiverOutflow * 1000.0D0
                LGSI_QpDirect(IRRRunoffSub,i)     = LGSI_PrecipitationReduction(IRRRunoffSub, i) * LGSIPrecipitation
                LGSI_Recharge(IRRRunoffSub,i)     = ((1.0D0 -LGSI_PrecipitationReduction(IRRRunoffSub, i))*LGSIPrecipitation - LGSI_EvaporationReduction(IRRRunoffSub, i)*LGSIPotEvap)
              Enddo

              LGSIRunoff = 0.D0
              LGSITotalRUnoff = 0.D0
              LGSIConverged = .true.
              Do i=1,LGSI_NrSubAreas(IRRRunoffSub)
                 LGSIRunoff = LGSI_Drainage(IRRRunoffSub,i) + LGSI_OverlandFlow(IRRRunoffSub,i) + LGSI_Quickflow(IRRRunoffSub,i) + LGSI_QpDirect(IRRRunoffSub,i)
                 if (i .eq. 1) LGSITotalRunoff = LGSIRunoff * RatioArea1
                 if (i .eq. 2) LGSITotalRunoff = LGSITotalRunoff + LGSIRunoff * RatioArea2
                 LGSIConverged = LGSIConverged .and.  (DAbs(LGSI_NewGwl(IRRRunoffSub,i) - LGSIPreviousGWL(i)) .le. 1.D-8)
              Enddo
              Call DelayFunction(IRRRunoffSub, LGSITotalRunoff, LGSIDelayedRunoff)
              LGSIConverged = (LGSIConverged .and. (DAbs(LGSITotalRunoff-LGSIPreviousTotalRunoff) .le. 1.D-8)) .or. (iter .ge. 100)
              if (idebug .ne. 0) then
                  Write(Idebug,*) ' LGSITotalRunoff ', LGSITotalRunoff
                  Write(Idebug,*) ' LGSIPreviousTotalRunoff ', LGSIPreviousTotalRunoff
                  Write(Idebug,*) ' iter  ', iter
              Endif
           enddo
          ! store output in LGSI arrays
           if (idebug .ne. 0) then
              Write(Idebug,*) ' LGSIPotEvap         ', itmstp, LGSIPotEvap
              Write(Idebug,*) ' LGSIPrecipitation   ', itmstp, LGSIPrecipitation
              Do i=1,LGSI_NrSubAreas(IRRRunoffSub)
                 Write(Idebug,*) ' Initial Volume       ', itmstp, i, LGSI_InitVolume(IRRRunoffSub,i)
                 Write(Idebug,*) ' New Total Volume     ', itmstp, i, LGSI_NewVolume(IRRRunoffSub,i)
                 Write(Idebug,*) ' Initial grw depth    ', itmstp, i, LGSI_InitGwl (IRRRunoffSub,i)
                 Write(Idebug,*) ' Groundwater depth    ', itmstp, i, LGSI_NewGwl  (IRRRunoffSub,i)
                 Write(Idebug,*) ' Recharge (mm/ts)     ', itmstp, i, LGSI_Recharge(IRRRunoffSub,i)
                 Write(Idebug,*) ' Qriver   (mm/ts)     ', itmstp, i, LGSI_Quickflow(IRRRunoffSub,i)
                 Write(Idebug,*) ' Qoverland(mm/ts)     ', itmstp, i, LGSI_OverlandFlow(IRRRunoffSub,i)
                 Write(Idebug,*) ' Drainage (mm/ts)     ', itmstp, i, LGSI_Drainage(IRRRunoffSub,i)
                 Write(Idebug,*) ' Precipitation (mm/ts)', itmstp, i, LGSI_Precipitation(IRRRunoffSub,i)
                 Write(Idebug,*) ' Evaporation   (mm/ts)', itmstp, i, LGSI_Evaporation(IRRRunoffSub,i)
                 Write(Idebug,*) ' LGSI_Type            ', itmstp, i, LGSI_Type(IRRRunoffSub,i)
                 Write(Idebug,*) ' PrecipReduction      ', itmstp, i, LGSI_PrecipitationReduction(IRRRunoffSub,i)
                 Write(Idebug,*) ' EvapReduction        ', itmstp, i, LGSI_EvaporationReduction(IRRRunoffSub,i)
                 Write(Idebug,*) ' Seepage Flow  (mm/ts)', itmstp, i, LGSIInterflow * 1000. * RatioArea
                 Write(Idebug,*) ' Outflow (abstr.mm/ts)', itmstp, i, LGSI_Qout(IRRRunoffSub,i)
                 Write(Idebug,*) ' QpDirect      (mm/ts)', itmstp, i, LGSI_QpDirect(IRRRunoffSub,i)
                 Write(Idebug,*) ' TotalRunoff   (mm/ts)', itmstp, i, LGSITotalRunoff
                 Write(Idebug,*) ' DelayedRunoff (mm/ts)', itmstp, i, LGSIDelayedRunoff
              Enddo
           Endif
           LGSI_Runoff(IRRRunoffSub) = LGSIDelayedRunoff * Area_RRRunoffNode(IRRRunoff) * mm2m / timeSettings%TimestepSize
           QF2 = LGSI_Runoff(IRRRunoffSub)
           RRRunoffNode_Outflow(iRRRunoff) = LGSI_Runoff(IRRRunoffSub)
      elseif (RRRunoff_CompOption(IRRRunoff) .eq. 5) then
!         Runoff node option 5: Wageningen model
          if (idebug .ne. 0) then
             Write(Idebug,*) ' Rain (m3/s) ', Rain(imeteo)
             Write(Idebug,*) ' Evap (m3/s) ', Evap(imeteo)
             if (Ievap .gt. 0) Write(Idebug,*) ' Evap via rainfall data station Ievap (m3/s) ', Rain(ievap)
          Endif
          Wagmod_P(IRRRunoffSub)   = AAFNodeRainfall(inode) * Rain(imeteo) * TimeSettings%Timestepsize / mm2m   ! in mm
          Wagmod_ETG(IRRRunoffSub) = Evap(imeteo) * TimeSettings%Timestepsize / mm2m   ! in mm
          if (IEvap .gt. 0) WagMod_ETG (IrrRunoffSub) = Rain(Ievap) * TimeSettings%Timestepsize / mm2m   ! in mm

          ! initialize actual/computed evap to input (potential evap)
          WagMod_ETA (IRRRunoffSub) = WagMod_ETG(IRRRunoffSub)
          WagMod_ET  (IRRRunoffSub) = WagMod_ETG(IRRRunoffSub)
          ! end hack
          Call Wagmod_DivCn2(IRRRunoffSub, Itmstp, Idebug)
          WagMod_Runoff(IRRRunoffSub) = WagMod_QC(IRRRunoffSub) * Area_RRRunoffNode(IRRRunoff) * mm2m / timeSettings%TimestepSize
          QF2 = WagMod_Runoff(IRRRunoffSub)
          RRRunoffNode_Outflow(iRRRunoff) = QF2

          TempVol = 0.
          Do i = 2, Wagmod_MaxNrTimestepsSimulation
             TempVol = TempVol + WagMod_QD(IRRRunoffSub,i)
          Enddo
          Wagmod_RoutVol(IRRRunoffSub) = TempVol

          if (idebug .ne. 0) then
              Write(Idebug,*) ' Wagmod_Rainfallstation', NamMet(Inode), imeteo
              Write(Idebug,*) ' Wagmod_Evaporationstation', NamEvap(Inode), ievap
              Write(Idebug,*) ' Wagmod_PotEvap       ', itmstp, Wagmod_ETG(IRRRunoffSub)
              Write(Idebug,*) ' Wagmod_ActEvap       ', itmstp, WagMod_ETA(IRRRunoffSub)
              Write(Idebug,*) ' Wagmod_Precipitation ', itmstp, Wagmod_P(IRRRunoffSub)
              Write(Idebug,*) ' Initial gw volume    ', itmstp, Wagmod_GStoreT1(IRRRunoffSub)
              Write(Idebug,*) ' Final gw volume      ', itmstp, Wagmod_GStore (IRRRunoffSub)
              Write(Idebug,*) ' Initial sm volume    ', itmstp, Wagmod_SMT1(IRRRunoffSub)
              Write(Idebug,*) ' Final sm volume      ', itmstp, Wagmod_SM (IRRRunoffSub)
              Write(Idebug,*) ' Eff. precip.  (mm/ts)', itmstp, Wagmod_PEF(IRRRunoffSub)
              Write(Idebug,*) ' Division PEF to J/CD ', itmstp, Wagmod_DIV(IRRRunoffSub)
              Write(Idebug,*) ' Eff. precip. J(mm/ts)', itmstp, Wagmod_PEFJ(IRRRunoffSub)
              Write(Idebug,*) ' Eff. precip.CD(mm/ts)', itmstp, Wagmod_PEFCD(IRRRunoffSub)
              Write(Idebug,*) ' Capil. rise   (mm/ts)', itmstp, Wagmod_CAP(IRRRunoffSub)
!              Write(Idebug,*) ' Direct runoff (mm/ts)', itmstp, Wagmod_QD(IRRRunoffSub,itmstp)
              Write(Idebug,*) ' Direct runoff (mm/ts)', itmstp, Wagmod_QD(IRRRunoffSub,1)
!              Write(Idebug,*) ' GW outflow    (mm/ts)', itmstp, Wagmod_QG(IRRRunoffSub,itmstp)
              Write(Idebug,*) ' GW outflow    (mm/ts)', itmstp, Wagmod_QG(IRRRunoffSub,1)
              Write(Idebug,*) ' total outflow (mm/ts)', itmstp, Wagmod_QC(IRRRunoffSub)
              Write(Idebug,*) ' total outflow (m3/s) ', itmstp, Wagmod_Runoff(IRRRunoffSub)
              Write(Idebug,*) ' Seepage Flow  (mm/ts)', itmstp, WagMod_Seep(IRRRunoffSub)
              Write(Idebug,*) ' QSNEW                ',         WagMod_QSNEW(IRRRunoffSub)
              Write(Idebug,*) ' RUJREST              ',         WagMod_RUJREST(IRRRunoffSub)
              Write(Idebug,*) ' Wagmod_RoutVol1      ',         WagMod_RoutVol1(IRRRunoffSub)
              Write(Idebug,*) ' Wagmod_RoutVol       ',         WagMod_RoutVol (IRRRunoffSub)
          Endif

      elseif (RRRunoff_CompOption(IRRRunoff) .eq. 6) then

!        Write(*,*) ' CHECKING timestep ', itmstp
!        only CHECKING whether all data to WALRUS was correctly set and kept ok!
         if (itmstp .le. 2) then
           retValWalrusCall = WalrusGet(IRRRunoffSub, wc_par_area, TempVar,WalrusFirst)
           if (retValWalrusCall .ne. 0) then
               call ErrMsgStandard (981, 0, ' Some error during WALRUS routine Get Area ', ' RRRunoffNode_Init1' )
           endif
!          Write(*,*) ' WalrusGet 11 Area ', TempVar, (Area_RRRunoffNode(IRRRunoff))/1000000.D0
! default numerical parameters
           retValWalrusCall = WalrusGet(IRRRunoffSub, wc_par_min_deltime, TempVar,WalrusFirst)
           if (retValWalrusCall .ne. 0) then
               call ErrMsgStandard (981, 0, ' Some error during WALRUS routine Get min_deltime', ' RRRunoffNode_Init1' )
           endif
!          Write(*,*) ' WalrusGet 41 min_deltime', TempVar, 60.D0
           retValWalrusCall = WalrusGet(IRRRunoffSub, wc_par_max_h_change, TempVar,WalrusFirst)
           if (retValWalrusCall .ne. 0) then
               call ErrMsgStandard (981, 0, ' Some error during WALRUS routine Get max_hchange ', ' RRRunoffNode_Init1' )
           endif
!          Write(*,*) ' WalrusGet 42 max_hchange', TempVar, 10.D0
           retValWalrusCall = WalrusGet(IRRRunoffSub, wc_par_min_h, TempVar,WalrusFirst)
           if (retValWalrusCall .ne. 0) then
               call ErrMsgStandard (981, 0, ' Some error during WALRUS routine Get minh ', ' RRRunoffNode_Init1' )
           endif
!          Write(*,*) ' WalrusGet 43 hmin ', TempVar, 1.0D-3
           retValWalrusCall = WalrusGet(IRRRunoffSub, wc_par_max_Pstep, TempVar,WalrusFirst)
           if (retValWalrusCall .ne. 0) then
               call ErrMsgStandard (981, 0, ' Some error during WALRUS routine Get max_Pstep ', ' RRRunoffNode_Init1' )
           endif
!          Write(*,*) ' WalrusGet 44 max_PStep ', TempVar, 10.D0
           retValWalrusCall = WalrusGet(IRRRunoffSub, wc_par_max_substeps, TempVar,WalrusFirst)
           if (retValWalrusCall .ne. 0) then
               call ErrMsgStandard (981, 0, ' Some error during WALRUS routine Get max_substeps ', ' RRRunoffNode_Init1' )
           endif
!          Write(*,*) ' WalrusGet 45 max_substeps', TempVar, 288.D0
! user defined parameters
           retValWalrusCall = WalrusGet(IRRRunoffSub, wc_par_cW, TempVar,WalrusFirst)
           if (retValWalrusCall .ne. 0) then
               call ErrMsgStandard (981, 0, ' Some error during WALRUS routine Get CW ', ' RRRunoffNode_Init1' )
           endif
!          Write(*,*) ' WalrusGet 1 CW', TempVar, Walrus_CW(IRRRunoffSub)
           retValWalrusCall = WalrusGet(IRRRunoffSub, wc_par_cV, TempVar,WalrusFirst)
           if (retValWalrusCall .ne. 0) then
               call ErrMsgStandard (981, 0, ' Some error during WALRUS routine Get CV ', ' RRRunoffNode_Init1' )
           endif
!          Write(*,*) ' WalrusGet 2 CV', TempVar, Walrus_CV(IRRRunoffSub)
           retValWalrusCall = WalrusGet(IRRRunoffSub, wc_par_cG, TempVar,WalrusFirst)
           if (retValWalrusCall .ne. 0) then
               call ErrMsgStandard (981, 0, ' Some error during WALRUS routine Get CG ', ' RRRunoffNode_Init1' )
           endif
!          Write(*,*) ' WalrusGet 3 CG', TempVar, Walrus_CG(IRRRunoffSub)
           retValWalrusCall = WalrusGet(IRRRunoffSub, wc_par_cQ, TempVar,WalrusFirst)
           if (retValWalrusCall .ne. 0) then
               call ErrMsgStandard (981, 0, ' Some error during WALRUS routine Get CQ ', ' RRRunoffNode_Init1' )
           endif
!          Write(*,*) ' WalrusGet 4 CQ', TempVar, Walrus_CQ(IRRRunoffSub)
           retValWalrusCall = WalrusGet(IRRRunoffSub, wc_par_cS, TempVar,WalrusFirst)
           if (retValWalrusCall .ne. 0) then
               call ErrMsgStandard (981, 0, ' Some error during WALRUS routine Get CS ', ' RRRunoffNode_Init1' )
           endif
!          Write(*,*) ' WalrusGet 5 CS', TempVar, Walrus_CS(IRRRunoffSub)
           retValWalrusCall = WalrusGet(IRRRunoffSub, wc_par_cD, TempVar,WalrusFirst)
           if (retValWalrusCall .ne. 0) then
               call ErrMsgStandard (981, 0, ' Some error during WALRUS routine Get CD ', ' RRRunoffNode_Init1' )
           endif
!          Write(*,*) ' WalrusGet 6 CD', TempVar, Walrus_CD(IRRRunoffSub)
           retValWalrusCall = WalrusGet(IRRRunoffSub,wc_par_cexpS, TempVar,WalrusFirst)
           if (retValWalrusCall .ne. 0) then
               call ErrMsgStandard (981, 0, ' Some error during WALRUS routine Get XS ', ' RRRunoffNode_Init1' )
           endif
!          Write(*,*) ' WalrusGet 12 XS', TempVar, Walrus_XS(IRRRunoffSub)
           retValWalrusCall = WalrusGet(IRRRunoffSub,wc_par_aS, TempVar,WalrusFirst)
           if (retValWalrusCall .ne. 0) then
               call ErrMsgStandard (981, 0, ' Some error during WALRUS routine Get AS ', ' RRRunoffNode_Init1' )
           endif
!          Write(*,*) ' WalrusGet 10 As', TempVar, Walrus_AS(IRRRunoffSub)
!          Write(*,*) ' END CHECKING timestep ', itmstp
         endif   ! end CHECKING

! update HSMin if time table
          if (Walrus_HST(IRRRunoffSub)) then
             Call GetHSminFromTable (Walrus_HSmin(IRRRunoffSub), IRRRunoffSub)
!            Write(*,*) ' WalrusSet HSMin'
             retValWalrusCall = WalrusSet(IRRRunoffSub,wc_hSmin, Walrus_HSMIN(IRRRunoffSub),WalrusFirst)
             if (retValWalrusCall .ne. 0) then
                call ErrMsgStandard (981, 0, ' Some error during WALRUS routine Set HSMin ', ' RRRunoffNode_Init1' )
             endif
          endif

!         Runoff node option 6: Walrus model
          ! Walrus.dostep (dble(TimeSettings%TimestepSize))
!         Write(*,*) ' Walrus_DOStep;IRRRunoffSub -timestep size', IRRRunoffSub, dble(TimeSettings%TimestepSize)
          RetValWalrusCall = WALRUSDOSTEP (IRRRunoffSub, dble(TimeSettings%TimestepSize), WalrusFirst)
          if (retValWalrusCall .ne. 0) then
              call ErrMsgStandard (981, 0, ' Some error during WALRUS routine DoStep', ' RRRunoffNode_Init1')
          endif

          ! extract Walrus output
          ! current time
          ! Walrus.get (60, Cur_time)
! StartTime
          retValWalrusCall = WalrusGet (IRRRunoffSub, wc_cur_time, Cur_Time, WalrusFirst)
          if (retValWalrusCall .ne. 0) then
              call ErrMsgStandard (981, 0, ' Some error during WALRUS routine Get DV', ' RRRunoffNode_Init1')
          endif
!         Write(*,*) ' Got WalrusStartTime', Cur_time
          ! states
          ! Walrus.get (61, Walrus_DVcurrent(IRRRunoffSub)
          RetValWalrusCall = WALRUSGet (IRRRunoffSub, wc_cur_dV, Walrus_DVcurrent(IRRRunoffSub), WalrusFirst)
          if (retValWalrusCall .ne. 0) then
              call ErrMsgStandard (981, 0, ' Some error during WALRUS routine Get DV', ' RRRunoffNode_Init1')
          endif
!         Write(*,*) ' Got DV ', Walrus_DVCurrent(IRRRunoffSub)
          ! Walrus.get (62, Walrus_DGcurrent(IRRRunoffSub)
          RetValWalrusCall = WALRUSGet (IRRRunoffSub, wc_cur_dG, Walrus_DGcurrent(IRRRunoffSub), WalrusFirst)
          if (retValWalrusCall .ne. 0) then
              call ErrMsgStandard (981, 0, ' Some error during WALRUS routine Get DG', ' RRRunoffNode_Init1')
          endif
!         Write(*,*) ' Got DG ', Walrus_DGCurrent(IRRRunoffSub)
          ! Walrus.get (63, Walrus_hQcurrent(IRRRunoffSub)
          RetValWalrusCall = WALRUSGet (IRRRunoffSub, wc_cur_hQ, Walrus_hQcurrent(IRRRunoffSub), WalrusFirst)
          if (retValWalrusCall .ne. 0) then
              call ErrMsgStandard (981, 0, ' Some error during WALRUS routine Get hQ', ' RRRunoffNode_Init1')
          endif
!         Write(*,*) ' Got hQ ', Walrus_hQCurrent(IRRRunoffSub)
          ! Walrus.get (64, Walrus_hScurrent(IRRRunoffSub)
          RetValWalrusCall = WALRUSGet (IRRRunoffSub, wc_cur_hS, Walrus_hScurrent(IRRRunoffSub), WalrusFirst)
          if (retValWalrusCall .ne. 0) then
              call ErrMsgStandard (981, 0, ' Some error during WALRUS routine Get hS', ' RRRunoffNode_Init1')
          endif
!         Write(*,*) ' Got hS ', Walrus_hSCurrent(IRRRunoffSub)
          ! functions
          ! Walrus.get (71, Walrus_WIcurrent(IRRRunoffSub)
          RetValWalrusCall = WALRUSGet (IRRRunoffSub, wc_cur_W, Walrus_WIcurrent(IRRRunoffSub), WalrusFirst)
          if (retValWalrusCall .ne. 0) then
              call ErrMsgStandard (981, 0, ' Some error during WALRUS routine Get WI', ' RRRunoffNode_Init1')
              endif
!         Write(*,*) ' Got WI ', Walrus_WICurrent(IRRRunoffSub)
          ! Walrus.get (72, Walrus_BETAcurrent(IRRRunoffSub)
          RetValWalrusCall = WALRUSGet (IRRRunoffSub, wc_cur_beta, Walrus_BETAcurrent(IRRRunoffSub), WalrusFirst)
          if (retValWalrusCall .ne. 0) then
              call ErrMsgStandard (981, 0, ' Some error during WALRUS routine Get BETA', ' RRRunoffNode_Init1')
          endif
!         Write(*,*) ' Got Beta ', Walrus_BETACurrent(IRRRunoffSub)
          ! Walrus.get (73, Walrus_DVEQcurrent(IRRRunoffSub)
          RetValWalrusCall = WALRUSGet (IRRRunoffSub, wc_cur_dVeq, Walrus_DVEQcurrent(IRRRunoffSub), WalrusFirst)
          if (retValWalrusCall .ne. 0) then
              call ErrMsgStandard (981, 0, ' Some error during WALRUS routine Get DVEQ', ' RRRunoffNode_Init1')
          endif
!         Write(*,*) ' Got DVEQ ', Walrus_DVEQCurrent(IRRRunoffSub)
          ! Walrus.get (80, Walrus_lastdt   (IRRRunoffSub)
          RetValWalrusCall = WALRUSGet (IRRRunoffSub, wc_last_deltime, Walrus_lastdt   (IRRRunoffSub), WalrusFirst)
          if (retValWalrusCall .ne. 0) then
              call ErrMsgStandard (981, 0, ' Some error during WALRUS routine Get lastdt', ' RRRunoffNode_Init1')
          endif
!         Write(*,*) ' Got lastdt ', Walrus_lastdt(IRRRunoffSub)
          ! Walrus.get (81, Walrus_lastFXG  (IRRRunoffSub)
          RetValWalrusCall = WALRUSGet (IRRRunoffSub, wc_last_fXG, Walrus_lastFXG  (IRRRunoffSub), WalrusFirst)
          if (retValWalrusCall .ne. 0) then
              call ErrMsgStandard (981, 0, ' Some error during WALRUS routine Get lastFXG', ' RRRunoffNode_Init1')
          endif
!         Write(*,*) ' Got lastFXG ', Walrus_lastFXG(IRRRunoffSub)
          ! Walrus.get (82, Walrus_lastFXS  (IRRRunoffSub)
          RetValWalrusCall = WALRUSGet (IRRRunoffSub, wc_last_fXS, Walrus_lastFXS  (IRRRunoffSub), WalrusFirst)
          if (retValWalrusCall .ne. 0) then
              call ErrMsgStandard (981, 0, ' Some error during WALRUS routine Get lastFXS', ' RRRunoffNode_Init1')
          endif
!         Write(*,*) ' Got lastFXS ', Walrus_lastFXS(IRRRunoffSub)
          ! Walrus.get (83, Walrus_lastPQ   (IRRRunoffSub)
          RetValWalrusCall = WALRUSGet (IRRRunoffSub, wc_last_PQ, Walrus_lastPQ   (IRRRunoffSub), WalrusFirst)
          if (retValWalrusCall .ne. 0) then
              call ErrMsgStandard (981, 0, ' Some error during WALRUS routine Get lastPQ', ' RRRunoffNode_Init1')
          endif
!         Write(*,*) ' Got lastPQ ', Walrus_lastPQ(IRRRunoffSub)
          ! Walrus.get (84, Walrus_lastPV   (IRRRunoffSub)
          RetValWalrusCall = WALRUSGet (IRRRunoffSub, wc_last_PV, Walrus_lastPV   (IRRRunoffSub), WalrusFirst)
          if (retValWalrusCall .ne. 0) then
              call ErrMsgStandard (981, 0, ' Some error during WALRUS routine Get lastPV', ' RRRunoffNode_Init1')
          endif
!         Write(*,*) ' Got lastPV ', Walrus_lastPV(IRRRunoffSub)
          ! Walrus.get (85, Walrus_lastPS   (IRRRunoffSub)
          RetValWalrusCall = WALRUSGet (IRRRunoffSub, wc_last_PS, Walrus_lastPS   (IRRRunoffSub), WalrusFirst)
          if (retValWalrusCall .ne. 0) then
              call ErrMsgStandard (981, 0, ' Some error during WALRUS routine Get lastPS', ' RRRunoffNode_Init1')
          endif
!         Write(*,*) ' Got lastPS ', Walrus_lastPS(IRRRunoffSub)
          ! Walrus.get (86, Walrus_lastETV  (IRRRunoffSub)
          RetValWalrusCall = WALRUSGet (IRRRunoffSub, wc_last_ETV, Walrus_lastETV  (IRRRunoffSub), WalrusFirst)
          if (retValWalrusCall .ne. 0) then
              call ErrMsgStandard (981, 0, ' Some error during WALRUS routine Get lastETV', ' RRRunoffNode_Init1')
          endif
!         Write(*,*) ' Got lastETV ', Walrus_lastETV(IRRRunoffSub)
          ! Walrus.get (87, Walrus_lastETS  (IRRRunoffSub)
          RetValWalrusCall = WALRUSGet (IRRRunoffSub, wc_last_ETS, Walrus_lastETS  (IRRRunoffSub), WalrusFirst)
          if (retValWalrusCall .ne. 0) then
              call ErrMsgStandard (981, 0, ' Some error during WALRUS routine Get lastETS', ' RRRunoffNode_Init1')
          endif
!         Write(*,*) ' Got lastETS ', Walrus_lastETS(IRRRunoffSub)
          ! Walrus.get (88, Walrus_lastETAct(IRRRunoffSub)
          RetValWalrusCall = WALRUSGet (IRRRunoffSub, wc_last_ETact, Walrus_lastETAct(IRRRunoffSub), WalrusFirst)
          if (retValWalrusCall .ne. 0) then
              call ErrMsgStandard (981, 0, ' Some error during WALRUS routine Get lastETAct', ' RRRunoffNode_Init1')
          endif
!         Write(*,*) ' Got lastETAct ', Walrus_lastETAct(IRRRunoffSub)
          ! Walrus.get (89, Walrus_lastFQS  (IRRRunoffSub)
          RetValWalrusCall = WALRUSGet (IRRRunoffSub, wc_last_fQS, Walrus_lastFQS  (IRRRunoffSub), WalrusFirst)
          if (retValWalrusCall .ne. 0) then
              call ErrMsgStandard (981, 0, ' Some error during WALRUS routine Get lastFQS', ' RRRunoffNode_Init1')
          endif
!         Write(*,*) ' Got lastFQS ', Walrus_lastFQS(IRRRunoffSub)
          ! Walrus.get (90, Walrus_lastFGS  (IRRRunoffSub)
          RetValWalrusCall = WALRUSGet (IRRRunoffSub, wc_last_fGS, Walrus_lastFGS  (IRRRunoffSub), WalrusFirst)
          if (retValWalrusCall .ne. 0) then
              call ErrMsgStandard (981, 0, ' Some error during WALRUS routine Get lastFGS', ' RRRunoffNode_Init1')
          endif
!         Write(*,*) ' Got lastFGS ', Walrus_lastFGS(IRRRunoffSub)
          ! Walrus.get (91, Walrus_lastQ    (IRRRunoffSub)
          RetValWalrusCall = WALRUSGet (IRRRunoffSub, wc_last_Q, Walrus_lastQ    (IRRRunoffSub), WalrusFirst)
          if (retValWalrusCall .ne. 0) then
              call ErrMsgStandard (981, 0, ' Some error during WALRUS routine Get lastQ', ' RRRunoffNode_Init1')
          endif
!         Write(*,*) ' Got lastQ ', Walrus_lastQ(IRRRunoffSub)
          ! Walrus.get (92, Walrus_lastQdis (IRRRunoffSub)
!          Write(*,*) ' Walrus_Get lastQDis'
!          RetValWalrusCall = WALRUSGet (IRRRunoffSub, wc_last_Qdischarge, Walrus_lastQDis (IRRRunoffSub), WalrusFirst)
!          if (retValWalrusCall .ne. 0) then
!              call ErrMsgStandard (981, 0, ' Some error during WALRUS routine Get lastQDis', ' RRRunoffNode_Init1')
!          endif
!          Write(*,*) ' Got lastQDis ', Walrus_lastQDis(IRRRunoffSub)
!          compute lastQdis in m3/s from lastQ in mm
          Walrus_lastQDis (IRRRunoffSub) = Walrus_lastQ(IRRRUnoffSub) * Area_RRRunoffNode(IRRRunoff) * mm2m / timeSettings%TimestepSize

          RetValWalrusCall = WALRUSGet (IRRRunoffSub, wc_last_fXSact, Walrus_FXSAct (IRRRunoffSub), WalrusFirst)
          if (retValWalrusCall .ne. 0) then
              call ErrMsgStandard (981, 0, ' Some error during WALRUS routine Get P', ' RRRunoffNode_Init1')
          endif

          ! Walrus.get (94, Walrus_lastP    (IRRRunoffSub)
          RetValWalrusCall = WALRUSGet (IRRRunoffSub, wc_last_P, Walrus_P    (IRRRunoffSub), WalrusFirst)
          if (retValWalrusCall .ne. 0) then
              call ErrMsgStandard (981, 0, ' Some error during WALRUS routine Get P', ' RRRunoffNode_Init1')
          endif
!         Write(*,*) ' Got P ', Walrus_P(IRRRunoffSub)
          RetValWalrusCall = WALRUSGet (IRRRunoffSub, wc_last_ETPot, Walrus_ETPot (IRRRunoffSub), WalrusFirst)
          if (retValWalrusCall .ne. 0) then
              call ErrMsgStandard (981, 0, ' Some error during WALRUS routine Get ETPOT', ' RRRunoffNode_Init1')
          endif
!         Write(*,*) ' Got ETPot ', Walrus_ETPot(IRRRunoffSub)

          if (idebug .ne. 0) then
             Write(Idebug,*) ' Rain (m3/s) ', Rain(imeteo)
             Write(Idebug,*) ' Evap (m3/s) ', Evap(imeteo)
             if (Ievap .gt. 0) Write(Idebug,*) ' Evap via rainfall data station Ievap (m3/s) ', Rain(ievap)
          Endif

          QF2 = Walrus_lastQDis (IRRRunoffSub)
          RRRunoffNode_Outflow(iRRRunoff) = QF2

!         Walrus_P (IRRRunoffSub) = AAFNodeRainfall(inode) * Rain(imeteo) * TimeSettings%Timestepsize / mm2m   ! in mm/timestep ; note rain(imeteo) is in m/s
          ! ievap = NodEvap(ievap) al in NodeLP
!         Walrus_ETPot (IRRRunoffSub) = Rain(ievap) / mm2m *  timeSettings%TimestepSize
          Walrus_ETAct (IRRRunoffSub) = Walrus_lastETAct(IRRRunoffSub)
          Walrus_FXSDef(IRRRunoffSub) = Walrus_lastFXS(IRRRunoffSub)
!          Walrus_FXSAct(IRRRunoffSub) = Walrus_lastFXS(IRRRunoffSub)  ! not needed, directly read into this variable
          Walrus_FXGAct(IRRRunoffSub) = Walrus_lastFXG(IRRRunoffSub)
          if (idebug .ne. 0) then
              Write(Idebug,*) ' Walrus_Rainfallstation', NamMet(Inode), NodMet(inode)
              Write(Idebug,*) ' Walrus_Evaporationstation', NamEvap(Inode), NodEvap(inode)
              Write(Idebug,*) ' Walrus_sw abstraction station', NamFXS(Inode), NodFXS(inode)
              Write(Idebug,*) ' Walrus_gw abstraction station', NamFXG(Inode), NodFXG(inode)
              Write(Idebug,*) ' Walrus_Precipitation ', itmstp, Walrus_P     (IRRRunoffSub)
              Write(Idebug,*) ' Walrus_PotEvap       ', itmstp, Walrus_ETPot  (IRRRunoffSub)
              Write(Idebug,*) ' Walrus_ActEvap       ', itmstp, Walrus_ETAct(IRRRunoffSub)
              Write(Idebug,*) ' total outflow (mm/ts)  ', itmstp, Walrus_LastQ(IRRRunoffSub)
              Write(Idebug,*) ' total outflow (m3/s)   ', itmstp, Walrus_LastQDis(IRRRunoffSub)
              Write(Idebug,*) ' Walrus_DV storage defic', itmstp, Walrus_DVCurrent(IRRRunoffSub)
              Write(Idebug,*) ' Walrus_DG gw depth     ', itmstp, Walrus_DGCurrent(IRRRunoffSub)
              Write(Idebug,*) ' Walrus_hQ level quickfl', itmstp, Walrus_hQCurrent(IRRRunoffSub)
              Write(Idebug,*) ' Walrus_hS level surfwat', itmstp, Walrus_hSCurrent(IRRRunoffSub)
              Write(Idebug,*) ' Walrus_lastFXS         ', itmstp, Walrus_lastFXS(IRRRunoffSub)
              Write(Idebug,*) ' Walrus_lastFXG         ', itmstp, Walrus_lastFXG(IRRRunoffSub)
              Write(Idebug,*) ' Walrus_FXSDef        ', itmstp, Walrus_FXSDef (IRRRunoffSub)
              Write(Idebug,*) ' Walrus_FXSAct        ', itmstp, Walrus_FXSAct (IRRRunoffSub)
              Write(Idebug,*) ' Walrus_FXGAct        ', itmstp, Walrus_FXGAct (IRRRunoffSub)
              Write(Idebug,*) ' Walrus_PQ              ', itmstp, Walrus_lastPQ(IRRRunoffSub)
              Write(Idebug,*) ' Walrus_PV              ', itmstp, Walrus_lastPV(IRRRunoffSub)
              Write(Idebug,*) ' Walrus_PS              ', itmstp, Walrus_lastPS(IRRRunoffSub)
              Write(Idebug,*) ' Walrus_ETV             ', itmstp, Walrus_lastETV(IRRRunoffSub)
              Write(Idebug,*) ' Walrus_ETS             ', itmstp, Walrus_lastETS(IRRRunoffSub)
              Write(Idebug,*) ' Walrus_ActEvap ETAct ', itmstp, Walrus_lastETAct(IRRRunoffSub)
              Write(Idebug,*) ' Walrus_FQS             ', itmstp, Walrus_lastFQS(IRRRunoffSub)
              Write(Idebug,*) ' Walrus_FGS             ', itmstp, Walrus_lastFGS(IRRRunoffSub)
              Write(Idebug,*) ' Walrus_WI wetness index', itmstp, Walrus_WICurrent(IRRRunoffSub)
              Write(Idebug,*) ' Walrus_BETA beta       ', itmstp, Walrus_BETACurrent(IRRRunoffSub)
          Endif

      else
!         unknown option
!         not yet implemented
          call ErrMsgStandard (981, 0, ' Unknown RR-Runoff node option; not available', ' ' )
      endif
! final situation

      If (Ibnd .gt. 0) then
         QBND(IBND) = QBND(IBND) + QF2
         QINBND(IBND)%totalSacramento = QINBND(IBND)%totalSacramento + QF2
      ElseIf (IPluv .gt. 0) then
         QinPluv(IPluv) = QInPluv(IPluv) + QF2
         QPluv(IPluv)%totalSacramento = QPluv(IPluv)%totalSacramento + QF2
      ElseIf (Iow .gt. 0) then
         QINOw(IOw,7) = QINOw(Iow,7) + QF2
      ElseIf (IBifur .gt. 0) then
         QINLink(ilink) = QF2
         if (LinkType(DownstreamLinkNr(inode)) .ne. 30) then
            QBifur(iBifur) = QBifur(Ibifur) + QF2
         endif
      ElseIf (IConn .gt. 0) then
         QINLink(ilink) = QF2
         if (LinkType(DownstreamLinkNr(inode)) .ne. 30) then
            QinConn(iConn)%TotalConnection = QinConn(iConn)%TotalConnection + QinLink(ilink)
            QConn(iConn) = QConn(iConn) + Qinlink(ilink)
         else
            ! will be done in RRRoutingLink
         endif
      Endif

      IF (iDebug .ne. 0) THEN
         WRITE(IDEBUG,*) ' Channel Inflow (m3/s)       :', QF2
      ENDIF


    ! *********************************************************************
    ! *** End
    ! *********************************************************************

    RETURN
  END subroutine CMPRRRunoffNode


  Subroutine GetGwPumpFlowfromTable (GwPumpFlow, Inam)
    ! *********************************************************************
    ! *** Last update:  7 March 1997                      by: Geert Prinsen
    ! *********************************************************************
    ! *** Brief description:
    ! *** ------------------
    ! ***    Bepaal target level open water, en zomer/winter
    ! *********************************************************************
    ! *** Input/output parameters:
    ! *** ------------------------
    ! ***  TLVL   = target level
    ! ***  IOW    = index open water
    ! *********************************************************************

    INTEGER Inam, rowNr, TabelNr, Idebug, Iout1
    Double precision GwPumpFlow
    type (Date) currentDate
    type (Time) currentTime
    Logical DateTimeOutsideTable

    IOut1  = ConfFil_get_IOut1()
    Idebug = ConfFil_get_IDEBUG()

    currentDate%year = ConfArr_get_IYear()
    currentDate%month = ConfArr_get_iMonth()
    currentDate%day = ConfArr_get_iDay()
    currentTime%hour = ConfArr_get_iHour()
    currentTime%minute = ConfArr_get_iMinute()
    currentTime%second = 0
! nieuwe tabellen
    RowNr = -1
    TabelNr = NAMRefToGWPump_TTable(inam)
    GwPumpFlow = GetNewValue(TableHandle, TabelNr, 1, RowNr, CurrentDate, CurrentTime, &
                        Idebug, iout1, DateTimeOutsideTable, .true.)
    if (idebug .ne. 0) WRITE (IDEBUG,*) ' GwPump Inam rowNr GwPumpFlow', Inam, RowNr, GwPumpFlow

    RETURN
  END subroutine GetGwPumpFlowfromTable



  Subroutine WrInputDataRRRunoffNode (Iout9)

        ! *********************************************************************
        ! *** Last update:
        ! *********************************************************************
        ! *** Brief description:
        ! *** ------------------
        ! ***    Stuk uit sub WrData: uitvoer van RRRunoffNode nodes in *.Out files
        ! *********************************************************************

        Implicit none

!        Integer      INODE, IKIND, INR, idum
        Integer      IOUT9

! RRRunoffNode area


  Return
  End subroutine WrInputDataRRRunoffNode


  Subroutine LGSI_ConstructInterpolationTables

        ! *********************************************************************
        ! *** Last update:
        ! *********************************************************************
        ! *** Brief description:
        ! *** ------------------
        ! ***    Construct Interpolation tables gwl - storages, fluxes, depending on distribution
        ! *********************************************************************

        Implicit none

        Integer      INode, IRRRunoff, IRRRunoffSub
        Integer      ikind, i

! LGSI
        Do iNode=1,NcNode
           IKind = Einode(Inode,3)
           If (IKind .eq. 22) then   ! LGSI
              IRRRunoff = Einode(Inode,2)
              IRRRunoffSub = RRRunoff_SubIndex(IRRRunoff)
              Do i=1,LGSI_NrSubAreas(IRRRunoffSub)
                 If (LGSI_Type(IRRRunoffSub,i) .eq. 1) then  !normal distribution
                    Call ConstructNormalGwlGWVol (IRRRunoffSub,i)
                    Call ConstructNormalGwlUnsatVol (IRRRunoffSub,i)
                    Call ConstructNormalGwlSurfVol (IRRRunoffSub,i)
                    Call ConstructGwlTotalVol (IRRRunoffSub,i)
                    Call ConstructNormalGwlDrainFlow (IRRRunoffSub,i)
                 ElseIf (LGSI_Type(IRRRunoffSub,i) .eq. 2) then  !gamma distribution
                    Call ConstructGammaGwlGWVol (IRRRunoffSub,i)
                    Call ConstructGammaGwlUnsatVol (IRRRunoffSub,i)
                    Call ConstructGammaGwlSurfVol (IRRRunoffSub,i)
                    Call ConstructGwlTotalVol (IRRRunoffSub,i)
                    Call ConstructGammaGwlDrainFlow (IRRRunoffSub,i)
                 Endif
              Enddo
              If (LGSI_NrSubAreas(IRRRunoffSub) .eq. 2) Call ConstructGWLSeepageInterflow (IRRRunoffSub)
           Endif
        Enddo

  Return
  End subroutine LGSI_ConstructInterpolationTables


  Subroutine ConstructGammaGwlGWVol (IRRRunoffSub, i)

  implicit none

  integer IRRRunoffSub, i
  integer j
  double precision x, result
  double precision mean, Sd, Tets

  j = 0
  Tets = LGSI_Tets (IRRRunoffSub,i)
  do x = -10., 30.1, 0.1
     j = j+1
     LGSI_InterpGWLevelGWV(j,IRRRunoffSub,i) = x
     mean = x
     Call ComputePsdGamma (LGSI_GammaAg(IRRRunoffSub,i), LGSI_GammaBg(IRRRunoffSub,i), x, Sd)
     result = 30 * Tets - Tets * IntegrateGwVol(mean,Sd)
     LGSI_InterpGWVolume(j,IRRRunoffSub,i) = result
  enddo
  Return
  End subroutine ConstructGammaGWLGWVol


  Subroutine ConstructGammaGwlUnsatVol (IRRRunoffSub, i)

  implicit none

  integer IRRRunoffSub, i
  integer j
  double precision x, result
  double precision mean, Sd, Tets

  j = 0
  Tets = LGSI_Tets (IRRRunoffSub,i)
  do x = -10., 30.1, 0.1
     j = j+1
     LGSI_InterpGWLevelUnsatV(j,IRRRunoffSub,i) = x
     mean = x
     Call ComputePsdGamma (LGSI_GammaAg(IRRRunoffSub,i), LGSI_GammaBg(IRRRunoffSub,i), x, Sd)
     result = Tets *IntegrateUnsatVol(mean,Sd, LGSI_ALP(IRRRunoffSub,i), LGSI_N(IRRRunoffSub,i))
     LGSI_InterpUnsatVolume(j,IRRRunoffSub,i) = result
  enddo

  Return
  End subroutine ConstructGammaGWLUnsatVol


  Subroutine ConstructGammaGwlSurfVol (IRRRunoffSub, i)

  implicit none

  integer IRRRunoffSub, i
  integer j
  double precision mean, Sd
  double precision x, result

  j = 0

  do x = -10., 30.1, 0.1
!    write(*,*) ' GwlSurfVol x=', x
     j = j+1
     LGSI_InterpGWLevelSurfV(j,IRRRunoffSub,i) = x
     mean = x
!    write(*,*) ' GwlSurfVol call ComputePsdGamma '
     Call ComputePsdGamma (LGSI_GammaAg(IRRRunoffSub,i), LGSI_GammaBg(IRRRunoffSub,i), x, Sd)
!    write(*,*) ' GwlSurfVol call IntegrateSurfVol '
     result = IntegrateSurfVol(mean,Sd)
     LGSI_InterpSurfVolume(j,IRRRunoffSub,i) = result
  enddo

  Return
  End subroutine ConstructGammaGWLSurfVol


  Subroutine ConstructGammaGwlDrainFlow (IRRRunoffSub, i)

  implicit none

  integer IRRRunoffSub, i
  integer j
  double precision mean, Sd
  double precision x, result

  j = 0

  do x = -10, 30.1, 0.1
     j = j+1
     LGSI_InterpGWLevelDrain(j,IRRRunoffSub,i) = x
     mean   = x
     Call ComputePsdGamma (LGSI_GammaAg(IRRRunoffSub,i), LGSI_GammaBg(IRRRunoffSub,i), x, Sd)
!    write(*,*) 'Gamma IntegrateFluxDrain'
     result = IntegrateFluxDrain(x, Sd, LGSI_ASDR(IRRRunoffSub,i), LGSI_RDR(IRRRunoffSub,i), LGSI_Area(IRRRunoffSub,i),LGSI_ASD(IRRRunoffSub,i), LGSI_DDR(IRRRunoffSub,i))
     LGSI_InterpDrainageFlow(j,IRRRunoffSub,i)= result
  enddo

  Return
  End subroutine ConstructGammaGwlDrainFlow


  Subroutine ConstructGwlTotalVol (IRRRunoffSub, i)

  implicit none

  integer IRRRunoffSub, i
  integer j, k, ilast1, ilast2, ilast3
  double precision x
  double precision GWVol, UnsatVol, SurfVol

  Double precision  GWLevelArray(LGSI_MaxInterpLengthPlus1), GWVolumeArray (LGSI_MaxInterpLengthPlus1)
  Double precision  UnsatLevelArray(LGSI_MaxInterpLengthPlus1), UnsatVolumeArray (LGSI_MaxInterpLengthPlus1)
  Double precision  SurfLevelArray(LGSI_MaxInterpLengthPlus1), SurfVolumeArray (LGSI_MaxInterpLengthPlus1)


  do k=1, LGSI_MaxInterpLengthPlus1
     GWLevelArray(k)    = LGSI_InterpGwLevelGWV(k,iRRRunoffsub,i)
     GWVolumeArray(k)   = LGSI_InterpGWVolume(k,iRRRunoffsub,i)
     UnsatLevelArray(k) = LGSI_InterpGwLevelUnsatV(k,iRRRunoffsub,i)
     UnsatVolumeArray(k)= LGSI_InterpUnsatVolume(k,iRRRunoffsub,i)
     SurfLevelArray(k)  = LGSI_InterpGwLevelSurfV(k,iRRRunoffsub,i)
     SurfVolumeArray(k) = LGSI_InterpSurfVolume(k,iRRRunoffsub,i)
  enddo

  ilast1 = 1
  ilast2 = 1
  ilast3 = 1
  j = 0
  do x = -10, 30.1, 0.1
     j = j+1
     LGSI_InterpGWLevelTotalV(j,IRRRunoffSub,i) = x
     Call RR_D_INTERP (LGSI_MaxInterpLength, GWLevelArray, GWVolumeArray, X,GWvol,Ilast1)
     Call RR_D_INTERP (LGSI_MaxInterpLength, UnsatLevelArray, UnsatVolumeArray, X,UnsatVol,Ilast2)
     Call RR_D_INTERP (LGSI_MaxInterpLength, SurfLevelArray, SurfVolumeArray, X,SurfVol,Ilast3)
     LGSI_InterpTotalVolume(j,IRRRunoffSub,i) = GWVol + UnsatVol + SurfVol
  enddo

  Return
  End subroutine ConstructGwlTotalVol


  Subroutine ConstructNormalGwlGWVol (IRRRunoffSub, i)

  implicit none

  integer IRRRunoffSub, i
  integer j
  double precision x, result
  double precision mean, Sd, Tets

  j = 0
  Tets = LGSI_Tets (IRRRunoffSub,i)

  do x = -10, 30.1, 0.1
     j = j+1
     LGSI_InterpGWLevelGWV(j,IRRRunoffSub,i) = x
     mean = x
     Call ComputePsdNormal (LGSI_NormalMaxSD(IRRRunoffSub,i), LGSI_NormalMinSD(IRRRunoffSub,i), LGSI_NormalNusDMax(IRRRunoffSub,i), LGSI_NormalNb(IRRRunoffSub,i), x, Sd)
     result = 30*Tets - Tets * IntegrateGwVol(mean,Sd)
     LGSI_InterpGWVolume(j,IRRRunoffSub,i) = result
  enddo

  Return
  End subroutine ConstructNormalGWLGWVol


  Subroutine ConstructNormalGwlUnsatVol (IRRRunoffSub, i)


  implicit none

  integer IRRRunoffSub, i
  integer j
  double precision x, mean, sD, Tets, result

  j = 0
  Tets = LGSI_Tets (IRRRunoffSub,i)

   do x=-10,30.1,0.1
      j = j+1
      LGSI_InterpGWLevelUnsatV(j,IRRRunoffSub,i) = x
      mean = x
      Call ComputePsdNormal (LGSI_NormalMaxSD(IRRRunoffSub,i), LGSI_NormalMinSD(IRRRunoffSub,i), LGSI_NormalNuSdMax(IRRRunoffSub,i), LGSI_NormalNb(IRRRunoffSub,i), x, Sd)
      result = Tets * IntegrateUnsatVol(mean,Sd, LGSI_ALP(IRRRunoffSub,i), LGSI_N(IRRRunoffSub,i))
      LGSI_InterpUnsatVolume(j,IRRRunoffSub,i) = result
   enddo

  Return
  End subroutine ConstructNormalGWLUnsatVol


  Subroutine ConstructNormalGwlSurfVol (IRRRunoffSub, i)


  implicit none

  integer IRRRunoffSub, i
  integer j
  double precision x, mean, sD, result

  j = 0

  do x = -10.,30.1, 0.1
!    write(*,*) ' GwlSurfVol x=', x
     j = j+1
     LGSI_InterpGWLevelSurfV(j,IRRRunoffSub,i) = x
     mean = x
     Call ComputePsdNormal (LGSI_NormalMaxSD(IRRRunoffSub,i), LGSI_NormalMinSD(IRRRunoffSub,i), LGSI_NormalNusDMax(IRRRunoffSub,i), LGSI_NormalNb(IRRRunoffSub,i), x, Sd)
     result = IntegrateSurfVol(mean,Sd)
     LGSI_InterpSurfVolume(j,IRRRunoffSub,i) = result
  enddo

  Return
  End subroutine ConstructNormalGWLSurfVol



  Subroutine ConstructNormalGwlDrainFlow (IRRRunoffSub, i)


  implicit none
  integer IRRRunoffSub, i
  integer j
  double precision mean, Sd
  double precision x, result

  j = 0
!  do x=-10, 10.05, 0.05
  do x=-10, 30.1, 0.1
     j = j+1
     LGSI_InterpGWLevelDrain(j,IRRRunoffSub,i)= x
     mean = x
     Call ComputePsdNormal (LGSI_NormalMaxSD(IRRRunoffSub,i), LGSI_NormalMinSD(IRRRunoffSub,i), LGSI_NormalNusDMax(IRRRunoffSub,i), LGSI_NormalNb(IRRRunoffSub,i), x, Sd)
     result = IntegrateFluxDrain(x, Sd, LGSI_ASDR(IRRRunoffSub,i), LGSI_RDR(IRRRunoffSub,i), LGSI_Area(IRRRunoffSub,i),LGSI_ASD(IRRRunoffSub,i), LGSI_DDR(IRRRunoffSub,i))
     LGSI_InterpDrainageFlow(j,IRRRunoffSub,i)= result
     result = IntegrateFluxOverland (x, Sd, LGSI_As(IRRRunoffSub,i), LGSI_AREA(IRRRunoffSub, i), LGSI_NormalFp(IRRRunoffSub,i), LGSI_NormalRov(IRRRunoffSub,i))
     LGSI_InterpOverlandFlow(j,IRRRunoffSub,i)= result
!    result = IntegrateFluxRiv (x, Sd, LGSI_As(IRRRunoffSub,i), LGSI_Area(IRRRunoffSub,i), LGSI_NormalFp(IRRRUnoffSub,i), LGSI_NormalRex(IRRRUnoff,i))
!    gebruik Atot ipv AE
!    result = IntegrateFluxRiv (x, Sd, LGSI_As(IRRRunoffSub,i), LGSI_AreaTot(IRRRunoffSub), LGSI_NormalFp(IRRRunoffSub,i), LGSI_NormalRex(IRRRunoffSub,i))
!  moet zijn Ar/AE
     result = IntegrateFluxRiv (x, Sd, LGSI_Ar(IRRRunoffSub,i), LGSI_Area(IRRRunoffSub,i), LGSI_NormalFp(IRRRunoffSub,i), LGSI_NormalRex(IRRRUnoffSub,i))
     LGSI_InterpQuickFlow(j,IRRRunoffSub,i)= result
  enddo

  Return
  End subroutine ConstructNormalGwlDrainFlow


  Subroutine ConstructGWLSeepageInterflow (IRRRunoffSub)

  implicit none

  integer IRRRunoffSub
  integer j
! double precision Sd
  double precision x, result

  j = 0

  do x=-30., 10.1, 0.1
     j = j+1
     LGSI_InterpGWLevelSeepage(j,IRRRunoffSub) = x
! 2 juli 2013: niet afkappen op nul
!    if ( (LGSI_HDIF(IRRRunoffSub) - x) > 0) then
        result = 1/LGSI_C(IRRRunoffSub) * ( LGSI_HDIF(iRRRunoffSub) - x)
!    else
!       result = 0.0
!    endif
     LGSI_InterpSeepageFlow(j,IRRRunoffSub) = result
  enddo

  Return
  End subroutine ConstructGWLSeepageInterflow



  Subroutine ComputePsdNormal (MaxSD, MinSD, NuSDMax, Nb, x, result)

  implicit none

  double precision MaxSD, MinSD, NuSdMax, Nb
  double precision x, result
  double precision help

  help   = (x-NusDMax)/Nb
  result = (MaxSD - MinSD) * Exp(-(help*help)) + MinSD

  Return
  End subroutine ComputePsdNormal


  Subroutine ComputePsdGamma (Ag, Bg, x, result)

  implicit none

  double precision Ag, Bg
  double precision x, result

  result = abs (aG*x+bG)

  Return
  End subroutine ComputePsdGamma


  Subroutine ComputePrecipitationReduction (mean, sD, result)

  implicit none

  double precision sD
  double precision mean, result

  result = pnorm (0D0,mean,Sd)

  Return
  End subroutine ComputePrecipitationReduction


  Subroutine ComputeEvaporationReduction (x, mean, sd, result)

  implicit none

  double precision mean, sd
  double precision x, result

  result = 1. - pnorm(x,mean, sd)

  Return
  End subroutine ComputeEvaporationReduction


  double precision function Pnorm (x, mean, sd)

  double precision mean, sd
  double precision x, result
     Call Normal_Cdf ( x, mean, sd, result)
     pnorm = result
  return
  end function Pnorm



  double precision function Qnorm (x, mean, sd)


  implicit none

  double precision mean, sd
  double precision x, result
     Call Normal_Cdf_Inv ( x, mean, sd, result)
     qnorm = result
  return
  end function Qnorm


  double precision function Dnorm (x, mean, sd)


  implicit none

  double precision mean, sd
  double precision x, result
     Call Normal_Pdf ( x, mean, sd, result)
     Dnorm = result
  return
  end function Dnorm


  double precision function IntegrateGwVol (mean, sd)

  ! Integrate GWVol:
!    use quadpack library for numerical integration of dnorm(x)*x for x=0 to Infinity

  implicit none
!
  double precision, parameter :: a = 0.0D+00
  integer, parameter          :: key = 6
  double precision               abserr
! double precision, parameter :: epsabs = 0.0D+00
  double precision, parameter :: epsabs = 1.0D-300
  double precision, parameter :: epsrel = 0.0001D+00
  double precision, external :: f05
  integer ier
  integer, parameter :: inf = 1
  integer neval
  double precision result, tempresult

  double precision mean, sd, meantemp
  Integer iOut1, Idebug

! put mean and sd in global variables, for use in f05
  f05mean = mean
  f05sd   = sd
! fWSsat <- function(z){30*Tets-Tets*integrate(function(x){dnorm(x,z,PsdNW(z))*x}, 0, Inf)$value}
! fESsat <- function(z){30*Tets-Tets*integrate(function(x){dnorm(x,z,PsdNE(z))*x}, 0, Inf)$value}

  iOut1 = ConfFil_get_iOut1()
  iDebug = ConfFil_get_iDebug()

! prevent numerical problems, by splitting up integration in two parts
  if (mean -3*sd .gt. a) then
  ! if integrand function >>0, split in two parts: from zero to mean, and from mean to infinity
    meantemp = mean
    call qag  ( f05, a, meantemp, epsabs, epsrel, key, tempresult, abserr, neval, ier )
    if (idebug .ne. 0) then
      write (idebug,*) ' Qag in IntegrateGwVol called with mean and sd=', mean, sd
      write (idebug, '(a,g14.6)' ) '  Integral left endpoint A =    ', a
      write (idebug, '(a,g14.6)' ) '  Integral right endpoint B =   ', meantemp
      write (idebug, '(a,g14.6)' ) '  Estimated integral is         ', tempresult
      write (idebug, '(a,g14.6)' ) '  Estimated integral error =    ', abserr
      write (idebug, '(a,i4)' ) '  Number of function evaluations, NEVAL = ', neval
      write (idebug, '(a,i4)' ) '  Error return code IER = ', ier
    endif
    call qagi ( f05, meantemp, inf, epsabs, epsrel, result, abserr, neval, ier )
    if (idebug .ne. 0) then
      write (idebug,*) ' Qagi in IntegrateGwVol called with mean and sd=', mean, sd
      write (idebug, '(a,g14.6)' ) '  Integral left endpoint A =    ', meantemp
      write (idebug, '(a,g14.6)' ) '  Integral right endpoint B =    Infinity'
      write (idebug, '(a,g14.6)' ) '  Estimated integral is         ', result
      write (idebug, '(a,g14.6)' ) '  Estimated integral error =    ', abserr
      write (idebug, '(a,i4)' ) '  Number of function evaluations, NEVAL = ', neval
      write (idebug, '(a,i4)' ) '  Error return code IER = ', ier
    endif
    result = result + tempresult
  else
    call qagi ( f05, a, inf, epsabs, epsrel, result, abserr, neval, ier )
    if (idebug .ne. 0) then
      write (idebug,*) ' Qagi in IntegrateGwVol called with mean and sd=', mean, sd
      write (idebug, '(a,g14.6)' ) '  Integral left endpoint A =    ', a
      write (idebug, '(a,g14.6)' ) '  Integral right endpoint B =    Infinity'
      write (idebug, '(a,g14.6)' ) '  Estimated integral is         ', result
      write (idebug, '(a,g14.6)' ) '  Estimated integral error =    ', abserr
!     write (idebug, '(a,g14.6)' ) '  Exact integral error =        ', true - result
      write (idebug, '(a,i4)' ) '  Number of function evaluations, NEVAL = ', neval
      write (idebug, '(a,i4)' ) '  Error return code IER = ', ier
    endif
  endif

    iOut1 = ConfFil_get_iOut1()
    if (ier .gt. 0) write(iout1,*) ' Error', ier, 'in routine Qagi in IntegrateGwVol, for mean=', mean
    IntegrateGwVol = result

  return
  end function IntegrateGwVol


  double precision function IntegrateUnsatVol (mean, sd, alp, n)

  implicit none
  double precision mean, sd, alp, n, meantemp
! double precision x
  double precision result, tempresult
!
  double precision, parameter :: a = 0.0D+00
  integer, parameter          :: key = 6
  double precision               abserr
! double precision, parameter :: epsabs = 0.0D+00
  double precision, parameter :: epsabs = 1.0D-300
  double precision, parameter :: epsrel = 0.0001D+00
  double precision, external :: f06, f07
  integer ier
  integer, parameter :: inf = 1
  integer neval

  Integer iOut1, Idebug

! put mean and sd in global variables, for use in f06 and f07
  f06mean = mean
  f06sd   = sd
  f07alp  = alp
  f07n    = n

! fWSunsat <- function(z){Tets*integrate(function(x){dnorm(x,z,PsdNW(z))*integrate(function(u){(1+(alp*u)^n)^(1/n-1)}, 0, x)$value}, 0, Inf)$value}
! fESunsat <- function(z){Tets*integrate(function(x){dnorm(x,z,PsdNE(z))*integrate(function(u){(1+(alp*u)^n)^(1/n-1)}, 0, x)$value}, 0, Inf)$value}

  iOut1 = ConfFil_get_iOut1()
  iDebug = ConfFil_get_iDebug()

  if (mean -3*sd .gt. a) then
  ! if integrand function >>0, split in two parts: from zero to mean, and from mean to infinity
     meantemp = mean
     call qag  ( f06, a, meantemp, epsabs, epsrel, key, tempresult, abserr, neval, ier )
     if (idebug .ne. 0) then
       write (idebug,*) ' Qag in IntegrateUnsatVol called with mean and sd=', mean, sd
       write (idebug, '(a,g14.6)' ) '  Integral left endpoint A =    ', a
       write (idebug, '(a,g14.6)' ) '  Integral right endpoint B =   ', meantemp
       write (idebug, '(a,g14.6)' ) '  Estimated integral is         ', tempresult
       write (idebug, '(a,g14.6)' ) '  Estimated integral error =    ', abserr
       write (idebug, '(a,i4)' ) '  Number of function evaluations, NEVAL = ', neval
       write (idebug, '(a,i4)' ) '  Error return code IER = ', ier
     endif
     call qagi ( f06, meantemp, inf, epsabs, epsrel, result, abserr, neval, ier )
     if (idebug .ne. 0) then
        write (idebug,*) ' Qagi in IntegrateUnsatVol called with mean, sd, alp, n=', mean, sd, alp, n
        write (idebug, '(a,g14.6)' ) '  Integral left endpoint A =    ', meantemp
        write (idebug, '(a,g14.6)' ) '  Integral right endpoint B =    infinity '
        write (idebug, '(a,g14.6)' ) '  Estimated integral is         ', result
        write (idebug, '(a,g14.6)' ) '  Estimated integral error =    ', abserr
        write (idebug, '(a,i4)' ) '  Number of function evaluations, NEVAL = ', neval
        write (idebug, '(a,i4)' ) '  Error return code IER = ', ier
     endif
     result = result + tempresult
   else
     call qagi ( f06, a, inf, epsabs, epsrel, result, abserr, neval, ier )
     if (idebug .ne. 0) then
        write (idebug,*) ' Qagi in IntegrateUnsatVol called with mean, sd, alp, n=', mean, sd, alp, n
        write (idebug, '(a,g14.6)' ) '  Integral left endpoint A =    ', a
        write (idebug, '(a,g14.6)' ) '  Integral right endpoint B =    infinity '
        write (idebug, '(a,g14.6)' ) '  Estimated integral is         ', result
        write (idebug, '(a,g14.6)' ) '  Estimated integral error =    ', abserr
        write (idebug, '(a,i4)' ) '  Number of function evaluations, NEVAL = ', neval
        write (idebug, '(a,i4)' ) '  Error return code IER = ', ier
     endif
   endif

     iOut1 = ConfFil_get_iOut1()
     if (ier .gt. 0) write(Iout1,*) ' Error', ier, 'in routine Qagi in IntegrateUnsatVol for mean=', mean
     IntegrateUnsatVol = result

  return
  end function IntegrateUnsatVol


  double precision function IntegrateSurfVol (mean, sd)

  implicit none

  double precision mean, sd, meantemp
! double precision x
  double precision result, tempresult
     ! Integrate SurfVol:   dnorm(x)*-x for x=-Infinity to 0
!    use quadpack library for numerical integration of dnorm(x)*x for x=0 to Infinity
!
  double precision, parameter :: a = 0.0D+00
  integer, parameter          :: key = 6
  double precision               abserr
! double precision, parameter :: epsabs = 0.0D+00
  double precision, parameter :: epsabs = 1.0D-300
  double precision, parameter :: epsrel = 0.0001D+00
  double precision, external :: f08
  integer ier
  integer, parameter :: inf = -1
  integer neval

  Integer iOut1, Idebug

! put mean and sd in global variables, for use in f08
  f08mean = mean
  f08sd   = sd

! fWSsurf <- function(z){integrate(function(x){dnorm(x,z,PsdNW(z))*-x}, -Inf, 0)$value}
! fESsurf <- function(z){integrate(function(x){dnorm(x,z,PsdNE(z))*-x}, -Inf, 0)$value}


  iOut1 = ConfFil_get_iOut1()
  iDebug = ConfFil_get_iDebug()
! prevent numerical problems, by splitting up integration in two parts
  if (mean +3*sd .lt. a) then
     meantemp = mean
     call qag  ( f08, meantemp, a, epsabs, epsrel, key, tempresult, abserr, neval, ier )
     if (idebug .ne. 0) then
       write (idebug,*) ' Qag in IntegrateSurfVol called with mean, sd=', mean, sd
       write (idebug, '(a,g14.6)' ) '  Integral left endpoint B =    ', meantemp
       write (idebug, '(a,g14.6)' ) '  Integral right endpoint A =    ', a
       write (idebug, '(a,g14.6)' ) '  Estimated integral is         ', tempresult
       write (idebug, '(a,g14.6)' ) '  Estimated integral error =    ', abserr
       write (idebug, '(a,i4)' ) '  Number of function evaluations, NEVAL = ', neval
       write (idebug, '(a,i4)' ) '  Error return code IER = ', ier
     endif
     call qagi ( f08, meantemp, inf, epsabs, epsrel, result, abserr, neval, ier )
     if (idebug .ne. 0) then
       write (idebug,*) ' Qagi in IntegrateSurfVol called with mean, sd=', mean, sd
       write (idebug, '(a,g14.6)' ) '  Integral left endpoint B =    -Infinity'
       write (idebug, '(a,g14.6)' ) '  Integral right endpoint A =    ', meantemp
       write (idebug, '(a,g14.6)' ) '  Estimated integral is         ', result
       write (idebug, '(a,g14.6)' ) '  Estimated integral error =    ', abserr
       write (idebug, '(a,i4)' ) '  Number of function evaluations, NEVAL = ', neval
       write (idebug, '(a,i4)' ) '  Error return code IER = ', ier
     endif
     result = result + tempresult
  else
    call qagi ( f08, a, inf, epsabs, epsrel, result, abserr, neval, ier )
    if (idebug .ne. 0) then
       write (idebug,*) ' Qagi in IntegrateSurfVol called with mean, sd=', mean, sd
       write (idebug, '(a,g14.6)' ) '  Integral left endpoint B =    -Infinity'
       write (idebug, '(a,g14.6)' ) '  Integral right endpoint A =    ', a
       write (idebug, '(a,g14.6)' ) '  Estimated integral is         ', result
       write (idebug, '(a,g14.6)' ) '  Estimated integral error =    ', abserr
       write (idebug, '(a,i4)' ) '  Number of function evaluations, NEVAL = ', neval
       write (idebug, '(a,i4)' ) '  Error return code IER = ', ier
    endif
  endif

     iOut1 = ConfFil_get_iOut1()
     if (ier .gt. 0) write(Iout1,*) ' Error', ier, 'in routine Qagi in IntegrateSurfVol for mean=', mean
     IntegrateSurfVol = result

  return
  end function IntegrateSurfVol


  double precision function IntegrateFluxRiv (mean, sd, As, Atot, m, Rex)

  implicit none

  double precision mean, sd, As, Atot, m, Rex
! double precision x
  double precision result
!
  double precision               a
  double precision               abserr
! double precision, parameter :: epsabs = 0.0D+00
  double precision, parameter :: epsabs = 1.0D-300
  double precision, parameter :: epsrel = 0.0001D+00
  double precision, external :: f09
  integer ier
  integer, parameter :: inf = -1
  integer neval

  Integer iOut1

! put mean and sd in global variables
  f09mean = mean
  f09sd   = sd

  a = Qnorm (As/Atot,mean, sd)
  if (a .gt. 0.D0) a = 0.0D0
  call qagi ( f09, a, inf, epsabs, epsrel, result, abserr, neval, ier )

     ! Integrate Flux:   dnorm(x)*(Ddr-x), min c (qnorm, ...), same function for Drain, Quickflow, Overland Flow but with different parameters
!    fEQriv <- function(z){((m-1)/Rex)*integrate(function(x){dnorm(x,z,PsdNE(z))*x},-Inf,min(c(qnorm(As/Atot,z,PsdNE(z)),0)))$value} ### onderverdelen in oost en west deze parameters?

     iOut1 = ConfFil_get_iOut1()
     if (ier .gt. 0) write(Iout1,*) ' Error', ier, 'in routine Qagi in IntegrateFluxRiv for mean=', mean
     IntegrateFluxRiv = (m-1.D0) / Rex * Result

  return
  end function IntegrateFluxRiv


  double precision function IntegrateFluxOverland (mean, sd, As, Ae, m, Rov)

  implicit none

  double precision mean, sd, As, Ae, m, Rov
! double precision x
  double precision result
!
  double precision               a
  double precision               b
  double precision               abserr
! double precision, parameter :: epsabs = 0.0D+00
  double precision, parameter :: epsabs = 1.0D-300
  double precision, parameter :: epsrel = 0.0001D+00
  double precision, external :: f09
  integer ier
  integer neval

  Integer iOut1

! put mean and sd in global variables
  f09mean = mean
  f09sd   = sd

  a = Qnorm (As/Ae,mean, sd)
  if (a .gt. 0.D0) a = 0.0D0
  b = 0.0D0
  call qags ( f09, a, b, epsabs, epsrel, result, abserr, neval, ier )

!    fEQov  <- function(z){((m-1)/Rov)*integrate(function(x){dnorm(x,z,PsdNE(z))*x},min(c(qnorm(As/AE,z,PsdNE(z)),0)),0)$value}

     iOut1 = ConfFil_get_iOut1()
     if (ier .gt. 0) write(Iout1,*) ' Error', ier, 'in routine Qags in IntegrateFluxOverland for mean=', mean
     IntegrateFluxOverland = (m-1.D0) / Rov * Result

  return
  end function IntegrateFluxOverland


  double precision function IntegrateFluxDrain (mean, sd, Adr, Rdr, Area, Asd, Ddr)

  implicit none

  double precision mean, sd, Adr, Rdr, Area, Asd, Ddr
! double precision x
  double precision result
  !
  double precision               a, b
  double precision               abserr
! double precision, parameter :: epsabs = 0.0D+00
  double precision, parameter :: epsabs = 1.0D-300
  double precision, parameter :: epsrel = 0.0001D+00
  double precision, external :: f10
  integer ier
  integer, parameter :: inf = 1
  integer neval
  Integer iOut1

! put mean and sd in global variables
  f10mean = mean
  f10sd   = sd
  f10ddr  = Ddr
  a = Qnorm (Asd/Area, mean, sd)
  b = Ddr
  if (a .gt. b) a = b

     ! Integrate Flux:   dnorm(x)*(Ddr-x), min c (qnorm, ...), same function for Drain, Quickflow, Overland Flow but with different parameters
!    fWQdr <- function(z){AdrW/(rdr*AW)*integrate(function(x){dnorm(x,z,PsdNW(z))*(DdrW-x)}, min(c(qnorm(AsdW/AW,z,PsdNW(z)),DdrW)), DdrW)$value}
!    FEQDR <- FUNCTION(Z){ADRE/(RDR*AE)*INTEGRATE(FUNCTION(X){DNORM(X,Z,PSDNE(Z))*(DDRE-X)}, MIN(C(QNORM(ASDE/AE,Z,PSDNE(Z)),DDRE)), DDRE)$VALUE}
!    fEQov <- function(z){((m-1)/Rov)*integrate(function(x){dnorm(x,z,PsdNE(z))*x},min(c(qnorm(As/AE,z,PsdNE(z)),0)),0)$value}

     call qags ( f10, a, b, epsabs, epsrel, result, abserr, neval, ier )

     iOut1 = ConfFil_get_iOut1()
     if (ier .gt. 0) write(Iout1,*) ' Error', ier, 'in routine Qags in IntegrateFluxDrain for mean=', mean

     Result = Adr / (Rdr * Area) * result
     IntegrateFluxDrain = result

     return
  end function IntegrateFluxDrain


  Subroutine LGSI_ConstructDelayTable

  integer          IRRRunoffSub, i, j, k, idebug, iout1

  Idebug = Conffil_get_Idebug()
  if (idebug .ne. 0) write(idebug,*) 'IRRRunoffSub j     k  LGSI_DelayCoefficients(i,j)'

  Do i=1,NcRRRunoff
    IRRRunoffSub = RRRunoff_SubIndex(i)
    if (RRRunoff_CompOption(i) .eq. 4) then
       if (LGSI_DelayNrTimesteps(iRRRunoffSub) * LGSI_DelayTimestepSize(iRRRUnoffSub) .gt. LGSI_MaxDelayLength * TimeSettings%TimestepSize + 0.1) then
         write(Iout1,*) ' Delay coefficients for LGSI node ', i, ' too long'
         write(Iout1,*) ' Delay coefficients are ',  (LGSI_DefinedDelayCoefficients(iRRRunoffSub,k),k=1,LGSI_DelayNrTimesteps(iRRRunoffSub))
         call ErrMsgStandard (981, 0, ' Error LGSI_DelayDefinitions: too large for arraylength and timestep size', ' Increase simulation timestep or reduce nr. of delay coefficients')
       endif
       Do j=1,LGSI_MaxDelayLengthPlus1
          LGSI_DelayCoefficients(iRRRunoffSub,j ) = 0.0
       Enddo
       Do j=1,LGSI_MaxDelayLengthPlus1
          k = 1 + Int ( (j-1) * TimeSettings%TimestepSize / Dble(LGSI_DelayTimestepSize(iRRRunoffSub)) )
          LGSI_DelayCoefficients(iRRRunoffSub,j ) = LGSI_DefinedDelayCoefficients(iRRRunoffSub,k) * TimeSettings%TimestepSize / Dble( LGSI_DelayTimestepSize(iRRRunoffSub) )
          if (idebug .ne. 0) write(idebug,*) iRRRunoffSub, j, k, LGSI_DelayCoefficients(iRRRunoffSub,j)
       Enddo
    endif
  Enddo

  Return
  END subroutine LGSI_ConstructDelayTable


  Subroutine DelayFunction(IRRRunoffSub, LGSITotalRunoff, LGSIDelayedRunoff)

  double precision LGSITotalRunoff, LGSIDelayedRunoff
  integer          IRRRUnoffSub, i, j, k, LGSIDelaySteps

  i = IRRRunoffSub
  LGSI_HistoryQtot(i,1) = LGSITotalRunoff

! assumed delaycoefficient specified at timestep size !!! to be made flexible !!
  LGSIDelayedRunoff = 0.0

  LGSIDelaySteps = LGSI_DelayNrTimesteps(i) * LGSI_DelayTimestepSize(i) / TimeSettings%TimestepSize
  LGSIDelaySteps = min (LGSIDelaySteps, LGSI_MaxDelayLengthPlus1)

! Do j=1,LGSI_MaxDelayLengthPlus1
  Do j=1,LGSIDelaySteps
     k = LGSIDelaySteps-j+1
     LGSIDelayedRunoff = LGSIDelayedRunoff + LGSI_DelayCoefficients(i,k) * LGSI_HistoryQtot(i,j)
  Enddo
  LGSI_HistoryQdelayed(i,1) = LGSIDelayedRunoff

  Return
  END subroutine DelayFunction


!*************************************************************!
!* J-model waarden voor convolutie en staartcorrectie        *!
!*************************************************************!

      Subroutine WagMod_JMOD(iRRRunoffSub)

      Implicit NONE
      Double Precision PI, PI2, FACTOR, DELTA, FIL, FEX, FEXJ, FEFA
      Double Precision DT
      Double Precision RTOL2
      Integer          K, I, IRRRunoffSub

     ! PI,             !* pi
     ! PI2,            !* pi^2
     ! FACTOR,         !* 8 * PI2
     ! DELTA,          !* }
     ! DEPLET,         !* } voor functie
     ! FEX             !* }
     ! DT              !* lengte tijdstap (niet variabel in oorspronkelijke code) GP: ook hier niet!!!
     ! RTOL2
     ! K,              !* loopteller
     ! I               !* loopteller
     !

      PI = 4. * ATAN(1.0)
      PI2 = PI ** 2
      FACTOR = 8.0 / PI2
      ! Altijd KIS=0, nieuw algorithme (formule Kraaijenhof vd Leur)
         DT = 1.0
         RTOL2 = RTOL**2
         Do I = 1,WagMod_IUHSlow(IRRRunoffSub)
            FEX = 0.0
            Do K=1,WagMod_NTERMUJ(IRRRunoffSub)
               DELTA = ( ( DBLE(2*K-1) )**2 ) * DBLE(I)*DT / WagMod_J(IRRRunoffSub)
               If (DELTA.LT.RTOL) exit
               DELTA = EXP(-DELTA)
               If (DELTA.LT.RTOL2) exit
               FEX = FEX + DELTA
            Enddo
            WagMod_UJ(IRRRunoffSub, I) = FEX * FACTOR / Wagmod_J(IRRRunoffSub)
         Enddo

         FIL = EXP(-1.0D0 / Wagmod_J(IRRRunoffSub))
         FEX = 1.0D0 - FIL
         FEXJ = Wagmod_J(IRRRunoffSub) * FEX
         FEFA = FACTOR * (1.0D0 - FEXJ)
         WagMod_FAFX(IRRRunoffSub) = FACTOR * FEX

         ! staart deel voor nieuwe staart correctie
         WagMod_RUJREST(IRRRunoffSub) = 0.0D0
         Do I=1,WagMod_IUHSlow(IRRRunoffSub)
            Wagmod_RUJrest(IRRRunoffSub) = Wagmod_RUJRest(IRRRunoffSub) + WagMod_UJ(IRRRunoffSub,I)
         Enddo
         WagMod_RUJREST(IRRRunoffSub) = 1.0D0 - WagMod_RUJREST(IRRRunoffSub)

      Return
      End Subroutine Wagmod_JMOD


!*************************************************************!
!* help routine for CD-model
!*************************************************************!

      function FormCDFunction (EE, FF, T)

      Implicit none

      Double precision PI, EP, EE, FF, T,  WO, Result
      Double precision FormCDFunction
      !                PI,             !* pi
      !                EP,             !* hulp-variabele
      !                EE,             !* parameter CD-model (TE OPTIMALISEREN)
      !                FF,             !* parameter CD-model (TE OPTIMALISEREN)
      !                T,              !* uit CDMOD
      !                WO,             !* hulp-variabele
      !                FORMCD          !* functie-waarde

      PI = 4. * ATAN(1.0)
      EP = -(EE - FF * T) ** 2. / T
      IF (EP .LT. -70.0) EP = -60.0
      WO = SQRT(PI * T ** 3.)
      Result = (EE / WO) * EXP(EP)

      FormCDFunction = Result

      Return
      End function FormCDFunction

!*************************************************************!
!* CD-model  convection-diffusion
!*************************************************************!

      Subroutine Wagmod_CDmod(IRRRunoffSub)

      Implicit none
      Integer          TT, TTT, TTM,IU, K,  JTTT
      Integer          NU0MX, NU1MX, IERR, IRRRUnoffSub
      Double Precision T
!     Double Precision  FormCDFunction
      Double Precision, ALLOCATABLE ::  U0(:), S(:), U1(:)
     ! TT,             !* loopteller
     ! TTT,            !* "pointer"
     ! TTM,            !* "pointer"
     ! IU,             !* loopteller
     ! K,              !* loopteller
     ! JTTT            !* loopteller
     ! NU0MX,          !* lengte voor U0 array
     ! NU1MX,          !* lengte voor S en U1 arrays
     ! IERR            !* allocation error code
     ! T,              !* hulp-variabele (tijd)
     ! EE,             !* parameter CD-model
     ! FF,             !* parameter CD-model
     ! FORMCD          !* functie
     !, ALLOCATABLE ::
     ! U0(:),          !* t.b.v. S-curve
     ! S(:),           !* geeft S-curve weer
     ! U1(:)           !* t.b.v. curve
      !*************************************************************!

      !* allocate arrays for S-curve
      NU0MX = WagMod_IUHQuick(IRRRunoffSub) * Wagmod_MaxNrTimesteps * 2
      NU1MX = WagMod_IUHQuick(IRRRunoffSub) * Wagmod_MaxNrTimesteps

      if (Wagmod_IUHQuick(IRRRunoffSub) .gt. WagMod_NUCDMX) then
         call ErrMsgStandard (981, 0, ' IUHQuick in WagmodTimesteps is too large (>2500) ', 'RRRunoffNode_Wagmod_CDMod')
      endif

      Allocate(U0(0:NU0MX), S(0:NU1MX), U1(0:NU1MX), STAT=IERR)
      If (IERR.NE.0) then
         call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', ' RRRunoffNode_Wagmod_CDMod' )
      Endif

      !*************************************************************!
      !* calculation of U0 with time increment 1/(2*NSTEP)         *!
      !*************************************************************!
      Do IU = 1, Wagmod_IUHQuick(IRRRunoffSub)
         Do K = 0, 2 * WagMod_ISubQuick(IRRRunoffSub) -1
            TTT = (IU - 1) * 2 * WagMod_ISubQuick(IRRRunoffSub) + K
            T = 1.0 * TTT / (2 * WagMod_ISubQuick(IRRRunoffSub))
            IF (T .LE. 0.0) THEN
               U0(TTT) = 0.0
            ELSE
               U0(TTT) = FormCDFunction (WagMod_E(IRRRunoffSub), WagMod_F(IRRRunoffSub),T)
            ENDIF
            IF ( U0(TTT) .LE. 0.00001 .AND. T .GT. WagMod_E(IRRRunoffSub)/WagMod_F(IRRRunoffSub) ) THEN
               GO TO 570
            ENDIF
         Enddo
      Enddo
      TTT = WagMod_IUHQuick(IRRRunoffSub) * WagMod_ISubQuick(IRRRunoffSub) * 2
 570  Do JTTT = TTT, WagMod_IUHQuick(IRRRunoffSub) * WagMod_ISubQuick(IRRRunoffSub)*2 -1
         U0(JTTT) = 0.0
      Enddo

      !* calculation of S-curve
      S(0) = 0.0
      Do TT = 1,WagMod_IUHQuick(IRRRunoffSub) * WagMod_ISubQuick(IRRRunoffSub)
         TTT = 2 * TT
         S(TT) = S(TT-1)+(U0(TTT-2)+4*U0(TTT-1)+U0(TTT))/(6.0*WagMod_ISubQuick(IRRRunoffSub))
      Enddo

      !* calculation of U1-ordinates
      U1(0) = 0.0
      Do TT = 1,WagMod_IUHQuick(IRRRunoffSub) * WagMod_ISubQuick(IRRRunoffSub)
         TTM = TT - WagMod_ISubQuick(IRRRunoffSub)
         IF (TTM .LE. 0) TTM = 0
         U1(TT) = S(TT) - S(TTM)
      Enddo

      !* calculation of U1 quantized blocks
      Do IU=1,WagMod_IUHQuick(IRRRunoffSub)
         WagMod_UCD(IRRRunoffSub,IU) = 0.0
         TT = (IU - 1) * WagMod_ISubQuick(IRRRunoffSub)
         WagMod_UCD(IRRRunoffSub,IU) = WagMod_UCD(IRRRunoffSub,IU) + U1(TT)
 630     TT = TT + 1
         WagMod_UCD(IRRRunoffSub,IU) = WagMod_UCD(IRRRunoffSub,IU) + 4 * U1(TT)
         TT = TT + 1
!June2014: subscript error possible, since two times +1 between label 630 and 640!
!        IF (TT .EQ. (IU * WagMod_ISubQuick(IRRRunoffSub))) GO TO 640
         IF (TT .GE. (IU * WagMod_ISubQuick(IRRRunoffSub))) then
           TT = IU * WagMod_ISubQuick(IRRRunoffSub)
           GO TO 640
         endif
         WagMod_UCD(IRRRunoffSub,IU) = WagMod_UCD(IRRRunoffSub,IU) + 2 * U1(TT)
         GO TO 630
 640     WagMod_UCD(IRRRunoffSub,IU) = (WagMod_UCD(IRRRunoffSub,IU) + U1(TT)) / (WagMod_ISubQuick(IRRRunoffSub) * 3)
      Enddo

      DEAllocate(U0, S, U1, STAT=IERR)

      Return
      End Subroutine Wagmod_CDMod


      Subroutine Wagmod_DIVCN2(IRRRunoffSub, Itmstp, Idebug)

      IMPLICIT NONE
      !*          this subroutine convolves the effective precipitation with the J- and CD-model;
      !*          the pef is divided over the two models as a function of the storage in the
      !*          groundwater system at the end of the beforegoing interval

      Integer          OUT, IRRRunoffSub, Itmstp, IU, IOUnit, Idebug
      Double precision RF, RECHA, PI, TNG, CPT, QSOLD, QSNEW, QL
     ! OUT,        !* "pointer"
     ! IU          !* loopteller
     ! RF,         !* SM/SAT  relatief vochtgehalte
     ! RECHA,      !* bruto aanvulling van GSTORE op t=I, afvoer moet er nog vanaf
     ! PI,         !* pi
     ! TNG,        !*
     ! CPT ,       !* reductiefactor in de berekening van de actuele verdamping
     ! QSOLD,      !* oud   volume staartterm reservoir (was QS(I-1)van QS(0:STAP))
     ! QSNEW,      !* nieuw volume staartterm reservoir (was QS(I)  van QS(0:STAP))
     ! QL          !* restterm (was array (0:STAP), alleen QL(I) werd gebruikt)

      !*************************************************************!
      !* bereken GSTORE(0) uit SM(0) en X(1)      (= J)
      !* initialiseer GSTORE(I) en QD(I), en bereken QG(I) uit* QG(0)
      !* This is all moved to INIT1
      !*************************************************************!
      !* berekening voor bodemvocht-bakje (SM, CAP, PEF)
      !*************************************************************!
      PI = 4.0D0 * ATAN(1.0D0)
      WagMod_ET(iRRRunoffSub) = WagMod_ETG(iRRRunoffSub)
      if (Wagmod_ActEvapCompOption(IRRRunoffSub) .eq. 1) then
         ! acuele verdamping wordt berekend
            TNG = (WagMod_SMT1(IRRRunoffSub) / Wagmod_FC(IRRRunoffSub)) * PI
            If (WagMod_SMT1(IRRRunoffSub) .lt. 0.0) TNG = 0.0
            If (TNG .GT. PI) TNG = PI
            CPT = (1.D0 - ((COS(TNG) + 1.D0) / 2.D0))
            Wagmod_ETA(IRRRunoffSub) = CPT * WagMod_ETG(IRRRunoffSub)
            Wagmod_SM(IRRRunoffSub) = Wagmod_SMT1(IRRRunoffSub) + Wagmod_P(IRRRunoffSub) - Wagmod_ETA(IRRRunoffSub)
      else
         ! ingevoerde verdamping wordt gebruikt voor bodemvochtbakje
            Wagmod_SM(IRRRunoffSub) = Wagmod_SMT1(IRRRunoffSub) + Wagmod_P(IRRRunoffSub) - Wagmod_ET(IRRRunoffSub)
      Endif
      ! nu verder met berekening PEF, CAP, SM op basis initiele schatting SM via ingevoerde of actuele verdamping
      RF = Wagmod_SM(IRRRunoffSub) / WagMod_SAT(IRRRunoffSub)
      If ( Wagmod_SM(IRRRunoffSub) .ge. WagMod_FC(IRRRunoffSub) ) then
          ! soil moisture above field capacity: no cap.rise, but percolation (effective rain)
          Wagmod_CAP(IRRRunoffSub) = 0.0
          Wagmod_PEF(IRRRunoffSub) = ( Wagmod_SM(IRRRunoffSub) - Wagmod_FC(IRRRunoffSub)) * RF * Wagmod_REPA(IRRRunoffSub)
          Wagmod_SM(IRRRunoffSub) = Wagmod_SM(IRRRunoffSub) - Wagmod_PEF(IRRRunoffSub)
      Elseif (Wagmod_SM(IRRRunoffSub) .GE. 0.0) then
          Wagmod_CAP(IRRRunoffSub) = Wagmod_FOS(IRRRunoffSub) * (Wagmod_FC(IRRRunoffSub) - Wagmod_SM(IRRRunoffSub)) * 0.01
          Wagmod_PEF(IRRRunoffSub) = -Wagmod_CAP(IRRRunoffSub)
          Wagmod_SM(IRRRunoffSub) = Wagmod_SM(IRRRunoffSub) + Wagmod_CAP(IRRRunoffSub)
      Else
!         write(*,*) ' Negative Soil Moisture in WagMod'
          Call ErrMsgStandard (999, 2, ' Negative Soil Moisture in WagMod', ' Soil Moisture is reset to zero')
          Wagmod_PEF(IRRRunoffSub) = Wagmod_SM(IRRRunoffSub)
          Wagmod_SM(IRRRunoffSub) = 0.0
      Endif

      !*************************************************************!
      !* bepaling van verdeling tussen snel (DIV), langzaam (1-DIV), en convolutie van J-model
      !*************************************************************!
         QSOLD = WagMod_QSNEW(IRRRunoffSub)
         !*********************************************************!
         !* berekening van verdeling tussen de twee takken        *!
         !* DIV = 1 : alles naar snelle tak (CDMOD)               *!
         !* DIV = 0 : alles naar langzame tak (JMOD)              *!
         !*********************************************************!
         If (WagMod_GSTORET1(IRRRunoffSub) .LT. 0.0 .OR. Wagmod_PEF(IRRRunoffSub) .LE. 0.0) Then
             WagMod_DIV(IRRRunoffSub) = 0.0
         Else
             Wagmod_DIV(IRRRunoffSub) = WagMod_CR(IRRRunoffSub) * Wagmod_GSTORET1(IRRRunoffSub) / 1000.0
         Endif

         !* berekening grondwaterafvoer QG met convolutie J-model
         WagMod_PEFJ(IRRRunoffSub) = (1.0 - WagMod_DIV(IRRRunoffSub)) * WagMod_PEF(IRRRunoffSub)
         RECHA = WagMod_PEFJ(IRRRunoffSub) + WagMod_SEEP(IRRRunoffSub)
         If (WagMod_QGT1(IRRRunoffSub) .LT. 0.0) then  ! GP: lelijk, correctie waarde Qg van vorige tijdstap!!!
           RECHA = RECHA + WagMod_QGT1(IRRRunoffSub)
           WagMod_QGT1(IRRRunoffSub) = 0.0
         Endif
         WagMod_GSTORE(IRRRunoffSub) = WagMod_GSTORET1(IRRRunoffSub) + RECHA

         !* convolutie J-model
         Do IU=1,WagMod_IUHSlow(IRRRunoffSub)
! 50469     OUT = Itmstp + IU - 1
            OUT = IU
            If (OUT .GT. Wagmod_MaxNrTimestepsSimulation) EXIT
            Wagmod_QG(IRRRunoffSub,OUT) = Wagmod_QG(IRRRunoffSub,OUT) + Wagmod_UJ(IRRRunoffSub,IU) * RECHA
         Enddo
         !* staart J-model: KISCODE.EQ.4 altijd,  replace tail with remainder of RECHA  ! for RCHKQG
         QSNEW = QSOLD + Wagmod_RUJREST(IRRRunoffSub)*RECHA
         QL    = Wagmod_FAFX(IRRRunoffSub) * QSNEW
         QSNEW = QSNEW - QL
! 50469  WagMod_QG(IRRRunoffSub,Itmstp) = WagMod_QG(IRRRunoffSub,Itmstp) + QL
         WagMod_QG(IRRRunoffSub,1) = WagMod_QG(IRRRunoffSub,1) + QL
! 50469  WagMod_GSTORE(IRRRunoffSub) = WagMod_GSTORE(IRRRunoffSub) - WagMod_QG(IRRRunoffSub,Itmstp)
         WagMod_GSTORE(IRRRunoffSub) = WagMod_GSTORE(IRRRunoffSub) - WagMod_QG(IRRRunoffSub,1)

         !* convolution of direct runoff with CD-model
         WagMod_PEFCD(IRRRunoffSub) = WagMod_DIV(IRRRunoffSub) * WagMod_PEF(IRRRunoffSub)
         Do IU = 1,WagMod_IUHQuick(IRRRunoffSub)
! 50469     OUT = ITmstp + IU - 1
            OUT = IU
            If (OUT .GT. Wagmod_MaxNrTimestepsSimulation) EXIT
            WagMod_QD(IRRRunoffSub,OUT) = WagMod_QD(IRRRunoffSub,OUT) + WagMod_UCD(IRRRunoffSub, IU) * WagMod_PEFCD(IRRRunoffSub)
         Enddo

         !* adding of outputblocks of the two model elements
! 50469  WagMod_QC(IRRRunoffSub) = max(0.0, WagMod_QG(IRRRunoffSub,Itmstp)) + WagMod_QD(IRRRunoffSub,Itmstp)
         WagMod_QC(IRRRunoffSub) = max(0.0d0, WagMod_QG(IRRRunoffSub,1)) + WagMod_QD(IRRRunoffSub,1)

         ! store QSNew
         WagMod_QSNEW(IRRRunoffSub) = QSNEW

         ! write output to plotfile
         if (idebug .ne. 0) write(Idebug,*) 'write to IoutWagmod Plotfile', WagMod_PlotfileUnit(IRRRunoffSub)
         IoUnit = Wagmod_PlotfileUnit(IRRRunoffSub)
         If (Wagmod_ActEvapCompOption(IRRRunoffSub) .eq. 1) then
              if (GenerateOldWagmodOutputFiles) Write (Iounit,232)  Wagmod_P(IRRRunoffSub),Wagmod_ET(IRRRunoffSub), 0.0 ,Itmstp, &
                                  Wagmod_QC(IRRRunoffSub),Wagmod_ETA(IRRRunoffSub),Max(0.0d0,Wagmod_QG(IRRRunoffSub,1)), &
                                  Wagmod_PEFJ(IRRRunoffSub),Wagmod_QD(IRRRunoffSub, 1), &
                                  Wagmod_PEFCD(IRRRunoffSub),Wagmod_DIV(IRRRunoffSub), &
                                  Wagmod_GSTORE(IRRRunoffSub),Wagmod_SM(IRRRunoffSub),Wagmod_CAP(IRRRunoffSub)
!             if (GenerateOldWagmodOutputFiles) Write (Iounit,232)  Wagmod_P(IRRRunoffSub),Wagmod_ET(IRRRunoffSub), 0.0 ,Itmstp, &
!                                 Wagmod_QC(IRRRunoffSub),Wagmod_ETA(IRRRunoffSub),Max(0.0,Wagmod_QG(IRRRunoffSub,Itmstp)), &
!                                 Wagmod_PEFJ(IRRRunoffSub),Wagmod_QD(IRRRunoffSub, Itmstp), &
!                                 Wagmod_PEFCD(IRRRunoffSub),Wagmod_DIV(IRRRunoffSub), &
!                                 Wagmod_GSTORE(IRRRunoffSub),Wagmod_SM(IRRRunoffSub),Wagmod_CAP(IRRRunoffSub)
232           Format(F7.2,2F8.3,I11,7F11.5,F12.5,2F11.5)
         Else
              if (GenerateOldWagmodOutputFiles) Write (Iounit,230)  Wagmod_P(IRRRunoffSub),Wagmod_ET(IRRRunoffSub), 0.0 ,Itmstp, &
                                  Wagmod_QC(IRRRunoffSub),Wagmod_PEF(IRRRunoffSub),Max(0.0d0,Wagmod_QG(IRRRunoffSub,1)), &
                                  Wagmod_PEFJ(IRRRunoffSub),Wagmod_QD(IRRRunoffSub, 1), &
                                  Wagmod_PEFCD(IRRRunoffSub),Wagmod_DIV(IRRRunoffSub), &
                                  Wagmod_GSTORE(IRRRunoffSub),Wagmod_SM(IRRRunoffSub),Wagmod_CAP(IRRRunoffSub)
!             if (GenerateOldWagmodOutputFiles) Write (Iounit,230)  Wagmod_P(IRRRunoffSub),Wagmod_ET(IRRRunoffSub), 0.0 ,Itmstp, &
!                                 Wagmod_QC(IRRRunoffSub),Wagmod_PEF(IRRRunoffSub),Max(0.0,Wagmod_QG(IRRRunoffSub,Itmstp)), &
!                                 Wagmod_PEFJ(IRRRunoffSub),Wagmod_QD(IRRRunoffSub, Itmstp), &
!                                 Wagmod_PEFCD(IRRRunoffSub),Wagmod_DIV(IRRRunoffSub), &
!                                 Wagmod_GSTORE(IRRRunoffSub),Wagmod_SM(IRRRunoffSub),Wagmod_CAP(IRRRunoffSub)
230          Format(F7.2,2F8.3,I11,7F11.5,F12.5,2F11.5)
         Endif

         if (Idebug .ne. 0) then
            Write(idebug,*) ' itmstp               Qd               Qg '
            Do IU = 1,WagMod_IUHQuick(IRRRunoffSub)
               write(idebug,*) iu, Wagmod_QD(IRRRunoffSub,iu), Wagmod_QG(IRRRunoffSub,iu)
            enddo
         endif

      Return
      End subroutine Wagmod_DivCn2



  !> If success, function returns Values array of length ElementCount
  !! for paved elementset on specific quantity handle
  function RR_GetHBVDataByIntId(QuantityHandle, ElementCount, Values) result(success)

    use RR_open_mi_support
    use wl_open_mi_support

    implicit none

    ! return value
    logical  :: success

    ! arguments
    integer,          intent(in)      :: QuantityHandle   !< quant. handle
    integer,          intent(in)      :: ElementCount     !< #elems in HBV elementset
    double precision, intent(out), &
            dimension(1:ElementCount) :: Values           !< values in HBV elemenset

    ! locals

    Values  = 0
    success = .true.

    select case (QuantityHandle)
    ! Subdivide for various variables
    case(RRiRRunoffOutflow)
    !RR HBV outflow
            ! If a stack overflow occurs due to this array operation, use a do loop
            if (NRRRUNOFF > 0) then
                Values(1:NRRRUNOFF) = RSLMAP19_RRRunoff(1, 1:NRRRUNOFF, 1)
            else
                success = .false.
            endif
    case(RRiRRunoffRainfall)
        !RR HBV precipitation
            ! If a stack overflow occurs due to this array operation, use a do loop
            if (NRRRUNOFF > 0) then
                Values(1:NRRRUNOFF) = RSLMAP19_RRRunoff(2, 1:NRRRUNOFF, 1)
            else
                success = .false.
            endif
        case(RRiRRunoffPotEvap)
        !RR HBV potential evapotranspiration
            ! If a stack overflow occurs due to this array operation, use a do loop
            if (NRRRUNOFF > 0) then
                Values(1:NRRRUNOFF) = RSLMAP19_RRRunoff(3, 1:NRRRUNOFF, 1)
            else
                success = .false.
            endif
        case(RRiRRunoffActEvap)
        !RR HBV actual evapotranspiration
            ! If a stack overflow occurs due to this array operation, use a do loop
            if (NRRRUNOFF > 0) then
                Values(1:NRRRUNOFF) = RSLMAP19_RRRunoff(4, 1:NRRRUNOFF, 1)
            else
                success = .false.
            endif
        case(RRiRRunoffTotalRunoff)
        !RR HBV total runoff
            ! If a stack overflow occurs due to this array operation, use a do loop
            if (NRRRUNOFF > 0) then
                Values(1:NRRRUNOFF) = RSLMAP19_RRRunoff(5, 1:NRRRUNOFF, 1)
            else
                success = .false.
            endif
    case(RRiRRunoffSnowfall)
        !RR HBV snow fall
            ! If a stack overflow occurs due to this array operation, use a do loop
            if (NRRRUNOFF > 0) then
                Values(1:NRRRUNOFF) = RSLMAP19_RRRunoff(NStartHBV, 1:NRRRUNOFF, 1)
            else
                success = .false.
            endif
        case(RRiRRunoffBaseflow)
        !RR HBV baseflow
            ! If a stack overflow occurs due to this array operation, use a do loop
            if (NRRRUNOFF > 0) then
                Values(1:NRRRUNOFF) = RSLMAP19_RRRunoff(NStartHBV+1, 1:NRRRUNOFF, 1)
            else
                success = .false.
            endif
        case(RRiRRunoffInterflow)
        !RR HBV interflow
            ! If a stack overflow occurs due to this array operation, use a do loop
            if (NRRRUNOFF > 0) then
                Values(1:NRRRUNOFF) = RSLMAP19_RRRunoff(NStartHBV+2, 1:NRRRUNOFF, 1)
            else
                success = .false.
            endif
        case(RRiRRunoffQuickflow)
        !RR HBV quickflow
            ! If a stack overflow occurs due to this array operation, use a do loop
            if (NRRRUNOFF > 0) then
                Values(1:NRRRUNOFF) = RSLMAP19_RRRunoff(NStartHBV+3, 1:NRRRUNOFF, 1)
            else
                success = .false.
            endif
        case(RRiRRunoffDrySnowContent)
        !RR HBV dry snow content
            ! If a stack overflow occurs due to this array operation, use a do loop
            if (NRRRUNOFF > 0) then
                Values(1:NRRRUNOFF) = RSLMAP19_RRRunoff(NStartHBV+4, 1:NRRRUNOFF, 1)
            else
                success = .false.
            endif
        case(RRiRRunoffFreeWaterContent)
        !RR HBV free water content
            ! If a stack overflow occurs due to this array operation, use a do loop
            if (NRRRUNOFF > 0) then
                Values(1:NRRRUNOFF) = RSLMAP19_RRRunoff(NStartHBV+5, 1:NRRRUNOFF, 1)
            else
                success = .false.
            endif
        case(RRiRRunoffSoilMoisture)
        !RR HBV soil moisture
            ! If a stack overflow occurs due to this array operation, use a do loop
            if (NRRRUNOFF > 0) then
                Values(1:NRRRUNOFF) = RSLMAP19_RRRunoff(NStartHBV+6, 1:NRRRUNOFF, 1)
            else
                success = .false.
            endif
        case(RRiRRunoffUpperZoneContent)
        !RR HBV upper zone content
            ! If a stack overflow occurs due to this array operation, use a do loop
            if (NRRRUNOFF > 0) then
                Values(1:NRRRUNOFF) = RSLMAP19_RRRunoff(NStartHBV+7, 1:NRRRUNOFF, 1)
            else
                success = .false.
            endif
        case(RRiRRunoffLowerZoneContent)
        !RR HBV lower zone content
            ! If a stack overflow occurs due to this array operation, use a do loop
            if (NRRRUNOFF > 0) then
                Values(1:NRRRUNOFF) = RSLMAP19_RRRunoff(NStartHBV+8, 1:NRRRUNOFF, 1)
            else
                success = .false.
            endif
        case(RRiRRunoffTemperature)
        !RR HBV temperature
            ! If a stack overflow occurs due to this array operation, use a do loop
            if (NRRRUNOFF > 0) then
                Values(1:NRRRUNOFF) = RSLMAP19_RRRunoff(NStartHBV+9, 1:NRRRUNOFF, 1)
            else
                success = .false.
            endif
    case default
    ! Something is wrong
        success = .false.
    end select

  end function

end module RRRunoff
