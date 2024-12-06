!> This module contains the definition of the input items for the [output] section of the
!! MDU file.
module m_output_config
   use MessageHandling
   use netcdf_utils, only: realloc, nc_att_set
   use m_ug_nc_attribute, only: nc_attribute => ug_nc_attribute
   use netcdf, only: nf90_double
   use fm_location_types
   implicit none
   private

   public scan_input_tree
   public set_properties
   public add_output_config
   public realloc
   public dealloc
   public id_nc_type2nc_type_his

   interface realloc
      module procedure reallocate_config_set
   end interface
   interface dealloc
      module procedure deallocate_config_set
   end interface

   !> indices for output variables
   integer, public :: IDX_HIS_VOLTOT
   integer, public :: IDX_HIS_STOR
   integer, public :: IDX_HIS_VOLERR
   integer, public :: IDX_HIS_BNDIN
   integer, public :: IDX_HIS_BNDOUT
   integer, public :: IDX_HIS_BNDTOT
   integer, public :: IDX_HIS_EXCHIN
   integer, public :: IDX_HIS_EXCHOUT
   integer, public :: IDX_HIS_EXCHTOT
   integer, public :: IDX_HIS_PRECIP_TOTAL
   integer, public :: IDX_HIS_EVAP
   integer, public :: IDX_HIS_SOUR
   integer, public :: IDX_HIS_INTERNALTIDESDISSIPATION
   integer, public :: IDX_HIS_GravInput
   integer, public :: IDX_HIS_SalInput
   integer, public :: IDX_HIS_SalInput2
   integer, public :: IDX_HIS_GRWIN
   integer, public :: IDX_HIS_GRWOUT
   integer, public :: IDX_HIS_GRWTOT
   integer, public :: IDX_HIS_LATIN
   integer, public :: IDX_HIS_LATOUT
   integer, public :: IDX_HIS_LATTOT
   integer, public :: IDX_HIS_LATIN1D
   integer, public :: IDX_HIS_LATOUT1D
   integer, public :: IDX_HIS_LATTOT1D
   integer, public :: IDX_HIS_LATIN2D
   integer, public :: IDX_HIS_LATOUT2D
   integer, public :: IDX_HIS_LATTOT2D
   integer, public :: IDX_HIS_EXTIN
   integer, public :: IDX_HIS_EXTOUT
   integer, public :: IDX_HIS_EXTTOT
   integer, public :: IDX_HIS_EXTIN1D
   integer, public :: IDX_HIS_EXTOUT1D
   integer, public :: IDX_HIS_EXTTOT1D
   integer, public :: IDX_HIS_EXTIN2D
   integer, public :: IDX_HIS_EXTOUT2D
   integer, public :: IDX_HIS_EXTTOT2D
   integer, public :: IDX_HIS_ICEPT
   integer, public :: IDX_HIS_EVAP_ICEPT
   integer, public :: IDX_HIS_PRECIP_GROUND
   integer, public :: IDX_HIS_RUG_RUHEIGHT
   integer, public :: IDX_HIS_RUG_RUX
   integer, public :: IDX_HIS_RUG_RUY
   integer, public :: IDX_HIS_SOURCE_SINK_PRESCRIBED_DISCHARGE
   integer, public :: IDX_HIS_SOURCE_SINK_PRESCRIBED_SALINITY_INCREMENT
   integer, public :: IDX_HIS_SOURCE_SINK_PRESCRIBED_TEMPERATURE_INCREMENT
   integer, public :: IDX_HIS_SOURCE_SINK_CURRENT_DISCHARGE
   integer, public :: IDX_HIS_SOURCE_SINK_CUMULATIVE_VOLUME
   integer, public :: IDX_HIS_SOURCE_SINK_DISCHARGE_AVERAGE
   integer, public :: IDX_HIS_GENERAL_STRUCTURE_DISCHARGE
   integer, public :: IDX_HIS_GENERAL_STRUCTURE_CREST_LEVEL
   integer, public :: IDX_HIS_GENERAL_STRUCTURE_GATE_LOWER_EDGE_LEVEL
   integer, public :: IDX_HIS_GENERAL_STRUCTURE_GATE_OPENING_WIDTH
   integer, public :: IDX_HIS_GENERAL_STRUCTURE_S1UP
   integer, public :: IDX_HIS_GENERAL_STRUCTURE_S1DN
   integer, public :: IDX_HIS_GENERAL_STRUCTURE_HEAD
   integer, public :: IDX_HIS_GENERAL_STRUCTURE_FLOW_AREA
   integer, public :: IDX_HIS_GENERAL_STRUCTURE_VELOCITY
   integer, public :: IDX_HIS_GENERAL_STRUCTURE_CREST_WIDTH
   integer, public :: IDX_HIS_GENERAL_STRUCTURE_DISCHARGE_THROUGH_GATE_OPENING
   integer, public :: IDX_HIS_GENERAL_STRUCTURE_DISCHARGE_OVER_GATE
   integer, public :: IDX_HIS_GENERAL_STRUCTURE_DISCHARGE_UNDER_GATE
   integer, public :: IDX_HIS_GENERAL_STRUCTURE_GATE_OPENING_HEIGHT
   integer, public :: IDX_HIS_GENERAL_STRUCTURE_GATE_UPPER_EDGE_LEVEL
   integer, public :: IDX_HIS_GENERAL_STRUCTURE_VELOCITY_THROUGH_GATE_OPENING
   integer, public :: IDX_HIS_GENERAL_STRUCTURE_VELOCITY_OVER_GATE
   integer, public :: IDX_HIS_GENERAL_STRUCTURE_VELOCITY_UNDER_GATE
   integer, public :: IDX_HIS_GENERAL_STRUCTURE_FLOW_AREA_IN_GATE_OPENING
   integer, public :: IDX_HIS_GENERAL_STRUCTURE_FLOW_AREA_OVER_GATE
   integer, public :: IDX_HIS_GENERAL_STRUCTURE_FLOW_AREA_UNDER_GATE
   integer, public :: IDX_HIS_GENERAL_STRUCTURE_STATE
   integer, public :: IDX_HIS_GENERAL_STRUCTURE_S1_ON_CREST
   integer, public :: IDX_HIS_GENERAL_STRUCTURE_FORCE_DIFFERENCE
   integer, public :: IDX_HIS_CDAM_DISCHARGE
   integer, public :: IDX_HIS_CDAM_CREST_LEVEL
   integer, public :: IDX_HIS_CDAM_S1UP
   integer, public :: IDX_HIS_CDAM_S1DN
   integer, public :: IDX_HIS_PUMP_STRUCTURE_DISCHARGE
   integer, public :: IDX_HIS_PUMP_CAPACITY
   integer, public :: IDX_HIS_PUMP_DISCHARGE_DIR
   integer, public :: IDX_HIS_PUMP_S1UP
   integer, public :: IDX_HIS_PUMP_S1DN
   integer, public :: IDX_HIS_PUMP_STRUCTURE_HEAD
   integer, public :: IDX_HIS_PUMP_ACTUAL_STAGE
   integer, public :: IDX_HIS_PUMP_HEAD
   integer, public :: IDX_HIS_PUMP_REDUCTION_FACTOR
   integer, public :: IDX_HIS_PUMP_S1_DELIVERY_SIDE
   integer, public :: IDX_HIS_PUMP_S1_SUCTION_SIDE
   integer, public :: IDX_HIS_GATE_DISCHARGE
   integer, public :: IDX_HIS_GATE_LOWER_EDGE_LEVEL
   integer, public :: IDX_HIS_GATE_S1UP
   integer, public :: IDX_HIS_GATE_S1DN
   integer, public :: IDX_HIS_GATEGEN_DISCHARGE
   integer, public :: IDX_HIS_GATEGEN_CREST_LEVEL
   integer, public :: IDX_HIS_GATEGEN_CREST_WIDTH
   integer, public :: IDX_HIS_GATEGEN_GATE_LOWER_EDGE_LEVEL
   integer, public :: IDX_HIS_GATEGEN_FLOW_THROUGH_HEIGHT
   integer, public :: IDX_HIS_GATEGEN_GATE_OPENING_WIDTH
   integer, public :: IDX_HIS_GATEGEN_S1UP
   integer, public :: IDX_HIS_GATEGEN_S1DN
   integer, public :: IDX_HIS_WEIRGEN_DISCHARGE
   integer, public :: IDX_HIS_WEIRGEN_CREST_LEVEL
   integer, public :: IDX_HIS_WEIRGEN_CREST_WIDTH
   integer, public :: IDX_HIS_WEIRGEN_S1UP
   integer, public :: IDX_HIS_WEIRGEN_S1DN
   integer, public :: IDX_HIS_WEIRGEN_STRUCTURE_HEAD
   integer, public :: IDX_HIS_WEIRGEN_VELOCITY
   integer, public :: IDX_HIS_WEIRGEN_FLOW_AREA
   integer, public :: IDX_HIS_WEIRGEN_STATE
   integer, public :: IDX_HIS_WEIRGEN_FORCE_DIFFERENCE
   integer, public :: IDX_HIS_WEIRGEN_S1_ON_CREST
   integer, public :: IDX_HIS_ORIFICE_DISCHARGE
   integer, public :: IDX_HIS_ORIFICE_CREST_LEVEL
   integer, public :: IDX_HIS_ORIFICE_CREST_WIDTH
   integer, public :: IDX_HIS_ORIFICE_GATE_LOWER_EDGE_LEVEL
   integer, public :: IDX_HIS_ORIFICE_S1UP
   integer, public :: IDX_HIS_ORIFICE_S1DN
   integer, public :: IDX_HIS_ORIFICE_GATE_OPENING_HEIGHT
   integer, public :: IDX_HIS_ORIFICE_HEAD
   integer, public :: IDX_HIS_ORIFICE_FLOW_AREA
   integer, public :: IDX_HIS_ORIFICE_STATE
   integer, public :: IDX_HIS_ORIFICE_S1_ON_CREST
   integer, public :: IDX_HIS_ORIFICE_VELOCITY
   integer, public :: IDX_HIS_ORIFICE_FORCE_DIFFERENCE
   integer, public :: IDX_HIS_BRIDGE_DISCHARGE
   integer, public :: IDX_HIS_BRIDGE_S1UP
   integer, public :: IDX_HIS_BRIDGE_S1DN
   integer, public :: IDX_HIS_BRIDGE_HEAD
   integer, public :: IDX_HIS_BRIDGE_FLOW_AREA
   integer, public :: IDX_HIS_BRIDGE_VELOCITY
   integer, public :: IDX_HIS_BRIDGE_BLUP
   integer, public :: IDX_HIS_BRIDGE_BLDN
   integer, public :: IDX_HIS_BRIDGE_BL_ACTUAL
   integer, public :: IDX_HIS_CULVERT_DISCHARGE
   integer, public :: IDX_HIS_CULVERT_CREST_LEVEL
   integer, public :: IDX_HIS_CULVERT_GATE_LOWER_EDGE_LEVEL
   integer, public :: IDX_HIS_CULVERT_S1UP
   integer, public :: IDX_HIS_CULVERT_S1DN
   integer, public :: IDX_HIS_CULVERT_GATE_OPENING_HEIGHT
   integer, public :: IDX_HIS_CULVERT_HEAD
   integer, public :: IDX_HIS_CULVERT_FLOW_AREA
   integer, public :: IDX_HIS_CULVERT_VELOCITY
   integer, public :: IDX_HIS_CULVERT_STATE
   integer, public :: IDX_HIS_DAMBREAK_S1UP
   integer, public :: IDX_HIS_DAMBREAK_S1DN
   integer, public :: IDX_HIS_DAMBREAK_DISCHARGE
   integer, public :: IDX_HIS_DAMBREAK_CUMULATIVE_DISCHARGE
   integer, public :: IDX_HIS_DAMBREAK_VELOCITY
   integer, public :: IDX_HIS_DAMBREAK_HEAD
   integer, public :: IDX_HIS_DAMBREAK_FLOW_AREA
   integer, public :: IDX_HIS_DAMBREAK_CREST_LEVEL
   integer, public :: IDX_HIS_DAMBREAK_CREST_WIDTH
   integer, public :: IDX_HIS_DAMBREAK_BREACH_WIDTH_TIME_DERIVATIVE
   integer, public :: IDX_HIS_DAMBREAK_WATER_LEVEL_JUMP
   integer, public :: IDX_HIS_UNIWEIR_DISCHARGE
   integer, public :: IDX_HIS_UNIWEIR_CREST_LEVEL
   integer, public :: IDX_HIS_UNIWEIR_S1UP
   integer, public :: IDX_HIS_UNIWEIR_S1DN
   integer, public :: IDX_HIS_UNIWEIR_HEAD
   integer, public :: IDX_HIS_UNIWEIR_FLOW_AREA
   integer, public :: IDX_HIS_UNIWEIR_VELOCITY
   integer, public :: IDX_HIS_CMPSTRU_DISCHARGE
   integer, public :: IDX_HIS_CMPSTRU_S1UP
   integer, public :: IDX_HIS_CMPSTRU_S1DN
   integer, public :: IDX_HIS_CMPSTRU_HEAD
   integer, public :: IDX_HIS_CMPSTRU_FLOW_AREA
   integer, public :: IDX_HIS_CMPSTRU_VELOCITY
   integer, public :: IDX_HIS_LONGCULVERT_DISCHARGE
   integer, public :: IDX_HIS_LONGCULVERT_S1UP
   integer, public :: IDX_HIS_LONGCULVERT_S1DN
   integer, public :: IDX_HIS_LONGCULVERT_HEAD
   integer, public :: IDX_HIS_LONGCULVERT_FLOW_AREA
   integer, public :: IDX_HIS_LONGCULVERT_VELOCITY
   integer, public :: IDX_HIS_LONGCULVERT_VALVE_RELATIVE_OPENING

   integer, public :: IDX_HIS_WATERLEVEL
   integer, public :: IDX_HIS_BEDLEVEL
   integer, public :: IDX_HIS_WATERDEPTH
   integer, public :: IDX_HIS_X_VELOCITY
   integer, public :: IDX_HIS_Y_VELOCITY
   integer, public :: IDX_HIS_Z_VELOCITY
   integer, public :: IDX_HIS_DEPTH_AVERAGED_X_VELOCITY
   integer, public :: IDX_HIS_DEPTH_AVERAGED_Y_VELOCITY
   integer, public :: IDX_HIS_VELOCITY_MAGNITUDE
   integer, public :: IDX_HIS_VELOCITY_MAGNITUDE_EULERIAN
   integer, public :: IDX_HIS_DISCHARGE_MAGNITUDE

   integer, public :: IDX_HIS_VIU
   integer, public :: IDX_HIS_VICWWS
   integer, public :: IDX_HIS_VICWWU
   integer, public :: IDX_HIS_TKIN
   integer, public :: IDX_HIS_EPS
   integer, public :: IDX_HIS_TAU
   integer, public :: IDX_HIS_RICH
   integer, public :: IDX_HIS_SALINITY
   integer, public :: IDX_HIS_TEMPERATURE
   integer, public :: IDX_HIS_POTENTIAL_DENSITY
   integer, public :: IDX_HIS_DENSITY
   integer, public :: IDX_HIS_BRUNT_VAISALA_N2

   integer, public :: IDX_HIS_HWAV
   integer, public :: IDX_HIS_HWAV_SIG
   integer, public :: IDX_HIS_TWAV
   integer, public :: IDX_HIS_PHIWAV
   integer, public :: IDX_HIS_RLABDA
   integer, public :: IDX_HIS_R
   integer, public :: IDX_HIS_UORB
   integer, public :: IDX_HIS_USTOKES
   integer, public :: IDX_HIS_VSTOKES
   integer, public :: IDX_HIS_TAUSX
   integer, public :: IDX_HIS_TAUSY

   integer, public :: IDX_HIS_PATM
   integer, public :: IDX_HIS_WINDX
   integer, public :: IDX_HIS_WINDX_SFERIC
   integer, public :: IDX_HIS_WINDY
   integer, public :: IDX_HIS_WINDY_SFERIC
   integer, public :: IDX_HIS_RAIN
   integer, public :: IDX_HIS_INFILTRATION_CAP
   integer, public :: IDX_HIS_INFILTRATION_INFILTRATION_ACTUAL

   integer, public :: IDX_HIS_AIR_DENSITY

   integer, public :: IDX_HIS_WIND
   integer, public :: IDX_HIS_TAIR
   integer, public :: IDX_HIS_RHUM
   integer, public :: IDX_HIS_CLOU
   integer, public :: IDX_HIS_QSUN
   integer, public :: IDX_HIS_QEVA
   integer, public :: IDX_HIS_QCON
   integer, public :: IDX_HIS_QLONG
   integer, public :: IDX_HIS_QFREVA
   integer, public :: IDX_HIS_QFRCON
   integer, public :: IDX_HIS_QTOT

   integer, public :: IDX_HIS_SED_FRAC_NAME
   integer, public :: IDX_HIS_SED
   integer, public :: IDX_HIS_WS
   integer, public :: IDX_HIS_SEDDIF
   integer, public :: IDX_HIS_BODSED
   integer, public :: IDX_HIS_DPSED

   integer, public :: IDX_HIS_HWQ_ABSTRACT
   integer, public :: IDX_HIS_WQBOT_ABSTRACT
   integer, public :: IDX_HIS_WQBOT3D_ABSTRACT

   integer, public :: IDX_HIS_TRACERS_ABSTRACT

   integer, public :: IDX_HIS_OBSCRS_DISCHARGE
   integer, public :: IDX_HIS_OBSCRS_DISCHARGE_CUMUL
   integer, public :: IDX_HIS_OBSCRS_AREA
   integer, public :: IDX_HIS_OBSCRS_VELOCITY
   integer, public :: IDX_HIS_OBSCRS_CONST_ABSTRACT
   integer, public :: IDX_HIS_OBSCRS_CONST_1
   integer, public :: IDX_HIS_OBSCRS_CONST_N
   integer, public :: IDX_HIS_OBSCRS_SED_BTRANSPORT
   integer, public :: IDX_HIS_OBSCRS_SED_STRANSPORT
   integer, public :: IDX_HIS_OBSCRS_SED_BTRANSPORT_PERFRAC_ABSTRACT
   integer, public :: IDX_HIS_OBSCRS_SED_STRANSPORT_PERFRAC_ABSTRACT

   integer, public :: IDX_HIS_LATERAL_PRESCRIBED_DISCHARGE_INSTANTANEOUS
   integer, public :: IDX_HIS_LATERAL_PRESCRIBED_DISCHARGE_AVERAGE
   integer, public :: IDX_HIS_LATERAL_REALIZED_DISCHARGE_INSTANTANEOUS
   integer, public :: IDX_HIS_LATERAL_REALIZED_DISCHARGE_AVERAGE

   integer, public :: IDX_HIS_TAUB
   integer, public :: IDX_HIS_SBCX
   integer, public :: IDX_HIS_SBCY
   integer, public :: IDX_HIS_SBWX
   integer, public :: IDX_HIS_SBWY
   integer, public :: IDX_HIS_SSWX
   integer, public :: IDX_HIS_SSWY
   integer, public :: IDX_HIS_SSCX
   integer, public :: IDX_HIS_SSCY
   integer, public :: IDX_HIS_MSED
   integer, public :: IDX_HIS_THLYR
   integer, public :: IDX_HIS_POROS
   integer, public :: IDX_HIS_LYRFRAC
   integer, public :: IDX_HIS_FRAC
   integer, public :: IDX_HIS_MUDFRAC
   integer, public :: IDX_HIS_SANDFRAC
   integer, public :: IDX_HIS_FIXFRAC
   integer, public :: IDX_HIS_HIDEXP
   integer, public :: IDX_HIS_MFLUFF

   integer, public :: IDX_HIS_DRED_AREA_NAME
   integer, public :: IDX_HIS_DUMP_AREA_NAME
   integer, public :: IDX_HIS_DRED_LINK_DISCHARGE
   integer, public :: IDX_HIS_DRED_DISCHARGE
   integer, public :: IDX_HIS_DUMP_DISCHARGE
   integer, public :: IDX_HIS_DRED_TIME_FRAC
   integer, public :: IDX_HIS_PLOUGH_TIME_FRAC

   integer, public :: IDX_MAP_S0
   integer, public :: IDX_MAP_S1
   integer, public :: IDX_MAP_POTEVAP
   integer, public :: IDX_MAP_ACTEVAP
   integer, public :: IDX_MAP_PRESCREVAP
   integer, public :: IDX_MAP_VOL1
   integer, public :: IDX_MAP_WATERDEPTH
   integer, public :: IDX_MAP_HU
   integer, public :: IDX_MAP_NEGDPT
   integer, public :: IDX_MAP_NEGDPT_CUM
   integer, public :: IDX_MAP_NOITER
   integer, public :: IDX_MAP_NOITER_CUM
   integer, public :: IDX_MAP_LIMTSTEP
   integer, public :: IDX_MAP_LIMTSTEP_CUM
   integer, public :: IDX_MAP_COURANT
   integer, public :: IDX_MAP_AU
   integer, public :: IDX_MAP_U1
   integer, public :: IDX_MAP_U0
   integer, public :: IDX_MAP_UCXQ_EULERIAN
   integer, public :: IDX_MAP_UCYQ_EULERIAN
   integer, public :: IDX_MAP_UCXQ
   integer, public :: IDX_MAP_UCYQ
   integer, public :: IDX_MAP_UCMAG
   integer, public :: IDX_MAP_UCMAG_EULER
   integer, public :: IDX_MAP_UCMAGA_GLM
   integer, public :: IDX_MAP_UCMAGA
   integer, public :: IDX_MAP_WW1
   integer, public :: IDX_MAP_RHO
   integer, public :: IDX_MAP_VIU
   integer, public :: IDX_MAP_DIU
   integer, public :: IDX_MAP_Q1
   integer, public :: IDX_MAP_Q1_MAIN
   integer, public :: IDX_MAP_FIXED_WEIR_ENERGY_LOSS
   integer, public :: IDX_MAP_SPIRCRV
   integer, public :: IDX_MAP_SPIRINT
   integer, public :: IDX_MAP_NUMLIMDT
   integer, public :: IDX_MAP_TAUSX
   integer, public :: IDX_MAP_TAUSY
   integer, public :: IDX_MAP_TAUS
   integer, public :: IDX_MAP_TAUSMAX
   integer, public :: IDX_MAP_Z0UCUR
   integer, public :: IDX_MAP_Z0UROU
   integer, public :: IDX_MAP_SA1
   integer, public :: IDX_MAP_CZS
   integer, public :: IDX_MAP_CZU
   integer, public :: IDX_MAP_CFU
   integer, public :: IDX_MAP_CFUTYP
   integer, public :: IDX_MAP_TEM1
   integer, public :: IDX_MAP_SED
   integer, public :: IDX_MAP_CONST
   integer, public :: IDX_MAP_TURKIN1
   integer, public :: IDX_MAP_VICWWU
   integer, public :: IDX_MAP_TUREPS1
   integer, public :: IDX_MAP_TUREPS1_3
   integer, public :: IDX_MAP_TUREPS1_4
   integer, public :: IDX_MAP_CFRT_0
   integer, public :: IDX_MAP_CFRT_1
   integer, public :: IDX_MAP_CFRT_2
   integer, public :: IDX_MAP_CFRT
   integer, public :: IDX_MAP_CFCL
   integer, public :: IDX_MAP_RAINFALL_RATE
   integer, public :: IDX_MAP_INTERCEPTION_WATERDEPTH
   integer, public :: IDX_MAP_PATM
   integer, public :: IDX_MAP_WINDX
   integer, public :: IDX_MAP_WINDY
   integer, public :: IDX_MAP_WINDXU
   integer, public :: IDX_MAP_WINDYU
   integer, public :: IDX_MAP_WINDX_SFERIC
   integer, public :: IDX_MAP_WINDY_SFERIC
   integer, public :: IDX_MAP_WINDXU_SFERIC
   integer, public :: IDX_MAP_WINDYU_SFERIC
   integer, public :: IDX_MAP_WINDSTRESSX
   integer, public :: IDX_MAP_WINDSTRESSY
   integer, public :: IDX_MAP_WINDSTRESSX_SFERIC
   integer, public :: IDX_MAP_WINDSTRESSY_SFERIC
   integer, public :: IDX_MAP_TAIR
   integer, public :: IDX_MAP_RHUM
   integer, public :: IDX_MAP_CLOU
   integer, public :: IDX_MAP_QSUN
   integer, public :: IDX_MAP_QEVA
   integer, public :: IDX_MAP_QCON
   integer, public :: IDX_MAP_QLONG
   integer, public :: IDX_MAP_QFREVA
   integer, public :: IDX_MAP_QFRCON
   integer, public :: IDX_MAP_QTOT
   integer, public :: IDX_MAP_TIDALPOTENTIAL
   integer, public :: IDX_MAP_SALPOTENTIAL
   integer, public :: IDX_MAP_INTERNAL_TIDES_DISSIPATION
   integer, public :: IDX_MAP_TNUDGE
   integer, public :: IDX_MAP_NUDGE_TEM
   integer, public :: IDX_MAP_NUDGE_SAL
   integer, public :: IDX_MAP_NUDGE_DTEM
   integer, public :: IDX_MAP_NUDGE_DSAL
   integer, public :: IDX_MAP_HWAV
   integer, public :: IDX_MAP_HWAV_SIG
   integer, public :: IDX_MAP_TP
   integer, public :: IDX_MAP_DIR
   integer, public :: IDX_MAP_SXWAV
   integer, public :: IDX_MAP_SYWAV
   integer, public :: IDX_MAP_SXBWAV
   integer, public :: IDX_MAP_SYBWAV
   integer, public :: IDX_MAP_MX
   integer, public :: IDX_MAP_MY
   integer, public :: IDX_MAP_DISSURF
   integer, public :: IDX_MAP_DISWCAP
   integer, public :: IDX_MAP_UORB
   integer, public :: IDX_MAP_E
   integer, public :: IDX_MAP_R
   integer, public :: IDX_MAP_DR
   integer, public :: IDX_MAP_D
   integer, public :: IDX_MAP_DF
   integer, public :: IDX_MAP_SXX
   integer, public :: IDX_MAP_SYY
   integer, public :: IDX_MAP_SXY
   integer, public :: IDX_MAP_CWAV
   integer, public :: IDX_MAP_CGWAV
   integer, public :: IDX_MAP_SIGMWAV
   integer, public :: IDX_MAP_KWAV
   integer, public :: IDX_MAP_NWAV
   integer, public :: IDX_MAP_CTHETA
   integer, public :: IDX_MAP_L1
   integer, public :: IDX_MAP_SWE
   integer, public :: IDX_MAP_SWT
   integer, public :: IDX_MAP_UST_CC
   integer, public :: IDX_MAP_VST_CC
   integer, public :: IDX_MAP_USTOKES
   integer, public :: IDX_MAP_VSTOKES
   integer, public :: IDX_MAP_THETAMEAN
   integer, public :: IDX_MAP_TWAV
   integer, public :: IDX_MAP_FX
   integer, public :: IDX_MAP_FY
   integer, public :: IDX_MAP_WAVFU
   integer, public :: IDX_MAP_WAVFV
   integer, public :: IDX_MAP_DTCELL
   integer, public :: IDX_MAP_TIME_WATER_ON_GROUND
   integer, public :: IDX_MAP_FREEBOARD
   integer, public :: IDX_MAP_WATERDEPTH_ON_GROUND
   integer, public :: IDX_MAP_VOLUME_ON_GROUND
   integer, public :: IDX_MAP_CURRENT_TOTAL_NET_INFLOW_1D2D
   integer, public :: IDX_MAP_CUMULATIVE_TOTAL_NET_INFLOW_1D2D
   integer, public :: IDX_MAP_CURRENT_TOTAL_NET_INFLOW_LATERAL
   integer, public :: IDX_MAP_CUMULATIVE_TOTAL_NET_INFLOW_LATERAL
   integer, public :: IDX_MAP_WATER_LEVEL_GRADIENT
   integer, public :: IDX_MAP_QIN
   integer, public :: IDX_CLS_S1
   integer, public :: IDX_CLS_WATERDEPTH
   integer, public :: IDX_CLS_UCMAG
   integer, public :: IDX_CLS_UCMAG_EULER
   integer, public :: IDX_CLS_UCDIR
   integer, public :: IDX_CLS_UCDIR_EULER

   integer, public, parameter :: id_nc_undefined = -50
   integer, public, parameter :: id_nc_byte = -51
   integer, public, parameter :: id_nc_char = -52
   integer, public, parameter :: id_nc_short = -53
   integer, public, parameter :: id_nc_int = -54
   integer, public, parameter :: id_nc_float = -55
   integer, public, parameter :: id_nc_double = -56

   public t_output_quantity_config
   !> Derived type for the input items, defining one entry [output] section of the MDU file.
   type t_output_quantity_config
      character(len=Idlen) :: key !< Key of the input item in the MDU file (e.g. wrimap_s1).
      character(len=Idlen) :: name !< Name of the output item on the NETCDF file.
      integer :: id_nc_type !< ID indicating NetCDF variable type, one of: id_nc_double, id_nc_int, etc.
      character(len=Idlen) :: long_name !< Long name of the output item on the NETCDF file.
      character(len=Idlen) :: unit !< unit of the output item on the NETCDF file.
      character(len=Idlen) :: standard_name !< Standard name of the output item on the NETCDF file.
      character(len=Idlen) :: input_value = '' !< Original user-provided input valuestring (unparsed) (<<key>> = <<input value>>.
      character(len=Idlen) :: description !< Description of the input paragraph, key combination.
      integer :: location_specifier !< Specifies the location where the variable is specified (use parameters from fm_location_types)
      type(nc_att_set) :: additional_attributes !< optional additional NetCDF attributes for this quantity
      type(t_station_nc_dimensions), allocatable :: nc_dim_ids !< optional detailed specification of NetCDF dim-ids for observation stations
   end type t_output_quantity_config

   type, public :: t_output_quantity_config_set
      integer :: count = 0 !< Number of configs in config set
      integer :: capacity = 0 !< Allocated size of config set (size = count + # of empty configs)
      type(t_output_quantity_config), allocatable, dimension(:) :: configs !< array of output quantity configs in config set
   end type t_output_quantity_config_set

   !> Derived type that stores flags to include/exclude netcdf dimensions NetCDF variables for observation stations, since they do are not uniform.
   type, public :: t_station_nc_dimensions
      logical :: laydim = .false.
      logical :: laydim_interface_center = .false.
      logical :: laydim_interface_edge = .false.
      logical :: nlyrdim = .false.
      logical :: statdim = .false.
      logical :: sedsusdim = .false.
      logical :: sedtotdim = .false.
      logical :: timedim = .false.
   end type t_station_nc_dimensions

contains

!> Reallocate config set.
   subroutine reallocate_config_set(config_set)
      use m_alloc

      type(t_output_quantity_config_set), intent(inout) :: config_set !< Output configuration set.

      type(t_output_quantity_config), dimension(:), allocatable :: new_configs

      if (allocated(config_set%configs) .and. config_set%capacity > 0) then
         if (config_set%count > config_set%capacity) then !only increase capacity if necessary
            config_set%capacity = config_set%capacity * 2
            allocate (new_configs(config_set%capacity))
            new_configs(1:size(config_set%configs)) = config_set%configs
            call move_alloc(new_configs, config_set%configs)
         end if
      else
         config_set%capacity = 200 ! hardcoded default start size of 200
         allocate (config_set%configs(config_set%capacity))
      end if
      config_set%capacity = size(config_set%configs)

   end subroutine reallocate_config_set

!> Deallocate config set.
   subroutine deallocate_config_set(config_set)
      ! Input/output parameters
      type(t_output_quantity_config_set), intent(inout) :: config_set !< Output configuration set.

      if (config_set%capacity > 0) then
         deallocate (config_set%configs)
      end if
   end subroutine deallocate_config_set

!> Define an output configuration quantity. And set the IDX variable to the current entry
   subroutine add_output_config(config_set, idx, key, name, long_name, standard_name, unit, location_specifier, nc_dim_ids, id_nc_type, nc_attributes, description)
      use netcdf, only: nf90_double, nf90_float

      type(t_output_quantity_config_set), intent(inout) :: config_set !< Array containing all output quantity configs.
      integer, intent(out) :: idx !< Index for the current variable.
      character(len=*), intent(in) :: key !< Key in the MDU file.
      character(len=*), intent(in) :: name !< Name of the variable on the NETCDF file.
      character(len=*), intent(in) :: long_name !< Long name of the variable on the NETCDF file.
      character(len=*), intent(in) :: standard_name !< Standard name of the variable on the NETCDF file.
      character(len=*), intent(in) :: unit !< Unit of the variable on the NETCDF file.
      integer, intent(in) :: location_specifier !< Location specifier of the variable.
      type(t_station_nc_dimensions), optional, intent(in) :: nc_dim_ids !< Included NetCDF dimensions
      integer, optional, intent(in) :: id_nc_type !< ID indicating NetCDF variable type, one of: id_nc_double, id_nc_int, etc. Default: id_nc_undefined.
      type(nc_attribute), optional, intent(in) :: nc_attributes(:) !< (optional) list of additional NetCDF attributes to be stored for this output variable.
      character(len=*), optional, intent(in) :: description !< Description of the MDU key, used when printing an MDU or .dia file.

      integer :: num_entries, id_nc_type_, num_attributes

      if (present(id_nc_type)) then
         ! Safety
         if (.not. (id_nc_type == id_nc_undefined .or. &
                    id_nc_type == id_nc_byte .or. &
                    id_nc_type == id_nc_char .or. &
                    id_nc_type == id_nc_short .or. &
                    id_nc_type == id_nc_int .or. &
                    id_nc_type == id_nc_float .or. &
                    id_nc_type == id_nc_double)) then
            call mess(LEVEL_ERROR, 'add_output_config - Internal error: id_nc_type must be one of the id_nc_[type]s!')
         end if
         id_nc_type_ = id_nc_type
      else
         ! By default, use the netcdf precision that is later read from the mdu
         id_nc_type_ = id_nc_undefined
      end if

      config_set%count = config_set%count + 1
      if (config_set%count > config_set%capacity) then
         call realloc(config_set)
      end if
      num_entries = config_set%count
      idx = num_entries
      config_set%configs(num_entries)%key = key
      config_set%configs(num_entries)%name = name
      config_set%configs(num_entries)%id_nc_type = id_nc_type_
      config_set%configs(num_entries)%long_name = long_name
      config_set%configs(num_entries)%standard_name = standard_name
      config_set%configs(num_entries)%unit = unit
      config_set%configs(num_entries)%location_specifier = location_specifier
      config_set%configs(num_entries)%input_value = ''

      if (present(nc_dim_ids)) then
         config_set%configs(num_entries)%nc_dim_ids = nc_dim_ids
      end if

      if (present(nc_attributes)) then
         num_attributes = size(nc_attributes)
         call realloc(config_set%configs(num_entries)%additional_attributes, num_attributes, keepExisting=.false.)
         config_set%configs(num_entries)%additional_attributes%count = num_attributes
         config_set%configs(num_entries)%additional_attributes%atts = nc_attributes
      end if

      if (present(description)) then
         config_set%configs(num_entries)%description = description
      else
         config_set%configs(num_entries)%description = ''
      end if

   end subroutine add_output_config

!> convert id_nc_type to actual nc_type for his file variables
   function id_nc_type2nc_type_his(id_nc_type) result(nc_type)
      use netcdf, only: nf90_byte, nf90_char, nf90_short, nf90_int, nf90_float, nf90_double
      use m_map_his_precision, only: md_nc_his_precision, netcdf_data_type
      implicit none

      integer, intent(in) :: id_nc_type !< ID indicating NetCDF variable type, one of: id_nc_double, id_nc_int, etc.
      integer :: nc_type !> Actual netcdf type, one of: nf90_double, nf90_int, etc.

      select case (id_nc_type)
      case default
         call mess(LEVEL_ERROR, 'id_nc_type2nc_type_his - Internal error: id_nc_type must be one of the id_nc_[type]s!')
      case (id_nc_undefined)
         ! Use the netcdf precision for his files defined in the mdu
         nc_type = netcdf_data_type(md_nc_his_precision)
      case (id_nc_byte)
         nc_type = nf90_byte
      case (id_nc_char)
         nc_type = nf90_char
      case (id_nc_short)
         nc_type = nf90_short
      case (id_nc_int)
         nc_type = nf90_int
      case (id_nc_float)
         nc_type = nf90_float
      case (id_nc_double)
         nc_type = nf90_double
      end select

   end function id_nc_type2nc_type_his

!> scan the input tree, using the keys in the config_set
   subroutine scan_input_tree(tree, paragraph, config_set)
      use properties

      type(tree_data), pointer, intent(in) :: tree !< Property tree
      character(len=*), intent(in) :: paragraph !< Paragraph of the location of the input data.
      type(t_output_quantity_config_set), intent(inout) :: config_set !< Contains the keys and configuration information on the output variables.

      integer :: i

      do i = 1, config_set%count
         call prop_get(tree, paragraph, config_set%configs(i)%key, config_set%configs(i)%input_value)
      end do

   end subroutine scan_input_tree

!> Set the properties for the diagnostics file
   subroutine set_properties(tree, paragraph, config_set)
      use properties

      type(tree_data), pointer, intent(in) :: tree !< Property tree
      character(len=*), intent(in) :: paragraph !< Paragraph of the location of the input data.
      type(t_output_quantity_config_set), intent(inout) :: config_set !< Contains the keys and configuration information on the output variables.

      integer :: i

      do i = 1, config_set%count
         associate (config => config_set%configs(i))
            if (len_trim(config%description) > 0 .and. len_trim(config%input_value) > 0) then
               if (trim(config%input_value) == 'current') then
                  config%input_value = '1'
               end if
               call prop_set(tree, trim(paragraph), trim(config%key), trim(config%input_value), trim(config%description))
            end if
         end associate
      end do

   end subroutine set_properties

end module m_output_config
