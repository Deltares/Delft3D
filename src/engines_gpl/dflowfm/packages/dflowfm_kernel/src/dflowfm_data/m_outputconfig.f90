!> This module contains the definition of the input items for the [output] section of the 
!! MDU file.
module m_output_config
   use MessageHandling
   use coordinate_reference_system
   use m_ug_nc_attribute
private
   
   public scan_input_tree
   public set_properties
   public addoutval
   public realloc
   public dealloc
   
   interface realloc
      module procedure realloc_config_output
   end interface
   interface dealloc
      module procedure dealloc_config_output
   end interface

   integer, parameter, public :: UNC_LOC_CN  = 1  !< Data location: corner point.
   integer, parameter, public :: UNC_LOC_S   = 2  !< Data location: pressure point.
   integer, parameter, public :: UNC_LOC_U   = 3  !< Data location: horizontal velocity point.
   integer, parameter, public :: UNC_LOC_L   = 13 !< Data location: horizontal net link.
   integer, parameter, public :: UNC_LOC_S3D = 4  !< Data location: pressure point in all layers.
   integer, parameter, public :: UNC_LOC_U3D = 5  !< Data location: horizontal velocity point in all layers.
   integer, parameter, public :: UNC_LOC_W   = 6  !< Data location: vertical velocity point on all layer interfaces.
   integer, parameter, public :: UNC_LOC_WU  = 16 !< Data location: vertical viscosity point on all layer interfaces.
   integer, parameter, public :: UNC_LOC_WB        = 21 !< Data location: his file water balance
   integer, parameter, public :: UNC_LOC_SOSI      = 22 !< Data location: his file sources and sinks
   integer, parameter, public :: UNC_LOC_GENSTRU   = 23 !< Data location: his file general structure data
   integer, parameter, public :: UNC_LOC_DAM       = 24   !< Data location: his file controllable dam data
   integer, parameter, public :: UNC_LOC_PUMP      = 25   !< Data location: his file pump data
   integer, parameter, public :: UNC_LOC_GATE      = 26   !< Data location: his file gate data
   integer, parameter, public :: UNC_LOC_WEIRGEN   = 27   !< Data location: his file weir data
   integer, parameter, public :: UNC_LOC_ORIFICE   = 28   !< Data location: his file orifice data
   integer, parameter, public :: UNC_LOC_BRIDGE    = 29   !< Data location: his file bridge data
   integer, parameter, public :: UNC_LOC_CULVERT   = 30   !< Data location: his file culvert data
   integer, parameter, public :: UNC_LOC_DAMBREAK  = 31   !< Data location: his file dambreak data
   integer, parameter, public :: UNC_LOC_UNIWEIR   = 32   !< Data location: his file universal weir data
   integer, parameter, public :: UNC_LOC_CMPSTRU   = 33   !< Data location: his file compound structure data
   integer, parameter, public :: UNC_LOC_LONGCULVERT = 34 !< Data location: his file long culvert data
   integer, parameter, public :: UNC_LOC_STATION     = 35 !< Data location: his file monitoring station data
   integer, parameter, public :: UNC_LOC_LATERAL     = 36 !< Data location: his file lateral locations data
   
   !> indexes for output variables 
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

   integer, public :: IDX_HIS_TKE
   integer, public :: IDX_HIS_VICWW
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
   integer, public :: IDX_HIS_UORB
   integer, public :: IDX_HIS_USTOKES
   integer, public :: IDX_HIS_VSTOKES
   integer, public :: IDX_HIS_TAUSX
   integer, public :: IDX_HIS_TAUSY

   integer, public :: IDX_HIS_WINDX
   integer, public :: IDX_HIS_WINDX_SFERIC
   integer, public :: IDX_HIS_WINDY
   integer, public :: IDX_HIS_WINDY_SFERIC
   integer, public :: IDX_HIS_RAIN
   integer, public :: IDX_HIS_INFILTRATION_CAP
   integer, public :: IDX_HIS_INFILTRATION_INFILTRATION_ACTUAL

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

   integer, public :: IDX_HIS_SED
   integer, public :: IDX_HIS_WS
   integer, public :: IDX_HIS_SEDDIF

   integer, public :: IDX_HIS_CONSTITUENTS

   integer, public :: IDX_HIS_LATERAL_PRESCRIBED_DISCHARGE_INSTANTANEOUS
   integer, public :: IDX_HIS_LATERAL_PRESCRIBED_DISCHARGE_AVERAGE
   integer, public :: IDX_HIS_LATERAL_REALIZED_DISCHARGE_INSTANTANEOUS
   integer, public :: IDX_HIS_LATERAL_REALIZED_DISCHARGE_AVERAGE

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
   integer, public :: IDX_MAP_CONST
   integer, public :: IDX_MAP_MORS
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
   
   public t_output_quantity_config
   !> Derived type for the input items, defining one entry [output] section of the MDU file. 
   type t_output_quantity_config
      character(len=Idlen)             :: key             !< Key of the input item in the MDU file (e.g. wrimap_s1).                       
      character(len=Idlen)             :: name            !< Name of the output item on the NETCDF file.      
      character(len=Idlen)             :: long_name       !< Long name of the output item on the NETCDF file.      
      character(len=Idlen)             :: unit            !< unit of the output item on the NETCDF file.      
      character(len=Idlen)             :: standard_name   !< Standard name of the output item on the NETCDF file.                     
      character(len=Idlen)             :: input_value     !< Original user-provided input valuestring (unparsed) (<<key>> = <<input value>>.         
      character(len=Idlen)             :: description     !< Description of the input paragraph, key combination.
      integer                          :: location_specifier !< Specifies the locationwhere the variable is specified (One of UNC_LOC_CN, UNC_LOC_S
                                                             !< UNC_LOC_U, UNC_LOC_L, UNC_LOC_S3D, UNC_LOC_U3, DUNC_LOC_W, UNC_LOC_WU, ...)
      integer                          :: num_additional_attributes  !< number of additional attributes
      type(ug_nc_attribute), pointer      :: additional_attributes(:)   !< optional additional attributes for this entity
   end type t_output_quantity_config

   type, public :: t_output_quantity_config_set
      integer                                                :: size = 0                  !< Actual size of cross-section set
      integer                                                :: growsby = 200             !< Increment for cross-section set
      integer                                                :: count= 0                  !< Actual number of cross-section sets
      type(t_output_quantity_config), pointer, dimension(:)    :: statout                   !< Current cross-section
   end type t_output_quantity_config_set

contains

!> Reallocate config set.
subroutine realloc_config_output(confoutput)
   ! Modules
   use m_alloc

   implicit none
   ! Input/output parameters
   type(t_output_quantity_config_set), intent(inout)   :: confoutput !< Output configuration set.
   

   ! Local variables
   integer                   :: ierr
   type(t_output_quantity_config), pointer, dimension(:)    :: oldstats

   ! Program code

   if (confoutput%size > 0) then
      oldstats=>confoutput%statout
   endif

   if (confoutput%growsBy <=0) then
      confoutput%growsBy = 200
   endif
   allocate(confoutput%statout(confoutput%size+confoutput%growsBy),stat=ierr)
   call aerr('statoutput%statout(statoutput%size+statoutput%growsBy)',ierr,confoutput%size+confoutput%growsBy)

   if (confoutput%size > 0) then
      confoutput%statout(1:confoutput%size) = oldstats(1:confoutput%size)
      deallocate(oldstats)
   endif
   confoutput%size = confoutput%size+confoutput%growsBy
end subroutine realloc_config_output

!> Deallocate config set.
subroutine dealloc_config_output(confoutput)
   implicit none
   ! Input/output parameters
   type(t_output_quantity_config_set), intent(inout)   :: confoutput !< Output configuration set.

   if (confoutput%size> 0) then
      deallocate(confoutput%statout)
   endif
end subroutine dealloc_config_output

   !> Define an output configuration quantity. And set the IDX variable to the current entry
subroutine addoutval(config_set, idx, key, name, long_name, standard_name, unit, location_specifier, description)
   type(t_output_quantity_config_set),  intent(inout) :: config_set         !< Array containing all output quantity configs.
   integer,                         intent(inout) :: idx                 !< Index for the current variable.
   character(len=*),                intent(in   ) :: key                 !< Key in the MDU file.
   character(len=*),                intent(in   ) :: name                !< Name of the variable on the NETCDF file.
   character(len=*),                intent(in   ) :: long_name           !< Long name of the variable on the NETCDF file.
   character(len=*),                intent(in   ) :: standard_name       !< Standard name of the variable on the NETCDF file.
   character(len=*),                intent(in   ) :: unit                !< Unit of the variable on the NETCDF file.
   integer,                         intent(in   ) :: location_specifier  !< Location specifier of the variable.
   character(len=*), optional,      intent(in   ) :: description         !< Description for the 

   integer :: numentries

   config_set%count = config_set%count+1
   if (config_set%count > config_set%size) then
      call realloc(config_set)
   endif
   numentries = config_set%count
   idx = numentries
   config_set%statout(numentries)%key                = key             
   config_set%statout(numentries)%name               = name            
   config_set%statout(numentries)%long_name          = long_name       
   config_set%statout(numentries)%standard_name      = standard_name   
   config_set%statout(numentries)%unit               = unit            
   config_set%statout(numentries)%location_specifier = location_specifier
   config_set%statout(numentries)%num_additional_attributes = 0
   if (present(description)) then
      config_set%statout(numentries)%description = description
   else
      config_set%statout(numentries)%description = ''
   endif

end subroutine addoutval

!> scan the input tree, using the keys in the statout_set
subroutine scan_input_tree(tree, paragraph, statout_set)
   use properties
   
   type(tree_data), pointer,                    intent(in   )     :: tree        !< Property tree
   character(len=*),                            intent(in   )     :: paragraph   !< Paragraph of the location of the input data.
   type(t_output_quantity_config_set),          intent(inout)     :: statout_set !< Contains the keys and configuration information on the output variables.

   integer i
   type(t_output_quantity_config), pointer, dimension(:) :: statout
   
   statout => statout_set%statout

   do i = 1, statout_set%count
      statout(i)%input_value = ''
      call prop_get_string(tree, paragraph, statout(i)%key, statout(i)%input_value)
   enddo

end subroutine scan_input_tree

!> Set the properties for the diagnostics file
subroutine set_properties(tree, paragraph, statout_set)
   use properties

   type(tree_data), pointer,                    intent(in   )     :: tree        !< Property tree
   character(len=*),                            intent(in   )     :: paragraph   !< Paragraph of the location of the input data.
   type(t_output_quantity_config_set),          intent(inout)     :: statout_set !< Contains the keys and configuration information on the output variables.

   integer i
   type(t_output_quantity_config), pointer, dimension(:) :: statout
   
   statout => statout_set%statout

   do i = 1, statout_set%count
      if (len_trim(statout(i)%description)>0) then
         call prop_set(tree, trim(paragraph), trim(statout(i)%key), trim(statout(i)%input_value), trim(statout(i)%description))
      endif
   enddo

end subroutine set_properties

end module m_output_config