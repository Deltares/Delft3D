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

 !> Module with input/output elementsets/quantities for RR. Character parameters are meant to
!! be used by OpenMI, use these to extend OpenMI elementsets/quantities. Integer parameters 
!! are meant to be used by Delta Shell (D-RR), use these to extend D-RR elementsets/quantities
!! Note: Character parameters always need a integer parameter equivalent!

module RR_Open_mi_support

    use wl_open_mi_support

    ! variables

    ! component id
    Character(len=oes_id_len), parameter :: RRComponent   = 'RR'

    ! element set ids
    Character(len=oes_id_len), parameter ::  RRRainfallElmSet   = 'Rainfall stations'
    Character(len=oes_id_len), parameter ::  RRUnpavedElmSet    = 'RR-Unpaved nodes'
    Character(len=oes_id_len), parameter ::  RROpenwaterElmSet  = 'RR-Openwater Nodes'
    Character(len=oes_id_len), parameter ::  RRStructureElmSet  = 'RR-Structures'
    Character(len=oes_id_len), parameter ::  RRBoundaryElmSet   = 'RR-Boundaries'
    Character(len=oes_id_len), parameter ::  RRNWRWElmSet       = 'NWRW nodes'
    Character(len=oes_id_len), parameter ::  RRSacramentoElmSet = 'RR-Sacramento Nodes'

    ! quantity ids
    Character(len=oes_id_len), parameter ::  RRRainfall        = 'Rainfall'
    Character(len=oes_id_len), parameter ::  RRSeepage         = 'Seepage'
    Character(len=oes_id_len), parameter ::  RRPumpstop        = 'Pumpstop'
    Character(len=oes_id_len), parameter ::  RROpenWaterLevel  = 'Openwater level'
    Character(len=oes_id_len), parameter ::  RRGroundwaterLevel= 'Groundwater level'
    Character(len=oes_id_len), parameter ::  RRGwRecharge      = 'Groundwater recharge'
    Character(len=oes_id_len), parameter ::  RRUnsatZoneContent= 'Unsaturated zone content'
    Character(len=oes_id_len), parameter ::  RRStorageCoeff    = 'Storage coefficient'
    Character(len=oes_id_len), parameter ::  RRFlow            = 'Flow'
    Character(len=oes_id_len), parameter ::  RRBndLevels       = 'Boundary levels'
    Character(len=oes_id_len), parameter ::  RRBndDepths       = 'Boundary depths'
    Character(len=oes_id_len), parameter ::  RRBndAreas        = 'Boundary areas'
    Character(len=oes_id_len), parameter ::  RRBndSaltConcentrations = 'Boundary salt concentrations'
    Character(len=oes_id_len), parameter ::  RRSacrUZTWC       = 'Sacr. UpperZoneTensionWaterContent'
    Character(len=oes_id_len), parameter ::  RRSacrUZFWC       = 'Sacr. UpperZoneFreeWaterContent'
    Character(len=oes_id_len), parameter ::  RRSacrLZTWC       = 'Sacr. LowerZoneTensionWaterContent'
    Character(len=oes_id_len), parameter ::  RRSacrLZFPC       = 'Sacr. LowerZoneFreePrimaryWaterContent'
    Character(len=oes_id_len), parameter ::  RRSacrLZFSC       = 'Sacr. LowerZoneFreeSupplementaryWaterContent'
    Character(len=oes_id_len), parameter ::  RRSacrBaseFlow    = 'Sacr. BaseFlow'
    Character(len=oes_id_len), parameter ::  RRSacrSurfFlow    = 'Sacr. SurfaceFlow'
    Character(len=oes_id_len), parameter ::  RRSacrTotalRunoff = 'Sacr. Total Runoff'
	Character(len=oes_id_len), parameter ::  RRSacrPotEvap     = 'Sacr. pot. evaporation'
	Character(len=oes_id_len), parameter ::  RRSacrActEvap     = 'Sacr. act. evaporation'
	Character(len=oes_id_len), parameter ::  RRSacrPrecip      = 'Sacr. precipitation'
	Character(len=oes_id_len), parameter ::  RRSacrRunoffImpArea = 'Sacr. runoff imp. area'
	Character(len=oes_id_len), parameter ::  RRSacrChannelInflow = 'Sacr. channel inflow'
	Character(len=oes_id_len), parameter ::  RRSubSurfaceSideOutflow = 'Sacr. side + subsurface outflow'
	Character(len=oes_id_len), parameter ::  RRAdImpAreaContent = 'Sacr. add. imp. area content'

    ! element set integer ids
    integer, public, parameter :: RRiRainfallElmSet            = 1
    integer, public, parameter :: RRiUnpavedElmSet             = 2
    integer, public, parameter :: RRiOpenwaterElmSet           = 3
    integer, public, parameter :: RRiStructureElmSet           = 4
    integer, public, parameter :: RRiBoundaryElmSet            = 5
    integer, public, parameter :: RRiNWRWElmSet                = 6
    integer, public, parameter :: RRiSacramentoElmSet          = 7
    integer, public, parameter :: RRiPavedElmSet               = 8
    integer, public, parameter :: RRiGreenhouseElmSet          = 9
    integer, public, parameter :: RRiWWTPElmSet                = 10
    integer, public, parameter :: RRiBalanceNodeElmSet         = 11
    integer, public, parameter :: RRiBalanceModelElmSet        = 12
    integer, public, parameter :: RRiLinkElmSet                = 13
	integer, public, parameter :: RRiHBVElmSet				   = 14

    ! quantity integer ids                                   
    integer, public, parameter :: RRiRainfall                  = 1
    integer, public, parameter :: RRiSeepage                   = 2
    integer, public, parameter :: RRiPumpstop                  = 3
    integer, public, parameter :: RRiOpenWaterLevel            = 4
    integer, public, parameter :: RRiGroundwaterLevel          = 5
    integer, public, parameter :: RRiGwRecharge                = 6 
    integer, public, parameter :: RRiUnsatZoneContent          = 7
    integer, public, parameter :: RRiStorageCoeff              = 8
    integer, public, parameter :: RRiFlow                      = 9
    integer, public, parameter :: RRiBndLevels                 = 10
    integer, public, parameter :: RRiBndDepths                 = 11
    integer, public, parameter :: RRiBndAreas                  = 12
    integer, public, parameter :: RRiBndSaltConcentrations     = 13
    integer, public, parameter :: RRiSacrUZTWC                 = 14
    integer, public, parameter :: RRiSacrUZFWC                 = 15
    integer, public, parameter :: RRiSacrLZTWC                 = 16
    integer, public, parameter :: RRiSacrLZFPC                 = 17
    integer, public, parameter :: RRiSacrLZFSC                 = 18
    integer, public, parameter :: RRiSacrBaseFlow              = 19
    integer, public, parameter :: RRiSacrSurfFlow              = 20
    integer, public, parameter :: RRiSacrTotalRunoff           = 21
    integer, public, parameter :: RRiSurfRunoff                = 22
    integer, public, parameter :: RRiInfiltration              = 23
    integer, public, parameter :: RRiEvaporationSurface        = 24
    integer, public, parameter :: RRiEvaporationActual         = 25
    integer, public, parameter :: RRiEvaporationPotential      = 26
    integer, public, parameter :: RRiPercolation               = 27
    integer, public, parameter :: RRiCapillaryRise             = 28
    integer, public, parameter :: RRiStorage_mm                = 29
    integer, public, parameter :: RRiStorage_m3                = 30
    integer, public, parameter :: RRiGroundwaterVolume         = 31
    integer, public, parameter :: RRiGroundwaterLevelThreshold = 32
    integer, public, parameter :: RRiGroundwaterLevelSurface   = 33
    integer, public, parameter :: RRiUnsaturatedZoneVolume     = 34
    integer, public, parameter :: RRiStorageDWA_mm             = 35
    integer, public, parameter :: RRiStorageRWA_mm             = 36
    integer, public, parameter :: RRiStorageStreet_mm          = 37
    integer, public, parameter :: RRiSpillingTotal             = 38
    integer, public, parameter :: RRiSpillingDWA               = 39
    integer, public, parameter :: RRiSpillingRWA               = 40
    integer, public, parameter :: RRiPumpedTotal               = 41
    integer, public, parameter :: RRiPumpedDWA                 = 42
    integer, public, parameter :: RRiPumpedRWA                 = 43
    integer, public, parameter :: RRiDWA2RWA                   = 44
    integer, public, parameter :: RRiDWA2DWA                   = 45
    integer, public, parameter :: RRiRWA2DWA                   = 46
    integer, public, parameter :: RRiSurfaceRWA                = 47
    integer, public, parameter :: RRiStorageVolDyn             = 48
    integer, public, parameter :: RRiWaterUse                  = 49
    integer, public, parameter :: RRiFlowIn                    = 50
    integer, public, parameter :: RRiTotalInAtNode_m3          = 51
    integer, public, parameter :: RRiTotalInViaLinks_m3        = 52
    integer, public, parameter :: RRiTotalOutAtNode_m3         = 53
    integer, public, parameter :: RRiTotalOutViaLinks_m3       = 54
    integer, public, parameter :: RRiDeltaStorage_m3           = 55
    integer, public, parameter :: RRiBalanceError_m3           = 56
    integer, public, parameter :: RRiCumInAtNode_m3            = 57
    integer, public, parameter :: RRiCumInViaLinks_m3          = 58
    integer, public, parameter :: RRiCumOutAtNode_m3           = 59
    integer, public, parameter :: RRiCumOutViaLinks_m3         = 60
    integer, public, parameter :: RRiCumDeltaStorage_m3        = 61
    integer, public, parameter :: RRiCumBalanceError_m3        = 62
    integer, public, parameter :: RRiEvaporationPaved          = 63
    integer, public, parameter :: RRiEvaporationUnpaved        = 64
    integer, public, parameter :: RRiEvaporationOpenWater      = 65
    integer, public, parameter :: RRiDWFPaved                  = 66
    integer, public, parameter :: RRiNetSeepageUnpaved         = 67
    integer, public, parameter :: RRiStoragePaved              = 68
    integer, public, parameter :: RRiStorageUnpaved            = 69
    integer, public, parameter :: RRiStorageGreenhouses        = 70
    integer, public, parameter :: RRiStorageWWTP               = 71
    integer, public, parameter :: RRiBoundariesOut             = 72
    integer, public, parameter :: RRiBoundariesIn              = 73
    integer, public, parameter :: RRiExternalInflowRRRunoff    = 74
    integer, public, parameter :: RRiStorageChangeRRRunoff     = 75
    integer, public, parameter :: RRiGwOutflow                 = 76
	integer, public, parameter :: RRiSacrPrecip			       = 77
	integer, public, parameter :: RRiSacrPotEvap			   = 78
	integer, public, parameter :: RRiSacrActEvap               = 79
	integer, public, parameter :: RRiSacrRunoffImpArea         = 80
	integer, public, parameter :: RRiSacrChannelInflow         = 81
	integer, public, parameter :: RRiSacrSideSubSurfaceOutflow = 82
	integer, public, parameter :: RRiSacrAddImpAreaContent     = 83
	integer, public, parameter :: RRiRRunoffOutflow			   = 84
	integer, public, parameter :: RRiRRunoffRainfall           = 85
	integer, public, parameter :: RRiRRunoffSnowfall           = 86
	integer, public, parameter :: RRiRRunoffPotEvap            = 87
	integer, public, parameter :: RRiRRunoffActEvap            = 88
	integer, public, parameter :: RRiRRunoffBaseflow           = 89
	integer, public, parameter :: RRiRRunoffInterflow          = 90
	integer, public, parameter :: RRiRRunoffQuickflow          = 91
	integer, public, parameter :: RRiRRunoffDrySnowContent     = 92
	integer, public, parameter :: RRiRRunoffFreeWaterContent   = 93
	integer, public, parameter :: RRiRRunoffSoilMoisture       = 94
	integer, public, parameter :: RRiRRunoffUpperZoneContent   = 95
	integer, public, parameter :: RRiRRunoffLowerZoneContent   = 96
	integer, public, parameter :: RRiRRunoffTemperature        = 97
	integer, public, parameter :: RRiRRunoffTotalRunoff        = 98
	
end Module RR_Open_mi_support
