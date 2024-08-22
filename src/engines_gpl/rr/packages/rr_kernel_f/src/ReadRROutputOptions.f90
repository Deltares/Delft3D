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

        Subroutine ReadRROutputOptions (InputFileName, MaxSeriesPerMap, NMAP)

!*********************************************************************
!***                D E L F T         H Y D R A U L I C S
!***
!***              WATER RESOURCES AND ENVIRONMENT DIVISION
!*********************************************************************
!*** Program :  Sobek_RR                                August 2006
!*** Module  :
!*********************************************************************
!*** Created    : Okt 2000                        By : Geert Prinsen
!*********************************************************************
!*********************************************************************
!*** Brief description:
!*** ------------------
!***    Read output options using Properties module
!*********************************************************************
!*********************************************************************
!*** ADRESS     : Deltares,
!***              P.O.BOX 177,
!***              2600 MH DELFT, THE NETHERLANDS
!**********************************************************************

  use DH_Alloc
  use RR_PROPERTIES
  !use dio_prop !all the prop_get stuff is defined here too. 
  !prop_seems to defined in properties, in dio-prop.F90 and propert.f90
  
  use Readlib
  use Conf_Fil
  use Network


  IMPLICIT NONE
 

    Integer       Iout1, Idebug
    Character(Len=FilCharIdLength) InputFileName, String

    Logical       success
    Integer       NMAP, i
    Integer       MaxSeriesPerMap(NMAP)


!  Get info from Ini file
    Call LowerC(InputFileName)
    Call Prop_file (InputFileName)

! fill with default values, if nothing specified
    Success = DH_AllocInit (MaxSeriesPerMap(1), OutputPaved, -1)
    Success = success .and. DH_AllocInit (MaxSeriesPerMap(2), OutputUnpaved, -1)
    Success = success .and. DH_AllocInit (MaxSeriesPerMap(3), OutputGreenhouse, -1)
    Success = success .and. DH_AllocInit (MaxSeriesPerMap(4), OutputOpenWater, -1)
    Success = success .and. DH_AllocInit (MaxSeriesPerMap(5), OutputStructure, -1)
    Success = success .and. DH_AllocInit (MaxSeriesPerMap(6), OutputBoundary, -1)
    Success = success .and. DH_AllocInit (MaxSeriesPerMap(7), OutputNWRW, -1)
    Success = success .and. DH_AllocInit (MaxSeriesPerMap(8), OutputBalance, -1)
    Success = success .and. DH_AllocInit (MaxSeriesPerMap(9), OutputSalt, -1)
    Success = success .and. DH_AllocInit (MaxSeriesPerMap(10), OutputWWTP, -1)
    Success = success .and. DH_AllocInit (MaxSeriesPerMap(11), OutputIndustry, -1)
    Success = success .and. DH_AllocInit (MaxSeriesPerMap(12), OutputSacramento, -1)
    Success = success .and. DH_AllocInit (MaxSeriesPerMap(13), OutputLink, -1)
    Success = success .and. DH_AllocInit (MaxSeriesPerMap(14), OutputCell, -1)
    Success = success .and. DH_AllocInit (MaxSeriesPerMap(15), OutputRRRunoff, -1)

! Get RR output options paved
    Call Prop_get  ('OutputPaved', 'StorageRWA',  OutputPaved(1))
    Call Prop_get  ('OutputPaved', 'StorageDWA',  OutputPaved(2))
    Call Prop_get  ('OutputPaved', 'StorageStreet',  OutputPaved(3))
    Call Prop_get  ('OutputPaved', 'Spilling',  OutputPaved(4))
    Call Prop_get  ('OutputPaved', 'PumpedFlow',  OutputPaved(5))
    Call Prop_get  ('OutputPaved', 'Q_openwater',  OutputPaved(6))
    Call Prop_get  ('OutputPaved', 'Rainfall',  OutputPaved(7))
    Call Prop_get  ('OutputPaved', 'DWAinflowRWA',  OutputPaved(8))
    Call Prop_get  ('OutputPaved', 'DWAinflowDWA',  OutputPaved(9))
    Call Prop_get  ('OutputPaved', 'StreetToRWA',  OutputPaved(10))
    Call Prop_get  ('OutputPaved', 'RWAToDWA',  OutputPaved(11))
    Call Prop_get  ('OutputPaved', 'SpillingRWA',  OutputPaved(12))
    Call Prop_get  ('OutputPaved', 'PumpedRWA',  OutputPaved(13))
    Call Prop_get  ('OutputPaved', 'SpillingDWA',  OutputPaved(14))
    Call Prop_get  ('OutputPaved', 'PumpedDWA',  OutputPaved(15))
    Call Prop_get  ('OutputPaved', 'EvapSurface',  OutputPaved(16))
    Call Prop_get  ('OutputPaved', 'VolDynStorage',  OutputPaved(17))

! Get RR output options Unpaved
    Call Prop_get  ('OutputUnpaved', 'SurfaceRunoff',  OutputUnpaved(1))
    Call Prop_get  ('OutputUnpaved', 'GroundwaterOutflow',  OutputUnpaved(2))
    Call Prop_get  ('OutputUnpaved', 'Rainfall',  OutputUnpaved(3))
    Call Prop_get  ('OutputUnpaved', 'EvapSurface',  OutputUnpaved(4))
    Call Prop_get  ('OutputUnpaved', 'Infiltration',  OutputUnpaved(5))
    Call Prop_get  ('OutputUnpaved', 'Seepage',  OutputUnpaved(6))
    Call Prop_get  ('OutputUnpaved', 'ActualEvap',  OutputUnpaved(7))
    Call Prop_get  ('OutputUnpaved', 'PotentialEvap',  OutputUnpaved(8))
    Call Prop_get  ('OutputUnpaved', 'Percolation',  OutputUnpaved(9))
    Call Prop_get  ('OutputUnpaved', 'CapilRise',  OutputUnpaved(10))
    Call Prop_get  ('OutputUnpaved', 'GroundwaterLevel',  OutputUnpaved(11))
    Call Prop_get  ('OutputUnpaved', 'InundationPercentage',  OutputUnpaved(12))
    Call Prop_get  ('OutputUnpaved', 'StorageLandInmm',  OutputUnpaved(13))
    Call Prop_get  ('OutputUnpaved', 'GroundwaterVolume',  OutputUnpaved(14))
    Call Prop_get  ('OutputUnpaved', 'StorageLandInm3',  OutputUnpaved(15))
    Call Prop_get  ('OutputUnpaved', 'GwThresholdExceedance',  OutputUnpaved(16))
    Call Prop_get  ('OutputUnpaved', 'SurfaceLevel-GwLevel',  OutputUnpaved(17))
    Call Prop_get  ('OutputUnpaved', 'IrrigationSupply',  OutputUnpaved(18))
    Call Prop_get  ('OutputUnpaved', 'IrrigationGWdem',  OutputUnpaved(19))
    Call Prop_get  ('OutputUnpaved', 'StorageCoefficent',  OutputUnpaved(20))
    Call Prop_get  ('OutputUnpaved', 'UnsatZoneInmm',  OutputUnpaved(21))
    Call Prop_get  ('OutputUnpaved', 'UnsatZoneInm3',  OutputUnpaved(22))

! Get RR output options Greenhouse
    Call Prop_get  ('OutputGreenhouse', 'StorageBasins',  OutputGreenhouse(1))
    Call Prop_get  ('OutputGreenhouse', 'FlowBasins',  OutputGreenhouse(2))
    Call Prop_get  ('OutputGreenhouse', 'Rainfall',  OutputGreenhouse(3))
    Call Prop_get  ('OutputGreenhouse', 'Evaporation',  OutputGreenhouse(4))
    Call Prop_get  ('OutputGreenhouse', 'WaterUse',  OutputGreenhouse(5))

! Get RR output options OpenWater
    Call Prop_get  ('OutputOpenWater', 'WaterLevel',  OutputOpenWater(1))
    Call Prop_get  ('OutputOpenWater', 'WaterVolume',  OutputOpenWater(2))
    Call Prop_get  ('OutputOpenWater', 'Rainfall',  OutputOpenWater(3))
    Call Prop_get  ('OutputOpenWater', 'Evaporation',  OutputOpenWater(4))
    Call Prop_get  ('OutputOpenWater', 'Seepage',  OutputOpenWater(5))
    Call Prop_get  ('OutputOpenWater', 'MaxLvlThresholdExceedance',  OutputOpenWater(6))
    Call Prop_get  ('OutputOpenWater', 'IterationBalanceError',  OutputOpenWater(7))
    Call Prop_get  ('OutputOpenWater', 'FillingPercentage',  OutputOpenWater(8))
    Call Prop_get  ('OutputOpenWater', 'TargetLevel',  OutputOpenWater(9))

! Get RR output options Structure
    Call Prop_get  ('OutputStructure', 'Flow',  OutputStructure(1))
    Call Prop_get  ('OutputStructure', 'CrestLevel/OpeningHeight',  OutputStructure(2))
    Call Prop_get  ('OutputStructure', 'Flow1',  OutputStructure(3))
    Call Prop_get  ('OutputStructure', 'Flow2',  OutputStructure(4))

! Get RR output options Boundary
    Call Prop_get  ('OutputBoundary', 'Flow',  OutputBoundary(1))
    Call Prop_get  ('OutputBoundary', 'Level',  OutputBoundary(2))

! Get RR output options NWRW
    Call Prop_get  ('OutputNWRW', 'StorageDepressions',  OutputNWRW(1))
    Call Prop_get  ('OutputNWRW', 'DynamicStorage',  OutputNWRW(2))
    Call Prop_get  ('OutputNWRW', 'InfiltrationCapacityDepressions',  OutputNWRW(3))
    Call Prop_get  ('OutputNWRW', 'InfiltrationCapacityDynamicStorage',  OutputNWRW(4))
    Call Prop_get  ('OutputNWRW', 'InflowSewer',  OutputNWRW(5))
    Call Prop_get  ('OutputNWRW', 'InfiltrationFromDepressions',  OutputNWRW(6))
    Call Prop_get  ('OutputNWRW', 'InfiltrationFromRunoff',  OutputNWRW(7))
    Call Prop_get  ('OutputNWRW', 'Rainfall',  OutputNWRW(8))
    Call Prop_get  ('OutputNWRW', 'Evaporation',  OutputNWRW(9))
    Call Prop_get  ('OutputNWRW', 'RWF',  OutputNWRW(10))
    Call Prop_get  ('OutputNWRW', 'DWF',  OutputNWRW(11))
    ! max. inflows for 12 types
    Do i=12,23
       OutputNWRW(i) = OutputNWRW(5)
    Enddo
    ! max. inflows for 12 special types
    Do i=24,35
       OutputNWRW(i) = OutputNWRW(5)
    Enddo
    ! max. storages for 12 special types
    Do i=36,47
       OutputNWRW(i) = OutputNWRW(1)
    Enddo
    ! WADI output
    Do i=48,53
       OutputNWRW(i) = -1
    Enddo

! Get RR output options WWTP
    Call Prop_get  ('OutputWWTP', 'Inflow',  OutputWWTP(1))
    Call Prop_get  ('OutputWWTP', 'Outflow',  OutputWWTP(2))

! Get RR output options Industry
    Call Prop_get  ('OutputIndustry', 'Demand',  OutputIndustry(1))
    Call Prop_get  ('OutputIndustry', 'Allocation',  OutputIndustry(2))
    Call Prop_get  ('OutputIndustry', 'Shortage',  OutputIndustry(3))
    Call Prop_get  ('OutputIndustry', 'Discharge',  OutputIndustry(4))

! Get RR output options Salt
    Call Prop_get  ('OutputSalt', 'Concentration',  OutputSalt(1))

! Get RR output options Sacramento
    Call Prop_get  ('OutputSacramento', 'UpperZoneTensionWaterContent',  OutputSacramento(1))
    Call Prop_get  ('OutputSacramento', 'UpperZoneFreeWaterContent',  OutputSacramento(2))
    Call Prop_get  ('OutputSacramento', 'LowerZoneTensionWaterContenteet',  OutputSacramento(3))
    Call Prop_get  ('OutputSacramento', 'LowerZoneFreePrimaryWaterContent',  OutputSacramento(4))
    Call Prop_get  ('OutputSacramento', 'LowerZoneFreeSupplementaryWaterContent',  OutputSacramento(5))
    Call Prop_get  ('OutputSacramento', 'Rainfall',  OutputSacramento(6))
    Call Prop_get  ('OutputSacramento', 'PotentialEvap',  OutputSacramento(7))
    Call Prop_get  ('OutputSacramento', 'ActualEvap',  OutputSacramento(8))
    Call Prop_get  ('OutputSacramento', 'Baseflow',  OutputSacramento(9))
    Call Prop_get  ('OutputSacramento', 'SurfaceRunoff',  OutputSacramento(10))
    Call Prop_get  ('OutputSacramento', 'RunoffImpervious', OutputSacramento(11))
    Call Prop_get  ('OutputSacramento', 'TotalRunoff',  OutputSacramento(12))
    Call Prop_get  ('OutputSacramento', 'ChannelInflow',  OutputSacramento(13))
    Call Prop_get  ('OutputSacramento', 'SideSsOut',  OutputSacramento(14))
    Call Prop_get  ('OutputSacramento', 'ADIMPContent',  OutputSacramento(15))

! Get RR output options Cell
    Call Prop_get  ('OutputCell', 'PavedHighRainfall',  OutputCell(1))
    Call Prop_get  ('OutputCell', 'PavedHighSurfaceEvap',  OutputCell(2))
    Call Prop_get  ('OutputCell', 'PavedHighSewerInflow',  OutputCell(3))
    Call Prop_get  ('OutputCell', 'PavedHighThroughPut',  OutputCell(4))
    Call Prop_get  ('OutputCell', 'PavedHighStorage',  OutputCell(5))
    Call Prop_get  ('OutputCell', 'PavedLowRainfall',  OutputCell(6))
    Call Prop_get  ('OutputCell', 'PavedLowSurfaceEvap',  OutputCell(7))
    Call Prop_get  ('OutputCell', 'PavedLowInfiltration',  OutputCell(8))
    Call Prop_get  ('OutputCell', 'PavedLowSewerInflow',  OutputCell(9))
    Call Prop_get  ('OutputCell', 'PavedLowThroughPut',  OutputCell(10))
    Call Prop_get  ('OutputCell', 'PavedLowStorage',  OutputCell(11))
    Call Prop_get  ('OutputCell', 'UnpavedHighRainfall',  OutputCell(12))
    Call Prop_get  ('OutputCell', 'UnpavedHighHighSurfaceEvap',  OutputCell(13))
    Call Prop_get  ('OutputCell', 'UnpavedHighThroughFall',  OutputCell(14))
    Call Prop_get  ('OutputCell', 'UnpavedHighStorageHigh',  OutputCell(15))
    Call Prop_get  ('OutputCell', 'UnpavedHighLowSurfaceEvap',  OutputCell(16))
    Call Prop_get  ('OutputCell', 'UnpavedHighInfiltration',  OutputCell(17))
    Call Prop_get  ('OutputCell', 'UnpavedHighSewerInflow',  OutputCell(18))
    Call Prop_get  ('OutputCell', 'UnpavedHighThroughPut',  OutputCell(19))
    Call Prop_get  ('OutputCell', 'UnpavedHighStorageLow',  OutputCell(20))
    Call Prop_get  ('OutputCell', 'UnpavedLowRainfall',  OutputCell(21))
    Call Prop_get  ('OutputCell', 'UnpavedLowSurfaceEvap',  OutputCell(22))
    Call Prop_get  ('OutputCell', 'UnpavedLowInfiltration',  OutputCell(23))
    Call Prop_get  ('OutputCell', 'UnpavedLowSewerInflow',  OutputCell(24))
    Call Prop_get  ('OutputCell', 'UnpavedLowThroughPut',  OutputCell(25))
    Call Prop_get  ('OutputCell', 'UnpavedLowStorage',  OutputCell(26))
    Call Prop_get  ('OutputCell', 'OpenWaterRainfall',  OutputCell(27))
    Call Prop_get  ('OutputCell', 'OpenWaterEvap',  OutputCell(28))
    Call Prop_get  ('OutputCell', 'OpenWaterStorage',  OutputCell(29))
    Call Prop_get  ('OutputCell', 'OpenWaterLevel',  OutputCell(30))
    Call Prop_get  ('OutputCell', 'OpenWaterInfiltration',  OutputCell(31))
    Call Prop_get  ('OutputCell', 'OpenWaterIrrDemand',  OutputCell(32))
    Call Prop_get  ('OutputCell', 'OpenWaterIrrSupply',  OutputCell(33))
    Call Prop_get  ('OutputCell', 'OpenWaterThroughPut',  OutputCell(34))
    Call Prop_get  ('OutputCell', 'SewerStorage',  OutputCell(35))
    Call Prop_get  ('OutputCell', 'SewerInflowRWF',  OutputCell(36))
    Call Prop_get  ('OutputCell', 'SewerInflowDWF',  OutputCell(37))
    Call Prop_get  ('OutputCell', 'SewerLeakage',  OutputCell(38))
    Call Prop_get  ('OutputCell', 'SewerPumpDischarge',  OutputCell(39))
    Call Prop_get  ('OutputCell', 'SewerOverflow',  OutputCell(40))
    Call Prop_get  ('OutputCell', 'GroundwaterLevel',  OutputCell(41))
    Call Prop_get  ('OutputCell', 'GroundwaterVolume',  OutputCell(42))
    Call Prop_get  ('OutputCell', 'GroundwaterDrainage',  OutputCell(43))
    Call Prop_get  ('OutputCell', 'GroundwaterIrrSupply',  OutputCell(44))
    Call Prop_get  ('OutputCell', 'UnsatZonePotEvap',  OutputCell(45))
    Call Prop_get  ('OutputCell', 'UnsatZoneActEvap',  OutputCell(46))
    Call Prop_get  ('OutputCell', 'UnsatZonePercolation',  OutputCell(47))
    Call Prop_get  ('OutputCell', 'UnsatZoneStorage',  OutputCell(48))
    Call Prop_get  ('OutputCell', 'SurfaceThroughPut',  OutputCell(49))
    Call Prop_get  ('OutputCell', 'TotalRainfall',  OutputCell(50))
    Call Prop_get  ('OutputCell', 'TotalEvap',  OutputCell(51))
    Call Prop_get  ('OutputCell', 'DeltaStorage',  OutputCell(52))
    Call Prop_get  ('OutputCell', 'IrrSupplyFromExt',  OutputCell(53))
    Call Prop_get  ('OutputCell', 'TotalCellInflowSeepDWA',  OutputCell(54))
    Call Prop_get  ('OutputCell', 'TotalCellOutflowToExternal',  OutputCell(55))

! Get RR output options RR Runoff
    Call Prop_get  ('OutputRRRunoff', 'TotalOutflow',  OutputRRRunoff(1))

! Get RR output options Link
    Call Prop_get  ('OutputLink', 'Flow',  OutputLink(1))
    Call Prop_get  ('OutputLink', 'Inflow',  OutputLink(2))
    Call Prop_get  ('OutputLink', 'Outflow',  OutputLink(3))
    ! additional output OWPrecip and OWEvap always on
    OutputLink(4) = -1
    OutputLink(5) = -1






    Return
    End Subroutine ReadRROutputOptions


