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

module RTC_Open_mi_support

  use wl_open_mi_support

  ! variables

! component id
  Character(len=oes_id_len), parameter :: RTCComponent   = 'RTC'

! element set ids
  Character(len=oes_id_len), parameter ::  RTCRRLocationsElmSet    = 'RRLocations'
  Character(len=oes_id_len), parameter ::  RTCCalcPointElmSet      = 'RTC-CalcPointElmSet'
  Character(len=oes_id_len), parameter ::  RTCReachSegElmSet       = 'RTC-ReachSegments'
  Character(len=oes_id_len), parameter ::  RTCStructures           = 'Structures'
  Character(len=oes_id_len), parameter ::  RTCPumps                = 'Pumps'
  Character(len=oes_id_len), parameter ::  RTCCFSetpoints          = 'CF measures'
  Character(len=oes_id_len), parameter ::  RTCRRStructures         = 'RR structures'

! quantity ids
  Character(len=oes_id_len), parameter ::  RTCOpenwaterlevel       = 'Openwater level'
  Character(len=oes_id_len), parameter ::  RTCGroundwaterlevel     = 'Groundwater level'
  Character(len=oes_id_len), parameter ::  RTCWaterlevel           = 'Water level'
  Character(len=oes_id_len), parameter ::  RTCDischarge            = 'Discharge'
  Character(len=oes_id_len), parameter ::  RTCVelocity             = 'Velocity'
  Character(len=oes_id_len), parameter ::  RTCStructurePar         = 'Structure par'
  Character(len=oes_id_len), parameter ::  RTCSurfaceArea          = 'Storage surface area'
  Character(len=oes_id_len), parameter ::  RTCWaterDepth           = 'Water depth'
  Character(len=oes_id_len), parameter ::  RTCCrestLevel           = 'Crest level'
  Character(len=oes_id_len), parameter ::  RTCCrestWidth           = 'Crest width'
  Character(len=oes_id_len), parameter ::  RTCGateLowerEdgeLevel   = 'Gate lower edge level'
  Character(len=oes_id_len), parameter ::  RTCGateOpeningHeight    = 'Gate opening height'
  Character(len=oes_id_len), parameter ::  RTCFlowArea             = 'Flow area'
  Character(len=oes_id_len), parameter ::  RTCWaterlevelup         = 'Water level up'
  Character(len=oes_id_len), parameter ::  RTCWaterleveldown       = 'Water level down'
  Character(len=oes_id_len), parameter ::  RTCHead                 = 'Head'
  Character(len=oes_id_len), parameter ::  RTCForcedifference      = 'Force difference per unit width'
  Character(len=oes_id_len), parameter ::  RTCCFSetpoint           = 'Setpoint'
  Character(len=oes_id_len), parameter ::  RTCPumpstop             = 'Pumpstop'
  Character(len=oes_id_len), parameter ::  RTCRRHighOn             = 'RR high on'
  Character(len=oes_id_len), parameter ::  RTCRRHighOff            = 'RR high off'
  Character(len=oes_id_len), parameter ::  RTCRRLowOn              = 'RR low on'
  Character(len=oes_id_len), parameter ::  RTCRRLowOff             = 'RR low off'

end Module RTC_Open_mi_support
